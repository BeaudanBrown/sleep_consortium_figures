plot_grid_layout <- function(left_plot, right_plot) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))

  pushViewport(viewport(layout.pos.col = 1))
  plot(left_plot)
  popViewport()

  pushViewport(viewport(layout.pos.col = 2))
  plot(right_plot)
  popViewport(2)
}

create_meta_csv <- function(
  full_meta,
  axis_label,
  dataset_label,
  is_hr,
  file_suffix = "assoc",
  interaction_var = NULL
) {
  effect_col <- if (is_hr) "HR" else "Effect"
  transform_fn <- if (is_hr) exp else identity

  format_effect <- function(te, lower, upper) {
    sprintf(
      "%.3f [%.3f; %.3f]",
      transform_fn(te),
      transform_fn(lower),
      transform_fn(upper)
    )
  }

  base_cols <- list(
    Cohort = full_meta$data$.studlab,
    Outcome = axis_label,
    Predictor = full_meta$data$.subgroup,
    p = full_meta$p.value
  )

  if (!is.null(interaction_var)) {
    base_cols <- c(list(Interaction = interaction_var), base_cols)
  }

  output <- do.call(data.table, base_cols)
  output[[effect_col]] <- format_effect(
    full_meta$TE,
    full_meta$lower,
    full_meta$upper
  )

  pooled <- rbindlist(lapply(unique(full_meta$byvar), function(pred) {
    msub <- update(full_meta, subset = full_meta$byvar == pred)
    dt_cols <- base_cols
    dt_cols$Cohort <- "Pooled Effects"
    dt_cols$Predictor <- pred
    dt_cols$p <- full_meta$pooled.p.value[[pred]]

    dt <- do.call(data.table, dt_cols)
    dt[[effect_col]] <- format_effect(
      msub$TE.random,
      msub$lower.random,
      msub$upper.random
    )
    dt
  }))

  output <- rbind(output, pooled)
  output[,
    Cohort := factor(
      Cohort,
      levels = c(setdiff(unique(Cohort), "Pooled Effects"), "Pooled Effects")
    )
  ]
  output[, Predictor := factor(Predictor, levels = unique(Predictor))]

  col_order <- if (!is.null(interaction_var)) {
    c("Outcome", "Predictor", "Interaction", "Cohort", effect_col, "p")
  } else {
    c("Outcome", "Predictor", "Cohort", effect_col, "p")
  }
  setcolorder(output, col_order)
  setorder(output, Outcome, Predictor, Cohort)

  csv_path <- paste0("results/csvs/", dataset_label, "_", file_suffix, ".csv")
  fwrite(output, csv_path)
  csv_path
}

calculate_xticks <- function(
  left_meta,
  right_meta,
  is_hr = FALSE,
  n_ticks = 5
) {
  get_pooled_bounds <- function(meta_obj) {
    subgroups <- unique(meta_obj$byvar)
    bounds <- lapply(subgroups, function(sg) {
      msub <- update(meta_obj, subset = meta_obj$byvar == sg)
      c(msub$lower.random, msub$upper.random)
    })
    unlist(bounds)
  }

  if (is_hr) {
    lower_bound <- min(
      exp(left_meta$lower),
      exp(right_meta$lower),
      exp(get_pooled_bounds(left_meta)),
      exp(get_pooled_bounds(right_meta)),
      na.rm = TRUE
    )
    upper_bound <- max(
      exp(left_meta$upper),
      exp(right_meta$upper),
      exp(get_pooled_bounds(left_meta)),
      exp(get_pooled_bounds(right_meta)),
      na.rm = TRUE
    )
    max_dist_from_1 <- max(abs(lower_bound - 1), abs(upper_bound - 1))
    range_size <- max_dist_from_1 * 2
    tick_interval <- range_size / (n_ticks - 1)
    tick_interval_rounded <- round(tick_interval * 20) / 20
    max_dist_rounded <- tick_interval_rounded * (n_ticks - 1) / 2
    ticks <- seq(
      1 - max_dist_rounded,
      1 + max_dist_rounded,
      length.out = n_ticks
    )
  } else {
    lower_bound <- min(
      left_meta$lower,
      right_meta$lower,
      get_pooled_bounds(left_meta),
      get_pooled_bounds(right_meta),
      na.rm = TRUE
    )
    upper_bound <- max(
      left_meta$upper,
      right_meta$upper,
      get_pooled_bounds(left_meta),
      get_pooled_bounds(right_meta),
      na.rm = TRUE
    )
    max_abs <- max(abs(lower_bound), abs(upper_bound))
    range_size <- max_abs * 2
    tick_interval <- range_size / (n_ticks - 1)
    tick_interval_rounded <- round(tick_interval * 20) / 20
    max_abs_rounded <- tick_interval_rounded * (n_ticks - 1) / 2
    ticks <- seq(-max_abs_rounded, max_abs_rounded, length.out = n_ticks)
  }
  round(ticks, 2)
}

load_data <- function(file_path, sheet = "Association", is_hr = FALSE) {
  dat <- read_excel(file_path, sheet = sheet)
  dat <- dat[order(dat$Cohort), ]

  dat$Parameter2 <- paste("CLV", substr(dat$Parameter, 8, 8), sep = " ")
  dat$Mvar <- paste(dat$Parameter2, dat$Response)

  setDT(dat)

  dat[,
    Domain := fcase(
      Parameter2 == "CLV 1",
      "Sleep quantity and efficiency",
      Parameter2 == "CLV 2",
      "Sleep fragmentation",
      Parameter2 == "CLV 3",
      "Light NREM predominance",
      Parameter2 == "CLV 4",
      "N3 predominance",
      Parameter2 == "CLV 5",
      "Spindle number and duration",
      Parameter2 == "CLV 6",
      "REM sleep bouts",
      Parameter2 == "CLV 7",
      "Respiratory disturbances",
      Parameter2 == "CLV 8",
      "SO-spindle coupling",
      Parameter2 == "CLV 9",
      "Spindle amplitude",
      default = "UNK"
    )
  ]
  if (is_hr && !("HazardRatio" %in% names(dat))) {
    dat$HazardRatio <- exp(dat$Estimate)
    dat$HRLowerCL <- exp(dat$Estimate - 1.96 * dat[["Standard_Error"]])
    dat$HRUpperCL <- exp(dat$Estimate + 1.96 * dat[["Standard_Error"]])
  }
  dat$is_hr <- is_hr
  dat
}

get_meta <- function(dat, is_hr = FALSE) {
  if (is_hr) {
    meta_dat <- metagen(
      log(dat$HazardRatio),
      lower = log(dat$HRLowerCL),
      upper = log(dat$HRUpperCL),
      studlab = dat$Cohort,
      n.e = dat$Total,
      n.c = dat$Event,
      pval = dat$ProbChiSq,
      comb.fixed = FALSE,
      comb.random = TRUE,
      method.tau = "SJ",
      method.random.ci = "HK",
      adhoc.hakn.ci = "se",
      adhoc.hakn.pi = "se",
      prediction = FALSE,
      sm = "HR",
      byvar = dat$Domain
    )
  } else {
    meta_dat <- metagen(
      dat$Estimate,
      lower = dat$LowerCL,
      upper = dat$UpperCL,
      studlab = dat$Cohort,
      n.e = dat$N,
      pval = dat$Probt,
      common = FALSE,
      random = TRUE,
      method.tau = "SJ",
      method.random.ci = "HK",
      adhoc.hakn.ci = "se",
      adhoc.hakn.pi = "se",
      prediction = FALSE,
      sm = "SMD",
      subgroup = dat$Domain
    )
  }
  meta_dat$p.value <- ifelse(
    round(meta_dat$pval, 3) == 0,
    "<0.001",
    meta:::formatPT(round(meta_dat$pval, 3), digits = 3)
  )

  meta_dat$pooled.p.value <- ifelse(
    round(meta_dat$pval.random.w, 3) == 0,
    "<0.001",
    meta:::formatPT(round(meta_dat$pval.random.w, 3), digits = 3)
  )
  meta_dat
}

make_plot <- function(
  meta_dat,
  label_offset = 0,
  is_hr = TRUE,
  xlab = "Incident Dementia",
  xticks = c(0.6, 1, 1.4)
) {
  studies <- data.frame(
    subgroup = meta_dat$byvar,
    Study = meta_dat$studlab,
    N = meta_dat$n.e,
    N_events = if (is_hr) meta_dat$n.c else NA_integer_,
    TE = meta_dat$TE,
    seTE = meta_dat$seTE,
    w = meta_dat$w.random,
    i2 = meta_dat$I2.w,
    i2_lower = meta_dat$lower.I2.w,
    i2_upper = meta_dat$upper.I2.w,
    i2_p = meta_dat$pval.Q.w,
    mean = if (is_hr) exp(meta_dat$TE) else meta_dat$TE,
    lower = if (is_hr) exp(meta_dat$lower) else meta_dat$lower,
    upper = if (is_hr) exp(meta_dat$upper) else meta_dat$upper,
    p = meta_dat$p.value,
    stringsAsFactors = FALSE
  )

  groups <- split(
    studies,
    factor(studies$subgroup, levels = unique(studies$subgroup))
  )
  original_names <- names(groups)
  names(groups) <- paste0(
    letters[seq_along(groups) + label_offset],
    ") ",
    names(groups)
  )

  effect_label <- if (is_hr) "HR" else "β"

  if (is_hr) {
    header_row <- c("Study", "N events", "N", effect_label, "95% CI", "P value")
  } else {
    header_row <- c("Study", "N", effect_label, "95% CI", "P value")
  }

  rows_list <- list(header_row)
  is_summary <- c(TRUE)
  mean_v <- c(NA)
  lower_v <- c(NA)
  upper_v <- c(NA)
  hrzl_lines <- list()
  row_ctr <- 1

  for (g in seq_along(groups)) {
    datg <- groups[[g]]
    gname <- names(groups)[g]
    gname_original <- original_names[g]

    if (is_hr) {
      rows_list[[length(rows_list) + 1]] <- c(gname, "", "", "", "", "")
    } else {
      rows_list[[length(rows_list) + 1]] <- c(gname, "", "", "", "")
    }
    is_summary <- c(is_summary, TRUE)
    mean_v <- c(mean_v, NA)
    lower_v <- c(lower_v, NA)
    upper_v <- c(upper_v, NA)
    row_ctr <- row_ctr + 1
    n_subgroup <- 0
    n_events_subgroup <- 0

    # Study rows
    for (i in seq_len(nrow(datg))) {
      n_subgroup <- n_subgroup + datg$N[i]
      if (is_hr) {
        n_events_subgroup <- n_events_subgroup + datg$N_events[i]
      }

      if (is_hr) {
        rows_list[[length(rows_list) + 1]] <- c(
          datg$Study[i],
          as.character(datg$N_events[i]),
          as.character(datg$N[i]),
          sprintf("%.2f", datg$mean[i]),
          sprintf("[%.2f; %.2f]", datg$lower[i], datg$upper[i]),
          datg$p[i]
        )
      } else {
        rows_list[[length(rows_list) + 1]] <- c(
          datg$Study[i],
          as.character(datg$N[i]),
          sprintf("%.2f", datg$mean[i]),
          sprintf("[%.2f; %.2f]", datg$lower[i], datg$upper[i]),
          datg$p[i]
        )
      }

      is_summary <- c(is_summary, FALSE)
      if (is_hr) {
        mean_v <- c(mean_v, datg$mean[i])
        lower_v <- c(lower_v, datg$lower[i])
        upper_v <- c(upper_v, datg$upper[i])
      } else {
        mean_v <- c(mean_v, datg$mean[i])
        lower_v <- c(lower_v, datg$lower[i])
        upper_v <- c(upper_v, datg$upper[i])
      }
      row_ctr <- row_ctr + 1
    }

    # Subgroup pooled effect (summary row)
    msub <- update(meta_dat, subset = studies$subgroup == gname_original)
    m_mean <- msub$TE.random
    m_lower <- msub$lower.random
    m_upper <- msub$upper.random

    i2_strs <- sprintf(
      "I² = %.1f%% [%.1f%%-%.1f%%], p=%.2f",
      datg$i2[i] * 100,
      datg$i2_lower[i] * 100,
      datg$i2_upper[i] * 100,
      datg$i2_p[i]
    )

    if (is_hr) {
      rows_list[[length(rows_list) + 1]] <- c(
        "Pooled effects",
        as.character(n_events_subgroup),
        as.character(n_subgroup),
        sprintf("%.2f", exp(m_mean)),
        sprintf("[%.2f; %.2f]", exp(m_lower), exp(m_upper)),
        meta_dat$pooled.p.value[[gname_original]]
      )
      rows_list[[length(rows_list) + 1]] <- c(
        i2_strs,
        "",
        "",
        "",
        "",
        ""
      )
    } else {
      rows_list[[length(rows_list) + 1]] <- c(
        "Pooled effects",
        as.character(n_subgroup),
        sprintf("%.2f", m_mean),
        sprintf("[%.2f; %.2f]", m_lower, m_upper),
        meta_dat$pooled.p.value[[gname_original]]
      )
      rows_list[[length(rows_list) + 1]] <- c(
        i2_strs,
        "",
        "",
        "",
        ""
      )
    }
    is_summary <- c(is_summary, TRUE)

    is_summary <- c(is_summary, FALSE)
    if (is_hr) {
      mean_v <- c(mean_v, exp(m_mean))
      lower_v <- c(lower_v, exp(m_lower))
      upper_v <- c(upper_v, exp(m_upper))
    } else {
      mean_v <- c(mean_v, m_mean)
      lower_v <- c(lower_v, m_lower)
      upper_v <- c(upper_v, m_upper)
    }
    row_ctr <- row_ctr + 2
    mean_v <- c(mean_v, NA)
    lower_v <- c(lower_v, NA)
    upper_v <- c(upper_v, NA)

    hrzl_lines[[as.character(row_ctr + 1)]] <- gpar(col = "gray80")
  }

  tabletext <- do.call(rbind, rows_list)

  graph_pos <- if (is_hr) 6 else 5

  forestplot(
    labeltext = tabletext,
    is.summary = is_summary,
    mean = mean_v,
    lower = lower_v,
    upper = upper_v,
    zero = if (is_hr) 1 else 0,
    boxsize = 0.3,
    col = fpColors(box = "darkblue", line = "darkblue", summary = "royalblue"),
    xticks = xticks,
    hrzl_lines = hrzl_lines,
    txt_gp = fpTxtGp(
      label = list(gpar(cex = 0.75)),
      ticks = gpar(cex = 0.7),
      xlab = gpar(cex = 0.8)
    ),
    xlab = xlab,
    colgap = unit(3, "mm")
  ) |>
    fp_decorate_graph(graph.pos = graph_pos)
}
