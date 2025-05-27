interaction_targets <- list(
  tar_target(
    dem_int_dt,
    load_data(dem_file, sheet = "Interaction", is_hr = TRUE)[
      Outcome == "Incident Dementia" &
        Model == 1 &
        Condition == "Full follow-up"
    ]
  ),
  tar_map(
    values = dependents,
    names = name,
    tar_target(
      cog_int_dt,
      load_data(cog_file, sheet = "Interaction")[Dependent == dependent]
    )
  ),

  tar_map(
    values = data.table(
      dataset_name = rlang::syms(c(
        "cog_int_dt_attention",
        "cog_int_dt_executive",
        "cog_int_dt_cognition",
        "cog_int_dt_verbal",
        "cog_int_dt_language",
        "cog_int_dt_visuospatial",
        "dem_int_dt"
      )),
      dataset_label = c(
        "attention",
        "executive",
        "cognition",
        "verbal",
        "language",
        "visuospatial",
        "dem"
      ),
      axis_label = c(
        "Attention & Processing Speed",
        "Executive Function",
        "Global Cognition",
        "Verbal Learning and Memory",
        "Language",
        "Visuospatial Function",
        "Incident Dementia"
      ),
      is_hr = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
    ),
    names = dataset_label,

    tar_map(
      values = data.table(
        intvars = c("Sex", "APOE"),
        int_name = c("sex", "apoe")
      ),
      names = int_name,
      tar_target(
        full_dt,
        dataset_name[intvar == intvars]
      ),
      tar_target(
        left_dt,
        {
          dataset_name[
            intvar == intvars &
              Parameter %in% paste0("Cluster", 1:4, "_s1")
          ]
        }
      ),
      tar_target(
        right_dt,
        dataset_name[
          intvar == intvars &
            Parameter %in% paste0("Cluster", 5:9, "_s1")
        ]
      ),
      tar_target(
        full_meta,
        get_meta(full_dt, is_hr)
      ),
      tar_target(
        full_csv,
        create_meta_csv(
          full_meta = full_meta,
          axis_label = axis_label,
          dataset_label = dataset_label,
          is_hr = is_hr,
          file_suffix = int_name,
          interaction_var = intvars
        ),
        format = "file"
      ),
      tar_target(
        left_meta,
        get_meta(left_dt, is_hr)
      ),
      tar_target(
        right_meta,
        get_meta(right_dt, is_hr)
      ),
      tar_target(
        xticks,
        calculate_xticks(left_meta, right_meta, is_hr)
      ),
      tar_target(
        left_plot,
        make_plot(
          left_meta,
          is_hr = is_hr,
          xlab = axis_label,
          xticks = xticks
        )
      ),
      tar_target(
        right_plot,
        make_plot(
          right_meta,
          label_offset = length(unique(left_dt$Parameter)),
          is_hr = is_hr,
          xlab = axis_label,
          xticks = xticks
        )
      ),
      tar_target(
        save_plot,
        {
          pdf_path <- paste0(
            "results/pdfs/",
            dataset_label,
            "_",
            int_name,
            ".pdf"
          )
          png_path <- paste0(
            "results/pngs/",
            dataset_label,
            "_",
            int_name,
            ".png"
          )

          # PDF
          pdf(file = pdf_path, width = 17, height = 7)
          plot_grid_layout(left_plot, right_plot)
          dev.off()

          # PNG
          png(file = png_path, width = 17, height = 7, units = "in", res = 300)
          plot_grid_layout(left_plot, right_plot)
          dev.off()

          c(pdf_path, png_path)
        },
        format = "file"
      )
    )
  )
)

assoc_targets <- list(
  tar_target(
    dem_assoc_dt,
    load_data(dem_file, is_hr = TRUE)[
      Outcome == "Incident Dementia" &
        Model == 1 &
        Condition == "Full follow-up"
    ]
  ),
  tar_target(
    dem_assoc_model3_dt,
    load_data(dem_file, is_hr = TRUE)[
      Outcome == "Incident Dementia" &
        Model == 3 &
        Condition == "Full follow-up"
    ]
  ),
  tar_map(
    values = dependents,
    names = name,
    tar_target(
      cog_assoc_dt,
      load_data(cog_file)[Dependent == dependent]
    )
  ),

  tar_map(
    values = data.table(
      dataset_name = rlang::syms(c(
        "cog_assoc_dt_attention",
        "cog_assoc_dt_executive",
        "cog_assoc_dt_cognition",
        "cog_assoc_dt_verbal",
        "cog_assoc_dt_language",
        "cog_assoc_dt_visuospatial",
        "dem_assoc_dt",
        "dem_assoc_model3_dt"
      )),
      dataset_label = c(
        "attention",
        "executive",
        "cognition",
        "verbal",
        "language",
        "visuospatial",
        "dem",
        "dem3"
      ),
      axis_label = c(
        "Attention & Processing Speed",
        "Executive Function",
        "Global Cognition",
        "Verbal Learning and Memory",
        "Language",
        "Visuospatial Function",
        "Incident Dementia",
        "Incident Dementia"
      ),
      is_hr = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
    ),
    names = dataset_label,
    tar_target(
      left_dt,
      {
        dataset_name[Parameter %in% paste0("Cluster", 1:4, "_s1")]
      }
    ),
    tar_target(
      right_dt,
      dataset_name[Parameter %in% paste0("Cluster", 5:9, "_s1")]
    ),
    tar_target(
      full_meta,
      get_meta(dataset_name, is_hr)
    ),
    tar_target(
      full_csv,
      create_meta_csv(
        full_meta = full_meta,
        axis_label = axis_label,
        dataset_label = dataset_label,
        is_hr = is_hr,
        file_suffix = "assoc"
      ),
      format = "file"
    ),
    tar_target(
      left_meta,
      get_meta(left_dt, is_hr)
    ),
    tar_target(
      right_meta,
      get_meta(right_dt, is_hr)
    ),
    tar_target(
      xticks,
      calculate_xticks(left_meta, right_meta, is_hr)
    ),
    tar_target(
      left_plot,
      make_plot(
        left_meta,
        is_hr = is_hr,
        xlab = axis_label,
        xticks = xticks
      )
    ),
    tar_target(
      right_plot,
      make_plot(
        right_meta,
        label_offset = length(unique(left_dt$Parameter)),
        is_hr = is_hr,
        xlab = axis_label,
        xticks = xticks
      )
    ),
    tar_target(
      save_plot,
      {
        pdf_path <- paste0("results/pdfs/", dataset_label, "_assoc", ".pdf")
        png_path <- paste0("results/pngs/", dataset_label, "_assoc", ".png")

        # PDF
        pdf(file = pdf_path, width = 17, height = 7)
        plot_grid_layout(left_plot, right_plot)
        dev.off()

        # PNG
        png(file = png_path, width = 17, height = 7, units = "in", res = 300)
        plot_grid_layout(left_plot, right_plot)
        dev.off()

        c(pdf_path, png_path)
      },
      format = "file"
    )
  )
)
