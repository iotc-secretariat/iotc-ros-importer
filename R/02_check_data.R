library(data.table)

load_ros_csv_input_files <- function(mapping, directory) {
  sheet_names <- mapping$sheet_names()
  result <- list()
  file <- file.path(directory, "META.csv")
  sheet_content <- fread(file)
  result$META <- sheet_content
  sheets_size <- length(sheet_names)
  for (i in seq(1:sheets_size)) {
    sheet_name <- sheet_names[[i]]
    sheet_columns <- unlist(mapping$sheet_columns(sheet_name))
    file <- file.path(directory, paste0(sheet_name, ".csv"))
    if (file.exists(file)) {
      sheet_content <- fread(file)
    }else {
      sheet_content <- data.table(data.frame(matrix(nrow = 0, ncol = length(sheet_columns))))
      names(sheet_content) <- sheet_columns
    }
    result[[sheet_name]] <- sheet_content
  }
  result
}

load_meta_values <- function(models, data) {
  input_mapping_model <- models$input_mapping_model
  meta_names <- input_mapping_model$meta_cell_names()
  mapping <- input_mapping_model$meta_sheet()
  input_file_content <- data$META
  result_data <- list()
  for (meta_name in meta_names) {
    position <- mapping[[meta_name]]
    split2 <- unlist(strsplit(position, ":"))
    row <- split2[[1]]
    column <- split2[[2]]
    result <- NULL
    tryCatch({
      result <- input_file_content[[as.integer(row), as.integer(column)]]
      if (result == "") {
        result <- NA
      }
    },
      error = function(e) {
      })
    result_data[[meta_name]] <- result
  }
  result_data
}

new_report_meta_table <- function() {
  data.table::data.table(
    meta_name = character(0),
    meta_value = character(0),
    category = character(0),
    message = character(0))
}

new_report_meta_message <- function(meta_name, meta_value, category, message) {
  data.table::data.table(
    meta_name = meta_name,
    meta_value = meta_value,
    category = category,
    message = message
  )
}

new_report_data_table <- function() {
  data.table::data.table(
    column_name = integer(0),
    column = integer(0),
    row = integer(0),
    value = character(0),
    category = character(0),
    message = character(0))
}

new_report_data_message <- function(column, row, column_name, value, category, message) {
  data.table::data.table(
    column_name = column_name,
    column = column,
    row = row,
    value = value,
    category = category,
    message = message
  )
}

is_value_null <- function(value) {
  is.null(value) || is.na(value) || "" == value
}

meta_checks <- c(
  check_meta_mandatory = function(ros_cache, meta_values, check_content) {
    result <- new_report_meta_table()
    for (meta in check_content) {
      value <- meta_values[[meta]]
      if (is_value_null(value)) {
        result <- rbind(result, new_report_meta_message(
          meta,
          value,
          "ERROR",
          "Value is mandatory"
        ))
      }
    }
    result
  },
  check_meta_format = function(ros_cache, meta_values, check_content) {
    result <- new_report_meta_table()
    for (meta in names(check_content)) {
      value <- meta_values[[meta]]
      if (is_value_null(value)) {
        next
      }
      check_format <- check_content[[meta]]
      if (!grepl(check_format, value)) {
        result <- rbind(result, new_report_meta_message(
          meta,
          value,
          "ERROR",
          paste0("Value does not respect format: `", check_format, "`")))
      }
    }
    result
  },
  check_meta_exists_in_code_list = function(ros_cache, meta_values, check_content) {
    result <- new_report_meta_table()
    for (meta in names(check_content)) {
      value <- meta_values[[meta]]
      if (is_value_null(value)) {
        next
      }
      code_list_table <- check_content[[meta]]
      column_gav <- column_location$new(paste0(code_list_table, "→code"))
      table_name <- column_gav$table()$gav()
      cache <- ros_cache$code_list(table_name)
      if (!cache$contains_code(value)) {
        result <- rbind(result, new_report_meta_message(
          meta,
          value,
          "ERROR",
          paste0("Value does not exists in code list `", code_list_table, "`")))
      }
    }
    result
  },
  check_meta_exists_in_database = function(ros_cache, meta_values, check_content) {
    result <- new_report_meta_table()
    for (meta in names(check_content)) {
      value <- meta_values[[meta]]
      if (is_value_null(value)) {
        next
      }
      column <- check_content[[meta]]
      column_gav <- column_location$new(column)
      table_name <- column_gav$table()$gav()
      column_name <- column_gav$column()
      cache <- ros_cache$data_registry(table_name)
      existing_row <- cache$find(column_name, value)
      if (nrow(existing_row) != 1) {
        result <- rbind(result, new_report_meta_message(
          meta,
          value,
          "ERROR",
          paste0("Value does not exists in registry `", column, "`")))
      }
    }
    result
  }
)

data_checks <- c(
  check_column_mandatory = function(ros_cache, sheet_starting_row, sheet_column_names, sheet_data, check_content, category) {
    result <- new_report_data_table()
    for (check_column in check_content) {
      column_index <- as.integer(which(sheet_column_names == check_column))
      values <- sheet_data[[check_column]]
      row_index <- 0
      for (value in values) {
        row_index <- row_index + 1
        if (is_value_null(value)) {
          result <- rbind(result, new_report_data_message(
            column_index,
            row_index + sheet_starting_row,
            check_column,
            value,
            category,
            "Value is mandatory"))
        }
      }
    }
    result
  },
  check_column_mandatory_if_other_columns_are_filled = function(ros_cache, sheet_starting_row, sheet_column_names, sheet_data, check_content, category) {
    result <- new_report_data_table()
    for (check_column in names(check_content)) {
      column_index <- as.integer(which(sheet_column_names == check_column))
      values <- sheet_data[[check_column]]
      dependencies_columns <- check_content[[check_column]]
      all_dependencies_values <- lapply(dependencies_columns, function(f) {
        sheet_data[[f]]
      })
      names(all_dependencies_values) <- dependencies_columns
      row_index <- 0
      for (value in values) {
        row_index <- row_index + 1
        if (is_value_null(value)) {
          # check on dependencies
          missing_dependencies <- list()
          for (dependency in dependencies_columns) {
            dependencies_values <- all_dependencies_values[[dependency]]
            dependency_value <- dependencies_values[[row_index]]
            if (!is_value_null(dependency_value)) {
              missing_dependencies <- append(missing_dependencies, dependency)
            }
          }
          if (length(missing_dependencies) > 0) {
            result <- rbind(result, new_report_data_message(
              column_index,
              row_index + sheet_starting_row,
              check_column,
              value,
              category,
              paste0("The column is mandatory from his required columns `", missing_dependencies, "`")))
          }
        }
      }
    }
    result
  },
  check_column_exists_in_database = function(ros_cache, sheet_starting_row, sheet_column_names, sheet_data, check_content, category) {
    result <- new_report_data_table()
    for (check_column in names(check_content)) {
      column_index <- which(sheet_column_names == check_column)
      database_location <- check_content[[check_column]]
      column_gav <- column_location$new(database_location)
      column_name <- column_gav$column()
      cache <- ros_cache$data_registry(column_gav$table()$gav())
      values <- sheet_data[[check_column]]
      row_index <- 0
      for (value in values) {
        row_index <- row_index + 1
        if (is_value_null(value)) {
          next
        }
        existing_row <- cache$find(column_name, value)
        if (nrow(existing_row) != 1) {
          result <- rbind(result, new_report_data_message(
            column_index,
            row_index + sheet_starting_row,
            check_column,
            value,
            category,
            paste0("Value not found in registry `", database_location, "`")))
        }
      }
    }
    result
  },
  check_column_exists_in_code_list = function(ros_cache, sheet_starting_row, sheet_column_names, sheet_data, check_content, category) {
    result <- new_report_data_table()
    for (check_column in names(check_content)) {
      code_list_table <- check_content[[check_column]]
      column_index <- which(sheet_column_names == check_column)
      column_gav <- column_location$new(paste0(code_list_table, "→code"))
      cache <- ros_cache$code_list(column_gav$table()$gav())
      values <- sheet_data[[check_column]]
      row_index <- 0
      for (value in values) {
        row_index <- row_index + 1
        if (is_value_null(value)) {
          next
        }
        if (!cache$contains_code(value)) {
          result <- rbind(result, new_report_data_message(
            column_index,
            row_index + sheet_starting_row,
            check_column,
            value,
            category,
            paste0("Value not found in code list `", code_list_table, "`")))
        }
      }
    }
    result
  },
  check_column_values = function(ros_cache, sheet_starting_row, sheet_column_names, sheet_data, check_content, category) {
    result <- new_report_data_table()
    for (check_column in names(check_content)) {
      column_index <- which(sheet_column_names == check_column)
      check_format <- check_content[[check_column]]
      values <- sheet_data[[check_column]]
      row_index <- 0
      for (value in values) {
        row_index <- row_index + 1
        if (is_value_null(value)) {
          next
        }
        if (!grepl(check_format, value)) {
          result <- rbind(result, new_report_data_message(
            column_index,
            row_index + sheet_starting_row,
            check_column,
            value,
            category,
            paste0("Value does not respect format `", check_format, "`")))
        }
      }
    }
    result
  }
)
data_checks$check_column_should_exists_in_database <- data_checks$check_column_exists_in_database
data_checks$check_column_should_exists_in_code_list <- data_checks$check_column_exists_in_code_list

check_metas <- function(models, ros_cache, meta_values, output_directory) {
  if (!file.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  result <- new_report_meta_table()
  checks_model <- models$checks_model$content()$META
  for (check_name in names(checks_model)) {
    check_content <- checks_model[[check_name]]
    check_result <- meta_checks[[check_name]](ros_cache, meta_values, check_content)
    if (nrow(check_result) > 0) {
      result <- rbind(result, check_result)
    }
  }
  file <- file.path(output_directory, "META.csv")
  if (nrow(result) > 0) {
    write_file(result, file)
  } else {
    if (file.exists(file)) {
      file.remove(file)
    }
  }
  result
}

check_data <- function(models, ros_cache, data, output_directory) {
  if (!file.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  result <- list()
  checks_model <- models$checks_model
  checks_content <- checks_model$content()
  input_mapping_model <- models$input_mapping_model
  for (sheet_name in checks_model$sheet_names()) {
    sheet_result <- new_report_data_table()
    checks <- checks_content[[sheet_name]]
    sheet_column_names <- input_mapping_model$sheet_columns(sheet_name)
    sheet_starting_row <- input_mapping_model$sheet_starting_row(sheet_name) - 1
    sheet_data <- data[[sheet_name]]
    for (check_name in names(checks)) {
      check_content <- checks[[check_name]]
      category <- ifelse(grepl(".*should.*", check_name), "WARNING", "ERROR")
      check_result <- data_checks[[check_name]](ros_cache, sheet_starting_row, sheet_column_names, sheet_data, check_content, category)
      if (nrow(check_result) > 0) {
        sheet_result <- rbind(sheet_result, check_result)
      }
    }
    file <- file.path(output_directory, paste0(sheet_name, ".csv"))
    if (nrow(sheet_result) > 0) {
      write_file(sheet_result, file)
    } else {
      if (file.exists(file)) {
        file.remove(file)
      }
    }
  }
  result
}

do_check <- function(models, ros_cache, input) {
  data <- load_ros_csv_input_files(models$input_mapping_model, file.path(input, "input"))
  meta_values <- load_meta_values(models, data)
  check_directory <- file.path(input, "checks")
  result <- list()
  result$META <- check_metas(models, ros_cache, meta_values, check_directory)
  append(result, check_data(models, ros_cache, data, check_directory))
}

do_check_all <- function(models, data_root_directory, ros_cache_directory, force = FALSE) {
  ros_cache <- create_ros_cache(models, ros_cache_directory)
  files <- list.files(data_root_directory,
                      recursive = TRUE,
                      pattern = "(.)+\\.xlsx$",
                      full.names = TRUE)
  task_report <- task_report$new(file.path(data_root_directory,"02_check.json"), "Checks input files", "Check %3s/%3s : %s", files)
  for (file in files) {
    task_report$start_task(file)
    input_directory <- dirname(file)
    check_directory <- file.path(input_directory, "checks")
    if (file.exists(check_directory)) {
      if (!force) {
        task_report$skip_task(file)
        # print(sprintf("Skip %3s/%3s : %s", index, length, file))
        next
      }
      lapply(list.files(check_directory,
                        recursive = TRUE,
                        pattern = "(.)+\\.csv",
                        full.names = TRUE), file.remove)

    }
    # print(sprintf("Check %3s/%3s : %s", index, length, file))
    do_check(models, ros_cache, input_directory)
    gc()
    task_report$end_task(file)
  }
  task_report$end()
}

# code_lists <- load_ros_databse_code_lists(list(LL_LATEST_MODEL, PS_LATEST_MODEL), "../iotc-ros-input-data/build/ros/code_lists")
# data_registries <- load_ros_databse_registries(list(LL_LATEST_MODEL, PS_LATEST_MODEL), "../iotc-ros-input-data/build/ros/registries")
# ros_cache <- ros_cache$new(code_lists, data_registries)


# check_data <- do_check(LL_LATEST_MODEL, "../iotc-ros-input-data/build/LL/2021/TAIWAN/AN_WEN_FA_NO_3_51812")
# do_check_all(LL_LATEST_MODEL, "../iotc-ros-input-data/build/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU_FRA_REU_2022_v2", "../iotc-ros-input-data/build/ros", force = TRUE)
# do_check_all(LL_LATEST_MODEL, "../iotc-ros-input-data/build/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU_FRA_REU_2022", "../iotc-ros-input-data/build/ros", force = TRUE)
# do_check_all(LL_LATEST_MODEL, "../iotc-ros-input-data/build/LL/2021/TAIWAN/AN_WEN_FA_NO_3_51812", "../iotc-ros-input-data/build/ros", force = TRUE)

# do_check_all(LL_LATEST_MODEL, "../iotc-ros-input-data/build/LL", "../iotc-ros-input-data/build/ros", force = TRUE)
# do_check_all(PS_LATEST_MODEL, "../iotc-ros-input-data/build/PS", "../iotc-ros-input-data/build/ros", force = TRUE)
