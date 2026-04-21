library(data.table)
library(openxlsx)
library(stringr)

INTIAL_INPUT_DIRECTORY <- "1_input"
CONSOLIDATE_INPUT_DIRECTORY <- "2_consolidate"
CHECKS_INPUT_DIRECTORY <- "3_checks"

load_ros_xlsx_file <- function(mapping, file) {
  sheet_names <- mapping$sheet_names()
  # The result (list of data.table, one per sheet)
  result <- list()

  # Load xsl meta sheet
  yata_frame <- openxlsx::read.xlsx(xlsxFile = file,
                                    colNames = FALSE,
                                    skipEmptyCols = FALSE,
                                    detectDates = TRUE,
                                    sheet = "META")
  result[["META"]] <- as.data.table(yata_frame)
  # Load xsl frames, one per sheet
  yata_frame <- lapply(sheet_names,
                       openxlsx::read.xlsx,
                       xlsxFile = file,
                       colNames = FALSE,
                       skipEmptyCols = FALSE,
                       detectDates = TRUE)
  sheets_size <- length(sheet_names)
  for (i in seq(1:sheets_size)) {
    sheet_name <- sheet_names[[i]]
    sheet_columns <- mapping$sheet_columns(sheet_name)
    sheet_content <- as.data.table(yata_frame[i])
    column_names <- unlist(sheet_columns)
    if (nrow(sheet_content) < 6) {
      # No data in this sheet
      sheet_content <- data.table(data.frame(matrix(nrow = 0, ncol = length(column_names))))
    } else {
      sheet_content <- sheet_content[6:nrow(sheet_content)]
    }
    result[[sheet_name]] <- sheet_content
  }
  result
}

check_sheet_names <- function(content, check_content) {
  result_data <- c()
  logs <- list()
  actual_sheet_names <- names(content)
  for (expected_sheet_name in check_content) {
    if (!expected_sheet_name %in% actual_sheet_names) {
      result_data <- append(result_data, expected_sheet_name)
      logs <- append(logs, paste0("Could not find sheet named `",
                                  expected_sheet_name, "`"))
    }
  }
  list(data = result_data, logs = logs)
}

check_sheet_columns_count <- function(content, check_content) {
  result_data <- c()
  logs <- list()
  for (sheet_name in names(check_content)) {
    expected_columns_count <- check_content[[sheet_name]]
    data <- content[[sheet_name]]
    if (is.null(data)) {
      next
    }
    actual_columns_count <- ncol(data)
    if (expected_columns_count != actual_columns_count) {
      result_data[[sheet_name]] <- list(
        expected_columns_count = expected_columns_count,
        actual_columns_count = actual_columns_count
      )
      logs <- append(logs, paste0("Sheet named `",
                                  sheet_name,
                                  "` expects ",
                                  expected_columns_count,
                                  " column(s) but have ",
                                  actual_columns_count))
    }
  }
  list(data = result_data, logs = logs)
}

check_sheet_rows_count <- function(content, check_content) {
  result_data <- c()
  logs <- list()
  for (sheet_name in names(check_content)) {
    expected_rows_count <- check_content[[sheet_name]]
    data <- content[[sheet_name]]
    if (is.null(data)) {
      next
    }
    actual_rows_count <- nrow(data)
    if (expected_rows_count != actual_rows_count) {
      result_data[[sheet_name]] <- list(
        expected_rows_count = expected_rows_count,
        actual_rows_count = actual_rows_count
      )
      logs <- append(logs, paste0("Sheet named `",
                                  sheet_name,
                                  "` expects ",
                                  expected_rows_count,
                                  " row(s) but have ",
                                  actual_rows_count))
    }
  }
  list(data = result_data, logs = logs)
}

check_structure <- function(checks_model, data) {
  result <- list()
  structure <- checks_model$content()$STRUCTURE
  for (check_name in names(structure)) {
    # print(check_name)
    check_result <- do.call(check_name, list(content = data, check_content = structure[[check_name]]))
    if (length(check_result$logs) > 0) {
      result[[check_name]] <- check_result$data
      # if (grepl(".*should.*", check_name)) {
      #   self$add_warnings(check_result$logs)
      # } else {
      #   self$add_errors(check_result$logs)
      # }
    }
  }
  result
}

load_column_names <- function(mapping, data) {
  sheet_names <- mapping$sheet_names()
  result <- data
  sheets_size <- length(sheet_names)
  for (i in seq(1:sheets_size)) {
    sheet_name <- sheet_names[[i]]
    sheet_columns <- mapping$sheet_columns(sheet_name)
    column_names <- unlist(sheet_columns)
    names(result[[sheet_name]]) <- column_names
  }
  result
}

sanitize_values <- function(data) {
  for (sheet_name in names(data)) {
    sheet <- data[[sheet_name]]
    if (nrow(sheet) == 0) {
      next
    }
    for (col_name in names(sheet)) {
      values <- sheet[[col_name]]
      sanitized_values <- unlist(lapply(values, function(value) {
        if (is.na(value)) {
          return(value)
        }
        if (!is.character(value)) {
          return(value)
        }
        if ("-" == value || "" == value) {
          return(NA)
        }
        value <- str_remove_all(str_squish(value), "\\\t|\\\n|\\\r")
        if ("-" == value || "" == value) {
          return(NA)
        }
        value
      }))
      sheet[[col_name]] <- sanitized_values
    }
    data[[sheet_name]] <- sheet
  }
  data
}

transform_one_to_csv <- function(models, file, export_directory) {
  data <- load_ros_xlsx_file(models$input_mapping_model, file)
  check_result <- check_structure(models$checks_model, data)
  if (length(check_result) > 0) {
    stop(simpleError(sprintf("There is some structure errors found on `%s`:\n %s", file, toString(check_result))))
  }
  data2 <- load_column_names(models$input_mapping_model, data)
  data3 <- sanitize_values(data2)
  if (!dir.exists(export_directory)) {
    dir.create(export_directory, recursive = TRUE)
  }
  for (sheet_name in names(data3)) {
    path <- file.path(export_directory, paste0(sheet_name, ".csv"))
    if (file.exists(path)) {
      file.remove(path)
    }
    sheet_content <- data3[[sheet_name]]
    if (nrow(sheet_content) > 0) {
      fwrite(data3[[sheet_name]], file = path, sep = ",", sep2 = c("", "\"", ""), quote = "auto", encoding = "UTF-8")
    }
  }
  list(data = data3, location = export_directory)
  file.copy(file, file.path(dirname(export_directory), basename(file)))
}

ros_xlsx_file_to_working_directory <- function(file) {
  file.path(str_replace_all(str_trim(str_replace_all(str_replace(basename(file), ".xlsx$", ""), "[ ()-.]", " ")), "[ ]", "_"), INTIAL_INPUT_DIRECTORY)
}

transform_some_to_csv <- function(models, data_root_directory, export_root_directory, force = FALSE, timestamp = format_timestamp(Sys.time())) {
  files <- list.files(data_root_directory,
                      recursive = TRUE,
                      pattern = "(.)+\\.xlsx$",
                      full.names = TRUE)
  task_report <- task_report$new(export_root_directory, INTIAL_INPUT_DIRECTORY, "Extract input files", "Extract %3s/%3s : %s", files, timestamp)
  task_report$run(function(file) {
    relative_file <- str_sub(file, str_length(data_root_directory) + 1)
    export_directory <- file.path(export_root_directory, dirname(relative_file), ros_xlsx_file_to_working_directory(relative_file))
    if (file.exists(export_directory)) {
      if (!force) {
        return(list(skip = TRUE))
      }
    }
    result <- transform_one_to_csv(models, file, export_directory)
    return(list(skip = FALSE, result = NULL))
  })
}

transform_domain <- function(domain, data_root_directory, export_root_directory, model_version = LATEST_MODEL, force = FALSE, timestamp = format_timestamp(Sys.time())) {
  transform_some_to_csv(load_models(domain, model_version),
                        file.path(data_root_directory, domain),
                        file.path(export_root_directory, domain),
                        force,
                        timestamp)
  invisible()
}

# transform_domain("LL", "../iotc-ros-input-data/data", "../iotc-ros-input-data/build", force = TRUE, timestamp="-2026-04-20")
# transform_domain("PS", "../iotc-ros-input-data/data", "../iotc-ros-input-data/build", force = TRUE, timestamp = "-2026-04-20")
