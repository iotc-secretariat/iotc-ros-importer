check_sheet_names <- function(input_checks,
                              import_context,
                              input_file,
                              check_content) {
  result_data <- c()
  logs <- list()
  actual_sheet_names <- input_file$sheet_names()
  for (expected_sheet_name in check_content) {
    if (!expected_sheet_name %in% actual_sheet_names) {
      result_data <- append(result_data, expected_sheet_name)
      logs <- append(logs, paste0("Could not find sheet named `",
                                  expected_sheet_name, "`"))
    }
  }
  list(data = result_data, logs = logs)
}

check_sheet_columns_count <- function(input_checks,
                                      import_context,
                                      input_file,
                                      check_content) {
  result_data <- c()
  logs <- list()
  for (sheet_name in names(check_content)) {
    expected_columns_count <- check_content[[sheet_name]]
    data <- input_file$data()[[sheet_name]]
    if (is.null(data)) {
      next
    }
    actual_columns_count <- get_column_count(data)
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

check_sheet_rows_count <- function(input_checks,
                                   import_context,
                                   input_file,
                                   check_content) {
  result_data <- c()
  logs <- list()
  for (sheet_name in names(check_content)) {
    expected_rows_count <- check_content[[sheet_name]]
    data <- input_file$data()[[sheet_name]]
    if (is.null(data)) {
      next
    }
    actual_rows_count <- get_row_count(data)
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

check_meta_mandatory <- function(input_checks,
                                 import_context,
                                 input_file,
                                 check_content,
                                 sheet_name,
                                 input_file_content) {
  result_data <- c()
  logs <- list()
  for (meta in check_content) {
    value <- input_file$get_meta(meta)
    if (is.null(value) || is.na(value)) {
      position <- input_file$extract_meta_position(meta)
      coordinate <- unlist(strsplit(position, ":"))
      result_data[[meta]] <- list(
        row = as.integer(coordinate[[1]]),
        column = as.integer(coordinate[[2]])
      )
      coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
      logs <- append(logs, paste0(coordinate, " Could not find mandatory meta"))
    }
  }
  list(data = result_data, logs = logs)
}

check_meta_format <- function(input_checks,
                              import_context,
                              input_file,
                              check_content,
                              sheet_name,
                              input_file_content) {
  result_data <- c()
  logs <- list()
  for (meta in names(check_content)) {
    value <- input_file$get_meta(meta)
    if (is.null(value) || is.na(value)) {
      next
    }
    check_format <- check_content[[meta]]
    if (!grepl(check_format, value)) {
      position <- input_file$extract_meta_position(meta)
      coordinate <- unlist(strsplit(position, ":"))
      result_data[[meta]] <- list(
        row = as.integer(coordinate[[1]]),
        column = as.integer(coordinate[[2]]),
        expected_format = check_format,
        actual_value = value
      )
      coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
      logs <- append(logs, paste0(coordinate, " Value `",
                                  value, "` does not respect format `",
                                  check_format, "`"))
    }
  }
  list(data = result_data, logs = logs)
}

check_meta_exists_in_code_list <- function(input_checks,
                                           import_context,
                                           input_file,
                                           check_content,
                                           sheet_name,
                                           input_file_content) {
  result_data <- c()
  logs <- list()
  caches <- import_context$code_list_caches()
  for (meta in names(check_content)) {
    value <- input_file$get_meta(meta)
    if (is.null(value) || is.na(value)) {
      next
    }
    code_list_table <- check_content[[meta]]
    column_gav <- column_location$new(paste0(code_list_table, "→code"))
    table_name <- column_gav$table()$gav()
    column_name <- column_gav$column()
    cache <- caches$cache(table_name)
    if (!cache$contains_code(value)) {
      position <- input_file$extract_meta_position(meta)
      coordinate <- unlist(strsplit(position, ":"))
      result_data[[meta]] <- list(
        row = as.integer(coordinate[[1]]),
        column = as.integer(coordinate[[2]]),
        code_list = code_list_table,
        actual_value = value
      )
      coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
      logs <- append(logs, paste0(coordinate, " Value `", value, "` not found in code list `", code_list_table, "`"))
    }
  }
  list(data = result_data, logs = logs)
}

check_meta_exists_in_database <- function(input_checks,
                                          import_context,
                                          input_file,
                                          check_content,
                                          sheet_name,
                                          input_file_content) {
  result_data <- c()
  logs <- list()
  caches <- import_context$data_table_caches()
  for (meta in names(check_content)) {
    value <- input_file$get_meta(meta)
    if (is.null(value) || is.na(value)) {
      next
    }
    column <- check_content[[meta]]
    column_gav <- column_location$new(column)
    table_name <- column_gav$table()$gav()
    column_name <- column_gav$column()
    cache <- caches$cache(table_name)
    existing_row <- cache$find(column_name, value)
    if (get_row_count(existing_row) != 1) {
      position <- input_file$extract_meta_position(meta)
      coordinate <- unlist(strsplit(position, ":"))
      result_data[[meta]] <- list(
        row = as.integer(coordinate[[1]]),
        column = as.integer(coordinate[[2]]),
        data_table = column,
        actual_value = value)
      coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
      logs <- append(logs, paste0(coordinate, " Value `", value, "` not found at database location `", column_gav$gav(), "`"))
    }
  }
  list(data = result_data, logs = logs)
}

check_column_mandatory <- function(input_checks,
                                   import_context,
                                   input_file,
                                   check_content,
                                   sheet_name,
                                   input_file_content) {
  result_data <- c()
  logs <- list()
  column_names <- input_file$mapping()$sheets()[[sheet_name]]
  for (check_column in check_content) {
    missing_values <- list()
    column_index <- as.integer(which(column_names == check_column))
    values <- input_file_content[[check_column]]
    row_index <- 5
    for (value in values) {
      row_index <- row_index + 1
      if (is.null(value) || is.na(value)) {
        rows <- missing_values[[check_column]]
        if (is.null(rows)) {
          rows <- c(row_index)
        } else {
          rows <- append(rows, row_index)
        }
        missing_values[[check_column]] <- rows
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", check_column, "):")
        logs <- append(logs, paste0(coordinate, " Could not find mandatory column"))
      }
    }
    if (length(missing_values) > 0) {
      result_data[[check_column]] = list(column = column_index,
                                         rows = missing_values[[check_column]])
    }
  }
  list(data = result_data, logs = logs)
}

check_column_mandatory_if_other_columns_are_filled <- function(input_checks,
                                                               import_context,
                                                               input_file,
                                                               check_content,
                                                               sheet_name,
                                                               input_file_content) {
  result_data <- c()
  logs <- list()
  column_names <- input_file$mapping()$sheets()[[sheet_name]]
  for (check_column in names(check_content)) {
    missing_values <- list()
    column_index <- as.integer(which(column_names == check_column))
    values <- input_file_content[[check_column]]
    dependencies_columns <- check_content[[check_column]]
    all_dependencies_values <- lapply(dependencies_columns, function(f) {
      input_file_content[[f]]
    })
    names(all_dependencies_values) <- dependencies_columns

    row_index <- 5
    for (value in values) {
      row_index <- row_index + 1
      row_index_str <- sprintf("%s", row_index)
      if (is.null(value) || is.na(value)) {
        # check on dependencies
        missing_dependencies <- list()
        for (dependency in dependencies_columns) {
          dependencies_values <- all_dependencies_values[[dependency]]
          dependency_value <- dependencies_values[[row_index - 5]]
          if (!is.null(dependency_value) && !is.na(dependency_value)) {
            missing_dependencies <- append(missing_dependencies, dependency)
          }
        }
        if (length(missing_dependencies) > 0) {
          rows <- missing_values[[row_index_str]]
          if (is.null(rows)) {
            rows <- list()
          }
          rows <- append(rows, missing_dependencies)
          missing_values[[row_index_str]] <- rows
          coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", check_column, "):")
          logs <- append(logs, paste0(coordinate, " The column is mandatory from his required columns: ", missing_dependencies))
        }
      }
    }
    if (length(missing_values) > 0) {
      result_data[[check_column]] = list(column = column_index,
                                         rows = missing_values)
    }
  }
  list(data = result_data, logs = logs)
}

check_column_exists_in_database <- function(input_checks,
                                            import_context,
                                            input_file,
                                            check_content,
                                            sheet_name,
                                            input_file_content) {
  result_data <- c()
  logs <- list()
  column_names <- input_file$mapping()$sheets()[[sheet_name]]
  caches <- import_context$data_table_caches()
  for (check_column in names(check_content)) {
    missing_values <- c()
    column_index <- which(column_names == check_column)
    database_location <- check_content[[check_column]]
    column_gav <- column_location$new(database_location)
    column_name <- column_gav$column()
    cache <- caches$cache(column_gav$table()$gav())
    values <- input_file_content[[check_column]]
    row_index <- 5
    for (value in values) {
      row_index <- row_index + 1
      if (is.null(value) || is.na(value)) {
        next
      }
      existing_row <- cache$find(column_name, value)
      if (get_row_count(existing_row) != 1) {
        rows <- missing_values[[value]]
        if (is.null(rows)) {
          rows <- c(row_index)
        } else {
          rows <- append(rows, row_index)
        }
        missing_values[[value]] <- rows
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", check_column, "):")
        logs <- append(logs, paste0(coordinate, " Value `", value, "` not found at database location `", database_location, "`"))
      }
    }
    if (length(missing_values) > 0) {
      result_data[[check_column]] = list(
        data_table = database_location,
        column = column_index,
        missing_values_on_rows = missing_values)
    }
  }
  list(data = result_data, logs = logs)
}

check_column_exists_in_code_list <- function(input_checks,
                                             import_context,
                                             input_file,
                                             check_content,
                                             sheet_name,
                                             input_file_content) {
  result_data <- c()
  logs <- list()
  caches <- import_context$code_list_caches()
  column_names <- input_file$mapping()$sheets()[[sheet_name]]
  for (check_column in names(check_content)) {
    code_list_table <- check_content[[check_column]]
    missing_values <- c()
    column_index <- which(column_names == check_column)
    column_gav <- column_location$new(paste0(code_list_table, "→code"))
    cache <- caches$cache(column_gav$table()$gav())
    values <- input_file_content[[check_column]]
    row_index <- 5
    for (value in values) {
      row_index <- row_index + 1
      if (is.null(value) || is.na(value)) {
        next
      }
      if (!cache$contains_code(value)) {
        rows <- missing_values[[value]]
        if (is.null(rows)) {
          rows <- c(row_index)
        } else {
          rows <- append(rows, row_index)
        }
        missing_values[[value]] <- rows
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", check_column, "):")
        logs <- append(logs, paste0(coordinate, "Value `", value, "` not found in code list `", code_list_table, "`"))
      }
    }
    if (length(missing_values) > 0) {
      result_data[[check_column]] = list(
        code_list = code_list_table,
        column = column_index,
        missing_values_on_rows = missing_values)
    }
  }
  list(data = result_data, logs = logs)
}

check_column_values <- function(input_checks,
                                import_context,
                                input_file,
                                check_content,
                                sheet_name,
                                input_file_content) {
  result_data <- c()
  logs <- list()
  column_names <- input_file$mapping()$sheets()[[sheet_name]]
  for (check_column in names(check_content)) {
    column_index <- which(column_names == check_column)
    missing_values <- c()
    check_format <- check_content[[check_column]]
    values <- input_file_content[[check_column]]
    row_index <- 5
    for (value in values) {
      row_index <- row_index + 1
      if (is.null(value) || is.na(value)) {
        next
      }
      if (!grepl(check_format, value)) {
        rows <- missing_values[[value]]
        if (is.null(rows)) {
          rows <- c(row_index)
        } else {
          rows <- append(rows, row_index)
        }
        missing_values[[value]] <- rows
        logs <- append(logs, paste0("On sheet named `", sheet_name,
                                    "`, column ", check_column,
                                    " at row ", row_index,
                                    " with value `",
                                    value, "` does not respect format `",
                                    check_format, "`"))
      }
    }
    if (length(missing_values) > 0) {
      result_data[[check_column]] = list(expected_format = check_format,
                                         column = column_index,
                                         missing_values = missing_values)
    }
  }
  list(data = result_data, logs = logs)
}

check_column_should_exists_in_database <- check_column_exists_in_database

check_column_should_exists_in_code_list <- check_column_exists_in_code_list
