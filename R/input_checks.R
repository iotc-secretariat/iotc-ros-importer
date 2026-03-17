library(R6)
library(jsonlite)

input_checks_list <- c(
  general_check_sheet_names = function(input_checks, import_context, input_file, check_content, sheet_name = NULL, input_file_content = NULL) {
    result <- list()
    actual_sheet_names <- input_file$sheet_names()
    for (expected_sheet_name in check_content) {
      if (!expected_sheet_name %in% actual_sheet_names) {
        result <- append(result, paste0("Could not find sheet named `",
                                        expected_sheet_name, "`"))
      }
    }
    result
  },
  general_check_sheet_columns_count = function(input_checks,
                                               import_context,
                                               input_file,
                                               check_content,
                                               sheet_name = NULL,
                                               input_file_content = NULL) {
    result <- list()
    for (sheet_name in names(check_content)) {
      expected_columns_count <- check_content[[sheet_name]]
      data <- input_file$data()[[sheet_name]]
      if (is.null(data)) {
        break
      }
      actual_columns_count <- dim(data)[2]
      if (expected_columns_count != actual_columns_count) {
        result <- append(result, paste0("Sheet named `",
                                        sheet_name,
                                        "` expects ",
                                        expected_columns_count,
                                        " column(s) but have ",
                                        actual_columns_count))
      }
    }
    result
  },
  sheets_check_meta_mandatory = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    for (meta in check_content) {
      value <- input_file$extract_meta(meta, input_file_content)
      if (is.null(value) || is.na(value)) {
        position <- input_file$extract_meta_position(meta)
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
        result <- append(result, paste0(coordinate, " Could not find mandatory meta"))
      }
    }
    result
  },
  sheets_check_meta_format = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    for (meta in names(check_content)) {
      value <- input_file$extract_meta(meta, input_file_content)
      if (is.null(value) || is.na(value)) {
        break
      }
      check_format <- check_content[[meta]]
      if (!grepl(check_format, value)) {
        position <- input_file$extract_meta_position(meta)
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
        result <- append(result, paste0(coordinate, " Value `",
                                        value, "` does not respect format `",
                                        check_format, "`"))
      }
    }
    result
  },
  sheets_check_meta_exists_in_code_list = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    caches <- import_context$code_list_caches()
    for (meta in names(check_content)) {
      value <- input_file$extract_meta(meta, input_file_content)
      if (is.null(value) || is.na(value)) {
        break
      }
      code_list_table <- check_content[[meta]]
      column_gav <- column_location$new(paste0(code_list_table, "→code"))
      table_name <- column_gav$table()$gav()
      column_name <- column_gav$column()
      cache <- caches$cache(table_name)
      if (!cache$contains_code(value)) {
        position <- input_file$extract_meta_position(meta)
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
        result <- append(result, paste0(coordinate, " Could not find code list (", code_list_table, ") with code `", value, "`"))
      }
    }
    result
  },
  sheets_check_meta_exists_in_database = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    caches <- import_context$data_table_caches()
    for (meta in names(check_content)) {
      value <- input_file$extract_meta(meta, input_file_content)
      if (is.null(value) || is.na(value)) {
        break
      }
      column <- check_content[[meta]]
      column_gav <- column_location$new(column)
      table_name <- column_gav$table()$gav()
      column_name <- column_gav$column()
      cache <- caches$cache(table_name)
      existing_row <- cache$find(column_name, value)
      if (get_row_count(existing_row) != 1) {
        position <- input_file$extract_meta_position(meta)
        coordinate <- paste0("On sheet named `", sheet_name, "` cell [", position, "]", "(", meta, "):")
        result <- append(result, paste0(coordinate, " Could not find data of type `", column, "` with value `", value, "`"))
      }
    }
    result
  },
  sheets_check_column_mandatory = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    column_names <- input_file$mapping()$sheets()[[sheet_name]]
    for (column in check_content) {
      column_index <- which(column_names == column)
      values <- input_file_content[[column]]
      row_index <- 5
      for (value in values) {
        row_index <- row_index + 1
        # print(paste0("sheets_check_column_mandatory ", sheet_name, "-", column, "[", row, "]→", value, "\n"))
        if (is.null(value) || is.na(value)) {
          coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", column, "):")
          result <- append(result, paste0(coordinate, " Could not find mandatory column"))
          break
        }
      }
    }
    result
  },
  sheets_check_column_exists_in_database = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    column_names <- input_file$mapping()$sheets()[[sheet_name]]
    caches <- import_context$data_table_caches()
    for (column in names(check_content)) {
      column_index <- which(column_names == column)
      # print(paste0("sheets_check_column_exists_in_code_list ", sheet_name, "-", column, "\n"))
      column_gav <- column_location$new(paste0(check_content[[column]]))
      column_name <- column_gav$column()
      cache <- caches$cache(column_gav$table()$gav())
      values <- input_file_content[[column]]
      row_index <- 5
      for (value in values) {
        row_index <- row_index + 1
        # print(paste0("sheets_check_column_exists_in_database ", sheet_name, "-", column, "[", row, "]→", value, "\n"))
        if (is.null(value) || is.na(value)) {
          break
        }
        existing_row <- cache$find(column_name, value)
        if (get_row_count(existing_row) != 1) {
          coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", column, "):")
          result <- append(result, paste0(coordinate, " Could not find data of type `", column_gav$gav(), "` with value `", value, "`"))
        }
      }
    }
    result
  },
  sheets_check_column_exists_in_code_list = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    caches <- import_context$code_list_caches()
    column_names <- input_file$mapping()$sheets()[[sheet_name]]
    for (column in names(check_content)) {
      column_index <- which(column_names == column)
      # print(paste0("sheets_check_column_exists_in_code_list ", sheet_name, "-", column, "\n"))
      code_list_table <- check_content[[column]]
      column_gav <- column_location$new(paste0(code_list_table, "→code"))
      cache <- caches$cache(column_gav$table()$gav())
      values <- input_file_content[[column]]
      row_index <- 5
      for (value in values) {
        row_index <- row_index + 1
        # print(paste0("sheets_check_column_exists_in_database ", sheet_name, "-", column, "[", row, "]→", value, "\n"))
        if (is.null(value) || is.na(value)) {
          break
        }
        if (!cache$contains_code(value)) {
          coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", column, "):")
          result <- append(result, paste0(coordinate, " Could not find code list (", code_list_table, ") with code `", value, "`"))
        }
      }
    }
    result
  },
  sheets_check_column_should_exists_in_database = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    caches <- import_context$data_table_caches()
    column_names <- input_file$mapping()$sheets()[[sheet_name]]
    for (column in names(check_content)) {
      column_index <- which(column_names == column)
      # print(paste0("sheets_check_column_exists_in_code_list ", sheet_name, "-", column, "\n"))
      column_gav <- column_location$new(paste0(check_content[[column]]))
      cache <- caches$cache(column_gav$table()$gav())
      # cache <- cache$values() |> subset(select = column_gav$column())
      values <- input_file_content[[column]]
      row_index <- 5
      for (value in values) {
        row_index <- row_index + 1
        # print(paste0("sheets_check_column_exists_in_database ", sheet_name, "-", column, "[", row, "]→", value, "\n"))
        if (is.null(value) || is.na(value)) {
          break
        }
        existing_row <- cache$find(column, value)
        if (is.null(existing_row)) {
          coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", column, "):")
          result <- append(result, paste0(coordinate, ": could not find code list with value `", value, "`"))
        }
      }
    }
    result
  },
  sheets_check_column_should_exists_in_code_list = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    caches <- import_context$code_list_caches()
    column_names <- input_file$mapping()$sheets()[[sheet_name]]
    for (column in names(check_content)) {
      column_index <- which(column_names == column)
      # print(paste0("sheets_check_column_exists_in_code_list ", sheet_name, "-", column, "\n"))
      code_list_table <- check_content[[column]]
      column_gav <- column_location$new(paste0(code_list_table, "→code"))
      cache <- caches$cache(column_gav$table()$gav())
      values <- input_file_content[[column]]
      row_index <- 5
      for (value in values) {
        row_index <- row_index + 1
        # print(paste0("sheets_check_column_exists_in_database ", sheet_name, "-", column, "[", row, "]→", value, "\n"))
        if (is.null(value) || is.na(value)) {
          break
        }
        if (!cache$contains_code(value)) {
          coordinate <- paste0("On sheet named `", sheet_name, "` cell [", row_index, ":", column_index, "]", "(", column, "):")
          result <- append(result, paste0(coordinate, " Could not find code list (", code_list_table, ") with value `", value, "`"))
        }
      }
    }
    result
  },
  sheets_check_column_values = function(input_checks, import_context, input_file, check_content, sheet_name, input_file_content) {
    result <- list()
    column_names <- input_file$mapping()$sheets()[[sheet_name]]
    for (column in names(check_content)) {
      column_index <- which(column_names == column)
      check_format <- check_content[[column]]
      # print(paste0("sheets_check_column_values ", sheet_name, "-", column, " → ", check_format, "\n"))
      values <- input_file_content[[column]]
      row_index <- 5
      for (value in values) {
        row_index <- row_index + 1
        # print(paste0("sheets_check_column_exists_in_database ", sheet_name, "-", column, "[", row, "]→", value, "\n"))
        if (is.null(value) || is.na(value)) {
          break
        }
        if (!grepl(check_format, value)) {
          result <- append(result, paste0("On sheet named `", sheet_name,
                                          "`, column ", column,
                                          " at row ", row_index,
                                          " with value `",
                                          value, "` does not respect format `",
                                          check_format, "`"))
        }
      }
    }
    result
  })

input_checks <- R6Class(
  "InputChecks",
  public = list(
    initialize = function(domain, version, directory, input_checks_list) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
      stopifnot(!is.null(directory))
      private$.name <- paste(domain, version, sep = "-")
      file <- paste(directory, domain, version, "input-checks.json", sep = "/")
      private$.content <- jsonlite::read_json(file)
      private$.checks <- input_checks_list
    },
    name = function() {
      private$.name
    },
    content = function() {
      private$.content
    },
    general = function() {
      private$.content$general
    },
    sheets = function() {
      private$.content$sheets
    },
    required_code_lists = function() {
      result <- list()
      sheets <- self$sheets()
      for (sheet_name in names(sheets)) {
        sheet <- sheets[[sheet_name]]
        for (check_name in names(sheet)) {
          if (check_name == "column_exists_in_code_list"
            || check_name == "column_should_exists_in_code_list") {
            for (check in unique(sheet[[check_name]])) {
              result <- append(result, check)
            }
          }
        }
      }
      unique(result)
    },
    required_data_tables = function() {
      result <- list()
      sheets <- self$sheets()
      for (sheet_name in names(sheets)) {
        sheet <- sheets[[sheet_name]]
        for (check_name in names(sheet)) {
          if (check_name == "column_exists_in_database" ||
            check_name == "column_should_exists_in_database") {
            for (check in sheet[[check_name]]) {
              result <- append(result, column_location$new(check)$table()$gav())
            }
          }
        }
      }
      unique(result)
    },
    check_general = function(input_file, import_context) {
      general <- self$general()
      for (check in names(general)) {
        check_name <- paste0("general_check_", check)
        action <- private$.checks[[check_name]]
        if (is.null(action)) {
          print(paste0("No action ", check_name))
        }
        check_result <- action(self,
                               import_context,
                               input_file,
                               general[[check]],
                               NULL,
                               NULL)
        if (length(check_result) > 0) {
          import_context$add_errors(check_result)
        }
      }
    },
    check_sheets = function(input_file, import_context) {
      sheets <- self$sheets()
      data <- input_file$data()
      for (sheet_name in names(sheets)) {
        sheet <- sheets[[sheet_name]]
        for (check in names(sheet)) {
          check_name <- paste0("sheets_check_", check)
          action <- private$.checks[[check_name]]
          if (is.null(action)) {
            print(paste0("No action ", check_name))
          }
          # print(paste0("Do action ", check_name))
          check_result <- action(self,
                                 import_context,
                                 input_file,
                                 sheet[[check]],
                                 sheet_name,
                                 data[[sheet_name]])
          if (length(check_result) > 0) {
            import_context$add_errors(check_result)
          }
        }
      }
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # content (as JSON)
    .content = NULL,
    # checks
    .checks = NULL
  )
)