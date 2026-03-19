library(R6)
library(jsonlite)
library(data.table)

load_models <- function(domain, model_version, model_directory) {
  c(
    input_mapping_model =
      input_mapping$new(domain, model_version, model_directory),
    output_mapping_model =
      ouput_mapping$new(domain, model_version, model_directory),
    checks_model =
      input_checks$new(domain, model_version, model_directory)
  )
}

input_mapping <- R6Class(
  "InputMapping",
  public = list(
    initialize = function(domain, version, directory) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
      stopifnot(!is.null(directory))
      private$.name <- paste(domain, version, sep = "-")
      file <- paste(directory, domain, version, "input-mapping.json", sep = "/")
      json = json = jsonlite::read_json(file)
      private$.meta_sheet <- json$META
      private$.sheets <- json$sheets
    },
    name = function() {
      private$.name
    },
    meta_sheet = function() {
      private$.meta_sheet
    },
    meta_cell_names = function() {
      names(private$.meta_sheet)
    },
    sheets = function() {
      private$.sheets
    },
    sheet_names = function() {
      names(private$.sheets)
    },
    sheet_columns = function(sheet_name) {
      private$.sheets[[sheet_name]]
    },
    print = function() {
      cat("InputMapping: ", self$name(), " with ", length(self$sheets()), " sheet(s)\n", sep = "")
      cat("META:\n", sep = "")
      for (c in self$meta_cell_names()) {
        cat(c, " → ", private$.meta_sheet[[c]], "\n", sep = "")
      }
      cat("Sheets:\n", sep = "")
      for (sheet_name in self$sheet_names()) {
        sheet <- self$sheet_columns(sheet_name)
        cat(sheet_name, " - ", length(sheet), " column(s)", "\n", sep = "")
        i <- 1
        for (cell in sheet) {
          cat("  ", i, " → ", cell, "\n", sep = "")
          i <- i + 1
        }
      }
      invisible(self)
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # meta sheet
    .meta_sheet = NULL,
    # columns of the sheet
    .sheets = NULL
  )
)

ouput_mapping <- R6Class(
  "OutputMapping",
  public = list(
    initialize = function(domain, version, directory) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
      stopifnot(!is.null(directory))
      private$.name <- paste(domain, version, sep = "-")
      file <- paste(directory, domain, version, "output-mapping.json", sep = "/")
      json = json = jsonlite::read_json(file)
      private$.meta_sheet <- json$META
      private$.sheets <- json$sheets
    },
    name = function() {
      private$.name
    },
    meta_sheet = function() {
      private$.meta_sheet
    },
    meta_cell_names = function() {
      names(private$.meta_sheet)
    },
    sheets = function() {
      private$.sheets
    },
    sheet_names = function() {
      names(private$.sheets)
    },
    sheet_columns = function(sheet_name) {
      private$.sheets[[sheet_name]]
    },
    print = function() {
      cat("OuputMapping: ", self$name(), " with ", length(self$sheets()), " sheet(s)\n", sep = "")
      cat("META:\n", sep = "")
      for (c in self$meta_cell_names()) {
        cat(c, " → ", private$.meta_sheet[[c]], "\n", sep = "")
      }
      cat("Sheets:\n", sep = "")
      for (sheet_name in self$sheet_names()) {
        sheet <- self$sheet_columns(sheet_name)
        cat(sheet_name, " - ", length(sheet), " column(s)", "\n", sep = "")
        for (cell in names(sheet)) {
          cat("  ", cell, " → ", sheet[[cell]], "\n", sep = "")
        }
      }
      invisible(self)
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # meta sheet
    .meta_sheet = NULL,
    # columns of the sheet
    .sheets = NULL
  )
)

input_checks <- R6Class(
  "InputChecks",
  public = list(
    initialize = function(domain,
                          version,
                          directory) {
      # checks = input_checks_list) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
      stopifnot(!is.null(directory))
      private$.name <- paste(domain, version, sep = "-")
      file <- paste(directory, domain, version, "checks-mapping.json", sep = "/")
      private$.content <- jsonlite::read_json(file)
      # private$.checks <- checks
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
          if (check_name == "check_column_exists_in_code_list"
            || check_name == "check_column_should_exists_in_code_list") {
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
          if (check_name == "check_column_exists_in_database" ||
            check_name == "check_column_should_exists_in_database") {
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
      result <- list()
      for (check_name in names(general)) {
        # print(check_name)
        check_result <- do.call(check_name,
                                list(input_checks = self,
                                     import_context = import_context,
                                     input_file = input_file,
                                     check_content = general[[check_name]]))
        if (length(check_result$logs) > 0) {
          result[[check_name]] <- check_result$data
          if (grepl(".*should.*", check_name)) {
            import_context$add_warnings(check_result$logs)
          } else {
            import_context$add_errors(check_result$logs)
          }
        }
      }
      result
    },
    check_sheets = function(input_file, import_context) {
      sheets <- self$sheets()
      data <- input_file$data()
      result <- list()
      for (sheet_name in names(sheets)) {
        # print(sheet_name)
        sheet_result <- list()
        sheet <- sheets[[sheet_name]]
        for (check_name in names(sheet)) {
          # print(check_name)
          check_result <- do.call(check_name,
                                  list(self,
                                       import_context,
                                       input_file,
                                       sheet[[check_name]],
                                       sheet_name,
                                       data[[sheet_name]]))
          if (length(check_result$logs) > 0) {
            sheet_result[[check_name]] <- check_result$data
            if (grepl(".*should.*", check_name)) {
              import_context$add_warnings(check_result$logs)
            } else {
              import_context$add_errors(check_result$logs)
            }
          }
        }
        if (length(sheet_result) > 0) {
          result[[sheet_name]] <- sheet_result
        }
      }
      result
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