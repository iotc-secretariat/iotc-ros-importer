library(R6)
library(jsonlite)
library(data.table)

load_models <- function(domain, model_version, model_directory) {
  list(
    input_mapping_model =
      input_mapping$new(domain, model_version, model_directory),
    output_mapping_model =
      ouput_mapping$new(domain, model_version, model_directory),
    checks_model =
      input_checks$new(domain, model_version, model_directory),
    extra_mapping_model = load_extra_mapping(domain, model_version, model_directory)
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
      file <- file.path(directory, domain, version, "input-mapping.json")
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
      file <- file.path(directory, domain, version, "output-mapping.json")
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

load_extra_mapping <- function(domain,
                               version,
                               directory) {
  stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
  stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
  stopifnot(!is.null(directory))
  file <- file.path(directory, domain, version, "extra-mapping.json")
  result <- list()
  if (file.exists(file)) {
    tmp <- jsonlite::read_json(file)
    for (i in tmp) {
      for (n in names(i)) {
        print(n)
        result[[n]] <- i[[n]]
      }
    }
  }
  result
}

input_checks <- R6Class(
  "InputChecks",
  public = list(
    initialize = function(domain,
                          version,
                          directory) {
      stopifnot(!is.na(domain), is.character(domain), nchar(domain) > 0)
      stopifnot(!is.na(version), is.character(version), nchar(version) > 0)
      stopifnot(!is.null(directory))
      private$.name <- paste(domain, version, sep = "-")
      file <- file.path(directory, domain, version, "checks-mapping.json")
      private$.content <- jsonlite::read_json(file)
    },
    name = function() {
      private$.name
    },
    content = function() {
      private$.content
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
    required_data_tables = function(extra_table_locations) {
      result <- list()
      result <- append(result, extra_table_locations)
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
    check_structure = function(input_file, import_context) {
      structure <- private$.content$structure
      result <- list()
      for (check_name in names(structure)) {
        # print(check_name)
        check_result <- do.call(check_name,
                                list(input_checks = self,
                                     import_context = import_context,
                                     input_file = input_file,
                                     check_content = structure[[check_name]]))
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