library(R6)
library(jsonlite)
library(data.table)
library(stringi)
library(stringr)
library(R.oo)
library(lubridate)

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


input_file <- R6Class(
  "InputFile",
  public = list(
    initialize = function(mapping, file) {
      stopifnot(!is.null(mapping))
      stopifnot(!is.null(file))
      private$.file <- file
      private$.mapping <- mapping
    },
    mapping = function() {
      private$.mapping
    },
    file = function() {
      private$.file
    },
    data = function() {
      private$.data
    },
    sheet_names = function() {
      names(self$data())
    },
    extract_meta_position = function(meta_name) {
      mapping <- self$mapping()$meta_sheet()
      mapping[[meta_name]]
    },
    extract_meta = function(meta_name, input_file_content) {
      position <- self$extract_meta_position(meta_name)
      split2 <- unlist(strsplit(position, ":"))
      row <- split2[[1]]
      column <- split2[[2]]
      input_file_content[[as.integer(row), as.integer(column)]]
    },
    load = function() {
      mapping <- private$.mapping
      sheet_names <- mapping$sheet_names()
      # The result (list of data.table, one per sheet, if sheet is empty,
      # then it won't be available in result)
      result <- list()

      # Load xsl meta sheet
      yata_frame <- openxlsx::read.xlsx(xlsxFile = private$.file,
                                        colNames = FALSE,
                                        skipEmptyCols = FALSE,
                                        detectDates = TRUE,
                                        sheet = "META")
      result[["META"]] <- as.data.table(yata_frame)
      # Load xsl frames, one per sheet
      yata_frame <- lapply(sheet_names,
                           openxlsx::read.xlsx,
                           xlsxFile = private$.file,
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
          sheet_content <- data.table(data.frame(
            matrix(nrow = 0, ncol = length(column_names))))
        } else {
          sheet_content <- sheet_content[6:nrow(sheet_content)]
        }
        names(sheet_content) <- column_names
        result[[sheet_name]] <- sheet_content
      }
      private$.data <- result
    }
  ),
  private = list(
    # mapping
    .mapping = NULL,
    # input file
    .file = NULL,
    # loaded data
    .data = NULL
  )
)

import_context <- R6Class(
  "ImportContext",
  public = list(
    initialize = function(mapping_model, checks_model, file, connection = NULL, timestamp) {
      stopifnot(!is.null(mapping_model))
      stopifnot(!is.null(checks_model))
      stopifnot(!is.null(connection))
      stopifnot(!is.null(file))
      private$.mapping_model <- mapping_model
      private$.checks_model <- checks_model
      private$.log_file <- paste0(file, "-", timestamp, ".log")
      self$log_info(paste0("Loading input data file: ", file))
      if (file.exists(private$.log_file)) {
        file.remove(private$.log_file)
      }
      data_table_caches_names <- checks_model$required_data_tables()
      .data_table_caches <- lapply(data_table_caches_names, function(x) {
        t <- table_location$new(x)
        values <- load_table(t$schema(), t$table(), columns = NULL, connection)
        data_table_cache$new(t, values)
      })
      names(.data_table_caches) <- data_table_caches_names
      private$.data_table_caches <- data_table_caches$new(.data_table_caches)
      private$.input_file <- input_file$new(mapping_model, file)
      code_list_tables <- checks_model$required_code_lists()
      .code_list_caches <- lapply(code_list_tables, function(x) {
        t <- table_location$new(x)
        values <- load_table(t$schema(), t$table(), columns = NULL, connection)
        setorder(values, code)
        code_list_cache$new(x, values)
      })
      names(.code_list_caches) <- code_list_tables
      private$.code_list_caches <- code_list_caches$new(.code_list_caches)
      .data_table_ids <- lapply(data_table_caches_names, function(x) {
        t <- table_location$new(x)
        query <- paste0("SELECT t.last_value FROM pg_catalog.pg_sequences t WHERE t.schemaname = '", t$schema(), "' AND sequencename ='", t$table(), "_id_seq'")
        result_set <- dbSendQuery(connection, query)
        id <- as.integer(dbFetch(result_set)[["last_value"]])
        dbClearResult(result_set)
        id
      })
      names(.data_table_ids) <- data_table_caches_names
      private$.data_table_ids <- data_id_cache$new(.data_table_ids)
    },
    input_file = function() {
      private$.input_file
    },
    log_file = function() {
      private$.log_file
    },
    data_table_ids = function() {
      private$.data_table_ids
    },
    data_table_caches = function() {
      private$.data_table_caches
    },
    code_list_caches = function() {
      private$.code_list_caches
    },
    sheet_names = function() {
      private$.mapping_model$sheet_names()
    },
    check_errors_count = function() {
      private$.check_errors_count
    },
    check_warnings_count = function() {
      private$.check_warnings_count
    },
    add_errors = function(errors) {
      private$.check_errors_count <- private$.check_errors_count + length(errors)
      for (i in errors) {
        self$log_error(i)
      }
    },
    add_warnings = function(warnings) {
      private$.check_warnings_count <- private$.check_warnings_count + length(warnings)
      for (i in warnings) {
        self$log_warning(i)
      }
    },
    check = function() {
      self$log_info("--- Loading input file...")
      private$.input_file$load()
      self$log_info("--- Checking General...")
      private$.checks_model$check_general(private$.input_file, self)
      self$log_info("--- Checking sheets...")
      private$.checks_model$check_sheets(private$.input_file, self)
    },
    log_info = function(message) {
      cat(sprintf("[%s] [info] %s\n", now(), message), file = self$log_file(), append = TRUE)
    },
    log_warning = function(message) {
      cat(sprintf("[%s] [warning] %s\n", now(), message), file = self$log_file(), append = TRUE)
    },
    log_error = function(message) {
      cat(sprintf("[%s] [error] %s\n", now(), message), file = self$log_file(), append = TRUE)
    },
    find_data_in_cache = function(table_location, column, value) {
      table_cache <- self$data_table_caches()$cache(table_location)
      row <- table_cache$find(column, value)
      if (get_row_count(row) != 1) {
        NULL
      } else {
        row
      }
    }
  ),
  private = list(
    # mapping model
    .mapping_model = NULL,
    # checks model
    .checks_model = NULL,
    # input_file
    .input_file = NULL,
    # log file
    .log_file = NULL,
    # data table caches
    .data_table_caches = NULL,
    # data id cahces
    .data_table_ids = NULL,
    # code list caches
    .code_list_caches = NULL,
    # count of check errors
    .check_errors_count = 0,
    # count of check warnings
    .check_warnings_count = 0
  )
)
