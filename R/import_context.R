library(R6)
library(jsonlite)
library(stringr)

import_context <- R6Class(
  "ImportContext",
  public = list(
    initialize = function(models,
                          root_directory,
                          report_root_directory,
                          file,
                          connection = NULL,
                          timestamp) {
      stopifnot(!is.null(models))
      stopifnot(!is.null(root_directory))
      stopifnot(!is.null(report_root_directory))
      stopifnot(!is.null(connection))
      stopifnot(!is.null(file))
      private$.input_mapping_model <- models$input_mapping_model
      private$.checks_model <- models$checks_model
      relative_file <- str_sub(file, str_length(root_directory) + 1)
      report_file <- paste0(report_root_directory, relative_file)
      report_directory <- dirname(report_file)
      if (!dir.exists(file.path(report_directory))) {
        dir.create(file.path(report_directory), recursive = TRUE)
      }
      private$.log_file <- paste0(report_file, "-", timestamp, ".log")
      private$.report_file <- paste0(report_file, "-", timestamp, ".json")
      self$log_info(paste0("Loading input data file: ", file))
      if (file.exists(private$.log_file)) {
        file.remove(private$.log_file)
      }
      # if (file.exists(private$.report_file)) {
      #   file.remove(private$.report_file)
      # }
      data_table_caches_names <- models$checks_model$required_data_tables()
      .data_table_caches <- lapply(data_table_caches_names, function(x) {
        t <- table_location$new(x)
        values <- load_table(t$schema(), t$table(), columns = NULL, connection)
        data_table_cache$new(t, values)
      })
      names(.data_table_caches) <- data_table_caches_names
      private$.data_table_caches <- data_table_caches$new(.data_table_caches)
      private$.input_file <- input_file$new(models$input_mapping_model, file)
      code_list_tables <- models$checks_model$required_code_lists()
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
    report_file = function() {
      private$.report_file
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
      private$.input_mapping_model$sheet_names()
    },
    check_duration = function() {
      private$.check_duration
    },
    set_check_duration = function(duration) {
      private$.check_duration <- duration
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
      private$.report_content = c()
      result <- private$.checks_model$check_general(private$.input_file, self)
      if (length(result) > 0) {
        private$.report_content$general <- result
      }
      self$log_info("--- Checking sheets...")
      result <- private$.checks_model$check_sheets(private$.input_file, self)
      if (length(result) > 0) {
        private$.report_content$sheets <- result
      }
      write_json(private$.report_content, private$.report_file, pretty = TRUE, auto_unbox = TRUE)
    },
    log_info = function(message) {
      cat(sprintf("[%s] [info] %s\n", str_extract(Sys.time(), "([^\\.]+)?"), message), file = self$log_file(), append = TRUE)
    },
    log_warning = function(message) {
      cat(sprintf("[%s] [warning] %s\n", str_extract(Sys.time(), "([^\\.]+)?"), message), file = self$log_file(), append = TRUE)
    },
    log_error = function(message) {
      cat(sprintf("[%s] [error] %s\n", str_extract(Sys.time(), "([^\\.]+)?"), message), file = self$log_file(), append = TRUE)
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
    .input_mapping_model = NULL,
    # checks model
    .checks_model = NULL,
    # input_file
    .input_file = NULL,
    # log file
    .log_file = NULL,
    # report file
    .report_file = NULL,
    # report content (to push to report file)
    .report_content = NULL,
    # data table caches
    .data_table_caches = NULL,
    # data id cahces
    .data_table_ids = NULL,
    # code list caches
    .code_list_caches = NULL,
    # count of check errors
    .check_errors_count = 0,
    # count of check warnings
    .check_warnings_count = 0,
    # duration of check method
    .check_duration = NULL
  )
)

