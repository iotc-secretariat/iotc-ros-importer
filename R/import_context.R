library(R6)
library(jsonlite)
library(stringr)


clean_full_name <- function(value) {
  if (is.null(value)) {
    return(value)
  }
  str_replace_all(str_replace_all(normalize_full_name(value), "-", ""), "\\.", "")
}

compute_full_name_hash <- function(full_name) {
  lapply(str_split(clean_full_name(full_name), " "), function(x) { str_replace_all(toString(sort(x)), ", ", "") })
}

compute_full_names_hash <- function(contact_table) {
  result <- contact_table[, full_name_parts := unlist(lapply(full_name, compute_full_name_hash))]
  setorder(result, id)
  result
}

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
      extra_mapping_model <- models$extra_mapping_model
      data_table_caches_names <- models$checks_model$required_data_tables(
        c("ros_meta.contact", "ros_meta.observer_identifier_mapping", "ros_meta.focal_point"))
      .data_table_caches <- lapply(data_table_caches_names, function(x) {
        t <- table_location$new(x)
        values <- load_table(t$schema(), t$table(), columns = NULL, connection)
        cache <- data_table_cache$new(t, values)
        mapping <- extra_mapping_model[[x]]
        if (!is.null(mapping)) {
          for (i in names(mapping)) {
            cache$set_extra_simple_column_mapping(i, mapping[[i]])
          }
        }
        cache
      })
      names(.data_table_caches) <- data_table_caches_names
      # add custom extra mapping
      observer_identifier_mapping_table <- .data_table_caches$ros_meta.observer_identifier_mapping
      .data_table_caches$
        ros_meta.observer$
        set_extra_column_mapping("iotc_observer_identifier", function(cache, column, value) {
        observer_identifier_mapping_table$find("legacy_iotc_observer_identifier", value)
      })
      contact_table <- .data_table_caches$ros_meta.contact
      compute_full_names_hash(contact_table$values())
      contact_table$set_extra_column_mapping("full_name", function(cache, column, value) {
        # transform value to be upper case and with no accent
        if (is.null(value)) {
          return(NULL)
        }
        value2 <- compute_full_name_hash(value)
        cache$find0("full_name_parts", value2)
      })
      private$.data_table_caches <- data_table_caches$new(.data_table_caches)
      private$.input_file <- input_file$new(models$input_mapping_model, file)
      code_list_tables <- models$checks_model$required_code_lists()
      .code_list_caches <- lapply(code_list_tables, function(x) {
        t <- table_location$new(x)
        values <- load_table(t$schema(), t$table(), columns = NULL, connection)
        setorder(values, code)
        cache <- code_list_cache$new(x, values)
        mapping <- extra_mapping_model[[x]]
        if (!is.null(mapping)) {
          for (i in names(mapping)) {
            cache$set_extra_simple_column_mapping(i, mapping[[i]])
          }
        }
        cache
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
    check = function() {
      self$log_info("--- Loading input file...")
      private$.input_file$load()
      self$log_info("--- Checking file structure...")
      private$.report_content = c()
      result <- private$.check_structure(private$.checks_model, private$.input_file)
      if (length(result) > 0) {
        private$.report_content$structure <- result
      } else {
        self$log_info("--- Loading columns names...")
        private$.input_file$load_column_names()
        self$log_info("--- Loading metas...")
        private$.input_file$load_meta_values()
        self$log_info("--- Checking sheets...")
        result <- private$.check_data(private$.checks_model, private$.input_file)
        if (length(result) > 0) {
          summary <- private$.compute_summary(private$.checks_model, result)
          if (length(summary) > 0) {
            private$.report_content$summary <- summary
          }
          private$.report_content$data <- result
        }
      }
      write_json(private$.report_content, private$.report_file, pretty = TRUE, auto_unbox = TRUE)
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
    report_content = function() {
      private$.report_content
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
    .check_duration = NULL,
    .check_structure = function(check_model, input_file) {
      check_model <- private$.checks_model
      structure <- check_model$content()$structure
      result <- list()
      for (check_name in names(structure)) {
        # print(check_name)
        check_result <- do.call(check_name,
                                list(input_checks = check_model,
                                     import_context = self,
                                     input_file = input_file,
                                     check_content = structure[[check_name]]))
        if (length(check_result$logs) > 0) {
          result[[check_name]] <- check_result$data
          if (grepl(".*should.*", check_name)) {
            self$add_warnings(check_result$logs)
          } else {
            self$add_errors(check_result$logs)
          }
        }
      }
      result
    },
    .check_data = function(check_model, input_file) {
      sheets <- check_model$sheets()
      data <- input_file$data()
      result <- list()
      for (sheet_name in names(sheets)) {
        # print(sheet_name)
        sheet_result <- list()
        sheet <- sheets[[sheet_name]]
        for (check_name in names(sheet)) {
          # print(check_name)
          check_result <- do.call(check_name,
                                  list(check_model,
                                       self,
                                       input_file,
                                       sheet[[check_name]],
                                       sheet_name,
                                       data[[sheet_name]]))
          if (length(check_result$logs) > 0) {
            sheet_result[[check_name]] <- check_result$data
            if (grepl(".*should.*", check_name)) {
              self$add_warnings(check_result$logs)
            } else {
              self$add_errors(check_result$logs)
            }
          }
        }
        if (length(sheet_result) > 0) {
          result[[sheet_name]] <- sheet_result
        }
      }
      result
    },
    .compute_summary = function(check_model, results) {
      sheets <- check_model$sheets()
      result <- list(
        missing_code_list = list(),
        missing_data = list()
      )
      for (sheet_name in names(sheets)) {
        result_sheet <- results[[sheet_name]]
        if (is.null(result_sheet)) {
          next
        }
        # print(sheet_name)
        sheet_result <- list()
        sheet <- sheets[[sheet_name]]
        for (check_name in names(sheet)) {
          if (check_name == "check_meta_exists_in_database") {
            # print(check_name)
            check_result <- result_sheet[[check_name]]
            if (is.null(check_result)) {
              next
            }
            for (check_column_name in names(check_result)) {
              check_column_result <- check_result[[check_column_name]]
              data_table <- check_column_result$data_table
              old_result <- result$missing_data[[data_table]]
              if (is.null(old_result)) {
                old_result <- list()
              }
              result$missing_data[[data_table]] <- unique(append(old_result, check_column_result$actual_value))
            }
            next
          }
          if (check_name == "check_meta_exists_in_code_list") {
            # print(check_name)
            check_result <- result_sheet[[check_name]]
            if (is.null(check_result)) {
              next
            }
            for (check_column_name in names(check_result)) {
              check_column_result <- check_result[[check_column_name]]
              data_table <- check_column_result$code_list
              old_result <- result$missing_code_list[[data_table]]
              if (is.null(old_result)) {
                old_result <- list()
              }
              result$missing_code_list[[data_table]] <- unique(append(old_result, check_column_result$actual_value))
            }
            next
          }
          if (check_name == "check_column_exists_in_database") {
            # print(check_name)
            check_result <- result_sheet[[check_name]]
            if (is.null(check_result)) {
              next
            }
            for (check_column_name in names(check_result)) {
              check_column_result <- check_result[[check_column_name]]
              if (is.null(check_column_result)) {
                next
              }
              data_table <- check_column_result$data_table
              old_result <- result$missing_data[[data_table]]
              if (is.null(old_result)) {
                old_result <- list()
              }
              result$missing_data[[data_table]] <- unique(append(old_result, names(check_column_result$missing_values_on_rows)))
            }
            next
          }
          if (check_name == "check_column_exists_in_code_list") {
            # print(check_name)
            check_result <- result_sheet[[check_name]]
            if (is.null(check_result)) {
              next
            }
            for (check_column_name in names(check_result)) {
              check_column_result <- check_result[[check_column_name]]
              if (is.null(check_column_result)) {
                next
              }
              data_table <- check_column_result$code_list
              old_result <- result$missing_code_list[[data_table]]
              if (is.null(old_result)) {
                old_result <- list()
              }
              result$missing_code_list[[data_table]] <- unique(append(old_result, names(check_column_result$missing_values_on_rows)))
            }
          }
        }
      }
      if (length(result$missing_code_list) == 0) {
        result$missing_code_list <- NULL
      }
      if (length(result$missing_data) == 0) {
        result$missing_data <- NULL
      }
      result
    }
  )
)

