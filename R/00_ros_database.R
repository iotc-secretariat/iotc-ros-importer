library(iotc.ros.registries)
library(DBI)
library(RPostgres)
library(data.table, warn.conflicts = FALSE)
library(stringr)
library(jsonlite)
library(R6, warn.conflicts = FALSE)

#'The constants holding the name of the latest IOTC_Ros database
#'@export
IOTC_ROS <- "IOTC_Ros_3_3_0_2026_04_16"

#' Connects to an instance of \code{Ros} on a given server machine
#'
#' @param host The server name / IP address (defaults to \code{\link{SERVER_DEFAULT, "localhost}})
#' @param dbname The database name (defaults to \code{Sys.getenv("IOTC_ROS_DB_SERVER", IOTC_ROS)})
#' @param user The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param client_encoding The character set used by the client (defaults to \code{UTF-8})
#' @return An Sql connection to \code{Ros} database
#' @export
connect_to_ros <- function(host = Sys.getenv("IOTC_ROS_DB_HOST", "localhost"),
                           port = Sys.getenv("IOTC_ROS_DB_PORT", 5432),
                           dbname = Sys.getenv("IOTC_ROS_DB_NAME", IOTC_ROS),
                           user = Sys.getenv("IOTC_ROS_DB_USER"),
                           password = Sys.getenv("IOTC_ROS_DB_PWD"),
                           client_encoding = "UTF-8") {
  DBI::dbConnect(drv = RPostgres::Postgres(),
                 host = host,
                 dbname = dbname,
                 port = port,
                 user = user,
                 password = password,
                 client_encoding = client_encoding)
}

#' Execute the given code \code{\link{code_to_execute}} which must be a function
#' with one parameter (the connection provided by this function).
#'
#' The connection is released after the code is executed.
#'
#' @param connection_supplier function to get the connection (defaults to \code{\link{connect_to_ros}})
#' @param code_to_execute the code to execute
#' @return the \code{\link{code_to_execute}} result
#' @export
use_connection <- function(connection_supplier = connect_to_ros, code_to_execute) {
  connection <- NULL
  tryCatch({
    connection <- do.call(connection_supplier, args = list())
    code_to_execute(connection)
  }, finally = {
    if (!is.null(connection)) {
      RPostgres::dbDisconnect(connection)
    }
  })
}

split_table_location <- function(value) {
  unlist(strsplit(value, "\\."))
}

split_column_location <- function(value) {
  unlist(strsplit(value, "→"))
}

split_foreign_key <- function(value) {
  unlist(strsplit(value, ":"))
}

simple_quote <- function(text) {
  ifelse(is.na(text), "NULL", paste0("'", text, "'"))
}

#' Performs and SQL query through a provided JDBC connection and return its results as a \code{data.table}
#'
#' @param connection An JDBC connection to a RDBMS server
#' @param query The query to perform
#' @return The results of executing \code{query} through \code{connection} as a data table
#' @examples
#' query(connection = DB_IOTC_ROS(), query = "SELECT * FROM V_LEGACY_NC")
#' @export
query <- function(connection, query) {
  data.table(dbGetQuery(connection, query))
}

#' Generic method to load the content of a given table.
#'
#' @param schema_name name of schema
#' @param table_name name of table
#' @param columns The optional columns to load (if not specified, then will load all columns of the table)
#' @param connection where to load the table content
#' @return the loaded table as a data.Table
#' @export
load_table <- function(schema_name, table_name, columns = NULL, connection) {
  if (is.null(columns)) {
    columns <- "*"
  } else {
    columns <- paste0(columns, collapse = ", ")
  }
  query(
    connection,
    paste0("SELECT ", columns, " FROM ", schema_name, ".", table_name)
  )
}

prepare_query <- function(sql, column_location, other_params = NULL) {
  sql <- str_replace_all(sql, "\\$schema_name", column_location$table()$schema())
  sql <- str_replace_all(sql, "\\$table_name", column_location$table()$table())
  sql <- str_replace_all(sql, "\\$column_name", column_location$column())
  if (!is.null(other_params)) {
    for (key in names(other_params)) {
      sql <- str_replace_all(sql, key, other_params[key])
    }
  }
  sql
}

get_sequence_value <- function(schema_name, table_name, connection) {
  query <- paste0("SELECT t.last_value FROM pg_catalog.pg_sequences t WHERE t.schemaname = '", schema_name, "' AND sequencename ='", table_name, "_id_seq'")
  result_set <- dbSendQuery(connection, query)
  id <- as.integer(dbFetch(result_set)[["last_value"]])
  dbClearResult(result_set)
  id
}

table_location <- R6Class(
  "TableLocation",
  public = list(
    initialize = function(gav) {
      stopifnot(!is.na(gav), is.character(gav), nchar(gav) > 0, gav %like% ".+\\..+")
      split2 <- split_table_location(gav)
      private$.schema <- split2[[1]]
      private$.table <- split2[[2]]
    },
    table = function() {
      private$.table
    },
    schema = function() {
      private$.schema
    },
    gav = function() {
      sprintf("%s.%s", self$schema(), self$table())
    },
    print = function() {
      cat(self$gav())
    }
  ),
  private = list(
    # schema
    .schema = NULL,
    # table name
    .table = NULL
  )
)

column_location <- R6Class(
  "ColumnLocation",
  public = list(
    initialize = function(gav) {
      stopifnot(!is.na(gav), is.character(gav), nchar(gav) > 0, gav %like% ".+\\..+→.+")
      split <- split_column_location(gav)
      private$.table <- table_location$new(split[[1]])
      private$.column <- split[[2]]
    },
    table = function() {
      private$.table
    },
    column = function() {
      private$.column
    },
    gav = function() {
      sprintf("%s→%s", self$table()$gav(), self$column())
    },
    print = function() {
      cat(self$gav())
    }
  ),
  private = list(
    # schema + table name
    .table = NULL,
    # column name
    .column = NULL
  )
)

foreign_key <- R6Class(
  "ForeignKey",
  public = list(
    initialize = function(gav) {
      stopifnot(!is.na(gav), is.character(gav), nchar(gav) > 0, gav %like% ".+\\..+→.+:.+")
      split <- split_foreign_key(gav)
      split1 <- split_column_location(split[[1]])
      private$.table <- table_location$new(split1[[1]])
      private$.column <- split1[[2]]
      private$.foreign_key <- split[[2]]
    },
    table = function() {
      private$.table
    },
    column = function() {
      private$.column
    },
    foreign_key = function() {
      private$.foreign_key
    },
    gav = function() {
      sprintf("%s→%s:%s", self$table()$gav(), self$column(), self$foreign_key())
    },
    print = function() {
      cat(self$gav())
    }
  ),
  private = list(
    # schema + table name
    .table = NULL,
    # column name
    .column = NULL,
    # foreign key name
    .foreign_key = NULL
  )
)

data_id_cache <- R6Class(
  "DataIdCache",
  public = list(
    initialize = function(caches) {
      stopifnot(!is.null(caches))
      private$.caches <- caches
    },
    caches = function() {
      private$.caches
    },
    get_id = function(table_location) {
      stopifnot(table_location %in% names(private$.caches))
      self$caches()[[table_location]]
    },
    new_id = function(table_location) {
      stopifnot(table_location %in% names(private$.caches))
      cache <- private$.caches[[table_location]] + 1
      private$.caches[[table_location]] <- cache
      cache
    }
  ),
  private = list(
    # all caches
    .caches = NULL
  )
)

format_timestamp <- function(timestamp) {
  str_replace_all(timestamp, "[ :.]", "_")
}

task_report <- R6Class(
  "TaskReport",
  public = list(
    initialize = function(output_directory, output_file_name, comment, log_pattern, files, timestamp="", to_string_file = function(x) x) {
      stopifnot(!is.null(output_directory))
      stopifnot(!is.null(output_file_name))
      stopifnot(!is.null(timestamp))
      stopifnot(!is.null(log_pattern))
      stopifnot(!is.null(files))
      private$.to_string_file <- to_string_file
      private$.comment <- comment
      private$.log_pattern <- log_pattern
      private$.output_file <- file.path(output_directory, sprintf("%s%s.json", output_file_name, timestamp))
      private$.start_time <- Sys.time()
      private$.task_total_count <- length(files)
      private$.files <- files
    },
    run = function(function_for_file) {
      result <- list()
      for (file in private$.files) {
        self$start_task(file)
        tryCatch({
          file_result <- function_for_file(file)
          if (file_result$skip == TRUE) {
            self$skip_task(file)
          }else {
            self$end_task(file)
            result[[private$.to_string_file(file)]] <- file_result$result
          }
        }, error = function(e) {
          print(e$message)
          calls <- sys.calls()
          if (length(calls) >= 2L) {
            sink(stderr())
            on.exit(sink(NULL))
            cat("Backtrace:\n")
            calls <- rev(calls[-length(calls)])
            for (i in seq_along(calls)) {
              cat(i, ": ", deparse(calls[[i]], nlines = 1L), "\n", sep = "")
            }
          }
          private$.error_count <- private$.error_count + 1
          private$.summary$error[[private$.to_string_file(file)]] <- unlist(str_split(e$message, "\n"))
        })
      }
      self$end()
      result
    },
    start_task = function(file) {
      private$.current_time <- Sys.time()
      private$.task_count <- private$.task_count + 1
      cat(paste0("Start ", sprintf(private$.log_pattern,
                                   private$.task_count, private$.task_total_count, private$.to_string_file(file)), "\n"))
    },
    skip_task = function(file) {
      now <- Sys.time()
      private$.skip_count <- private$.skip_count + 1
      private$.total_duration <- difftime(now, private$.start_time)
      task_duration <- difftime(now, private$.current_time)
      if (is.null(private$.min_duration) || private$.min_duration > task_duration) {
        private$.min_duration <- task_duration
      }
      if (is.null(private$.max_duration) || private$.max_duration < task_duration) {
        private$.max_duration <- task_duration
      }
      private$.summary$skip[[private$.to_string_file(file)]] <- private$.format_duration(task_duration)
      cat(paste0("Skip  ", sprintf(private$.log_pattern,
                                   private$.task_count, private$.task_total_count, private$.to_string_file(file)), "\n"))

      self$end()
    },
    end_task = function(file) {
      now <- Sys.time()
      private$.total_duration <- difftime(now, private$.start_time)
      task_duration <- difftime(now, private$.current_time)
      if (is.null(private$.min_duration) || private$.min_duration > task_duration) {
        private$.min_duration <- task_duration
      }
      if (is.null(private$.max_duration) || private$.max_duration < task_duration) {
        private$.max_duration <- task_duration
      }
      private$.summary$success[[private$.to_string_file(file)]] <- private$.format_duration(task_duration)
      cat(paste0("End   ", sprintf(private$.log_pattern,
                                   private$.task_count, private$.task_total_count, private$.to_string_file(file)), sprintf(" - duration: %s, total duration %s\n", private$.format_duration(task_duration), private$.format_duration(private$.total_duration))))
      self$end()
    },
    end = function() {
      now <- Sys.time()
      private$.total_duration <- difftime(now, private$.start_time)
      units(private$.total_duration) <- "secs"
      total_duration_str <- private$.format_duration(private$.total_duration)
      success_count <- private$.task_count -
        private$.skip_count -
        private$.error_count
      content <- c(
        time = list(
          start = paste0("", private$.start_time),
          end = paste0("", Sys.time())),
        comment = private$.comment,
        count = list(
          total = private$.task_count,
          success = success_count,
          skip = private$.skip_count,
          error = private$.error_count),
        duration = list(
          total = total_duration_str,
          min = private$.format_duration(private$.min_duration),
          avg = private$.format_duration(private$.total_duration / success_count),
          max = private$.format_duration(private$.max_duration)),
        result = list(private$.summary))
      write_json(content,
                 private$.output_file,
                 pretty = TRUE,
                 auto_unbox = TRUE)
    }

  ),
  private = list(
    .to_string_file = NULL,
    # comment
    .comment = NULL,
    # files to process
    .files = NULL,
    # log pattern
    .log_pattern = NULL,
    # output file
    .output_file = NULL,
    # total task count
    .task_total_count = 0,
    # task count
    .task_count = 0,
    # skip task count
    .skip_count = 0,
    # error task count
    .error_count = 0,
    # starting time
    .start_time = NULL,
    # current task time
    .current_time = NULL,
    # min duration
    .min_duration = NULL,
    # max duration
    .max_duration = NULL,
    # total duration
    .total_duration = NULL,
    # summary
    .summary = list(success = list(), skip = list(), error = list()),
    .format_duration = function(duration) {
      ifelse(is.null(duration), "", sprintf("%.2f %s", duration, units(duration)))
    })
)

data_table_cache <- R6Class(
  "DataTableCache",
  public = list(
    initialize = function(column_location, values) {
      stopifnot(!is.null(table_location))
      stopifnot(!is.null(values))
      private$.table_location <- column_location$table()
      private$.values <- values
    },
    table_location = function() {
      private$.table_location
    },
    values = function() {
      private$.values
    },
    find = function(column, value) {
      result <- self$find0(column, value)
      if (nrow(result == 1)) {
        return(result)
      }
      simple_mapping <- private$.extra_simple_column_mapping[[column]]
      if (!is.null(simple_mapping)) {
        # Fixme we need to add proper type on incoming data
        simple_value <- simple_mapping[[as.character(value)]]
        if (!is.null(simple_value)) {
          return(self$find(column, simple_value))
        }
      }
      extra_mapping <- private$.extra_column_mapping[[column]]
      if (!is.null(extra_mapping)) {
        mapped_value <- extra_mapping(self, column, value)
        if (!is.null(mapped_value) && nrow(mapped_value) == 1) {
          return(mapped_value)
        }
      }
      NULL
    },
    find0 = function(column, value) {
      self$values()[self$values()[[column]] == value]
    },
    ids = function() {
      private$.ids
    },
    contains_id = function(id) {
      id %in% self$ids()
    },
    set_extra_column_mapping = function(column, mapping_function) {
      private$.extra_column_mapping[[column]] <- mapping_function
    },
    set_extra_simple_column_mapping = function(column, mappings) {
      private$.extra_simple_column_mapping[[column]] <- mappings
    }
  ),
  private = list(
    # location of the data table
    .table_location = NULL,
    # values for the data table
    .values = NULL,
    # ids available for this data table
    .ids = NULL,
    # extra colum mamping we can use
    .extra_column_mapping = c(),
    # extra simple colum mamping we can use (this is fill by the extra-mapping.json file)
    .extra_simple_column_mapping = c()
  )
)

code_list_cache <- R6Class(
  "CodeListCache",
  public = list(
    initialize = function(table_location, values) {
      stopifnot(!is.null(table_location))
      stopifnot(!is.null(values))
      private$.table_location <- table_location
      private$.values <- values
      private$.codes <- as.list(values[, code])
      private$.with_code_orig <- "code_orig" %in% names(values)
      if (private$.with_code_orig) {
        private$.code_orig_mapping <- as.list(values[!is.null(code_orig), .(code_orig, code)][, code_orig := str_trim(code_orig)])
      }
    },
    table_location = function() {
      private$.table_location
    },
    values = function() {
      private$.values
    },
    codes = function() {
      private$.codes
    },
    code_orig_mapping = function() {
      private$.code_orig_mapping
    },
    with_code_orig = function() {
      private$.with_code_orig
    },
    get_code = function(code) {
      if (self$with_code_orig() && code %in% self$code_orig_mapping()$code_orig) {
        position <- which(code == self$code_orig_mapping()$code_orig)
        code <- self$code_orig_mapping()$code[position]
      } else if (!code %in% self$codes()) {
        code <- NULL
      }
      code
    },
    contains_code = function(code) {
      if (code %in% self$codes()) {
        return(TRUE)
      }
      simple_mapping <- private$.extra_simple_column_mapping[["code"]]
      if (!is.null(simple_mapping)) {
        simple_value <- simple_mapping[[code]]
        if (!is.null(simple_value)) {
          return(self$contains_code0(simple_value))
        }
      }
      if (self$with_code_orig()) {
        if (code %in% self$code_orig_mapping()$code_orig) {
          return(TRUE)
        }
      }
      FALSE
    },
    contains_code0 = function(code) {
      if (code %in% self$codes()) {
        return(TRUE)
      }
      if (self$with_code_orig()) {
        if (code %in% self$code_orig_mapping()$code_orig) {
          return(TRUE)
        }
      }
      FALSE
    },
    set_extra_simple_column_mapping = function(column, mappings) {
      private$.extra_simple_column_mapping[[column]] <- mappings
    }
  ),
  private = list(
    # location of the code list table
    .table_location = NULL,
    # values for the code list
    .values = NULL,
    # codes available for this code list
    .codes = NULL,
    # if code_orig is available
    .with_code_orig = NULL,
    # optional mapping of code_orig to code
    .code_orig_mapping = NULL,
    # extra simple colum mamping we can use (this is fill by the extra-mapping.json file)
    .extra_simple_column_mapping = c()
  )
)

load_ros_databse_code_lists <- function(models, directory, connection_provider = connect_to_ros) {
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  code_list_tables <- unique(unlist(lapply(models, function(x) { x$checks_model$required_code_lists() })))
  use_connection(connection_provider, function(connection) {
    result <- lapply(code_list_tables, function(code_list_name) {
      gav <- table_location$new(code_list_name)
      table <- load_table(gav$schema(), gav$table(), columns = NULL, connection)
      file <- file.path(directory, paste0(code_list_name, ".csv"))
      write_file(table, file)
      table
    })
    names(result) <- code_list_tables
    result
  })
}

compute_vessel_names_hash <- function(vessel_table) {
  result <- vessel_table[, full_name_hash := unlist(lapply(vessel_name, compute_full_name_hash))]
  setorder(result, id)
  result
}

load_ros_databse_registries <- function(models, directory, connection_provider = connect_to_ros) {
  if (!file.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
  data_tables <- unique(unlist(lapply(models, function(x) { x$checks_model$required_data_tables(c("ros_meta.vessel", "ros_meta.contact", "ros_meta.observer_identifier_mapping", "ros_meta.focal_point")) })))
  use_connection(connection_provider, function(connection) {
    result <- lapply(data_tables, function(data_table) {
      gav <- table_location$new(data_table)
      table <- load_table(gav$schema(), gav$table(), columns = NULL, connection)
      table
    })
    names(result) <- data_tables
    # Add full_name_hash column on ros_meta.contact
    contact_table <- result$ros_meta.contact
    compute_full_names_hash(contact_table)
    # Denormalize ros_meta.focal_point
    result$ros_meta.focal_point <- contact_table[result$ros_meta.focal_point, on = .(id = contact_id)][, .(id, full_name, nationality_code, email, organisation_name, comment, full_name_hash)]
    # Denormalize ros_meta.observer
    result$ros_meta.observer <- contact_table[result$ros_meta.observer, on = .(id = contact_id)][, .(id, full_name, nationality_code, iotc_observer_identifier, national_observer_id, full_name_hash)]
    # Denormalize ros_meta.ros_meta.observer_identifier_mapping
    result$ros_meta.observer_identifier_mapping <- result$ros_meta.observer_identifier_mapping[result$ros_meta.observer, on = .(iotc_observer_identifier = iotc_observer_identifier)][!is.na(legacy_iotc_observer_identifier)][, .(legacy_iotc_observer_identifier, iotc_observer_identifier, id)]
    # Add full_name_hash column on ros_meta.vessel
    vessel_table <- result$ros_meta.vessel
    compute_vessel_names_hash(vessel_table)
    for (data_table in data_tables) {
      file <- file.path(directory, paste0(data_table, ".csv"))
      write_file(result[[data_table]], file)
    }
    result
  })
}


ros_cache <- R6Class(
  "RosCache",
  public = list(
    initialize = function(models, code_lists, data_registries) {
      stopifnot(!is.null(models))
      stopifnot(!is.null(code_lists))
      stopifnot(!is.null(data_registries))
      extra_mapping_model <- models$extra_mapping_model
      .data_table_caches <- lapply(names(data_registries), function(x) {
        t <- table_location$new(x)
        cache <- data_table_cache$new(t, data_registries[[x]])
        mapping <- extra_mapping_model[[x]]
        if (!is.null(mapping)) {
          for (i in names(mapping)) {
            cache$set_extra_simple_column_mapping(i, mapping[[i]])
          }
        }
        cache
      })
      names(.data_table_caches) <- names(data_registries)
      # add custom extra mapping
      observer_identifier_mapping_table <- .data_table_caches$ros_meta.observer_identifier_mapping
      observer_table <- .data_table_caches$ros_meta.observer
      observer_table$set_extra_column_mapping("iotc_observer_identifier", function(cache, column, value) {
        observer_identifier_mapping_table$find("legacy_iotc_observer_identifier", value)
      })
      contact_table <- .data_table_caches$ros_meta.contact
      focal_point_table <- .data_table_caches$ros_meta.focal_point
      vessel_table <- .data_table_caches$ros_meta.vessel

      find_full_name <- function(cache, column, value) {
        # transform value to be upper case and with no accent
        if (is.null(value)) {
          return(NULL)
        }
        value2 <- compute_full_name_hash(value)
        cache$find0("full_name_hash", value2)
      }

      contact_table$set_extra_column_mapping("full_name", find_full_name)
      observer_table$set_extra_column_mapping("full_name", find_full_name)
      focal_point_table$set_extra_column_mapping("full_name", find_full_name)
      vessel_table$set_extra_column_mapping("vessel_name", find_full_name)
      private$.data_registries <- .data_table_caches

      .code_list_caches <- lapply(names(code_lists), function(x) {
        t <- table_location$new(x)
        values <- code_lists[[x]]
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
      private$.code_lists <- .code_list_caches
      names(private$.code_lists) <- names(code_lists)
    },
    code_lists = function() {
      private$.code_lists
    },
    data_registries = function() {
      private$.data_registries
    },
    code_list = function(table_location) {
      stopifnot(table_location %in% names(private$.code_lists))
      private$.code_lists[[table_location]]
    },
    data_registry = function(table_location) {
      stopifnot(table_location %in% names(private$.data_registries))
      private$.data_registries[[table_location]]
    },
    contains_code = function(table_location, code) {
      code_list <- self$code_list(table_location)
      code_list$contains_code(code_list, code)
    },
    get_registry = function(table_location, column, value) {
      data_registry <- self$data_registry(table_location)
      data_registry$find(column, value)
    }),
  private = list(
    # cache of code lists
    .code_lists = NULL,
    # cache of ros registires
    .data_registries = NULL
  )
)

create_ros_cache <- function(models, directory) {
  code_lists <- load_ros_databse_code_lists(list(models), file.path(directory, "code_lists"))
  data_registries <- load_ros_databse_registries(list(models), file.path(directory, "registries"))
  ros_cache <- ros_cache$new(models, code_lists, data_registries)
}

create_ros_cache(LL_LATEST_MODEL, "../iotc-ros-input-data/build/ros")
