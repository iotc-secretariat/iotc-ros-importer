library(DBI)
library(RPostgres)
library(data.table)
library(stringr)
library(R6)
library(lubridate)
library(R.oo)

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
        private$.code_orig_mapping <- as.list(values[!is.null(code_orig), .(code_orig, code)][, code_orig := trim(code_orig)])
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
      code %in% self$codes()
    },
    contains_code0 = function(code) {
      if (self$with_code_orig()) {
        if (code %in% self$code_orig_mapping()$code_orig) {
          return(TRUE)
        }
      }
      code %in% self$codes()
    },
    set_extra_simple_column_mapping = function(column, mappings) {
      private$.extra_simple_column_mapping[[column]] <- mappings
    },
    print = function(prefix = "") {
      cat(prefix, " code-list ( ", self$table_location()$gav(), " ) - codes: ( ", length(self$codes()), " ) with code orig (", self$with_code_orig(), ")", sep = "")
      if (self$with_code_orig()) {
        cat(" code-orig-list ( ", length(self$code_orig_mapping()), " )\n", sep = "")
      }
      invisible(self)
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
code_list_caches <- R6Class(
  "CodeListCaches",
  public = list(
    initialize = function(caches) {
      stopifnot(!is.null(caches))
      private$.caches <- caches
    },
    caches = function() {
      private$.caches
    },
    cache = function(table_location) {
      stopifnot(table_location %in% names(private$.caches))
      private$.caches[[table_location]]
    },
    get_code = function(table_location, code) {
      cache <- self$cache(table_location)
      cache$get_code(code)
    },
    contains_code = function(table_location, code) {
      cache <- self$cache(table_location)
      cache$contains_code(code)
    }
  ),
  private = list(
    # all caches
    .caches = NULL
  )
)

data_table_cache <- R6Class(
  "DataTableCache",
  public = list(
    initialize = function(column_location, values) {
      stopifnot(!is.null(table_location))
      stopifnot(!is.null(values))
      private$.table_location <- column_location$table()
      private$.values <- values
      # private$.ids <- as.list(values[, column_location$column()])
    },
    table_location = function() {
      private$.table_location
    },
    values = function() {
      private$.values
    },
    find = function(column, value) {
      simple_mapping <- private$.extra_simple_column_mapping[[column]]
      if (!is.null(simple_mapping)) {
        simple_value <- simple_mapping[[value]]
        if (!is.null(simple_value)) {
          return(self$find(column, simple_value))
        }
      }
      extra_mapping <- private$.extra_column_mapping[[column]]
      if (!is.null(extra_mapping)) {
        mapped_value <- extra_mapping(self, column, value)
        if (!is.null(mapped_value)) {
          return(mapped_value)
        }
      }
      self$find0(column, value)
    },
    find0 = function(column, value) {
      self$values()[self$values()[[column]] == value]
    },
    ids = function() {
      private$.ids
    },
    contains_id = function(id) {
      code %in% self$ids()
    },
    set_extra_column_mapping = function(column, mapping_function) {
      private$.extra_column_mapping[[column]] <- mapping_function
    },
    set_extra_simple_column_mapping = function(column, mappings) {
      private$.extra_simple_column_mapping[[column]] <- mappings
    },
    print = function(prefix = "") {
      cat(prefix, " data-table-cache ( ", self$table_location()$gav(), " ) - ids: ( ", length(self$ids()), ")", sep = "")
      invisible(self)
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

data_table_caches <- R6Class(
  "DataTableCaches",
  public = list(
    initialize = function(caches) {
      stopifnot(!is.null(caches))
      private$.caches <- caches
    },
    caches = function() {
      private$.caches
    },
    cache = function(table_location) {
      stopifnot(table_location %in% names(private$.caches))
      private$.caches[[table_location]]
    },
    contains_id = function(table_location, id) {
      cache <- self$cache(table_location)
      cache$contains_id(id)
    }
  ),
  private = list(
    # all caches
    .caches = NULL
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
