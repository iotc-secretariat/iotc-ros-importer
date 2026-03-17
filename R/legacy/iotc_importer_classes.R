# importing the required library
library(R6)
library(openxlsx)
library(RPostgres)
library(data.table)
library(stringi)
library(stringr)
library(R.oo)
library(lubridate)
# library(iotc.base.common.data)

DEBUG <- FALSE

AbstractColumn <- R6Class(
  "AbstractColumn",
  public = list(
    initialize = function(name, mandatory = FALSE, comment = NA, checks = NA, actions = NA) {
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(mandatory), is.logical(mandatory))
      private$.name <- name
      private$.mandatory <- mandatory
      private$.comment <- comment
      private$.actions <- actions
      private$.checks <- checks
    },
    name = function() {
      private$.name
    },
    mandatory = function() {
      private$.mandatory
    },
    comment = function() {
      private$.comment
    },
    checks = function() {
      private$.checks
    },
    actions = function() {
      private$.actions
    },
    collect_data_tables = function() {
    },
    collect_code_list_tables = function() {
    }
  ),
  private = list(
    # column name
    .name = NULL,
    # optional comment
    .comment = NULL,
    # is column mandatory
    .mandatory = NULL,
    # optional actions
    .actions = NULL,
    # optional checks
    .checks = NULL,
    .print = function(prefix = "", name) {
      cat(prefix, name, ": ", self$name(), sep = "")
      if (self$mandatory()) {
        cat(" (Mandatory)", sep = "")
      } else {
        cat(" (Optional)", sep = "")
      }
      if (length(self$actions()) == 1 && is.na(self$actions())) {

      } else if (!is.null(self$actions())) {
        cat(" - actions: [")
        i <- 1
        m <- length(self$actions())
        for (action in self$actions()) {
          action$print()
          if (i < m) {
            cat(", ")
          }
          i <- i + 1
        }
        cat(" ]")
      }
    }
  )
)

AbstractColumnWithColumnLocation <- R6Class(
  "AbstractColumnWithColumnLocation",
  inherit = AbstractColumn,
  public = list(
    initialize = function(name, mandatory = FALSE, column_location, comment = NA, checks = NA, actions = NA) {
      super$initialize(name, mandatory, comment, checks, actions)
      stopifnot(!rlang::is_empty(column_location))
      private$.column_location <- column_location
    },
    column_location = function() {
      private$.column_location
    },
    data_row = function() {
      private$.data_row
    },
    init_data_row = function(import_context, value) {
      location <- self$column_location()
      private$.data_row <- import_context$find_data_in_cache(location$table()$gav(), location$column(), value)
    },
    collect_data_tables = function() {
      result <- c()
      for (a in self$actions()) {
        if (is_new_query(a)) {
          if (a$with_table_location()) {
            result[length(c)] <- a$table_location()
          } else {
            result[length(c)] <- self$column_location()$table()$gav()
          }
        }
      }
      result
    }
  ),
  private = list(
    # column location
    .column_location = NULL,
    # optional data row if column location is filled
    .data_row = NULL,
    .print = function(prefix = "", name) {
      super$.print(prefix, name)
      cat(" - location: ( ")
      self$column_location()$print()
      cat(" )")
    }
  )
)

IgnoredColumn <- R6Class(
  "IgnoredColumn",
  inherit = AbstractColumn,
  public = list(
    initialize = function(name, comment = NA) {
      if (DEBUG) {
        cat("> IgnoredColumn:", name, "\n")
      }
      super$initialize(name, FALSE, comment)
    },
    print = function(prefix = "") {
      super$.print(prefix, "IgnoredColumn")
      cat("\n")
      invisible(self)
    }
  )
)

SimpleColumn <- R6Class(
  "SimpleColumn",
  inherit = AbstractColumnWithColumnLocation,
  public = list(
    initialize = function(name, mandatory = FALSE, column_location, comment = NA, checks = NA, actions) {
      if (DEBUG) {
        cat("> SimpleColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment, checks, actions)
    },
    print = function(prefix = "") {
      super$.print(prefix, "SimpleColumn")
      cat("\n", sep = "")
      invisible(self)
    }
  )
)

ForeignKeyColumn <- R6Class(
  "ForeignKeyColumn",
  inherit = AbstractColumnWithColumnLocation,
  public = list(
    initialize = function(name,
                          mandatory = FALSE,
                          column_location,
                          foreign_column_location,
                          comment = NA,
                          checks = NA,
                          actions) {
      if (DEBUG) {
        cat("> ForeignKeyColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment, checks, actions)
      stopifnot(!rlang::is_empty(foreign_column_location))
      private$.foreign_column_location <- foreign_column_location
    },
    foreign_column_location = function() {
      private$.foreign_column_location
    },
    collect_code_list_tables = function() {
      self$foreign_column_location()$table()$gav()
    },
    print = function(prefix = "") {
      super$.print(prefix, "ForeignKeyColumn")
      cat(" - foreign-key: ( ", sep = "")
      self$foreign_column_location()$print()
      cat(" )\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    # foreign column location
    .foreign_column_location = NULL
  )
)

MeasurementValueColumn <- R6Class(
  "MeasurementValueColumn",
  inherit = AbstractColumnWithColumnLocation,
  public = list(
    initialize = function(name, mandatory = FALSE, column_location, measurement_table, unit = NA, comment = NA, checks = NA, actions) {
      if (DEBUG) {
        cat("> MeasurementValueColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment, checks, actions)
      stopifnot(!is.na(measurement_table),
                is.character(measurement_table), nchar(measurement_table) > 0)
      if (!is.na(unit)) {
        stopifnot(nchar(unit) > 0)
      }
      private$.measurement_table <- measurement_table
      private$.unit <- unit
    },
    measurement_table = function(x) {
      private$.measurement_table
    },
    unit = function() {
      private$.unit
    },
    print = function(prefix = "") {
      super$.print(prefix, "MeasurementValueColumn")
      cat(" - measurement_table: (", self$measurement_table(), ")", sper = "")
      if (!is.na(self$unit())) {
        cat(" - unit: (", self$unit(), ")", sper = "")
      }
      cat("\n")
      invisible(self)
    }
  ),
  private = list(
    # measurement table
    .measurement_table = NULL,
    # optional unit
    .unit = NULL
  )
)

MeasurementUnitColumn <- R6Class(
  "MeasurementUnitColumn",
  inherit = AbstractColumn,
  public = list(
    initialize = function(name, mandatory = FALSE, units, checks = NA, actions) {
      if (DEBUG) {
        cat("> MeasurementUnitColumn:", name, "\n")
      }
      super$initialize(name, mandatory, checks, actions)
      stopifnot(!is.na(units), is.vector(units), length(units) > 0)
      private$.units <- units
    },
    units = function() {
      private$.units
    },
    accept_unit = function(unit) {
      unit %in% self$units()
    },
    print = function(prefix = "") {
      super$.print(prefix, "MeasurementUnitColumn")
      cat(" - units: (", as.character(self$units()), ")\n", sper = "")
      invisible(self)
    }
  ),
  private = list(
    # possible units
    .units = NULL
  )
)

MetaCell <- R6Class(
  "MetaCell",
  public = list(
    initialize = function(name, mandatory, column, row, expected = NULL, format = NULL, column_location = NULL) {
      if (DEBUG) {
        cat("MetaCell:", name, " - mandatory:", mandatory, " at ", column, ":", row, "\n")
      }
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(mandatory))
      stopifnot(!is.na(column), is.numeric(column))
      stopifnot(!is.na(row), is.numeric(row))
      private$.name <- name
      private$.mandatory <- mandatory
      private$.column <- column
      private$.row <- row
      private$.expected <- expected
      private$.format <- format
      private$.column_location <- column_location
    },
    name = function() {
      private$.name
    },
    mandatory = function() {
      private$.mandatory
    },
    column = function() {
      private$.column
    },
    row = function() {
      private$.row
    },
    column_location = function() {
      private$.column_location
    },
    init_value = function(meta_sheet, i_context) {
      tryCatch(private$.value <- meta_sheet[[self$row()]][[self$column()]], error = function(e) {
        i_context$add_errors(list(paste0("Could not init meta value ", self$name(), ", error: ", e)))
      })
    },
    check_value = function(import_context) {
      result <- list()
      is_na <- is.na(self$value()) || is.null(self$value())
      if (self$mandatory() && is_na) {
        result <- append(result, paste("Meta:", self$name(), "is mandatory and no value found in META sheet"))
      }
      name <- self$name()
      value <- self$value()
      if (!is_na) {
        # Not null value
        if (!is.null(self$expected())) {
          # With expected value, let's check it
          if (self$expected() != value) {
            result <- append(result, paste0("Meta:", name, " expected  value is `", self$expected(), "` but current value is `", value, "`"))
          }
        } else if (!is.null(self$format())) {
          # With format, let's check it
          if (!grepl(self$format(), value)) {
            result <- append(result, paste0("Meta:", name, " should respect format `", self$format(), "` but current value is `", value, "`"))
          }
        }
        location <- self$column_location()
        if (!is.null(location)) {
          # check if value exists in database
          import_context$log_info(paste0("--- Checking ", name, " with value `", value, "` exists at location ", location$gav(), "..."))
          private$.data_row <- import_context$find_data_in_cache(location$table()$gav(), location$column(), value)
          if (is.null(private$.data_row)) {
            result <- append(result, paste0("Could not find ", name, " with value (", value, ") in the database (", location$gav(), ")"))
          }
        }
      }
      if (length(result) == 0) {
        import_context$log_info(paste0("Meta:", name, " value `", value, "` is correct."))
      }
      result
    },
    value = function() {
      private$.value
    },
    data_row = function() {
      private$.data_row
    },
    expected = function() {
      private$.expected
    },
    format = function() {
      private$.format
    },
    print = function(prefix = "") {
      cat(prefix, "Meta: ", self$name(), " - mandatory:", self$mandatory(), " at ", self$column(), ":", self$row(), " - value:", self$value(), "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    # name of the meta
    .name = NULL,
    # is meta mandatory?
    .mandatory = NULL,
    # column number
    .column = NULL,
    # row number
    .row = NULL,
    # value from the import file
    .value = NULL,
    # optional expected value
    .expected = NULL,
    # optional format of the value (as a regular expression)
    .format = NULL,
    # optional column location
    .column_location = NULL,
    # optional data row if column location is filled
    .data_row = NULL
  )
)

AbstractMetaSheet <- R6Class(
  "AbstractMetaSheet",
  public = list(
    initialize = function(name, cells) {
      if (DEBUG) {
        cat("AbstractMetaSheet:", name, "\n")
      }
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(cells), is.vector(cells), length(cells) > 0)
      private$.name <- name
      private$.cells <- cells
      names(private$.cells) <- self$cells_names()
    },
    name = function() {
      private$.name
    },
    cells = function() {
      private$.cells
    },
    cells_names = function() {
      lapply(self$cells(),
             function(x) {
               x$name()
             })
    },
    init_values = function(meta_sheet, i_context) {
      for (c in self$cells()) {
        c$init_value(meta_sheet, i_context)
      }
    },
    check_values = function(import_context) {
      result <- list()
      for (c in self$cells()) {
        r <- c$check_value(import_context)
        if (length(r) > 0) {
          result <- append(result, r)
        }
      }
      result
    },
    print = function(prefix = "") {
      cat(prefix, "AbstractMetaSheet: ", self$name(), " with ", length(self$cells()), " cell(s)\n", sep = "")
      for (c in self$cells()) {
        c$print(paste(prefix, " "))
      }
      invisible(self)
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # meta cells
    .cells = NULL
  )
)

Sheet <- R6Class(
  "Sheet",
  public = list(
    initialize = function(name, comment, pk, columns) {
      if (DEBUG) {
        cat("Sheet:", name, "\n")
      }
      stopifnot(!is.na(comment), is.character(comment), nchar(comment) > 0)
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(columns), is.vector(columns), length(columns) > 0)
      private$.name <- name
      private$.columns <- columns
      names(private$.columns) <- self$column_names()
      private$.comment <- comment
      private$.pk <- private$.columns[seq(1, pk)]
    },
    name = function() {
      private$.name
    },
    comment = function() {
      private$.comment
    },
    columns = function() {
      private$.columns
    },
    pk = function() {
      private$.pk
    },
    column_names = function() {
      lapply(self$columns(),
             function(x) {
               x$name()
             })
    },
    collect_data_tables = function() {
      unlist(lapply(self$columns(), function(c) {
        c$collect_data_tables()
      }))
    },
    collect_code_list_tables = function() {
      unlist(unique(lapply(self$columns(), function(c) {
        c$collect_code_list_tables()
      })))
    },
    print = function(prefix = "") {
      cat(prefix, "Sheet: ", self$name(), " with ", length(self$columns()), " column(s)\n", sep = "")
      for (c in self$columns()) {
        c$print(paste(prefix, " "))
      }
      invisible(self)
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # comment
    .comment = NULL,
    # columns of the sheet
    .columns = NULL,
    # pk columns
    .pk = NULL
  )
)

ImportFile <- R6Class(
  "ImportFile",
  public = list(
    initialize = function(name, meta_sheet, sheets) {
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.null(meta_sheet))
      stopifnot(!is.na(sheets), is.vector(sheets), length(sheets) > 0)
      private$.name <- name
      private$.meta_sheet <- meta_sheet
      private$.sheets <- sheets
      names(private$.sheets) <- self$sheet_names()
    },
    name = function() {
      private$.name
    },
    meta_sheet = function() {
      private$.meta_sheet
    },
    sheets = function() {
      private$.sheets
    },
    print = function() {
      cat("ImportFile: ", self$name(), " with ", length(self$sheets()), " sheet(s)\n", sep = "")
      for (c in self$sheets()) {
        c$print("  ")
      }
      invisible(self)
    },
    sheet_names = function() {
      lapply(self$sheets(),
             function(x) {
               x$name()
             })
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

AbstractCheck <- R6Class(
  "AbstractCheck",
  public = list(
    initialize = function() {
    }
  ),
  private = list(
    .print = function(prefix) {
      cat(prefix, class(self)[[1]])
    }
  )
)

AbstractColumnAction <- R6Class(
  "AbstractColumnAction",
  public = list(
    initialize = function() {
    }
  ),
  private = list(
    .print = function(prefix) {
      cat(prefix, class(self)[[1]])
    }
  )
)

NewQuery <- R6Class(
  "NewQueryColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function(table_location = NA) {
      if (!is.na(table_location)) {
        private$.table_location <- table_location
      }
    },
    table_location = function() {
      private$.table_location
    },
    with_table_location = function() {
      !is.null(self$table_location())
    },
    print = function(prefix = "") {
      super$.print(prefix)
      if (self$with_table_location()) {
        cat(" - table location: ( ", self$table_location(), ")", sep = "")
      }
      invisible(self)
    }
  ),
  private = list(
    # optional table location, if not set will use the table location of the associated column
    .table_location = NULL
  )
)

NewMeasurementQuery <- R6Class(
  "NewMeasurementQueryColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function() {
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)

NewAssociationQuery <- R6Class(
  "NewAssociationQueryColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function(table_location, column_location) {
      stopifnot(!is.null(column_location))
      stopifnot(!is.na(table_location))
      private$.column_location <- column_location
      private$.table_location <- table_location
    },
    column_location = function() {
      private$.column_location
    },
    table_location = function() {
      private$.table_location
    },
    print = function(prefix = "") {
      super$.print(prefix)
      cat(prefix, " from ( ", self$table_location(), " ) to ( ", sep = "")
      self$column_location()$print()
      cat(" )")
      invisible(self)
    }
  ),
  private = list(
    # association table
    .table_location = NULL,
    # column location to set as foreign key
    .column_location = NULL
  )
)
FlushQuery <- R6Class(
  "FlushQueryColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function(table_location = NA) {
      if (!is.na(table_location)) {
        private$.table_location <- table_location
      }
    },
    table_location = function() {
      private$.table_location
    },
    print = function(prefix = "") {
      super$.print(prefix)
      if (!is.null(self$table_location())) {
        cat(" - table location: ( ", self$table_location(), ")", sep = "")
      }
      invisible(self)
    }
  ),
  private = list(
    # optional table location, if not set will use the table location of the associated column
    .table_location = NULL
  )
)

FlushMeasurementQuery <- R6Class(
  "FlushMeasurementQueryColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function() {
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)

FlushAssociationQuery <- R6Class(
  "FlushAssociationQueryColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function() {
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)

CheckExists <- R6Class(
  "CheckExists",
  inherit = AbstractCheck,
  public = list(
    initialize = function() {
    },
    check = function(import_context, sheet, column, value) {
      cat(paste0("Check exists: ", column$name(), "with value `", value, "`"))
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)

CheckShouldExists <- R6Class(
  "CheckShouldExists",
  inherit = AbstractCheck,
  public = list(
    initialize = function() {
    },
    check = function(import_context, sheet, column, value) {
      cat(paste0("Check should exists: ", column$name(), "with value `", value, "`"))
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)
GetId <- R6Class(
  "GetIdAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function() {
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)

AddColumn <- R6Class(
  "AddColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function() {
    },
    print = function(prefix = "") {
      super$.print(prefix)
      invisible(self)
    }
  )
)

AddForeignKeyColumn <- R6Class(
  "AddForeignKeyColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function(column_location) {
      stopifnot(!is.null(column_location))
      private$.column_location <- column_location
    },
    column_location = function() {
      private$.column_location
    },
    print = function(prefix = "") {
      super$.print(prefix)
      cat(" on ( ")
      self$column_location()$print()
      cat(" )")
      invisible(self)
    }
  ),
  private = list(
    # column location to set as foreign key
    .column_location = NULL
  )
)

AddExternalForeignKeyColumn <- R6Class(
  "AddExternalForeignKeyColumnAction",
  inherit = AbstractColumnAction,
  public = list(
    initialize = function(table_location, column_location) {
      stopifnot(!is.null(column_location))
      stopifnot(!is.na(table_location))
      private$.column_location <- column_location
      private$.table_location <- table_location
    },
    column_location = function() {
      private$.column_location
    },
    table_location = function() {
      private$.table_location
    },
    print = function(prefix = "") {
      super$.print(prefix)
      cat(prefix, " from ( ", self$table_location(), " ) to ( ", sep = "")
      self$column_location()$print()
      cat(" )")
      invisible(self)
    }
  ),
  private = list(
    # external table
    .table_location = NULL,
    # column location to set as foreign key
    .column_location = NULL
  )
)


CodeListCache <- R6Class(
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
      if (self$with_code_orig()) {
        if (code %in% self$code_orig_mapping()$code_orig) {
          return(TRUE)
        }
      }
      code %in% self$codes()
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
    .code_orig_mapping = NULL
  )
)

DataTableCache <- R6Class(
  "DataTableCache",
  public = list(
    initialize = function(column_location, values) {
      stopifnot(!is.null(table_location))
      stopifnot(!is.null(values))
      private$.table_location <- column_location$table()
      private$.values <- values
      private$.ids <- as.list(values[, column_location$column()])
    },
    table_location = function() {
      private$.table_location
    },
    values = function() {
      private$.values
    },
    find = function(column, value) {
      extra_mapping <- private$.extra_column_mapping[column]
      if (!is.null(extra_mapping)) {
        extra_mapping <- extra_mapping[[1]]
        mapped_value <- extra_mapping[old == value, ]
        if (get_row_count(mapped_value) == 1) {
          return(self$values()[self$values()[[column]] == mapped_value$new])
        }
      }
      self$values()[self$values()[[column]] == value]
    },
    ids = function() {
      private$.ids
    },
    contains_id = function(id) {
      code %in% self$ids()
    },
    set_extra_column_mapping = function(column, mapping) {
      private$.extra_column_mapping[column] <- list(mapping)
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
    .extra_column_mapping = c()
  )
)

DataTableCaches <- R6Class(
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
DataIdCache <- R6Class(
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

CodeListCaches <- R6Class(
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

ImportContext <- R6Class(
  "ImportContext",
  public = list(
    initialize = function(model, file, connection, extra_data_tables, data_table_caches, timestamp) {
      stopifnot(!is.null(model))
      stopifnot(!is.null(connection))
      stopifnot(!is.null(file))
      private$.model <- model
      private$.log_file <- paste0(file, "-", timestamp, ".log")
      self$log_info(paste0("Loading input data file: ", file))
      if (file.exists(private$.log_file)) {
        file.remove(private$.log_file)
      }
      data_table_caches_names <- lapply(data_table_caches, function(x) {
        column_location(x)$table()$gav()
      })
      .data_table_caches <- lapply(data_table_caches, function(x) {
        t <- column_location(x)
        values <- load_table(t$table()$schema(), t$table()$table(), columns = NULL, connection)
        data_table_cache(t, values)
      })
      names(.data_table_caches) <- data_table_caches_names
      private$.data_table_caches <- data_table_caches(.data_table_caches)
      private$.xls_data <- load_xls(model, file)
      code_list_tables <- private$.collect_code_list_tables()
      .code_list_caches <- lapply(code_list_tables, function(x) {
        t <- table_location(x)
        values <- load_table(t$schema(), t$table(), columns = NULL, connection)
        setorder(values, code)
        code_list_cache(x, values)
      })
      names(.code_list_caches) <- code_list_tables
      private$.code_list_caches <- code_list_caches(.code_list_caches)
      data_tables <- private$.collect_data_tables()
      data_tables <- append(data_tables, extra_data_tables)
      .data_table_ids <- lapply(data_tables, function(x) {
        t <- table_location(x)
        query <- paste0("SELECT t.last_value FROM pg_catalog.pg_sequences t WHERE t.schemaname = '", t$schema(), "' AND sequencename ='", t$table(), "_id_seq'")
        result_set <- dbSendQuery(connection, query)
        id <- as.integer(dbFetch(result_set)[["last_value"]])
        dbClearResult(result_set)
        id
      })
      names(.data_table_ids) <- data_tables
      private$.data_table_ids <- data_id_cache(.data_table_ids)
    },
    model = function() {
      private$.model
    },
    log_file = function() {
      private$.log_file
    },
    xls_data = function() {
      private$.xls_data
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
      private$.model$sheet_names()
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
    check_before_import = function(check_meta_submission = TRUE,
                                   check_meta_general = TRUE,
                                   check_code_lists = TRUE,
                                   check_mandatory = TRUE,
                                   check_exists = TRUE,
                                   check_measurements_units = TRUE) {
      if (check_meta_submission) {
        result <- self$check_meta_submission()
        if (length(result) > 0) {
          self$add_errors(result)
        }
      }
      if (check_meta_general) {
        result <- self$check_meta_general()
        if (length(result) > 0) {
          self$add_errors(result)
        }
      }
      if (check_code_lists) {
        result <- self$check_code_lists()
        if (length(result) > 0) {
          self$add_errors(result)
        }
      }
      if (check_mandatory) {
        result <- self$check_mandatory()
        if (length(result) > 0) {
          self$add_errors(result)
        }
      }
      if (check_exists) {
        result <- self$check_exists()
        if (length(result) > 0) {
          self$add_errors(result)
        }
        result <- self$check_should_exists()
        if (length(result) > 0) {
          self$add_warnings(result)
        }
      }
      if (check_measurements_units) {
        result <- self$check_measurements_units()
        if (length(result) > 0) {
          self$add_errors(result)
        }
      }
      warnings_count <- self$check_warnings_count()
      if (warnings_count > 0) {
        message <- paste0("Please fix all the ", warnings_count, " warning(s) listed above.")
        self$log_warning(message)
      }
      errors_count <- self$check_errors_count()
      if (errors_count > 0) {
        message <- paste0("Please fix all the ", errors_count, " error(s) listed above.")
        self$log_warning(message)
        throw(message)
      }
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
    },
    check_meta_submission = function() {
      result <- list()
      model <- self$model()
      metas <- model$meta_sheet()
      self$log_info("--- Checking Meta Submission Information...")
      r <- metas$check_values(self)
      if (length(r) > 0) {
        result <- append(result, r)
      }
      full_name_id <- metas$cells()$liaison_officer_full_name$data_row()
      with_full_name_id <- !is.null(full_name_id)
      email_id <- metas$cells()$liaison_officer_email$data_row()
      with_email_id <- !is.null(email_id)
      if (with_full_name_id &&
        with_email_id &&
        email_id$id != full_name_id$id) {
        result <- append(result, paste0("Mismatch liaison officer from his email (", liaison_officer_email, "- id: ", email_id$id, ") and his full name (", liaison_officer_full_name, " - id:", full_name_id$id, ") in the database (table ros_common.iotc_person_contact_details)"))
      }
      result
    },
    check_meta_general = function() {
      result <- list()
      model <- self$model()
      metas <- model$meta_sheet()
      self$log_info("--- Checking Meta General Information...(TODO)")
      result
    },
    check_code_lists = function() {
      result <- list()
      model <- self$model()
      metas <- model$meta_sheet()
      self$log_info("--- Checking Code lists references...")
      code_list_caches <- self$code_list_caches()
      all_missing_codes <- c()
      xls_data <- self$xls_data()
      for (model_sheet in model$sheets()) {
        sheet_name <- model_sheet$name()
        for (column in model_sheet$columns()) {
          if (is_fk_column(column)) {
            column_name <- column$name()
            data <- xls_data[[model_sheet$name()]]
            if (!column_name %in% names(data)) {
              break
            }
            data <- data |> subset(select = column_name)
            code_list_table_gav <- column$foreign_column_location()$table()$gav()
            code_list_cache <- code_list_caches$caches()[[code_list_table_gav]]
            # cat("--- Checking Code list references on sheet:", sheet_name, "for column", column_name, "on code list table", code_list_table_gav, "...\n")
            row_number <- 5
            height <- dim(data)[1]
            for (i in seq_len(height)) {
              value <- data[i]
              if (!is.na(value)) {
                value <- as.character(value)
                if (!code_list_cache$contains_code(value)) {
                  # if (DEBUG) {
                  result <- append(result, paste0("On ", sheet_name, ".", column_name, "[", (row_number + i), "] value `", value, "` is not found in the code list `", code_list_table_gav, "`"))
                  # }
                  missing_codes <- all_missing_codes[[code_list_table_gav]]
                  if (is.null(missing_codes)) {
                    missing_codes <- list()
                  }
                  if (!value %in% missing_codes) {
                    missing_codes <- append(missing_codes, value)
                    all_missing_codes[code_list_table_gav] <- list(missing_codes)
                  }
                }
              }
            }
          }
        }
      }
      if (length(all_missing_codes) > 0) {
        message <- "Missing codes:"
        for (i in names(all_missing_codes)) {
          message <- paste(message, "\n", i, all_missing_codes[i])
        }
        print(message)
        result <- append(result, message)
      }
      result
    },
    check_mandatory = function() {
      result <- list()
      model <- self$model()
      metas <- model$meta_sheet()
      xls_data <- self$xls_data()
      self$log_info("--- Checking mandatory...")
      for (model_sheet in model$sheets()) {
        sheet_name <- model_sheet$name()
        for (column in model_sheet$columns()) {
          if (column$mandatory()) {
            column_name <- column$name()
            # cat("--- Checking mandatory property on sheet:", sheet_name, "for column", column_name, "...\n")
            data <- xls_data[[model_sheet$name()]]
            if (!column_name %in% names(data)) {
              break
            }
            data <- data |> subset(select = column_name)
            row_number <- 5
            height <- dim(data)[1]
            for (i in seq_len(height)) {
              value <- data[i]
              if (is.na(value)) {
                result <- append(result, paste0("On ", sheet_name, ".", column_name, "[", (row_number + i), "] value is missing."))
              }
            }
          }
        }
      }
      result
    },
    check_exists = function() {
      result <- list()
      model <- self$model()
      xls_data <- self$xls_data()
      self$log_info("--- Checking exists...")
      all_missing_codes <- c()
      for (model_sheet in model$sheets()) {
        sheet_name <- model_sheet$name()
        for (column in model_sheet$columns()) {
          checks <- column$checks()
          if (is.null(checks)) {
            next
          }
          for (check in checks) {
            if (is_check_exists(check)) {
              column_name <- column$name()
              # cat("--- Checking exists property on sheet:", sheet_name, "for column", column_name, "...\n")
              data <- xls_data[[model_sheet$name()]]
              if (!column_name %in% names(data)) {
                next
              }
              data <- data |> subset(select = column_name)
              row_number <- 5
              height <- dim(data)[1]
              for (i in seq_len(height)) {
                value <- data[i]
                if (is.na(value)) {
                  next
                }
                value <- value[[1]]
                location <- column$column_location()
                code_list_table_gav <- location$gav()
                # self$log_info(paste0("--- Checking ", column_name, " with value `", value, "` exists at location ", location$gav(), "..."))
                .data_row <- self$find_data_in_cache(location$table()$gav(), location$column(), value)
                if (is.null(.data_row)) {
                  result <- append(result, paste0("On ", sheet_name, ".", column_name, "[", (row_number + i), "] value `", value, "` does not exists in database at location: ", location$gav(), "."))
                  missing_codes <- all_missing_codes[[code_list_table_gav]]
                  if (is.null(missing_codes)) {
                    missing_codes <- list()
                  }
                  if (!value %in% missing_codes) {
                    missing_codes <- append(missing_codes, value)
                    all_missing_codes[code_list_table_gav] <- list(missing_codes)
                  }
                }
              }
            }
          }
        }
      }
      if (length(all_missing_codes) > 0) {
        message <- "Missing data in database:"
        for (i in names(all_missing_codes)) {
          message <- paste(message, "\n", i, all_missing_codes[i])
        }
        print(message)
        result <- append(result, message)
      }
      result
    },
    check_should_exists = function() {
      result <- list()
      model <- self$model()
      xls_data <- self$xls_data()
      self$log_info("--- Checking should exists...")
      all_missing_codes <- c()
      for (model_sheet in model$sheets()) {
        sheet_name <- model_sheet$name()
        for (column in model_sheet$columns()) {
          checks <- column$checks()
          if (is.null(checks)) {
            next
          }
          for (check in checks) {
            if (is_check_should_exists(check)) {
              column_name <- column$name()
              # cat("--- Checking sould exists property on sheet:", sheet_name, "for column", column_name, "...\n")
              data <- xls_data[[model_sheet$name()]]
              if (!column_name %in% names(data)) {
                next
              }
              data <- data |> subset(select = column_name)
              row_number <- 5
              height <- dim(data)[1]
              for (i in seq_len(height)) {
                value <- data[i]
                if (is.na(value)) {
                  next
                }
                value <- value[[1]]
                location <- column$column_location()
                code_list_table_gav <- location$gav()
                # self$log_info(paste0("--- Checking ", column_name, " with value `", value, "` should exists at location ", location$gav(), "..."))
                .data_row <- self$find_data_in_cache(location$table()$gav(), location$column(), value)
                if (is.null(.data_row)) {
                  result <- append(result, paste0("On ", sheet_name, ".", column_name, "[", (row_number + i), "] value `", value, "` does not exists in database at location: ", location$gav(), "."))
                  missing_codes <- all_missing_codes[[code_list_table_gav]]
                  if (is.null(missing_codes)) {
                    missing_codes <- list()
                  }
                  if (!value %in% missing_codes) {
                    missing_codes <- append(missing_codes, value)
                    all_missing_codes[code_list_table_gav] <- list(missing_codes)
                  }
                }
              }
            }
          }
        }
      }
      if (length(all_missing_codes) > 0) {
        message <- "Missing data in database:"
        for (i in names(all_missing_codes)) {
          message <- paste(message, "\n", i, all_missing_codes[i])
        }
        print(message)
        result <- append(result, message)
      }
      result
    },
    check_measurements_units = function() {
      result <- list()
      model <- self$model()
      metas <- model$meta_sheet()
      xls_data <- self$xls_data()
      self$log_info("--- Checking measurements unit consistency...")
      for (model_sheet in model$sheets()) {
        sheet_name <- model_sheet$name()
        for (column in model_sheet$columns()) {
          if (is_measurement_unit_column(column)) {
            column_name <- column$name()
            authorized_units <- column$units()
            # cat("--- Checking measurements unit consistency on sheet:", sheet_name, "for column", column_name, "amoung", authorized_units, "...\n")
            data <- xls_data[[model_sheet$name()]]
            if (!column_name %in% names(data)) {
              break
            }
            data <- data |> subset(select = column_name)
            row_number <- 5
            height <- dim(data)[1]
            for (i in seq_len(height)) {
              value <- data[i]
              if (!is.na(value)) {
                if (!column$accept_unit(value)) {
                  result <- append(result, paste0("On ", sheet_name, ".", column_name, "[", (row_number + i), "] value `", value, "` is not among authrized units."))
                }
              }
            }
          }
        }
      }
      result
    }
  ),
  private = list(
    # import model
    .model = NULL,
    # log file
    .log_file = NULL,
    # xls data to import
    .xls_data = NULL,
    # data tables ids
    .data_table_ids = NULL,
    # data table caches
    .data_table_caches = NULL,
    # code list caches
    .code_list_caches = NULL,
    # count of check errors
    .check_errors_count = 0,
    # count of check warnings
    .check_warnings_count = 0,
    .collect_data_tables = function() {
      unique(unlist(lapply(private$.model$sheets(), function(s) {
        s$collect_data_tables()
      })))
    },
    .collect_code_list_tables = function() {
      unique(unlist(lapply(private$.model$sheets(), function(s) {
        s$collect_code_list_tables()
      })))
    }
  )
)

mandatory_meta_cell <- function(name, column, row, expected = NULL, format = NULL, column_location = NULL) {
  MetaCell$new(name, mandatory = TRUE, column, row, expected, format, column_location)
}

optional_meta_cell <- function(name, column, row, expected = NULL, format = NULL, column_location = NULL) {
  MetaCell$new(name, mandatory = FALSE, column, row, expected, format, column_location)
}

ignored_column <- function(name, comment = NA) {
  IgnoredColumn$new(name, comment)
}

optional_simple_column <- function(name, column_location, comment = NA, checks = NA, actions = NA) {
  SimpleColumn$new(name, mandatory = FALSE, column_location, comment, checks, actions)
}

mandatory_simple_column <- function(name, column_location, comment = NA, checks = NA, actions = NA) {
  SimpleColumn$new(name, mandatory = TRUE, column_location, comment, checks, actions)
}

optional_fk_column <- function(name, column_location, foreign_column_location, comment = NA, checks = NA, actions = NA) {
  ForeignKeyColumn$new(name, mandatory = FALSE, column_location, foreign_column_location, comment, checks, actions)
}

mandatory_fk_column <- function(name, column_location, foreign_column_location, comment = NA, checks = NA, actions = NA) {
  ForeignKeyColumn$new(name, mandatory = TRUE, column_location, foreign_column_location, comment, checks, actions)
}

optional_measurement_column <- function(name, column_location, measurement_table, unit = NA, comment = NA, checks = NA, actions = NA) {
  MeasurementValueColumn$new(name, mandatory = FALSE, column_location, measurement_table, unit, comment, checks, actions)
}

mandatory_measurement_column <- function(name, column_location, measurement_table, unit = NA, comment = NA, checks = NA, actions = NA) {
  MeasurementValueColumn$new(name, mandatory = TRUE, column_location, measurement_table, unit, comment, checks, actions)
}

optional_measurement_unit_column <- function(name, units, checks = NA, actions = NA) {
  MeasurementUnitColumn$new(name, mandatory = FALSE, units, checks, actions)
}

mandatory_measurement_unit_column <- function(name, units, checks = NA, actions = NA) {
  MeasurementUnitColumn$new(name, mandatory = TRUE, units, checks, actions)
}

new_query <- function(table_location = NA) {
  NewQuery$new(table_location)
}

new_measurement_query <- function() {
  NewMeasurementQuery$new()
}

new_association_query <- function(table_location, column_location) {
  NewAssociationQuery$new(table_location, column_location)
}

flush_measurement_query <- function() {
  FlushMeasurementQuery$new()
}

flush_association_query <- function() {
  FlushAssociationQuery$new()
}

flush_query <- function(table_location = NA) {
  FlushQuery$new(table_location)
}

add_column <- function() {
  AddColumn$new()
}

get_id <- function() {
  GetId$new()
}

check_exists <- function() {
  CheckExists$new()
}

check_should_exists <- function() {
  CheckShouldExists$new()
}

add_fk_column <- function(column_location) {
  AddForeignKeyColumn$new(column_location)
}

add_external_fk_column <- function(table_location, column_location) {
  AddExternalForeignKeyColumn$new(table_location, column_location)
}

meta_sheet <- function(name, cells) {
  AbstractMetaSheet$new(name, cells)
}

sheet <- function(name, comment, pk, columns) {
  Sheet$new(name, comment, pk, columns)
}

import_file <- function(name, meta_sheet, sheets) {
  ImportFile$new(name, meta_sheet, sheets)
}

import_context <- function(model, file, connection, extra_data_tables, data_tables_caches, timestamp) {
  ImportContext$new(model, file, connection, extra_data_tables, data_tables_caches, timestamp)
}

code_list_cache <- function(table_location, values) {
  CodeListCache$new(table_location, values)
}

code_list_caches <- function(caches) {
  CodeListCaches$new(caches)
}

data_table_cache <- function(column_location, values) {
  DataTableCache$new(column_location, values)
}

data_table_caches <- function(caches) {
  DataTableCaches$new(caches)
}

data_id_cache <- function(caches) {
  DataIdCache$new(caches)
}

is_ignored_column <- function(object) {
  class(object)[[1]] == "IgnoredColumn"
}

is_simple_column <- function(object) {
  class(object)[[1]] == "SimpleColumn"
}

is_fk_column <- function(object) {
  class(object)[[1]] == "ForeignKeyColumn"
}

is_mandatory_measurement_column <- function(object) {
  class(object)[[1]] == "MeasurementValueColumn"
}

is_measurement_unit_column <- function(object) {
  class(object)[[1]] == "MeasurementUnitColumn"
}

is_new_query <- function(object) {
  class(object)[[1]] == "NewQueryColumnAction"
}

is_new_measurement_query <- function(object) {
  class(object)[[1]] == "NewMeasurementQueryColumnAction"
}

is_new_association_query <- function(object) {
  class(object)[[1]] == "NewAssociationQueryColumnAction"
}

is_flush_measurement_query <- function(object) {
  class(object)[[1]] == "FlushMeasurementQueryColumnAction"
}

is_flush_association_query <- function(object) {
  class(object)[[1]] == "FlushAssociationQueryColumnAction"
}

is_flush_query <- function(object) {
  class(object)[[1]] == "FlushQueryColumnAction"
}

is_check_exists <- function(object) {
  class(object)[[1]] == "CheckExists"
}

is_check_should_exists <- function(object) {
  class(object)[[1]] == "CheckShouldExists"
}

is_get_id <- function(object) {
  class(object)[[1]] == "GetIdAction"
}

is_add_column <- function(object) {
  class(object)[[1]] == "AddColumnAction"
}

is_add_fk_column <- function(object) {
  class(object)[[1]] == "AddForeignKeyColumnAction"
}

is_add_external_fk_column <- function(object) {
  class(object)[[1]] == "AddExternalForeignKeyColumnAction"
}

#' Generic method to load xsl \code{file}, using the given \code{import_model} xls model.
#' @param import_model The xls column names model per sheet
#' @param file The file to import
#' @return the loaded xls
#' @export
load_xls <- function(import_model, file) {
  sheet_names <- import_model$sheet_names()
  sheet_models <- import_model$sheets()
  # The result (list of data.table, one per sheet, if sheet is empty, then it won't be available in result)
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
  sheets_size <- length(sheet_models)
  for (i in seq(1:sheets_size)) {
    sheet_model <- sheet_models[[i]]
    sheet_name <- sheet_model$name()
    sheet_content <- as.data.table(yata_frame[i])
    if (nrow(sheet_content) < 6) {
      # No data in this sheet
      sheet_content <- NULL
    } else {
      sheet_content <- sheet_content[6:nrow(sheet_content)]
      column_names <- unlist(sheet_model$column_names())
      names(sheet_content) <- column_names
    }
    result[[sheet_name]] <- sheet_content
  }
  result
}