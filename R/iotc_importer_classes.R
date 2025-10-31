# importing the required library
library(R6)
library(openxlsx)
library(RPostgres)
library(data.table)
library(stringi)
library(stringr)
library(R.oo)
# library(iotc.base.common.data)

DEBUG <- FALSE

split_table_location <- function(value) {
  unlist(strsplit(value, "\\."))
}

split_column_location <- function(value) {
  unlist(strsplit(value, "→"))
}

TableLocation <- R6Class(
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

ColumnLocation <- R6Class(
  "ColumnLocation",
  public = list(
    initialize = function(gav) {
      stopifnot(!is.na(gav), is.character(gav), nchar(gav) > 0, gav %like% ".+\\..+→.+")
      split <- split_column_location(gav)
      private$.table <- table_location(split[[1]])
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

AbstractColumn <- R6Class(
  "AbstractColumn",
  public = list(
    initialize = function(name, mandatory = FALSE, comment = NA, actions = NA) {
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(mandatory), is.logical(mandatory))
      private$.name <- name
      private$.mandatory <- mandatory
      private$.comment <- comment
      private$.actions <- actions
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
    initialize = function(name, mandatory = FALSE, column_location, comment = NA, actions = NA) {
      super$initialize(name, mandatory, comment, actions)
      stopifnot(!rlang::is_empty(column_location))
      private$.column_location <- column_location
    },
    column_location = function() {
      private$.column_location
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
      super$initialize(name, TRUE, comment)
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
    initialize = function(name, mandatory = FALSE, column_location, comment = NA, actions) {
      if (DEBUG) {
        cat("> SimpleColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment, actions)
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
                          actions) {
      if (DEBUG) {
        cat("> ForeignKeyColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment, actions)
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
    initialize = function(name, mandatory = FALSE, column_location, measurement_table, unit = NA, comment = NA, actions) {
      if (DEBUG) {
        cat("> MeasurementValueColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment, actions)
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
    initialize = function(name, mandatory = FALSE, units, actions) {
      if (DEBUG) {
        cat("> MeasurementUnitColumn:", name, "\n")
      }
      super$initialize(name, mandatory, actions)
      stopifnot(!is.na(units), is.vector(units), length(units) > 0)
      private$.units <- units
    },
    units = function() {
      private$.units
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
    initialize = function(name, mandatory, column, row, expected = NULL, format = NULL) {
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
    init_value = function(meta_sheet) {
      private$.value <- meta_sheet[[self$row()]][[self$column()]]
    },
    check_value = function() {
      is_na <- is.na(self$value())
      if (self$mandatory() && is_na) {
        throw("Meta: ", self$name(), " is mandatory and no value found in META sheet")
        invisible(self)
      }
      if (!is_na) {
        # Not null value
        if (!is.null(self$expected())) {
          # With expected value, let's check it
          if (self$expected() != self$value()) {
            throw("Meta: ", self$name(), " should have value ", self$expected(), " but is ", self$value())
            invisible(self)
          }
        }
        if (!is.null(self$format())) {
          # With format, let's check it
          if (!grepl(self$format(), self$value())) {
            throw("Meta: ", self$name(), " should have format ", self$format(), " but is ", self$value())
            invisible(self)
          }
        }
      }
      cat("Meta: ", self$name(), " value ", self$value(), " is correct", "\n", sep = "")
    },
    value = function() {
      private$.value
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
    .format = NULL
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
    init_values = function(meta_sheet) {
      for (c in self$cells()) {
        c$init_value(meta_sheet)
      }
    },
    check_values = function() {
      for (c in self$cells()) {
        c$check_value()
      }
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
        private$.code_orig_mapping <- as.list(values[!is.null(code_orig), .(code_orig, code)])
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
    get_code = function(table_location, code) {
      stopifnot(table_location %in% names(private$.caches))
      cache <- private$.caches[[table_location]]
      cache$get_code(code)
    },
    contains_code = function(table_location, code) {
      stopifnot(table_location %in% names(private$.caches))
      cache <- private$.caches[[table_location]]
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
    initialize = function(model, file, connection, extra_data_tables) {
      stopifnot(!is.null(model))
      stopifnot(!is.null(connection))
      stopifnot(!is.null(file))
      private$.model <- model
      private$.xls_data <- load_xls(model, file)
      code_list_tables <- private$.collect_code_list_tables()
      .code_list_caches <- lapply(code_list_tables, function(x) {
        t <- table_location(x)
        values <- load_codelist(t$schema(), t$table(), columns = NULL, connection)
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
    xls_data = function() {
      private$.xls_data
    },
    data_table_ids = function() {
      private$.data_table_ids
    },
    code_list_caches = function() {
      private$.code_list_caches
    },
    sheet_names = function() {
      private$.model$sheet_names()
    },
    check_code_list_in_data = function() {
      code_list_caches <- self$code_list_caches()

    }
  ),
  private = list(
    # import model
    .model = NULL,
    # xls data to import
    .xls_data = NULL,
    # data tables ids
    .data_table_ids = NULL,
    # code list caches
    .code_list_caches = NULL,
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

mandatory_meta_cell <- function(name, column, row, expected = NULL, format = NULL) {
  MetaCell$new(name, mandatory = TRUE, column, row, expected, format)
}

optional_meta_cell <- function(name, column, row, expected = NULL, format = NULL) {
  MetaCell$new(name, mandatory = FALSE, column, row, expected, format)
}

table_location <- function(gav) {
  TableLocation$new(gav)
}

column_location <- function(gav) {
  ColumnLocation$new(gav)
}

ignored_column <- function(name, comment = NA) {
  IgnoredColumn$new(name, comment)
}

optional_simple_column <- function(name, column_location, comment = NA, actions = NA) {
  SimpleColumn$new(name, mandatory = FALSE, column_location, comment, actions)
}

mandatory_simple_column <- function(name, column_location, comment = NA, actions = NA) {
  SimpleColumn$new(name, mandatory = TRUE, column_location, comment, actions)
}

optional_fk_column <- function(name, column_location, foreign_column_location, comment = NA, actions = NA) {
  ForeignKeyColumn$new(name, mandatory = FALSE, column_location, foreign_column_location, comment, actions)
}

mandatory_fk_column <- function(name, column_location, foreign_column_location, comment = NA, actions = NA) {
  ForeignKeyColumn$new(name, mandatory = TRUE, column_location, foreign_column_location, comment, actions)
}

optional_measurement_column <- function(name, column_location, measurement_table, unit = NA, comment = NA, actions = NA) {
  MeasurementValueColumn$new(name, mandatory = FALSE, column_location, measurement_table, unit, comment, actions)
}

mandatory_measurement_column <- function(name, column_location, measurement_table, unit = NA, comment = NA, actions = NA) {
  MeasurementValueColumn$new(name, mandatory = TRUE, column_location, measurement_table, unit, comment, actions)
}

optional_measurement_unit_column <- function(name, units, actions = NA) {
  MeasurementUnitColumn$new(name, mandatory = FALSE, units, actions)
}

mandatory_measurement_unit_column <- function(name, units, actions = NA) {
  MeasurementUnitColumn$new(name, mandatory = TRUE, units, actions)
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

import_context <- function(model, file, connection, extra_data_tables) {
  ImportContext$new(model, file, connection, extra_data_tables)
}

code_list_cache <- function(table_location, values) {
  CodeListCache$new(table_location, values)
}

code_list_caches <- function(caches) {
  CodeListCaches$new(caches)
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

#' Generic method to load the content of a given code list.
#'
#' @param codelist_domain The code list domain
#' @param codelist_name The code list name
#' @param columns The optional columns to load (if not specified, then will load all columns of the code list)
#' @param connection where to load the code list
#' @return the loaded code list as a data.Table
#' @export
load_codelist <- function(codelist_domain, codelist_name, columns = NULL, connection) {
  if (is.null(columns)) {
    columns <- "*"
  } else {
    columns <- paste0(columns, collapse = ", ")
  }
  query(
    connection,
    paste0("SELECT ", columns, " FROM ", codelist_domain, ".", codelist_name)
  )
}