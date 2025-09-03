# importing the required library
library("R6")

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
      # cat(paste0("TableLocation ", gav))
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
      # cat(paste0("ColumnLocation ", gav))
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
      sprintf("%s→%s", self$table(), self$column())
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

CodeListCache <- R6Class(
  "CodeListCache",
  public = list(
    initialize = function(table_location, values) {
      stopifnot(!is.null(table_location))
      stopifnot(!is.null(values))
      private$.table_location <- table_location
      private$.values <- values
      # print(table_location)
      private$.codes <- as.list(values[, code])
      private$.with_code_orig <- "code_orig" %in% names(values)
      if (private$.with_code_orig) {
        private$.code_orig_mapping <- as.list(values[!is.null(code_orig), .(code_orig, code)])
      }
      # self$print()
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
      self$foreign_column_location()$table()
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

Sheet <- R6Class(
  "Sheet",
  public = list(
    initialize = function(name, comment, columns) {
      if (DEBUG) {
        cat("Sheet:", name, "\n")
      }
      stopifnot(!is.na(comment), is.character(comment), nchar(comment) > 0)
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(columns), is.vector(columns), length(columns) > 0)
      private$.name <- name
      private$.columns <- columns
      private$.comment <- comment
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
    .columns = NULL
  )
)

ImportFile <- R6Class(
  "ImportFile",
  public = list(
    initialize = function(name, sheets) {
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(sheets), is.vector(sheets), length(sheets) > 0)
      private$.name <- name
      private$.sheets <- sheets

    },
    init = function(connection) {
      stopifnot(!is.null(connection))
      code_list_tables <- private$.collect_code_list_tables()
      private$.code_list_caches <- lapply(code_list_tables, function(x) {
        values <- load_codelist(x$schema(), x$table(), columns = NULL, connection)
        setorder(values, code)
        code_list_cache(x, values)
      })
      names(private$.code_list_caches) <- lapply(code_list_tables, function(x) { x$gav() })
      data_tables <- private$.collect_data_tables()
      private$.data_table_ids <- lapply(data_tables, function(x) {
        query <- paste0("SELECT nextval('", x, "_id_seq');")
        # print(query)
        result_set <- dbSendQuery(connection, query)
        id <- as.integer(dbFetch(result_set)[["nextval"]]) - 1
        dbClearResult(result_set)
        id
      })
      names(private$.data_table_ids) <- data_tables
    },
    name = function() {
      private$.name
    },
    sheets = function() {
      private$.sheets
    },
    data_table_ids = function() {
      private$.data_table_ids
    },
    code_list_caches = function() {
      private$.code_list_caches
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
    # columns of the sheet
    .sheets = NULL,
    # data tables ids
    .data_table_ids = NULL,
    # code list caches
    .code_list_caches = NULL,
    .collect_data_tables = function() {
      unique(unlist(lapply(self$sheets(), function(s) {
        s$collect_data_tables()
      })))
    },
    .collect_code_list_tables = function() {
      temp <- unlist(lapply(self$sheets(), function(s) {
        s$collect_code_list_tables()
      }))
      set <- c()
      result <- list()
      for (i in temp) {
        if (!i$gav() %in% set) {
          set[length(set) + 1] <- i$gav()
          result[length(result) + 1] <- i
        }
      }
      result
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

code_list_cache <- function(table_location, values) {
  CodeListCache$new(table_location, values)
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

sheet <- function(name, comment, columns) {
  Sheet$new(name, comment, columns)
}

import_file <- function(name, sheets) {
  ImportFile$new(name, sheets)
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
