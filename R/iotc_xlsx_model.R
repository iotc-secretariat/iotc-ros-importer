# importing the required library
library("R6")

DEBUG <- FALSE

ColumnLocation <- R6Class(
  "ColumnLocation",
  public = list(
    initialize = function(table, column) {
      stopifnot(!is.na(table), is.character(table), nchar(table) > 0)
      stopifnot(!is.na(column), is.character(column), nchar(column) > 0)
      private$.table <- table
      private$.column <- column
    },
    table = function() {
      private$.table
    },
    column = function() {
      private$.column
    },
    print = function() {
      cat(self$table(), " → ", self$column(), sep = "")
    },
    toJson = function() {
      sprintf('"%s→%s"', self$table(), self$column())
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
    },
    toJson = function() {
      if (is.na(self$comment())) {
        com <- ""
      } else {
        com <- sprintf(', "comment": "%s"', self$comment())
      }
      sprintf('{ "IgnoredColumn": "%s"%s}',
              self$name(),
              com)
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
    },
    toJson = function() {
      if (self$mandatory()) {
        tmp <- "MandatorySimpleColumn"
      } else {
        tmp <- "OptionalSimpleColumn"
      }
      if (is.na(self$comment())) {
        com <- ""
      } else {
        com <- sprintf(', "comment": "%s"', self$comment())
      }
      sprintf('{ "%s": "%s", "location": %s%s}',
              tmp,
              self$name(),
              self$column_location()$toJson(),
              com)
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
    print = function(prefix = "") {
      super$.print(prefix, "ForeignKeyColumn")
      cat(" - foreign-key: ( ", sep = "")
      self$foreign_column_location()$print()
      cat(" )\n", sep = "")
      invisible(self)
    },
    toJson = function() {
      if (self$mandatory()) {
        tmp <- "MandatoryForeignKeyColumn"
      } else {
        tmp <- "OptionalForeignKeyColumn"
      }
      if (is.na(self$comment())) {
        com <- ""
      } else {
        com <- sprintf(', "comment": "%s"', self$comment())
      }
      sprintf('{ "%s": "%s", "location": %s, "fk": %s%s}',
              tmp,
              self$name(),
              self$column_location()$toJson(),
              self$foreign_column_location()$toJson(),
              com)
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
    },
    toJson = function() {
      if (self$mandatory()) {
        tmp <- "MandatoryMeasurementValueColumn"
      } else {
        tmp <- "OptionalMeasurementValueColumn"
      }
      if (is.na(self$comment())) {
        com <- ""
      } else {
        com <- sprintf(', "comment": "%s"', self$comment())
      }
      if (is.null(self$unit()) || is.na(self$unit())) {
        unit <- ""
      } else {
        unit <- sprintf(', "unit": "%s"', self$unit())
      }
      sprintf('{ "%s": "%s", "location": %s, "measurement_table": "%s"%s%s}',
              tmp,
              self$name(),
              self$column_location()$toJson(),
              self$measurement_table(),
              unit,
              com)
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
    },
    toJson = function() {
      if (self$mandatory()) {
        tmp <- "MandatoryMeasurementUnitColumn"
      } else {
        tmp <- "OptionalMeasurementUnitColumn"
      }
      u <- ""
      length_result <- length(self$units())
      for (i in seq(1, length_result)) {
        u <- paste0(u, '"', self$units()[[i]], '"')
        if (i < length_result) {
          u <- paste0(u, " ,")
        }
      }
      sprintf('{ "%s": "%s", "units": [%s]}',
              tmp,
              self$name(),
              u)
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
    print = function(prefix = "") {
      cat(prefix, "Sheet: ", self$name(), " with ", length(self$columns()), " column(s)\n", sep = "")
      for (c in self$columns()) {
        c$print(paste(prefix, " "))
      }
      invisible(self)
    },
    toJson = function() {
      tmp <- "["
      max <- length(self$columns())
      for (s in seq(1, max)) {
        ss <- self$columns()[[s]]
        sss <- ss$toJson()
        tmp <- paste(tmp, "\n", sss)
        if (s < max) {
          tmp <- paste(tmp, ",")
        }
      }
      tmp <- paste(tmp, "]")
      sprintf('"%s": { "comment": "%s", "columns": %s }',
              self$name(),
              self$comment(),
              tmp)
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
    name = function() {
      private$.name
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
    },
    toJson = function() {
      tmp <- "{"
      max <- length(self$sheets())
      for (s in seq(1, max)) {
        ss <- self$sheets()[[s]]
        sss <- ss$toJson()
        tmp <- paste(tmp, "\n", sss)
        if (s < max) {
          tmp <- paste(tmp, ",")
        }
      }
      tmp <- paste(tmp, "\n}")
      sprintf('{ "%s": %s}',
              self$name(),
              tmp)[[1]]
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
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

column_location <- function(table, column) {
  ColumnLocation$new(table, column)
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

flush_measurement_query <- function() {
  FlushMeasurementQuery$new()
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

is_mandatory_simple_column <- function(object) {
  class(object)[[1]] == "SimpleColumn"
}

is_mandatory_fk_column <- function(object) {
  class(object)[[1]] == "ForeignKeyColumn"
}

is_mandatory_measurement_column <- function(object) {
  class(object)[[1]] == "MeasurementValueColumn"
}

is_mandatory_measurement_unit_column <- function(object) {
  class(object)[[1]] == "MeasurementUnitColumn"
}

is_new_query <- function(object) {
  class(object)[[1]] == "NewQuery"
}

is_new_measurement_query <- function(object) {
  class(object)[[1]] == "NewMeasurementQuery"
}

is_flush_measurement_query <- function(object) {
  class(object)[[1]] == "FlushMeasurementQuery"
}

is_flush_query <- function(object) {
  class(object)[[1]] == "FlushQuery"
}

is_add_column <- function(object) {
  class(object)[[1]] == "AddColumn"
}

is_add_fk_column <- function(object) {
  class(object)[[1]] == "AddForeignKeyColumn"
}

is_add_external_fk_column <- function(object) {
  class(object)[[1]] == "AddExternalForeignKeyColumn"
}
