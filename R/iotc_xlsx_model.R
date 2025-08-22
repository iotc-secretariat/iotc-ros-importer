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
    initialize = function(name, mandatory = FALSE) {
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(mandatory), is.logical(mandatory))
      private$.name <- name
      private$.mandatory <- mandatory
    },
    name = function() {
      private$.name
    },
    mandatory = function() {
      private$.mandatory
    }
  ),
  private = list(
    # column name
    .name = NULL,
    # is column mandatory
    .mandatory = NULL,
    .print = function(prefix = "", name) {
      cat(prefix, name, ": ", self$name(), sep = "")
      if (self$mandatory()) {
        cat(" (Mandatory)", sep = "")
      } else {
        cat(" (Optional)", sep = "")
      }
    }
  )
)

AbstractColumnWithColumnLocation <- R6Class(
  "AbstractColumnWithColumnLocation",
  inherit = AbstractColumn,
  public = list(
    initialize = function(name, mandatory = FALSE, column_location) {
      super$initialize(name, mandatory)
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

SimpleColumn <- R6Class(
  "SimpleColumn",
  inherit = AbstractColumnWithColumnLocation,
  public = list(
    initialize = function(name, mandatory = FALSE, column_location) {
      if (DEBUG) {
        cat("> SimpleColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location)
    },
    print = function(prefix = "") {
      super$.print(prefix, "SimpleColumn")
      cat("\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    # column name
    .name = NULL,
    # is column mandatory
    .mandatory = NULL
  )
)

ForeignKeyColumn <- R6Class(
  "ForeignKeyColumn",
  inherit = AbstractColumnWithColumnLocation,
  public = list(
    initialize = function(name,
                          mandatory = FALSE,
                          column_location,
                          foreign_column_location) {
      if (DEBUG) {
        cat("> ForeignKeyColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location)
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
    initialize = function(name, mandatory = FALSE, column_location, measurement_table, unit = NA) {
      if (DEBUG) {
        cat("> MeasurementValueColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location)
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
    initialize = function(name, mandatory = FALSE, units) {
      if (DEBUG) {
        cat("> MeasurementUnitColumn:", name, "\n")
      }
      super$initialize(name, mandatory)
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
    initialize = function(name, columns) {
      if (DEBUG) {
        cat("Sheet:", name, "\n")
      }
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(columns), is.vector(columns), length(columns) > 0)
      private$.name <- name
      private$.columns <- columns
    },
    name = function() {
      private$.name
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
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
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
    }
  ),
  private = list(
    # sheet name
    .name = NULL,
    # columns of the sheet
    .sheets = NULL
  )
)
column_location <- function(table, column) {
  ColumnLocation$new(table, column)
}

optional_simple_column <- function(name, column_location) {
  SimpleColumn$new(name, mandatory = FALSE, column_location)
}

mandatory_simple_column <- function(name, column_location) {
  SimpleColumn$new(name, mandatory = TRUE, column_location)
}

optional_fk_column <- function(name, column_location, foreign_column_location) {
  ForeignKeyColumn$new(name, mandatory = FALSE, column_location, foreign_column_location)
}

mandatory_fk_column <- function(name, column_location, foreign_column_location) {
  ForeignKeyColumn$new(name, mandatory = TRUE, column_location, foreign_column_location)
}

optional_measurement_column <- function(name, column_location, measurement_table, unit = NA) {
  MeasurementValueColumn$new(name, mandatory = FALSE, column_location, measurement_table, unit)
}

mandatory_measurement_column <- function(name, column_location, measurement_table, unit = NA) {
  MeasurementValueColumn$new(name, mandatory = TRUE, column_location, measurement_table, unit)
}

optional_measurement_unit_column <- function(name, units) {
  MeasurementUnitColumn$new(name, mandatory = FALSE, units)
}

mandatory_measurement_unit_column <- function(name,  units) {
  MeasurementUnitColumn$new(name, mandatory = TRUE, units)
}

sheet <- function(name, columns) {
  Sheet$new(name, columns)
}

import_file <- function(name, sheets) {
  ImportFile$new(name, sheets)
}