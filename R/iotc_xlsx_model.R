# importing the required library
library("R6")

Column <- R6Class(
  "Column",
  public = list(
    initialize = function(name, mandatory = FALSE) {
      stopifnot(!is.na(name), is.character(name), length(name) > 0)
      stopifnot(!is.na(mandatory), is.logical(mandatory))
      private$.name <- name
      private$.mandatory <- mandatory
    },
    name = function() {
      private$.name
    },
    mandatory = function() {
      private$.mandatory
    },
    print = function(prefix = "") {
      cat(prefix, "Column: ", self$name(), sep = "")
      if (self$mandatory()) {
        cat(" (Mandatory)", sep = "")
      } else {
        cat(" (Optional)", sep = "")
      }
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
  inherit = Column,
  public = list(
    initialize = function(name,
                          mandatory = FALSE,
                          foreign_table,
                          foreign_column) {
      super$initialize(name, mandatory)
      stopifnot(!is.na(foreign_table),
                is.character(foreign_table),
                length(foreign_table) > 0)
      stopifnot(!is.na(foreign_column),
                is.character(foreign_column),
                length(foreign_column) > 0)
      private$.foreign_table <- foreign_table
      private$.foreign_column <- foreign_column
    },
    foreign_table = function(x) {
      private$.foreign_table
    },
    foreign_column = function() {
      private$.foreign_column
    },
    print = function(prefix = "") {
      cat(prefix, "Column: ", self$name(), sep = "")
      if (self$mandatory()) {
        cat(" (Mandatory)", sep = "")
      } else {
        cat(" (Optional)", sep = "")
      }
      cat(" (foreign-key: ", self$foreign_table(),
          ".", self$foreign_column(), ")\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    # foreign table
    .foreign_table = NULL,
    # foreign column
    .foreign_column = NULL
  )
)

MeasurementValueColumn <- R6Class(
  "MeasurementValueColumn",
  inherit = Column,
  public = list(
    initialize = function(name, mandatory = FALSE, measurement_table, unit) {
      super$initialize(name, mandatory)
      print(append(" field :",name))
      stopifnot(!is.na(measurement_table),
                is.character(measurement_table), length(measurement_table) > 0)
      if (!is.na(unit)) {
        stopifnot(length(unit) > 0)
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
      cat(prefix, "Column: ", self$name(), sep = "")
      if (self$mandatory()) {
        cat(" (Mandatory)", sep = "")
      } else {
        cat(" (Optional)", sep = "")
      }
      cat(" (measurement_table: ", self$measurement_table(), sper = "")
      if (!is.na(private$.unit)) {
        cat(" (unit: ", as.character(self$unit()), ")\n", sper = "")
      }
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
  inherit = Column,
  public = list(
    initialize = function(name, mandatory = FALSE, units) {
      super$initialize(name, mandatory)
      stopifnot(!is.na(units), is.vector(units), length(units) > 0)
      private$.units <- units
    },
    units = function() {
      private$.units
    },
    print = function(prefix = "") {
      cat(prefix, "Column: ", self$name(), sep = "")
      if (self$mandatory()) {
        cat(" (Mandatory)", sep = "")
      } else {
        cat(" (Optional)", sep = "")
      }
      cat(" (units: ", as.character(self$units()), ")\n", sper = "")
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
      print(name)
      stopifnot(!is.na(name), is.character(name), length(name) > 0)
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
      stopifnot(!is.na(name), is.character(name), length(name) > 0)
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