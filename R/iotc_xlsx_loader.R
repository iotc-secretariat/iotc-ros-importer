# importing the required library
library(openxlsx)
library(data.table)

#' Generic method to load xsl \code{file}, using the given \code{column_names_per_sheet} xls model.
#' @param file The file to import
#' @param column_names_per_sheet The xls column names model per sheet
#' @return the loaded xls
#' @export
load_xls <- function(file, column_names_per_sheet) {
  sheet_names <- names(column_names_per_sheet)
  # Load xsl frames, one per sheet
  yata_frame <- lapply(sheet_names,
                       openxlsx::read.xlsx,
                       xlsxFile = file,
                       colNames = FALSE,
                       skipEmptyCols = FALSE,
                       detectDates = TRUE)
  # The result (list of data.table, one per sheet, if sheet is empty, then it won't be available in result)
  result <- list()
  sheets_size <- length(sheet_names)
  for (i in seq(1:sheets_size)) {
    sheet_name <- as.character(sheet_names[[i]])
    print(sheet_name)
    sheet_content <- as.data.table(yata_frame[i])
    if (nrow(sheet_content) < 6) {
      # No data in this sheet
      sheet_content <- NULL
    } else {
      yo <- lapply(column_names_per_sheet[[sheet_name]],
                   function(x) {
                     x$name()
                   })
      print(" YO: ")
      print(yo)
      sheet_content <- sheet_content[6:nrow(sheet_content)]
      names(sheet_content) <- yo
    }
    result[[sheet_name]] <- sheet_content
  }
  result
}

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
    initialize = function(name, mandatory = FALSE, measurement_table, units) {
      super$initialize(name, mandatory)
      stopifnot(!is.na(measurement_table),
                is.character(measurement_table), length(measurement_table) > 0)
      stopifnot(!is.na(units), is.list(units), length(units) > 0)
      private$.measurement_table <- measurement_table
      private$.units <- units
    },
    measurement_table = function(x) {
      private$.measurement_table
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
      cat(" (measurement_table: ", self$measurement_table(), sper = "")
      cat(" (units: ", as.character(self$units()), ")\n", sper = "")
      invisible(self)
    }
  ),
  private = list(
    # measurement table
    .measurement_table = NULL,
    # possible units
    .units = NULL
  )
)

MeasurementUnitColumn <- R6Class(
  "MeasurementUnitColumn",
  inherit = Column,
  public = list(
    initialize = function(name, mandatory = FALSE, units) {
      super$initialize(name, mandatory)
      stopifnot(!is.na(units), is.list(units), length(units) > 0)
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
      stopifnot(!is.na(name), is.character(name), length(name) > 0)
      stopifnot(!is.na(columns), is.list(columns), length(columns) > 0)
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
      stopifnot(!is.na(sheets), is.list(sheets), length(sheets) > 0)
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
# a <- Column$new("Adam")
# Column$new("Adam")$print()
# Column$new("Adam", mandatory = TRUE)$print()
# ForeignKeyColumn$new("Adam", mandatory = TRUE, foreign_table = "ref_admin.countries", foreign_column = "code")$print()
# MeasurementValueColumn$new("Adam", mandatory = TRUE, measurement_table = "ros_common.lengths", units = list("cm", "mm"))$print()
# MeasurementUnitColumn$new("Adam", mandatory = TRUE, units = list("cm", "mm"))$print()
# sheet <- Sheet$new("Taiste", list(a, a, a))$print()
# sheet$column_names()
# ImportFile$new("Import-file", list(sheet))$print()


#' Generic method to load xsl \code{file}, using the given \code{column_names_per_sheet} xls model.
#' @param file The file to import
#' @param import_model The xls column names model per sheet
#' @return the loaded xls
#' @export
load_xls2 <- function(file, import_model) {
  sheet_names <- import_model$sheet_names()
  sheet_models <- import_model$sheets()
  # Load xsl frames, one per sheet
  yata_frame <- lapply(sheet_names,
                       openxlsx::read.xlsx,
                       xlsxFile = file,
                       colNames = FALSE,
                       skipEmptyCols = FALSE,
                       detectDates = TRUE)
  # The result (list of data.table, one per sheet, if sheet is empty, then it won't be available in result)
  result <- list()
  sheets_size <- length(sheet_models)
  for (i in seq(1:sheets_size)) {
    sheet_model <- sheet_models[[i]]
    sheet_name <- sheet_model$name()
    print(sheet_name)
    sheet_content <- as.data.table(yata_frame[i])
    if (nrow(sheet_content) < 6) {
      # No data in this sheet
      sheet_content <- NULL
    } else {
      sheet_content <- sheet_content[6:nrow(sheet_content)]
      column_names <- sheet_model$column_names()
      print(column_names)
      print(class(column_names))
      names(sheet_content) <- column_names
    }
    result[[sheet_name]] <- sheet_content
  }
  result
}