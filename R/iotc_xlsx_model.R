# importing the required library
library("R6")
library('jsonlite')

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
    initialize = function(name, mandatory = FALSE, comment = NA) {
      stopifnot(!is.na(name), is.character(name), nchar(name) > 0)
      stopifnot(!is.na(mandatory), is.logical(mandatory))
      private$.name <- name
      private$.mandatory <- mandatory
      private$.comment <- comment
    },
    name = function() {
      private$.name
    },
    mandatory = function() {
      private$.mandatory
    },
    comment = function() {
      private$.comment
    }
  ),
  private = list(
    # column name
    .name = NULL,
    # optional comment
    .comment = NULL,
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
    initialize = function(name, mandatory = FALSE, column_location, comment = NA) {
      super$initialize(name, mandatory, comment)
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
    initialize = function(name, mandatory = FALSE, column_location, comment = NA) {
      if (DEBUG) {
        cat("> SimpleColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment)
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
                          comment = NA) {
      if (DEBUG) {
        cat("> ForeignKeyColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment)
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
    initialize = function(name, mandatory = FALSE, column_location, measurement_table, unit = NA, comment = NA) {
      if (DEBUG) {
        cat("> MeasurementValueColumn:", name, "\n")
      }
      super$initialize(name, mandatory, column_location, comment)
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

split_location <- function(value) {
  unlist(strsplit(value, "→"))
}

column_location <- function(table, column) {
  ColumnLocation$new(table, column)
}

ignored_column <- function(name, comment = NA) {
  IgnoredColumn$new(name, comment)
}

optional_simple_column <- function(name, column_location, comment = NA) {
  SimpleColumn$new(name, mandatory = FALSE, column_location, comment)
}

mandatory_simple_column <- function(name, column_location, comment = NA) {
  SimpleColumn$new(name, mandatory = TRUE, column_location, comment)
}

optional_fk_column <- function(name, column_location, foreign_column_location, comment = NA) {
  ForeignKeyColumn$new(name, mandatory = FALSE, column_location, foreign_column_location, comment)
}

mandatory_fk_column <- function(name, column_location, foreign_column_location, comment = NA) {
  ForeignKeyColumn$new(name, mandatory = TRUE, column_location, foreign_column_location, comment)
}

optional_measurement_column <- function(name, column_location, measurement_table, unit = NA, comment = NA) {
  MeasurementValueColumn$new(name, mandatory = FALSE, column_location, measurement_table, unit, comment)
}

mandatory_measurement_column <- function(name, column_location, measurement_table, unit = NA, comment = NA) {
  MeasurementValueColumn$new(name, mandatory = TRUE, column_location, measurement_table, unit, comment)
}

optional_measurement_unit_column <- function(name, units) {
  MeasurementUnitColumn$new(name, mandatory = FALSE, units)
}

mandatory_measurement_unit_column <- function(name, units) {
  MeasurementUnitColumn$new(name, mandatory = TRUE, units)
}

sheet <- function(name, comment, columns) {
  Sheet$new(name, comment, columns)
}

import_file <- function(name, sheets) {
  ImportFile$new(name, sheets)
}


load_model <- function(path) {
  content <- fromJSON(path)
  real_content <- content[[1]]
  sheet_names <- names(real_content)
  result <- list()
  for (i in sheet_names) {
    sheet <- load_sheet(i, real_content)
    result[i] <- list(sheet)
  }
  result
}

load_sheet <- function(name, content) {
  # cat(name)
  # cat("\n")
  sheet_content <- content[[name]]
  sheet_comment <- sheet_content$comment
  sheet_comluns <- sheet_content$columns
  types <- names(sheet_comluns)
  column_types <- c()
  result_columns <- list()
  index <- 1
  for (i in types) {
    # if (i |> str_detect("Column")) {
    if (grepl("Column", i)) {
      column_types[index] <- i
      index <- index + 1
    }
  }
  index <- 1
  locations <- sheet_comluns[["location"]]
  measurement_tables <- sheet_comluns[["measurement_table"]]
  with_fks <- "fk" %in% types

  if (with_fks) {
    fks <- sheet_comluns[["fk"]]
  }
  with_location <- "location" %in% types
  with_unit <- "unit" %in% types
  if (with_unit) {
    units <- sheet_comluns[["unit"]]
  }
  with_units <- "units" %in% types
  if (with_units) {
    unitss <- sheet_comluns[["units"]]
  }
  with_comment <- "comment" %in% types
  if (with_comment) {
    comments <- sheet_comluns[["comment"]]
  }
  for (j in seq(1, nrow(sheet_comluns))) {
    for (i in column_types) {
      tmp <- sheet_comluns[[i]]
      y <- tmp[[j]]
      if (!is.na(tmp[[j]])) {
        if (with_location) {
          location <- locations[j]
          if (!is.na(location)) {
            l <- split_location(location)
          }
        }
        if (with_fks) { fk <- fks[j]
          if (!is.na(fk)) {
            f <- split_location(fk)
          }
        }
        if (with_comment) {
          comment <- comments[j]
          if (is.na(comment)) {
            comment <- ""
          } else {
            comment <- sprintf(', "%s"', comment)
          }
        } else {
          comment <- ""
        }
        if (with_unit) {
          unit <- units[j]
          if (is.na(unit)) {
            unit <- ""
          } else {
            unit <- sprintf(', "%s"', unit)
          }
        } else {
          unit <- ""
        }
        if (grepl("IgnoredColumn", i)) {
          t <- sprintf('ignored_column("%s"%s)', y, comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatorySimpleColumn", i)) {
          t <- sprintf('mandatory_simple_column("%s", column_location("%s", "%s")%s)', y, l[[1]], l[[2]], comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalSimpleColumn", i)) {
          t <- sprintf('optional_simple_column("%s", column_location("%s", "%s")%s)', y, l[[1]], l[[2]], comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryForeignKeyColumn", i)) {
          t <- sprintf('mandatory_fk_column("%s", column_location("%s", "%s"), column_location("%s", "%s")%s)', y, l[[1]], l[[2]], f[[1]], f[[2]], comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalForeignKeyColumn", i)) {
          t <- sprintf('optional_fk_column("%s", column_location("%s", "%s"), column_location("%s", "%s")%s)', y, l[[1]], l[[2]], f[[1]], f[[2]], comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryMeasurementValueColumn", i)) {
          t <- sprintf('mandatory_measurement_column("%s", column_location("%s", "%s"), "%s"%s%s)', y, l[[1]], l[[2]], measurement_tables[j], unit, comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalMeasurementValueColumn", i)) {
          t <- sprintf('optional_measurement_column("%s", column_location("%s", "%s"), "%s"%s%s)', y, l[[1]], l[[2]], measurement_tables[j], unit, comment)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryMeasurementUnitColumn", i)) {
          t <- sprintf('mandatory_measurement_unit_column("%s", %s)', y, unitss[j])
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalMeasurementUnitColumn", i)) {
          t <- sprintf('optional_measurement_unit_column("%s", %s)', y, unitss[j])
          result_columns <- append(result_columns, t)
        }
      }
    }
  }
  result <- list(comment = sheet_comment, columns = result_columns)
  cat("\n")
  result
}

write_model <- function(name, content, target_file_path) {
  sheet_names <- names(content)
  cat(paste0("#' The import file model for the ", name, "\n"), file = target_file_path)
  cat("#' @export\n", file = target_file_path, append = TRUE)
  cat(paste0(name, "_MODEL <-\n"), file = target_file_path, append = TRUE)
  cat(sprintf('  import_file("%s", c(\n', name), file = target_file_path, append = TRUE)
  i <- 1
  n <- length(sheet_names)
  for (sheet_name in sheet_names) {
    sheet_content <- content[[sheet_name]]
    sheet_comment <- sheet_content[["comment"]]
    sheet_columns <- sheet_content[["columns"]]
    s <- length(sheet_columns)
    cat(sprintf('    sheet("%s", "%s", c(\n', sheet_name, sheet_comment), file = target_file_path, append = TRUE)
    for (j in seq(1, s)) {
      t <- ""
      if (j < s) {
        t <- ","
      } else {
        t <- paste0(t, ")")
        if (i < n) {
          t <- paste0(t, "),")
        } else {
          t <- paste0(t, ")))")
        }
      }
      t <- paste0(t, "\n")
      cat(sprintf('      %s%s', sheet_columns[[j]], t), file = target_file_path, append = TRUE)
    }
    i <- i + 1
  }
}

#' ROS LL v3.2.1
#' @export
ROS_LL_v3_2_1 <- "ROS_LL_v3_2_1"

#' Generate the R file for the ROS LL v3.2.1, using the \code{json} model.
#'
#' Working directory is in this file directory, adapt if necessary
#' @export
generate_ros_ll_3_2_1_model <- function() {
  json_model_path <- paste0("../models/", ROS_LL_v3_2_1, ".json")
  r_file_path <- paste0("./", ROS_LL_v3_2_1, "_model.R")
  model_content <- load_model(json_model_path)
  write_model(ROS_LL_v3_2_1, model_content, r_file_path)
}

#' To generate ros ll v3.2.1 model
# generate_ros_ll_3_2_1_model()
