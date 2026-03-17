library(jsonlite)

#' ROS LL v3.2.1 constant
#' @export
ROS_LL_v3_2_1 <- "ROS_LL_v3_2_1"

#' ROS PS v3.2.1 constant
#' @export
ROS_PS_v3_2_1 <- "ROS_PS_v3_2_1"

#' ROS GN v3.2.1 constant
#' @export
ROS_GN_v3_2_1 <- "ROS_GN_v3_2_1"

#' ROS PL v3.2.1 constant
#' @export
ROS_PL_v3_2_1 <- "ROS_PL_v3_2_1"


split_meta_cell_location <- function(value) {
  unlist(strsplit(value, "\\:"))
}

load_model <- function(path) {
  content <- fromJSON(path)
  real_content <- content[[1]]
  sheet_names <- names(real_content)
  result <- list()
  meta_sheet <- NULL
  for (i in sheet_names) {
    if (grepl("META", i)) {
      meta_sheet <- load_meta_sheet(i, real_content)
    } else {
      sheet <- load_sheet(i, real_content)
      result[i] <- list(sheet)
    }
  }
  list("meta" = meta_sheet, "sheets" = result)
}

load_meta_sheet <- function(name, content) {
  sheet_content <- content[[name]]
  sheet_comment <- sheet_content$comment
  sheet_cells <- sheet_content$cells
  types <- names(sheet_cells)
  cell_types <- c()
  result_cells <- list()
  index <- 1
  for (i in types) {
    if (grepl("Meta", i)) {
      cell_types[index] <- i
      index <- index + 1
    }
  }
  index <- 1
  cells <- sheet_cells$cell
  locations <- sheet_cells$location
  expecteds <- sheet_cells$expected
  formats <- sheet_cells$format
  for (j in seq(1, nrow(sheet_cells))) {
    for (i in cell_types) {
      tmp <- sheet_cells[[i]]
      y <- tmp[[j]]
      cell <- cells[j]
      location <- locations[j]
      expected <- expecteds[j]
      format <- formats[j]
      with_expected <- !is.na(expected)
      with_format <- !is.na(format)
      with_location <- !is.na(location)
      if (!is.na(tmp[[j]])) {
        if (grepl("MandatoryMetaCell", i)) {
          l <- split_meta_cell_location(cell)
          e <- ""
          if (with_expected) {
            e <- sprintf(', expected = "%s"', expected)
          }
          if (with_format) {
            e <- sprintf(', format = "%s"', format)
          }
          if (with_location) {
            e <- sprintf(', column_location = column_location("%s")', location)
          }
          t <- sprintf('mandatory_meta_cell("%s", %s, %s%s)', y, l[1], l[2], e)
          result_cells <- append(result_cells, t)
        } else if (grepl("OptionalMetaCell", i)) {
          l <- split_meta_cell_location(cell)
          e <- ""
          if (with_expected) {
            e <- sprintf(', expected = "%s"', expected)
          }
          if (with_format) {
            e <- sprintf(', format = "%s"', format)
          }
          if (with_location) {
            e <- sprintf(', column_location = column_location("%s")', location)
          }
          t <- sprintf('optional_meta_cell("%s", %s, %s%s)', y, l[1], l[2], e)
          result_cells <- append(result_cells, t)
        }
      }
    }
  }
  result <- list(comment = sheet_comment, cells = result_cells)
  cat("\n")
  result
}

load_sheet <- function(name, content) {
  sheet_content <- content[[name]]
  sheet_comment <- sheet_content$comment
  sheet_pk <- sheet_content$pk
  sheet_comluns <- sheet_content$columns
  types <- names(sheet_comluns)
  column_types <- c()
  result_columns <- list()
  index <- 1
  for (i in types) {
    if (grepl("Column", i)) {
      column_types[index] <- i
      index <- index + 1
    }
  }
  index <- 1
  with_checks <- "checks" %in% types
  if (with_checks) {
    checkss <- sheet_comluns[["checks"]]
  }
  with_actions <- "actions" %in% types
  if (with_actions) {
    actionss <- sheet_comluns[["actions"]]
  }
  measurement_tables <- sheet_comluns[["measurement_table"]]
  with_fks <- "fk" %in% types
  if (with_fks) {
    fks <- sheet_comluns[["fk"]]
  }
  locations <- sheet_comluns[["location"]]
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
        }
        check <- ""
        if (with_checks) {
          checks <- load_checks(checkss[[j]])
          if (str_length(checks) > 0) {
            check <- sprintf(", checks = %s", checks)
          }
        }
        action <- ""
        if (with_actions) {
          actions <- load_actions(actionss[[j]])
          if (str_length(actions) > 0) {
            action <- sprintf(", actions = %s", actions)
          }
        }
        if (with_fks) {
          fk <- fks[j]
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
          t <- sprintf('mandatory_simple_column("%s", column_location("%s")%s%s%s)', y, location, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalSimpleColumn", i)) {
          t <- sprintf('optional_simple_column("%s", column_location("%s")%s%s%s)', y, location, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryForeignKeyColumn", i)) {
          t <- sprintf('mandatory_fk_column("%s", column_location("%s"), column_location("%s")%s%s%s)', y, location, fk, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalForeignKeyColumn", i)) {
          t <- sprintf('optional_fk_column("%s", column_location("%s"), column_location("%s")%s%s%s)', y, location, fk, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryAssociationForeignKeyColumn", i)) {
          t <- sprintf('mandatory_association_fk_column("%s", column_location("%s"), column_location("%s")%s%s%s)', y, location, fk, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalAssociationForeignKeyColumn", i)) {
          t <- sprintf('optional_association_fk_column("%s", column_location("%s"), column_location("%s")%s%s%s)', y, location, fk, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryMeasurementValueColumn", i)) {
          t <- sprintf('mandatory_measurement_column("%s", column_location("%s"), "%s"%s%s%s%s)', y, location, measurement_tables[j], unit, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalMeasurementValueColumn", i)) {
          t <- sprintf('optional_measurement_column("%s", column_location("%s"), "%s"%s%s%s%s)', y, location, measurement_tables[j], unit, comment, check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryMeasurementUnitColumn", i)) {
          t <- sprintf('mandatory_measurement_unit_column("%s", %s%s%s)', y, unitss[j], check, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalMeasurementUnitColumn", i)) {
          t <- sprintf('optional_measurement_unit_column("%s", %s%s%s)', y, unitss[j], check, action)
          result_columns <- append(result_columns, t)
        }
      }
    }
  }
  result <- list(comment = sheet_comment, pk = sheet_pk, columns = result_columns)
  cat("\n")
  result
}


CHECKS <- c("exists" = function(parameters) {
  "check_exists()"
}, "should_exists" = function(parameters) {
  "check_should_exists()"
})

ACTIONS <- c("newQuery" = function(parameters) {
  result <- "new_query("
  if (!is.null(parameters)) {
    result <- paste0(result, "\"", parameters, "\"")
  }
  result <- paste0(result, ")")
  result
},
             "newMeasurementQuery" = function(parameters) { "new_measurement_query()" },
             "newAssociationQuery" = function(parameters) {
               real_parameters <- unlist(strsplit(parameters, "\\|"))
               sprintf("new_association_query(\"%s\", column_location(\"%s\"))", real_parameters[[1]], real_parameters[[2]])
             },
             "flushMeasurementQuery" = function(parameters) { "flush_measurement_query()" },
             "flushQuery" = function(parameters) { "flush_query()" },
             "addColumn" = function(parameters) { "add_column()" },
             "getId" = function(parameters) { "get_id()" },
             "addFkColumn" = function(parameters) {
               sprintf("add_fk_column(column_location(\"%s\"))", parameters[[1]])
             },
             "addExternalFkColumn" = function(parameters) {
               real_parameters <- unlist(strsplit(parameters, "\\|"))
               sprintf("add_external_fk_column(\"%s\", column_location(\"%s\"))", real_parameters[[1]], real_parameters[[2]])
             }
)

load_checks <- function(checks) {
  i <- 1
  result <- ""
  max <- length(checks)
  for (check in checks) {
    if (grepl("\\:", check)) {
      check_parts <- unlist(strsplit(check, ":"))
      check_type <- check_parts[[1]]
      check_arguments <- check_parts[[2]]
    } else {
      check_type <- check
      check_arguments <- NULL
    }
    if (!check_type %in% names(CHECKS)) {
      cat("ERROR: can't find check type: ", check_type)
    }
    generator <- CHECKS[[check_type]]
    generator_result <- generator(check_arguments)
    result <- paste0(result, generator_result)
    if (i < max) {
      result <- paste0(result, ", ")
    }
    i <- i + 1
  }
  if (str_length(result) == 0) {
    return("")
  }
  paste0(" c(", result, ")")
}

load_actions <- function(actions) {
  i <- 1
  result <- ""
  max <- length(actions)
  # cat("\n")
  # cat(actions)
  for (action in actions) {
    if (grepl("\\:", action)) {
      action_parts <- unlist(strsplit(action, ":"))
      action_type <- action_parts[[1]]
      action_arguments <- action_parts[[2]]
    } else {
      action_type <- action
      action_arguments <- NULL
    }
    # cat("\naction-type: ")
    # cat(action_type)
    # cat("\n")
    if (!action_type %in% names(ACTIONS)) {
      cat("ERROR: can't find action type: ", action_type)
    }
    generator <- ACTIONS[[action_type]]
    generator_result <- generator(action_arguments)
    result <- paste0(result, generator_result)
    if (i < max) {
      result <- paste0(result, ", ")
    }
    i <- i + 1
  }
  if (str_length(result) == 0) {
    return("")
  }
  paste0(" c(", result, ")")
}

write_model <- function(name, global_content, target_file_path) {
  meta_sheet_content <- global_content$meta
  content <- global_content$sheets
  sheet_names <- names(content)
  cat(paste0("#' The import file model for the ", name, "\n"), file = target_file_path)
  cat("#' @export\n", file = target_file_path, append = TRUE)
  variable_name <- paste0(name, "_MODEL")
  cat(paste0(variable_name, " <-\n"), file = target_file_path, append = TRUE)
  cat(sprintf('  import_file("%s", \n', name), file = target_file_path, append = TRUE)
  sheet_columns <- meta_sheet_content[["cells"]]
  s <- length(sheet_columns)
  cat('    meta_sheet("META", c(\n', file = target_file_path, append = TRUE)
  i <- 1
  for (j in seq(1, s)) {
    t <- ""
    if (j < s) {
      t <- ","
    } else {
      t <- paste0(t, ")), c(")
    }
    t <- paste0(t, "\n")
    cat(sprintf('      %s%s', sheet_columns[[j]], t), file = target_file_path, append = TRUE)
  }
  # cat(' c(\n', file = target_file_path, append = TRUE)
  n <- length(sheet_names)
  for (sheet_name in sheet_names) {
    sheet_content <- content[[sheet_name]]
    sheet_comment <- sheet_content[["comment"]]
    sheet_columns <- sheet_content[["columns"]]
    sheet_pk <- sheet_content[["pk"]]
    s <- length(sheet_columns)
    cat(sprintf('    sheet("%s", "%s", %s, c(\n', sheet_name, sheet_comment, sheet_pk), file = target_file_path, append = TRUE)
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
  tmp <- "
#' Load xls content using %s
#'
#' @param file The xls file to import
#' @return the loaded xls as a list of data.table (one per none empty sheet)
#' @export
load_xls_%s <- function(file) {
  load_xls(file, %s)
}

#' Create import context using %s
#'
#' @param file path to xsl file to load
#' @param connection jdbc connectoion to db
#' @param timestamp current timestamp used for log_file name
#' @return the import context (See ImportContext class)
#' @export
import_context_%s <- function(file, connection, extra_data_tables, data_tables_caches, timestamp) {
  import_context(%s, file, connection, extra_data_tables, data_tables_caches, timestamp)
}
"
  cat(sprintf(tmp, variable_name, name, variable_name, variable_name, name, variable_name), file = target_file_path, append = TRUE)
}

#' Generate the R file for the given ROS model name, using his \code{json} model.
#'
#' @param model_name to use
#' Working directory is in this file directory, adapt if necessary
#' @export
generate_model <- function(model_name) {
  json_model_path <- paste0("./models/", model_name, ".json")
  r_file_path <- paste0("./R/", model_name, "_model.R")
  model_content <- load_model(json_model_path)
  write_model(model_name, model_content, r_file_path)
}

# TODO
# review json format → replace column type by flags and make the column name
#
#
