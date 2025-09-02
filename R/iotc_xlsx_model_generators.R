library("jsonlite")

#' ROS LL v3.2.1
#' @export
ROS_LL_v3_2_1 <- "ROS_LL_v3_2_1"


split_location <- function(value) {
  unlist(strsplit(value, "→"))
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
    if (grepl("Column", i)) {
      column_types[index] <- i
      index <- index + 1
    }
  }
  index <- 1
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
          if (!is.na(location)) {
            l <- split_location(location)
          }
        }
        action <- ""
        if (with_actions) {
          actions <- load_actions(actionss[[j]])
          if (!is.na(actions)) {
            action <- sprintf(", actions = %s", actions)
          }
        }
        if (with_fks) {
          fk <- fks[j]
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
          t <- sprintf('mandatory_simple_column("%s", column_location("%s", "%s")%s%s)', y, l[[1]], l[[2]], comment, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalSimpleColumn", i)) {
          t <- sprintf('optional_simple_column("%s", column_location("%s", "%s")%s%s)', y, l[[1]], l[[2]], comment, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryForeignKeyColumn", i)) {
          t <- sprintf('mandatory_fk_column("%s", column_location("%s", "%s"), column_location("%s", "%s")%s%s)', y, l[[1]], l[[2]], f[[1]], f[[2]], comment, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalForeignKeyColumn", i)) {
          t <- sprintf('optional_fk_column("%s", column_location("%s", "%s"), column_location("%s", "%s")%s%s)', y, l[[1]], l[[2]], f[[1]], f[[2]], comment, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryMeasurementValueColumn", i)) {
          t <- sprintf('mandatory_measurement_column("%s", column_location("%s", "%s"), "%s"%s%s%s)', y, l[[1]], l[[2]], measurement_tables[j], unit, comment, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalMeasurementValueColumn", i)) {
          t <- sprintf('optional_measurement_column("%s", column_location("%s", "%s"), "%s"%s%s%s)', y, l[[1]], l[[2]], measurement_tables[j], unit, comment, action)
          result_columns <- append(result_columns, t)
        } else if (grepl("MandatoryMeasurementUnitColumn", i)) {
          t <- sprintf('mandatory_measurement_unit_column("%s", %s%s)', y, unitss[j], action)
          result_columns <- append(result_columns, t)
        } else if (grepl("OptionalMeasurementUnitColumn", i)) {
          t <- sprintf('optional_measurement_unit_column("%s", %s%s)', y, unitss[j], action)
          result_columns <- append(result_columns, t)
        }
      }
    }
  }
  result <- list(comment = sheet_comment, columns = result_columns)
  cat("\n")
  result
}

ACTIONS <- c("newQuery" = function(parameters) {
  result <- "new_query("
  if (!is.null(parameters)) {
    result <- paste0(result, "\"", parameters, "\"")
  }
  result <- paste0(result, ")")
  result
},
             "newMeasurementQuery" = function(parameters) { "new_measurement_query()" },
             "flushMeasurementQuery" = function(parameters) { "flush_measurement_query()" },
             "flushQuery" = function(parameters) { "flush_query()" },
             "addColumn" = function(parameters) { "add_column()" },
             "addFkColumn" = function(parameters) {
               column_location <- split_location(parameters[[1]])
               sprintf("add_fk_column(column_location(\"%s\", \"%s\"))", column_location[[1]], column_location[[2]])
             },
             "addExternalFkColumn" = function(parameters) {
               real_parameters <- unlist(strsplit(parameters, "\\|"))
               column_location <- split_location(real_parameters[[2]])
               sprintf("add_external_fk_column(\"%s\", column_location(\"%s\", \"%s\"))", real_parameters[[1]], column_location[[1]], column_location[[2]])
             }
)

load_actions <- function(actions) {
  i <- 1
  result <- " c("
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
  result <- paste0(result, ")")
  # cat(" → ")
  # cat(result)
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
generate_ros_ll_3_2_1_model()
