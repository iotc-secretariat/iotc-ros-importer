library(DBI)
library(RPostgres)
library(data.table)
library(stringr)
library(R6)

split_table_location <- function(value) {
  unlist(strsplit(value, "\\."))
}

split_column_location <- function(value) {
  unlist(strsplit(value, "→"))
}

split_foreign_key <- function(value) {
  unlist(strsplit(value, ":"))
}

get_row_count <- function(table) {
  dim(table)[1]
}

get_column_count <- function(table) {
  dim(table)[2]
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

ForeignKey <- R6Class(
  "ForeignKey",
  public = list(
    initialize = function(gav) {
      stopifnot(!is.na(gav), is.character(gav), nchar(gav) > 0, gav %like% ".+\\..+→.+:.+")
      split <- split_foreign_key(gav)
      split1 <- split_column_location(split[[1]])
      private$.table <- table_location(split1[[1]])
      private$.column <- split1[[2]]
      private$.foreign_key <- split[[2]]
    },
    table = function() {
      private$.table
    },
    column = function() {
      private$.column
    },
    foreign_key = function() {
      private$.foreign_key
    },
    gav = function() {
      sprintf("%s→%s:%s", self$table()$gav(), self$column(), self$foreign_key())
    },
    print = function() {
      cat(self$gav())
    }
  ),
  private = list(
    # schema + table name
    .table = NULL,
    # column name
    .column = NULL,
    # foreign key name
    .foreign_key = NULL
  )
)

table_location <- function(gav) {
  TableLocation$new(gav)
}

column_location <- function(gav) {
  ColumnLocation$new(gav)
}

foreign_key <- function(gav) {
  ForeignKey$new(gav)
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

#' Generic method to load the content of a given table.
#'
#' @param schema_name name of schema
#' @param table_name name of table
#' @param columns The optional columns to load (if not specified, then will load all columns of the table)
#' @param connection where to load the table content
#' @return the loaded table as a data.Table
#' @export
load_table <- function(schema_name, table_name, columns = NULL, connection) {
  if (is.null(columns)) {
    columns <- "*"
  } else {
    columns <- paste0(columns, collapse = ", ")
  }
  query(
    connection,
    paste0("SELECT ", columns, " FROM ", schema_name, ".", table_name)
  )
}

prepare_query <- function(sql, column_location, other_params = NULL) {
  sql <- str_replace_all(sql, "\\$schema_name", column_location$table()$schema())
  sql <- str_replace_all(sql, "\\$table_name", column_location$table()$table())
  sql <- str_replace_all(sql, "\\$column_name", column_location$column())
  if (!is.null(other_params)) {
    for (key in names(other_params)) {
      sql <- str_replace_all(sql, key, other_params[key])
    }
  }
  sql
}

#' Obtain column locations of all foreign keys on the given \code{location}.
#'
#' @param connection sql connection
#' @param column_location the column location to inspect
#' @export
get_foreign_keys <- function(connection, location) {
  sql0 <- prepare_query("SELECT CONCAT(n.nspname || '.' || t.relname || '→' ||
UNNEST((select array_agg(attname) FROM pg_attribute WHERE attrelid = c.conrelid AND ARRAY [attnum] <@ c.conkey)) || ':' || c.conname)
FROM pg_constraint c
JOIN pg_attribute a ON c.confrelid = a.attrelid AND a.attnum = ANY (confkey)
JOIN pg_namespace n ON n.oid = c.connamespace
JOIN pg_class t ON t.oid = c.conrelid
WHERE a.attname = '$column_name'
  AND c.confrelid = (
    SELECT oid FROM pg_class
    WHERE relname = '$table_name'
    AND relnamespace = (SELECT oid FROM pg_namespace WHERE nspname = '$schema_name'))
  AND c.confrelid != c.conrelid;", location)
  # cat(paste0("SQL0 → ", sql0, "\n"))
  lapply(query(connection, sql0)[[1]], foreign_key)
}

get_schema_names <- function(connection, pattern) {
  sql0 <- paste0("SELECT nspname FROM pg_catalog.pg_namespace WHERE nspname LIKE '", pattern,"';")
  query(connection, sql0)[[1]]
}

get_table_names <- function(connection) {
  sql0 <- paste0("SELECT nspname FROM pg_catalog.pg_namespace WHERE nspname LIKE '", pattern,"';")
  query(connection, sql0)[[1]]
}

replace_id <- function(location, references, id_to_replace, id, output_file) {
  cat(paste0("replace in table ", location$gav(), " id:`", id_to_replace, "` by `", id, "`", "\n"))
  for (reference in references) {
    col <- reference$column()
    cat(file = output_file,
        append = TRUE,
        sprintf("UPDATE %s SET %s = %s WHERE %s = %s;\n", reference$table()$gav(), col, id, col, id_to_replace))
  }
  cat(file = output_file,
      append = TRUE,
      sprintf("DELETE FROM %s WHERE %s = %s;\n", location$table()$gav(), "id", id_to_replace))
}

#' To produce a sql script to remove doubloons on a given table,  method to load the content of a given table.
#'
#' @param connection sql connection
#' @param table_location the table to inspect
#' @param column_name the name of column to find doubloons
#' @param output_dir where to write the sql script
#' @param timestamp time stamp used to generate the script file name
#' @export
clean_doubloons <- function(connection, location, output_dir, timestamp) {
  schema_name <- location$table()$schema()
  table_name <- location$table()$table()
  column_name <- location$column()

  output_file <- sprintf("%s/clean-doubloons-%s_%s_%s-%s.sql", output_dir, schema_name, table_name, column_name, timestamp)
  cat(paste0("Clean doubloons for ", location$gav(), " into`", output_file, "` file.\n"))
  references <- get_foreign_keys(connection, column_location(paste0(location$table()$gav(), "→id")))
  # Query to find all doubloons
  sql1 <- prepare_query("SELECT DISTINCT a.$column_name as value,
(select b.id from $schema_name.$table_name b where a.$column_name = b.$column_name ORDER BY a.$column_name LIMIT 1) as id,
(SELECT COUNT(b.id) FROM $schema_name.$table_name b WHERE a.$column_name = b.$column_name) as count
FROM $schema_name.$table_name a
WHERE (SELECT COUNT(b.id) FROM $schema_name.$table_name b WHERE a.$column_name = b.$column_name) > 1
GROUP BY a.$column_name
ORDER BY a.$column_name;", location)
  # cat(paste0("SQL1 → ", sql1, "\n"))
  data <- query(connection, sql1)
  for (i in seq_len(get_row_count(data))) {
    row <- data[i]
    value <- row$value
    id <- row$id
    cat(paste0("Treat ", location$gav(), " for value `", value, "` using id `", id, "`\n"))
    query_parameters <- c()
    query_parameters["\\$id"] <- as.character(id)
    query_parameters["\\$value"] <- str_replace_all(value, "'", "''")
    sql2 <- prepare_query("SELECT a.id as id
FROM $schema_name.$table_name a
WHERE a.id != $id AND a.$column_name = '$value'
ORDER BY a.id;", location, query_parameters)
    # cat(paste0("SQL2 → ", sql2, "\n"))
    ids <- query(connection, sql2)
    for (id_to_replace in ids) {
      replace_id(location, references, id_to_replace, id, output_file)
    }
  }
  output_file
}

remove_foreign_keys <- function(location, references, output_file) {
  cat(file = output_file,
      append = TRUE,
      sprintf("-- remove %s foreign key(s) from %s\n", length(references), location$gav()))
  for (reference in references) {
    cat(file = output_file,
        append = TRUE,
        sprintf("-- remove the foreign key for %s\n", reference$gav()))
    cat(file = output_file,
        append = TRUE,
        sprintf("ALTER TABLE %s DROP CONSTRAINT %s;\n", reference$table()$gav(), reference$foreign_key()))
  }
}

add_foreign_keys <- function(location, references, output_file) {
  schema_name <- location$table()$schema()
  table_name <- location$table()$table()
  column_name <- location$column()
  cat(file = output_file,
      append = TRUE,
      sprintf("-- add %s foreign key(s) from %s\n", length(references), location$gav()))
  for (reference in references) {
    col <- reference$column()
    fk <- reference$foreign_key()
    cat(file = output_file,
        append = TRUE,
        sprintf("-- add the foreign key %s for %s\n", fk, reference$gav()))
    cat(file = output_file,
        append = TRUE,
        sprintf("ALTER TABLE %s ADD CONSTRAINT %s FOREIGN KEY (%s) REFERENCES %s.%s(%s);\n", reference$table()$gav(), fk, col, schema_name, table_name, column_name))
  }
}

import_observers <- function(observers_import_file, export_file) {
  cat("DELETE FROM ros_common.observer where TRUE;
DELETE FROM ros_common.contact_role where TRUE;
DELETE FROM ros_common.contact where TRUE;
", file = export_file)
  id_mapping <- c()
  iotc_number_mapping <- c()
  observers_import <- as.data.table(fread(observers_import_file))
  for (i in seq_len(get_row_count(observers_import))) {
    row <- observers_import[i]
    old_id <- row$id
    iotc_number <- row$iotc_number
    new_id <- i
    id_mapping[old_id] <- i
    iotc_number_mapping[iotc_number] <- i
    parameters <- paste0(new_id, ", '", row$full_name, "', TRUE")
    insert_sql <- sprintf("INSERT INTO ros_common.contact(id, full_name, active) OVERRIDING SYSTEM VALUE VALUES (%s);\n", parameters)
    cat(insert_sql, file = export_file, append = TRUE)
    insert_sql <- sprintf("INSERT INTO ros_common.contact_role(contact_id, role_code) VALUES(%s,'%s');\n", new_id, "OB")
    cat(insert_sql, file = export_file, append = TRUE)
    insert_sql <- sprintf("INSERT INTO ros_common.observer(contact_id, iotc_observer_identifier) OVERRIDING SYSTEM VALUE VALUES(%s,'%s');\n", new_id, iotc_number)
    cat(insert_sql, file = export_file, append = TRUE)
  }
  list(id_mapping, iotc_number_mapping)
}

import_focal_points <- function(focal_points_import_file, export_file, starting_id, observers_iotc_number_mapping) {
  id_mapping <- c()
  focal_points_import <- as.data.table(fread(focal_points_import_file))
  for (i in seq_len(get_row_count(focal_points_import))) {
    row <- focal_points_import[i]
    old_id <- row$id
    if (row$iotc_number != "") {
      # get id from incoming mapping
      id <- observers_iotc_number_mapping[row$iotc_number]
      id_mapping[old_id] <- id
      # add the fp role
      insert_sql <- sprintf("INSERT INTO ros_common.contact_role(contact_id, role_code) VALUES(%s,'%s');\n", id, "FP")
      cat(insert_sql, file = export_file, append = TRUE)
      # update the contact row
      update_sql <- sprintf("UPDATE ros_common.contact SET nationality_code = '%s' WHERE id = %s;\n", row$nationality_code, id)
      cat(update_sql, file = export_file, append = TRUE)
      if (row$email != "") {
        update_sql <- sprintf("UPDATE ros_common.contact SET email = '%s' WHERE id = %s;\n", row$email, id)
        cat(update_sql, file = export_file, append = TRUE)
      }
      if (row$phone != "") {
        update_sql <- sprintf("UPDATE ros_common.contact SET phone = '%s' WHERE id = %s;\n", row$phone, id)
        cat(update_sql, file = export_file, append = TRUE)
      }
      if (row$contact_details != "") {
        update_sql <- sprintf("UPDATE ros_common.contact SET comment = '%s' WHERE id = %s;\n", str_replace_all(row$contact_details, "'", "''"), id)
        cat(update_sql, file = export_file, append = TRUE)
      }
      next
    }
    starting_id <- starting_id + 1
    new_id <- starting_id
    id_mapping[old_id] <- new_id
    parameters <- paste0(new_id, ", '", row$full_name, "', TRUE")
    if (row$email == "") {
      parameters <- paste0(parameters, ", NULL")
    } else {
      parameters <- paste0(parameters, ", '", row$email, "'")
    }
    if (row$phone == "") {
      parameters <- paste0(parameters, ", NULL")
    } else {
      parameters <- paste0(parameters, ", '", row$phone, "'")
    }
    if (row$contact_details == "") {
      parameters <- paste0(parameters, ", NULL")
    } else {
      parameters <- paste0(parameters, ", '", str_replace_all(row$contact_details, "'", "''"), "'")
    }
    parameters <- paste0(parameters, ", '", row$nationality_code, "'")
    insert_sql <- sprintf("INSERT INTO ros_common.contact(id, full_name, active, email, phone, comment, nationality_code) OVERRIDING SYSTEM VALUE VALUES (%s);\n", parameters)
    cat(insert_sql, file = export_file, append = TRUE)
    insert_sql <- sprintf("INSERT INTO ros_common.contact_role(contact_id, role_code) VALUES(%s,'%s');\n", new_id, "FP")
    cat(insert_sql, file = export_file, append = TRUE)
  }
  id_mapping
}

import_person_details <- function(person_details_import_file, export_file, starting_id, all_skippers, all_fishing_masters) {
  id_mapping <- c()
  person_details_import <- as.data.table(fread(person_details_import_file))
  for (i in seq_len(get_row_count(person_details_import))) {
    row <- person_details_import[i]
    old_id <- row$id
    starting_id <- starting_id + 1
    new_id <- starting_id
    id_mapping[old_id] <- new_id
    parameters <- paste0(new_id, ", '", row$full_name, "', TRUE")
    if (row$nationality_code == "") {
      parameters <- paste0(parameters, ", NULL")
    } else {
      parameters <- paste0(parameters, ", '", row$nationality_code, "'")
    }
    insert_sql <- sprintf("INSERT INTO ros_common.contact(id, full_name, active, nationality_code) OVERRIDING SYSTEM VALUE VALUES (%s);\n", parameters)
    cat(insert_sql, file = export_file, append = TRUE)
    if (old_id %in% all_skippers) {
      insert_sql <- sprintf("INSERT INTO ros_common.contact_role(contact_id, role_code) VALUES(%s,'%s');\n", new_id, "SK")
      cat(insert_sql, file = export_file, append = TRUE)
    }
    if (old_id %in% all_fishing_masters) {
      insert_sql <- sprintf("INSERT INTO ros_common.contact_role(contact_id, role_code) VALUES(%s,'%s');\n", new_id, "FM")
      cat(insert_sql, file = export_file, append = TRUE)
    }
  }
  id_mapping
}

replace_ids <- function(export_file, id_mapping, references) {
  for (i in seq_len(length(id_mapping))) {
    old_id <- i
    new_id <- id_mapping[old_id]

    if (is.na(new_id) || old_id == new_id) {
      next
    }
    # cat(old_id,"→",new_id,"\n")
    for (name in names(references)) {
      reference <- references[[name]]
      col <- reference$column()
      sql <- sprintf("UPDATE %s SET %s = %s WHERE %s = %s;\n", reference$table()$gav(), col, new_id, col, old_id)
      cat(sql, file = export_file, append = TRUE)
    }
  }
}

optimize_vessel_identification <- function(connection, export_file, column, references) {
  id_mapping <- c()
  ids <- query(connection, "SELECT id FROM ros_common.vessel_identification ORDER BY id ASC")[[1]]
  starting_id <- 0
  for (old_id in ids) {
    new_id <- starting_id <- starting_id + 1
    if (old_id == new_id) {
      next
    }
    id_mapping[old_id] <- new_id
  }
  replace_ids(export_file, id_mapping, references, usages)
  gav <- column$table()$gav()
  col <- column$column()
  for (i in seq_len(length(id_mapping))) {
    old_id <- i
    new_id <- id_mapping[old_id]
    if (is.na(new_id) || old_id == new_id) {
      next
    }
    sql <- sprintf("UPDATE %s SET %s = %s WHERE %s = %s;\n", gav, col, new_id, col, old_id)
    cat(sql, file = export_file, append = TRUE)
  }
}

replace_smart_ids <- function(export_file, id_mapping, references, usages) {
  for (reference in references) {
    usage <- usages[[reference$gav()]]
    if (is.null(usage)) {
      next
    }
    col <- reference$column()
    for (i in seq_len(length(id_mapping))) {
      old_id <- i
      new_id <- id_mapping[old_id]
      if (is.na(new_id) ||
        old_id == new_id ||
        !old_id %in% usage) {
        next
      }
      sql <- sprintf("UPDATE %s SET %s = %s WHERE %s = %s;\n", reference$table()$gav(), col, new_id, col, old_id)
      cat(sql, file = export_file, append = TRUE)
    }
  }
}

optimize_table <- function(connection, file_index, export_directory, location) {
  gav <- location$table()$gav()
  column <- location$column()
  index <- file_index
  if (file_index < 10) {
    index <- paste0("0", index)
  }
  export_file <- sprintf("%s/05_%s_%s-%s.sql", export_directory, index, gav, column)
  id_mapping <- c()
  references <- get_foreign_keys(connection, location)
  ids <- query(connection, sprintf("SELECT %s FROM %s  ORDER BY %s ASC;", column, gav, column))[[1]]
  starting_id <- 0
  for (old_id in ids) {
    new_id <- starting_id <- starting_id + 1
    if (old_id == new_id) {
      next
    }
    id_mapping[old_id] <- new_id
  }
  if (is.null(id_mapping)) {
    return(FALSE)
  }
  usages <- c()
  for (reference in references) {
    col <- reference$column()
    sql <- sprintf("SELECT DISTINCT %s FROM %s;", col, reference$table()$gav())
    usage <- query(connection, sql)
    if (get_row_count(usage) > 0) {
      usages[reference$gav()] <- usage
    }
  }
  sql <- sprintf("ALTER TABLE %s ALTER COLUMN %s DROP IDENTITY;\n", gav, column)
  cat(sql, file = export_file, append = TRUE)
  if (!is.null(references)) {
    remove_foreign_keys(location, references, export_file)
  }
  replace_smart_ids(export_file, id_mapping, references, usages)
  for (i in seq_len(length(id_mapping))) {
    old_id <- i
    new_id <- id_mapping[old_id]
    if (is.na(new_id) || old_id == new_id) {
      next
    }
    sql <- sprintf("UPDATE %s SET %s = %s WHERE %s = %s;\n", gav, column, new_id, column, old_id)
    cat(sql, file = export_file, append = TRUE)
  }
  sql <- sprintf("ALTER TABLE %s ALTER COLUMN %s ADD GENERATED ALWAYS AS IDENTITY;\n", gav, column)
  cat(sql, file = export_file, append = TRUE)
  if (!is.null(references)) {
    add_foreign_keys(location, references, export_file)
  }
  TRUE
}

optimize_tables <- function(connection, export_directory, locations) {
  file_index <- 1
  table_index <- 0
  for (i in locations) {
    cat(sprintf("Treat table: %s (%s/%s)...\n", i, table_index <- table_index + 1, length(locations)))
    with_result <- optimize_table(connection, file_index <- file_index + 1, export_directory, column_location(i))
    if (!with_result) {
      file_index <- file_index - 1
    }
  }
}