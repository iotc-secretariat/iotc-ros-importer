library(DBI)
library(RPostgres)
library(data.table)
library(stringr)

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
