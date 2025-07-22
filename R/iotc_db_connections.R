library("iotc.core.db.connections")
library("RPostgres")

SERVER_CACHE <- new.env(hash = TRUE)
CREDENTIALS_CACHE <- new.env(hash = TRUE)

#'The constants holding the name of the IOTC_Ros database
#'@export
IOTC_ROS <- "IOTC_Ros"

#'The constants holding the name of the IOTC_ReferenceData database
#'@export
IOTC_REFERENCE_DATA <- "IOTC_ReferenceData"

#' Connects to an instance of \code{IOTC_ROS} on a given server machine
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "IOTC ReferenceData" database name (defaults to \code{\link{IOTC_ROS}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param client_encoding The character set used by the client (defaults to UTF-8)
#' @return An Sql connection to \code{IOTC_ROS} on \code{server}
#' @export
DB_IOTC_ROS <- function(server = get_default_db_server(),
                        database = IOTC_ROS,
                        username = get_username_for_db(IOTC_ROS),
                        password = get_password_for_db(IOTC_ROS),
                        client_encoding = "UTF-8") {
  return(connect_to_pg(server, database, username, password, client_encoding))
}

#' Connects to an instance of \code{IOTC_REFERENCE_DATA} on a given server machine
#'
#' @param server The server name / IP address (defaults to \code{\link{SERVER_DEFAULT}})
#' @param database The "IOTC ReferenceData" database name (defaults to \code{\link{IOTC_REFERENCE_DATA}})
#' @param username The username (defaults to the standard one for this specific DB)
#' @param password The password (defaults to the standard one for this specific DB)
#' @param client_encoding The character set used by the client (defaults to UTF-8)
#' @return An Sql connection to \code{IOTC_REFERENCE_DATA} on \code{server}
#' @export
DB_IOTC_REFERENCE_DATA <- function(server = get_default_db_server(),
                                   database = IOTC_REFERENCE_DATA,
                                   username = get_username_for_db(IOTC_ROS),
                                   password = get_password_for_db(IOTC_ROS),
                                   client_encoding = "UTF-8") {
  return(connect_to_pg(server, database, username, password, client_encoding))
}

connect_to_pg <- function(server = get_default_db_server(),
                          database,
                          username,
                          password,
                          client_encoding = "UTF-8") {
  DEBUG <- db_debug_connections()
  if (DEBUG) l_info(paste("Connecting to", database, "on", server, "using", username, "as username"))
  return(dbConnect(RPostgres::Postgres(),
                   host = server,
                   dbname = database,
                   user = username,
                   password = password,
                   client_encoding = client_encoding))
}
