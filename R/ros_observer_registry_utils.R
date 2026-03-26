library(data.table)
library(stringr)

#' Load the observer resources from the ROS database.
#'
#' The result is a \code{list} with entries:
#' \code{contact_table, contact_role_table, observer_table, observer_identifier_mapping_table, contact_sequence_value}
#'
#' @param connection_supplier connection supplier (the connection will be closed inside the method)
#' @return the loaded list
#' @export
load_observer_ros_tables <- function(connection_supplier) {
  connection <- NULL
  tryCatch({
    connection <- do.call(connection_supplier, args = list())
    list(
      contact_table = load_table("ros_common", "contact", columns = NULL, connection),
      contact_role_table = load_table("ros_common", "contact_role", columns = NULL, connection),
      observer_table = load_table("ros_common", "observer", columns = NULL, connection),
      observer_identifier_mapping_table = load_table("ros_common", "observer_identifier_mapping", columns = NULL, connection),
      contact_sequence_value = get_sequence_value("ros_common", "contact", connection)
    )
  }, finally = {
    if (!is.null(connection)) {
      RPostgres::dbDisconnect(connection)
    }
  })
}

#' Load the observer registry input file (in xlsx format).
#'
#' The result is a \code{data.table} with columns:
#' \code{full_name, iotc_observer_identifier, legacy_iotc_observer_identifier, nationality_code}
#'
#' all columns are trimmed and \code{full_name} is a concatenaiton of \code{last_name} and \code{first_name}
#' with no accents.
#'
#' We also checks that we have no \code{legacy_iotc_observer_identifier} doublons (otherwise an error is thrown).
#'
#' @param input_file the input file to load
#' @return the loaded cleaned data.table
#' @export
load_observer_registry_file <- function(input_file) {
  if (!file.exists(input_file)) {
    stop(simpleError(sprintf("File `%s` not found.", input_file)))
  }
  frame <- openxlsx::read.xlsx(xlsxFile = input_file,
                               colNames = TRUE,
                               sheet = "Observers")
  result <- as.data.table(frame)[, .(last_name, first_name, ob_iotc_id, legacy_iotc_number, nationality_code)]
  result <- result[, `:=`(iotc_observer_identifier = ob_iotc_id, legacy_iotc_observer_identifier = legacy_iotc_number)]
  result <- result[, last_name := str_trim(iconv(last_name, to = "ASCII//TRANSLIT"))]
  result <- result[, first_name := str_trim(iconv(first_name, to = "ASCII//TRANSLIT"))]
  result <- result[, full_name := ifelse(is.na(first_name), last_name, sprintf("%s %s", last_name, first_name))]
  result <- result[, iotc_observer_identifier := str_trim(iotc_observer_identifier)]
  result <- result[, legacy_iotc_observer_identifier := str_trim(legacy_iotc_observer_identifier)]
  result <- result[, nationality_code := str_trim(nationality_code)]
  result <- result[, .(full_name, iotc_observer_identifier, legacy_iotc_observer_identifier, nationality_code)]
  # check if there is no observer with no iotc_observer_identifier
  observers_with_no_identifier <- result[is.na(iotc_observer_identifier)]
  if (get_row_count(observers_with_no_identifier) > 0) {
    stop(simpleError(sprintf("There is some observer(s) with no identifiers: %s.", toString(observers_with_no_identifier$full_name))))
  }
  # check if there is no doublon one legacy identifiers
  legacy_doublons <- result[, .N, .(legacy_iotc_observer_identifier)][N > 1][!is.na(legacy_iotc_observer_identifier)]
  if (get_row_count(legacy_doublons) > 0) {
    stop(simpleError(sprintf("There is some legacy observer identifier(s) in doublon: %s.", toString(legacy_doublons$legacy_iotc_observer_identifier))))
  }
  result
}

add_missing_contacts <- function(input_data, db_tables, output_file) {
  contact_table <- db_tables$contact_table
  missing_contact <- unique(input_data[!full_name %in% contact_table$full_name], by = c("full_name", "iotc_observer_identifier", "nationality_code"))
  print(sprintf("Found %s observer(s) to add in Ros database", get_row_count(missing_contact)))
  doublon_full_names <- missing_contact[, .N, .(full_name)][N > 1]
  if (get_row_count(doublon_full_names) > 0) {
    print(sprintf("Can't add %s new contact(s), there is some doublon on full name: %s", get_row_count(doublon_full_names), toString(doublon_full_names$full_name)))
  }
  doublon_iotc_observer_identifier <- missing_contact[iotc_observer_identifier %in% db_tables$observer_table$iotc_observer_identifier]$iotc_observer_identifier
  if (length(doublon_iotc_observer_identifier) > 0) {
    print(sprintf("Can't add %s new contact(s), their iotc_observer_identifier are already in database: %s", get_row_count(doublon_full_names), toString(doublon_iotc_observer_identifier)))
  }
  contact_sequence_value <- db_tables$contact_sequence_value
  added_count <- 0
  result <- list()
  for (i in seq(1:get_row_count(missing_contact))) {
    row <- missing_contact[i]
    if (row$full_name %in% doublon_full_names$full_name) {
      next
    }
    if (row$iotc_observer_identifier %in% doublon_iotc_observer_identifier) {
      next
    }
    added_count <- added_count + 1
    contact_id <- contact_sequence_value + added_count
    full_name <- simple_quote(row$full_name)
    result[[full_name]] <- contact_id
    nationality_code <- simple_quote(row$nationality_code)
    iotc_observer_identifier <- simple_quote(row$iotc_observer_identifier)
    cat(
      sprintf("-- contact[%s] full_name=%s, nationality_code=%s, iotc_observer_identifier=%s", added_count, full_name, nationality_code, iotc_observer_identifier),
      sprintf("INSERT INTO ros_common.contact(full_name, nationality_code) VALUES(%s, %s);", full_name, nationality_code),
      sprintf("INSERT INTO ros_common.contact_role(contact_id, role_code) VALUES(%s, 'OB');", contact_id),
      sprintf("INSERT INTO ros_common.observer(contact_id, iotc_observer_identifier) VALUES(%s, %s);", contact_id, iotc_observer_identifier),
      file = output_file, sep = "\n", append = TRUE)
  }
  print(sprintf("Added %s observer(s) into add in Ros database", added_count))
  result
}

add_missing_legacy_observer_identifiers <- function(input_data, ros_db_tables, added_contacts_id_mapping, output_file) {
  existing_identifiers <- ros_db_tables$
    observer_table$
    iotc_observer_identifier
  old_mapping <- ros_db_tables$observer_identifier_mapping_table[, .(iotc_observer_identifier, legacy_iotc_observer_identifier)]
  new_mapping <- input_data[, .(iotc_observer_identifier, legacy_iotc_observer_identifier)]
  result <- list()
  for (i in seq(1:get_row_count(new_mapping))) {
    input_row <- new_mapping[i]
    input_iotc_observer_identifier <- input_row$iotc_observer_identifier
    if (!input_iotc_observer_identifier %in% existing_identifiers) {
      # If identifier does not exist in the Ros database, do nothing
      next
    }
    input_legacy_iotc_observer_identifier <- input_row$legacy_iotc_observer_identifier
    if (is.na(input_legacy_iotc_observer_identifier)) {
      # we should remove from Ros database? Not a very good idea, these data should never be removed...
      next
    }
    old_row <- old_mapping[legacy_iotc_observer_identifier == input_legacy_iotc_observer_identifier]
    if (get_row_count(old_row) == 0) {
      # Need to add it
      result[[input_legacy_iotc_observer_identifier]] <- input_iotc_observer_identifier
      cat(
        sprintf("INSERT INTO ros_common.observer_identifier_mapping(legacy_iotc_observer_identifier, iotc_observer_identifier) VALUES(%s, %s);", simple_quote(input_legacy_iotc_observer_identifier), simple_quote(input_iotc_observer_identifier)),
        file = output_file, sep = "\n", append = TRUE)
    }
  }
  result
}

generate_sql_update <- function(connectionSupplier, input_file, output_file_add_observers, output_file_update_observer_legacy_mapping_from_registry) {
  input_data <- load_observer_registry_file(input_file)
  ros_db_tables <- load_observer_ros_tables(connectionSupplier)
  added_contacts_id_mapping <- add_missing_contacts(input_data, ros_db_tables, output_file_add_observers)
  added_identifiers <- add_missing_legacy_observer_identifiers(input_data, ros_db_tables, added_contacts_id_mapping, output_file_update_observer_legacy_mapping_from_registry)
  list(added_contacts_id_mapping, added_identifiers)
}

# To test it
result <- generate_sql_update(
  DB_IOTC_ROS,
"../iotc-ros-input-data/IOTC_ROS_Observers_Registry-2026-03-25.xlsx",
  "../iotc-ros-model/3.3.0/sql/add_observers.sql",
  "../iotc-ros-model/3.3.0/sql/update_observer_legacy_mapping_from_registry.sql"
)

clean_full_name <- function(value) {
  ifelse(is.null(value), value, str_to_upper(str_trim(iconv(value, to = "ASCII//TRANSLIT"))))
}

compute_full_name_hash <- function(full_name) {
  lapply(str_split(clean_full_name(full_name), " "), function(x) { str_replace_all(toString(sort(x)), ", ", "") })
}

compute_full_name_hash("tony Chémit ")

compute_full_names_hash <- function(contact_table) {
  # result <- contact_table[, full_name_parts := unlist(lapply(str_split(full_name, " "), function(x) { str_replace_all(toString(sort(x)), ", ", "") }))]
  result <- contact_table[, full_name_parts := unlist(lapply(full_name, compute_full_name_hash))]
  setorder(result, id)
  result
}

# input_data <- load_observer_registry_file("../iotc-ros-input-data/IOTC_ROS_Observers_Registry-2026-03-25.xlsx")
ros_db_tables <- load_observer_ros_tables(DB_IOTC_ROS)
# added_contacts_id_mapping <- add_missing_contacts(input_data, ros_db_tables, "../iotc-ros-model/3.3.0/sql/2026-03-20/08_12_add_observers.sql")
# added_identifiers <- add_missing_legacy_observer_identifiers(input_data, ros_db_tables, added_contacts_id_mapping, "../iotc-ros-model/3.3.0/sql/2026-03-20/08_13_update_observer_legacy_mapping_from_registry.sql")

contact_table <- compute_full_names_hash(ros_db_tables$contact_table)
a_doublon_full_names <- contact_table[, .(id, full_name, .N), .(full_name_parts)][N > 1]
a_observers <- ros_db_tables$observer_table[contact_id %in% a_doublon_full_names$id]
a_full_observers <- a_doublon_full_names[a_observers, on = .(id = contact_id)]
a_full_contact <- a_doublon_full_names[!id %in% a_observers$contact_id]
# ye <- cbind(doublon_full_names, ya)