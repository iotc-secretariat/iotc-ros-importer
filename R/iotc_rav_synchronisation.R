import_rav <- function(file) {
  erav_all_records <- fread(file)
  erav_all_records[, authorized_from := as.Date(authorized_from)]
  erav_all_records[, authorized_to := as.Date(authorized_to)]
  # Select and format records
  unique(erav_all_records[
           !is.na(authorized_from) & authorized_from >= as.Date("2000-01-01"),
           .(imo_number = imo,
             iotc_number = iotc_no,
             ircs,
             vessel_name = trimws(toupper(vessel_name)),
             registration_number = registration_no,
             main_fishing_gear_code = vessel_type,
             flag_code = flag_country,
             port_code = trimws(toupper(port_of_registration)),
             authorized_from,
             authorized_to)
         ]
  )
}

loaded_ros_vessel_identifications <- query(connection, "SELECT
  id,
  imo_identifier,
  iotc_vessel_identifier,
  ircs_identifier,
  vessel_name,
  registration_identifier,
  main_fishing_gear_code,
  flag_code,
  port_code
  FROM ros_common.vessel_identification ORDER BY id
  ASC")

rav_data <- import_rav("../iotc-ros-input-data/rav_export_record_history_20251107.csv")

export_file <- "../iotc-ros-model/3.3.0/sql/2025-11-13/update_ros_vessel.sql"

vessel_identification_column <- column_location("ros_common.vessel_identification→id")
vessel_identification_foreign_keys <- get_foreign_keys(connection, vessel_identification_column)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 31, 26, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 46, 36, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 69, 39, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 71, 41, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 75, 53, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 76, 56, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 77, 68, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 83, 43, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 84, 43, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 127, 122, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 141, 130, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 132, 125, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 156, 155, export_file)
replace_id(vessel_identification_column, vessel_identification_foreign_keys, 145, 108, export_file)

update_ros_vessel_identification <- function(loaded_ros_vessel_identifications, rav_data, export_file, exclude_colum_names = c(), exclude_iotc_vessel_identifiers = c()) {
  not_found_count <- 0
  if (file.exists(export_file)) {
    file.remove(export_file)
  }

  check_change <- function(row_id, vessel_name, ros_column, rav_column, ros_row, rav_row, parameters) {
    ros_value <- ros_row[[ros_column]]
    rav_value <- rav_row[[rav_column]]
    if (ros_column %in% exclude_colum_names) {
      return(parameters)
    }
    if (rav_value == "NULL") {
      rav_value <- NA
    }
    if (is.na(ros_value) && is.na(ros_value)) {
      return(parameters)
    }
    if (is.na(ros_value) || rav_value != ros_value) {
      if (is.na(rav_value) && !is.na(ros_value)) {
        return(parameters)
      }
      cat(file = export_file, append = TRUE, sprintf("-- For id = %s (vessel_name: %s), change the value of the column '%s' from '%s' to '%s'.\n",
                                                     row_id,
                                                     vessel_name,
                                                     ros_column,
                                                     ros_value,
                                                     rav_value))
      parameters <- paste0(parameters, ", ", ros_column, " = ")
      if (rav_value == "" ||
        rav_value == "NULL" ||
        rav_value == "NA" ||
        is.na(rav_value) ||
        is.null(rav_value)) {
        parameters <- paste0(parameters, "NULL")
      } else {
        parameters <- paste0(parameters, "'", str_replace_all(rav_value, "'", "''"), "'")
      }
    }
    parameters
  }

  for (i in seq_len(get_row_count(loaded_ros_vessel_identifications))) {
    ros_now <- loaded_ros_vessel_identifications[i]
    row_id <- ros_now$id
    .vessel_name <- str_to_upper(ros_now$vessel_name)
    .iotc_vessel_identifier <- ros_now$iotc_vessel_identifier
    if (.iotc_vessel_identifier %in% exclude_iotc_vessel_identifiers) {
      next
    }
    .imo_identifier <- ros_now$imo_identifier
    .ircs_identifier <- ros_now$ircs_identifier
    .registration_identifier <- ros_now$registration_identifier
    rav_row <- rav_data[vessel_name == .vessel_name
                          |
                          iotc_number == .iotc_vessel_identifier
                        # |
                        # imo_number == .imo_identifier
                        # |
                        # ircs == .ircs_identifier
                        # |
                        # registration_number == .registration_identifier
      ,]
    rav_row_count <- get_row_count(rav_row)
    if (rav_row_count == 0) {
      cat(sprintf("No match for vessel (id %s, vessel_name %s, iotc_vessel_identifier %s, imo_identifier %s, ircs_identifier %s, registration_identifier %s)\n",
                  row_id,
                  .vessel_name,
                  .iotc_vessel_identifier,
                  .imo_identifier,
                  .ircs_identifier,
                  .registration_identifier))
      not_found_count <- not_found_count + 1
      next
    }
    if (rav_row_count > 1) {
      rav_row <- rav_row[rav_row_count]
    }
    parameters <- ""
    # imo_number → imo_identifier
    parameters <- check_change(row_id, .vessel_name, "imo_identifier", "imo_number", ros_now, rav_row, parameters)
    # iotc_number → iotc_vessel_identifier
    parameters <- check_change(row_id, .vessel_name, "iotc_vessel_identifier", "iotc_number", ros_now, rav_row, parameters)
    # ircs → ircs_identifier
    parameters <- check_change(row_id, .vessel_name, "ircs_identifier", "ircs", ros_now, rav_row, parameters)
    # vessel_name → vessel_name
    parameters <- check_change(row_id, .vessel_name, "vessel_name", "vessel_name", ros_now, rav_row, parameters)
    # registration_number → registration_identifier
    parameters <- check_change(row_id, .vessel_name, "registration_identifier", "registration_number", ros_now, rav_row, parameters)

    # main_fishing_gear_code → main_fishing_gear_code
    # FIXME The mapping is not correct in rav, need to add a mapping
    parameters <- check_change(row_id, .vessel_name, "main_fishing_gear_code", "main_fishing_gear_code", ros_now, rav_row, parameters)
    # flag_code → flag_code
    parameters <- check_change(row_id, .vessel_name, "flag_code", "flag_code", ros_now, rav_row, parameters)
    if (str_length(parameters) > 0) {
      cat(file = export_file, append = TRUE, sprintf("UPDATE ros_common.vessel_identification SET %s WHERE id = %s;\n", str_sub(parameters, 2), row_id))
    }
  }
  cat(sprintf("No match for %s vessel(s)\n", not_found_count))
}

update_ros_vessel_identification(loaded_ros_vessel_identifications, rav_data, export_file, c("main_fishing_gear_code"), c( "IOTC000369"))

add_vessel_from_rav <- function(export_file, rav_data, iotc_vessel_identifier) {
  rav_row <- rav_data[iotc_number == iotc_vessel_identifier,]
  rav_row_count <- get_row_count(rav_row)
  if (rav_row_count == 0) {
    throw("Could not find vessel in rav with iotc_vessel_identifier '", iotc_vessel_identifier, "'")
  } else if (rav_row_count > 1) {
    rav_row <- rav_row[rav_row_count]
  }

  check_change <- function(name, row, parameters) {
    value <- row[[name]]
    if (value == "" ||
      value == "NULL" ||
      value == "NA" ||
      is.na(value) ||
      is.null(value)) {
      parameters <- paste0(parameters, ", NULL")
    } else {
      parameters <- paste0(parameters, ", '", str_replace_all(value, "'", "''"), "'")
    }
    parameters
  }

  parameters <- ""
  # imo_number → imo_identifier
  parameters <- check_change("imo_number", rav_row, parameters)
  # iotc_number → iotc_vessel_identifier
  parameters <- check_change("iotc_number", rav_row, parameters)
  # ircs → ircs_identifier
  parameters <- check_change("ircs", rav_row, parameters)
  # vessel_name → vessel_name
  parameters <- check_change("vessel_name", rav_row, parameters)
  # registration_number → registration_identifier
  parameters <- check_change("registration_number", rav_row, parameters)

  # main_fishing_gear_code → main_fishing_gear_code
  parameters <- check_change("main_fishing_gear_code", rav_row, parameters)
  # flag_code → flag_code
  parameters <- check_change("flag_code", rav_row, parameters)
  sql <- sprintf("INSERT INTO ros_common.vessel_identification (imo_identifier, iotc_vessel_identifier, ircs_identifier, vessel_name, registration_identifier,main_fishing_gear_code, flag_code) VALUES(%s);\n", str_sub(parameters, 2))
  cat(file = export_file, append = TRUE, sql)
}

add_vessel_from_rav(export_file, rav_data, "IOTC000374")
add_vessel_from_rav(export_file, rav_data, "IOTC003577")
add_vessel_from_rav(export_file, rav_data, "IOTC001140")
add_vessel_from_rav(export_file, rav_data, "IOTC003652")
