source("./R/ROS_LL_v3_2_1_model.R")

import_ROS_LL_v3_2_1 <- function(file, connection, timestamp, iotc_observer_identifiers_mapping = c()) {
  cat("--- Init import from file: ", file, "\n", sep = "")
    # create importer context
    i_context <- import_context_ROS_LL_v3_2_1(file,
                                              connection,
                                              c("ros_ll.observer_data"),
                                              c(
                                                "ros_common.contact→id",
                                                "ros_common.observer→contact_id",
                                                "ros_common.vessel_identification→id"
                                              ),
                                              timestamp)
  i_context$data_table_caches()$cache("ros_common.observer")$set_extra_column_mapping("iotc_observer_identifier", iotc_observer_identifiers_mapping)
  # init meta informations
  metas <- i_context$model()$meta_sheet()
  metas$init_values(i_context$xls_data()$META, i_context)
  cat("Import log file is ", i_context$log_file())
  i_context$check_before_import(
    check_meta_submission = TRUE,
    check_meta_general = TRUE,
    check_code_lists = TRUE,
    check_mandatory = TRUE,
    check_exists = TRUE,
    check_measurements_units = TRUE
  )

  # v <- i_context$xls_data()
  # y <- i_context$code_list_caches()
  # z <- i_context$data_table_ids()
  # t <- c("GD", "GIL", "GILD")
  # ya <- lapply(t, function(x) {
  #   list(y$caches()$
  #          refs_fishery_config.gears$
  #          contains_code(x),
  #        y$caches()$refs_fishery_config.gears$get_code(x))
  # })
  # names(ya) <- t
  # c(z$get_id("ros_common.general_vessel_and_trip_information"),
  #   z$new_id("ros_common.general_vessel_and_trip_information"))
  i_context
}

import_all <- function(connection) {
  timestamp <- str_replace_all(Sys.time(), "[ :.]", "_")
  root_directory <- "../iotc-ros-input-data/data/LL"
  for (input in list.files(root_directory, recursive = T, pattern = "(.)+\\.xlsx$", full.names = T)) {
    cat("Input: ", input, "\n")
    try(i_context <- import_ROS_LL_v3_2_1(input, connection, timestamp))
  }
}

connection <- DB_IOTC_ROS()

timestamp <- str_replace_all(Sys.time(), "[ :.]", "_")


iotc_observer_identifiers_mapping <- as.data.table(fread("../iotc-ros-input-data/UPDATED_Observers_Registry_ROS_2025-11-07.csv"))[, .(old = old_iotc_number, new = ob_iotc_id)]

try(i_context <- import_ROS_LL_v3_2_1("../iotc-ros-input-data/data/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2022-v2.xlsx", connection, timestamp, iotc_observer_identifiers_mapping))
try(i_context <- import_ROS_LL_v3_2_1("../iotc-ros-input-data/data/LL/2023/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2023-v2.xlsx", connection, timestamp, iotc_observer_identifiers_mapping))
# i_context <- import_ROS_LL_v3_2_1("../iotc-ros-input-data/LL/2023/TAIWAN/SHUN SHENG NO.398(61550).xlsx", connection)
# Add some check when generating model (measurement_unit with unit both need to have same mandatory flag)


