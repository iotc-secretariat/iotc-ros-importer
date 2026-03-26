check_ll_one_test <- function() {
  ros_import_models <- load_models("LL", "3.3.0", "models")
  c <- check_one(DB_IOTC_ROS,
                 ros_import_models,
                 "../iotc-ros-input-data/data/LL",
                 "../iotc-ros-input-data/report/LL",
                 "../iotc-ros-input-data/data/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2022.xlsx",
                 "test")
}

check_ll_all_tests <- function(timestamp = Sys.time()) {
  ros_import_models <- load_models("LL", "3.3.0", "models")
  check_all(DB_IOTC_ROS,
            ros_import_models,
            "../iotc-ros-input-data/data/LL",
            "../iotc-ros-input-data/report/LL",
            timestamp = timestamp)
}

check_ps_one_test <- function(timestamp = Sys.time()) {
  ros_import_models <- load_models("PS", "3.3.0", "models")
  c <- check_one(DB_IOTC_ROS,
                 ros_import_models,
                 "../iotc-ros-input-data/data/PS",
                 "../iotc-ros-input-data/report/PS",
                 "../iotc-ros-input-data/data/PS/2023/EU-ITALY/ROS_PS_data_reporting_EU.ITA_2023.xlsx",
                 # "../iotc-ros-input-data/data/PS/2022/EU-FRANCE/ROS_PS_data_reporting_EU.FRA_2022.xlsx",
                 # "../iotc-ros-input-data/data/PS/2024/TANZANIA/Matola_TZN_NOP_PS_Pacific_Star_20240416.xlsx",
                 # "../iotc-ros-input-data/data/PS/2024/TANZANIA/Ali_TZA_NOP_PS_Playa de Azkorri_20240330.xlsx",
                 timestamp = timestamp)
}

check_ps_all_tests <- function(timestamp = Sys.time()) {
  ros_import_models <- load_models("PS", "3.3.0", "models")
  check_all(DB_IOTC_ROS,
            ros_import_models,
            "../iotc-ros-input-data/data/PS",
            "../iotc-ros-input-data/report/PS",
            timestamp = timestamp)
}

# check_ps_one_test("2026-03-26")
# check_ll_all_tests("2026-03-27")
# check_ps_all_tests("2026-03-27")


