check_one_test <- function() {
  ros_import_models <- load_models("LL", "3.3.0", "./models")
  c <- check_one(DB_IOTC_ROS,
                 ros_import_models,
                 "../iotc-ros-input-data/data/LL",
                 "../iotc-ros-input-data/report/LL",
                 "../iotc-ros-input-data/data/LL/2022/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2022.xlsx",
                 "test")
}

check_all_tests <- function(timestamp = Sys.time()) {
  ros_import_models <- load_models("LL", "3.3.0", "./models")
  check_all(DB_IOTC_ROS,
            ros_import_models,
            "../iotc-ros-input-data/data/LL",
            "../iotc-ros-input-data/report/LL",
            timestamp = timestamp)
}

# check_one_test()
# check_all_tests("2026-03-19-2")

