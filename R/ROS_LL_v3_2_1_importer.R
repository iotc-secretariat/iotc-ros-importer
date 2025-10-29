# source("./R/ROS_LL_v3_2_1_model.R")

import_ROS_LL_v3_2_1 <- function(file, connection) {

  import_context <- import_context_ROS_LL_v3_2_1(file, connection)
  v <- import_context$xls_data()
  y <- import_context$code_list_caches()
  z <- import_context$data_table_ids()
  t <- c("GD", "GIL", "GILD")
  ya <- lapply(t, function(x) {
    list(y$caches()$
           refs_fishery_config.gears$
           contains_code(x),
         y$caches()$refs_fishery_config.gears$get_code(x))
  })
  names(ya) <- t
  c(z$get_id("ros_common.general_vessel_and_trip_information"),
    z$new_id("ros_common.general_vessel_and_trip_information"))
}

connection <- DB_IOTC_ROS()
file <- "./inputs/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2022.xlsx"

