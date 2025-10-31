# source("./R/ROS_LL_v3_2_1_model.R")

import_ROS_LL_v3_2_1 <- function(file, connection) {
  cat("--- Init import from file: ", file, "\n", sep = "")
  # create importer context
  i_context <- import_context_ROS_LL_v3_2_1(file, connection, c("ros_ll.observer_data"))
  # init meta informations
  metas <- i_context$model()$meta_sheet()
  metas$init_values(i_context$xls_data()$META)
  check_before_import(i_context)

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

check_before_import <- function(i_context) {
  metas <- i_context$model()$meta_sheet()
  # check meta informations
  cat("--- Checking Meta Submission Information...\n")
  metas$check_values()
  # get the submiter id
  liaison_officer_email <- metas$cells()$liaison_officer_email$value()
  liaison_officer_email <- "aco20320@par.odn.ne.jp"

  cat("--- Checking liaison_officer_email", liaison_officer_email, "...\n")
  sql <- paste("SELECT t.id FROM ros_common.iotc_person_contact_details t WHERE email='", liaison_officer_email, "'", sep = "")
  result <- as.integer(dbGetQuery(connection, sql))
  if (is.na(result) || result < 1) {
    throw("Could not find liaison_officer from his email (", liaison_officer_email, ") in database (table ros_common.iotc_person_contact_details)")
  }
  cat("--- Checking Meta General Information...(TODO)\n")
  # check code lists references
  cat("--- Checking Code lists references...\n")
  code_list_caches <- i_context$code_list_caches()
  xls_data <- i_context$xls_data()
  model <- i_context$model()
  for (model_sheet in model$sheets()) {
    sheet_name <- model_sheet$name()
    for (column in model_sheet$columns()) {
      if (is_fk_column(column)) {
        column_name <- column$name()
        code_list_table_gav <- column$foreign_column_location()$table()$gav()
        code_list_cache <- code_list_caches$caches()[[code_list_table_gav]]
        cat("--- Checking Code list references on sheet:", sheet_name, "for column", column_name, "on code list table", code_list_table_gav, "...\n")
        data <- xls_data[[model_sheet$name()]] |> subset(select = column_name)
        row_number <- 5
        height <- dim(data)[1]
        for (i in seq_len(height)) {
          value <- data[i]
          if (!is.na(value)) {
            # if ("refs_biological.incidental_captures_conditions" == code_list_table_gav && value == "DE") {
            #   value <- "D"
            # }
            if (!code_list_cache$contains_code(value)) {
              throw("On ", sheet_name, ".", column_name, "[", (row_number + i), "] value `", value, "` is not found in the code list `", code_list_table_gav, "`")
            }
          }
        }
      }
    }
  }
  # check measurements units consistency
  cat("--- Checking measurements units consistency...\n")
}

connection <- DB_IOTC_ROS()
file <- "./inputs/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2022.xlsx"
i_context <- import_ROS_LL_v3_2_1(file, connection)


