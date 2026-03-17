connection <- DB_IOTC_ROS()

# contact_column <- column_location("ros_common.contact→id")
# observer_column <- column_location("ros_common.observer→contact_id")
# vessel_identification_column <- column_location("ros_common.vessel_identification→id")
# vessel_identification_foreign_keys <- get_foreign_keys(connection, vessel_identification_column)
# iotc_person_contact_details_column <- column_location("ros_common.iotc_person_contact_details→id")
# iotc_person_contact_details_foreign_keys <- get_foreign_keys(connection, iotc_person_contact_details_column)
# observer_identification_column <- column_location("ros_common.observer_identification→id")
# observer_identification_foreign_keys <- get_foreign_keys(connection, observer_identification_column)
# person_details_column <- column_location("ros_common.person_details→id")
# person_details_foreign_keys <- get_foreign_keys(connection, person_details_column)
# person_contact_details_column <- column_location("ros_common.person_contact_details→id")
# person_contact_details_foreign_keys <- get_foreign_keys(connection, person_contact_details_column)

# timestamp <- str_replace_all(Sys.time(), "[ :.]", "_")
# output_dir <- paste0("./data")

# output_file <- clean_doubloons(connection, column_location('ros_common.vessel_identification→vessel_name'), output_dir, timestamp)
# remove_foreign_keys(vessel_identification_column, vessel_identification_foreign_keys, output_dir)
# add_foreign_keys(vessel_identification_column, vessel_identification_foreign_keys, output_dir)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 467, 474, output_file)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 473, 474, output_file)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 476, 474, output_file)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 480, 474, output_file)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 483, 474, output_file)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 484, 474, output_file)
# replace_id(vessel_identification_column, vessel_identification_foreign_keys, 485, 474, output_file)
#
#
# output_file <- clean_doubloons(connection, column_location("ros_common.iotc_person_contact_details→full_name"), output_dir, timestamp)
# replace_id(iotc_person_contact_details_column, iotc_person_contact_details_foreign_keys, 13, 51, output_file)
# remove_foreign_keys(iotc_person_contact_details_column, iotc_person_contact_details_foreign_keys, output_dir)
# add_foreign_keys(contact_column, iotc_person_contact_details_foreign_keys, output_dir)
#
# output_file <- clean_doubloons(connection, column_location("ros_common.observer_identification→full_name"), output_dir, timestamp)
# remove_foreign_keys(observer_identification_column, observer_identification_foreign_keys, output_dir)
# add_foreign_keys(observer_column, observer_identification_foreign_keys, output_dir)
# replace_id(observer_identification_column, observer_identification_foreign_keys, 83, 153, output_file)
# replace_id(observer_identification_column, observer_identification_foreign_keys, 52, 27, output_file)
# replace_id(observer_identification_column, observer_identification_foreign_keys, 33, 73, output_file)
#
# output_file <- clean_doubloons(connection, column_location("ros_common.person_details→full_name"), output_dir, timestamp)
# remove_foreign_keys(person_details_column, person_details_foreign_keys, output_dir)
# add_foreign_keys(contact_column, person_details_foreign_keys, output_dir)
# replace_id(person_details_column, person_details_foreign_keys, 748, 35, output_file)
# replace_id(person_details_column, person_details_foreign_keys, 752, 35, output_file)
# replace_id(person_details_column, person_details_foreign_keys, 753, 35, output_file)
# replace_id(person_details_column, person_details_foreign_keys, 747, 35, output_file)
#
# remove_foreign_keys(person_contact_details_column, person_contact_details_foreign_keys, output_dir)
# add_foreign_keys(contact_column, person_contact_details_foreign_keys, output_dir)

# rav_data <- import_rav("../iotc-ros-input-data/rav_export_record_history_20251107.csv")

# observers_import_file <- "../iotc-ros-input-data/observer_identification.csv"
# focal_points_import_file <- "../iotc-ros-input-data/iotc_person_contact_details.csv"
# person_details_import_file <- "../iotc-ros-input-data/person_details.csv"
#
# export_file <- "../iotc-ros-model/3.3.0/sql/04_06_fill_contacts.sql"
#
# observers_mapping_list <- import_observers(observers_import_file, export_file)
# observers_id_mapping <- unlist(observers_mapping_list[1])
# observers_iotc_number_mapping <- unlist(observers_mapping_list[2])
#
# starting_id <- observers_id_mapping[length(observers_id_mapping)]
# focal_points_id_mapping <- import_focal_points(focal_points_import_file, export_file, starting_id, observers_iotc_number_mapping)
#
# starting_id <- focal_points_id_mapping[length(focal_points_id_mapping)]
#
# all_skippers <- query(connection, "SELECT DISTINCT skipper_id FROM ros_common.vessel_owner_and_personnel WHERE skipper_id IS NOT NULL")[[1]]
# all_fishing_masters <- query(connection, "SELECT DISTINCT fishing_master_id FROM ros_common.vessel_owner_and_personnel WHERE fishing_master_id IS NOT NULL")[[1]]
# person_details_id_mapping <- import_person_details(person_details_import_file, export_file, starting_id, all_skippers, all_fishing_masters)
#
# export_file <- "../iotc-ros-model/3.3.0/sql/04_08_use_new_contacts.sql"
#
# replace_ids(export_file, observers_id_mapping, observer_identification_foreign_keys)
# replace_ids(export_file, focal_points_id_mapping, iotc_person_contact_details_foreign_keys)
# replace_ids(export_file, person_details_id_mapping, person_details_foreign_keys)

# export_file <- "../iotc-ros-model/3.3.0/sql/04_04_consolidate-ros_common-vessel_dentification.sql"
#
# optimize_vessel_identification(connection, export_file, vessel_identification_column, vessel_identification_foreign_keys)

export_directory <- "../iotc-ros-model/3.3.0/sql"
locations <- list(
  "ros_common.activity_details→id",
  "ros_common.additional_details_on_non_target_species→id",
  "ros_common.biometric_information→id",
  "ros_common.capacities→id",
  "ros_common.carrier_vessel_identification→id",
  "ros_common.daily_activities→id",
  "ros_common.depredation_details→id",
  "ros_common.depths→id",
  "ros_common.diameters→id",
  "ros_common.distances→id",
  "ros_common.engines→id",
  "ros_common.estimated_weights→id",
  "ros_common.general_vessel_and_trip_information→id",
  "ros_common.heights→id",
  "ros_common.lengths→id",
  "ros_common.locations→id",
  "ros_common.maturity_stages→id",
  "ros_common.measured_lengths→id",
  "ros_common.observed_trip_summary→id",
  "ros_common.observer_trip_details→id",
  "ros_common.powers→id",
  "ros_common.properties→id",
  "ros_common.ranges→id",
  "ros_common.reasons_for_days_lost→id",
  "ros_common.sample_collection_details→id",
  "ros_common.sampling_details→id",
  "ros_common.sizes→id",
  "ros_common.thicknesses→id",
  "ros_common.tonnages→id",
  "ros_common.vessel_attributes→id",
  "ros_common.vessel_electronics→id",
  "ros_common.vessel_identification→id",
  "ros_common.vessel_owner_and_personnel→id",
  "ros_common.vessel_trip_details→id",
  "ros_common.waste_managements→id",
  "ros_common.weights→id",
  "ros_ll.additional_catch_details_on_ssi→id",
  "ros_ll.baits_by_conditions→id",
  "ros_ll.biteoffs_by_branchlines_set→id",
  "ros_ll.branchline_configurations→id",
  "ros_ll.branchline_sections→id",
  "ros_ll.branchlines_set→id",
  "ros_ll.catch_details→id",
  "ros_ll.fishing_events→id",
  "ros_ll.floatlines→id",
  "ros_ll.gear_specifications→id",
  "ros_ll.general_gear_attributes→id",
  "ros_ll.hauling_operations→id",
  "ros_ll.hooks_by_type→id",
  "ros_ll.leader_set→id",
  "ros_ll.lights_by_type_and_colour→id",
  "ros_ll.mitigation_measures→id",
  "ros_ll.observer_data→id",
  "ros_ll.setting_operations→id",
  "ros_ll.special_equipment→id",
  "ros_ll.specimens→id",
  "ros_ll.tag_details→id",
  "ros_ll.tori_line_details→id",
  "ros_ps.additional_catch_details_on_ssi→id",
  "ros_ps.catch_details→id",
  "ros_ps.cetaceans_whale_shark_sightings→id",
  "ros_ps.fishing_events→id",
  "ros_ps.gear_specifications→id",
  "ros_ps.general_gear_attributes→id",
  "ros_ps.object_details→id",
  "ros_ps.observer_data→id",
  "ros_ps.setting_operations→id",
  "ros_ps.special_equipment→id",
  "ros_ps.specimens→id",
  "ros_ps.tag_details→id"
)
optimize_tables(connection, export_directory, locations)

# 2025-11-13
timestamp <- str_replace_all(Sys.time(), "[ :.]", "_")
output_dir <- paste0("./data")
output_file <- clean_doubloons(connection, column_location("ros_common.locations→location_name"), output_dir, timestamp)

export_directory <- "../iotc-ros-model/3.3.0/sql/2025-11-13"

optimize_tables(connection, export_directory, list("ros_common.locations→id"))

update_locations_coordinates <- query(connection, "SELECT t.id, p.lat, p.lon
FROM ros_common.locations t
JOIN refs_admin.ports p ON t.country_code = p.country_code AND t.location_name ILIKE p.name_en
WHERE p.lat IS NOT NULL AND p.lon IS NOT NULL
ORDER BY t.id;")

for (i in seq_len(get_row_count(update_locations_coordinates))) {
  row <- update_locations_coordinates[i]
  cat(sprintf("UPDATE ros_common.locations SET latitude = %s, longitude = %s WHERE id = %s;\n", row$lat, row$lon, row$id))
}

# 2025-11-15
export_file <- "../iotc-ros-model/3.3.0/sql/2025-11-13/06_02_clean_contacts.sql"
contact_column <- column_location("ros_common.contact→id")
contact_foreign_keys <- get_foreign_keys(connection, contact_column)
replace_id(contact_column, contact_foreign_keys, 196, 70, export_file)
replace_id(contact_column, contact_foreign_keys, 189, 57, export_file)
replace_id(contact_column, contact_foreign_keys, 150, 58, export_file)
replace_id(contact_column, contact_foreign_keys, 193, 65, export_file)
replace_id(contact_column, contact_foreign_keys, 185, 52, export_file)
replace_id(contact_column, contact_foreign_keys, 156, 11, export_file)
replace_id(contact_column, contact_foreign_keys, 195, 67, export_file)
replace_id(contact_column, contact_foreign_keys, 191, 60, export_file)
replace_id(contact_column, contact_foreign_keys, 171, 32, export_file)
replace_id(contact_column, contact_foreign_keys, 170, 31, export_file)
replace_id(contact_column, contact_foreign_keys, 146, 40, export_file)
replace_id(contact_column, contact_foreign_keys, 169, 30, export_file)

replace_id(contact_column, contact_foreign_keys, 163, 37, export_file)
replace_id(contact_column, contact_foreign_keys, 167, 25, export_file)
replace_id(contact_column, contact_foreign_keys, 173, 34, export_file)
replace_id(contact_column, contact_foreign_keys, 168, 29, export_file)
replace_id(contact_column, contact_foreign_keys, 154, 9, export_file)
replace_id(contact_column, contact_foreign_keys, 172, 33, export_file)
replace_id(contact_column, contact_foreign_keys, 188, 55, export_file)
replace_id(contact_column, contact_foreign_keys, 194, 66, export_file)
replace_id(contact_column, contact_foreign_keys, 155, 10, export_file)
replace_id(contact_column, contact_foreign_keys, 151, 15, export_file)
replace_id(contact_column, contact_foreign_keys, 177, 43, export_file)
replace_id(contact_column, contact_foreign_keys, 179, 45, export_file)
replace_id(contact_column, contact_foreign_keys, 187, 54, export_file)
replace_id(contact_column, contact_foreign_keys, 158, 13, export_file)
replace_id(contact_column, contact_foreign_keys, 178, 44, export_file)
replace_id(contact_column, contact_foreign_keys, 160, 16, export_file)
replace_id(contact_column, contact_foreign_keys, 143, 71, export_file)
replace_id(contact_column, contact_foreign_keys, 197, 71, export_file)
replace_id(contact_column, contact_foreign_keys, 181, 50, export_file)
replace_id(contact_column, contact_foreign_keys, 161, 18, export_file)
replace_id(contact_column, contact_foreign_keys, 142, 28, export_file)
replace_id(contact_column, contact_foreign_keys, 157, 12, export_file)
replace_id(contact_column, contact_foreign_keys, 159, 14, export_file)
replace_id(contact_column, contact_foreign_keys, 175, 39, export_file)
replace_id(contact_column, contact_foreign_keys, 174, 38, export_file)
replace_id(contact_column, contact_foreign_keys, 164, 22, export_file)
replace_id(contact_column, contact_foreign_keys, 166, 24, export_file)
replace_id(contact_column, contact_foreign_keys, 161, 17, export_file)
replace_id(contact_column, contact_foreign_keys, 153, 35, export_file)
replace_id(contact_column, contact_foreign_keys, 186, 53, export_file)
replace_id(contact_column, contact_foreign_keys, 144, 36, export_file)
replace_id(contact_column, contact_foreign_keys, 149, 26, export_file)
replace_id(contact_column, contact_foreign_keys, 180, 46, export_file)
replace_id(contact_column, contact_foreign_keys, 176, 41, export_file)
replace_id(contact_column, contact_foreign_keys, 198, 75, export_file)
replace_id(contact_column, contact_foreign_keys, 162, 18, export_file)
replace_id(contact_column, contact_foreign_keys, 165, 23, export_file)

optimize_tables(connection, export_directory, list("ros_common.contact→id"))

iotc_observer_identifiers_mapping <- as.data.table(fread("../iotc-ros-input-data/UPDATED_Observers_Registry_ROS_2025-11-07.csv"))[, c("old_iotc_number", "ob_iotc_id")]