#' The import file model for the ROS version 3.2.1 on Longline domain
#' @export

ROS_3_2_1_LL_IMPORT_MODEL <-
  import_file("Ros 3.2.1 - LL", c(
    #[ 2] = "O-INFO" | From row 6 - 25 columns
    sheet("O-INFO", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      mandatory_simple_column("OBSERVER_IDENTIFICATION_OBSERVER_IOTC_NUMBER", column_location("ros_common.observer_identification", "iotc_number")),
      optional_simple_column("OBSERVER_IDENTIFICATION_FULL_NAME", column_location("ros_common.observer_identification", "full_name")),
      optional_fk_column("OBSERVER_IDENTIFICATION_NATIONALITY_CODE", column_location("ros_common.observer_identification", "nationality_code"), column_location("refs_admin.countries", "code")),
      optional_fk_column("OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_COUNTRY_CODE", column_location("ros_common.locations", "country_code"), column_location("refs_admin.countries", "code")),
      # Fixme
      optional_fk_column("OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_PORT_CODE", column_location("ros_common.locations", "name"), column_location("refs_admin.ports", "code")),
      optional_simple_column("OBSERVER_TRIP_DETAILS_EMBARKATION_AT_SEA_LATITUDE", column_location("ros_common.locations", "latitude")),
      optional_simple_column("OBSERVER_TRIP_DETAILS_EMBARKATION_AT_SEA_LONGITUDE", column_location("ros_common.locations", "longitude")),
      optional_simple_column("OBSERVER_TRIP_DETAILS_EMBARKATION_AT_SEA_TIMESTAMP", column_location("ros_common.observer_trip_details", "date_time_embarkation")),
      optional_fk_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_COUNTRY_CODE", column_location("ros_common.locations", "country_code"), column_location("refs_admin.countries", "code")),
      # Fixme
      optional_fk_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_PORT_CODE", column_location("ros_common.locations", "name"), column_location("refs_admin.ports", "code")),
      optional_simple_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_AT_SEA_LATITUDE", column_location("ros_common.locations", "latitude")),
      optional_simple_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_AT_SEA_LONGITUDE", column_location("ros_common.locations", "longitude")),
      optional_simple_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_AT_SEA_TIMESTAMP", column_location("ros_common.observer_trip_details", "date_time_disembarkation")),
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_SETS_CONDUCTED", column_location("ros_common.observed_trip_summary", "number_of_conducted_fishing_events_with_observer_onboard")),
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_SETS_OBSERVED", column_location("ros_common.observed_trip_summary", "number_of_observed_fishing_events")),
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_SEARCHING", column_location("ros_common.observed_trip_summary", "number_of_days_searching")),
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_ACTIVELY_FISHING", column_location("ros_common.observed_trip_summary", "number_of_active_fishing_days")),
      optional_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_IN_FISHING_AREA", column_location("ros_common.observed_trip_summary", "number_of_days_in_fishing_area")),
      optional_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_TRANSITING", column_location("ros_common.observed_trip_summary", "number_of_days_transiting")),
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_LOST", column_location("ros_common.observed_trip_summary", "number_of_days_lost")),
      # Fixe me (not a fk?)
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_1_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")),
      # Fixe me (not a fk?)
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_2_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")),
      # Fixe me (not a fk?)
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_3_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")),
      # Fixe me (not a fk?)
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_4_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")))),
    #[ 3] = "V-INFO" | From row 6 - 32 columns
    sheet("V-INFO", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      mandatory_simple_column("VESSEL_INFORMATION_IDENTIFICATION_NAME", column_location("ros_common.vessel_identification", "NAME")),
      # Fix me (not found in table)
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING_CODE", column_location("ros_common.vessel_identification", "flag_code"), column_location("refs_admin.countries", "code")),
      mandatory_simple_column("VESSEL_INFORMATION_IDENTIFICATION_IOTC_NUMBER", column_location("ros_common.vessel_identification", "iotc_number")),
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_IMO_NUMBER", column_location("ros_common.vessel_identification", "imo_number")),
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_IRCS", column_location("ros_common.vessel_identification", "ircs")),
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_COUNTRY_CODE", column_location("ros_common.vessel_identification", "flag_code"), column_location("refs_admin.countries", "code")),
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_PORT_CODE", column_location("ros_common.vessel_identification", "port_code"), column_location("refs_admin.ports", "code")),
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_NUMBER", column_location("ros_common.vessel_identification", "registration_number")),
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_1", column_location("ros_common.texts", "value")), # by vessel_identification_phone
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_2", column_location("ros_common.texts", "value")), # by vessel_identification_phone
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_1", column_location("ros_common.texts", "value")), # by vessel_identification_fax
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_2", column_location("ros_common.texts", "value")), # by vessel_identification_fax
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_1", column_location("ros_common.texts", "value")), # by vessel_identification_email
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_2", column_location("ros_common.texts", "value")), # by vessel_identification_email
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_1_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_2_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_3_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_4_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_MAIN_FISHING_GEAR_CODE", column_location("ros_common.vessel_identification", "main_fishing_gear_code"), column_location("refs_fishery.gears", "code")),
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_FULL_NAME", column_location("ros_common.person_contact_details", "full_name")), # by vessel_owner_and_personnel.registered_vessel_owner_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_NATIONALITY_CODE", column_location("ros_common.person_contact_details", "nationality_code"), column_location("refs_admin.countries", "code")), # by vessel_owner_and_personnel.registered_vessel_owner_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_CONTACT_DETAILS", column_location("ros_common.person_contact_details", "contact_details")), # by vessel_owner_and_personnel.registered_vessel_owner_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_FULL_NAME", column_location("ros_common.person_contact_details", "full_name")), # by vessel_owner_and_personnel.charter_or_operator_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_NATIONALITY_CODE", column_location("ros_common.person_contact_details", "nationality_code"), column_location("refs_admin.countries", "code")), # by vessel_owner_and_personnel.charter_or_operator_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS", column_location("ros_common.person_contact_details", "contact_details")), # by vessel_owner_and_personnel.charter_or_operator_id
      # Fix me : not found
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS2", column_location("ros_common.person_contact_details", "contact_details")),
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_FULL_NAME", column_location("ros_common.person_details", "full_name")), # by vessel_owner_and_personnel.fishing_master_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_NATIONALITY_CODE", column_location("ros_common.person_details", "nationality_code"), column_location("refs_admin.countries", "code")), # by vessel_owner_and_personnel.fishing_master_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_FULL_NAME", column_location("ros_common.person_details", "full_name")), # by vessel_owner_and_personnel.skipper_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_NATIONALITY_CODE", column_location("ros_common.person_details", "nationality_code"), column_location("refs_admin.countries", "code")), # by vessel_owner_and_personnel.skipper_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CREW_NUMBER", column_location("ros_common.vessel_owner_and_personnel", "number_of_crew")))),
    #[ 4] = "V-TRIP" | From row 6 - 11 columns
    sheet("V-TRIP", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fix me : not found
      optional_fk_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_COUNTRY_CODE", column_location("a", "b"), column_location("refs_admin.countries", "code")),
      optional_fk_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_PORT_CODE", column_location("ros_common.vessel_trip_details", "departure_port_code"), column_location("refs_admin.ports", "code")),
      # Fix me : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_LATITUDE", column_location("a", "b")),
      # Fix me : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_LONGITUDE", column_location("a", "b")),
      optional_simple_column("TRIP_DETAILS_VESSEL_DEPARTURE_TIMESTAMP", column_location("ros_common.vessel_trip_details", "date_time_vessel_sailed")),
      # Fix me : not found
      optional_fk_column("TRIP_DETAILS_VESSEL_RETURN_PORT_COUNTRY_CODE", column_location("a", "b"), column_location("refs_admin.countries", "code")),
      optional_fk_column("TRIP_DETAILS_VESSEL_RETURN_PORT_PORT_CODE", column_location("ros_common.vessel_trip_details", "return_port_code"), column_location("refs_admin.ports", "code")),
      # Fix me : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_RETURN_PORT_LATITUDE", column_location("a", "b")),
      # Fix me : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_RETURN_PORT_LONGITUDE", column_location("a", "b")),
      optional_simple_column("TRIP_DETAILS_VESSEL_RETURN_TIMESTAMP", column_location("ros_common.vessel_trip_details", "date_time_vessel_returned_to_port")))),
    #[ 5] = "V-ATTRIBUTES" | From row 6 - 50 columns
    sheet("V-ATTRIBUTES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      mandatory_measurement_column("VESSEL_ATTRIBUTES_TONNAGE_VALUE", column_location("a", "b"), "ros_common.tonnages"),
      mandatory_measurement_unit_column("VESSEL_ATTRIBUTES_TONNAGE_GT_GRT", c("GT", "GRT")),
      mandatory_measurement_column("VESSEL_ATTRIBUTES_LENGTH_OVERALL_VALUE", column_location("a", "b"), "ros_common.lengths"),
      mandatory_measurement_unit_column("VESSEL_ATTRIBUTES_LENGTH_OVERALL_M_FT", c("M", "FT")),
      mandatory_fk_column("VESSEL_ATTRIBUTES_HULL_MATERIAL_CODE", column_location("a", "b"), column_location("refs_fishery.hull_material_types", "code")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_1_MAKE", column_location("a", "b")),
      mandatory_measurement_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_1_POWER_VALUE", column_location("a", "b"), "ros_common.powers"),
      mandatory_measurement_unit_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_1_KW_HP_BHP", c("KW", "HP", "BHP")),
      optional_simple_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_2_MAKE", column_location("a", "b")),
      optional_measurement_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_2_POWER_VALUE", column_location("a", "b"), "ros_common.engines"),
      optional_measurement_unit_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_2_KW_HP_BHP", c("KW", "HP", "BHP")),
      mandatory_measurement_column("VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_VALUE", column_location("a", "b"), "ros_common.capacities"),
      mandatory_simple_column("VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_T_M3", column_location("a", "b")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_1_CODE", column_location("a", "b"), column_location("refs_fishery.fish_preservation_methods", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_2_CODE", column_location("a", "b"), column_location("refs_fishery.fish_preservation_methods", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_3_CODE", column_location("a", "b"), column_location("refs_fishery.fish_preservation_methods", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_4_CODE", column_location("a", "b"), column_location("refs_fishery.fish_preservation_methods", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_1_CODE", column_location("a", "b"), column_location("refs_fishery.fish_storage_types", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_2_CODE", column_location("a", "b"), column_location("refs_fishery.fish_storage_types", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_3_CODE", column_location("a", "b"), column_location("refs_fishery.fish_storage_types", "code")),
      optional_measurement_column("VESSEL_ATTRIBUTES_AUTONOMY_RANGE_VALUE", column_location("a", "b"), "ros_common.ranges"),
      optional_measurement_unit_column("VESSEL_ATTRIBUTES_AUTONOMY_RANGE_DAYS_NM", c("DAYS", "NM")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_GPS", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VMS", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_AIS", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_RADARS", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_TRACK_PLOTTER", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DEPTH_SOUNDER", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SONAR", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DOPPLER_CURRENT_METER", column_location("a", "b")),
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_XBT", column_location("a", "b")),
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VHF_RADIOS", column_location("a", "b")),
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_HF_RADIOS", column_location("a", "b")),
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SATELLITE_COMM", column_location("a", "b")),
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SST_GAUGE", column_location("a", "b")),
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_WEATHER_FAX", column_location("a", "b")),
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_FIS", column_location("a", "b")),
      mandatory_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_CATEGORY_1_CODE", column_location("a", "b"), column_location("refs_fishery.waste_categories", "code")),
      mandatory_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_STORAGE_DISPOSAL_METHOD_1_CODE", column_location("a", "b"), column_location("refs_fishery.waste_disposal_methods", "code")),
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_1", column_location("a", "b")),
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_CATEGORY_2_CODE", column_location("a", "b"), column_location("refs_fishery.waste_categories", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_STORAGE_DISPOSAL_METHOD_2_CODE", column_location("a", "b"), column_location("refs_fishery.waste_disposal_methods", "code")),
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_OTHER_2", column_location("a", "b")),
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_CATEGORY_3_CODE", column_location("a", "b"), column_location("refs_fishery.waste_categories", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_STORAGE_DISPOSAL_METHOD_3_CODE", column_location("a", "b"), column_location("refs_fishery.waste_disposal_methods", "code")),
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_OTHER_3", column_location("a", "b")),
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_CATEGORY_4_CODE", column_location("a", "b"), column_location("refs_fishery.waste_categories", "code")),
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_STORAGE_DISPOSAL_METHOD_4_CODE", column_location("a", "b"), column_location("refs_fishery.waste_disposal_methods", "code")),
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_OTHER_4", column_location("a", "b")))),
    #[ 6] = "G-GENERAL" | From row 6 - 33 columns
    sheet("G-GENERAL", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SPECIAL_EQUIPMENT_OR_MACHINERY_LINE_SETTER", column_location("a", "b")),
      mandatory_simple_column("SPECIAL_EQUIPMENT_OR_MACHINERY_LINE_HAULER", column_location("a", "b")),
      mandatory_simple_column("SPECIAL_EQUIPMENT_OR_MACHINERY_BAIT_CASTING_MACHINE", column_location("a", "b")),
      mandatory_fk_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_MATERIAL_CODE", column_location("a", "b"), column_location("refs_fishery.line_material_types", "code")),
      mandatory_measurement_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_LENGTH_VALUE", column_location("a", "b"), "ros_common.lengths"),
      mandatory_measurement_unit_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_LENGTH_KM_NM", c("KM", "NM")),
      optional_measurement_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_DIAMETER_VALUE", column_location("a", "b"), "ros_common.diameters"),
      optional_measurement_unit_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_DIAMETER_MM_CM", c("MM", "CM")),
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_1_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_2_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_3_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_4_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      optional_fk_column("MITIGATION_DEVICES_DEVICE_1_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("MITIGATION_DEVICES_DEVICE_2_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("MITIGATION_DEVICES_DEVICE_3_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("MITIGATION_DEVICES_DEVICE_4_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("MITIGATION_DEVICES_DEVICE_5_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_measurement_column("TORI_LINE_DETAILS_TORI_LINE_LENGTH_VALUE", column_location("a", "b"), "ros_common.lengths"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_TORI_LINE_LENGTH_M_FT", c("M", "FT")),
      optional_fk_column("TORI_LINE_DETAILS_STREAMER_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.streamer_types", "code")),
      optional_simple_column("TORI_LINE_DETAILS_STREAMER_REACH_SURFACE", column_location("a", "b")),
      optional_measurement_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_VALUE", column_location("a", "b"), "ros_common.lengths"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_CM_M_FT", c("CM", "M", "FT")),
      optional_measurement_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_VALUE", column_location("a", "b"), "ros_common.lengths"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_CM_M_FT", c("CM", "M", "FT")),
      optional_simple_column("TORI_LINE_DETAILS_STREAMER_NUMBER_PER_LINE", column_location("a", "b")),
      optional_measurement_column("TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_VALUE", column_location("a", "b"), "ros_common.distances"),
      optional_simple_column("TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_CM_M_FT", column_location("a", "b")),
      optional_measurement_column("TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_VALUE", column_location("a", "b"), "ros_common.heights"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_CM_M_FT", c("CM", "M", "FT")),
      optional_simple_column("TORI_LINE_DETAILS_TOWED_OBJECTS_NUMBER", column_location("a", "b")),
      optional_simple_column("TORI_LINE_DETAILS_TOWED_OBJECTS_TYPE", column_location("a", "b")))),
    #[ 7] = "G-CONFIG-BRANCHLINES" | From row 6 - 8 columns
    sheet("G-CONFIG-BRANCHLINES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("BRANCHLINE_CONFIGURATIONS_CONFIGURATION_NUMBER", column_location("a", "b")),
      mandatory_simple_column("BRANCHLINE_CONFIGURATIONS_SECTION_NUMBER", column_location("a", "b")),
      optional_fk_column("BRANCHLINE_CONFIGURATIONS_MATERIAL_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.line_material_types", "code")),
      mandatory_measurement_column("BRANCHLINE_CONFIGURATIONS_LENGTH_VALUE", column_location("a", "b"), "ros_common.lengths"),
      mandatory_measurement_unit_column("BRANCHLINE_CONFIGURATIONS_LENGTH_CM_M", c("CM", "M")),
      mandatory_measurement_column("BRANCHLINE_CONFIGURATIONS_DIAMETER_VALUE", column_location("a", "b"), "ros_common.diameters"),
      mandatory_measurement_unit_column("BRANCHLINE_CONFIGURATIONS_DIAMETER_MM_CM", c("MM", "CM")))),
    #[ 8] = "E-SET" | From row 6 - 24 columns
    sheet("E-SET", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_START_SETTING_DATE_TIME_UTC", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_START_SETTING_LATITUDE", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_START_SETTING_LONGITUDE", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_END_SETTING_DATE_TIME_UTC", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_END_SETTING_LATITUDE", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_END_SETTING_LONGITUDE", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_SPEED_VESSEL_VALUE_KNOTS", column_location("a", "b")), # Move this to a ros_common.speeds measurements table
      optional_simple_column("SETTING_OPERATIONS_SPEED_LINE_SETTER_VALUE_MS", column_location("a", "b")), # Move this to a ros_common.speeds measurements table
      # optional_measurement_column("SETTING_OPERATIONS_SPEED_VESSEL_VALUE_KNOTS", column_location("a", "b"), "ros_common.speeds","KNOTS"),
      # optional_measurement_column("SETTING_OPERATIONS_SPEED_LINE_SETTER_VALUE_MS", column_location("a", "b"), "ros_common.speeds","MS"),
      mandatory_measurement_column("SETTING_OPERATIONS_MAINLINE_LENGTH_VALUE", column_location("a", "b"), "ros_common.lengths"),
      mandatory_simple_column("SETTING_OPERATIONS_MAINLINE_LENGTH_KM_NM", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_CLIP_ON_TIME_BRANCHLINE", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_CLIP_ON_TIME_BUOYS", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_NUM_TOTAL_HOOKS_SET", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_NUM_TOTAL_FLOATS_SET", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_NUM_HOOKS_BETWEEN_FLOATS", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_SHARK_LINES_SET", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_SHARK_LINES_NUM_LINES", column_location("a", "b")),
      mandatory_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_1_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      optional_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_2_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      optional_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_3_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      optional_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_4_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      optional_simple_column("SETTING_OPERATIONS_VMS_ON", column_location("a", "b")))),
    #[ 9] = "E-SET-LIGHTS" | From row 6 - 5 columns
    sheet("E-SET-LIGHTS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      optional_fk_column("SETTING_OPERATIONS_ATTACHED_LIGHTS_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.light_types", "code")),
      optional_fk_column("SETTING_OPERATIONS_ATTACHED_LIGHTS_COLOR_CODE", column_location("a", "b"), column_location("refs_fishery.light_colours", "code")),
      optional_simple_column("SETTING_OPERATIONS_ATTACHED_LIGHTS_NUM_LIGHTS", column_location("a", "b")))),
    #[ 10] = "E-SET-BRANCHLINES" | From row 6 - 4 columns
    sheet("E-SET-BRANCHLINES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_BRANCHLINE_DETAILS_CONFIGURATION_NUMBER", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_BRANCHLINE_DETAILS_NUM_BRANCHLINES_SET", column_location("a", "b")))),
    #[ 11] = "E-SET-HOOKS" | From row 6 - 5 columns
    sheet("E-SET-HOOKS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_fk_column("SETTING_OPERATIONS_HOOKS_DETAILS_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.hook_types", "code")),
      mandatory_simple_column("SETTING_OPERATIONS_HOOKS_DETAILS_PERCENTAGE", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_HOOKS_DETAILS_VARIATIONS", column_location("a", "b")))),
    #[ 12] = "E-SET-BAITS" | From row 6 - 6 columns
    sheet("E-SET-BAITS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_fk_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_CONDITION_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")), # Not Found
      mandatory_fk_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_SPECIES_CODE", column_location("a", "b"), column_location("refs_biological.bait_conditions", "code")),
      mandatory_simple_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_PERCENTAGE", column_location("a", "b")),
      optional_simple_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_DYE_COLOUR", column_location("a", "b")))),
    #[13] = "E-SET-MITIGATION-MEASURES" | From row 6 - 14 columns
    sheet("E-SET-MITIGATION-MEASURES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_NUM_TORI_LINES_DEPLOYED", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_MIN_DECK_LIGHTING_USED", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_HOOKS_SET_BETWEEN_DUSK_DAWN", column_location("a", "b")),
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_BRANCHLINE_WEIGHTED", column_location("a", "b")),
      mandatory_measurement_column("SETTING_OPERATIONS_MITIGATION_MEASURES_AVG_BRANCHLINE_WEIGHT_G", column_location("a", "b"), "ros_common.weights", "G"),
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_PERC_BRANCHLINE_WEIGHTED", column_location("a", "b")),
      mandatory_measurement_column("SETTING_OPERATIONS_MITIGATION_MEASURES_HOOK_SINKER_DISTANCE_CM", column_location("a", "b"), "ros_common.distances", "CM"),
      optional_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_UNDERWATER_SETTING", column_location("a", "b")),
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_1_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_2_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_3_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")),
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_4_CODE", column_location("a", "b"), column_location("refs_fishery.mitigation_devices", "code")))),
    #[14] = "E-SET-HAULING" | From row 6 - 20 columns
    sheet("E-SET-HAULING", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_simple_column("HAULING_OPERATIONS_START_HAULING_DATE_TIME_UTC", column_location("a", "b")),
      mandatory_simple_column("HAULING_OPERATIONS_START_HAULING_LATITUDE", column_location("a", "b")),
      mandatory_simple_column("HAULING_OPERATIONS_START_HAULING_LONGITUDE", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_END_HAULING_DATE_TIME_UTC", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_END_HAULING_LATITUDE", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_END_HAULING_LONGITUDE", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_OFFAL_MANAGEMENT", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_1_CODE", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_2_CODE", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_3_CODE", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_4_CODE", column_location("a", "b")),
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_1_CODE", column_location("a", "b"), column_location("refs_fishery.stunning_methods", "code")),
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_2_CODE", column_location("a", "b"), column_location("refs_fishery.stunning_methods", "code")),
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_3_CODE", column_location("a", "b"), column_location("refs_fishery.stunning_methods", "code")),
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_4_CODE", column_location("a", "b"), column_location("refs_fishery.stunning_methods", "code")),
      optional_simple_column("HAULING_OPERATIONS_BIRD_SCARING_DEVICE_AT_HAULER", column_location("a", "b")),
      mandatory_simple_column("HAULING_OPERATIONS_NUM_HOOKS_RETRIEVED_DURING_OBSERVATION", column_location("a", "b")),
      mandatory_fk_column("HAULING_OPERATIONS_SAMPLING_PROTOCOL_CODE", column_location("a", "b"), column_location("refs_biological.sampling_protocols", "code")))),
    #[15] = "E-SET-HAULING-BITEOFFS" | From row 6 - 4 columns
    sheet("E-SET-HAULING-BITEOFFS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_BITEOFFS_DETAILS_BRANCHLINE_CONFIGURATION_NUMBER", column_location("a", "b")),
      optional_simple_column("HAULING_OPERATIONS_BITEOFFS_DETAILS_BRANCHLINE_NUM_BITEOFFS", column_location("a", "b")))),
    #[16] = "E-SET-CATCHES" | From row 6 - 12 columns
    sheet("E-SET-CATCHES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_simple_column("CATCH_NUMBER", column_location("a", "b")),
      mandatory_fk_column("CATCH_DETAILS_SPECIES_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      mandatory_fk_column("CATCH_DETAILS_FATE_TYPE", column_location("a", "b"), column_location("refs_biological.types_of_fate", "code")),
      mandatory_fk_column("CATCH_DETAILS_FATE_CODE", column_location("a", "b"), column_location("refs_biological.fates", "code")),
      mandatory_fk_column("CATCH_DETAILS_SAMPLING_METHOD_CODE", column_location("a", "b"), column_location("refs_biological.sampling_methods_for_catch_estimation", "code")),
      optional_simple_column("CATCH_DETAILS_NUM_FISH", column_location("a", "b")),
      optional_fk_column("CATCH_DETAILS_WEIGHT_PROCESSING_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.fish_processing_types", "code")),
      optional_measurement_column("CATCH_DETAILS_WEIGHT_VALUE", column_location("a", "b"), "ros_common.weights"),
      optional_measurement_unit_column("CATCH_DETAILS_WEIGHT_KG_T", c("KG", "T")),
      optional_fk_column("CATCH_DETAILS_WEIGHT_ESTIMATION_METHOD_CODE", column_location("a", "b"), column_location("refs_biological.sampling_methods_for_catch_estimation", "code")))),
    #[17] = "E-SET-CATCHES-SPECIMEN" | From row 6 - 32 columns
    sheet("E-SET-CATCHES-SPECIMEN", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_simple_column("CATCH_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SPECIMEN_NUMBER", column_location("a", "b")),
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLING_PERIOD_CODE", column_location("a", "b"), column_location("refs_biological.sampling_periods", "code")),
      optional_fk_column("SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_CAPTURE_CODE", column_location("a", "b"), column_location("refs_biological.incidental_captures_conditions", "code")),
      optional_fk_column("SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_RELEASE_CODE", column_location("a", "b"), column_location("refs_biological.incidental_captures_conditions", "code")),
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_DEPREDATION_SOURCE_CODE", column_location("a", "b"), column_location("refs_biological.depredation_sources", "code")), # Link not found
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_OBSERVED_PREDATOR_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLING_METHOD_CODE", column_location("a", "b"), column_location("refs_biological.sampling_methods_for_sampling_collections", "code")),
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_TYPE_CODE", column_location("a", "b"), column_location("refs_biological.types_of_measurement", "code")), # Check
      mandatory_measurement_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_VALUE_CM", column_location("a", "b"), "ros_common.measured_lengths", "CM"),
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_MEASURING_TOOL_CODE", column_location("a", "b"), column_location("refs_biological.measurement_tools", "code")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_IS_STRAIGHT", column_location("a", "b")),
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_TYPE_CODE", column_location("a", "b"), column_location("refs_biological.types_of_measurement", "code")),
      optional_measurement_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_VALUE_CM", column_location("a", "b"), "ros_common.measured_lengths", "CM"),
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_MEASURING_TOOL_CODE", column_location("a", "b"), column_location("refs_biological.measurement_tools", "code")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_IS_STRAIGHT", column_location("a", "b")),
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_PROCESSING_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.fish_processing_types", "code")),
      optional_measurement_column("SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_VALUE_KG", column_location("a", "b"), "ros_common.estimated_weights", "KG"),
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_ESTIMATION_METHOD_CODE", column_location("a", "b"), column_location("refs_biological.", "code")), # Not found
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_SEX", column_location("a", "b"), column_location("refs_biological.sex", "code")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_MATURITY_SCALE", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_MATURITY_STAGE", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLE_COLLECTED_TYPE", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLE_COLLECTED_PRESERVATION_METHOD", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RELEASE", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RECOVERY", column_location("a", "b")),
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TYPE_CODE", column_location("a", "b"), column_location("refs_biological.tag_types", "code")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_1", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_2", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_FINDER_NAME_AND_CONTACT_DETAILS", column_location("a", "b")))),
    #[18] = "E-SET-CATCHES-SPECIMEN-SSI" | From row 6 - 16 columns
    sheet("E-SET-CATCHES-SPECIMEN-SSI", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SET_NUMBER", column_location("a", "b")),
      mandatory_simple_column("CATCH_NUMBER", column_location("a", "b")),
      mandatory_simple_column("SPECIMEN_NUMBER", column_location("a", "b")),
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_GEAR_INTERACTION_CODE", column_location("a", "b"), column_location("refs_biological.", "code")),
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HOOK_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.hook_types", "code")),
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_CONDITION_CODE", column_location("a", "b"), column_location("refs_biological.bait_conditions", "code")),
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_TYPE_CODE", column_location("a", "b"), column_location("refs_biological.bait_types", "code")),
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_MATERIAL_CODE", column_location("a", "b"), column_location("refs_fishery.line_material_types", "code")),
      optional_measurement_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_DIAMETER_MM", column_location("a", "b"), "ros_common.thicknesses", "MM"), # Check
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_DE_HOOKER_DEVICE_CODE", column_location("a", "b"), column_location("refs_fishery.dehooker_types", "code")),
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LIGHT_ATTACHED", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BROUGHT_ONBOARD", column_location("a", "b")),
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HANDLING_METHOD_CODE", column_location("a", "b"), column_location("refs_biological.handling_methods", "code")),
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_REVIVAL", column_location("a", "b")),
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_PHOTO_ID", column_location("a", "b")))),
    #[19] = "T-EVENTS" | From row 6 - 15 columns
    sheet("T-EVENTS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("TRANSSHIPMENT_NUMBER", column_location("a", "b")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_START_EVENT_DATE_TIME_UTC", column_location("a", "b")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_START_EVENT_LATITUDE", column_location("a", "b")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_START_EVENT_LONGITUDE", column_location("a", "b")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_END_EVENT_DATE_TIME_UTC", column_location("a", "b")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_END_EVENT_LATITUDE", column_location("a", "b")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_END_EVENT_LONGITUDE", column_location("a", "b")),
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CATEGORY_CODE", column_location("a", "b"), column_location("refs_fishery.transhipment_categories", "code")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_NAME", column_location("a", "b")),
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_FLAG_OR_CHARTERING_CODE", column_location("a", "b"), column_location("refs_admin.countries", "code")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_IRCS", column_location("a", "b")),
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_REGISTRATION_COUNTRY_CODE", column_location("a", "b"), column_location("refs_admin.countries", "code")),
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_REGISTRATION_PORT_CODE", column_location("a", "b"), column_location("refs_admin.ports", "code")),
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_REGISTRATION_NUMBER", column_location("a", "b")))),
    #[20] = "T-PRODUCTS" | From row 6 - 6 columns
    sheet("T-PRODUCTS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("a", "b")),
      mandatory_simple_column("TRANSSHIPMENT_NUMBER", column_location("a", "b")),
      optional_fk_column("PRODUCT_TRANSSHIPPED_SPECIES_CODE", column_location("a", "b"), column_location("refs_biological.species", "code")),
      optional_fk_column("PRODUCT_TRANSSHIPPED_PROCESSING_TYPE_CODE", column_location("a", "b"), column_location("refs_fishery.fish_processing_types", "code")),
      optional_measurement_column("PRODUCT_TRANSSHIPPED_QUANTITY_VALUE", column_location("a", "b"), "ros_common.species_by_product_type"),
      optional_measurement_unit_column("PRODUCT_TRANSSHIPPED_QUANTITY_KG_T", c("KG", "T"))))
  ))

ROS_3_2_1_LL_IMPORT_MODEL$print()

#' Load xls content for Longline domain with ROS model 3.2.1
#'
#' @param path The file to import
#' @return the loaded xls as a list of data.table (one per none empty sheet)
#' @export
load_ll_3_2_1_xls <- function(path) {
  load_xls(path, ROS_3_2_1_LL_IMPORT_MODEL)
}

path <- "/home/tchemit/projects/iotc/2025/ROS_DATA_TO_INPUT/EU-FRANCE/ROS_LL_data_reporting_EU.FRA.REU_2023.xlsx"
# taiste <- load_ll_3_2_1_xls(path)
