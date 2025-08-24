#' The import file model for the ROS version 3.2.1 on Longline domain
#' @export

ROS_3_2_1_LL_IMPORT_MODEL <-
  import_file("Ros 3.2.1 - LL", c(
    #[ 2] = "O-INFO" | From row 6 - 25 columns
    sheet("O-INFO", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # by ros_common.general_vessel_and_trip_information.observer_identification_id
      mandatory_simple_column("OBSERVER_IDENTIFICATION_OBSERVER_IOTC_NUMBER", column_location("ros_common.observer_identification", "iotc_number")),
      # by ros_common.general_vessel_and_trip_information.observer_identification_id
      optional_simple_column("OBSERVER_IDENTIFICATION_FULL_NAME", column_location("ros_common.observer_identification", "full_name")),
      # by ros_common.general_vessel_and_trip_information.observer_identification_id
      optional_fk_column("OBSERVER_IDENTIFICATION_NATIONALITY_CODE", column_location("ros_common.observer_identification", "nationality_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.embarkation_location_id
      optional_fk_column("OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_COUNTRY_CODE", column_location("ros_common.locations", "country_code"), column_location("refs_admin.countries", "code")),
      # Fixme: does not exist in ros_common.locations
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.embarkation_location_id
      optional_fk_column("OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_PORT_CODE", column_location("ros_common.locations", "name"), column_location("refs_admin.ports", "code")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.embarkation_location_id
      optional_simple_column("OBSERVER_TRIP_DETAILS_EMBARKATION_AT_SEA_LATITUDE", column_location("ros_common.locations", "latitude")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.embarkation_location_id
      optional_simple_column("OBSERVER_TRIP_DETAILS_EMBARKATION_AT_SEA_LONGITUDE", column_location("ros_common.locations", "longitude")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      optional_simple_column("OBSERVER_TRIP_DETAILS_EMBARKATION_AT_SEA_TIMESTAMP", column_location("ros_common.observer_trip_details", "date_time_embarkation")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.disembarkation_location_id
      optional_fk_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_COUNTRY_CODE", column_location("ros_common.locations", "country_code"), column_location("refs_admin.countries", "code")),
      # Fixme: does not exist in ros_common.locations
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.disembarkation_location_id
      optional_fk_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_PORT_CODE", column_location("ros_common.locations", "name"), column_location("refs_admin.ports", "code")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.disembarkation_location_id
      optional_simple_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_AT_SEA_LATITUDE", column_location("ros_common.locations", "latitude")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      # by ros_common.observer_trip_details.disembarkation_location_id
      optional_simple_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_AT_SEA_LONGITUDE", column_location("ros_common.locations", "longitude")),
      # by ros_common.general_vessel_and_trip_information.observer_trip_detail_id
      optional_simple_column("OBSERVER_TRIP_DETAILS_DISEMBARKATION_AT_SEA_TIMESTAMP", column_location("ros_common.observer_trip_details", "date_time_disembarkation")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_SETS_CONDUCTED", column_location("ros_common.observed_trip_summary", "number_of_conducted_fishing_events_with_observer_onboard")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_SETS_OBSERVED", column_location("ros_common.observed_trip_summary", "number_of_observed_fishing_events")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_SEARCHING", column_location("ros_common.observed_trip_summary", "number_of_days_searching")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_ACTIVELY_FISHING", column_location("ros_common.observed_trip_summary", "number_of_active_fishing_days")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      optional_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_IN_FISHING_AREA", column_location("ros_common.observed_trip_summary", "number_of_days_in_fishing_area")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      optional_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_TRANSITING", column_location("ros_common.observed_trip_summary", "number_of_days_transiting")),
      # by ros_common.general_vessel_and_trip_information.observed_trip_summary_id
      mandatory_simple_column("OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_LOST", column_location("ros_common.observed_trip_summary", "number_of_days_lost")),
      # Fixme (not a fk?)
      # by ros_common.reasons_for_days_lost.observed_trip_summary_id
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_1_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")),
      # Fixme (not a fk?)
      # by ros_common.reasons_for_days_lost.observed_trip_summary_id
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_2_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")),
      # Fixme (not a fk?)
      # by ros_common.reasons_for_days_lost.observed_trip_summary_id
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_3_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")),
      # Fixme (not a fk?)
      # by ros_common.reasons_for_days_lost.observed_trip_summary_id
      optional_fk_column("OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_4_CODE", column_location("ros_common.reasons_for_days_lost", "code"), column_location("refs_fishery.reasons_days_lost", "code")))),
    #[ 3] = "V-INFO" | From row 6 - 32 columns
    sheet("V-INFO", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      mandatory_simple_column("VESSEL_INFORMATION_IDENTIFICATION_NAME", column_location("ros_common.vessel_identification", "NAME")),
      # Fixme (not found in table)
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING_CODE", column_location("ros_common.vessel_identification", "flag_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      mandatory_simple_column("VESSEL_INFORMATION_IDENTIFICATION_IOTC_NUMBER", column_location("ros_common.vessel_identification", "iotc_number")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_IMO_NUMBER", column_location("ros_common.vessel_identification", "imo_number")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_IRCS", column_location("ros_common.vessel_identification", "ircs")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_COUNTRY_CODE", column_location("ros_common.vessel_identification", "flag_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_PORT_CODE", column_location("ros_common.vessel_identification", "port_code"), column_location("refs_admin.ports", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_NUMBER", column_location("ros_common.vessel_identification", "registration_number")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification.vessel_identification_phone
      # Fixme Remove this nasty table texts...
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_1", column_location("ros_common.texts", "value")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification.vessel_identification_phone
      # Fixme Remove this nasty table texts...
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_2", column_location("ros_common.texts", "value")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification.vessel_identification_fax
      # Fixme Remove this nasty table texts...
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_1", column_location("ros_common.texts", "value")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification.vessel_identification_fax
      # Fixme Remove this nasty table texts...
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_2", column_location("ros_common.texts", "value")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification.vessel_identification_email
      # Fixme Remove this nasty table texts...
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_1", column_location("ros_common.texts", "value")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification.vessel_identification_email
      # Fixme Remove this nasty table texts...
      optional_simple_column("VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_2", column_location("ros_common.texts", "value")), # by vessel_identification_email
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification_licensed_target_species.vessel_identification_id
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_1_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification_licensed_target_species.vessel_identification_id
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_2_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification_licensed_target_species.vessel_identification_id
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_3_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      # by ros_common.vessel_identification_licensed_target_species.vessel_identification_id
      optional_fk_column("VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_4_CODE", column_location("ros_common.vessel_identification_licensed_target_species", "licensed_target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_identification_id
      mandatory_fk_column("VESSEL_INFORMATION_IDENTIFICATION_MAIN_FISHING_GEAR_CODE", column_location("ros_common.vessel_identification", "main_fishing_gear_code"), column_location("refs_fishery.gears", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.registered_vessel_owner_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_FULL_NAME", column_location("ros_common.person_contact_details", "full_name")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.registered_vessel_owner_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_NATIONALITY_CODE", column_location("ros_common.person_contact_details", "nationality_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.registered_vessel_owner_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_CONTACT_DETAILS", column_location("ros_common.person_contact_details", "contact_details")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.charter_or_operator_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_FULL_NAME", column_location("ros_common.person_contact_details", "full_name")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.charter_or_operator_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_NATIONALITY_CODE", column_location("ros_common.person_contact_details", "nationality_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.charter_or_operator_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS", column_location("ros_common.person_contact_details", "contact_details")),
      # Fixme : not found
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS2", column_location("ros_common.person_contact_details", "contact_details")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.fishing_master_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_FULL_NAME", column_location("ros_common.person_details", "full_name")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.fishing_master_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_NATIONALITY_CODE", column_location("ros_common.person_details", "nationality_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.fishing_master_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_FULL_NAME", column_location("ros_common.person_details", "full_name")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      # by ros_common.vessel_owner_and_personnel.fishing_master_id
      optional_fk_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_NATIONALITY_CODE", column_location("ros_common.person_details", "nationality_code"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_owner_and_personnel_id
      optional_simple_column("VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CREW_NUMBER", column_location("ros_common.vessel_owner_and_personnel", "number_of_crew")))),
    #[ 4] = "V-TRIP" | From row 6 - 11 columns
    sheet("V-TRIP", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      optional_fk_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_COUNTRY_CODE", column_location("a", "b"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_trip_details_id
      optional_fk_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_PORT_CODE", column_location("ros_common.vessel_trip_details", "departure_port_code"), column_location("refs_admin.ports", "code")),
      # Fixme : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_LATITUDE", column_location("a", "b")),
      # Fixme : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_DEPARTURE_PORT_LONGITUDE", column_location("a", "b")),
      # by ros_common.general_vessel_and_trip_information.vessel_trip_details_id
      optional_simple_column("TRIP_DETAILS_VESSEL_DEPARTURE_TIMESTAMP", column_location("ros_common.vessel_trip_details", "date_time_vessel_sailed")),
      # Fixme : not found
      optional_fk_column("TRIP_DETAILS_VESSEL_RETURN_PORT_COUNTRY_CODE", column_location("a", "b"), column_location("refs_admin.countries", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_trip_details_id
      optional_fk_column("TRIP_DETAILS_VESSEL_RETURN_PORT_PORT_CODE", column_location("ros_common.vessel_trip_details", "return_port_code"), column_location("refs_admin.ports", "code")),
      # Fixme : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_RETURN_PORT_LATITUDE", column_location("a", "b")),
      # Fixme : not found
      optional_simple_column("TRIP_DETAILS_VESSEL_RETURN_PORT_LONGITUDE", column_location("a", "b")),
      # by ros_common.general_vessel_and_trip_information.vessel_trip_details_id
      optional_simple_column("TRIP_DETAILS_VESSEL_RETURN_TIMESTAMP", column_location("ros_common.vessel_trip_details", "date_time_vessel_returned_to_port")))),
    #[ 5] = "V-ATTRIBUTES" | From row 6 - 50 columns
    sheet("V-ATTRIBUTES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      mandatory_measurement_column("VESSEL_ATTRIBUTES_TONNAGE_VALUE", column_location("ros_common.vessel_attributes", "tonnage_id"), "ros_common.tonnages"),
      mandatory_measurement_unit_column("VESSEL_ATTRIBUTES_TONNAGE_GT_GRT", c("GT", "GRT")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      mandatory_measurement_column("VESSEL_ATTRIBUTES_LENGTH_OVERALL_VALUE", column_location("ros_common.vessel_attributes", "loa_id"), "ros_common.lengths"),
      mandatory_measurement_unit_column("VESSEL_ATTRIBUTES_LENGTH_OVERALL_M_FT", c("M", "FT")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      mandatory_fk_column("VESSEL_ATTRIBUTES_HULL_MATERIAL_CODE", column_location("ros_common.vessel_attributes", "hull_material_code"), column_location("refs_fishery.hull_material_types", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.vessel_attributes_id_me
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.main_engine_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_1_MAKE", column_location("ros_common.engines", "make")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.vessel_attributes_id_me
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.main_engine_id
      mandatory_measurement_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_1_POWER_VALUE", column_location("ros_common.engines", "value"), "ros_common.engines"),
      mandatory_measurement_unit_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_1_KW_HP_BHP", c("KW", "HP", "BHP")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.vessel_attributes_id_me
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.main_engine_id
      optional_simple_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_2_MAKE", column_location("ros_common.engines", "make")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.vessel_attributes_id_me
      # by ros_common.vessel_attributes.vessel_attributes_main_engines.main_engine_id
      optional_measurement_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_2_POWER_VALUE", column_location("ros_common.engines", "value"), "ros_common.engines"),
      optional_measurement_unit_column("VESSEL_ATTRIBUTES_MAIN_ENGINE_2_KW_HP_BHP", c("KW", "HP", "BHP")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      mandatory_measurement_column("VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_VALUE", column_location("ros_common.vessel_attributes", "fish_storage_capacity_id"), "ros_common.capacities"),
      optional_measurement_unit_column("VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_T_M3", c("T", "M3")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_preservation_method.vessel_attributes_id_fpm
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_1_CODE", column_location("ros_common.vessel_attributes_fish_preservation_method", "fish_preservation_method_code"), column_location("refs_fishery.fish_preservation_methods", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_preservation_method.vessel_attributes_id_fpm
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_2_CODE", column_location("ros_common.vessel_attributes_fish_preservation_method", "fish_preservation_method_code"), column_location("refs_fishery.fish_preservation_methods", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_preservation_method.vessel_attributes_id_fpm
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_3_CODE", column_location("ros_common.vessel_attributes_fish_preservation_method", "fish_preservation_method_code"), column_location("refs_fishery.fish_preservation_methods", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_preservation_method.vessel_attributes_id_fpm
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_4_CODE", column_location("ros_common.vessel_attributes_fish_preservation_method", "fish_preservation_method_code"), column_location("refs_fishery.fish_preservation_methods", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_storage_type.vessel_attributes_id_fst
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_1_CODE", column_location("ros_common.vessel_attributes_fish_storage_type", "fish_storage_type_code"), column_location("refs_fishery.fish_storage_types", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_storage_type.vessel_attributes_id_fst
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_2_CODE", column_location("ros_common.vessel_attributes_fish_storage_type", "fish_storage_type_code"), column_location("refs_fishery.fish_storage_types", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      # by ros_common.vessel_attributes_fish_storage_type.vessel_attributes_id_fst
      optional_fk_column("VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_3_CODE", column_location("ros_common.vessel_attributes_fish_storage_type", "fish_storage_type_code"), column_location("refs_fishery.fish_storage_types", "code")),
      # by ros_common.general_vessel_and_trip_information.vessel_attributes_id
      optional_measurement_column("VESSEL_ATTRIBUTES_AUTONOMY_RANGE_VALUE", column_location("ros_common.vessel_attributes", "autonomy_range_id"), "ros_common.ranges"),
      optional_measurement_unit_column("VESSEL_ATTRIBUTES_AUTONOMY_RANGE_DAYS_NM", c("DAYS", "NM")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_GPS", column_location("ros_common.vessel_electronics", "gps")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VMS", column_location("ros_common.vessel_electronics", "vms")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_AIS", column_location("ros_common.vessel_electronics", "ais")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_RADARS", column_location("ros_common.vessel_electronics", "radars")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_TRACK_PLOTTER", column_location("ros_common.vessel_electronics", "track_plotter")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DEPTH_SOUNDER", column_location("ros_common.vessel_electronics", "depth_sounder")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SONAR", column_location("ros_common.vessel_electronics", "sonar")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DOPPLER_CURRENT_METER", column_location("ros_common.vessel_electronics", "doppler_current_meter")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      mandatory_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_XBT", column_location("ros_common.vessel_electronics", "expendable_bathythermographs")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VHF_RADIOS", column_location("ros_common.vessel_electronics", "vhf_radios")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_HF_RADIOS", column_location("ros_common.vessel_electronics", "hf_radios")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SATELLITE_COMM", column_location("ros_common.vessel_electronics", "satellite_communication_systems")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SST_GAUGE", column_location("ros_common.vessel_electronics", "sea_surface_temperature_gauge")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_WEATHER_FAX", column_location("ros_common.vessel_electronics", "weather_facsimile")),
      # by ros_common.general_vessel_and_trip_information.vessel_electronics_id
      optional_simple_column("VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_FIS", column_location("ros_common.vessel_electronics", "fisheries_information_services")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id      
      mandatory_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_CATEGORY_1_CODE", column_location("ros_common.waste_managements", "waste_category_code"), column_location("refs_fishery.waste_categories", "code")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      mandatory_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_STORAGE_DISPOSAL_METHOD_1_CODE", column_location("ros_common.waste_managements", "waste_storage_or_disposal_method_code"), column_location("refs_fishery.waste_disposal_methods", "code")),
      # Fixme : not found
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_1", column_location("ros_common.waste_managements", "b")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_CATEGORY_2_CODE", column_location("ros_common.waste_managements", "waste_category_code"), column_location("refs_fishery.waste_categories", "code")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_STORAGE_DISPOSAL_METHOD_2_CODE", column_location("ros_common.waste_managements", "waste_storage_or_disposal_method_code"), column_location("refs_fishery.waste_disposal_methods", "code")),
      # Fixme : not found
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_OTHER_2", column_location("ros_common.waste_managements", "b")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_CATEGORY_3_CODE", column_location("ros_common.waste_managements", "waste_category_code"), column_location("refs_fishery.waste_categories", "code")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_STORAGE_DISPOSAL_METHOD_3_CODE", column_location("ros_common.waste_managements", "waste_storage_or_disposal_method_code"), column_location("refs_fishery.waste_disposal_methods", "code")),
      # Fixme : not found
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_OTHER_3", column_location("ros_common.waste_managements", "b")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_CATEGORY_4_CODE", column_location("ros_common.waste_managements", "waste_category_code"), column_location("refs_fishery.waste_categories", "code")),
      # by ros_common.waste_managements.general_vessel_and_trip_information_id
      optional_fk_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_STORAGE_DISPOSAL_METHOD_4_CODE", column_location("ros_common.waste_managements", "waste_storage_or_disposal_method_code"), column_location("refs_fishery.waste_disposal_methods", "code")),
      # Fixme : not found
      optional_simple_column("VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_OTHER_4", column_location("ros_common.waste_managements", "b")))),
    #[ 6] = "G-GENERAL" | From row 6 - 33 columns
    sheet("G-GENERAL", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.special_equipment_id
      mandatory_simple_column("SPECIAL_EQUIPMENT_OR_MACHINERY_LINE_SETTER", column_location("ros_ll.special_equipment", "line_setter")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.special_equipment_id
      mandatory_simple_column("SPECIAL_EQUIPMENT_OR_MACHINERY_LINE_HAULER", column_location("ros_ll.special_equipment", "line_hauler")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.special_equipment_id
      mandatory_simple_column("SPECIAL_EQUIPMENT_OR_MACHINERY_BAIT_CASTING_MACHINE", column_location("ros_ll.special_equipment", "bait_casting_machine")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      mandatory_fk_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_MATERIAL_CODE", column_location("ros_ll.general_gear_attributes", "line_material_type_code"), column_location("refs_fishery.line_material_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      mandatory_measurement_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_LENGTH_VALUE", column_location("ros_ll.general_gear_attributes", "mainline_length_id"), "ros_common.lengths"),
      mandatory_measurement_unit_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_LENGTH_KM_NM", c("KM", "NM")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      optional_measurement_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_DIAMETER_VALUE", column_location("ros_ll.general_gear_attributes", "mainline_diameter_id"), "ros_common.diameters"),
      optional_measurement_unit_column("GENERAL_GEAR_ATTRIBUTES_MAINLINE_DIAMETER_MM_CM", c("MM", "CM")),
      # Fixme : not found
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_1_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      # Fixme : not found
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_2_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      # Fixme : not found
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_3_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      # Fixme : not found
      optional_fk_column("ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_4_CODE", column_location("a", "b"), column_location("refs_fishery.branchline_storages", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      # by ros_ll.gear_specifications_mitigation_device.gear_specification_id
      optional_fk_column("MITIGATION_DEVICES_DEVICE_1_CODE", column_location("ros_ll.gear_specifications_mitigation_device", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      # by ros_ll.gear_specifications_mitigation_device.gear_specification_id
      optional_fk_column("MITIGATION_DEVICES_DEVICE_2_CODE", column_location("ros_ll.gear_specifications_mitigation_device", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      # by ros_ll.gear_specifications_mitigation_device.gear_specification_id
      optional_fk_column("MITIGATION_DEVICES_DEVICE_3_CODE", column_location("ros_ll.gear_specifications_mitigation_device", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      # by ros_ll.gear_specifications_mitigation_device.gear_specification_id
      optional_fk_column("MITIGATION_DEVICES_DEVICE_4_CODE", column_location("ros_ll.gear_specifications_mitigation_device", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.general_gear_attributes_id
      # by ros_ll.gear_specifications_mitigation_device.gear_specification_id
      optional_fk_column("MITIGATION_DEVICES_DEVICE_5_CODE", column_location("ros_ll.gear_specifications_mitigation_device", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_measurement_column("TORI_LINE_DETAILS_TORI_LINE_LENGTH_VALUE", column_location("ros_ll.tori_line_details", "tori_line_length_id"), "ros_common.lengths"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_TORI_LINE_LENGTH_M_FT", c("M", "FT")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_fk_column("TORI_LINE_DETAILS_STREAMER_TYPE_CODE", column_location("ros_ll.tori_line_details", "streamer_type"), column_location("refs_fishery.streamer_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_simple_column("TORI_LINE_DETAILS_STREAMER_REACH_SURFACE", column_location("ros_ll.tori_line_details", "streamers_reach_surface")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_measurement_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_VALUE", column_location("ros_ll.tori_line_details", "streamer_line_length_max_id"), "ros_common.lengths"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_CM_M_FT", c("CM", "M", "FT")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_measurement_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_VALUE", column_location("ros_ll.tori_line_details", "streamer_line_length_min_id"), "ros_common.lengths"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_CM_M_FT", c("CM", "M", "FT")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_simple_column("TORI_LINE_DETAILS_STREAMER_NUMBER_PER_LINE", column_location("ros_ll.tori_line_details", "number_of_streamers_per_line")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_measurement_column("TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_VALUE", column_location("ros_ll.tori_line_details", "streamer_distance_id"), "ros_common.distances"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_CM_M_FT", c("CM", "M", "FT")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_measurement_column("TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_VALUE", column_location("ros_ll.tori_line_details", "attached_height_id"), "ros_common.heights"),
      optional_measurement_unit_column("TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_CM_M_FT", c("CM", "M", "FT")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_simple_column("TORI_LINE_DETAILS_TOWED_OBJECTS_NUMBER", column_location("ros_ll.tori_line_details", "towed_objects_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.gear_specifications.tori_line_detail_id
      optional_simple_column("TORI_LINE_DETAILS_TOWED_OBJECTS_TYPE", column_location("ros_ll.tori_line_details", "towed_objects_type")))),
    #[ 7] = "G-CONFIG-BRANCHLINES" | From row 6 - 8 columns
    sheet("G-CONFIG-BRANCHLINES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data.gear_specifications_id
      # by ros_ll.branchline_configurations.gear_specifications_id
      mandatory_simple_column("BRANCHLINE_CONFIGURATIONS_CONFIGURATION_NUMBER", column_location("ros_ll.branchline_configurations", "configuration_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data.gear_specifications_id
      # by ros_ll.branchline_configurations.gear_specifications_id
      # by ros_ll.branchline_sections.branchline_configuration_id
      mandatory_simple_column("BRANCHLINE_CONFIGURATIONS_SECTION_NUMBER", column_location("ros_ll.branchline_sections", "section_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data.gear_specifications_id
      # by ros_ll.branchline_configurations.gear_specifications_id
      # by ros_ll.branchline_sections.branchline_configuration_id
      optional_fk_column("BRANCHLINE_CONFIGURATIONS_MATERIAL_TYPE_CODE", column_location("ros_ll.branchline_sections", "branchline_material_type_code"), column_location("refs_fishery.line_material_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data.gear_specifications_id
      # by ros_ll.branchline_configurations.gear_specifications_id
      # by ros_ll.branchline_sections.branchline_configuration_id
      mandatory_measurement_column("BRANCHLINE_CONFIGURATIONS_LENGTH_VALUE", column_location("ros_ll.branchline_sections", "length_id"), "ros_common.lengths"),
      mandatory_measurement_unit_column("BRANCHLINE_CONFIGURATIONS_LENGTH_CM_M", c("CM", "M")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data.gear_specifications_id
      # by ros_ll.branchline_configurations.gear_specifications_id
      # by ros_ll.branchline_sections.branchline_configuration_id      
      mandatory_measurement_column("BRANCHLINE_CONFIGURATIONS_DIAMETER_VALUE", column_location("ros_ll.branchline_sections", "diameter_id"), "ros_common.diameters"),
      mandatory_measurement_unit_column("BRANCHLINE_CONFIGURATIONS_DIAMETER_MM_CM", c("MM", "CM")))),
    #[ 8] = "E-SET" | From row 6 - 24 columns
    sheet("E-SET", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "b")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_START_SETTING_DATE_TIME_UTC", column_location("ros_ll.setting_operations", "start_setting_date_and_time")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_START_SETTING_LATITUDE", column_location("ros_ll.setting_operations", "start_setting_latitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_START_SETTING_LONGITUDE", column_location("ros_ll.setting_operations", "start_setting_longitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_END_SETTING_DATE_TIME_UTC", column_location("ros_ll.setting_operations", "end_setting_date_and_time")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_END_SETTING_LATITUDE", column_location("ros_ll.setting_operations", "end_setting_latitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_END_SETTING_LONGITUDE", column_location("ros_ll.setting_operations", "end_setting_longitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_SPEED_VESSEL_VALUE_KNOTS", column_location("ros_ll.setting_operations", "vessel_speed")), # Move this to a ros_common.speeds measurements table
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_SPEED_LINE_SETTER_VALUE_MS", column_location("ros_ll.setting_operations", "line_setter_speed")), # Move this to a ros_common.speeds measurements table
      # optional_measurement_column("SETTING_OPERATIONS_SPEED_VESSEL_VALUE_KNOTS", column_location("ros_ll.setting_operations", "b"), "ros_common.speeds","KNOTS"),
      # optional_measurement_column("SETTING_OPERATIONS_SPEED_LINE_SETTER_VALUE_MS", column_location("ros_ll.setting_operations", "b"), "ros_common.speeds","MS"),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_measurement_column("SETTING_OPERATIONS_MAINLINE_LENGTH_VALUE", column_location("ros_ll.setting_operations", "mainline_set_length_id"), "ros_common.lengths"),
      mandatory_measurement_unit_column("SETTING_OPERATIONS_MAINLINE_LENGTH_KM_NM", c("KM", "NM")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_CLIP_ON_TIME_BRANCHLINE", column_location("ros_ll.setting_operations", "branchline_clip_on_time")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_CLIP_ON_TIME_BUOYS", column_location("ros_ll.setting_operations", "buoys_clip_on_time")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_NUM_TOTAL_HOOKS_SET", column_location("ros_ll.setting_operations", "total_number_of_hooks_set")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_NUM_TOTAL_FLOATS_SET", column_location("ros_ll.setting_operations", "total_number_of_floats_set")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_NUM_HOOKS_BETWEEN_FLOATS", column_location("ros_ll.setting_operations", "number_of_hooks_set_between_floats")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_SHARK_LINES_SET", column_location("ros_ll.setting_operations", "shark_lines_set")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_SHARK_LINES_NUM_LINES", column_location("ros_ll.setting_operations", "number_of_shark_lines_set")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.setting_operations_target_species.setting_operation_id
      mandatory_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_1_CODE", column_location("ros_ll.setting_operations_target_species", "target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.setting_operations_target_species.setting_operation_id
      optional_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_2_CODE", column_location("ros_ll.setting_operations_target_species", "target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.setting_operations_target_species.setting_operation_id
      optional_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_3_CODE", column_location("ros_ll.setting_operations_target_species", "target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.setting_operations_target_species.setting_operation_id
      optional_fk_column("SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_4_CODE", column_location("ros_ll.setting_operations_target_species", "target_species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.setting_operations_target_species.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_VMS_ON", column_location("ros_ll.setting_operations", "vms_on")))),
    #[ 9] = "E-SET-LIGHTS" | From row 6 - 5 columns
    sheet("E-SET-LIGHTS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.lights_by_type_and_colour.setting_operation_id
      optional_fk_column("SETTING_OPERATIONS_ATTACHED_LIGHTS_TYPE_CODE", column_location("ros_ll.lights_by_type_and_colour", "light_type_code"), column_location("refs_fishery.light_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.lights_by_type_and_colour.setting_operation_id
      optional_fk_column("SETTING_OPERATIONS_ATTACHED_LIGHTS_COLOR_CODE", column_location("ros_ll.lights_by_type_and_colour", "light_colour_code"), column_location("refs_fishery.light_colours", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.lights_by_type_and_colour.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_ATTACHED_LIGHTS_NUM_LIGHTS", column_location("ros_ll.lights_by_type_and_colour", "number_of_lights_by_type_and_colour")))),
    #[ 10] = "E-SET-BRANCHLINES" | From row 6 - 4 columns
    sheet("E-SET-BRANCHLINES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.branchlines_set.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_BRANCHLINE_DETAILS_CONFIGURATION_NUMBER", column_location("ros_ll.branchlines_set", "branchline_configuration_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.branchlines_set.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_BRANCHLINE_DETAILS_NUM_BRANCHLINES_SET", column_location("ros_ll.branchlines_set", "number_of_branchlines")))),
    #[ 11] = "E-SET-HOOKS" | From row 6 - 5 columns
    sheet("E-SET-HOOKS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      mandatory_fk_column("SETTING_OPERATIONS_HOOKS_DETAILS_TYPE_CODE", column_location("ros_ll.hooks_by_type", "hook_type_code"), column_location("refs_fishery.hook_types", "code")),
      mandatory_simple_column("SETTING_OPERATIONS_HOOKS_DETAILS_PERCENTAGE", column_location("ros_ll.hooks_by_type", "percentage_of_set")),
      optional_simple_column("SETTING_OPERATIONS_HOOKS_DETAILS_VARIATIONS", column_location("ros_ll.hooks_by_type", "variations")))),
    #[ 12] = "E-SET-BAITS" | From row 6 - 6 columns
    sheet("E-SET-BAITS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.baits_by_conditions.setting_operation_id
      mandatory_fk_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_CONDITION_CODE", column_location("ros_ll.baits_by_conditions", "bait_condition_code"), column_location("refs_biological.bait_conditions", "code")), # Not Found
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.baits_by_conditions.setting_operation_id
      mandatory_fk_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_SPECIES_CODE", column_location("ros_ll.baits_by_conditions", "species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.baits_by_conditions.setting_operation_id
      mandatory_simple_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_PERCENTAGE", column_location("ros_ll.baits_by_conditions", "ratio")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.baits_by_conditions.setting_operation_id
      optional_simple_column("SETTING_OPERATIONS_BAITS_DETAILS_BAIT_DYE_COLOUR", column_location("ros_ll.baits_by_conditions", "dye_colour")))),
    #[13] = "E-SET-MITIGATION-MEASURES" | From row 6 - 14 columns
    sheet("E-SET-MITIGATION-MEASURES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_NUM_TORI_LINES_DEPLOYED", column_location("ros_ll.mitigation_measures", "number_of_tori_lines_deployed")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_MIN_DECK_LIGHTING_USED", column_location("ros_ll.mitigation_measures", "minimum_deck_lighting_used")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_HOOKS_SET_BETWEEN_DUSK_DAWN", column_location("ros_ll.mitigation_measures", "hooks_set_between_dusk_and_dawn")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_BRANCHLINE_WEIGHTED", column_location("ros_ll.mitigation_measures", "branchline_weighted")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_measurement_column("SETTING_OPERATIONS_MITIGATION_MEASURES_AVG_BRANCHLINE_WEIGHT_G", column_location("ros_ll.mitigation_measures", "branchline_average_weight_id"), "ros_common.weights", "G"),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_PERC_BRANCHLINE_WEIGHTED", column_location("ros_ll.mitigation_measures", "percentage_of_branchlines_weighted")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      mandatory_measurement_column("SETTING_OPERATIONS_MITIGATION_MEASURES_HOOK_SINKER_DISTANCE_CM", column_location("ros_ll.mitigation_measures", "hook_sinker_distance_id"), "ros_common.distances", "CM"),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      optional_simple_column("SETTING_OPERATIONS_MITIGATION_MEASURES_UNDERWATER_SETTING", column_location("ros_ll.mitigation_measures", "underwater_setting")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      # by ros_ll.mitigation_measures_mitigation_devices.mitigation_measure_id
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_1_CODE", column_location("ros_ll.mitigation_measures_mitigation_devices", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      # by ros_ll.mitigation_measures_mitigation_devices.mitigation_measure_id
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_2_CODE", column_location("ros_ll.mitigation_measures_mitigation_devices", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      # by ros_ll.mitigation_measures_mitigation_devices.mitigation_measure_id
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_3_CODE", column_location("ros_ll.mitigation_measures_mitigation_devices", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.mitigation_measure_id
      # by ros_ll.mitigation_measures_mitigation_devices.mitigation_measure_id
      optional_fk_column("SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_4_CODE", column_location("ros_ll.mitigation_measures_mitigation_devices", "mitigation_device_code"), column_location("refs_fishery.mitigation_devices", "code")))),
    #[14] = "E-SET-HAULING" | From row 6 - 20 columns
    sheet("E-SET-HAULING", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      mandatory_simple_column("HAULING_OPERATIONS_START_HAULING_DATE_TIME_UTC", column_location("ros_ll.hauling_operations", "start_hauling_date_and_time")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      mandatory_simple_column("HAULING_OPERATIONS_START_HAULING_LATITUDE", column_location("ros_ll.hauling_operations", "start_hauling_latitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      mandatory_simple_column("HAULING_OPERATIONS_START_HAULING_LONGITUDE", column_location("ros_ll.hauling_operations", "start_hauling_longitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_END_HAULING_DATE_TIME_UTC", column_location("ros_ll.hauling_operations", "end_hauling_date_and_time")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_END_HAULING_LATITUDE", column_location("ros_ll.hauling_operations", "end_hauling_latitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_END_HAULING_LONGITUDE", column_location("ros_ll.hauling_operations", "end_hauling_longitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_OFFAL_MANAGEMENT", column_location("ros_ll.hauling_operations", "offal_management")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_offal_disposal_positions.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_1_CODE", column_location("ros_ll.hauling_offal_disposal_positions", "offal_disposal_position")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_offal_disposal_positions.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_2_CODE", column_location("ros_ll.hauling_offal_disposal_positions", "offal_disposal_position")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_offal_disposal_positions.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_3_CODE", column_location("ros_ll.hauling_offal_disposal_positions", "offal_disposal_position")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_offal_disposal_positions.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_4_CODE", column_location("ros_ll.hauling_offal_disposal_positions", "offal_disposal_position")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_operations_stunning_methods.hauling_operation_id
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_1_CODE", column_location("ros_ll.hauling_operations_stunning_methods", "stunning_method_code"), column_location("refs_fishery.stunning_methods", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_operations_stunning_methods.hauling_operation_id
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_2_CODE", column_location("ros_ll.hauling_operations_stunning_methods", "stunning_method_code"), column_location("refs_fishery.stunning_methods", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_operations_stunning_methods.hauling_operation_id
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_3_CODE", column_location("ros_ll.hauling_operations_stunning_methods", "stunning_method_code"), column_location("refs_fishery.stunning_methods", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      # by ros_ll.hauling_operations_stunning_methods.hauling_operation_id
      optional_fk_column("HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_4_CODE", column_location("ros_ll.hauling_operations_stunning_methods", "stunning_method_code"), column_location("refs_fishery.stunning_methods", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      optional_simple_column("HAULING_OPERATIONS_BIRD_SCARING_DEVICE_AT_HAULER", column_location("ros_ll.hauling_operations", "bird_scaring_device_at_hauler")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      mandatory_simple_column("HAULING_OPERATIONS_NUM_HOOKS_RETRIEVED_DURING_OBSERVATION", column_location("ros_ll.hauling_operations", "number_of_hooks_observed")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.hauling_operation_id
      mandatory_fk_column("HAULING_OPERATIONS_SAMPLING_PROTOCOL_CODE", column_location("ros_ll.hauling_operations", "sampling_protocol_code"), column_location("refs_biological.sampling_protocols", "code")))),
    #[15] = "E-SET-HAULING-BITEOFFS" | From row 6 - 4 columns
    sheet("E-SET-HAULING-BITEOFFS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.baits_by_conditions.setting_operations_id
      optional_simple_column("HAULING_OPERATIONS_BITEOFFS_DETAILS_BRANCHLINE_CONFIGURATION_NUMBER", column_location("ros_ll.biteoffs_by_branchlines_set", "branchline_configuration_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.fishing_events.setting_operation_id
      # by ros_ll.baits_by_conditions.setting_operations_id
      optional_simple_column("HAULING_OPERATIONS_BITEOFFS_DETAILS_BRANCHLINE_NUM_BITEOFFS", column_location("ros_ll.biteoffs_by_branchlines_set", "number_of_biteoffs")))),
    #[16] = "E-SET-CATCHES" | From row 6 - 12 columns
    sheet("E-SET-CATCHES", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      mandatory_simple_column("CATCH_NUMBER", column_location("ros_ll.catch_details", "catch_detail_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      mandatory_fk_column("CATCH_DETAILS_SPECIES_CODE", column_location("ros_ll.catch_details", "species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      mandatory_fk_column("CATCH_DETAILS_FATE_TYPE", column_location("ros_ll.catch_details", "type_of_fate_code"), column_location("refs_biological.types_of_fate", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      mandatory_fk_column("CATCH_DETAILS_FATE_CODE", column_location("ros_ll.catch_details", "fates_code"), column_location("refs_biological.fates", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      mandatory_fk_column("CATCH_DETAILS_SAMPLING_METHOD_CODE", column_location("ros_ll.catch_details", "estimated_weight_sampling_method_code"), column_location("refs_biological.sampling_methods_for_catch_estimation", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      optional_simple_column("CATCH_DETAILS_NUM_FISH", column_location("ros_ll.catch_details", "estimated_catch_in_numbers")),
      # Fixme : not found
      optional_fk_column("CATCH_DETAILS_WEIGHT_PROCESSING_TYPE_CODE", column_location("ros_ll.catch_details", "b"), column_location("refs_fishery.fish_processing_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      optional_measurement_column("CATCH_DETAILS_WEIGHT_VALUE", column_location("ros_ll.catch_details", "estimated_weight_id"), "ros_common.estimated_weights"),
      optional_measurement_unit_column("CATCH_DETAILS_WEIGHT_KG_T", c("KG", "T")),
      # Fixme : not found
      optional_fk_column("CATCH_DETAILS_WEIGHT_ESTIMATION_METHOD_CODE", column_location("ros_ll.catch_details", "b"), column_location("refs_biological.sampling_methods_for_catch_estimation", "code")))),
    #[17] = "E-SET-CATCHES-SPECIMEN" | From row 6 - 32 columns
    sheet("E-SET-CATCHES-SPECIMEN", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      mandatory_simple_column("CATCH_NUMBER", column_location("ros_ll.catch_details", "catch_detail_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      mandatory_simple_column("SPECIMEN_NUMBER", column_location("ros_ll.specimens", "specimen_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLING_PERIOD_CODE", column_location("ros_ll.specimens", "b"), column_location("refs_biological.sampling_periods", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_specimen_details_non_target_species_id
      optional_fk_column("SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_CAPTURE_CODE", column_location("ros_common.additional_details_on_non_target_species", "condition_at_capture_code"), column_location("refs_biological.incidental_captures_conditions", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_specimen_details_non_target_species_id
      optional_fk_column("SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_RELEASE_CODE", column_location("ros_common.additional_details_on_non_target_species", "condition_at_release_code"), column_location("refs_biological.incidental_captures_conditions", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.depredation_detail_id (Fixme : why not using refs_biological.depredation_sources?)
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_DEPREDATION_SOURCE_CODE", column_location("ros_common.depredation_details", "depredation_source_code"), column_location("refs_biological.scars", "code")), # Link not found
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.depredation_detail_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_OBSERVED_PREDATOR_CODE", column_location("ros_common.depredation_details", "predator_observed_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLING_METHOD_CODE", column_location("ros_common.biometric_information", "bio_collection_sampling_method_code"), column_location("refs_biological.sampling_methods_for_sampling_collections", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_TYPE_CODE", column_location("ros_common.measured_lengths", "type_of_measurement_code"), column_location("refs_biological.types_of_measurement", "code")), # Check
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      mandatory_measurement_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_VALUE_CM", column_location("ros_common.measured_lengths", "value"), "ros_common.measured_lengths", "CM"),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      mandatory_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_MEASURING_TOOL_CODE", column_location("ros_common.measured_lengths", "length_measuring_tool_code"), column_location("refs_biological.measurement_tools", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_IS_STRAIGHT", column_location("ros_common.measured_lengths", "curved")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_TYPE_CODE", column_location("ros_common.measured_lengths", "type_of_measurement_code"), column_location("refs_biological.types_of_measurement", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      optional_measurement_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_VALUE_CM", column_location("ros_common.measured_lengths", "value"), "ros_common.measured_lengths", "CM"),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_MEASURING_TOOL_CODE", column_location("ros_common.measured_lengths", "length_measuring_tool_code"), column_location("refs_biological.measurement_tools", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.measured_length_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_IS_STRAIGHT", column_location("ros_common.measured_lengths", "curved")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.estimated_weight_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_PROCESSING_TYPE_CODE", column_location("ros_common.measured_weights", "processing_type_code"), column_location("refs_fishery.fish_processing_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.estimated_weight_id
      optional_measurement_column("SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_VALUE_KG", column_location("ros_common.measured_weights", "value"), "ros_common.estimated_weights", "KG"),
      # Fixme : not found
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_ESTIMATION_METHOD_CODE", column_location("a", "b"), column_location("refs_biological.", "code")), # Not found
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_SEX", column_location("ros_common.biometric_information", "sex_code"), column_location("refs_biological.sex", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.maturity_stage_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_MATURITY_SCALE", column_location("ros_common.maturity_stages", "scale")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.maturity_stage_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_MATURITY_STAGE", column_location("ros_common.maturity_stages", "maturity_level")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.sample_collection_detail_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLE_COLLECTED_TYPE", column_location("ros_common.sample_collection_detailsa", "sample_type")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.biometric_information_id
      # by ros_common.biometric_information.sample_collection_detail_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_SAMPLE_COLLECTED_PRESERVATION_METHOD", column_location("ros_common.sample_collection_details", "preservation_method")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.tag_detail_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RELEASE", column_location("ros_ll.tag_details", "tag_release")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.tag_detail_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RECOVERY", column_location("ros_ll.tag_details", "tag_recovery")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.tag_detail_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TYPE_CODE", column_location("ros_ll.tag_details", "tag_type_code"), column_location("refs_biological.tag_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.tag_detail_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_1", column_location("ros_ll.tag_details", "tag_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.tag_detail_id
      optional_simple_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_2", column_location("ros_ll.tag_details", "alternate_tag_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.tag_detail_id
      optional_fk_column("SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_FINDER_NAME_AND_CONTACT_DETAILS", column_location("ros_ll.tag_details", "tag_finder_id"), column_location("ros_common.person_contact_details", "id")))),
    #[18] = "E-SET-CATCHES-SPECIMEN-SSI" | From row 6 - 16 columns
    sheet("E-SET-CATCHES-SPECIMEN-SSI", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("SET_NUMBER", column_location("ros_ll.setting_operations", "set_number")),
      mandatory_simple_column("CATCH_NUMBER", column_location("ros_ll.catch_details", "catch_detail_number")),
      mandatory_simple_column("SPECIMEN_NUMBER", column_location("ros_ll.specimens", "specimen_number")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_GEAR_INTERACTION_CODE", column_location("ros_ll.additional_catch_details_on_ssi", "gear_interaction_code"), column_location("refs_biological.gear_interactions", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HOOK_TYPE_CODE", column_location("ros_ll.additional_catch_details_on_ssi", "b"), column_location("refs_fishery.hook_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_CONDITION_CODE", column_location("ros_ll.additional_catch_details_on_ssi", "bait_condition_code"), column_location("refs_biological.bait_conditions", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_TYPE_CODE", column_location("os_ll.additional_catch_details_on_ssi", "bait_type"), column_location("refs_biological.bait_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_MATERIAL_CODE", column_location("ros_ll.additional_catch_details_on_ssi", "leader_material_type_code"), column_location("refs_fishery.line_material_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_measurement_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_DIAMETER_MM", column_location("ros_ll.additional_catch_details_on_ssi", "leader_thickness_id"), "ros_common.thicknesses", "MM"), # Check
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_DE_HOOKER_DEVICE_CODE", column_location("ros_ll.additional_catch_details_on_ssi", "dehooker_device_code"), column_location("refs_fishery.dehooker_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LIGHT_ATTACHED", column_location("ros_ll.additional_catch_details_on_ssi", "light_attached_to_branchline")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BROUGHT_ONBOARD", column_location("ros_ll.additional_catch_details_on_ssi", "brought_on_board")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_fk_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HANDLING_METHOD_CODE", column_location("ros_ll.additional_catch_details_on_ssi", "handling_method_code"), column_location("refs_biological.handling_methods", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_REVIVAL", column_location("ros_ll.additional_catch_details_on_ssi", "revival")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.fishing_events.observer_data_id
      # by ros_ll.catch_details.fishing_event_id
      # by ros_ll.specimens.catch_detail_id
      # by ros_ll.specimens.additional_catch_details_on_ssis_id
      optional_simple_column("SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_PHOTO_ID", column_location("ros_ll.additional_catch_details_on_ssi", "photo_id")))),
    #[19] = "T-EVENTS" | From row 6 - 15 columns
    sheet("T-EVENTS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("TRANSSHIPMENT_NUMBER", column_location("a", "b")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_START_EVENT_DATE_TIME_UTC", column_location("ros_common.transhipment_details", "transhipment_start_date_time")),
      # Fixme : there is only latitude and longitude on this table...
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_START_EVENT_LATITUDE", column_location("ros_common.transhipment_details", "latitude")),
      # Fixme : there is only latitude and longitude on this table...
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_START_EVENT_LONGITUDE", column_location("ros_common.transhipment_details", "longitude")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_END_EVENT_DATE_TIME_UTC", column_location("ros_common.transhipment_details", "transhipment_end_date_time")),
      # Fixme : there is only latitude and longitude on this table...
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_END_EVENT_LATITUDE", column_location("ros_common.transhipment_details", "latitude")),
      # Fixme : there is only latitude and longitude on this table...
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_END_EVENT_LONGITUDE", column_location("ros_common.transhipment_details", "longitude")),
      # Fixme : this is not linked to code-list refs_fishery.transhipment_categories
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CATEGORY_CODE", column_location("ros_common.transhipment_details", "category")),
      # optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CATEGORY_CODE", column_location("ros_common.transhipment_details", "category"), column_location("refs_fishery.transhipment_categories", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details.carrier_vessel_identification_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_NAME", column_location("ros_common.carrier_vessel_identification", "vessel_name")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details.carrier_vessel_identification_id
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_FLAG_OR_CHARTERING_CODE", column_location("ros_common.carrier_vessel_identification", "vessel_flag_code"), column_location("refs_admin.countries", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details.carrier_vessel_identification_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_IRCS", column_location("ros_common.carrier_vessel_identification", "vessel_ircs")),
      # Fixme : not found, w<er have only here flag_code
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details.carrier_vessel_identification_id
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_REGISTRATION_COUNTRY_CODE", column_location("ros_common.carrier_vessel_identification", "b"), column_location("refs_admin.countries", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details.carrier_vessel_identification_id
      optional_fk_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_REGISTRATION_PORT_CODE", column_location("ros_common.carrier_vessel_identification", "vessel_registration_port_code"), column_location("refs_admin.ports", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details.carrier_vessel_identification_id
      optional_simple_column("VESSEL_TRANSSHIPMENT_EVENT_CARRIER_OR_FISHING_VESSEL_REGISTRATION_NUMBER", column_location("ros_common.carrier_vessel_identification", "vessel_registration_number")))),
    #[20] = "T-PRODUCTS" | From row 6 - 6 columns
    sheet("T-PRODUCTS", c(
      mandatory_simple_column("OBSERVED_TRIP_NUMBER", column_location("ros_common.general_vessel_and_trip_information", "trip_number")),
      # Fixme : not found
      mandatory_simple_column("TRANSSHIPMENT_NUMBER", column_location("a", "b")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details_product_transhipped.transhipment_detail_id
      # by ros_common.transhipment_details_product_transhipped.species_by_product_id
      optional_fk_column("PRODUCT_TRANSSHIPPED_SPECIES_CODE", column_location("ros_common.species_by_product_type", "species_code"), column_location("refs_biological.species", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details_product_transhipped.transhipment_detail_id
      # by ros_common.transhipment_details_product_transhipped.species_by_product_id
      optional_fk_column("PRODUCT_TRANSSHIPPED_PROCESSING_TYPE_CODE", column_location("ros_common.species_by_product_type", "processing_type_code"), column_location("refs_fishery.fish_processing_types", "code")),
      # by ros_ll.observer_data.vessel_and_trip_information_id
      # by ros_ll.observer_data_transhipment_details.transhipment_detail_id
      # by ros_common.transhipment_details_product_transhipped.transhipment_detail_id
      # by ros_common.transhipment_details_product_transhipped.species_by_product_id
      optional_measurement_column("PRODUCT_TRANSSHIPPED_QUANTITY_VALUE", column_location("ros_common.species_by_product_type", "value"), "ros_common.species_by_product_type"),
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
taiste <- load_ll_3_2_1_xls(path)
