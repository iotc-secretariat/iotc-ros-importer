# LIBRARIES ####
library(iotc.base.common.data)

load_codelist <- function(codelist_domain, codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  if (is.null(columns)) {
    columns <- "*"
  } else {
    columns <- paste0(columns, collapse = ", ")
  }
  return(
    query(
      connection,
      paste0("SELECT ", columns, " FROM ", codelist_domain, ".", codelist_name)
    )
  )
}

admin_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_admin", codelist_name, columns, connection))
}

gis_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_gis", codelist_name, columns, connection))
}

fishery_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_fishery", codelist_name, columns, connection))
}

fishery_config_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_fishery_config", codelist_name, columns, connection))
}

biological_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_biological", codelist_name, columns, connection))
}

biological_config_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_biological_config", codelist_name, columns, connection))
}

data_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_data", codelist_name, columns, connection))
}

legacy_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_legacy", codelist_name, columns, connection))
}

socio_economics_domain <- function(codelist_name, columns = NULL, connection = DB_IOTC_REFERENCE_DATA()) {
  return(load_codelist("refs_socio_economics", codelist_name, columns, connection))
}

# ADMIN REFERENCES ####

ADMIN_COUNTRIES <- admin_domain("countries")
ADMIN_COUNTRIES_CODES <- as.list(ADMIN_COUNTRIES[, code])
ADMIN_CPC_HISTORY <- admin_domain("cpc_history")
# ADMIN_CPC_HISTORY_CODES <- as.list(ADMIN_CPC_HISTORY[, cpc_code])
ADMIN_CPC_TO_FLAGS <- admin_domain("cpc_to_flags")
# ADMIN_CPC_TO_FLAGS_CODES <- as.list(ADMIN_CPC_TO_FLAGS[, code])
ADMIN_CPCS <- admin_domain("cpcs")
ADMIN_CPCS_CODES <- as.list(ADMIN_CPCS[, code])
ADMIN_ENTITIES <- admin_domain("entities")
ADMIN_ENTITIES_CODES <- as.list(ADMIN_ENTITIES[, code])
ADMIN_FLEETS <- admin_domain("fleets")
ADMIN_FLEETS_CODES <- as.list(ADMIN_FLEETS[, code])
ADMIN_FLEET_TO_FLAGS_AND_FISHERIES <- admin_domain("fleet_to_flags_and_fisheries")
# ADMIN_FLEET_TO_FLAGS_AND_FISHERIES_CODES <- as.list(ADMIN_FLEET_TO_FLAGS_AND_FISHERIES[, code])
ADMIN_FLEET_TO_FLAGS_AND_FISHERIES = merge(ADMIN_FLEET_TO_FLAGS_AND_FISHERIES, ADMIN_FLEETS, by.x = "fleet_code", by.y = "code")
# ADMIN_FLEET_TO_FLAGS_AND_FISHERIES_CODES = as.list(ADMIN_FLEET_TO_FLAGS_AND_FISHERIES[, code])
ADMIN_IO_MAIN_AREAS <- admin_domain("io_main_areas")
ADMIN_IO_MAIN_AREAS_CODES <- as.list(ADMIN_IO_MAIN_AREAS[, code])
ADMIN_PORTS <- admin_domain("ports")
ADMIN_PORTS_CODES <- as.list(ADMIN_PORTS[, code])
ADMIN_SPECIES_REPORTING_REQUIREMENTS <- admin_domain("species_reporting_requirements")
# ADMIN_SPECIES_REPORTING_REQUIREMENTS_CODES <- as.list(ADMIN_SPECIES_REPORTING_REQUIREMENTS[, code])


# DATA REFERENCES ####

DATA_COVERAGE_TYPES <- data_domain("coverage_types")
DATA_COVERAGE_TYPES_CODES <- as.list(DATA_COVERAGE_TYPES[, code])
DATA_DATASETS <- data_domain("datasets")
DATA_DATASETS_CODES <- as.list(DATA_DATASETS[, code])
DATA_ESTIMATIONS <- data_domain("estimations")
DATA_ESTIMATIONS_CODES <- as.list(DATA_ESTIMATIONS[, code])
DATA_PROCESSINGS <- data_domain("processings")
DATA_PROCESSINGS_CODES <- as.list(DATA_PROCESSINGS[, code])
DATA_RAISINGS <- data_domain("raisings")
DATA_RAISINGS_CODES <- as.list(DATA_RAISINGS[, code])
DATA_SOURCES <- data_domain("sources")
DATA_SOURCES_CODES <- as.list(DATA_SOURCES[, code])
DATA_TYPES <- data_domain("types")
DATA_TYPES_CODES <- as.list(DATA_TYPES[, code])


# GIS REFERENCES ####

# AREA_INTERSECTIONS <- "area_intersections"
# AREA_INTERSECTIONS_IOTC <- "area_intersections_iotc"
# AREA_TYPES <- "area_types"
# AREAS <- "areas"
#
# AREAS_COLUMNS = c("CODE", "NAME_EN", "NAME_FR", "OCEAN_AREA_KM2", "OCEAN_AREA_IO_KM2", "OCEAN_AREA_IOTC_KM2", "CENTER_LAT", "CENTER_LON")

# IOTC_AREA            = gis_domain("V_IOTC_AREA_OF_COMPETENCE",           columns = AREAS_COLUMNS)
# IOTC_AREAS           = gis_domain("V_IOTC_AREAS",          columns = AREAS_COLUMNS)
# IOTC_MAIN_AREAS      = gis_domain("V_IOTC_MAIN_AREAS",     columns = AREAS_COLUMNS)
# IOTC_GRIDS_CE_SF     = gis_domain("V_IOTC_GRIDS_CE_SF",    columns = AREAS_COLUMNS)
# IOTC_GRIDS_CE_SF_AR  = gis_domain("V_IOTC_GRIDS_CE_SF_AR", columns = AREAS_COLUMNS)
#
# IOTC_GRIDS_01x01     = gis_domain("V_IOTC_GRIDS_01x01", columns = AREAS_COLUMNS)
# IOTC_GRIDS_05x05     = gis_domain("V_IOTC_GRIDS_05x05", columns = AREAS_COLUMNS)
# IOTC_GRIDS_10x10     = gis_domain("V_IOTC_GRIDS_10x10", columns = AREAS_COLUMNS)
# IOTC_GRIDS_10x20     = gis_domain("V_IOTC_GRIDS_10x20", columns = AREAS_COLUMNS)
# IOTC_GRIDS_20x20     = gis_domain("V_IOTC_GRIDS_20x20", columns = AREAS_COLUMNS)
# IOTC_GRIDS_30x30     = gis_domain("V_IOTC_GRIDS_30x30", columns = AREAS_COLUMNS)
#
# IO_GRIDS_01x01       = gis_domain("V_IO_GRIDS_01x01",   columns = AREAS_COLUMNS)
# IO_GRIDS_05x05       = gis_domain("V_IO_GRIDS_05x05",   columns = AREAS_COLUMNS)
# IO_GRIDS_10x10       = gis_domain("V_IO_GRIDS_10x10",   columns = AREAS_COLUMNS)
# IO_GRIDS_10x20       = gis_domain("V_IO_GRIDS_10x20",   columns = AREAS_COLUMNS)
# IO_GRIDS_20x20       = gis_domain("V_IO_GRIDS_20x20",   columns = AREAS_COLUMNS)
# IO_GRIDS_30x30       = gis_domain("V_IO_GRIDS_30x30",   columns = AREAS_COLUMNS)

# FISHERY REFERENCES ####


FISHERY_ACTIVITIES <- fishery_domain("activities")
FISHERY_ACTIVITIES_CODES <- as.list(FISHERY_ACTIVITIES[, code])
FISHERY_BAIT_FISHING_METHODS <- fishery_domain("bait_fishing_methods")
FISHERY_BAIT_FISHING_METHODS_CODES <- as.list(FISHERY_BAIT_FISHING_METHODS[, code])
FISHERY_BAIT_SCHOOL_DETECTION_METHODS <- fishery_domain("bait_school_detection_methods")
FISHERY_BAIT_SCHOOL_DETECTION_METHODS_CODES <- as.list(FISHERY_BAIT_SCHOOL_DETECTION_METHODS[, code])
FISHERY_BRANCHLINE_STORAGES <- fishery_domain("branchline_storages")
FISHERY_BRANCHLINE_STORAGES_CODES <- as.list(FISHERY_BRANCHLINE_STORAGES[, code])
FISHERY_BUOY_ACTIVITY_TYPES <- fishery_domain("buoy_activity_types")
FISHERY_BUOY_ACTIVITY_TYPES_CODES <- as.list(FISHERY_BUOY_ACTIVITY_TYPES[, code])
FISHERY_CARDINAL_POINTS <- fishery_domain("cardinal_points")
FISHERY_CARDINAL_POINTS_CODES <- as.list(FISHERY_CARDINAL_POINTS[, code])
FISHERY_CATCH_UNITS <- fishery_domain("catch_units")
FISHERY_CATCH_UNITS_CODES <- as.list(FISHERY_CATCH_UNITS[, code])
FISHERY_DEHOOKER_TYPES <- fishery_domain("dehooker_types")
FISHERY_DEHOOKER_TYPES_CODES <- as.list(FISHERY_DEHOOKER_TYPES[, code])
FISHERY_EFFORT_UNITS <- fishery_domain("effort_units")
FISHERY_EFFORT_UNITS_CODES <- as.list(FISHERY_EFFORT_UNITS[, code])
FISHERY_FAD_RAFT_DESIGNS <- fishery_domain("fad_raft_designs")
FISHERY_FAD_RAFT_DESIGNS_CODES <- as.list(FISHERY_FAD_RAFT_DESIGNS[, code])
FISHERY_FAD_TAIL_DESIGNS <- fishery_domain("fad_tail_designs")
FISHERY_FAD_TAIL_DESIGNS_CODES <- as.list(FISHERY_FAD_TAIL_DESIGNS[, code])
FISHERY_FISH_PRESERVATION_METHODS <- fishery_domain("fish_preservation_methods")
FISHERY_FISH_PRESERVATION_METHODS_CODES <- as.list(FISHERY_FISH_PRESERVATION_METHODS[, code])
FISHERY_FISH_PROCESSING_TYPES <- fishery_domain("fish_processing_types")
FISHERY_FISH_PROCESSING_TYPES_CODES <- as.list(FISHERY_FISH_PROCESSING_TYPES[, code])
FISHERY_FISH_STORAGE_TYPES <- fishery_domain("fish_storage_types")
FISHERY_FISH_STORAGE_TYPES_CODES <- as.list(FISHERY_FISH_STORAGE_TYPES[, code])
FISHERY_FISHERIES <- fishery_domain("fisheries")
FISHERY_FISHERIES_CODES <- as.list(FISHERY_FISHERIES[, code])
FISHERY_FLOAT_TYPES <- fishery_domain("float_types")
FISHERY_FLOAT_TYPES_CODES <- as.list(FISHERY_FLOAT_TYPES[, code])
FISHERY_FOB_ACTIVITY_TYPES <- fishery_domain("fob_activity_types")
FISHERY_FOB_ACTIVITY_TYPES_CODES <- as.list(FISHERY_FOB_ACTIVITY_TYPES[, code])
FISHERY_FOB_TYPES <- fishery_domain("fob_types")
FISHERY_FOB_TYPES_CODES <- as.list(FISHERY_FOB_TYPES[, code])
FISHERY_GEAR_TYPES <- fishery_domain("gear_types")
FISHERY_GEAR_TYPES_CODES <- as.list(FISHERY_GEAR_TYPES[, code])
FISHERY_GILLNET_MATERIAL_TYPES <- fishery_domain("gillnet_material_types")
FISHERY_GILLNET_MATERIAL_TYPES_CODES <- as.list(FISHERY_GILLNET_MATERIAL_TYPES[, code])
FISHERY_HOOK_TYPES <- fishery_domain("hook_types")
FISHERY_HOOK_TYPES_CODES <- as.list(FISHERY_HOOK_TYPES[, code])
FISHERY_HULL_MATERIAL_TYPES <- fishery_domain("hull_material_types")
FISHERY_HULL_MATERIAL_TYPES_CODES <- as.list(FISHERY_HULL_MATERIAL_TYPES[, code])
FISHERY_LIGHT_COLOURS <- fishery_domain("light_colours")
FISHERY_LIGHT_COLOURS_CODES <- as.list(FISHERY_LIGHT_COLOURS[, code])
FISHERY_LIGHT_TYPES <- fishery_domain("light_types")
FISHERY_LIGHT_TYPES_CODES <- as.list(FISHERY_LIGHT_TYPES[, code])
FISHERY_LINE_MATERIAL_TYPES <- fishery_domain("line_material_types")
FISHERY_LINE_MATERIAL_TYPES_CODES <- as.list(FISHERY_LINE_MATERIAL_TYPES[, code])
FISHERY_MECHANIZATION_TYPES <- fishery_domain("mechanization_types")
FISHERY_MECHANIZATION_TYPES_CODES <- as.list(FISHERY_MECHANIZATION_TYPES[, code])
FISHERY_MITIGATION_DEVICES <- fishery_domain("mitigation_devices")
FISHERY_MITIGATION_DEVICES_CODES <- as.list(FISHERY_MITIGATION_DEVICES[, code])
FISHERY_NET_COLOURS <- fishery_domain("net_colours")
FISHERY_NET_COLOURS_CODES <- as.list(FISHERY_NET_COLOURS[, code])
FISHERY_NET_CONDITIONS <- fishery_domain("net_conditions")
FISHERY_NET_CONDITIONS_CODES <- as.list(FISHERY_NET_CONDITIONS[, code])
FISHERY_NET_CONFIGURATIONS <- fishery_domain("net_configurations")
FISHERY_NET_CONFIGURATIONS_CODES <- as.list(FISHERY_NET_CONFIGURATIONS[, code])
FISHERY_NET_DEPLOY_DEPTHS <- fishery_domain("net_deploy_depths")
FISHERY_NET_DEPLOY_DEPTHS_CODES <- as.list(FISHERY_NET_DEPLOY_DEPTHS[, code])
FISHERY_NET_SETTING_STRATEGIES <- fishery_domain("net_setting_strategies")
FISHERY_NET_SETTING_STRATEGIES_CODES <- as.list(FISHERY_NET_SETTING_STRATEGIES[, code])
FISHERY_OFFAL_MANAGEMENT_TYPES <- fishery_domain("offal_management_types")
FISHERY_OFFAL_MANAGEMENT_TYPES_CODES <- as.list(FISHERY_OFFAL_MANAGEMENT_TYPES[, code])
FISHERY_POLE_MATERIAL_TYPES <- fishery_domain("pole_material_types")
FISHERY_POLE_MATERIAL_TYPES_CODES <- as.list(FISHERY_POLE_MATERIAL_TYPES[, code])
FISHERY_REASONS_DAYS_LOST <- fishery_domain("reasons_days_lost")
FISHERY_REASONS_DAYS_LOST_CODES <- as.list(FISHERY_REASONS_DAYS_LOST[, code])
FISHERY_SCHOOL_DETECTION_METHODS <- fishery_domain("school_detection_methods")
FISHERY_SCHOOL_DETECTION_METHODS_CODES <- as.list(FISHERY_SCHOOL_DETECTION_METHODS[, code])
FISHERY_SCHOOL_SIGHTING_CUES <- fishery_domain("school_sighting_cues")
FISHERY_SCHOOL_SIGHTING_CUES_CODES <- as.list(FISHERY_SCHOOL_SIGHTING_CUES[, code])
FISHERY_SCHOOL_TYPE_CATEGORIES <- fishery_domain("school_type_categories")
FISHERY_SCHOOL_TYPE_CATEGORIES_CODES <- as.list(FISHERY_SCHOOL_TYPE_CATEGORIES[, code])
FISHERY_SINKER_MATERIAL_TYPES <- fishery_domain("sinker_material_types")
FISHERY_SINKER_MATERIAL_TYPES_CODES <- as.list(FISHERY_SINKER_MATERIAL_TYPES[, code])
FISHERY_STREAMER_TYPES <- fishery_domain("streamer_types")
FISHERY_STREAMER_TYPES_CODES <- as.list(FISHERY_STREAMER_TYPES[, code])
FISHERY_STUNNING_METHODS <- fishery_domain("stunning_methods")
FISHERY_STUNNING_METHODS_CODES <- as.list(FISHERY_STUNNING_METHODS[, code])
FISHERY_SURFACE_FISHERY_ACTIVITIES <- fishery_domain("surface_fishery_activities")
FISHERY_SURFACE_FISHERY_ACTIVITIES_CODES <- as.list(FISHERY_SURFACE_FISHERY_ACTIVITIES[, code])
FISHERY_TRANSHIPMENT_CATEGORIES <- fishery_domain("transhipment_categories")
FISHERY_TRANSHIPMENT_CATEGORIES_CODES <- as.list(FISHERY_TRANSHIPMENT_CATEGORIES[, code])
FISHERY_VESSEL_ARCHITECTURES <- fishery_domain("vessel_architectures")
FISHERY_VESSEL_ARCHITECTURES_CODES <- as.list(FISHERY_VESSEL_ARCHITECTURES[, code])
FISHERY_VESSEL_SECTIONS <- fishery_domain("vessel_sections")
FISHERY_VESSEL_SECTIONS_CODES <- as.list(FISHERY_VESSEL_SECTIONS[, code])
FISHERY_VESSEL_SIZE_TYPES <- fishery_domain("vessel_size_types")
FISHERY_VESSEL_SIZE_TYPES_CODES <- as.list(FISHERY_VESSEL_SIZE_TYPES[, code])
FISHERY_VESSEL_TYPES <- fishery_domain("vessel_types")
FISHERY_VESSEL_TYPES_CODES <- as.list(FISHERY_VESSEL_TYPES[, code])
FISHERY_WASTE_CATEGORIES <- fishery_domain("waste_categories")
FISHERY_WASTE_CATEGORIES_CODES <- as.list(FISHERY_WASTE_CATEGORIES[, code])
FISHERY_WASTE_DISPOSAL_METHODS <- fishery_domain("waste_disposal_methods")
FISHERY_WASTE_DISPOSAL_METHODS_CODES <- as.list(FISHERY_WASTE_DISPOSAL_METHODS[, code])
FISHERY_WIND_SCALES <- fishery_domain("wind_scales")
FISHERY_WIND_SCALES_CODES <- as.list(FISHERY_WIND_SCALES[, code])

# FISHERY CONFIG REFERENCES ###
FISHERY_CONFIG_AREAS_OF_OPERATION <- fishery_config_domain("areas_of_operation")
FISHERY_CONFIG_AREAS_OF_OPERATION_CODES <- as.list(FISHERY_CONFIG_AREAS_OF_OPERATION[, code])
FISHERY_CONFIG_FISHERY_CATEGORIES <- fishery_config_domain("fishery_categories")
FISHERY_CONFIG_FISHERY_CATEGORIES_CODES <- as.list(FISHERY_CONFIG_FISHERY_CATEGORIES[, code])
FISHERY_CONFIG_FISHERY_TYPES <- fishery_config_domain("fishery_types")
FISHERY_CONFIG_FISHERY_TYPES_CODES <- as.list(FISHERY_CONFIG_FISHERY_TYPES[, code])
FISHERY_CONFIG_FISHERY_TYPES_BKP <- fishery_config_domain("fishery_types_bkp")
FISHERY_CONFIG_FISHERY_TYPES_BKP_CODES <- as.list(FISHERY_CONFIG_FISHERY_TYPES_BKP[, code])
FISHERY_CONFIG_FISHERY_TYPES_NEW <- fishery_config_domain("fishery_types_new")
FISHERY_CONFIG_FISHERY_TYPES_NEW_CODES <- as.list(FISHERY_CONFIG_FISHERY_TYPES_NEW[, code])
FISHERY_CONFIG_FISHING_MODES <- fishery_config_domain("fishing_modes")
FISHERY_CONFIG_FISHING_MODES_CODES <- as.list(FISHERY_CONFIG_FISHING_MODES[, code])
FISHERY_CONFIG_GEAR_CONFIGURATIONS <- fishery_config_domain("gear_configurations")
FISHERY_CONFIG_GEAR_CONFIGURATIONS_CODES <- as.list(FISHERY_CONFIG_GEAR_CONFIGURATIONS[, code])
FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_CONFIGURATION <- fishery_config_domain("gear_fishery_type_to_configuration")
# FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_CONFIGURATION_CODES <- as.list(FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_CONFIGURATION[, code])
FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_CONFIGURATION_BKP <- fishery_config_domain("gear_fishery_type_to_configuration_bkp")
# FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_CONFIGURATION_BKP_CODES <- as.list(FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_CONFIGURATION_BKP[, code])
FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_FISHING_MODE <- fishery_config_domain("gear_fishery_type_to_fishing_mode")
# FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_FISHING_MODE_CODES <- as.list(FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_FISHING_MODE[, code])
FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_FISHING_MODE_BKP <- fishery_config_domain("gear_fishery_type_to_fishing_mode_bkp")
# FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_FISHING_MODE_BKP_CODES <- as.list(FISHERY_CONFIG_GEAR_FISHERY_TYPE_TO_FISHING_MODE_BKP[, code])
FISHERY_CONFIG_GEAR_GROUPS <- fishery_config_domain("gear_groups")
FISHERY_CONFIG_GEAR_GROUPS_CODES <- as.list(FISHERY_CONFIG_GEAR_GROUPS[, code])
FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE <- fishery_config_domain("gear_to_fishery_type")
# FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_CODES <- as.list(FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE[, code])
FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_BKP <- fishery_config_domain("gear_to_fishery_type_bkp")
# FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_BKP_CODES <- as.list(FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_BKP[, code])
FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_NEW <- fishery_config_domain("gear_to_fishery_type_new")
# FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_NEW_CODES <- as.list(FISHERY_CONFIG_GEAR_TO_FISHERY_TYPE_NEW[, code])
FISHERY_CONFIG_GEAR_TO_TARGET_SPECIES <- fishery_config_domain("gear_to_target_species")
# FISHERY_CONFIG_GEAR_TO_TARGET_SPECIES_CODES <- as.list(FISHERY_CONFIG_GEAR_TO_TARGET_SPECIES[, code])
FISHERY_CONFIG_GEARS <- fishery_config_domain("gears")
FISHERY_CONFIG_GEARS_CODES <- as.list(FISHERY_CONFIG_GEARS[, code])
FISHERY_CONFIG_LOA_CLASSES <- fishery_config_domain("loa_classes")
FISHERY_CONFIG_LOA_CLASSES_CODES <- as.list(FISHERY_CONFIG_LOA_CLASSES[, code])
FISHERY_CONFIG_PURPOSES <- fishery_config_domain("purposes")
FISHERY_CONFIG_PURPOSES_CODES <- as.list(FISHERY_CONFIG_PURPOSES[, code])
FISHERY_CONFIG_TARGET_SPECIES <- fishery_config_domain("target_species")
FISHERY_CONFIG_TARGET_SPECIES_CODES <- as.list(FISHERY_CONFIG_TARGET_SPECIES[, code])


# BIOLOGICAL CONFIG REFERENCES ####

BIOLOGICAL_CONFIG_RECOMMENDED_MEASUREMENTS <- biological_config_domain("recommended_measurements")

# BIOLOGICAL REFERENCES ####

BIOLOGICAL_BAIT_CONDITIONS <- biological_domain("bait_conditions")
BIOLOGICAL_BAIT_CONDITIONS_CODES <- as.list(BIOLOGICAL_BAIT_CONDITIONS[, code])
BIOLOGICAL_BAIT_TYPES <- biological_domain("bait_types")
BIOLOGICAL_BAIT_TYPES_CODES <- as.list(BIOLOGICAL_BAIT_TYPES[, code])
BIOLOGICAL_DEPREDATION_SOURCES <- biological_domain("depredation_sources")
BIOLOGICAL_DEPREDATION_SOURCES_CODES <- as.list(BIOLOGICAL_DEPREDATION_SOURCES[, code])
BIOLOGICAL_FATES <- biological_domain("fates")
BIOLOGICAL_FATES_CODES <- as.list(BIOLOGICAL_FATES[, code])
BIOLOGICAL_GEAR_INTERACTIONS <- biological_domain("gear_interactions")
BIOLOGICAL_GEAR_INTERACTIONS_CODES <- as.list(BIOLOGICAL_GEAR_INTERACTIONS[, code])
BIOLOGICAL_HANDLING_METHODS <- biological_domain("handling_methods")
BIOLOGICAL_HANDLING_METHODS_CODES <- as.list(BIOLOGICAL_HANDLING_METHODS[, code])
BIOLOGICAL_INCIDENTAL_CAPTURES_CONDITIONS <- biological_domain("incidental_captures_conditions")
BIOLOGICAL_INCIDENTAL_CAPTURES_CONDITIONS_CODES <- as.list(BIOLOGICAL_INCIDENTAL_CAPTURES_CONDITIONS[, code])
BIOLOGICAL_INDIVIDUAL_CONDITIONS <- biological_domain("individual_conditions")
BIOLOGICAL_INDIVIDUAL_CONDITIONS_CODES <- as.list(BIOLOGICAL_INDIVIDUAL_CONDITIONS[, code])
BIOLOGICAL_MEASUREMENT_TOOLS <- biological_domain("measurement_tools")
BIOLOGICAL_MEASUREMENT_TOOLS_CODES <- as.list(BIOLOGICAL_MEASUREMENT_TOOLS[, code])
BIOLOGICAL_MEASUREMENTS <- biological_domain("measurements")
BIOLOGICAL_MEASUREMENTS_CODES <- as.list(BIOLOGICAL_MEASUREMENTS[, code])
BIOLOGICAL_SAMPLING_METHODS_FOR_CATCH_ESTIMATION <- biological_domain("sampling_methods_for_catch_estimation")
BIOLOGICAL_SAMPLING_METHODS_FOR_CATCH_ESTIMATION_CODES <- as.list(BIOLOGICAL_SAMPLING_METHODS_FOR_CATCH_ESTIMATION[, code])
BIOLOGICAL_SAMPLING_METHODS_FOR_SAMPLING_COLLECTIONS <- biological_domain("sampling_methods_for_sampling_collections")
BIOLOGICAL_SAMPLING_METHODS_FOR_SAMPLING_COLLECTIONS_CODES <- as.list(BIOLOGICAL_SAMPLING_METHODS_FOR_SAMPLING_COLLECTIONS[, code])
BIOLOGICAL_SAMPLING_PERIODS <- biological_domain("sampling_periods")
BIOLOGICAL_SAMPLING_PERIODS_CODES <- as.list(BIOLOGICAL_SAMPLING_PERIODS[, code])
BIOLOGICAL_SAMPLING_PROTOCOLS <- biological_domain("sampling_protocols")
BIOLOGICAL_SAMPLING_PROTOCOLS_CODES <- as.list(BIOLOGICAL_SAMPLING_PROTOCOLS[, code])
BIOLOGICAL_SCARS <- biological_domain("scars")
BIOLOGICAL_SCARS_CODES <- as.list(BIOLOGICAL_SCARS[, code])
BIOLOGICAL_SEX <- biological_domain("sex")
BIOLOGICAL_SEX_CODES <- as.list(BIOLOGICAL_SEX[, code])
BIOLOGICAL_SPECIES <- biological_domain("species")
BIOLOGICAL_SPECIES_CODES <- as.list(BIOLOGICAL_SPECIES[, code])
BIOLOGICAL_SPECIES_AGGREGATES <- biological_domain("species_aggregates")
BIOLOGICAL_SPECIES_AGGREGATES_CODES <- as.list(BIOLOGICAL_SPECIES_AGGREGATES[, species_aggregate_code])
BIOLOGICAL_SPECIES_CATEGORIES <- biological_domain("species_categories")
BIOLOGICAL_SPECIES_CATEGORIES_CODES <- as.list(BIOLOGICAL_SPECIES_CATEGORIES[, code])
BIOLOGICAL_SPECIES_GROUPS <- biological_domain("species_groups")
BIOLOGICAL_SPECIES_GROUPS_CODES <- as.list(BIOLOGICAL_SPECIES_GROUPS[, code])
BIOLOGICAL_TAG_TYPES <- biological_domain("tag_types")
BIOLOGICAL_TAG_TYPES_CODES <- as.list(BIOLOGICAL_TAG_TYPES[, code])
BIOLOGICAL_TYPES_OF_FATE <- biological_domain("types_of_fate")
BIOLOGICAL_TYPES_OF_FATE_CODES <- as.list(BIOLOGICAL_TYPES_OF_FATE[, code])
BIOLOGICAL_TYPES_OF_MEASUREMENT <- biological_domain("types_of_measurement")
BIOLOGICAL_TYPES_OF_MEASUREMENT_CODES <- as.list(BIOLOGICAL_TYPES_OF_MEASUREMENT[, code])

# SOCIO-ECONOMIC REFERENCES ####

SOCIO_ECONOMICS_CURRENCIES <- socio_economics_domain("CURRENCIES")
SOCIO_ECONOMICS_CURRENCIES_CODES <- as.list(SOCIO_ECONOMICS_CURRENCIES[, currency_code])
SOCIO_ECONOMICS_PRICING_LOCATIONS <- socio_economics_domain("PRICING_LOCATIONS")
SOCIO_ECONOMICS_PRICING_LOCATIONS_CODES <- as.list(SOCIO_ECONOMICS_PRICING_LOCATIONS[, code])
SOCIO_ECONOMICS_PRODUCT_TYPES <- socio_economics_domain("PRODUCT_TYPES")
SOCIO_ECONOMICS_PRODUCT_TYPES_CODES <- as.list(SOCIO_ECONOMICS_PRODUCT_TYPES[, code])
SOCIO_ECONOMICS_DESTINATION_MARKETS <- socio_economics_domain("DESTINATION_MARKETS")
SOCIO_ECONOMICS_DESTINATION_MARKETS_CODES <- as.list(SOCIO_ECONOMICS_DESTINATION_MARKETS[, code])


# OTHER TYPES OF DATA ####

## Extract the data from IOTC database ####
### ISSUE: IOTC_EAST missing from AREA_INTERSECTIONS and AREA_INTERSECTIONS_IOTC
EEZ_TO_IOTC_MAIN_AREAS = query(DB_IOTC_REFERENCE_DATA(), "
  SELECT DISTINCT
  	RIGHT(TARGET_CODE, 3) AS FLAG_CODE,
  	SOURCE_CODE AS MAIN_IOTC_AREA_CODE
  FROM refs_gis.AREA_INTERSECTIONS_IOTC
  WHERE SOURCE_CODE IN ('IOTC_EAST', 'IOTC_WEST')
  AND TARGET_CODE LIKE 'NJA_%'
  AND LENGTH(TARGET_CODE) = 7    -- to remove disputed NJAs
")