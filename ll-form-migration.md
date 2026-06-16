# Abstract

This document describes migration from Form LL v3 to v4.

# META

## Notes

- add reporting year field
- add data source field
- **in documentation, missing comments field**

# O-INFO

## Renamed Columns

| Previous Name                                               | New Name                                               | Note |
|-------------------------------------------------------------|--------------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                      | `OBSERVER_TRIP_ID`                                     |      |
| `OBSERVER_IDENTIFICATION_OBSERVER_IOTC_NUMBER`              | `OBSERVER_IDENTIFICATION_OBSERVER_IOTC_ID`             |      |
| `OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_COUNTRY_CODE`    | `OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_COUNTRY`    |      |
| `OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_PORT_CODE`       | `OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_PORT`       |      |
| `OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_COUNTRY_CODE` | `OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_COUNTRY` |      |
| `OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_PORT_CODE`    | `OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_PORT`    |      |
| `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_1_CODE` | `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_1` |      |
| `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_2_CODE` | `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_2` |      |
| `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_3_CODE` | `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_3` |      |
| `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_4_CODE` | `OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_4` |      |

## Removed Columns

| Column                                     | Note |
|--------------------------------------------|------|
| `OBSERVER_IDENTIFICATION_NATIONALITY_CODE` |      |

## Mandatory State Changes

| Column                                                  | Change               | Note |
|---------------------------------------------------------|----------------------|------|
| `OBSERVER_IDENTIFICATION_FULL_NAME`                     | optional → mandatory |      |
| `OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_SEARCHING`        | mandatory → optional |      |
| `OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_ACTIVELY_FISHING` | mandatory → optional |      |
| `OBSERVED_TRIP_SUMMARY_NUMBER_OF_DAYS_LOST`             | mandatory → optional |      |

# V-INFO

## Renamed Columns

| Previous Name                                                             | New Name                                                             | Note |
|---------------------------------------------------------------------------|----------------------------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                                    | `OBSERVER_TRIP_ID`                                                   |      |
| `VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING_CODE`               | `VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING`               |      |
| `VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_COUNTRY_CODE`             | `VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_COUNTRY`             |      |
| `VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_PORT_CODE`                | `VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_PORT`                |      |
| `VESSEL_INFORMATION_IDENTIFICATION_MAIN_FISHING_GEAR_CODE`                | `VESSEL_INFORMATION_IDENTIFICATION_MAIN_FISHING_GEAR`                |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_NATIONALITY_CODE`  | `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_NATIONALITY`  |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_NATIONALITY_CODE` | `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_NATIONALITY` |      |

## Added Columns

| Column                                                      | Origin     | Note                          |
|-------------------------------------------------------------|------------|-------------------------------|
| `VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES` | New column | is this a multi-values field? |

## Removed Columns

| Column                                                                        | Note |
|-------------------------------------------------------------------------------|------|
| `VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_1`                           |      |
| `VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_2`                           |      |
| `VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_1`                             |      |
| `VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_2`                             |      |
| `VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_1`                           |      |
| `VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_2`                           |      |
| `VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_1_CODE`    |      |
| `VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_2_CODE`    |      |
| `VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_3_CODE`    |      |
| `VESSEL_INFORMATION_IDENTIFICATION_LICENSED_TARGET_SPECIES_SPECIES_4_CODE`    |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_FULL_NAME`                      |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_NATIONALITY_CODE`               |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_OWNER_CONTACT_DETAILS`                |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_FULL_NAME`        |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_NATIONALITY_CODE` |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS`  |      |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS2` |      |

## Mandatory State Changes

| Column                                                               | Change               | Note                                           |
|----------------------------------------------------------------------|----------------------|------------------------------------------------|
| `VESSEL_INFORMATION_IDENTIFICATION_NAME`                             | optional → mandatory |                                                |
| `VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING`               | optional → mandatory | only on my model, was already correct on forms |
| `VESSEL_INFORMATION_IDENTIFICATION_MAIN_FISHING_GEAR`                | optional → mandatory | only on my model, was already correct on forms |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_FULL_NAME`    | optional → mandatory |                                                |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_FISHING_MASTER_NATIONALITY`  | optional → mandatory |                                                |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_FULL_NAME`   | optional → mandatory |                                                |
| `VESSEL_INFORMATION_OWNER_AND_PERSONNEL_SKIPPER_CAPTAIN_NATIONALITY` | optional → mandatory |                                                |

# V-TRIP

## Renamed Columns

| Previous Name                                     | New Name                                     | Note |
|---------------------------------------------------|----------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                            | `OBSERVER_TRIP_ID`                           |      |
| `TRIP_DETAILS_VESSEL_DEPARTURE_PORT_COUNTRY_CODE` | `TRIP_DETAILS_VESSEL_DEPARTURE_PORT_COUNTRY` |      |
| `TRIP_DETAILS_VESSEL_DEPARTURE_PORT_PORT_CODE`    | `TRIP_DETAILS_VESSEL_DEPARTURE_PORT_PORT`    |      |
| `TRIP_DETAILS_VESSEL_RETURN_PORT_COUNTRY_CODE`    | `TRIP_DETAILS_VESSEL_RETURN_PORT_COUNTRY`    |      |
| `TRIP_DETAILS_VESSEL_RETURN_PORT_PORT_CODE`       | `TRIP_DETAILS_VESSEL_RETURN_PORT_PORT`       |      |

# V-ATTRIBUTES

## Renamed Columns

| Previous Name                                                                                 | New Name                                                                                 | Note             |
|-----------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------|------------------|
| `OBSERVER_TRIP_NUMBER`                                                                        | `OBSERVER_TRIP_ID`                                                                       |                  |
| `VESSEL_ATTRIBUTES_TONNAGE_VALUE`                                                             | `VESSEL_ATTRIBUTES_GROSS_TONNAGE_GT`                                                     | bad name in form |
| `VESSEL_ATTRIBUTES_LENGTH_OVERALL_VALUE`                                                      | `VESSEL_ATTRIBUTES_LENGTH_OVERALL_M`                                                     |                  |
| `VESSEL_ATTRIBUTES_HULL_MATERIAL_CODE`                                                        | `VESSEL_ATTRIBUTES_HULL_MATERIAL`                                                        |                  |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_VALUE`                                               | `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_M3`                                             |                  |
| `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_1_CODE`                                   | `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_1`                                   |                  |
| `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_2_CODE`                                   | `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_2`                                   |                  |
| `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_3_CODE`                                   | `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_3`                                   |                  |
| `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_4_CODE`                                   | `VESSEL_ATTRIBUTES_FISH_PRESERVATION_METHODS_METHOD_4`                                   |                  |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_1_CODE`                                            | `VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_1`                                            |                  |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_2_CODE`                                            | `VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_2`                                            |                  |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_3_CODE`                                            | `VESSEL_ATTRIBUTES_FISH_STORAGE_TYPES_TYPE_3`                                            |                  |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_VALUE`                                               | `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY`                                                |                  |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_GPS`                                                    | `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_GNSS`                                              |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_CATEGORY_1_CODE`                | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_CATEGORY_1`                |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_STORAGE_DISPOSAL_METHOD_1_CODE` | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_STORAGE_DISPOSAL_METHOD_1` |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_CATEGORY_2_CODE`                | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_CATEGORY_2`                |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_STORAGE_DISPOSAL_METHOD_2_CODE` | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_STORAGE_DISPOSAL_METHOD_2` |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_CATEGORY_3_CODE`                | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_CATEGORY_3`                |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_STORAGE_DISPOSAL_METHOD_3_CODE` | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_STORAGE_DISPOSAL_METHOD_3` |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_CATEGORY_4_CODE`                | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_CATEGORY_4`                |                  |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_STORAGE_DISPOSAL_METHOD_4_CODE` | `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_STORAGE_DISPOSAL_METHOD_4` |                  |

## Removed Columns

| Column                                                                 | Note                                                                                                                                                          |
|------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `VESSEL_ATTRIBUTES_TONNAGE_GT_GRT`                                     | all values of `VESSEL_ATTRIBUTES_GROSS_TONNAGE_GT` are now in GT unit, should we remove the unit field in database and convert all values in GT unit?         |
| `VESSEL_ATTRIBUTES_LENGTH_OVERALL_M_FT`                                | all values of `VESSEL_ATTRIBUTES_LENGTH_OVERALL_M` are now in m unit, should we remove the unit field in database and convert all values in m unit?           |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_T_M3`                         | all values of `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_M3` are now in m3 unit, should we remove the unit field in database and convert all values in m3 unit? |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SST_GAUGE`                       |                                                                                                                                                               |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_WEATHER_FAX`                     |                                                                                                                                                               |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_1` |                                                                                                                                                               |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_2_OTHER_2` |                                                                                                                                                               |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_3_OTHER_3` |                                                                                                                                                               |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_4_OTHER_4` |                                                                                                                                                               |

## Mandatory State Changes

| Column                                                                                   | Change               | Note |
|------------------------------------------------------------------------------------------|----------------------|------|
| `VESSEL_ATTRIBUTES_MAIN_ENGINE_1_MAKE`                                                   | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_MAIN_ENGINE_1_POWER_VALUE`                                            | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_MAIN_ENGINE_1_KW_HP_BHP`                                              | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY`                                                | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_GNSS`                                              | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VMS`                                               | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_AIS`                                               | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_RADARS`                                            | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_TRACK_PLOTTER`                                     | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DEPTH_SOUNDER`                                     | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SONAR`                                             | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DOPPLER_CURRENT_METER`                             | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_XBT`                                               | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_CATEGORY_1`                | mandatory → optional |      |
| `VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_STORAGE_DISPOSAL_METHOD_1` | mandatory → optional |      |

## Columns changed to use `refs_data.logical_responses`

| Column                                                       |
|--------------------------------------------------------------|
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_GNSS`                  |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VMS`                   |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_AIS`                   |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_RADARS`                |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_TRACK_PLOTTER`         |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DEPTH_SOUNDER`         |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SONAR`                 |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_DOPPLER_CURRENT_METER` |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_XBT`                   |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_VHF_RADIOS`            |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_HF_RADIOS`             |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SATELLITE_COMM`        |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_SST_GAUGE`             |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_WEATHER_FAX`           |
| `VESSEL_ATTRIBUTES_VESSEL_ELECTRONICS_FIS`                   |

# G-GENERAL

## Renamed Columns

| Previous Name          | New Name           | Note |
|------------------------|--------------------|------|
| `OBSERVER_TRIP_NUMBER` | `OBSERVER_TRIP_ID` |      |

## Removed Columns

| Column                                                 | Note                                                                |
|--------------------------------------------------------|---------------------------------------------------------------------|
| `GENERAL_GEAR_ATTRIBUTES_MAINLINE_MATERIAL_CODE`       |                                                                     |
| `GENERAL_GEAR_ATTRIBUTES_MAINLINE_LENGTH_VALUE`        |                                                                     |
| `GENERAL_GEAR_ATTRIBUTES_MAINLINE_LENGTH_KM_NM`        |                                                                     |
| `GENERAL_GEAR_ATTRIBUTES_MAINLINE_DIAMETER_VALUE`      |                                                                     |
| `GENERAL_GEAR_ATTRIBUTES_MAINLINE_DIAMETER_MM_CM`      |                                                                     |
| `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_CM_M_FT`   | TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_VALUE is now in unit m   |
| `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_CM_M_FT`   | TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_VALUE is now in unit m   |
| `TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_CM_M_FT` | TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_VALUE is now in unit m |
| `TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_CM_M_FT`  | TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_VALUE is now in unit m  |

## Moved Columns

| Column                                               | Destination Sheet             | Note |
|------------------------------------------------------|-------------------------------|------|
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_1_CODE`       | `G-CONFIG-BRANCHLINES`        |      |
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_2_CODE`       | `G-CONFIG-BRANCHLINES`        |      |
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_3_CODE`       | `G-CONFIG-BRANCHLINES`        |      |
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_4_CODE`       | `G-CONFIG-BRANCHLINES`        |      |
| `TORI_LINE_DETAILS_TORI_LINE_LENGTH_VALUE`           | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_TORI_LINE_LENGTH_M_FT`            | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_STREAMER_TYPE_CODE`               | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_STREAMER_REACH_SURFACE`           | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_VALUE`   | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_VALUE`   | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_STREAMER_NUMBER_PER_LINE`         | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_VALUE` | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_VALUE`  | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_TOWED_OBJECTS_NUMBER`             | `G-CONFIG-MITIGATION-DEVICES` |      |
| `TORI_LINE_DETAILS_TOWED_OBJECTS_TYPE`               | `G-CONFIG-MITIGATION-DEVICES` |      |
| `MITIGATION_DEVICES_DEVICE_1_CODE`                   | `G-CONFIG-MITIGATION-DEVICES` |      |
| `MITIGATION_DEVICES_DEVICE_2_CODE`                   | `G-CONFIG-MITIGATION-DEVICES` |      |
| `MITIGATION_DEVICES_DEVICE_3_CODE`                   | `G-CONFIG-MITIGATION-DEVICES` |      |
| `MITIGATION_DEVICES_DEVICE_4_CODE`                   | `G-CONFIG-MITIGATION-DEVICES` |      |


## Columns Using `refs_data.logical_responses`

| Column                                                |
|-------------------------------------------------------|
| `SPECIAL_EQUIPMENT_OR_MACHINERY_LINE_SETTER`          |
| `SPECIAL_EQUIPMENT_OR_MACHINERY_LINE_HAULER`          |
| `SPECIAL_EQUIPMENT_OR_MACHINERY_BAIT_CASTING_MACHINE` |

# G-CONFIG-BRANCELINES

## Renamed Columns

| Previous Name                                    | New Name                                     | Note |
|--------------------------------------------------|----------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                           | `OBSERVER_TRIP_ID`                           |      |
| `BRANCHLINE_CONFIGURATIONS_CONFIGURATION_NUMBER` | `BRANCHLINE_CONFIGURATIONS_CONFIGURATION_ID` |      |
| `BRANCHLINE_CONFIGURATIONS_SECTION_ID`           | `BRANCHLINE_CONFIGURATIONS_SECTION_ID`       |      |
| `BRANCHLINE_CONFIGURATIONS_MATERIAL_TYPE_CODE`   | `BRANCHLINE_CONFIGURATIONS_MATERIAL_TYPE`    |      |
| `BRANCHLINE_CONFIGURATIONS_LENGTH_VALUE`         | `BRANCHLINE_CONFIGURATIONS_LENGTH_M`         |      |
| `BRANCHLINE_CONFIGURATIONS_DIAMETER_VALUE`       | `BRANCHLINE_CONFIGURATIONS_DIAMETER_CM`      |      |

## Added Columns

| Column                                    | Origin                                                       | Note |
|-------------------------------------------|--------------------------------------------------------------|------|
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_1` | `G-GENERAL` → `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_1_CODE` |      |
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_2` | `G-GENERAL` → `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_2_CODE` |      |
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_3` | `G-GENERAL` → `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_3_CODE` |      |
| `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_4` | `G-GENERAL` → `ADDITIONAL_BRANCHLINE_DETAILS_STORAGE_4_CODE` |      |

## Removed Columns

| Column                                     | Note                                                                                                                                                      |
|--------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `BRANCHLINE_CONFIGURATIONS_LENGTH_CM_M`    | all values of BRANCHLINE_CONFIGURATIONS_LENGTH_VALUE are now in m unit, should we remove the unit field in database and convert all values in m unit?     |
| `BRANCHLINE_CONFIGURATIONS_DIAMETER_MM_CM` | all values of BRANCHLINE_CONFIGURATIONS_DIAMETER_VALUE are now in cm unit, should we remove the unit field in database and convert all values in cm unit? |

## Mandatory State Changes

| Column                                       | Change               | Note |
|----------------------------------------------|----------------------|------|
| `BRANCHLINE_CONFIGURATIONS_CONFIGURATION_ID` | mandatory → optional |      |
| `BRANCHLINE_CONFIGURATIONS_SECTION_ID`       | mandatory → optional |      |

# G-CONFIG-MITIGATION-DEVICES

New sheet

| Column                                           | Origin                                                             | Note             |
|--------------------------------------------------|--------------------------------------------------------------------|------------------|
| `OBSERVED_TRIP_ID`                               | `G-GENERAL` → `OBSERVED_TRIP_NUMBER`                               |                  |
| `TORI_LINE_DETAILS_TORI_LINE_LENGTH_M`           | `G-GENERAL` → `TORI_LINE_DETAILS_TORI_LINE_LENGTH_VALUE`           | bad name in form |
| `TORI_LINE_DETAILS_STREAMER_TYPE`                | `G-GENERAL` → `TORI_LINE_DETAILS_STREAMER_TYPE_CODE`               |                  |
| `TORI_LINE_DETAILS_STREAMER_REACH_SURFACE`       | `G-GENERAL` → `TORI_LINE_DETAILS_STREAMER_REACH_SURFACE`           |                  |
| `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_M`   | `G-GENERAL` → `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_VALUE`   |                  |
| `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_M`   | `G-GENERAL` → `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_VALUE`   |                  |
| `TORI_LINE_DETAILS_STREAMER_NUMBER_PER_LINE`     | `G-GENERAL` → `TORI_LINE_DETAILS_STREAMER_NUMBER_PER_LINE`         |                  |
| `TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_M` | `G-GENERAL` → `TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_VALUE` |                  |
| `TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_M`  | `G-GENERAL` → `TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_VALUE`  |                  |
| `TORI_LINE_DETAILS_TOWED_OBJECTS_NUMBER`         | `G-GENERAL` → `TORI_LINE_DETAILS_TOWED_OBJECTS_NUMBER`             |                  |
| `TORI_LINE_DETAILS_TOWED_OBJECTS_TYPE`           | `G-GENERAL` → `TORI_LINE_DETAILS_TOWED_OBJECTS_TYPE`               |                  |
| `MITIGATION_DEVICES_DEVICE_1`                    | `G-GENERAL` → `MITIGATION_DEVICES_DEVICE_1_CODE`                   |                  |
| `MITIGATION_DEVICES_DEVICE_2`                    | `G-GENERAL` → `MITIGATION_DEVICES_DEVICE_2_CODE`                   |                  |
| `MITIGATION_DEVICES_DEVICE_3`                    | `G-GENERAL` → `MITIGATION_DEVICES_DEVICE_3_CODE`                   |                  |
| `MITIGATION_DEVICES_DEVICE_4`                    | `G-GENERAL` → `MITIGATION_DEVICES_DEVICE_4_CODE`                   |                  |
| `MITIGATION_DEVICES_DEVICE_5`                    | `G-GENERAL` → `MITIGATION_DEVICES_DEVICE_5_CODE`                   |                  |

# E-SET

## Renamed Columns

| Previous Name                                      | New Name                                         | Note |
|----------------------------------------------------|--------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                             | `OBSERVER_TRIP_ID`                               |      |
| `SET_NUMBER`                                       | `SET_ID`                                         |      |
| `SETTING_OPERATIONS_NUM_TOTAL_HOOKS_SET`           | `SETTING_OPERATIONS_NUMBER_TOTAL_HOOKS_SET`      |      |
| `SETTING_OPERATIONS_NUM_TOTAL_FLOATS_SET`          | `SETTING_OPERATIONS_NUMBER_TOTAL_FLOATS_SET`     |      |
| `SETTING_OPERATIONS_NUM_HOOKS_BETWEEN_FLOATS`      | `SETTING_OPERATIONS_NUMBER_HOOKS_BETWEEN_FLOATS` |      |
| `SETTING_OPERATIONS_SHARK_LINES_NUM_LINES`         | `SETTING_OPERATIONS_SHARK_LINES_NUMBER`          |      |
| `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_1_CODE` | `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_1`    |      |
| `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_2_CODE` | `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_2`    |      |
| `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_3_CODE` | `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_3`    |      |
| `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_4_CODE` | `SETTING_OPERATIONS_TARGET_SPECIES_SPECIES_4`    |      |

## Added Columns

| Column                                                               | Origin     | Note                                      |
|----------------------------------------------------------------------|------------|-------------------------------------------|
| `SETTING_OPERATIONS_MAINLINE_MATERIAL`                               | New column | codelist refs_fishery.line_material_types |
| `SETTING_OPERATIONS_LEADER_MATERIAL_TYPE`                            | New column | code list link not in form                |
| `SETTING_OPERATIONS_LEADER_MATERIAL_PERCENTAGE`                      | New column |                                           |
| `SETTING_OPERATIONS_LEADER_MATERIAL_BRANCHLINE_LENGTH_MINIMUM_VALUE` | New column |                                           |
| `SETTING_OPERATIONS_LEADER_MATERIAL_BRANCHLINE_LENGTH_MINIMUM_UNIT`  | New column | unit not in form                          |
| `SETTING_OPERATIONS_LEADER_MATERIAL_BRANCHLINE_LENGTH_MAXIMUM_VALUE` | New column |                                           |
| `SETTING_OPERATIONS_LEADER_MATERIAL_BRANCHLINE_LENGTH_MAXIMUM_UNIT`  | New column | unit not in form                          |

## Removed Columns

| Column                      | Note |
|-----------------------------|------|
| `SETTING_OPERATIONS_VMS_ON` |      |

## Columns Using `refs_data.logical_responses`

| Column                               |
|--------------------------------------|
| `SETTING_OPERATIONS_SHARK_LINES_SET` |

# E-SET-LIGHTS

## Renamed Columns

| Previous Name                                   | New Name                                           | Note |
|-------------------------------------------------|----------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                          | `OBSERVER_TRIP_ID`                                 |      |
| `SET_NUMBER`                                    | `SET_ID`                                           |      |
| `SETTING_OPERATIONS_ATTACHED_LIGHTS_TYPE_CODE`  | `SETTING_OPERATIONS_ATTACHED_LIGHTS_TYPE`          |      |
| `SETTING_OPERATIONS_ATTACHED_LIGHTS_COLOR_CODE` | `SETTING_OPERATIONS_ATTACHED_LIGHTS_COLOR`         |      |
| `SETTING_OPERATIONS_ATTACHED_LIGHTS_NUM_LIGHTS` | `SETTING_OPERATIONS_ATTACHED_LIGHTS_NUMBER_LIGHTS` |      |

# E-SET-BRANCHLINES

## Sheet Notes

- Sheet removed.

# E-SET-MITIGATION-MEASURES

## Renamed Columns

| Previous Name                                                                          | New Name                                                                          | Note |
|----------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                                                 | `OBSERVER_TRIP_ID`                                                                |      |
| `SET_NUMBER`                                                                           | `SET_ID`                                                                          |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_NUM_TORI_LINES_DEPLOYED`                       | `SETTING_OPERATIONS_MITIGATION_MEASURES_NUMBER_TORI_LINES_DEPLOYED`               |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_AVG_BRANCHLINE_WEIGHT_G`                       | `SETTING_OPERATIONS_MITIGATION_MEASURES_AVERAGE_SINKER_WEIGHT_G`                  |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_PERC_BRANCHLINE_WEIGHTED`                      | `SETTING_OPERATIONS_MITIGATION_MEASURES_PERCENT_BRANCHLINE_WEIGHTED`              |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_UNDERWATER_SETTING`                            | `SETTING_OPERATIONS_MITIGATION_MEASURES_HOOK_PODS`                                |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_UNDERWATER_SETTING`                            | `SETTING_OPERATIONS_MITIGATION_MEASURES_HOOK_PODS`                                |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_1_CODE` | `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_1` |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_2_CODE` | `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_2` |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_3_CODE` | `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_3` |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_4_CODE` | `SETTING_OPERATIONS_MITIGATION_MEASURES_OTHER_MITIGATION_MEASURES_USED_MEASURE_4` |      |

## Added Columns

| Column                                                                           | Origin                                                                             | Note |
|----------------------------------------------------------------------------------|------------------------------------------------------------------------------------|------|
| `SETTING_OPERATIONS_MITIGATION_MEASURES_BRANCHLINE_DETAILS_CONFIGURATION_NUMBER` | `E-SET-BRANCHLINES` → `SETTING_OPERATIONS_BRANCHLINE_DETAILS_CONFIGURATION_NUMBER` |      |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_BRANCHLINE_DETAILS_NUM_BRANCHLINES_SET`  | `E-SET-BRANCHLINES` → `SETTING_OPERATIONS_BRANCHLINE_DETAILS_NUM_BRANCHLINES_SET`  |      |

## Removed Columns

| Column                                                               | Note |
|----------------------------------------------------------------------|------|
| `SETTING_OPERATIONS_MITIGATION_MEASURES_HOOKS_SET_BETWEEN_DUSK_DAWN` |      |

## Mandatory State Changes

| Column                                                       | Change               | Note |
|--------------------------------------------------------------|----------------------|------|
| `SETTING_OPERATIONS_MITIGATION_MEASURES_BRANCHLINE_WEIGHTED` | mandatory → optional |      |

## Columns Using `refs_data.logical_responses`

| Column                                                          |
|-----------------------------------------------------------------|
| `SETTING_OPERATIONS_MITIGATION_MEASURES_MIN_DECK_LIGHTING_USED` |
| `SETTING_OPERATIONS_MITIGATION_MEASURES_BRANCHLINE_WEIGHTED`    |

# E-SET-HOOKS

## Renamed Columns

| Previous Name                                | New Name                                | Note |
|----------------------------------------------|-----------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                       | `OBSERVER_TRIP_ID`                      |      |
| `SET_NUMBER`                                 | `SET_ID`                                |      |
| `SETTING_OPERATIONS_HOOKS_DETAILS_TYPE_CODE` | `SETTING_OPERATIONS_HOOKS_DETAILS_TYPE` |      |

# E-SET-BAITS

## Renamed Columns

| Previous Name          | New Name           | Note |
|------------------------|--------------------|------|
| `OBSERVER_TRIP_NUMBER` | `OBSERVER_TRIP_ID` |      |
| `SET_NUMBER`           | `SET_ID`           |      |

# E-SET-HAULING

## Renamed Columns

| Previous Name                                                    | New Name                                                       | Note |
|------------------------------------------------------------------|----------------------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                           | `OBSERVER_TRIP_ID`                                             |      |
| `SET_NUMBER`                                                     | `SET_ID`                                                       |      |
| `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_1_CODE` | `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_1`    |      |
| `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_2_CODE` | `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_2`    |      |
| `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_3_CODE` | `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_3`    |      |
| `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_4_CODE` | `HAULING_OPERATIONS_POSITIONS_OF_OFFAL_DISPOSAL_POSITION_4`    |      |
| `HAULING_OPERATIONS_NUM_HOOKS_RETRIEVED_DURING_OBSERVATION`      | `HAULING_OPERATIONS_NUMBER_HOOKS_RETRIEVED_DURING_OBSERVATION` |      |
| `HAULING_OPERATIONS_SAMPLING_PROTOCOL_CODE`                      | `HAULING_OPERATIONS_SAMPLING_PROTOCOL`                         |      |

## Added Columns

| Column                                                   | Origin     | Note |
|----------------------------------------------------------|------------|------|
| `HAULING_OPERATIONS_NUMBER_BRANCHLINE_HAULINGS_OBSERVED` | New column |      |

## Removed Columns

| Column                                                  | Note |
|---------------------------------------------------------|------|
| `HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_1_CODE` |      |
| `HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_2_CODE` |      |
| `HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_3_CODE` |      |
| `HAULING_OPERATIONS_METHODS_TO_STUN_FISH_METHOD_4_CODE` |      |

## Mandatory State Changes

| Column                                             | Change               | Note |
|----------------------------------------------------|----------------------|------|
| `HAULING_OPERATIONS_BIRD_SCARING_DEVICE_AT_HAULER` | optional → mandatory |      |

## Columns Using `refs_data.logical_responses`

| Column                                             |
|----------------------------------------------------|
| `HAULING_OPERATIONS_BIRD_SCARING_DEVICE_AT_HAULER` |

# E-SET-HAULING-BITEOFFS

## Renamed Columns

| Previous Name                                                         | New Name                                                    | Note |
|-----------------------------------------------------------------------|-------------------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                                | `OBSERVER_TRIP_ID`                                          |      |
| `SET_NUMBER`                                                          | `SET_ID`                                                    |      |
| `SETTING_OPERATIONS_BAITS_DETAILS_BAIT_CONDITION_CODE`                | `SETTING_OPERATIONS_BAITS_DETAILS_BAIT_CONDITION`           |      |
| `SETTING_OPERATIONS_BAITS_DETAILS_BAIT_SPECIES_CODE`                  | `SETTING_OPERATIONS_BAITS_DETAILS_BAIT_SPECIES`             |      |
| `HAULING_OPERATIONS_BITEOFFS_DETAILS_BRANCHLINE_CONFIGURATION_NUMBER` | `HAULING_OPERATIONS_BITEOFFS_BRANCHLINE_CONFIGURATION_ID`   |      |
| `HAULING_OPERATIONS_BITEOFFS_DETAILS_BRANCHLINE_NUM_BITEOFFS`         | `HAULING_OPERATIONS_BITEOFFS_BRANCHLINE_NUMBER_OF_BITEOFFS` |      |

# E-SET-CATCHES

## Renamed Columns

| Previous Name                                 | New Name                         | Note                              |
|-----------------------------------------------|----------------------------------|-----------------------------------|
| `OBSERVER_TRIP_NUMBER`                        | `OBSERVER_TRIP_ID`               |                                   |
| `SET_NUMBER`                                  | `SET_ID`                         |                                   |
| `CATCH_NUMBER`                                | `CATCH_ID`                       |                                   |
| `CATCH_DETAILS_SPECIES_CODE`                  | `CATCH_SPECIES`                  |                                   |
| `CATCH_DETAILS_FATE_TYPE_CODE`                | `CATCH_FATE_TYPE`                |                                   |
| `CATCH_DETAILS_FATE_CODE`                     | `CATCH_FATE_CODE`                |                                   |
| `CATCH_DETAILS_SAMPLING_METHOD_CODE`          | `CATCH_SAMPLING_METHOD`          |                                   |
| `CATCH_DETAILS_NUM_FISH`                      | `CATCH_NUMBER_OF_FISH`           |                                   |
| `CATCH_DETAILS_WEIGHT_PROCESSING_TYPE_CODE`   | `CATCH_WEIGHT_PROCESSING_TYPE`   |                                   |
| `CATCH_DETAILS_WEIGHT_VALUE`                  | `CATCH_WEIGHT_KG`                | keep in database the unit column? |
| `CATCH_DETAILS_WEIGHT_ESTIMATION_METHOD_CODE` | `CATCH_WEIGHT_ESTIMATION_METHOD` |                                   |

## Removed Columns

| Column                      | Note |
|-----------------------------|------|
| `CATCH_DETAILS_WEIGHT_KG_T` |      |

# E-SET-CATCH-SPECIMENS (renamed from sheet E-SET-CATCHES-SPECIMEN)

## Renamed Columns

| Previous Name                                                              | New Name                                          | Note |
|----------------------------------------------------------------------------|---------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                                     | `OBSERVER_TRIP_ID`                                |      |
| `SET_NUMBER`                                                               | `SET_ID`                                          |      |
| `CATCH_NUMBER`                                                             | `CATCH_ID`                                        |      |
| `SPECIMEN_NUMBER`                                                          | `SPECIMEN_ID`                                     |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_SAMPLING_PERIOD_CODE`                        | `SPECIMEN_SAMPLING_PERIOD`                        |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_SAMPLING_METHOD_CODE`                        | `SPECIMEN_SAMPLING_METHOD`                        |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_TYPE_CODE`                          | `SPECIMEN_LENGTH_1_TYPE`                          |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_VALUE_CM`                           | `SPECIMEN_LENGTH_1_VALUE_CM`                      |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_MEASURING_TOOL_CODE`                | `SPECIMEN_LENGTH_1_MEASURING_TOOL`                |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_IS_STRAIGHT`                        | `SPECIMEN_LENGTH_1_IS_STRAIGHT`                   |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_TYPE_CODE`                          | `SPECIMEN_LENGTH_2_TYPE`                          |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_VALUE_CM`                           | `SPECIMEN_LENGTH_2_VALUE_CM`                      |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_MEASURING_TOOL_CODE`                | `SPECIMEN_LENGTH_2_MEASURING_TOOL`                |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_IS_STRAIGHT`                        | `SPECIMEN_LENGTH_2_IS_STRAIGHT`                   |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_PROCESSING_TYPE_CODE`                 | `SPECIMEN_WEIGHT_PROCESSING_TYPE`                 |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_VALUE_KG`                             | `SPECIMEN_WEIGHT_VALUE_KG`                        |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_WEIGHT_ESTIMATION_METHOD_CODE`               | `SPECIMEN_WEIGHT_ESTIMATION_METHOD`               |      |
| `CATCH_DETAILS_FATE_TYPE_CODESPECIMEN_DETAILS_ALL_SPECIES_SEX_CODE`        | `SPECIMEN_SPECIES_SEX`                            |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_MATURITY_SCALE`                              | `SPECIMEN_MATURITY_SCALE`                         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_MATURITY_STAGE`                              | `SPECIMEN_MATURITY_STAGE`                         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_SAMPLE_COLLECTED_TYPE_CODE`                  | `SPECIMEN_SAMPLE_COLLECTED_TYPE`                  |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_SAMPLE_COLLECTED_PRESERVATION_METHOD_CODE`   | `SPECIMEN_SAMPLE_COLLECTED_PRESERVATION_METHOD`   |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_DEPREDATION_SOURCE_CODE` | `SPECIMEN_DEPREDATION_DETAILS_DEPREDATION_SOURCE` |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_OBSERVED_PREDATOR_CODE`  | `SPECIMEN_DEPREDATION_DETAILS_OBSERVED_PREDATOR`  |      |

## Added Columns

| Column                                  | Origin     | Note |
|-----------------------------------------|------------|------|
| `SPECIMEN_SAMPLE_COLLECTED_DESTINATION` | New column |      |

## Moved Columns

| Column                                                                             | Destination Sheet                 | Note |
|------------------------------------------------------------------------------------|-----------------------------------|------|
| `SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_CAPTURE_CODE` | `sheet E-SET-CATCH-SPECIMENS-SSI` |      |
| `SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_RELEASE_CODE` | `sheet E-SET-CATCH-SPECIMENS-SSI` |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RELEASE`                                 | `sheet E-SET-TAG-DETAILS`         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RECOVERY`                                | `sheet E-SET-TAG-DETAILS`         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TYPE_CODE`                               | `sheet E-SET-TAG-DETAILS`         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_1`                            | `sheet E-SET-TAG-DETAILS`         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_2`                            | `sheet E-SET-TAG-DETAILS`         |      |
| `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_FINDER_NAME_AND_CONTACT_DETAILS`         | `sheet E-SET-TAG-DETAILS`         |      |

## Columns Using `refs_data.logical_responses`

| Column                          |
|---------------------------------|
| `SPECIMEN_LENGTH_1_IS_STRAIGHT` |
| `SPECIMEN_LENGTH_2_IS_STRAIGHT` |

# E-SET-CATCH-SPECIMENS-SSI (renamed from sheet E-SET-CATCH-SPECIMEN-SSI)

## Renamed Columns

| Previous Name                                                         | New Name                                                   | Note |
|-----------------------------------------------------------------------|------------------------------------------------------------|------|
| `OBSERVER_TRIP_NUMBER`                                                | `OBSERVER_TRIP_ID`                                         |      |
| `SET_NUMBER`                                                          | `SET_ID`                                                   |      |
| `CATCH_NUMBER`                                                        | `CATCH_ID`                                                 |      |
| `SPECIMEN_NUMBER`                                                     | `SPECIMEN_ID`                                              |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_GEAR_INTERACTION_CODE` | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_GEAR_INTERACTION`   |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HOOK_TYPE_CODE`        | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_HOOK_TYPE`          |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_CONDITION_CODE`   | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_CONDITION`     |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_TYPE_CODE`        | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_BAIT_TYPE`          |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_MATERIAL_CODE`  | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_MATERIAL`    |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_DIAMETER_MM`    | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_DIAMETER_MM` |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_DE_HOOKER_DEVICE_CODE` | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_DE_HOOKER_DEVICE`   |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_LIGHT_ATTACHED`        | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_LIGHT_ATTACHED`     |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_BROUGHT_ONBOARD`       | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_BROUGHT_ONBOARD`    |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HANDLING_METHOD_CODE`  | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_HANDLING_METHOD`    |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_REVIVAL`               | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_REVIVAL`            |      |
| `SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_PHOTO_ID`              | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_PHOTO_ID`           |      |

## Added Columns

| Column                                             | Origin                                                                                                       | Note |
|----------------------------------------------------|--------------------------------------------------------------------------------------------------------------|------|
| `SPECIMEN_NON_TARGET_SPECIES_CONDITION_AT_CAPTURE` | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_CAPTURE_CODE` |      |
| `SPECIMEN_NON_TARGET_SPECIES_CONDITION_AT_RELEASE` | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_NON_TARGET_SPECIES_ADDITIONAL_DETAILS_CONDITION_AT_RELEASE_CODE` |      |

## Mandatory State Changes

| Column                                             | Change               | Note |
|----------------------------------------------------|----------------------|------|
| `SPECIMEN_NON_TARGET_SPECIES_CONDITION_AT_CAPTURE` | optional → mandatory |      |
| `SPECIMEN_NON_TARGET_SPECIES_CONDITION_AT_RELEASE` | optional → mandatory |      |

## Columns Using `refs_data.logical_responses`

| Column                                                  |
|---------------------------------------------------------|
| `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_LIGHT_ATTACHED`  |
| `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_BROUGHT_ONBOARD` |
| `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_REVIVAL`         |

# E-SET-TAG-DETAILS
New sheet
| Column                                        | Source                                                                                               | Note |
|-----------------------------------------------|------------------------------------------------------------------------------------------------------|------|
| `OBSERVER_TRIP_ID`                            | New column                                                                                           |      |
| `SET_ID`                                      | New column                                                                                           |      |
| `CATCH_ID`                                    | New column                                                                                           |      |
| `SPECIMEN_ID`                                 | New column                                                                                           |      |
| `SPECIMEN_TAG_DETAILS_RELEASE`                | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RELEASE`                         |      |
| `SPECIMEN_TAG_DETAILS_RECOVERY`               | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_RECOVERY`                        |      |
| `SPECIMEN_TAG_DETAILS_TYPE`                   | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TYPE_CODE`                       |      |
| `SPECIMEN_TAG_DETAILS_TAG_NUMBER_1`           | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_1`                    |      |
| `SPECIMEN_TAG_DETAILS_TAG_NUMBER_2`           | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_TAG_NUMBER_2`                    |      |
| `SPECIMEN_TAG_DETAILS_FINDER_NAME`            | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_FINDER_NAME_AND_CONTACT_DETAILS` |      |
| `SPECIMEN_TAG_DETAILS_FINDER_CONTACT_DETAILS` | `E-SET-CATCH-SPECIMENS` → `SPECIMEN_DETAILS_ALL_SPECIES_TAG_DETAILS_FINDER_NAME_AND_CONTACT_DETAILS` |      |

## Columns Using `refs_data.logical_responses`

| Column                          |
|---------------------------------|
| `SPECIMEN_TAG_DETAILS_RELEASE`  |
| `SPECIMEN_TAG_DETAILS_RECOVERY` |

# Unit management

We propose to remove any unit from the database. Here is the list of units appearances in the database (only for the LL model):

| sheet                         | column                                                              | Unit        | database location                                                           | values in database | check in database         | Note   |
|-------------------------------|---------------------------------------------------------------------|-------------|-----------------------------------------------------------------------------|--------------------|---------------------------|--------|
| `V-ATTRIBUTES`                | `VESSEL_ATTRIBUTES_GROSS_TONNAGE_GT`                                | `gt`        | `ros_common.trip_vessel.tonnage_unit`                                       | `grt,gt`           | ok                        | See 1. |
| `V-ATTRIBUTES`                | `VESSEL_ATTRIBUTES_LENGTH_OVERALL_M`                                | `m`         | `ros_common.trip_vessel.loa_unit`                                           | `m`                | Missing check in database |        |
| `V-ATTRIBUTES`                | `VESSEL_ATTRIBUTES_MAIN_ENGINE_1_POWER_VALUE`                       | `kW,hp,bhp` | `ros_common.trip_vessel_main_engines.main_engines_unit`                     | `hp,bhp`           | 2 checks in database...   |        |
| `V-ATTRIBUTES`                | `VESSEL_ATTRIBUTES_MAIN_ENGINE_2_POWER_VALUE`                       | `kW,hp,bhp` | `ros_common.trip_vessel_main_engines.main_engines_unit`                     | `hp,bhp`           | 2 checks in database...   |        |
| `V-ATTRIBUTES`                | `VESSEL_ATTRIBUTES_AUTONOMY_RANGE_DAYS_NM`                          | `DAYS,NM`   | `ros_common.trip_vessel.autonomy_range_unit`                                | `unk,null`         | Missing check in database | See 3. |
| `V-ATTRIBUTES`                | `VESSEL_ATTRIBUTES_FISH_STORAGE_CAPACITY_M3`                        | `m3`        | `ros_common.trip_vessel.fish_storage_capacity_unit`                         | `m3,mt`            | ok                        |        |
| `G-CONFIG-BRANCHLINES`        | `BRANCHLINE_CONFIGURATIONS_LENGTH_M`                                | `km,m`      | `ros_ll.branchline_sections.length_unit`                                    | `m`                | 2 checks in database...   |        |
| `G-CONFIG-BRANCHLINES`        | `BRANCHLINE_CONFIGURATIONS_DIAMETER_CM`                             | `cm`        | `ros_ll.branchline_sections.diameter_unit`                                  | `mm`               | 2 checks in database...   |        |
| `G-CONFIG-MITIGATION-DEVICES` | `TORI_LINE_DETAILS_TORI_LINE_LENGTH_M`                              | `m`         | `ros_ll.tori_line_details.tori_line_length_unit`                            | `m,null`           | 2 checks in database...   |        |
| `G-CONFIG-MITIGATION-DEVICES` | `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MAX_M`                      | `m`         | `ros_ll.tori_line_details.streamer_line_length_max_unit`                    | `null`             | 2 checks in database...   |        |
| `G-CONFIG-MITIGATION-DEVICES` | `TORI_LINE_DETAILS_STREAMER_LINE_LENGTH_MIN_M`                      | `m`         | `ros_ll.tori_line_details.streamer_line_length_min_unit`                    | `null`             | 2 checks in database...   |        |
| `G-CONFIG-MITIGATION-DEVICES` | `TORI_LINE_DETAILS_DISTANCE_BETWEEN_STREAMERS_VALUE_M`              | `m`         | `ros_ll.tori_line_details.streamer_distance_unit`                           | `null`             | Missing check in database |        |
| `G-CONFIG-MITIGATION-DEVICES` | `TORI_LINE_DETAILS_TORI_LINE_ATTACHED_HEIGHT_VALUE_M`               | `m`         | `ros_ll.tori_line_details.attached_height_unit`                             | `null`             | Missing check in database |        |
| `E-SET`                       | `SETTING_OPERATIONS_SPEED_VESSEL_VALUE_KNOTS`                       | `kn`        | `ros_ll.setting_operations.vessel_speed_unit`                               | `null,kn`          | 2 checks in database...   |        |
| `E-SET`                       | `SETTING_OPERATIONS_SPEED_LINE_SETTER_VALUE_MS`                     | `ms`        | `ros_ll.setting_operations.line_setter_speed_unit`                          | `null`             | 2 checks in database...   |        |
| `E-SET`                       | `SETTING_OPERATIONS_MAINLINE_LENGTH_KM_NM`                          | `km,m`      | `ros_ll.setting_operations.mainline_set_length_unit`                        | `null,km,m`        | 2 checks in database...   |        |
| `E-SET`                       | `SETTING_OPERATIONS_LEADER_MATERIAL_BRANCHLINE_LENGTH_MINIMUM_UNIT` | `km,m`      | `ros_ll.setting_operations.leader_set.total_branchline_minimum_length_unit` | ``                 | 2 checks in database...   |        |
| `E-SET`                       | `SETTING_OPERATIONS_LEADER_MATERIAL_BRANCHLINE_LENGTH_MAXIMUM_UNIT` | `km,m`      | `ros_ll.setting_operations.leader_set.total_branchline_maximum_length_unit` | ``                 | 2 checks in database...   |        |
| `E-SET-MITIGATION-MEASURES`   | `SETTING_OPERATIONS_MITIGATION_MEASURES_AVERAGE_SINKER_WEIGHT_G`    | `g`         | `ros_ll.mitigation_measures.average_sinker_weight_unit`                     | `null`             | Missing check in database |        |
| `E-SET-MITIGATION-MEASURES`   | `SETTING_OPERATIONS_MITIGATION_MEASURES_HOOK_SINKER_DISTANCE_CM`    | `cm`        | `ros_ll.mitigation_measures.hook_sinker_distance_unit`                      | `null`             | Missing check in database |        |
| `E-SET-CATCHES`               | `CATCH_WEIGHT_KG`                                                   | `kg`        | `ros_ll.catch_details`                                                      | `null,t`           | 2 checks in database...   |        |
| `E-SET-CATCH-SPECIMENS`       | `SPECIMEN_LENGTH_1_VALUE_CM`                                        | `cm`        | `ros_common.biometric_information.measured_length_unit`                     | `cm,null`          | 2 checks in database...   |        |
| `E-SET-CATCH-SPECIMENS`       | `SPECIMEN_LENGTH_2_VALUE_CM`                                        | `cm`        | `ros_common.biometric_information.alternative_measured_length_unit`         | `cm,null`          | 2 checks in database...   |        |
| `E-SET-CATCH-SPECIMENS`       | `SPECIMEN_WEIGHT_VALUE_KG`                                          | `kg`        | `ros_common.biometric_information.estimated_weight_unit`                    | `kg,null`          | 2 checks in database...   |        |
| `E-SET-CATCH-SPECIMENS-SSI`   | `SPECIMEN_ADDITIONAL_CATCH_DETAILS_SSI_LEADER_DIAMETER_MM`          | `mm`        | `ros_ll.additional_catch_details_on_ssi`                                    | `null`             | Missing check in database |        |

## Notes

1. can't convert gt and grt (grt is the old system and gt the modern one (since 1969 Tonnage convention))
2. could convert mt to m3 (with approximation: 1 mt fish ≈ 1.05 m³), but not exact conversion
3. can't convert autonomy range (days and nautical miles), but since we have no value in database we could use the nm unit
