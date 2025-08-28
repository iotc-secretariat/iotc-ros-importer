# Abstract

This document summarizes some points to fix to perform import of the ROS Longline data in version 3.2.1

## Sheet O-INFO

* [x] ```OBSERVER_TRIP_DETAILS_EMBARKATION_IN_PORT_PORT_CODE```, this data should be located in table ```ros_common.locations```, but there is no such column here, except the ```NAME``` but not linked to the port code list | **Locations mixes ports and "at sea" - link port to code list with foreign key?**
* [x] ```OBSERVER_TRIP_DETAILS_DISEMBARKATION_IN_PORT_PORT_CODE```, this data should be located in table ```ros_common.locations```, but there is no such column here, except the ```NAME``` but not linked to the port code list **Locations mixes ports and "at sea" - link port to code list with foreign key?**
* [x] ```OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_1_CODE```, this data should be located in column ```ros_common.reasons_for_days_lost.reason```, but this column is not linked to the table ```refs_fishery.reasons_days_lost```, must add the foreign key here | **Yes, reasons_for_days_lost.reason seems to be free text??**
* [x] ```OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_2_CODE```, this data should be located in column ```ros_common.reasons_for_days_lost.reason```, but this column is not linked to the table ```refs_fishery.reasons_days_lost```, must add the foreign key here | **Yes, reasons_for_days_lost.reason seems to be free text??**
* [x] ```OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_3_CODE```, this data should be located in column ```ros_common.reasons_for_days_lost.reason```, but this column is not linked to the table ```refs_fishery.reasons_days_lost```, must add the foreign key here | **Yes, reasons_for_days_lost.reason seems to be free text??**
* [x] ```OBSERVED_TRIP_SUMMARY_REASONS_FOR_DAYS_LOST_REASON_4_CODE```, this data should be located in column ```ros_common.reasons_for_days_lost.reason```, but this column is not linked to the table ```refs_fishery.reasons_days_lost```, must add the foreign key here | **Yes, reasons_for_days_lost.reason seems to be free text??**
* [ ] ```VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING_CODE```, this data should be located in table ```ros_common.vessel_identification```, we have here a ```flag_code``` which is used for ```VESSEL_INFORMATION_IDENTIFICATION_REGISTRATION_COUNTRY_CODE```, need to add a new column then | **A code indicating chartering or not you mean?**

Nope, in the import files, in this column ```VESSEL_INFORMATION_IDENTIFICATION_FLAG_OR_CHARTERING_CODE``` we have the flag code or the chartering code but also have a ```VESSEL_INFORMATION_IDENTIFICATION_FLAG_CODE```, on db table we only have one column named ```flag_code```, should I add a ```flag_or_chartering_code```?

* [x] ```VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_1```, this data is located in table ```ros_common.texts``` which need to add unique constraint of the ```value``` as it was done for other such tables (measurement tables) | **Yes! pas besoin de répéter les valeurs identiques quoi - Ces infos ne sont plus à collecter depuis la MAJ - on garde pour archivage?**
* [x] ```VESSEL_INFORMATION_IDENTIFICATION_CONTACT_PHONE_2```, this data is located in table ```ros_common.texts``` which need to add unique constraint of the ```value``` as it was done for other such tables (measurement tables) | **Yes! pas besoin de répéter les valeurs identiques quoi - Ces infos ne sont plus à collecter depuis la MAJ - on garde pour archivage?**
* [x] ```VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_1```, this data is located in table ```ros_common.texts``` which need to add unique constraint of the ```value``` as it was done for other such tables (measurement tables) | **Yes! pas besoin de répéter les valeurs identiques quoi - Ces infos ne sont plus à collecter depuis la MAJ - on garde pour archivage?**
* [x] ```VESSEL_INFORMATION_IDENTIFICATION_CONTACT_FAX_2```, this data is located in table ```ros_common.texts``` which need to add unique constraint of the ```value``` as it was done for other such tables (measurement tables) | **Yes! pas besoin de répéter les valeurs identiques quoi - Ces infos ne sont plus à collecter depuis la MAJ - on garde pour archivage?**
* [x] ```VESSEL_INFORMATION_IDENTIFICATION_CONTACT_EMAIL_1```, this data is located in table ```ros_common.texts``` which need to add unique constraint of the ```value``` as it was done for other such tables (measurement tables) | **Yes! pas besoin de répéter les valeurs identiques quoi - Ces infos ne sont plus à collecter depuis la MAJ - on garde pour archivage?**
* [x] ```VESSEL_INFORMATION_OWNER_AND_PERSONNEL_CHARTER_OR_OPERATOR_CONTACT_DETAILS2```, we can only have one such data which is located in table ```ros_common.person_contact_details``` linked by table ```ros_common.vessel_owner_and_personnel.charter_or_operator_id```| **Ces infos ne sont plus à collecter depuis la MAJ - on garde pour archivage?**

IMHO we have them in the current ROS 3.2.1 database, we should not import them any longer in the ROS 3.3.0 database

## Sheet V-TRIP

* [x] ```TRIP_DETAILS_VESSEL_DEPARTURE_PORT_COUNTRY_CODE```, this data should be located in table ```ros_common.vessel_trip_details```, but we do not have a ```departure_port_country_code``` column, not sure that we have to add it, since port has a country | **Agreed, the country code can be inferred from port code through ```refs_admin.ports.country_code```**
* [x] ```TRIP_DETAILS_VESSEL_RETURN_PORT_COUNTRY_CODE```, this data should be located in table ```ros_common.vessel_trip_details```, but we do not have a ```return_port_country_code``` column, not sure that we have to add it, since port has a country | **Agreed, the country code can be inferred from ```refs_admin.ports.country_code```**
* [x] ```TRIP_DETAILS_VESSEL_DEPARTURE_PORT_LATITUDE```, this data should be located in table ```ros_common.vessel_trip_details```, but we do not have a ```departure_port_latitude``` column, not sure that we have to add it, since port has a position | **Agreed, the port latitude can be inferred from ```refs_admin.ports.lat```**
* [x] ```TRIP_DETAILS_VESSEL_DEPARTURE_PORT_LONGITUDE```, this data should be located in table ```ros_common.vessel_trip_details```, but we do not have a ```departure_port_longitude``` column, not sure that we have to add it, since port has a position | **Agreed, the longitude can be inferred from ```refs_admin.ports.lon```**
* [x] ```TRIP_DETAILS_VESSEL_RETURN_PORT_LATITUDE```, this data should be located in table ```ros_common.vessel_trip_details```, but we do not have a ```return_port_latitude``` column, not sure that we have to add it, since port has a position | **Agreed, the latitude can be inferred from ```refs_admin.ports.lat```**
* [x] ```TRIP_DETAILS_VESSEL_RETURN_PORT_LONGITUDE```, this data should be located in table ```ros_common.vessel_trip_details```, but we do not have a ```return_port_longitude``` column, not sure that we have to add it, since port has a position | **Agreed, the longitude can be inferred from port code through ```refs_admin.ports.lon```**

## Sheet V-ATTRIBUTES

* [x] ```VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_1```, this data should be located in table ```ros_common.waste_managements```, but we do not have a ```other``` column, need to add it? | **Not sure what is expected in the columns OTHER - free text - no idea what this is**
* [x] ```VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_2```, this data should be located in table ```ros_common.waste_managements```, but we do not have a ```other``` column, need to add it? | **Not sure what is expected in the columns OTHER - free text - no idea what this is**
* [x] ```VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_3```, this data should be located in table ```ros_common.waste_managements```, but we do not have a ```other``` column, need to add it? **Not sure what is expected in the columns OTHER - free text - no idea what this is**
* [x] ```VESSEL_ATTRIBUTES_WASTE_MANAGEMENT_WASTE_MANAGEMENT_DETAIL_1_OTHER_4```, this data should be located in table ```ros_common.waste_managements```, but we do not have a ```other``` column, need to add it? **Not sure what is expected in the columns OTHER - free text - no idea what this is**

## Sheet G-GENERAL

All is good

## Sheet G-CONFIG-BRANCHLINES

Alles gut

## Sheet E-SET

* [ ] ```SET_NUMBER```, by deduction this should be located in column ```ros_ll.fishing_events.event_number```, please validate this

## Sheet E-SET-LIGHTS

Tout va bien

## Sheet E-SET-BRANCHLINES

tou keksoz i byen

## Sheet E-SET-HOOKS

her şey yolunda

## Sheet E-SET-BAITS

alles is in orde

## Sheet E-MITIGATION-MEASURES

ཡོད་ཚད་བདེ་མོ་ཡིན།

## Sheet E-SET-HAULING

E mea maitai te mau mea atoa

## Sheet E-SET-HAULING-BITEOFFS

kaikki on hyvin

## Sheet E-SET-CATCHES

allt är bra

## Sheet E-SET-CATCHES-SPECIMEN

* [ ] ```SPECIMEN_DETAILS_ALL_SPECIES_DEPREDATION_DETAILS_DEPREDATION_SOURCE_CODE```, this data is linked to the code-list ```refs_biological.scars```, but there is also the code-list ```refs_biological.depredation_sources```, please check this is ok.
* [x] ```SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_1_IS_STRAIGHT```, this data is located to column ```ros_common.measured_lengths.curved``` which means the opposite, maybe we could improve this? (review in meeting)
* [x] ```SPECIMEN_DETAILS_ALL_SPECIES_LENGTH_2_IS_STRAIGHT```, this data is located to column ```ros_common.measured_lengths.curved``` which means the opposite, maybe we could improve this? (review in meeting)


## Sheet E-SET-CATCHES-SPECIMEN-SSI

* [ ] ```SPECIMEN_DETAILS_ADDITIONAL_CATCH_DETAILS_SSI_HOOK_TYPE_CODE```, this data could not be found in table ```ros_ll.additional_catch_details_on_ssi```, need to add it with a foreign key to ```refs_fishery.hook_types.code```

## Sheet T-EVENTS
           
It was decided to not import this sheet

## Sheet T-PRODUCTS
                                       
It was decided to not import this sheet
