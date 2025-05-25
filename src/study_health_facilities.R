#######################
# AUTHOR: DR MOSES KITI
#######################

pacman::p_load(
  dplyr,
  ggplot2,
  readxl)



subcounty_hf <- tibble(
  subcounty = c("Ganze", "Ganze", "Kaloleni", "Kaloleni",
                "Kilifi North", "Kilifi North", "Kilifi North", 
                "Kilifi South", "Kilifi South", "Rabai", "Rabai", "Malindi"),
  dispensary = c("Ganze Health Centre", "Jaribuni Dispensary", # Ganze
                 "Mgamboni Dispensary", "Kinarani Dispensary", # Kaloleni
                 "Kiwandani Dispensary", "Kadzinuni Dispensary", 
                 "Kilifi District Hospital",  # KN
                 "Pingilikani Dispensary", "Tunzanani Dispensary", # KS
                 "Makanzani Dispensary", "Lenga Dispensary", # Rabai
                 "Malindi District Hospital"), # Malindi
  short_f_name = c("Ganze", "Jaribuni", "Mgamboni", "Kinarani", "Kiwandani",
                   "Kadzinuni", "Kilifi", "Pingilikani", 
                   "Tunzanani", "Makanzani", "Lenga", "Malindi")) 


## get lat-long of health facilities
### sourced from https://springernature.figshare.com/articles/dataset/Public_health_facilities_in_sub_Saharan_Africa/7725374?backTo=/collections/A_spatial_database_of_health_facilities_managed_by_the_public_health_sector_in_sub_Saharan_Africa/4399445 

# Tunzanani, Baolala, Gongoni dispensary was not in the MOH list, so we create manually
additional_hf <- tibble(
  county = c("Kilifi", "Kilifi", "Kilifi"),
  dispensary = c("Tunzanani Dispensary", "Baolala Dispensary", "Gongoni Dispensary"),
  F_Type = c("Dispensary", "Dispensary", "Health Centre"),
  Ownership = c("MOH", "MOH", "MOH"),
  Latitude = c(-3.891365, -3.179836, -3.037937),
  Longitude = c(39.695260, 39.785560, 40.128563),
  subcounty = c("Kilifi South", "Malindi", "Malindi"),
  short_f_name = c("Tunzanani", "Baolala", "Gongoni")
)

health_facilities <- readxl::read_excel("../data/SSA_health_facility_location_data.xlsx") %>%
  dplyr::filter(Admin1 == "Kilifi") %>%
  dplyr::rename(county = "Admin1",
                dispensary = "Facility name",
                F_Type = "Facility type",
                Latitude = Lat,
                Longitude = Long) %>%
  dplyr::filter(dispensary %in% subcounty_hf$dispensary) %>%
  dplyr::select(-c("LL source", "Country")) %>%
  left_join(subcounty_hf, by = "dispensary") %>%
  rbind(additional_hf)