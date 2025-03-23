#######################
# AUTHOR: DR MOSES KITI
#######################

#--1. generate Kenya basemap

# ke_map <- map_data("world") %>%
#   # extract map of kenya
#   dplyr::filter(region %in% c("Kenya")) %>%
#   dplyr::mutate(color = factor(region, levels = c("Kenya")))

ke_map <- st_read("../data/gadm41_KEN_shp/gadm41_KEN_0.shp")
# ggplot() +
#   geom_sf(data = ke_map, fill = "#bdbdbd", color = "#bdbdbd",  size=0)

#-- 2. generate health facility data
hf_list <- c("Ganze Health Centre", "Jaribuni Dispensary", "Mgamboni Dispensary", 
             "Kinarani Dispensary", "Makanzani Dispensary", "Lenga Dispensary", 
             "Kadzinuni Dispensary", "Kiwandani Dispensary", "Pingilikani Dispensary",
             "Kilifi District Hospital", "Gede Health Centre", "Malindi District Hospital",
             "Tunzanani")

## get lat-long of health facilities
### sourced from https://springernature.figshare.com/articles/dataset/Public_health_facilities_in_sub_Saharan_Africa/7725374?backTo=/collections/A_spatial_database_of_health_facilities_managed_by_the_public_health_sector_in_sub_Saharan_Africa/4399445 
health_facilities <- readxl::read_excel("../data/SSA_health_facility_location_data.xlsx") %>%
  dplyr::filter(Admin1 == "Kilifi") %>%
  dplyr::rename(F_Name = "Facility name",
         F_Type = "Facility type",
         Latitude = Lat,
         Longitude = Long) %>%
  dplyr::filter(F_Name %in% hf_list) %>%
  dplyr::mutate(Province = "Coast",
         District = "Kilifi", 
         Division = NA, 
         Location = NA, 
         Sub_Location = NA) %>%
  dplyr::mutate(short_name = case_when(F_Name == hf_list[1] ~ "Ganze",
                                F_Name == hf_list[2] ~ "Jaribuni", 
                                F_Name == hf_list[3] ~ "Mgamboni",
                                F_Name == hf_list[4] ~ "Kinarani",
                                F_Name == hf_list[5] ~ "Makanzani",
                                F_Name == hf_list[6] ~ "Lenga",
                                F_Name == hf_list[7] ~ "Kadzinuni",
                                F_Name == hf_list[8] ~ "Kiwandani",
                                F_Name == hf_list[9] ~ "Pingilikani",
                                F_Name == hf_list[10] ~ "KCTRH", # Kilifi County Teaching & Referral Hospital
                                F_Name == hf_list[11] ~ "Gede",
                                F_Name == hf_list[12] ~ "Malindi")) %>%
  dplyr::select(-c("LL source", "Country", "Admin1"))


#-- 3. generate map of study sub-counties
study_sites <- c("Ganze", "Kaloleni", "Kilifi North", "Kilifi South", "Malindi",
                 "Magarini", "Rabai")
kilifi_subcounty <- st_read("../data/gadm41_KEN_shp/gadm41_KEN_2.shp") %>%
  dplyr::filter(NAME_2 %in% c(study_sites))


kilifi <- ggplot() +
  geom_sf(data = ke_map, fill = "white", color = "#bdbdbd",  size=0) +
  geom_sf(data = kilifi_subcounty, 
          fill = c("#636363")) + #c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')) +
  coord_sf(expand = FALSE) +
  geom_text(aes(x=38.4, y=-2.0, label = "Kilifi County")) +
  theme_void()
# kilifi

# crop map of Kilifi to show area around study sub-counties
kilifi_crop <- st_crop(ke_map, xmin = 39, xmax = 40.3,
                       ymin = -4, ymax = -2.25)
map_cols <- c('#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69')

kilifi_crop_plt <- ggplot() +
  geom_sf(data = kilifi_crop, 
          fill = "#bdbdbd", color = "#bdbdbd",  size=0) +
  geom_sf(data = kilifi_subcounty, 
          fill = map_cols) +
  scale_fill_manual(values = map_cols) +
  geom_point(data = health_facilities, 
             aes(x = Longitude, y = Latitude), 
             color = "red", shape = "+", size = 4, font="bold") +
  ggrepel::geom_text_repel(data = health_facilities, 
                           aes(x = Longitude, y = Latitude, 
                               label = short_name), 
                           size = 3, font = "bold",
                           box.padding = unit(0.2, "lines")) +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.05) +
  theme_bw()
# kilifi_crop_plt

# combine the maps with Kenya inset
study_locations <- ggdraw(kilifi_crop_plt) +
  draw_plot({kilifi},
            x=0.25, y=0.674,
            width = 0.3, height = 0.3)

ggsave("images/fig1_study_locations.png", plot = study_locations, bg = "transparent")

