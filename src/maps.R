#######################
# AUTHOR: DR MOSES KITI
#######################

pacman::p_load(cowplot,
               ggplot2,
               sf,
               ggrepel,
               ggspatial,
               dplyr)

#--1. generate Kenya basemap
ke_map <- st_read("../data/gadm41_KEN_shp/gadm41_KEN_0.shp")
# ggplot() +
#   geom_sf(data = ke_map, fill = "#bdbdbd", color = "#bdbdbd",  size=0)


#--2 generate list of health facilities in the study
source("src/study_health_facilities.R")


#-- 3. generate map of study sub-counties
kilifi_subcounty_list <- c("Ganze", "Kaloleni", "Kilifi North", "Kilifi South",
                 "Rabai", "Malindi", "Magarini") 
kilifi_subcounty <- st_read("../data/gadm41_KEN_shp/gadm41_KEN_2.shp") %>%
  dplyr::filter(NAME_2 %in% c(kilifi_subcounty_list))


kilifi <- ggplot() +
  geom_sf(data = ke_map, fill = "white", color = "#bdbdbd",  size=2) +
  geom_sf(data = kilifi_subcounty, 
          fill = c("#636363")) + #c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')) +
  coord_sf(expand = FALSE) +
  geom_text(aes(x=38.4, y=-2.0, label = "Kilifi County")) +
  theme_void()
# kilifi

# crop map of Kilifi to show area around study sub-counties
kilifi_crop <- st_crop(ke_map, xmin = 39, xmax = 40.3,
                       ymin = -4, ymax = -2.25)

map_cols <- c('#8dd3c7', '#d8b365', '#ef8a62', '#bebada', '#bdbdbd', '#bdbdbd', '#b3de69')

# Magarini and Malindi mapped as gray 


kilifi_crop_plt <- ggplot() +
  geom_sf(data = kilifi_crop, 
          fill = "#bdbdbd", color = "#bdbdbd", alpha = 0.6,  size=0) +
  geom_sf(data = kilifi_subcounty, 
          fill = map_cols) +
  scale_fill_manual(values = map_cols) +
  geom_point(data = health_facilities, 
             aes(x = Longitude, y = Latitude), 
             color = "red", shape = "+", size = 6, font="bold") +
  ggrepel::geom_text_repel(data = health_facilities, 
                           aes(x = Longitude, y = Latitude, 
                               label = short_f_name), 
                           size = 4, font = "bold",
                           box.padding = unit(0.2, "lines")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=25),
        axis.text.y = element_text(size = 25),
        axis.title = element_text(size = 30),
        title = element_text(size=25)) +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.02) +
  theme_bw()
kilifi_crop_plt

# combine the maps with Kenya inset
fig1_study_locations <- ggdraw(kilifi_crop_plt) +
  draw_plot({kilifi},
            x=0.13, y=0.635,
            width = 0.3, height = 0.3)
fig1_study_locations

ggsave("images/fig1_study_locations.png", plot = fig1_study_locations, bg = "transparent")



# generate maps of subcounties and hfs
plot_kilifi_subcounty_map <- function(kilifi_crop, kilifi_subcounty, 
                                      health_facilities, subcounty_name, 
                                      map_cols) {
  
  # Ensure the input subcounty dataset is an sf object
  if (!inherits(kilifi_subcounty, "sf")) {
    stop("The subcounty dataset must be an 'sf' object.")
  }
  
  # Filter for the selected subcounty
  subcounty_data <- kilifi_subcounty %>%
    filter(NAME_2 == subcounty_name)
  
  # Check if data exists
  if (nrow(subcounty_data) == 0) {
    stop(paste("No data found for subcounty:", subcounty_name))
  }
  
  # Filter health facilities within the selected subcounty boundary

  health_facilities_filtered <- health_facilities %>%
    filter(subcounty == subcounty_name)
    # st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(kilifi_subcounty)) %>%
    # st_intersection(subcounty_data)
  
  # Plot the filtered map
  subcounty_plot <- ggplot() +
    # geom_sf(data = kilifi_crop, fill = "#bdbdbd", color = "#bdbdbd", size = 0) +  # Background
    geom_sf(data = subcounty_data, fill = map_cols, color = "black") +  # Filtered subcounty
    geom_point(data = health_facilities_filtered, 
               aes(x = Longitude, y = Latitude), 
               color = "red", shape = "+", size = 6, font = "bold") +
    ggrepel::geom_text_repel(data = health_facilities_filtered, 
                             aes(x = Longitude, y = Latitude, label = short_f_name), 
                             size = 4, font = "bold",
                             box.padding = unit(0.2, "lines")) +
    coord_sf(expand = FALSE) +
    # annotation_scale(location = "bl", width_hint = 0.2) +
    # annotation_north_arrow(location = "br", which_north = "true", 
    #                        pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size = 0.03) +
    theme_bw() +
    labs(title = paste("Health Facilities in", subcounty_name, "Subcounty"),
         subtitle = "Kilifi County",
         x = "Longitude",
         y = "Latitude")
  
  return(subcounty_plot)
}

ganze_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                          "Ganze", map_cols[1])

kaloleni_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                      "Kaloleni", map_cols[2])

kn_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                      "Kilifi North", map_cols[3])

ks_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                   "Kilifi South", map_cols[6])

rabai_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                   "Rabai", map_cols[7])

# magarini_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
#                                    "Magarini", map_cols[5])
# 
# malindi_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
#                                    "Malindi", map_cols[6])


