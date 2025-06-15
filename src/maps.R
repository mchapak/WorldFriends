#######################
# AUTHOR: DR MOSES KITI
#######################

pacman::p_load(cowplot,
               ggplot2,
               leaflet,
               sf,
               ggrepel,
               ggspatial,
               dplyr)


source("src/study_health_facilities.R")
map_cols <- c('#8dd3c7', '#d8b365', '#ef8a62', '#bebada', '#bdbdbd', '#bdbdbd', '#b3de69')


#--1. generate Kenya basemap

# ke_map <- map_data("world") %>%
#   # extract map of kenya
#   dplyr::filter(region %in% c("Kenya")) %>%
#   dplyr::mutate(color = factor(region, levels = c("Kenya")))

ke_map <- st_read("../data/gadm41_KEN_shp/gadm41_KEN_0.shp") %>% st_transform(4326)
# ggplot() +
#   geom_sf(data = ke_map, fill = "#bdbdbd", color = "#bdbdbd",  size=0)


#-- 3. generate map of study sub-counties
kilifi_subcounty_list <- c("Ganze", "Kaloleni", "Kilifi North", "Kilifi South",
                 "Rabai", "Malindi", "Magarini") 
kilifi_subcounty <- st_read("../data/gadm41_KEN_shp/gadm41_KEN_2.shp") %>%
  dplyr::filter(NAME_2 %in% c(kilifi_subcounty_list)) %>% st_transform(4326)


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

# # Ganze, Kaloleni, Kilifi North, Kilifi South, Malindi, Magarini, Rabai
# map_cols <- c('#8dd3c7', '#d8b365', '#ef8a62', '#bebada', '#bdbdbd', '#bdbdbd', '#b3de69')

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
  theme_bw() +
  ggrepel::geom_text_repel(data = health_facilities, 
                           aes(x = Longitude, y = Latitude, 
                               label = short_f_name), 
                           size = 4, font = "bold",
                           box.padding = unit(0.2, "lines")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=18),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 25),
        title = element_text(size=25)) +
  coord_sf(expand = FALSE) +
  # ggspatial::annotation_north_arrow(
  #   location = "tr",
  #   style = north_arrow_orienteering(
  #     line_width = 0.5,
  #     line_col = text_hil,
  #     fill = c(bg_col, text_hil),
  #     text_family = "body_font",
  #     text_col = text_hil,
  #     text_size = 15)) +
  # ggspatial::annotation_scale(
  #   location = "bl",
  #   text_family = "body_font",
  #   text_col = text_hil,
  #   bar_cols = c(bg_col, text_hil),
  #   text_cex = 1)
  annotation_scale(location = "br", width_hint = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.02)
# kilifi_crop_plt

# combine the maps with Kenya inset
fig1_study_locations <- ggdraw(kilifi_crop_plt) +
  draw_plot({kilifi},
            x=0.21, y=0.648,
            width = 0.3, height = 0.3)
fig1_study_locations

ggsave("images/fig1_study_locations.png", plot = fig1_study_locations, bg = "transparent")



# generate maps of subcounties and hfs
plot_kilifi_subcounty_map <- function(kilifi_crop, kilifi_subcounty, 
                                      health_facilities, subcounty_name, 
                                      map_cols){
  
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
    filter(subcounty == subcounty_name) %>%
    # Format coordinates to 1 decimal place
    mutate(Longitude = round(Longitude, 1),
           Latitude = round(Latitude, 1))
  
  # Plot the filtered map
  subcounty_plot <- ggplot() +
    # geom_sf(data = kilifi_crop, fill = "#bdbdbd", color = "#bdbdbd", size = 0) +  # Background
    geom_sf(data = subcounty_data, fill = map_cols, color = "black") +  # Filtered subcounty
    geom_point(data = health_facilities_filtered, 
               aes(x = Longitude, y = Latitude), 
               color = "red", shape = "+", size = 6, font = "bold") +
    
    ggrepel::geom_text_repel(data = health_facilities_filtered, 
                             aes(x = Longitude, y = Latitude, label = short_f_name), 
                             size = 5, font = "bold",
                             box.padding = unit(0.2, "lines")) +
    coord_sf(expand = FALSE) +
    # annotation_scale(location = "bl", width_hint = 0.2) +
    # annotation_north_arrow(location = "br", which_north = "true", 
    #                        pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size = 0.03) +
    theme_bw() +
    labs(
      # title = paste("Health Facilities in", subcounty_name, "Subcounty"),
         # subtitle = "Kilifi County",
         x = "Longitude",
         y = "Latitude") +
    theme(axis.text.x = element_text(hjust = 1, size=15),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 20),
          # title = element_text(size=25)
          ) +
    scale_x_continuous(labels = function(x) sprintf("%.1f", x)) +  # 1 decimal for x
    scale_y_continuous(labels = function(y) sprintf("%.1f", y))  # 1 decimal for y
    # annotation_north_arrow(location = "tr", which_north = "true",
    #                        pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.01) +
    # annotation_scale(location = "br", width_hint = 0.1)
  
  return(subcounty_plot)
}

ganze_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                          "Ganze", map_cols[1]) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.01) +
  annotation_scale(location = "bl", width_hint = 0.2)


kaloleni_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                      "Kaloleni", map_cols[2])  +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.01) +
  annotation_scale(location = "bl", width_hint = 0.2)


kn_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                      "Kilifi North", map_cols[3])  +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.01) +
  annotation_scale(location = "br", width_hint = 0.2)


ks_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                   "Kilifi South", map_cols[4])  +
  annotation_north_arrow(location = "tr", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.01) +
  annotation_scale(location = "br", width_hint = 0.2)


rabai_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
                                   "Rabai", map_cols[7])  +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), size=0.01) +
  annotation_scale(location = "br", width_hint = 0.2)

# magarini_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
#                                    "Magarini", map_cols[5])
# 
# malindi_hf <- plot_kilifi_subcounty_map(kilifi_crop, kilifi_subcounty, health_facilities,
#                                    "Malindi", map_cols[6])




# Alternative code to generate leaflet interactive map
pal <- colorFactor(map_cols, domain = kilifi_subcounty$NAME_2)

# 3. Create base Leaflet map
leaflet_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    data = ke_map,
    fillColor = "white",
    color = "#bdbdbd",
    weight = 2,
    fillOpacity = 0.6
  ) %>%
  addPolygons(
    data = kilifi_subcounty,
    fillColor = ~pal(NAME_2),
    color = "black",
    weight = 1,
    fillOpacity = 0.8,
    label = ~NAME_2,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      bringToFront = TRUE
    ),
    group = "Subcounties"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = kilifi_subcounty$NAME_2,
    title = "Kilifi Subcounties"
  ) %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(toggleDisplay = TRUE)

# 4. Add health facilities if available
if(exists("health_facilities")) {
  leaflet_map <- leaflet_map %>%
    addMarkers(
      data = health_facilities,
      lng = ~Longitude,
      lat = ~Latitude,
      label = ~short_f_name,
      icon = makeIcon(
        iconUrl = "https://cdn-icons-png.flaticon.com/512/684/684908.png",
        iconWidth = 20,
        iconHeight = 20
      ),
      group = "Health Facilities"
    ) %>%
    addLayersControl(
      overlayGroups = c("Subcounties", "Health Facilities"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# 5. Create function for subcounty-specific maps
plot_kilifi_subcounty_leaflet <- function(subcounty_name) {
  subcounty_data <- kilifi_subcounty %>% filter(NAME_2 == subcounty_name)

  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = subcounty_data,
      fillColor = ~pal(NAME_2),
      color = "black",
      weight = 2,
      fillOpacity = 0.8,
      label = subcounty_name
    ) %>%
    addMarkers(
      data = health_facilities %>% filter(subcounty == subcounty_name),
      lng = ~Longitude,
      lat = ~Latitude,
      label = ~short_f_name,
      clusterOptions = markerClusterOptions()
    ) %>%
    addScaleBar(position = "bottomleft") %>%
    setView(
      lng = mean(st_bbox(subcounty_data)[c(1,3)]),
      lat = mean(st_bbox(subcounty_data)[c(2,4)]),
      zoom = 10
    )
}

# 6. Save the main map
htmlwidgets::saveWidget(leaflet_map, "kilifi_interactive_map.html")
# 
# # 7. Example usage for subcounty maps
# kaloleni_leaflet <- plot_kilifi_subcounty_leaflet("Kaloleni")

