#######################
# AUTHOR: DR MOSES KITI
#######################
pacman::p_load(
  dplyr,
  ggplot2,
  lubridate,
  readxl,
  tibble,
  tidyr)

source("src/functions.R")

# generate names of health facilities in the sub-counties
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

# create functions to transform data
# a. write functions to reshape data into correct format
reshape_moh705 <- function(data) {
  data |> 
    pivot_longer(
      cols = -c(dispensary, data, subcounty),
      names_to = "date",
      values_to = "count"
    ) |> 
    pivot_wider(
      names_from = data,
      values_from = count
    ) |>
    rename_all(tolower)  |>
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
           month = month(date),
           year = substr(year(date), 3, 4), # extract last 2 digits of year value
           my = paste0(month,"-",year))
}

# b. define report month levels
report_month <- c("9-23","10-23","11-23","12-23","1-24","2-24","3-24","4-24",
                  "5-24","6-24","7-24","8-24","9-24","10-24","11-24","12-24",
                  "1-25","2-25","3-25","4-25","5-25")

col_case <- c("#bdbdbd", "#7570b3", "#d95f02") #  "#542788"

# create function to plot graphs
plot_malaria_cases <- function(data, title_text, file_name) {
  plot <- ggplot(data, aes(x = my, y = count, 
                           group = case_type, 
                           color = case_type, 
                           fill = case_type)) + 
    
    # Bar plot for suspected cases
    geom_col(data = data %>% filter(case_type == "Suspected"), 
             aes(fill = case_type), 
             width = 1, alpha = 0.4) +  # Transparency to differentiate bars
    
    # Line plot for tested and confirmed cases
    geom_line(data = data %>% filter(case_type != "Suspected"), 
              aes(color = case_type), 
              size = 1.2) + 
    
    # Points for tested and confirmed cases
    geom_point(data = data %>% filter(case_type != "Suspected"), 
               aes(color = case_type), 
               size = 2) + 
    
    # Labels and theme
    labs(title = "", # paste("Monthly", title_text, "malaria cases"),
         x = "Month-Year of reporting",
         y = "Number of Cases",
         color = "Case Type",
         fill = "Case Type") + 
    
    # Ensure both color and fill use the same scale
    scale_color_manual(values = c(col_case[1], col_case[2], col_case[3])) +  
    scale_fill_manual(values = c(col_case[1], col_case[2], col_case[3])) +  
    
    facet_wrap(~ dispensary, ncol = 2) +  
    theme_minimal() +
    
    # Improve readability of the plot
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 20),
          title = element_text(size = 20),
          strip.text = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20, face = "bold"))
  
  return(plot)
}


# Save the figure
save_plot <- function(plot, file_name){  
  ggsave(plot, filename = file_name, 
         height = 6, width = 15, dpi = 300, bg = "#FFFFFF")
}


#===============================================================================
# STEP 1: UNDER 5 MALARIA SUSPECTED, TESTED, CONFIRMED
#===============================================================================
#
# Uses data from MOH705A
moh705A <- readxl::read_excel("../data/WF/clean/khis_dispensary_data.xlsx", 
                      sheet = "MOH705A", skip=1) %>%
  rename_all(tolower) %>%
  dplyr::mutate(dispensary = case_when(dispensary == "Ganze H/C" ~ "Ganze",
                                TRUE ~ dispensary)) %>%
  left_join(subcounty_hf %>% select(-dispensary), 
            by = c("dispensary" = "short_f_name"))


### convert moh705A from wide to long format..
moh705A <- reshape_moh705(moh705A) %>%
  # check that confirmed < tested
  dplyr::mutate(diff = tested-confirmed)

# check and confirm these values
moh705A_check <- moh705A |>
  dplyr::filter(diff < 0)

# meanwhile, we filter out erroneous values above and generate positivity rate
moh705A <- moh705A |>
  dplyr::mutate(pos_rate = (confirmed/tested)*1000) # rate/1000 tested

moh705A$my <- factor(moh705A$my,
                       levels = report_month)

# convert to long format
moh705A_long <- moh705A |>
  tidyr::pivot_longer(cols = suspected:tested, 
                      names_to = "case_type", 
                      values_to = "count")
moh705A_long$case_type <- factor(moh705A_long$case_type,
                                 levels = c("suspected", "tested", "confirmed"),
                                 labels = c("Suspected", "Tested", "Confirmed"))

# GRAPH 2: shows line trend for suspected, tested, confirmed for each dispensary
fig2_moh705A <- plot_malaria_cases(moh705A_long, "under-5 years")

save_plot(fig2_moh705A, "images/fig2_moh705A.png")


# GRAPH 3: combined STC for all subcounties
total_moh705A <- moh705A_long %>%
  group_by(subcounty, my, case_type) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  ungroup()

fig3_moh705A <- plot_total_malaria_cases(total_moh705A, "under-5")                             


#===============================================================================
# STEP 2: OVER 5 MALARIA SUSPECTED, TESTED, CONFIRMED
#===============================================================================

# Uses data from MOH705B
moh705B <- readxl::read_excel("../data/WF/clean/khis_dispensary_data.xlsx", 
                      sheet = "MOH705B", skip=1) %>%
  rename_all(tolower) %>%
  select(-subcounty) %>%
  mutate(dispensary = case_when(dispensary == "Ganze H/C" ~ "Ganze",
                                TRUE ~ dispensary))  %>%
  left_join(subcounty_hf %>% select(-dispensary), 
            by = c("dispensary" = "short_f_name"))


# reshape moh705B data
moh705B <- reshape_moh705(moh705B) |>
  rename_all(tolower) |>
  rename(mip = "malaria in pregnancy") 

moh705B$my <- factor(moh705B$my,
                       levels = report_month)

# convert to long format
moh705B_long <- moh705B |>
  tidyr::pivot_longer(cols = suspected:tested, 
                      names_to = "case_type", 
                      values_to = "count")
moh705B_long$case_type <- factor(moh705B_long$case_type,
                                 levels = c("suspected", "tested", "confirmed"),
                                 labels = c("Suspected", "Tested", "Confirmed"))

# GRAPH 4: shows line trend for suspected, tested, confirmed for each dispensary
fig4_moh705B <- plot_malaria_cases(moh705B_long, "over-5 years")

save_plot(fig4_moh705B, "images/fig4_moh705B.png")


# GRAPH 5: combined STC for all subcounties
total_moh705B <- moh705B_long %>%
  group_by(subcounty, my, case_type) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  ungroup()

fig5_moh705B <- plot_total_malaria_cases(total_moh705B, "over-5")
# fig5_moh705B

#===============================================================================
# STEP 3: MALARIA IN PREGNANCY
#===============================================================================

moh705B_mip <- moh705B %>%
  select(dispensary, subcounty, date, mip, month, year, my)

fig6_moh705B <- ggplot(moh705B_mip, aes(x = my, y = mip)) +
  # Bar plot for mip
  geom_col() +
  facet_wrap(~ subcounty, ncol = 1) +  
  theme_minimal() +

    # Labels and theme
  labs(title = "", # paste("Monthly", title_text, "malaria cases"),
       x = "Month-Year of reporting",
       y = "Malaria in pregnancy") + 
  
  # Improve readability of the plot
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
        axis.text.y = element_text(size = 18),
        axis.title = element_text(size = 25),
        title = element_text(size = 20),
        strip.text = element_text(size = 18, face = "bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20, face = "bold"))

