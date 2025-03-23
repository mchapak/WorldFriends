#######################
# AUTHOR: DR MOSES KITI
#######################

library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)

# generate names of health facilities in the sub-counties
subcounty_hf <- tibble(
  subcounty = c("Ganze", "Ganze", "Kaloleni", "Kaloleni", "KF North", 
                "KF North", "KF South", "KF South", "Rabai", "Rabai"),
  dispensary = c("Ganze", "Jaribuni", "Makanzani", "Kinarani",
                 "Kiwandani", "Kadzinuni", "Pingilikani", "Tunzanani",
                 "Mgamboni", "Lenga"))

# create functions to transform data
# a. write functions to reshape data into correct format
reshape_moh705 <- function(data) {
  data |> 
    tidyr::pivot_longer(
      cols = -c(dispensary, data),
      names_to = "date",
      values_to = "count"
    ) |> 
    tidyr::pivot_wider(
      names_from = data,
      values_from = count
    ) |>
    rename_all(tolower)  |>
    dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
                  month = month(date),
                  year = substr(year(date), 3, 4), # extract last 2 digits of year value
                  my = paste0(month,"-",year))
}

# convert back to long format
process_moh705_data <- function(data, report_month) {
  data <- reshape_moh705(data) |> 
    rename_all(tolower) |> 
    rename(mip = "malaria in pregnancy")
  
  data$my <- factor(data$my, levels = report_month)
  
  # Convert to long format
  data_long <- data |> 
    tidyr::pivot_longer(cols = suspected:tested, 
                        names_to = "case_type", 
                        values_to = "count")
  
  data_long$case_type <- factor(data_long$case_type,
                                levels = c("suspected", "tested", "confirmed"),
                                labels = c("Suspected", "Tested", "Confirmed"))
  
  return(data_long)
}


# b. define report month levels
report_month <- c("9-23","10-23","11-23","12-23","1-24","2-24","3-24","4-24",
                  "5-24","6-24","7-24","8-24","9-24","10-24","11-24","12-24",
                  "1-25","2-25","3-25","4-25","5-25")

col_case <- c("#542788", "#fdb863", "#b35806")

# create function to plot graphs
plot_malaria_cases <- function(data, title_text, file_name) {
  plot <- ggplot(data, aes(x = my, y = count, 
                           group = case_type, color = case_type)) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    labs(title = paste("Monthly", title_text, "malaria cases"),
         x = "Month-Year",
         y = "Number of Cases",
         color = "Case Type") +
    theme_minimal() +
    scale_color_manual(values = c("Suspected" = "#542788", 
                                  "Tested" = "#fdb863", 
                                  "Confirmed" = "#b35806")) +
    facet_wrap(~ dispensary, ncol = 5) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = 15),
          axis.title = element_text(size = 18))
  
  # Save the figure
  ggsave(plot, filename = file_name, 
         height = 6, width = 15, dpi = 300, bg = "#FFFFFF")
  
  return(plot)
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
                                TRUE ~ dispensary))

### convert moh705A from wide to long format..
moh705A <- reshape_moh705(moh705A) |>
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
plot_malaria_cases(moh705A_long, "under-5 years", "images/fig2_moh705A.png")


# # GRAPH 3: graph 2, but suspected line is filled
# moh705A_fig3 <- ggplot() +
#   # Filled area for suspected cases
#   geom_area(data = moh705A_long %>% 
#               filter(case_type == "Suspected"),
#             aes(x = my, y = count, fill = case_type),
#             alpha = 0.3) + # Adjust transparency
#   
#   # Line plots for all cases
#   geom_line(data = moh705A_long %>% 
#               filter(case_type != "Suspected"), 
#             aes(x = my, y = count, group = case_type, color = case_type), 
#             size = 1.2) +
#   
#   # Points for all cases
#   geom_point(data = moh705A_long %>% 
#                filter(case_type != "Suspected"), 
#              aes(x = my, y = count, color = case_type), 
#              size = 2) +
#   
#   # Labels and theme
#   labs(title = "Under 5 monthly malaria cases",
#        x = "Month",
#        y = "Number of Cases",
#        color = "Case Type",
#        fill = "Case Type") +
#   theme_minimal() +
#   scale_color_manual(values = c(col_case[2:3])) +
#   scale_fill_manual(values = c(col_case[1])) + # Only fill suspected cases
#   facet_wrap(~ dispensary, ncol=5)
# moh705A_fig3


#===============================================================================
# STEP 2: OVER 5 MALARIA SUSPECTED, TESTED, CONFIRMED
# #===============================================================================

# Uses data from MOH705B

moh705B <- readxl::read_excel("../data/WF/clean/khis_dispensary_data.xlsx", 
                      sheet = "MOH705B", skip=1) %>%
  rename_all(tolower) %>%
  select(-subcounty) %>%
  mutate(dispensary = case_when(dispensary == "Ganze H/C" ~ "Ganze",
                                TRUE ~ dispensary))

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

# GRAPH 3: shows line trend for suspected, tested, confirmed for each dispensary
plot_malaria_cases(moh705B_long, "over-5 years", "images/fig3_moh705B.png")


