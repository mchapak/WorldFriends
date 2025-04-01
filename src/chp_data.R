#######################
# AUTHOR: DR MOSES KITI
#######################

library(dplyr)
library(ggplot2)
library(lubridate)
library(tibble)

## if we had wanted to rename the columns
# varnames <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
#                               sheet = "vars")
# # rename_vector <- setNames(varnames$data_name, varnames$var_name)
# chp_kfnorth <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
#                                   sheet = "kf_north") %>%
#   rename(!!!rename_vector)


# write function to format data
format_data <- function(data){
  data <- data %>%
    rename_all(tolower) %>%
    rename(var_name = data) %>%
    mutate(var_name = case_when(var_name == "People sensitised" ~ "n_sensitized",
                                var_name == "RDT Tests done" ~ "n_rdt",
                                var_name == "AL Treatment" ~ "n_treated",
                                var_name == "Malaria cases follow up" ~ "n_malaria_followed",
                                var_name == "Number of people who slept under a net the night before" ~ "n_sleep_net",
                                var_name == "Number of people who have at least one net in their household" ~ "n_have_net",
                                var_name == "Report on malaria DAR sent to CHEW" ~ "n_reports_chew",
                                var_name == "Report on malaria MOH100 sent to Dispensary" ~ "n_reports_disp")) %>%
    tidyr::pivot_longer(
      cols = -c(sub_county, dispensary, var_name),
      names_to = "date",
      values_to = "count") %>%
    
    tidyr::pivot_wider(
      names_from = var_name,
      values_from = count) %>%
    rename_all(tolower)  %>%
      
    dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
                  month = month(date),
                  year = substr(year(date), 3, 4), # extract last 2 digits of year value
                  my = paste0(month ,"-", year))
  }


read_chp_data <- function(subcounty){
  readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
                     sheet = subcounty)
}
  
ganze <- read_chp_data("ganze")
kf_north <- read_chp_data("klf_north")
kf_south <- read_chp_data("klf_south")
kaloleni <- read_chp_data("kaloleni")
rabai <- read_chp_data("rabai")
all_subcounties <- rbind(ganze, kf_north, kf_south, kaloleni, rabai)

chp_long <- format_data(all_subcounties) 


# SUMMARY : CHP reports by county
total_subcounty_chp_reports <- chp_long %>%
  group_by(sub_county) %>%
  summarise(n_sensitized = sum(n_sensitized, na.rm = TRUE),
            n_rdt = sum(n_rdt, na.rm = TRUE),
            n_treated = sum(n_treated, na.rm = TRUE),
            n_malaria_followed = sum(n_malaria_followed, na.rm = TRUE),
            n_sleep_net = sum(n_sleep_net, na.rm = TRUE),
            n_have_net = sum(n_have_net, na.rm = TRUE),
            n_reports_chew = sum(n_reports_chew, na.rm = TRUE),
            n_reports_disp = sum(n_reports_disp, na.rm = TRUE)) %>%
  mutate(prop_have_net = round(100*n_have_net/n_sensitized, 0),
         prop_sleep_net = round(100*n_sleep_net/n_sensitized, 0)) %>%
  ungroup()

# GRAPH by county
map_cols <- c('#8dd3c7', '#ffffb3', '#bebada', '#d8b365', '#b3de69')

fig_chp_subcounty <- ggplot(total_subcounty_chp_reports, 
                            aes(x = prop_have_net, y = reorder(sub_county, prop_have_net))) +
  
  geom_col(fill = map_cols) +
  
  geom_text(aes(label = paste0(prop_have_net, 
                               " (n=", n_sensitized, ")")),  # Display proportion as percentage
            hjust = -0.1, size = 5, color = "black", fontface = "bold") +  # Adjust text position
  
  labs(#title = "Proportion of people with nets\n out of those sensitized",
       x = "Proportion of people with nets out of those sensitized (n)",
       y = "Sub-County") +
  
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  
  theme_minimal() +
  
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))
fig_chp_subcounty

# SUMMARY : CHP reports by dispensary
total_hf_chp_reports <- chp_long %>%
  group_by(dispensary) %>%
  summarise(n_sensitized = sum(n_sensitized, na.rm = TRUE),
            n_rdt = sum(n_rdt, na.rm = TRUE),
            n_treated = sum(n_treated, na.rm = TRUE),
            n_malaria_followed = sum(n_malaria_followed, na.rm = TRUE),
            n_sleep_net = sum(n_sleep_net, na.rm = TRUE),
            n_have_net = sum(n_have_net, na.rm = TRUE),
            n_reports_chew = sum(n_reports_chew, na.rm = TRUE),
            n_reports_disp = sum(n_reports_disp, na.rm = TRUE)) %>%
  mutate(prop_have_net = round(100*n_have_net/n_sensitized, 0),
         prop_sleep_net = round(100*n_sleep_net/n_sensitized, 0)) %>%
  ungroup() %>%
  left_join(subcounty_hf %>% select(-dispensary), 
            by = c("dispensary" = "short_f_name"))

# GRAPH by dispensary
