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
    mutate(data = case_when(data == "People sensitised" ~ "n_sensitized",
                            data == "RDT Tests done" ~ "n_rdt",
                            data == "AL Treatment" ~ "n_treated",
                            data == "Malaria cases follow up" ~ "n_malaria_followed",
                            data == "Number of people who slept under a net the night before" ~ "n_sleep_net",
                            data == "Number of people who have at least one net in their household" ~ "n_have_net",
                            data == "Report on malaria DAR sent to CHEW" ~ "n_reports_chew",
                            data == "Report on malaria MOH100 sent to Dispensary" ~ "n_reports_ disp")) %>%
    tidyr::pivot_longer(
      cols = -c(sub_county, dispensary, data),
      names_to = "date",
      values_to = "count") %>%
    
    tidyr::pivot_wider(
      names_from = data,
      values_from = count) %>%
    rename_all(tolower)  %>%
      
    dplyr::mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
                  month = month(date),
                  year = substr(year(date), 3, 4), # extract last 2 digits of year value
                  my = paste0(month ,"-", year))
  }


chp_ganze <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
                                sheet = "ganze")
chp_ganze <- format_data(chp_ganze)


chp_klfnorth <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
                                sheet = "klf_north")
chp_klfnorth <- format_data(chp_klfnorth)


chp_klfsouth <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
                                sheet = "klf_south")
chp_klfsouth <- format_data(chp_klfsouth)


chp_kaloleni <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
                                sheet = "kaloleni")
chp_kaloleni <- format_data(chp_kaloleni)


chp_rabai <- readxl::read_excel("data/WF/clean/chp_data_summary.xlsx", 
                                sheet = "rabai")
chp_rabai <- format_data(chp_rabai)
