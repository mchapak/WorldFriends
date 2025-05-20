#######################
# AUTHOR: DR MOSES KITI
#######################

pacman::p_load(dplyr,
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

# b. define report month levels
report_month <- c("9-23","10-23","11-23","12-23","1-24","2-24","3-24","4-24",
                  "5-24","6-24","7-24","8-24","9-24","10-24","11-24","12-24",
                  "1-25","2-25","3-25","4-25","5-25")

col_case <- c("#bdbdbd", "#7570b3", "#d95f02") #  "#542788"


#===============================================================================
# STEP 1: UNDER 5 MALARIA BS AND RDT
#===============================================================================
#
# Uses data from MOH706
# in excel, I manually changed the data format. i joined the date row with the
# tested/ positive variable and then split them later as in the function below.

moh706 <- readxl::read_excel("../data/WF/clean/khis_dispensary_data.xlsx", 
                             sheet = "MOH706", skip=3) %>%
  # rename_all(tolower) %>%
  dplyr::mutate(Dispensary = case_when(Dispensary == "Ganze H/C" ~ "Ganze",
                                       TRUE ~ Dispensary))

### convert moh706 from wide to long format..
quarters <- c("Q3-23", "Q4-23", 
              "Q1-24", "Q2-24", "Q3-24", "Q4-24", 
              "Q1-25", "Q2-25")

moh706 <- moh706 |>
  mutate(across(everything(), as.character)) |>
  pivot_longer(
    cols = -c(Dispensary, Data, SubCounty),
    names_to = "date_text",
    values_to = "count"
  ) |>
  # Split the combined date_text column into separate date and text columns
  tidyr::separate(
    col = date_text,
    into = c("date", "case_type"),  # Temporary names for the split parts
    sep = " ",  # Split on first space
    extra = "merge"  # Keep any additional spaces in the text part
  ) |>
  pivot_wider(
    names_from = Data,
    values_from = count
  ) |>
  rename_all(tolower)  |>
  mutate(date = as.Date(date, format="%m/%d/%Y"),
         month = month(date),
         year = substr(year(date), 3, 4), # extract last 2 digits of year value
         my = paste0(month,"-",year),
         # mutate another period into quarters
         period = paste0("Q", quarter(date)), # Jan-Mar=Q1, Apr-Jun=Q2, etc.)
         period2 = paste0(period, "-", year),
         period2 = factor(period2,
                          levels = quarters)) |> 
  filter(my != "NA-NA")

### clean variable names and types
moh706 <- moh706 |>
  select(-c(subcounty)) %>%
  rename(mbs_u5 = "malaria bs (under 5)",
         mbs_o5 = "malaria bs (over 5)",
         mrdt_u5 = "malaria rdt (under 5)",
         mrdt_o5 = "malaria rdt (over 5)")  |>
  left_join(subcounty_hf %>% select(-dispensary), 
            by = c("dispensary" = "short_f_name")) |>
  mutate(mbs_u5 = as.numeric(mbs_u5),
         mbs_o5 = as.numeric(mbs_o5),
         mrdt_u5 = as.numeric(mrdt_u5),
         mrdt_o5 = as.numeric(mrdt_o5))

moh706$my <- factor(moh706$my,
                    levels = report_month)

### and the convert back to. long format
moh706$case_type <- factor(moh706$case_type,
                           levels = c("Tested", "Positive"),
                           labels = c("Tested", "Positive"))


### summaries of number of tests by subcounty 
moh706_subcounty_quarters <- moh706 |>
  select(subcounty, case_type, mbs_u5, mbs_o5, mrdt_u5, mrdt_o5,
         period, period2) |>
  group_by(subcounty, case_type, period2) |>
  summarise(mbs_u5 = sum(mbs_u5, na.rm = TRUE),
            mbs_o5 = sum(mbs_o5, na.rm = TRUE),
            mrdt_u5 = sum(mrdt_u5, na.rm = TRUE),
            mrdt_o5 = sum(mrdt_o5, na.rm = TRUE)) |>
  ungroup() 

fig_mbs_u5_subcounty <- plot_malaria_test_type(moh706_subcounty_quarters, "mbs_u5", "Microscopy", "<5 years", "subcounty")
fig_mbs_o5_subcounty <- plot_malaria_test_type(moh706_subcounty_quarters, "mbs_o5", "Microscopy", "≥5 years", "subcounty")
fig_mrdt_u5_subcounty <- plot_malaria_test_type(moh706_subcounty_quarters, "mbs_u5", "RDT", "<5 years", "subcounty")
fig_mrdt_o5_subcounty <- plot_malaria_test_type(moh706_subcounty_quarters, "mbs_o5", "RDT", "≥5 years", "subcounty")


### summary of number of tests by dispensary
moh706_dispensary_quarters <- moh706 |>
  select(subcounty, dispensary, case_type, mbs_u5, mbs_o5, mrdt_u5, mrdt_o5,
         period, period2) |>
  group_by(dispensary, case_type, period2) |>
  summarise(mbs_u5 = sum(mbs_u5, na.rm = TRUE),
            mbs_o5 = sum(mbs_o5, na.rm = TRUE),
            mrdt_u5 = sum(mrdt_u5, na.rm = TRUE),
            mrdt_o5 = sum(mrdt_o5, na.rm = TRUE)) |>
  ungroup()   |>
  left_join(subcounty_hf %>% select(-dispensary), 
            by = c("dispensary" = "short_f_name"))

## this shows for all subcounties. 
# fig_mbs_u5_dispensary <- plot_malaria_test_type(moh706_dispensary_quarters, "mbs_u5", "Microscopy", "<5 years", "dispensary")
# fig_mbs_o5_dispensary <- plot_malaria_test_type(moh706_dispensary_quarters, "mbs_o5", "Microscopy", "≥5 years", "dispensary")
# fig_mrdt_u5_dispensary <- plot_malaria_test_type(moh706_dispensary_quarters, "mbs_u5", "RDT", "<5 years", "dispensary")
# fig_mrdt_o5_dispensary <- plot_malaria_test_type(moh706_dispensary_quarters, "mbs_o5", "RDT", "≥5 years", "dispensary")




