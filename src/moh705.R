#######################
# AUTHOR: DR MOSES KITI
#######################


# generate names of health facilities in the sub-counties
subcounty_hf <- tibble(
  subcounty = c("Ganze", "Ganze", "Kaloleni", "Kaloleni", "KF North", 
                "KF North", "KF South", "KF South", "Rabai", "Rabai"),
  dispensary = c("Ganze", "Jaribuni", "Makanzani", "Kinarani",
                 "Kiwandani", "Kadzinuni", "Pingilikani", "Tunzanani",
                 "Mgamboni", "Lenga"))

# import MOH705A data and generate positivity rate
moh705A <- readxl::read_excel("../data/WF/clean/khis_dispensary_data.xlsx", 
                      sheet = "MOH705A", skip=1) %>%
  rename_all(tolower) %>%
  mutate(dispensary = case_when(dispensary == "Ganze H/C" ~ "Ganze",
                                TRUE ~ dispensary))

# write function to reshape data into correct format
reshape_moh705 <- function(data) {
  data |> 
    pivot_longer(
      cols = -c(dispensary, data),
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
           time = paste0(month,"-",year))
}

# report month levels
report_month <- c("9-23","10-23","11-23","12-23","1-24","2-24","3-24","4-24",
                  "5-24","6-24","7-24","8-24","9-24","10-24","11-24","12-24",
                  "1-25","2-25","3-25","4-25","5-25")


### convert moh705A from wide to long format..
moh705A <- reshape_moh705(moh705A) |>
  # check that confirmed < tested
  mutate(diff = tested-confirmed)

# check and confirm these values
moh705A_check <- moh705A |>
  filter(diff < 0)

# meanwhile, we filter out erroneous values above and generate positivity rate
moh705A <- moh705A |>
  mutate(pos_rate = (confirmed/tested)*1000) # rate/1000 tested

moh705A$time <- factor(moh705A$time,
                       levels = report_month)

# have all trends on one graph
# ggplot(moh705A |> filter(pos_rate<1001), 
#        aes(x = date, y = pos_rate, color = dispensary, group = dispensary)) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   labs(title = "Malaria Positivity Rate Trends",
#        x = "Month",
#        y = "Positivity Rate per 1000",
#        color = "Health Facility") +
#   scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") + # Format x-axis
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# vs facet the graphs by facility
# Plot with Facets
moh705A_fig <- ggplot(moh705A |> filter(pos_rate<1001), 
                      aes(x = date, y = confirmed, group = dispensary)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(color = "red", size = 2) + 
  labs(title = "Malaria Positivity by Health Facility",
       x = "Month",
       y = "Number malaria positive") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") + # Format x-axis
  ylim(0, 250) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ dispensary, ncol=5)  # Facet by facility; (scales = "free_y")



### MOH705B ###
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
moh705B$time <- factor(moh705B$time,
                       levels = report_month)

