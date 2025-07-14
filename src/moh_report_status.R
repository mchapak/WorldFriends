#######################
# AUTHOR: DR MOSES KITI
#######################

pacman::p_load(
  dplyr,
  ggplot2,
  readxl)

# read data, this was manually modified
moh_reports <- read_excel("data/WF/clean/khis_Update_March2025.xlsx", 
                              sheet = "moh_forms_report") %>%
  dplyr::mutate(dispensary = case_when(dispensary == "Ganze H/C" ~ "Ganze",
                                       TRUE ~ dispensary))

# First create a proper time-based ordering for periods
moh_reports <- moh_reports %>%
  mutate(
    period_order = case_when(
      period == "JAS" ~ 1,
      period == "OND" ~ 2,
      period == "JFM" ~ 3,
      TRUE ~ 4
    ),
    period_year = paste(period, year)
  )


# Create a combined period-year variable with proper ordering
moh_reports2 <- moh_reports %>%
  mutate(
    subcounty = factor(subcounty, levels = c("Ganze", "Kaloleni", 
                                             "Kilifi North", "Kilifi South", 
                                             "Rabai")),
    reports = factor(reports, levels = c("expected", "actual")),
    period_year = factor(
      paste(period, year),
      levels = c("JAS 2024", "OND 2024", "JFM 2025"),  # Ensures correct ordering
      ordered = TRUE
    )
  )

# Create a custom color palette (6 colors for 3 periods × 2 report types)
period_palette <- c(
  # JAS 2024 colors
  "actual.JAS 2024" = "#aec7e8",   # Light blue
  "expected.JAS 2024" = "#1f77b4", # DARK blue
  
  # OND 2024 colors
  "actual.OND 2024" = "#ffbb78",    # Light orange
  "expected.OND 2024" = "#ff7f0e",  # Dark orange
  
  # JFM 2025 colors
  "actual.JFM 2025" = "#98df8a",    # Light green
  "expected.JFM 2025" = "#2ca02c"   # Dark green
)

# Create the plot
fxn_moh_reports <- function(data, form_type) {
  form_data <- data %>% filter(form == form_type)
  
  # create the plot
  p <- ggplot(form_data,
             aes(x = subcounty, 
                 y = number, 
                 fill = interaction(reports, period_year))) +
    
    geom_col(position = position_dodge(width = 0.8), 
             width = 0.7) +
    
    # Set y-axis to whole numbers only
    scale_y_continuous(
      breaks = function(x) seq(0, max(x), by = 1),  # Whole numbers only
      expand = expansion(mult = c(0, 0.1))  # Add some padding at top
    ) +
    
    # Apply custom color palette
    scale_fill_manual(
      name = "",
      values = period_palette,
      labels = c(
        "Actual (Jul-Sep 2024)", "Expected (JAS 2024)",
        "Actual (Oct-Dec 2024)", "Expected (OND 2024)",
        "Actual (Jan-Mar 2025)", "Expected (JFM 2025)"
      )
    ) +
    
    labs(
      title = "",
      x = "",
      y = "Number of Reports",
      fill = NULL
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      title = element_text(size = 25),
      axis.text = element_text(size = 16, hjust = 1),
      axis.title = element_text(size = 18),
      panel.grid.major.x = element_blank()
    ) 
  
  return(p)
}
# +
#   
#   # Add value labels if desired
#   geom_text(
#     aes(label = number),
#     position = position_dodge(width = 0.8),
#     vjust = -0.5,
#     size = 3
#   )

# draw plots
fig_moh705A_report <- fxn_moh_reports(moh_reports2, "MOH705A")
fig_moh705B_report <- fxn_moh_reports(moh_reports2, "MOH705B")
fig_moh515_report <- fxn_moh_reports(moh_reports2, "MOH515")



