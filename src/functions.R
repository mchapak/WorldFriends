#######################
# AUTHOR: DR MOSES KITI
#######################


# SET GLOBAL VALUES
map_cols <- c('#8dd3c7', '#ef8a62', '#bebada', '#d8b365', '#b3de69')
disp_cols <- c('#8dd3c7', '#ef8a62')


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


# create function to plot malaria cases, overall
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


# Plot the total number of malaria cases by county
plot_total_malaria_cases <- function(data, title_text, file_name) {
  plot <- ggplot(data, aes(x = my, y = total, 
                           group = case_type, 
                           color = case_type, 
                           fill = case_type)) + 
    
    # Bar plot for suspected cases
    geom_col(data = data %>% filter(case_type == "Suspected",
    ), 
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
    
    facet_wrap(~ subcounty, ncol = 1) +  
    theme_minimal() +
    
    # Improve readability of the plot
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 25),
          title = element_text(size = 20),
          strip.text = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20, face = "bold"))
  
  return(plot)
}


# # Plot the number of people with nets and those using
# plot_net_usage <- function(data, value, title){
#   plot <- ggplot(data, 
#        aes(x = value, y = reorder(sub_county, value))) +
#   
#     geom_col() +
#     scale_fill_manual(values = map_cols) +
#   
#   geom_text(aes(label = paste0(value, 
#                                " (n=", n_sensitized, ")")),  # Display proportion as percentage
#             hjust = -0.1, size = 5, color = "black", fontface = "bold") +  # Adjust text position
#   
#   labs(#title = "Proportion of people with nets\n out of those sensitized",
#     x = "Proportion of people out of those sensitized (n)",
#     y = "Sub-County") +
#   
#   scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 75)) +
#   
#   theme_minimal() +
#   
#   theme(axis.text = element_text(size = 16),
#         axis.title = element_text(size = 20, face = "bold"))
#   return(plot)
# }


# GRAPH: OVERALL NET USAGE AT THE SUBCOUNTY LEVEL
fxn_fig_net_subcounty <- function(data, x_var, x_axis_label){
  ggplot(data, 
         aes(x = !!sym(x_var), # use the variable whose name is stored inside x_var
             y = reorder(sub_county, !!sym(x_var)),
             fill = sub_county)) +
    
    geom_col(fill = map_cols) +
    
    geom_text(aes(label = paste0(paste0(!!sym(x_var), "%"), 
                                 " (n=", n_sensitized, ")")),  # Display proportion as percentage
              hjust = -0.1, size = 7, color = "black", fontface = "bold") +  # Adjust text position
    
    labs(#title = "Proportion of people with nets\n out of those sensitized",
      x = x_axis_label,
      y = "Sub-County") +
    
    scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    
    theme_minimal() +
    
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 25, face = "bold"))
}


# GRAPH: NET USAGE AT THE HEALTH FACILITY LEVEL
fxn_fig_net_dispensary <- function(data, subcounty, x_var, x_axis_label){
  ggplot(data %>% filter(sub_county == subcounty), 
         aes(x = !!sym(x_var), # use the variable whose name is stored inside x_var
             y = reorder(dispensary, !!sym(x_var)),
             fill = dispensary)) +
    
    geom_col(fill = map_cols[1:2]) +
    
    geom_text(aes(label = paste0(paste0(!!sym(x_var), "%"), 
                                 " (n=", n_sensitized, ")")),  # Display proportion as percentage
              hjust = -0.1, size = 7, color = "black", fontface = "bold") +  # Adjust text position
    
    labs(#title = "Proportion of people with nets\n out of those sensitized",
      x = x_axis_label,
      y = "Health facility") +
    
    scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    
    theme_minimal() +
    
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 25, face = "bold"))
} 



# GRAPH: MALARIA TESTS AND POSITIVITY
plot_malaria_test_type <- function(data, test_type, test_name, age, facet_var){
  ggplot(data,
         aes(x = period2, y = !!sym(test_type), 
             group = case_type, 
             color = case_type, 
             fill = case_type)) +
    
    # Bar plot for suspected cases
    geom_col(data = data %>% filter(case_type == "Tested"), 
             aes(fill = case_type), 
             width = 1, alpha = 0.4) +  # Transparency to differentiate bars
    
    # Line plot for positive
    geom_line(data = data %>% filter(case_type == "Positive"),
              aes(color = case_type),
              size = 1.2) +
    
    # Points for tested and confirmed cases
    geom_point(data = data %>% filter(case_type == "Positive"), 
               aes(color = case_type), 
               size = 2) |>
    
    # Labels and theme
    labs(title = "", # paste("Monthly", title_text, "malaria cases"),
         x = "Month-year",
         y = paste0("Number of tests for age ", age),
         color = test_name,
         fill = test_name) + 
    
    # Ensure both color and fill use the same scale
    scale_color_manual(values = c(col_case[1], col_case[2])) +  
    scale_fill_manual(values = c(col_case[1], col_case[2])) +  
    
    facet_wrap(vars(!!sym(facet_var)), ncol = 2) +
    theme(panel.spacing.x = unit(1, "cm"),  # Horizontal space
          panel.spacing.y = unit(2, "cm")) +  # Vertical space
    
    theme_minimal() +
    
    # Improve readability of the plot
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 16),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 25),
          title = element_text(size = 20),
          strip.text = element_text(size = 18, face = "bold"),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20, face = "bold"))
}

#-------------------------------------------------------------------------------# 
# Compare the % tested and % positive by health facility for SOND 2023 and 2024
fxn_compare_MOH705A <- function(data, facility){
  data <- data %>% filter(dispensary == facility)
  
  ggplot(data, aes(x = facility, y = prop, fill = factor(year))) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~case_type, labeller = labeller(case_type = c("Test" = "% Tested", "Positive" = "% Positive"))) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(title = "", # Proportion of Tests (2023 vs. 2024)
         y = "",
         x = facility,
         fill = "Year") +
    theme_minimal() +
    # Improve readability of the plot
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 18),
          axis.title = element_text(size = 25),
          title = element_text(size = 20),
          strip.text = element_text(size = 18, face = "bold", margin = margin(b=15)),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20, face = "bold")) +
    scale_fill_manual(values = c('#8dd3c7', '#ef8a62')) +  # Blue (2023), Red (2024)
    geom_text(
      aes(label = sprintf("%.1f%%", prop * 100)),
      position = position_dodge(width = 0.8),
      vjust = -0.5, size = 5)
}