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
          axis.title = element_text(size = 20),
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
