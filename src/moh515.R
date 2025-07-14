#######################
# AUTHOR: DR MOSES KITI
#######################

pacman::p_load(dplyr,
               ggplot2,
               lubridate,
               tibble)

chp_treated_data <- readxl::read_excel("data/WF/clean/MOH515_chew_summary_data.xlsx", 
                                       sheet = "raw_data_dispensary_extract") %>%
  mutate(u5_treated_prop = round(100*(n_u5_malaria_treated/n_u5_rdtpos), 1),
         o5_treated_prop = round(100*(n_o5_malaria_treated/n_o5_rdtpos), 1))

fxn_treated_dispensary <- function(data, subcounty, x_var, x_axis_label, n_pos, n_treated){
  ggplot(data %>% filter(sub_county == subcounty), 
         aes(x = !!sym(x_var), # use the variable whose name is stored inside x_var
             y = reorder(dispensary, !!sym(x_var)),
             fill = dispensary)) +
    
    geom_col(fill = map_cols[1:2], width = 0.7) +
    
    # Add labels INSIDE bars (white text for contrast)
    geom_text(aes(label = paste0(!!sym(n_treated)," pos/", !!sym(n_pos), " treated")),  # Display proportion as percentage
              hjust = 1.1, size = 5, color = "white", fontface = "bold") +  # Adjust text position
    
    labs(#title = "Proportion of people with nets\n out of those sensitized",
      x = x_axis_label,
      y = "Health facility") +
    
    scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
    
    theme_minimal() +
    
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 20, face = "bold"))
}  

fig_ganze_u5_treated <- fxn_treated_dispensary(chp_treated_data, 
                                               "Ganze", "u5_treated_prop", "<5", 
                                               "n_u5_malaria_treated", "n_u5_rdtpos")
fig_ganze_o5_treated <- fxn_treated_dispensary(chp_treated_data, 
                                               "Ganze", "o5_treated_prop", "≥5", 
                                               "n_o5_malaria_treated", "n_o5_rdtpos")