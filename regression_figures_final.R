# script to generate figure

#defining factors for figures

geo_factor <- c("All Categories", "N. New England", "S. New England",
              "N. Mid Atlantic", "S. Mid Atlantic")
value_factor_boat <- c("All Categories", "$5,000-$100,000", 
                       "$100,000-$500,000", "> $500,000") 
value_factor_port <- c("All Categories", "$15,000 - 500,000", 
                       "$500,000-$3,000,000", "> $3,000,000")

boat_limit <- c(2.5, 4)

port_limit <- c(1,3)


#function for regression figures
  #data = regression prediction with no subsets
  #data_sub = subset regression prediction 
  #factor_order = factor order for relevant subset 
  #limit = y axis value limits 

reg_fig <- function(data, data_sub, factor_order, limit) 
  
  {
  input_data <- data_sub %>% union(., data) %>% 
    mutate(
      group = case_when(
        group == 1 ~ "All Categories", #if not on the list you can't get it in
        TRUE ~ as.character(group)),
      Y = group,
      group_color = case_when(
        group == 1 ~ 0, #if not on the list you can't get it in
        TRUE ~ 1),
      group = factor(group, 
                     levels = c(factor_order)))
  
  ggplot(input_data, aes(x = x, y = predicted)) + 
    geom_rect(data = subset(input_data, group == "All Categories"),aes(fill = group),xmin = -Inf,xmax = Inf,
              ymin = -Inf, ymax = Inf, alpha = 0.01) +
    geom_line(data = transform(input_data, group = NULL), aes(group = Y)
              , alpha = 0.35) +   
    geom_line(aes(group = group, colour = "black"), size = 1.2) + 
    geom_smooth(size = NA, method = 'loess') +
    scale_colour_identity() + 
    facet_grid(~ group) +
    theme_bw() +
    theme(
          legend.position = 'none',
          axis.title = element_blank()) +
    scale_x_continuous(breaks = c(2,4,6), limits = c(1,7)) +
    scale_y_continuous(limits = c(min(limit), max(limit)))
                      
  }

##


# halfway there...

shelf_reg_fig <- function(predict_data, point_data) {
  
  
  ggplot() + 
    geom_line(data = predict_data, aes(x = x, y = predicted), colour = "black", size = 1) + 
    geom_point(data = point_data, aes(x = avg_index_boat, y = log_cv_revenue_adj_boat), 
               size = .5, alpha = .3) +
    geom_smooth(size = NA, method = 'loess') +
    scale_colour_identity() + 
    theme_bw() +
    theme(
      legend.position = 'none',
      axis.title = element_blank()) +
    scale_x_continuous(breaks = c(2,4,6), limits = c(1,7)) 
}

shelf_reg_fig(boat_df, input_alt)

#vessel all with plot

#vessel plot
cowplot::plot_grid(reg_fig(boat_df, boat_df_geo, geo_factor, boat_limit), 
                   reg_fig(boat_df, boat_df_value, value_factor_boat, boat_limit), 
                   nrow = 2, labels = c("B", "C"))

#port plot
cowplot::plot_grid(reg_fig(port_df, port_df_geo, geo_factor, port_limit), 
                   reg_fig(port_df, port_df_value, value_factor_port, port_limit), 
                   nrow = 2, labels = c("B", "C"))



