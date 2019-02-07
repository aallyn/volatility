#run regressions_port_boat first to get the input data into the global environment 

#outline
  #generating regression subset figures (by value and geography)
  #generating shelf wide figures (all obs)
  #saving both to figures folder and to folder for rmarkdown doc 

#generating dataframes for labels, def better way via lapply etc...

pointz <- function(df, x_val, g_filt){
  df %>% filter(x == x_val, group == g_filt)
}

points_boat_geo <- bind_rows(
  pointz(boat_df_geo, 2, "N. New England"),
  pointz(boat_df_geo, 3, "S. New England"),
  pointz(boat_df_geo, 4, "N. Mid Atlantic"),
  pointz(boat_df_geo, 5.4, "S. Mid Atlantic"))

points_boat_value <- bind_rows(
  pointz(boat_df_value, 2, "$5K - 100K"),
  pointz(boat_df_value, 3.4, "$100K - 500K"),
  pointz(boat_df_value, 4.6, "> $500K"))

points_port_geo <- bind_rows(
  pointz(port_df_geo, 2, "N. New England"),
  pointz(port_df_geo, 3, "S. New England"),
  pointz(port_df_geo, 4, "N. Mid Atlantic"),
  pointz(port_df_geo, 5, "S. Mid Atlantic"))

points_port_value <- bind_rows(
  pointz(port_df_value, 2, "$15K - 500K"),
  pointz(port_df_value, 3, "$500K - 3,000K"),
  pointz(port_df_value, 4, "> $3,000K"))



line_plot <- function(data_input, limit, point_data) {
  
  point <- data_input %>% 
    filter(x == 3.6)

  ggplot(data_input, aes(x, predicted, group = group)) + 
    geom_ribbon(data = data_input, aes(ymin = conf.low, ymax = conf.high, group = group), 
                alpha = .1, color = "grey70") + 
    labs(y = "Rev. Volatility", x = "Rev. Diversity") +
    geom_line(data = data_input, aes(x, predicted, group = group, linetype = group), 
              size = .5) +  
    labs(linetype = "Region") +
    ggrepel::geom_label_repel(data = point_data, aes(x, predicted, label = group),
                              direction = "both",
                              nudge_y = -.9,
                              force = 1,
                              size = 1.8, #size of box?
                              segment.size = .3, #line segment width
                              segment.alpha = .6, #line segment transparency
                              box.padding = unit(0.1, "lines") #space around box
                              ) + 
    guides(linetype = guide_legend(nrow = 2)) +
    scale_linetype_manual(values=c("twodash", "solid", "dotted", "solid"))+
    theme_bw(base_size = 10) +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,7)) +
    scale_y_continuous(limits = c(min(limit), max(limit)))
  }

line_plot(boat_df_value, boat_limit, points_boat_value)

require(cowplot)  

p_geo <- line_plot(port_df_geo, port_limit, points_port_geo) + 
  theme(axis.title.x = element_blank())

p_value <- line_plot(port_df_value, port_limit, points_port_value) +
  theme(axis.title.x = element_blank())

b_geo <- line_plot(boat_df_geo, boat_limit, points_boat_geo)

b_value <- line_plot(boat_df_value, boat_limit, points_boat_value) 

boat <- cowplot::plot_grid(b_geo, b_value, nrow =1)

port <- cowplot::plot_grid(p_geo, p_value, nrow =1)

stacked_line <- cowplot::plot_grid(port, boat, nrow = 2, labels = c("a", "b"))

stacked_line

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/stacked_line.jpg", stacked_line,
       width = 6, height = 4, units = "in")

ggsave("C:/Users/brian/Dropbox/COCA--diversity/drafts/reports/stacked_line.jpg", stacked_line,
       width = 6, height = 4, units = "in")

#####Shelf Wide Figures

#boat point data, generalizing x and y
boat_point <- input_alt %>% 
  rename(avg_index = avg_index_boat, log_cv_revenue = log_cv_revenue_adj_boat)

#vessel point data, generalizing x and y
port_point <- input_agg %>% 
  rename(avg_index = avg_index_port, log_cv_revenue = log_cv_revenue_adj_port)

shelf_reg_fig <- function(predict_data, point_data) {
  
  ggplot(predict_data, aes(x, predicted)) + 
    geom_ribbon(data = predict_data, aes(ymin = conf.low, ymax = conf.high), 
                alpha = .1, color = "grey70", inherit.aes = TRUE) + 
    geom_line(data = predict_data, aes(x = x, y = predicted), colour = "black", size = .6) + 
    geom_point(data = point_data, aes(x = avg_index, y = log_cv_revenue), 
               size = .5, alpha = .1) +
    geom_smooth(size = NA, method = 'loess') +
    scale_colour_identity() + 
    theme_bw() +
    theme(
      legend.position = 'none') +
    scale_x_continuous(breaks = c(2,4,6), limits = c(1,7)) +
    xlab("Rev. Diversity") + ylab("Rev. Volatility")
}

boat_shelf <- shelf_reg_fig(boat_df, boat_point)+
  theme(axis.title.x = element_blank())

port_shelf <- shelf_reg_fig(port_df, port_point) +
  geom_point(data = port_point, aes(avg_index, log_cv_revenue), size = 1, alpha = .3)

#Final Final 'shelfwide' figure
shelf_port_boat <- cowplot::plot_grid(boat_shelf, port_shelf, 
                                      nrow = 2, labels = c("a", "b"))

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/shelf_port_boat.jpg", shelf_port_boat,
       width = 8.7, height = 11.4, units = "cm")

ggsave("C:/Users/brian/Dropbox/COCA--diversity/drafts/reports/shelf_port_boat.jpg", shelf_port_boat,
       width = 4, height = 4, units = "in")
  