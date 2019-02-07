
#####Subset Figures

boat_limit <- c(2.5, 3.9)

port_limit <- c(1,3)

temp_input <- port_df_value %>% 
  filter(x == 3.6)

x_temp <- seq(2, 1 + length(temp_input$x), by =1)

label_place <- data.frame(x_temp)

left_join(label_place,port_df_value, by = c("x_temp" = "x"))



bind_cols(temp_input, label_place)



line_plot <- function(data_input, limit) {
  
  point <- data_input %>% 
    filter(x == 3.6)

  ggplot(data_input, aes(x, predicted, group = group)) + 
    geom_ribbon(data = data_input, aes(ymin = conf.low, ymax = conf.high, group = group), 
                alpha = .1, color = "grey70") + 
    labs(y = "Rev. Volatility", x = "Catch Diversity") +
    geom_line(data = data_input, aes(x, predicted, group = group, linetype = group), 
              size = .5) +  
    labs(linetype = "Region") +
    ggrepel::geom_label_repel(data = point, aes(x, predicted, label = group),
                              direction = "both",
                              nudge_y = -.8,
                              size = 2.3,
                              box.padding = unit(0.5, "lines")
                              ) + 
    guides(linetype = guide_legend(nrow = 2)) +
    scale_linetype_manual(values=c("twodash", "solid", "dotted", "solid"))+
    theme_bw(base_size = 10, base_family ="serif") +
    theme(legend.position = "none") +
    scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,7)) +
    scale_y_continuous(limits = c(min(limit), max(limit)))
  }

line_plot(port_df_geo, port_limit)

require(cowplot)  

p_geo <- line_plot(port_df_geo, port_limit)

p_value <- line_plot(port_df_value, port_limit)

b_geo <- line_plot(boat_df_geo, boat_limit)

b_value <- line_plot(boat_df_value, boat_limit)

boat <- cowplot::plot_grid(b_geo, b_value, nrow =1)

port <- cowplot::plot_grid(p_geo, p_value, nrow =1)

cowplot::plot_grid(port, boat, nrow = 2, labels = c("a", "b"))

stacked_line <- cowplot::plot_grid(port, boat, nrow = 2, labels = c("a", "b"))

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
  
  ggplot() + 
    geom_line(data = predict_data, aes(x = x, y = predicted), colour = "black", size = .6) + 
    geom_point(data = point_data, aes(x = avg_index, y = log_cv_revenue), 
               size = .5, alpha = .1) +
    geom_smooth(size = NA, method = 'loess') +
    scale_colour_identity() + 
    theme_bw() +
    theme(
      legend.position = 'none') +
    scale_x_continuous(breaks = c(2,4,6), limits = c(1,7)) +
    xlab("Revenue Diversity") + ylab("Revenue Volatility")
}

boat_shelf <- shelf_reg_fig(boat_df, boat_point) +  
  ggtitle("Fitted relationships for vessels (a) \nand ports (b), all observations") +
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
  