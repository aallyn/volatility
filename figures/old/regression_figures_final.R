# script to generate regression output
  #order is as follows,
  #factors and limit parameters are defined 
  #function for  regression subset plots is defined
  #fnction for 'shelfwide' all observation regressions is defined
  #raw function outputs are modified and labeled 
  #figures are arranged, finalized, and saved in cowplot

#defining factors for figures

geo_factor <- c("All Categories", "N. New England", "S. New England",
              "N. Mid Atlantic", "S. Mid Atlantic")
value_factor_boat <- c("All Categories", "$5,000-$100,000", 
                       "$100,000-$500,000", "> $500,000") 
value_factor_port <- c("All Categories", "$15,000 - 500,000", 
                       "$500,000-$3,000,000", "> $3,000,000")

#defining limits for axis values
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
      legend.position = 'none') +
        scale_x_continuous(breaks = c(2,4,6), limits = c(1,7)) +
        xlab("Revenue Diversity") + ylab("Revenue Volatility")+
    scale_x_continuous(breaks = c(2,4,6), limits = c(1,7)) +
    scale_y_continuous(limits = c(min(limit), max(limit)))
                      
  }


#SHELF WIDE FUNCTION
  #predict data = data for geom_line() to fllow
  #point data is the data that plots the observations 
  #point size ommitted so it can be modified specific to ports and boats

#boat point data, generalizing x and y
boat_point <- input_alt %>% 
rename(avg_index = avg_index_boat, log_cv_revenue = log_cv_revenue_adj_boat)

#vessel point data, generalizing x and y
port_point <- input_agg %>% 
rename(avg_index = avg_index_port, log_cv_revenue = log_cv_revenue_adj_port)

shelf_reg_fig <- function(predict_data, point_data) {
  
  
  ggplot() + 
    geom_line(data = predict_data, aes(x = x, y = predicted), colour = "black", size = 1.3) + 
    geom_point(data = point_data, aes(x = avg_index, y = log_cv_revenue), 
               size = .5, alpha = .2) +
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
  geom_point(data = port_point, aes(avg_index, log_cv_revenue), size = 1.7, alpha = .3)

#Final Final 'shelfwide' figure
shelf_port_boat <- cowplot::plot_grid(boat_shelf, port_shelf, 
                   nrow = 2, labels = c("a", "b"))

ggsave("C:/Users/bkennedy/Dropbox/COCA--diversity/figures/shelf_port_boat.jpg", shelf_port_boat,
       width = 8.7, height = 11.4, units = "cm")


#vessel plot
sub_boat <- cowplot::plot_grid(
  #geo figure w/ plot title 
  reg_fig(boat_df, boat_df_geo, geo_factor, boat_limit) + 
    ggtitle("Fitted relationship for vessels subsetted by region (a) and operation size (b)", 
            subtitle = "'Shelf-wide' relationships demarcated by red background"), 
  #operation size figure                  
  reg_fig(boat_df, boat_df_value, value_factor_boat, boat_limit), 
                   nrow = 2, labels = c("a", "b"))

sub_boat

ggsave("C:/Users/bkennedy/Dropbox/COCA--diversity/figures/sub_boat.jpg", sub_boat,
       width = 17.8, height = 11.4, units = "cm")



require(extrafont)

#port plot
sub_port <- cowplot::plot_grid(
  #geo figure w/ plot title 
  reg_fig(port_df, port_df_geo, geo_factor, port_limit) + 
   ggtitle("Fitted relationship for ports subsetted by region (a) and operation size (b)", 
              subtitle = "'Shelf-wide' relationships demarcated by red background"), 
  #operation size figure  
  reg_fig(port_df, port_df_value, value_factor_port, port_limit), 
                   nrow = 2, labels = c("a", "b"))

sub_port

ggsave("C:/Users/bkennedy/Dropbox/COCA--diversity/figures/sub_port.jpg", sub_port,
       width = 17.8, height = 11.4, units = "cm")



 library(extrafont)
 library(ggplot2)
 if( 'xkcd' %in% fonts()) {
   p <- ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars) +
     theme(text = element_text(size = 16, family = "xkcd"))
   } else {
     warning("Not xkcd fonts installed!")
     p <- ggplot() + geom_point(aes(x=mpg, y=wt), data=mtcars)
 }
 p
 
  download.file("http://simonsoftware.se/other/xkcd.ttf",
                  dest="xkcd.ttf", mode="wb")
  system("mkdir ~/.fonts")
  system("cp xkcd.ttf ~/.fonts")
  font_import(pattern = "[X/x]kcd", prompt=FALSE)
  fonts()
  fonttable()
  if(.Platform$OS.type != "unix") {
    ## Register fonts for Windows bitmap output
      loadfonts(device="win")
    } else {
      loadfonts()
      }
 
  
 
 
 
 

