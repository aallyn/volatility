

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", 
       "gridExtra", "broom", "viridis", "sjPlot", "jtools",
       "ggstance", "MASS", "leaps", "ggeffects",
       "hrbrthemes")

lapply(x, require, character.only = TRUE)

options(scipen = 999)  #turning off scientific notation to control # of digits

input_alt <-  read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input_temp.csv") %>% 
  mutate(
    log_boat_index = log(avg_index_boat),
    value_cat = factor(value_cat, 
                       levels = c("$5,000-$100,000","$100,000-$500,000", 
                                  "> $500,000")),
    value_cat_port = factor(value_cat_port, 
                            levels = c("$15,000 - 500,000",
                                        "$500,000-$3,000,000", "> $3,000,000")),
    mega_subregion = factor(mega_subregion, 
                            levels = c("Northern New England", "Southern New England",
                                       "Northern Mid Atlantic","Southern Mid Atlantic")),
    mega_subregion_alt = case_when(
      mega_subregion == "Northern New England" ~ "N. New England",
      mega_subregion == "Southern New England" ~ "S. New England",
      mega_subregion == "Northern Mid Atlantic" ~ "N. Mid Atlantic",
      mega_subregion == "Southern Mid Atlantic" ~ "S. Mid Atlantic")) %>% 
        na.omit()


#summary stats for boats and ports
input_alt %>% 
  #group_by(mega_subregion) %>% 
  summarise(
            n = n_distinct(HULLNUM),
            mean_vol = mean(cv_revenue_boat),
            median_vol = mean(cv_revenue_boat),
            sd_rev = sd(cv_revenue_boat),
            mean_index = mean(avg_index_boat),
            median_index = median(avg_index_boat),
            sd_index = sd(avg_index_boat))


input_alt %>% 
  #group_by(mega_subregion) %>% 
  summarise(
    n = n_distinct(port_tidy),
    mean_index = mean(avg_index_port),
    median_index = median(avg_index_port),
    sd_index = sd(avg_index_port),
    mean_vol = mean(cv_revenue_port),
    median_vol = mean(cv_log_revenue_port),
    sd_rev = sd(cv_revenue_port))


sqldf("select count(distinct port_tidy) from input_alt where port_tidy like '%_NJ%'")


#BOAT REG

#model selection 
leaps_clean_boat <- regsubsets(log_cv_revenue_adj_boat ~ avg_index_boat + I(avg_index_boat^2) +
                                 avg_index_port + value_cat + mega_subregion, input_alt, nbest = 5)

plot(leaps_clean_boat, scale = "bic")
#regression: output, table, and diagnostics 

  clean_boat <- lm(log_cv_revenue_adj_boat ~ avg_index_port + log_boat_index + I(log_boat_index^2) + value_cat + mega_subregion, 
                    data = input_alt)
  
  #adding cubic term
  clean_boat_lm <- lm(log_cv_revenue_adj_boat ~ avg_index_port + avg_index_boat + I(avg_index_boat^2) + 
                        I(avg_index_boat^3) + value_cat + mega_subregion, data = input_alt)
  
  #adding cubic term and logging index variable
  clean_boat_lm_loglog <- lm(log_cv_revenue_adj_boat ~ avg_index_port + log_boat_index + I(log_boat_index^2) + 
                        I(log_boat_index^3) + value_cat + mega_subregion, data = input_alt)
  
  
  summary(clean_boat_lm)
    
  
  summary(clean_boat_lm_loglog)
  
  plot(clean_boat_lm)
  
  plot(clean_boat_lm_loglog)
  
  
  sample <- predict(clean_boat_lm, type = "response")
  

  ##generating preduction friendly data frame 
  library(ggeffects)
  boat_df <- ggpredict(clean_boat_lm, terms = c("avg_index_boat"))

  boat_loglog <- ggpredict(clean_boat_lm_loglog, terms = c("log_boat_index"))
  #plotting
a <-   ggplot(boat_df, aes(x, predicted)) + 
    geom_ribbon(data = boat_df, aes(ymin = conf.low, ymax = conf.high), 
                alpha = .2, fill = "cyan") +
    geom_point(data = input_alt, aes(avg_index_boat, log_cv_revenue_adj_boat), alpha = .05) +
    geom_line(size = 1) +
    theme_tufte()

b <- ggplot(boat_loglog, aes(x, predicted)) + 
  geom_ribbon(data = boat_loglog, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, fill = "red") +
  geom_point(data = input_alt, aes(log_boat_index, log_cv_revenue_adj_boat), alpha = .05) +
  geom_line(size = 1) +
  theme_tufte()

grid.arrange(a,b, nrow=2)
  
  
  #reg table
  tab_model(clean_boat_lm, title = "Vessel Level Regression Ouput (p-value: < 2.2e-16)",
            show.p = TRUE)
  #reg results visual
boat_regplot <-   plot_model(clean_boat_lm, show.values = TRUE, value.offset = .3, colors = "black") +
    theme_minimal()
boat_regplot  

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/boat_regplot.jpg", boat_regplot)


  #anova table
  anov_boat <- anova(clean_boat_lm)
  dfr_boat <- data.frame(anov_boat)
  kable(dfr_boat, caption="Anova table for the basic *port* model")
  
  #diagnostics
  plot(clean_boat_lm)
  
#PORT REG

  #aggregating data 
  #aggregated version of port data-set, vessel diversity is summarised by weighted mean 
  #ocean city deleted due to abberant values
  
  input_agg <- input_alt %>% 
    group_by(port_tidy) %>% 
    mutate(mean_boat_index = weighted.mean(avg_index_boat, avg_value_boat)) %>% 
    distinct(port_tidy, log_cv_revenue_adj_port, avg_index_port, mean_boat_index,
             value_cat_port, mega_subregion) %>% 
                filter(!port_tidy == "OCEANCITY_MD") %>% 
                  na.omit() 

#counting number of ports to make sure there are no duplicates    
duh <-   input_agg %>% distinct(port_tidy) 
  View(duh)
  
#model selection 
  leaps_clean_port <- regsubsets(log_cv_revenue_adj_port ~ avg_index_port + mean_boat_index + 
                                   value_cat_port + mega_subregion, input_agg, nbest = 5)
  
  plot(leaps_clean_port, scale = "bic")
  
#regression form  
agg_port <- lm(log_cv_revenue_adj_port ~ avg_index_port + mean_boat_index + 
                   value_cat_port + mega_subregion, input_agg)  
  
#regression: output, table, and diagnostics 
  
  #reg table
  tab_model(agg_port)
  #reg outputs visual
port_regplot <- plot_model(agg_port, show.values = TRUE, value.offset = .3, colors = "black") +
    theme_minimal()

port_regplot

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/port_regplot.jpg", port_regplot)


  #diagnostics
  plot(agg_port)


#prediction figures####
  
  #generating values to smooth line
number <- toString(seq(1, 8, by = .2))
  

geo_stuff <- input_alt %>% dplyr::distinct(mega_subregion_alt, mega_subregion)
  
  #input datasets for vessel level prediction plots
boat_df <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]"))
  
boat_df_geo <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                  "mega_subregion")) %>% 
  left_join(., geo_stuff, by = c("group" = "mega_subregion")) %>% 
  mutate(group = mega_subregion_alt) %>% 
    dplyr::select(-mega_subregion_alt)

boat_df_value <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                    "value_cat"))

##figure generation
shelf_boat <-  ggplot(boat_df, aes(x, predicted)) + 
  geom_ribbon(data = boat_df, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, color = "grey70") +
  labs(y = "Rev. Volatility", x = "Catch Diversity") +
  geom_point(data = input_alt, aes(avg_index_boat, log_cv_revenue_adj_boat), 
             alpha = .2, size = .6) +
  geom_line(size = 1) +
  theme_tufte(base_size = 10, base_family ="serif") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7), limits = c(1,7)) 


geo_boat <- ggplot(boat_df_geo, aes(x, predicted, group = group)) + 
  geom_ribbon(data = boat_df_geo, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .2, color = "grey70") + 
  labs(y = "Rev. Volatility", x = "Catch Diversity") +
  geom_line(data = boat_df_geo, aes(x, predicted, group = group, linetype = group), 
            size = 1) +  
    labs(linetype = "Region") +
  guides(linetype = guide_legend(nrow = 2)) +
  scale_linetype_manual(values=c("twodash", "solid", "dotted", "longdash"))+
  theme_tufte(base_size = 10, base_family ="serif") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,7)) +
  scale_y_continuous(breaks = seq(2.5,5,.5), limits = c(2.5,5))

value_boat <- ggplot(boat_df_value, aes(x, predicted, group = group)) + 
  geom_ribbon(data = boat_df_value, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .3,  color = "grey70") + 
  labs(y = "Rev. Volatility", x = "Catch Diversity") +
    geom_line(data = boat_df_value, aes(x, predicted, group = group, linetype = group), 
              size = 1) +
      labs(linetype = "Value Category") + #legend label
  guides(linetype = guide_legend(nrow = 2)) + #break in legend label
  scale_linetype_manual(values=c("twodash", "solid", "dotted"))+
  theme_tufte(base_size = 10, base_family ="serif") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,7)) +
  scale_y_continuous(breaks = seq(2.5,5,.5), limits = c(2.5,5))

#combining plots together
library(cowplot)

#bottom row in nested plot
boat_obj <- plot_grid(geo_boat, value_boat, labels = c("B", "C"))

#full plot
boat_predict <- plot_grid(shelf_boat, boat_obj, labels = c("A", ""), 
          ncol = 1)

boat_predict

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/shelf_boat.jpg", boat_predict,
       height = 6.25, width = 6.6, units = c("in"))
#two step process, first create small fig fo rprojections then align it with bigger figure 

#input datasets for port level prediction plots
port_df <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]"))
port_df_geo <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                  "mega_subregion")) %>% 
              left_join(., geo_stuff, by = c("group" = "mega_subregion")) %>% 
                  mutate(group = mega_subregion_alt) %>% 
  dplyr::select(-mega_subregion_alt)
              
port_df_value <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                    "value_cat_port"))


#port plots

shelf_port <-  ggplot(port_df, aes(x, predicted)) + 
  geom_line(data = port_df, aes(x, predicted), 
            size = 1) +  
  geom_ribbon(data = port_df, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, color = "grey70") +
  labs(y = "Rev. Volatility", x = "Catch Diversity") +
  geom_point(data = input_agg, aes(avg_index_port, log_cv_revenue_adj_port), alpha = .4) +
  theme_tufte(base_size = 10, base_family ="serif") +
    scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8), limits = c(1,8))

geo_port <- ggplot(port_df_geo, aes(x, predicted, group = group)) + 
  geom_ribbon(data = port_df_geo, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .1, color = "grey70") + 
  labs(y = "Rev. Volatility", x = "Catch Diversity") +
  geom_line(data = port_df_geo, aes(x, predicted, group = group, linetype = group), 
            size = 1) +  
  labs(linetype = "Region")+
  guides(linetype = guide_legend(nrow = 2)) + #break in legend label
  scale_linetype_manual(values=c("twodash", "solid", "dotted", "longdash")) +
  theme_tufte(base_size = 10, base_family ="serif") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,8)) +
  scale_y_continuous(breaks = seq(1.5,3.5,.5), limits = c(1,3.5))


value_port <- ggplot(port_df_value, aes(x, predicted, group = group)) + 
  geom_line(data = port_df_value, aes(x, predicted, group = group, linetype = group), 
            size = 1) +
  geom_ribbon(data = port_df_value, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .1,  color = "grey70") + 
  labs(linetype = "Value Category")+
  guides(linetype = guide_legend(nrow = 2)) + #break in legend label
  scale_linetype_manual(values=c("twodash", "solid", "dotted"))+
  labs(y = "Rev. Volatility", x = "Catch Diversity") +
  theme_tufte(base_size = 10, base_family ="serif") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,8)) +
  scale_y_continuous(breaks = seq(1.5,3.5,.5), limits = c(1,3.5))

#bottom row in nested plot
port_obj <- plot_grid(geo_port, value_port, labels = c("B", "C"))

#full plot
port_predict <- plot_grid(shelf_port, port_obj, labels = c("A", ""), 
          ncol = 1)

port_predict

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/port_predict.jpg", 
       port_predict, height = 6.25, width = 6.6, units = c("in"))

##alt figure w/ lme trend approach, works need to add the shelf wide and label 

#boats

boat_df_value_2 <- boat_df_value %>% union(., boat_df) %>% 
  mutate(
    group = case_when(
      group == 1 ~ "All Categories", #if not on the list you can't get it in
      TRUE ~ as.character(group)),
    Y = group)

boat_cool <- ggplot(boat_df_value_2, aes(x = x, y = predicted)) + 
  geom_line(data = transform(boat_df_value_2, group = NULL), aes(group = Y)
            , alpha = 0.35) +   
  geom_line(aes(group = group, colour = "black"), size = 1.2) + 
  geom_smooth(size = NA, method = 'loess') +
  scale_colour_identity() + 
  facet_grid(~ group, scales="free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank()) +
  scale_x_continuous(breaks = c(1,3,5,7), limits = c(1,7)) 

boat_cool



#ports

port_df_value_2 <- port_df_value %>% union(., port_df) %>% 
  mutate(
    group = case_when(
      group == 1 ~ "All Categories", #if not on the list you can't get it in
      TRUE ~ as.character(group)),
    Y = group)

port_cool <- ggplot(port_df_value_2, aes(x = x, y = predicted)) + 
  geom_line(data = transform(port_df_value_2, group = NULL), aes(group = Y)
            , alpha = 0.35) +   
  geom_line(aes(group = group, colour = "black"), size = 1.2) + 
  geom_smooth(size = NA, method = 'loess') +
  scale_colour_identity() + 
  facet_grid(~ group, scales="free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank())

require(cowplot)

#rough output
grid.arrange(boat_cool, port_cool)











#line not working right now, good first start; clearly a technique with a bunch of potential! 
  
  p_plot <- function(a){
    ggplot(a, aes(x, predicted, group = group)) + 
      geom_ribbon(data = a, aes(ymin = conf.low, ymax = conf.high, group = group), 
                  alpha = .2, fill = "cyan") +
      geom_line(data = a, aes(x, predicted, group = group, linetype = group), size = 1) +
      labs(y = "Revenue Volatility", x = "Catch Diversity") +
      geom_line(size = 1) +
      theme_tufte()
    }
    
p_plot(port_df) +   geom_point(data = input_alt, aes(avg_index_port, log_cv_revenue_adj_port), alpha = .05,
                               inherit.aes = FALSE)
p_plot(port_df_geo)
p_plot(port_df_value)    
  

#potnetial exists to replace dots with contours as a way to combat overplotting and 
  #give a more reasonable sense of where the 'center' of the data is 
  
#  https://www.statworx.com/at/blog/how-not-to-overplot/

    ggplot(boat_df, aes(x, predicted)) + 
    geom_ribbon(data = boat_df, aes(ymin = conf.low, ymax = conf.high), 
                alpha = .2, fill = "cyan") +
    geom_point(data = input_alt, aes(avg_index_boat, log_cv_revenue_adj_boat), alpha = .05) +
    stat_density_2d(data = input_alt, 
                    aes(avg_index_boat, log_cv_revenue_adj_boat, fill = stat(level)),
                    geom = "polygon", stat = "identity") +
    geom_line(size = 1) 
  
#very interesting...
  ggplot(port_df, aes(x, predicted)) + 
    geom_ribbon(data = port_df, aes(ymin = conf.low, ymax = conf.high), 
                alpha = .2, fill = "grey70") +
    labs(y = "Revenue Volatility", x = "Catch Diversity") +
    stat_density_2d(data = input_alt, 
                    aes(avg_index_port, log_cv_revenue_adj_port, fill = ..level..,
                    geom = "polygon")) +
    geom_point(data = input_alt, aes(avg_index_port, log_cv_revenue_adj_port), alpha = .05) +
    geom_line(size = 1) +
    theme_tufte()
  
##summary stats for input_alt and input_agg   
library(psych)
#port summary stats
describe(input_agg)

input_alt %>% 
  dplyr::select(log_cv_revenue_adj_boat, avg_index_boat, avg_index_port, value_cat, mega_subregion) %>% 
describe(.)
  
##prediction table

#generating sample index values
#we want 1, 10/90, 25,75, 25,25,50, 50,50 60,40,

index_examp <- read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/data/index_examples.csv")

examp_total <- index_examp %>% 
  group_by(group) %>% 
  summarise(
    number_total = sum(number))

examp_int <- left_join(index_examp, examp_total, by = "group") %>% 
  mutate(
    temp_index = (number / number_total)^2) %>% 
        group_by(group) %>% 
            mutate(index = 1 / sum(temp_index)) %>% 
              distinct(group, index, description)

examp_int$index

#prediction corresponding to index value

boat_index_table <- ggpredict(clean_boat_lm, 
                    terms = c("avg_index_boat [1, 2.0, 1.219512, 1.60, 2.666667, 3.0, 1.724138, 4.0, 5.0, 6.0, 7.0]"))

boat_index_table


port_index_table <- ggpredict(agg_port, 
                              terms = c("avg_index_port [1, 2.0, 1.219512, 1.60, 2.666667, 3.0, 1.724138, 4.0, 5.0, 6.0, 7.0]"))

port_index_table
