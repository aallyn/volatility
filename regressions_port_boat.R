

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", 
       "gridExtra", "broom", "viridis", "sjPlot", "jtools","ggstance", "MASS", "leaps", "ggeffects")

lapply(x, require, character.only = TRUE)


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
                                       "Northern Mid Atlantic","Southern Mid Atlantic"))) %>% na.omit()


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
  input_agg <- input_alt %>% 
    group_by(port_tidy) %>% 
    mutate(mean_boat_index = weighted.mean(avg_index_boat, avg_value_boat)) %>% 
    distinct(port_tidy, log_cv_revenue_adj_port, avg_index_port, mean_boat_index,
             value_cat_port, mega_subregion) %>% na.omit()

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
  
  #input datasets for vessel level prediction plots
boat_df <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]"))

boat_df_geo <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                  "mega_subregion"))

boat_df_value <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                    "value_cat"))

shelf_boat <-  ggplot(boat_df, aes(x, predicted)) + 
  geom_ribbon(data = boat_df, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, fill = "grey70") +
  labs(y = "Revenue Volatility", x = "Catch Diversity") +
  geom_point(data = input_alt, aes(avg_index_boat, log_cv_revenue_adj_boat), alpha = .05) +
  geom_line(size = 1) +
  theme_tufte()

geo_boat <- ggplot(boat_df_geo, aes(x, predicted, group = group)) + 
  geom_ribbon(data = boat_df_geo, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .2, fill = "grey70") + 
  labs(y = "Revenue Volatility", x = "Catch Diversity") +
  geom_line(data = boat_df_geo, aes(x, predicted, group = group, linetype = group), size = 1) +  
  theme_tufte() + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Sub Region"))

value_boat <- ggplot(boat_df_value, aes(x, predicted, group = group)) + 
  geom_ribbon(data = boat_df_value, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .3,  fill = "grey70") + 
    geom_line(data = boat_df_value, aes(x, predicted, group = group, linetype = group), size = 1) +
  labs(y = "Revenue Volatility", x = "Catch Diversity") +
  theme_tufte() +
  theme(legend.position = "bottom") +
      guides(fill=guide_legend(title="Value Category"))


boat_predict_temp <- grid.arrange(shelf_boat, geo_boat, value_boat, nrow = 3)


ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/boat_predict_temp.jpg", boat_predict_temp)
#two step process, first create small fig fo rprojections then align it with bigger figure 

#input datasets for port level prediction plots
port_df <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]"))
port_df_geo <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                  "mega_subregion"))

port_df_value <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                    "value_cat_port"))

#port plots

shelf_port <-  ggplot(port_df, aes(x, predicted)) + 
  geom_ribbon(data = port_df, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, fill = "grey70") +
  labs(y = "Revenue Volatility", x = "Catch Diversity") +
  geom_point(data = input_alt, aes(avg_index_port, log_cv_revenue_adj_port), alpha = .05) +
  geom_line(size = 1) +
  theme_tufte()

geo_port <- ggplot(port_df_geo, aes(x, predicted, group = group)) + 
  geom_ribbon(data = port_df_geo, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .2, fill = "grey70") + 
  labs(y = "Revenue Volatility", x = "Catch Diversity") +
  geom_line(data = port_df_geo, aes(x, predicted, group = group, linetype = group), size = 1) +  
  theme_tufte() + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Sub Region"))

value_port <- ggplot(port_df_value, aes(x, predicted, group = group)) + 
  geom_ribbon(data = port_df_value, aes(ymin = conf.low, ymax = conf.high, group = group), 
              alpha = .3,  fill = "grey70") + 
  geom_line(data = port_df_value, aes(x, predicted, group = group, linetype = group), size = 1) +
  labs(y = "Revenue Volatility", x = "Catch Diversity") +
  theme_tufte() +
  theme(legend.position = "bottom") +
  guides(fill=guide_legend(title="Value Category"))


port_predict_temp <- grid.arrange(shelf_port, geo_port, value_port, nrow = 3)

ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/port_predict_temp.jpg", port_predict_temp)



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
  