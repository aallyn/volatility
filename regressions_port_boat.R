

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", 
       "gridExtra", "broom", "viridis", "sjPlot", "jtools","ggstance", "MASS", "leaps", "ggeffects")

lapply(x, require, character.only = TRUE)


input_alt <- read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input_temp.csv") %>% 
  mutate(
    log_boat_index = log(avg_index_boat),
    value_cat = factor(value_cat, 
                       levels = c("$5,000-$100,000","$100,000-$500,000", 
                                  "$500,000-$1,000,000", "> $1,000,000")),
    mega_subregion = factor(mega_subregion, 
                            levels = c("Northern New England", "Southern New England",
                                       "Northern Mid Atlantic","Southern Mid Atlantic")))

#BOAT REG

library(caret)

bc_boat <- BoxCoxTrans(input_alt$avg_index_boat)

data.frame(bc_boat)





#model selection 
leaps_clean_boat <- regsubsets(log_cv_revenue_adj_boat ~ avg_index_boat + I(avg_index_boat^2) +
                                 avg_index_port + value_cat + mega_subregion, input_alt, nbest = 5)

plot(leaps_clean_boat, scale = "bic")
#regression: output, table, and diagnostics 

  clean_boat <- lm(log_cv_revenue_adj_boat ~ avg_index_port + log_boat_index + I(log_boat_index^2) + value_cat + mega_subregion, 
                    data = input_alt)
  
  clean_boat_lm <- lm(log_cv_revenue_adj_boat ~ avg_index_port + avg_index_boat + I(avg_index_boat^2) + 
                        I(avg_index_boat^3) + value_cat + mega_subregion, data = input_alt)
  
  clean_boat_lm_loglog <- lm(log_cv_revenue_adj_boat ~ avg_index_port + log_boat_index + I(log_boat_index^2) + 
                        I(log_boat_index^3) + value_cat + mega_subregion, data = input_alt)
  
  
    summary(clean_boat_lm)
  
  plot(clean_boat_lm)
  
  sample <- predict(clean_boat_lm, type = "response")
  

  
  ##generating preduction friendly data frame 
  boat_df <- ggpredict(clean_boat_lm, terms = c("avg_index_boat"))

  boat_loglog <- ggpredict(clean_boat_lm_loglog, terms = c("log_boat_index"))
  #plotting
a <-   ggplot(boat_df, aes(x, predicted)) + 
    geom_ribbon(data = boat_df, aes(ymin = conf.low, ymax = conf.high), 
                alpha = .2, fill = "red") +
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
  tab_model(clean_boat, title = "Vessel Level Regression Ouput (p-value: < 2.2e-16)",
            show.p = TRUE)
  #reg results visual
  plot_model(clean_boat, show.values = TRUE, value.offset = .3)
  
  #anova table
  anov_boat <- anova(clean_boat)
  dfr_boat <- data.frame(anov_boat)
  kable(dfr_boat, caption="Anova table for the basic *port* model")
  
  #diagnostics
  plot(clean_boat)
  
#PORT REG
  
  #FIX dublicate observations 
  #aggregating data 
  #aggregated version of port data-set, vessel diversity is summarised by weighted mean 
  input_agg <- input_alt %>% 
    group_by(port_tidy) %>% 
    mutate(mean_boat_index = weighted.mean(avg_index_boat, avg_value_boat)) %>% 
    distinct(port_tidy, log_cv_revenue_adj_port, avg_index_port, mean_boat_index,
             value_cat_port, mega_subregion) %>% na.omit()
  
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
  plot_model(agg_port, show.values = TRUE, value.offset = .3)
  
  
  #diagnostics
  plot(agg_port)
  