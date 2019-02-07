

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", 
       "gridExtra", "broom", "viridis", "sjPlot", "jtools",
       "ggstance", "MASS", "leaps", "ggeffects",
       "hrbrthemes", "cowplot")

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
      mega_subregion == "Southern Mid Atlantic" ~ "S. Mid Atlantic"),
    value_cat_alt = case_when(
      value_cat == "$5,000-$100,000" ~ "$5K - 100K",
      value_cat == "$100,000-$500,000" ~ "$100K - 500K",
      value_cat == "> $500,000" ~ "> $500K"),
    value_cat_port_alt = case_when(
      value_cat_port == "$15,000 - 500,000" ~ "$15K - 500K",
      value_cat_port == "$500,000-$3,000,000" ~ "$500K - 3,000K",
      value_cat_port == "> $3,000,000" ~ "> $3,000K")) %>% 
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


#BOAT REG

#model selection 
leaps_clean_boat <- regsubsets(log_cv_revenue_adj_boat ~ avg_index_boat + I(avg_index_boat^2) +
                                 avg_index_port + value_cat + mega_subregion_alt, input_alt, nbest = 5)

plot(leaps_clean_boat, scale = "bic")
#regression: output, table, and diagnostics 

  clean_boat <- lm(log_cv_revenue_adj_boat ~ avg_index_port + log_boat_index + I(log_boat_index^2) + value_cat_alt + mega_subregion_alt, 
                    data = input_alt)
  
  #adding cubic term
  clean_boat_lm <- lm(log_cv_revenue_adj_boat ~ avg_index_port + avg_index_boat + I(avg_index_boat^2) + 
                        I(avg_index_boat^3) + value_cat_alt + mega_subregion_alt, data = input_alt)
  
  #adding cubic term and logging index variable
  clean_boat_lm_loglog <- lm(log_cv_revenue_adj_boat ~ avg_index_port + log_boat_index + I(log_boat_index^2) + 
                        I(log_boat_index^3) + value_cat_alt + mega_subregion, data = input_alt)
  
  #comparing the two model specifications
  summary(clean_boat_lm)
    
  summary(clean_boat_lm_loglog)
  
  #seems much betta! 
  plot(clean_boat_lm)
  
  plot(clean_boat_lm_loglog)
  

  ##generating preduction friendly data frame 
  library(ggeffects)
  boat_df <- ggpredict(clean_boat_lm, terms = c("avg_index_boat"))

  boat_loglog <- ggpredict(clean_boat_lm_loglog, terms = c("log_boat_index"))

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
             value_cat_port_alt, mega_subregion_alt) %>% 
                filter(!port_tidy == "OCEANCITY_MD") %>% 
                  na.omit() 

#model selection 
  leaps_clean_port <- regsubsets(log_cv_revenue_adj_port ~ avg_index_port + mean_boat_index + 
                                   value_cat_port + mega_subregion, input_agg, nbest = 5)
  
  plot(leaps_clean_port, scale = "bic")
  
#regression form  
agg_port <- lm(log_cv_revenue_adj_port ~ avg_index_port + mean_boat_index + 
                   value_cat_port_alt + mega_subregion_alt, input_agg)  
  
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

#### boat prediction dataset for figure generation
  
  #generating values to smooth line
number <- toString(seq(1, 8, by = .2))
  

geo_stuff <- input_alt %>% dplyr::distinct(mega_subregion_alt, mega_subregion)
  
  #input datasets for vessel level prediction plots
boat_df <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]"))
  
boat_df_geo <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                  "mega_subregion_alt"))

boat_df_value <- ggpredict(clean_boat_lm, terms = c("avg_index_boat [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                    "value_cat_alt"))

#port prediction dataset for figure generation
port_df <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]"))
port_df_geo <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                  "mega_subregion_alt"))
              
port_df_value <- ggpredict(agg_port, terms = c("avg_index_port [1, 1.2, 1.4, 1.6, 1.8, 2, 2.2, 2.4, 2.6, 2.8, 3, 3.2, 3.4, 3.6, 3.8, 4, 4.2, 4.4, 4.6, 4.8, 5, 5.2, 5.4, 5.6, 5.8, 6, 6.2, 6.4, 6.6, 6.8, 7, 7.2, 7.4, 7.6, 7.8, 8]",
                                                    "value_cat_port_alt"))
