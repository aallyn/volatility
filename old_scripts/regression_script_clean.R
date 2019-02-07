
# clean regression script: boats, ports, then all variables
  # vessel results seem def 'hill shaped.' Port results are not significant 
  # using same form as boats. As an alterantive, port diversity index is used 
  # to predict vessel level CV
  
  # stepwise regression IDs full regresion form as best fit  

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", "gridExtra", "broom", "viridis",
       "sjPlot", "jtools","ggstance", "MASS", "leaps", "ggeffects")

lapply(x, require, character.only = TRUE) ##applying the function require to each point in the vector x

#reading in analysis data 
input <- read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input.csv")
input_alt <- read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input_temp.csv") %>% 
  mutate(
  value_cat = factor(value_cat, 
                     levels = c("$5,000-$100,000","$100,000-$500,000", 
                                "$500,000-$1,000,000", "> $1,000,000")),
  mega_subregion = factor(mega_subregion, 
                        levels = c("Northern New England", "Southern New England",
                                   "Northern Mid Atlantic","Southern Mid Atlantic")))
  

#assessing vol - catch div relationship for boats and ports using GAM via ggplot2
ggplot(input_alt, aes(avg_index_boat, log_cv_revenue_adj_boat)) + geom_point() + 
  geom_smooth() +
    labs(x = "Revenue Volatility", y = "Catch Diversity Index") +
      ggtitle("Boat Level Relationship")

ggplot(input_alt, aes(avg_index_port, log_cv_revenue_adj_port)) + geom_point() + 
  geom_smooth() +
  labs(x = "Revenue Volatility", y = "Catch Diversity Index") +
    ggtitle("Port Level Relationship")

ggplot(input_alt, aes(avg_index_port, log_cv_revenue_adj_boat)) + geom_point() + 
  geom_smooth() +
  ggtitle("Port Level Relationship")

    #using intuition from gam to assess goodness of fit for linear or quadratic
      #quadratic wins for boats
    boat_linear <- lm(log_cv_revenue_adj_boat ~ avg_index_boat, input_alt)
        ## r squared = 0.003033
    boat_quadratic <- lm(log_cv_revenue_adj_boat ~ avg_index_boat + + I(avg_index_boat^2), input_alt)
        ## r squared = 0.04271, order of magnitude better 
    summary(boat_quadratic)
    #quadratic and linear are quite similar, for now going with linear term
    port_linear <- lm(log_cv_revenue_adj_port ~ avg_index_port, input_alt)
    ## r squared = 0.149
    port_quadratic <- lm(log_cv_revenue_adj_port ~ avg_index_port + + I(avg_index_port^2), input_alt)
    ## r squared = 0.1611, order of magnitude better 


# boat-level regressions: ####
  #scope is lme --> geo --> operation size#### 

  #lme-scale
  lme_boat <- lm(cv_revenue ~ avg_index + I(avg_index^2) + avg_log_value, 
                  input)
  
  #geo-level regressions
  geo_boat <- input %>% group_by(mega_subregion) %>% 
    do(model = lm(cv_revenue ~ avg_index + I(avg_index^2) + avg_log_value,.)) 
      
      #readable reg outputs
      tidy_geo_boat <- geo_boat %>% tidy(model)
  
  #operation-level regreesions    
  value_boat <- input %>% group_by(value_cat) %>% 
    do(model = lm(cv_revenue ~ avg_index + I(avg_index^2) + avg_log_value,.))
      
      #readable reg outputs
      tidy_value_boat <- value_boat %>% tidy(model)
      
# port - level regressions: #### 
  #scope is lme --> geo 

  #reducing dataset to include only ports           
  input_port <- input %>% distinct(port_tidy, port_cv_revenue, port_index, port_value, mega_subregion)

  #lme-scale -- not sig!
  lme_port <- lm(port_cv_revenue ~ port_index + I(port_index^2) + port_value, 
                 input_port)

  #geo-scale -- not sig!
  geo_port <- input_port %>% group_by(mega_subregion) %>% 
    do(model = lm(port_cv_revenue ~ port_index + I(port_index^2) + port_value,.))
  
      #readable reg outputs 
      tidy_geo_port <- geo_port %>% tidy(model)
      
  #alternative: using port diversity to predict vessel diversity 
  lme_port_alt_quad <- lm(port_cv_revenue ~ port_index + I(port_index^2) + port_value, 
                     input)
  
  lme_port_alt <- lm(port_cv_revenue ~ port_index + I(port_index^2) + port_value, 
                    input)
    
    #printing results...quadratic form does not look important 
    sjt.lm(lme_port_alt_quad,  emph.p = TRUE, digits.est = 3)
    sjt.lm(lme_port_alt,  emph.p = TRUE, digits.est = 3)
      
# master regresion: motivation is to get at the effect of each variable without sub-sampling. 
                  #in addition we want to bring in port diversity as well, are diverse ports 
                  #made up of diverse or specialized operators? 
      
                  #we want to pick the best form for this multiple regression, this is asasisted by
                  #the MASS / leaps package
      
    #stepwise regresssion package
      library(leaps)
    #all-subsets regression package
      library(MASS)
      
    #Tutorial using both: https://www.statmethods.net/stats/regression.html

  #proto-master script  
  full_reg <- lm(cv_revenue ~ avg_index + I(avg_index^2) + port_index + I(port_index^2) + 
                      log(port_value) + mega_subregion + value_cat, input)

  full_reg_novalue <- lm(cv_revenue ~ avg_index + I(avg_index^2) + port_index + I(port_index^2) + 
                           log(port_value) + log(avg_value) + mega_subregion , input)
  
  reg_alt <- lm(log_cv_revenue_adj_boat ~ avg_index_boat + + I(avg_index_boat^2) +
                       avg_index_port + value_cat + mega_subregion, input_alt)
  
  full_reg_alt <- lm(log_cv_revenue_adj_boat ~ avg_index_boat + + I(avg_index_boat^2) +
                       avg_index_port + avg_value1000_boat + avg_value1000_port + 
                       + value_cat + mega_subregion, input_alt)
  
  lm(log_cv_revenue_adj_port ~ avg_index_boat + + I(avg_index_boat^2) +
       avg_index_port + avg_value1000_boat + avg_value1000_port + 
       + value_cat + mega_subregion, input_alt)
  
    #full regression results table
    sjt.lm(full_reg,  emph.p = TRUE, digits.est = 3)
    
    sjt.lm(reg_alt,  emph.p = TRUE, digits.est = 5)    

    #full regression results plotted
    plot_model(full_reg_alt)
    
  #using leaps to 'select' best model
    #picking best model via an all-subsets regression using the leaps() package
    #note the 'nbest' selects the top x models, here it is 10
  
    leaps_alt_boat <- regsubsets(log_cv_revenue_adj_boat ~ avg_index_boat + I(avg_index_boat^2) +
                              avg_index_port + avg_value1000_boat + avg_value1000_port + 
                              + value_cat + mega_subregion, input_alt, nbest = 5)
    
    leaps_alt_port <- regsubsets(log_cv_revenue_adj_port ~ avg_index_boat +
                                   avg_index_port + avg_value1000_port + 
                                   + mega_subregion, input_alt, nbest = 5)
    #leaps with 'ideal' variable set
    leaps_clean_boat <- regsubsets(log_cv_revenue_adj_boat ~ avg_index_boat + I(avg_index_boat^2) +
                                   avg_index_port + value_cat + mega_subregion, input_alt, nbest = 5)
    
    leaps_clean_port <- regsubsets(log_cv_revenue_adj_port ~ avg_index_port + avg_index_boat + 
                                     value_cat_port + mega_subregion, input_alt, nbest = 5)
    
    plot(leaps_clean_boat, scale = "r2")
    
    plot(leaps_clean_port, scale = "r2") ##full model looks good, maybe add in value category??
    
full_reg_port <-lm(log_cv_revenue_adj_port ~ avg_index_boat +
      avg_index_port + avg_value1000_port + 
      + mega_subregion, input_alt)

    plot_model(full_reg_port)
    
    
  leaps <- regsubsets(cv_revenue ~ avg_index + I(avg_index^2) + port_index + I(port_index^2) + 
                        value_cat + log(port_value) + mega_subregion, data = input, nbest = 5)
  
  leaps_novalue <- regsubsets(cv_revenue ~ avg_index + I(avg_index^2) + port_index + I(port_index^2) + 
                                log(port_value) + log(avg_value) + mega_subregion, data = input, nbest = 5)
  
    #results from all-subsets regression (LOOK AT PLOT)
    summary(leaps)
    summary(leaps_novalue)
   
    
    ##plot a table of models, models are ordered based on stat of interest (i.e. r2, aic, bic)
    plot(leaps, scale = "r2")
    
    plot(leaps_novalue, scale = "r2")
    
        #looks like full model has biggest affect? 
#coefficient direction 
plot_summs(full_reg, scale = TRUE, plot.distributions = TRUE)
    
# quick prediciton outputs via the effect_plot statement from sjtools 
    
boat <- effect_plot(full_reg, pred = port_index, interval = TRUE)
    
port <- effect_plot(full_reg, pred = avg_index, interval = TRUE)    

value <- effect_plot(full_reg, pred = avg_log_value, interval = TRUE)
    
  grid.arrange(boat,port, value, nrow=3)


##using package all effects 
  
library(effects)  
  
alleffects_fullreg <- allEffects(full_reg)  

# crude plotting of all variables
plot(alleffects_fullreg)

library(ggeffects)
data("efc")

#regression from dataset, on first glance only c12hour is a continuous variable
fit <- lm(barthtot ~ c12hour + neg_c_7 + c161sex + c172code, data = efc)

ggpredict(fit, terms = "c12hour")



##this easily incorporates into ggplot: 
mydf <- ggpredict(fit, terms = "c12hour")
ggplot(mydf, aes(x, predicted)) + geom_line()

##you can do this at different levels (i.e. category variables)

ggpredict(fit, terms = c("c12hour", "c172code"))

mydf <- ggpredict(fit, terms = c("c12hour", "c172code"))
ggplot(mydf, aes(x, predicted, colour = group)) + geom_line()

##trying the same but for our working but weird!!!!, faceting def way to go,
##i can do my other stuff here now to, like the sample poylgons and rippons
x <- seq(from = 1, to = 8, by = 0.1)

#so ugly!!! but whatever for now
x_mod <- paste(x, collapse = ", ")

mydata <- ggpredict(full_reg, 
                    terms = c("avg_index
[1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 
2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 
3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4, 
4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5, 
5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 6, 
6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 7, 
7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8]", "mega_subregion"))

mydata_value <- ggpredict(full_reg, 
                    terms = c("avg_index
[1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 
2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 
3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4, 
4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5, 
5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 6, 
6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 7, 
7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8]", "value_cat"))

ggplot(mydata_value, aes(x, predicted)) + geom_line() +
  facet_wrap(~group)

ggplot(mydata, aes(x, predicted)) + geom_line() +
  facet_wrap(~group)


input %>% group_by(value_cat) %>% summarise(
  mean_cv = mean(cv_revenue))

#Parallel regressions, selection and prediction##### 

#model selection
leaps_clean_boat <- regsubsets(log_cv_revenue_adj_boat ~ avg_index_boat + I(avg_index_boat^2) +
                                 avg_index_port + value_cat + mega_subregion, input_alt, nbest = 5)

leaps_clean_port <- regsubsets(log_cv_revenue_adj_port ~ avg_index_port + avg_index_boat + 
                                 value_cat_port + mega_subregion, input_alt, nbest = 5)
  #model selection plotting
plot(leaps_clean_boat, scale = "r2")

plot(leaps_clean_port, scale = "r2") ##full model looks good, maybe add in value category??

clean_boat <- lm(log_cv_revenue_adj_boat ~ avg_index_port + value_cat + mega_subregion + 
                    avg_index_boat + I(avg_index_boat^2), data = input_alt)

clean_port <- lm(log_cv_revenue_adj_port ~ avg_index_port + avg_index_boat + 
                   value_cat_port + mega_subregion, input_alt)

sjt.lm(clean_boat,  emph.p = TRUE, digits.est = 5) 


sample <- predict(clean_boat, type = "response")


##generating preduction friendly data frame 
boat_df <- ggpredict(clean_boat, terms = c("avg_index_boat"))
boat_df_value <- ggpredict(clean_boat, terms = c("avg_index_boat", "value_cat"), full.data = FALSE)
boat_df_geo <- ggpredict(clean_boat, terms = c("avg_index_boat", "mega_subregion"))

port_df <- ggpredict(clean_port, terms = c("avg_index_port"))
port_df_value <- ggpredict(clean_port, terms = c("avg_index_port", "value_cat_port"))
port_df_geo <- ggpredict(clean_port, terms = c("avg_index_port", "mega_subregion"))


#shelf wide...way quicker return to decrease in cv tan before
shelf_boat <- ggplot(boat_df, aes(x, predicted)) + 
  geom_ribbon(data = boat_df, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, fill = "red") +
  geom_point(data = input_alt, aes(avg_index_boat, log_cv_revenue_adj_boat), alpha = .05) +
  geom_line(size = 1) +
    theme_tufte()


value_boat <- ggplot(boat_df_value, aes(x, predicted)) + geom_line() +
  geom_ribbon(data = boat_df_value, aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  facet_wrap(~group) +
  theme_tufte()


geo_boat <- ggplot(boat_df_geo, aes(x, predicted)) + geom_line() +
  geom_ribbon(data = boat_df_geo, aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  facet_wrap(~group) +
    theme_tufte()

#linear, negative trend...hopefully we can defend!!!
shelf_port <- ggplot(port_df, aes(x, predicted)) + 
  geom_ribbon(data = port_df, aes(ymin = conf.low, ymax = conf.high), 
              alpha = .2, fill = "red") +
  geom_point(data = input_alt, aes(avg_index_port, log_cv_revenue_adj_port), alpha = .1) +
  geom_line() + theme_tufte()

value_port <- ggplot(port_df_value, aes(x, predicted)) + geom_line() +
  geom_ribbon(data = port_df_value, aes(ymin = conf.low, ymax = conf.high), alpha = .2) +
  facet_wrap(~group) + theme_tufte()

geo_port <- ggplot(port_df_geo, aes(x, predicted)) + geom_line() +
  facet_wrap(~group) + theme_tufte()

#inquiry into average cv levels by subregion

input_alt %>%
  group_by(mega_subregion) %>% 
  summarise(
  mean_cv = mean(log_cv_revenue_adj_port))


#boat figure 
plot_model(clean_boat, show.values = TRUE, value.offset = .3)

plot_model(clean_port, show.values = TRUE, value.offset = .3)

grid.arrange(shelf_boat, value_boat, geo_boat, nrow =2)

grid.arrange(shelf_port, value_port, geo_port, nrow =2)




# marginal effects for polynomial terms
data(efc)
fit <- glm(
  tot_sc_e ~ c12hour + e42dep + e17age + I(e17age^2) + I(e17age^3),
  data = efc,
  family = poisson()
)

ggeffect(fit, terms = c("e17age", "e42dep")) %>% 
ggplot(., aes(x, predicted)) + geom_line() + 
  facet_wrap(~group)


clean_boat <- glm(log_cv_revenue_adj_boat ~ avg_index_port + value_cat + mega_subregion + 
                    avg_index_boat + I(avg_index_boat^2), 
                  data = input_alt,
                  family = binomial())

boat_df_value <- ggpredict(clean_boat, terms = c("avg_index_boat", "value_cat"))


  ggplot(data = boat_df_value, aes(x, predicted)) +
    geom_point() +
    geom_smooth() +
  facet_wrap(~group, nrow = 1)

View(predict)




clean_boat <- glm(log_cv_revenue_adj_boat ~ avg_index_port + value_cat + mega_subregion + 
                    avg_index_boat + I(avg_index_boat^2), 
                  data = input_alt)    

dumb <- ggeffect(clean_boat, terms = c("avg_index_boat + I(avg_index_boat^2)", "value_cat"))  

ggplot(dumb, aes(x, predicted, group = group, colour = group)) + 
  geom_line() +
  geom_point(alpha = .1) 

dumber <- ggeffect(clean_boat, terms = c("avg_index_boat + I(avg_index_boat^2)
[1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 
2.1, 2.2, 2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3, 
3.1, 3.2, 3.3, 3.4, 3.5, 3.6, 3.7, 3.8, 3.9, 4, 
4.1, 4.2, 4.3, 4.4, 4.5, 4.6, 4.7, 4.8, 4.9, 5, 
5.1, 5.2, 5.3, 5.4, 5.5, 5.6, 5.7, 5.8, 5.9, 6, 
6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9, 7, 
7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8]
                                         ", "mega_subregion")) %>% 
  mutate(
  group = factor(group, 
                          levels = c("Northern New England", "Southern New England",
                                     "Northern Mid Atlantic","Southern Mid Atlantic")))



dumber_full<- ggeffect(clean_boat, terms = c("avg_index_boat + I(avg_index_boat^2)", "mega_subregion"),
                       full.data = TRUE)  


central_poly <- function(a, b)
{
  tibble(
    x = c(quantile(a, .90), 
          quantile(a, .90),
          quantile(a, .10), 
          quantile(a, .10)),
    
    y = c(quantile(b, .90), 
          quantile(b, .10),
          quantile(b, .10),
          quantile(b, .90)
    ))
}


trial <-  input_alt %>% group_by(mega_subregion) %>% 
  do(data.frame(x = central_poly(.$avg_index_boat, .$log_cv_revenue_adj_boat))) %>% 
  select(group = mega_subregion, x = x.x, predicted = x.y)

dots_boat <- input_alt %>% 
  distinct(HULLNUM, group = mega_subregion, value_cat, x = avg_index_boat, 
           predicted = log_cv_revenue_adj_boat)

mins <- dumber %>% group_by(group) %>% 
  filter(predicted == min(predicted)) %>% 
  mutate(predicted = round(predicted, 2))
  
  max <- dumber %>% group_by(group) %>% 
  filter(predicted == max(predicted)) %>% 
    mutate(predicted = round(predicted, 2))

start <- dumber %>% group_by(group) %>% 
  filter(x == min(x)) %>% 
  mutate(predicted = round(predicted, 2))

summarise(
  max = max(log_cv_revenue_adj_boat),
  min = min(log_cv_revenue_adj_boat),
  begin = min(avg_index_boat))

##need to figure out the max / min values here 
ggplot(dumber, aes(x, predicted)) +
  facet_grid(group ~ ., scales = "free_y") + 
  geom_polygon(data = trial, aes(x=x, y=predicted), 
              colour = "black", fill= "NA")+
  geom_ribbon(data = dumber, aes(ymin = conf.low, max = conf.high), 
              alpha = .4, fill = "blue") +
  geom_point(data = dots_boat, aes(x, predicted), alpha = .04) +
  geom_line(size = 1) +
  geom_point(data = mins, aes(x = x, y =predicted), col = "blue") +
  geom_point(data = max, aes(x = x, y =predicted), col = "red") +
  geom_point(data = start, aes(x = x, y =predicted), col = "black") +
  geom_text(data = mins, aes(label = predicted), vjust = 0) +
  geom_text(data = max, aes(label = predicted), vjust = -1.3) +
  geom_text(data = start, aes(label = predicted), hjust = 0, nudge_x = -.6) +
  geom_text(data = start, aes(label = group), hjust = 0, nudge_x = 5) +
  theme_tufte() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title = element_blank())

ggplot(dumber, aes(x, predicte, group = group)) + 
  geom_line()
  





