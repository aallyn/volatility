
# clean regression script: boats, ports, then all variables
  # vessel results seem def 'hill shaped.' Port results are not significant 
  # using same form as boats. As an alterantive, port diversity index is used 
  # to predict vessel level CV
  
  # stepwise regression IDs full regresion form as best fit  

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", "gridExtra", "broom", "viridis",
       "sjPlot", "jtools","ggstance")

lapply(x, require, character.only = TRUE) ##applying the function require to each point in the vector x


require(sjPlot) #loading sjPlot, did not load above for some reason

#reading in analysis data 
input <- read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input.csv")

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
                      avg_log_value + log(port_value) + mega_subregion, input)
    
    #full regression results table
    sjt.lm(full_reg,  emph.p = TRUE, digits.est = 3)

    #full regression results plotted
    plot_model(full_reg)
    
  #using leaps to 'select' best model
    #picking best model via an all-subsets regression using the leaps() package
    #note the 'nbest' selects the top x models, here it is 10

  leaps <- regsubsets(cv_revenue ~ avg_index + I(avg_index^2) + port_index + I(port_index^2) + 
                        avg_log_value + log(port_value) + mega_subregion, data = input, nbest = 5)
    
    #results from all-subsets regression (LOOK AT PLOT)
    summary(leaps)
    
    ##plot a table of models, models are ordered based on stat of interest (i.e. r2, aic, bic)
    plot(leaps, scale = "r2")
  
        #looks like full model has biggest affect? 