
input <- as_tibble(read.csv("C:/Users/bkennedy/Dropbox/COCA/Volatility Diversity_Project/redo/boat_analysis_input.csv")) %>% 
    mutate(
      value_cat = factor(value_cat,levels = c("$5,000-$50,000","$50,000-$100,000", "$100,000-$500,000",
                                              "$500,000-$1,000,000","> $1,000,000")),
      mega_subregion = factor(mega_subregion, 
                                       levels = c("Southern Mid Atlantic", "Northern Mid Atlantic", 
                                                  "Southern New England", "Northern New England")))

#GOAL: picking best model via stepwise or all-subsets approach


#stepwise regresssion package
library(leaps)
#all-subsets regression package
library(MASS)

  #Tutorial using both + good description 
  #https://www.statmethods.net/stats/regression.html

#full regression, everything thrown in!!!

andrews <- lm(cv_revenue ~ avg_index + I(avg_index^2) + port_avg_index + I(port_avg_index^2) + 
     avg_log_value + log(port_avg_value) + mega_subregion, input)

  #diagnostic plots
  plot(andrews)


##conventional regression output  
require(sjPlot)

sjt.lm(andrews,  emph.p = TRUE, digits.est = 3)


#picking best model via a stepwise regression package stepAIC

step <- stepAIC(andrews, directions = "both")
  
  #results from stepwise regression 
  step$anova

#picking best model via an all-subsets regression using the leaps() package
    #note the 'nbest' selects the top x models, here it is 10

leaps <- regsubsets(cv_revenue ~ avg_index + I(avg_index^2) + port_avg_index + I(port_avg_index^2) + 
     avg_log_value + log(port_avg_value) + mega_subregion, data = input, nbest = 5)

  #results from all-subsets regression
  summary(leaps)

  ##plot a table of models, models are ordered based on stat of interest (i.e. r2, aic, bic)
  plot(leaps, scale = "r2")
  
  #the best model has everything but the southern new england dummy, however a bunch of different iterations arte clustered around
  # anm r2 of .22. Need help on this model process perhaps, or should we switch the explanator??? I think I should proceed as planned and
  # first scope the port models...
  
  



