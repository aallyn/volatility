###provisional combo script 

#CFDERS RAW 
  # BOAT INDEX CALC > PRIMARY PORT > BOAT VARIABLE SUMMARY
  # PORT INDEX CALC > ports filtered by PRIMARY PORT > PORT VARIABLE SUMMARY
    #joined together to CREATE BOAT PORT INPUT
      #exported as boat_port_input.csv

x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", "gridExtra", "broom", "viridis",
       "sjPlot", "jtools","ggstance")

lapply(x, require, character.only = TRUE) ##applying the function require to each point in the vector x

#IMPORTING geo INFO
geo_table <- as_tibble(read.csv("C:/Users/brian/Dropbox/COCA/DATA/GMRI_aggregated data/geo_table.csv", header = TRUE))
  
#importing raw cfders_data and processing
    #understanding is that this cfders is a. infaltion adjusted, and b. sourced from OG SAS script which needs to 
    #be imported into R 
    #current location of SAS script: \\LAZ\Shared\Research\2015_2018_COCA\2_Data\IMPLAN\study area data\sunion of the cfders.egp
        #PROGRAM trimming CFDERS
cfders_raw <- as_tibble(read.csv("C:/Users/brian/Dropbox/COCA/DATA/GMRI_aggregated data/cfders/cfders_div.csv", header = TRUE)) %>% 
  filter(PORT_STATE %in% c('ME', 'NH', 'MA', 'RI', 'CT', 
                                        'NY', 'NJ', 'DE', 'MD', 'VA', 'NC')) %>% 
     mutate( mega_subregion = case_when(                                 
      PORT_STATE %in% c('ME', 'NH') ~ 'Northern New England',
      PORT_STATE %in% c('MA') ~ 'Southern New England',
      PORT_STATE %in% c('RI', 'CT', 'NY') ~ 'Northern Mid Atlantic',
      PORT_STATE %in% c('NJ', 'DE', 'MD', 'VA', 'NC') ~ 'Southern Mid Atlantic'),
      mega_subregion = factor(mega_subregion, 
                              levels = c("Northern New England", "Southern New England",
                                         "Northern Mid Atlantic","Southern Mid Atlantic"))) %>% 
        left_join(., geo_table, by = c("PORT" = "PORT_CODE")) %>% 
          select(year = YEAR, port_tidy, port_code = PORT, mega_subregion, HULLNUM, VESSEL_NAME, value, spp) %>% 
            na.omit()

#port / subregion list
port_region <- cfders_raw %>% 
  distinct(port_tidy, mega_subregion)
    
##INDEX CALC -- Vessel ####
  #boat * year
  boat_year <- cfders_raw %>% 
            group_by(year, HULLNUM) %>% 
               mutate(total_value = sum(value)) %>% 
                distinct(year, HULLNUM, total_value) %>% 
                  arrange(HULLNUM, year, total_value)

  #spp * boat * year
  spp_boat_year <- cfders_raw %>% 
              group_by(year, spp, HULLNUM) %>% 
                 mutate(value = sum(value)) %>% 
                    distinct(year, spp, HULLNUM, value) %>% 
                       arrange(HULLNUM, year, value)
  
  #index calculation 
  boat_index_calc <- left_join(boat_year, spp_boat_year, 
                      by = c("year" = "year", "HULLNUM" = "HULLNUM")) %>% 
                        mutate(temp_index = (value / total_value)^2) %>% 
                          group_by(year, HULLNUM) %>% 
                            mutate(index = 1/ sum(temp_index)) %>% 
                              distinct(year, HULLNUM, total_value, index) %>% 
                              arrange(HULLNUM, year) %>% na.omit() 

#PRIMARY PORT CALCULATION -- Vessel #### 
  #boat
  boat_total <- boat_year %>%
    group_by(HULLNUM) %>% 
      summarise(total_boat = sum(total_value))
  
  #boat * port
  boat_port <- cfders_raw %>%
    group_by(HULLNUM, port_tidy) %>% 
      summarise(port_boat = sum(value))
  
  #primary port 
  primary_port <- left_join(boat_port, boat_total, by = "HULLNUM") %>% 
    mutate(
      proportion = port_boat / total_boat,
      pport = ifelse(proportion == max(proportion), "primary", "secondary")) %>% 
        left_join(., port_region, by = c("port_tidy" = "port_tidy")) %>% 
    select(HULLNUM, port_tidy, proportion, pport, mega_subregion) %>% filter(pport == "primary")
  
  
##vARIABLE CREATION -- vESSEL ####  
  #creating short list
  unkown_boat <- sqldf("select distinct HULLNUM from primary_port where HULLNUM like '%9999%'")
  
  boat_processing <- left_join(boat_index_calc, primary_port, by = "HULLNUM") %>% 
    group_by(HULLNUM) %>% 
      mutate(
        log_total_value = log(total_value),
        avg_value = mean(total_value),
        yr_cnt = n_distinct(year),
        growth = ((total_value - lag(total_value)) / lag(total_value))*100) %>% 
          ungroup() %>% 
            filter(yr_cnt > 5, ##at least half the data duration
              avg_value > 5000, ##at least 5k a year on avg 
              !HULLNUM == "", !HULLNUM %in% unkown_boat$HULLNUM, ###eliminating the unkown boat categories
              !HULLNUM == "FROM_SHORE", ###eliminating the 'FROM_SHORE CATEGORIES
              !HULLNUM == "000000") %>% 
                na.omit() ###eliminates NA rows, i.e. the first year for growth rate
  
  #boat input dataset
  boat_analysis_summary <- boat_processing %>% 
    group_by(HULLNUM) %>% 
    mutate(
      avg_value = mean(total_value),
      avg_value1000_boat = avg_value / 1000,
      avg_log_value = mean(log(total_value)),
      cv_revenue = sd(total_value) / mean(total_value),
      log_cv_revenue_boat = log(cv_revenue),
      log_cv_revenue_adj_boat =  log_cv_revenue_boat + 4,
      cv_log_revenue_boat = sd(log_total_value) / mean(log_total_value),
      avg_growth = mean(growth),
      sd_growth = sd(growth),  ##calculated but will not use
      avg_index = mean(index),
      avg_pport = mean(proportion) * 100, #% vaught at primary port, a measure of transience
      value_cat = case_when(
        avg_value > 500000 ~ "> $500,000",
        avg_value > 100000 ~ "$100,000-$500,000",
        avg_value > 5000 ~ "$5,000-$100,000"),
      value_cat = factor(value_cat, 
                         levels = c("$5,000-$100,000","$100,000-$500,000", "> $500,000"))) %>%   ###port level information 
        distinct(HULLNUM, port_tidy, mega_subregion, yr_cnt, avg_value, avg_value1000_boat, avg_log_value,
                 cv_revenue, log_cv_revenue_boat, log_cv_revenue_adj_boat, cv_log_revenue_boat, avg_index, avg_pport, value_cat) %>% 
          rename(yr_cnt_boat = yr_cnt, avg_value_boat = avg_value, avg_log_value_boat = avg_log_value,
                 cv_revenue_boat = cv_revenue, avg_index_boat = avg_index)

  
  
  #primary port list
  primary_port_list <- primary_port %>% ungroup() %>% 
    select(port_tidy) %>% 
    distinct(port_tidy)  
  
##INDEX CALC -- PORT ####  
  #port * year
  port_year <- cfders_raw %>% 
    group_by(year, port_tidy) %>% 
      mutate(total_value = sum(value)) %>% 
        distinct(year, port_tidy, total_value) %>% 
          arrange(port_tidy, year, total_value) 
  
  #spp * port * year
  spp_port_year <- cfders_raw %>% 
    group_by(year, spp, port_tidy) %>% 
      mutate(value = sum(value)) %>% 
        distinct(year, spp, port_tidy, value) %>% 
          arrange(port_tidy, year, value)
  
  #index calculation 
  port_index_calc <- left_join(port_year, spp_port_year, 
                               by = c("year" = "year", "port_tidy" = "port_tidy")) %>% 
    mutate(temp_index = (value / total_value)^2) %>% 
      group_by(year, port_tidy) %>% 
        mutate(index = 1/ sum(temp_index)) %>% 
          distinct(year, port_tidy, total_value, index) %>% 
            arrange(port_tidy, year) %>% na.omit()
  
##vARIABLE CREATION -- PORT ####
  #right now we have a rolling mean, perhaps we could duplicate our observations every 4 years,
  #calculate cv and all 'averages' over that period to inflate sample size for ports
  
  port_processing <- port_index_calc %>% 
    group_by(port_tidy) %>% 
      mutate(log_total_value = log(total_value),
             growth = ((total_value - lag(total_value)) / lag(total_value))*100,
             rev_var = roll_sd(total_value, 3, na.rm = TRUE, align = "right", fill = NA),
             rev_mean = roll_mean(total_value, 3, na.rm = TRUE, align = "right", fill = NA),
             rev_cv = rev_var / rev_mean ) %>%
          filter(port_tidy %in% primary_port_list$port_tidy)
          
  port_analysis_summary <- port_processing %>% 
    group_by(port_tidy) %>% 
    summarise(
      yr_cnt = n_distinct(year),
      avg_index = mean(index),
      avg_value = mean(total_value),
      avg_value1000_port = avg_value / 1000,
      log_avg_value = log(avg_value),
      avg_growth = mean(growth),
      cv_revenue = sd(total_value) / mean(total_value),
      log_cv_revenue_port = log(cv_revenue),
      log_cv_revenue_adj_port = log_cv_revenue_port + 3,
      cv_log_revenue_port = sd(log_total_value) / mean(log_total_value),
      sd_growth = sd(growth)) %>% 
        mutate(
          value_cat_port = case_when(
            avg_value > 3000000 ~ "> $3,000,000",
            avg_value > 500000 ~ "$500,000-$3,000,000",
            avg_value > 15000 ~ "$15,000 - 500,000"),
          value_cat_port = factor(value_cat_port, 
                             levels = c("$15,000 - 500,000",
                                        "$500,000-$3,000,000", "> $3,000,000")))%>% 
        rename(avg_index_port = avg_index, yr_cnt_port = yr_cnt, avg_value_port = avg_value, 
               log_avg_value_port = log_avg_value, 
               cv_revenue_port = cv_revenue) %>% 
          distinct(port_tidy, value_cat_port, yr_cnt_port, avg_index_port, avg_value_port, avg_value1000_port, log_avg_value_port, 
                   cv_revenue_port, log_cv_revenue_port, log_cv_revenue_adj_port, cv_log_revenue_port)

ggplot(port_analysis_summary, aes(avg_value_port)) + geom_density()
  
summary(port_analysis_summary$avg_value_port)  
#ANALYSIS DATASET ####  
  #interpretation reference: http://home.wlu.edu/~gusej/econ398/notes/logRegressions.pdf
  
  #filtering step ID'd by below analyis 

  boat_port_input_temp <- left_join(boat_analysis_summary, port_analysis_summary,
                               by = c("port_tidy" = "port_tidy")) %>% 
    filter(avg_value_port > 14999)

sqldf("select distinct port_tidy from boat_port_input_temp")
sqldf("select distinct port_tidy, cv_revenue_port from boat_port_input_temp")



  #assessment of normality for cv revenues 
  a <- ggplot(boat_port_input_temp, aes(avg_index_boat, cv_revenue_boat)) + geom_boxplot()
  b <- ggplot(boat_port_input_temp, aes(avg_index_port, cv_revenue_port)) + geom_boxplot()
  grid.arrange(a, b, nrow=1)      
  
  #assessment of normality for logged cv revenues and cv of logged revenues 
  a_logcv <- ggplot(boat_port_input_temp, aes(log_cv_revenue_boat)) + geom_density()
  b_logcv <- ggplot(boat_port_input_temp, aes(log_cv_revenue_port)) + geom_density()
  grid.arrange(a_logcv, b_logcv, nrow=1)  
  
  a_cv_log <- ggplot(boat_port_input_temp, aes(cv_log_revenue_boat)) + geom_density()
  b_cv_log <- ggplot(boat_port_input_temp, aes(cv_log_revenue_port)) + geom_density()
  
  a_logcv_adj <- ggplot(boat_port_input_temp, aes(log_cv_revenue_adj_boat)) + geom_density()
  b_logcv_adj <- ggplot(boat_port_input_temp, aes(log_cv_revenue_adj_port)) + geom_density()
        
      #the log of cv_revenue checks out as normal for both samples w/out filtering
        #boats
          skewness(boat_port_input_temp$log_cv_revenue_adj_boat) # -0.5636424
          kurtosis(boat_port_input_temp$log_cv_revenue_adj_boat) # 3.706173
        #ports  
          skewness(boat_port_input_temp$log_cv_revenue_adj_port) # 0.405204
          kurtosis(boat_port_input_temp$log_cv_revenue_adj_port) # 2.849933
  
  grid.arrange(a_cv_log, a_logcv, a_logcv_adj, b_cv_log, b_logcv, b_logcv_adj, nrow=2)  
    
  write.csv(boat_port_input_temp, "C:/Users/bkennedy/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input_temp.csv") 
  
  
    
  
  
  #filtering based on visual inspection: goal is to get both cv's to be slightly normal
  boat_port_input <- boat_port_input_temp %>% filter(cv_revenue < 1.25, port_cv_revenue < .7, 
                                                          !port_tidy %in% c('OCEANSIDE_NY','BIRCHHARBOR_ME')) ##super volatile
  #validting via final input dataset via plot 
  a <- ggplot(boat_port_input, aes(avg_index, cv_revenue)) + geom_boxplot() + ggtitle("boat")
  b <- ggplot(boat_port_input, aes(avg_index, port_cv_revenue)) + geom_boxplot() + ggtitle("port")
  c <- ggplot(boat_port_input, aes(cv_revenue)) + geom_density() + ggtitle("boat")
  d <- ggplot(boat_port_input, aes( port_cv_revenue)) + geom_density() + ggtitle("port")
  
    #printing plot
    grid.arrange(a, b, c, d, nrow=2)   
  
###ANALYSIS FILE EXPORT: writing boat analysis raw data file####
write.csv(boat_port_input, "C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input.csv")
  
  