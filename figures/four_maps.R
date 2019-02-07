####by sub region 
x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", 
       "gridExtra", "broom", "viridis", "sjPlot", "jtools",
       "ggstance", "MASS", "leaps", "ggeffects",
       "cowplot")

lapply(x, require, character.only = TRUE) ##applying the function require to each point in the vector x


options(scipen = 999)  

input_alt <- read.csv("C:/Users/brian/Dropbox/COCA/Volatility Diversity_Project/redo/boat_port_input_temp.csv") %>%
  mutate(
    value_cat = factor(value_cat, 
                       levels = c("$5,000-$100,000","$100,000-$500,000", 
                                  "> $500,000")),
    mega_subregion = factor(mega_subregion, 
                            levels = c("Northern New England", "Southern New England",
                                       "Northern Mid Atlantic","Southern Mid Atlantic")))

##GGMAP STUFF


###ggmap county names w/ fips codes, splitting the combined county state variable into two variables 
countyfipscodes <- as_tibble(county.fips) %>% 
  separate(polyname, c("state", "county"), sep = ",")

###ggmap county names w/ spatial info
cnty <- as_tibble(map_data("county"))

state_data <- as_tibble(map_data("state")) %>% 
  rename(state_group = group, state_order = order) %>% 
    filter(region %in% c("maine", "new hampshire", "massachusetts", "rhode island", "connecticut", "new york", "new jersey",
                         "pennsylvania", "delaware", "maryland", "district of columbia", "virginia", 
                         "north carolina")) %>% 
      mutate(
        subregion = case_when(
          region %in% c('maine', 'new hampshire') ~ 'N. New England',
          region %in% c('massachusetts') ~ 'S. New England',
          region %in% c('rhode island', 'connecticut', 'new york') ~ 'N. Mid Atlantic',
          region %in% c('new jersey', 'pennsylvania', 'delaware', 'maryland', 'district of columbia', 'virginia', 
                      'north carolina') ~ 'S. Mid Atlantic'))

map_cnty <- left_join(cnty, countyfipscodes, by = c("region" = "state", "subregion" = "county")) %>% 
  rename( gg_state = region,  gg_cnty = subregion)


###only ports included in the analysis

geo_table <- as_tibble(read.csv("C:/Users/brian/Dropbox/COCA/DATA/GMRI_aggregated data/geo_table.csv", 
                                header = TRUE)) %>% 
  filter(State_abbrv %in% c('ME', 'NH', 'MA', 'RI', 'CT', 
                            'NY', 'NJ', 'DE', 'MD', 'VA', 'NC')) %>% 
  mutate( mega_subregion = case_when(                                 
    State_abbrv %in% c('ME', 'NH') ~ 'Northern New England',
    State_abbrv %in% c('MA') ~ 'Southern New England',
    State_abbrv %in% c('RI', 'CT', 'NY') ~ 'Northern Mid Atlantic',
    State_abbrv %in% c('NJ', 'DE', 'MD', 'VA', 'NC') ~ 'Southern Mid Atlantic'),
    mega_subregion = factor(mega_subregion, 
                            levels = c("Northern New England", "Southern New England",
                                       "Northern Mid Atlantic","Southern Mid Atlantic")))%>% 
  mutate(
    county_pad =  sprintf("%03d", CountyFIPS),  #inserting 0 in COUNTY
    fips_geo = as.numeric(paste0(StateFIPS,county_pad))) %>%  #changing backt to code and merging to FIPS code
  dplyr::select(port_tidy, CountyName, statename, fips_geo, mega_subregion, region)


###joining geo_table on our fips codes, right join so we can still draw the map
map_input_mega <- right_join(geo_table, map_cnty, by = c("fips_geo" = "fips")) %>% 
  filter(gg_state %in% c("maine", "new hampshire", "massachusetts", "vermont", "rhode island", 
                         "connecticut", "new york", "new jersey",
                         "pennsylvania", "delaware", "maryland", "district of columbia", 
                         "virginia", "north carolina")) %>% 
  mutate(  ###creating binary variable for counties included in the study and for our sub regions
    study_area = case_when(
      !is.na(mega_subregion) ~ "study area",
      is.na(mega_subregion) ~ "other"))

write.csv(map_input_mega, "C:/Users/brian/Dropbox/COCA/DATA/GMRI_aggregated data/map_input_mega.csv") 

#creating map data sets for each sub region
#joining just state data 



#Making Base Maps
##creating base state layer

#need to make 4 unqiue maps special ones for mid atlantic ones

  ggplot(state_data, aes(x=long, y=lat, group=state_group)) +
  geom_polygon(color = "black", alpha = .02) +
    coord_fixed(1.2) +
      facet_wrap(~subregion, scales = "free") 
        


