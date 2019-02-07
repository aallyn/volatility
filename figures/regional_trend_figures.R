# multiple ways of making lme and regional trend figures 
  #figure ultimately printed at end of script and saved to file


x <- c("sqldf", "tidyverse", "RcppRoll", "ggthemes", "moments", "gridExtra", "broom", "viridis",
     "jtools","ggstance", "cowplot")

lapply(x, require, character.only = TRUE) ##applying the function require to each point in the vector x

lmeraw <- as_tibble(read.csv("C:/Users/brian/Dropbox/COCA/DATA/GMRI_aggregated data/lme.csv", header = TRUE))

rolled <- lmeraw %>% 
  mutate(index2 = roll_mean(index_value, 5, na.rm = TRUE, align = "right", fill = NA),
         rev_var = roll_sd(total_value, 5, na.rm = TRUE, align = "right", fill = NA),
         rev_mean = roll_mean(total_value, 5, na.rm = TRUE, align = "right", fill = NA),
         rev_cv = rev_var / rev_mean) %>% 
  filter(year >1984)


lme_agg_df <- as.data.frame(rolled) 


lme_agg_vol <- lme_agg_df %>% 
  dplyr::select(year, rev_cv) %>% 
  mutate(var = case_when(rev_cv > 0 ~ "Rev. Volatility"),
         region = case_when(rev_cv > 0 ~ "Northeast Shelf"),
         value = rev_cv)

lme_agg_index <- lme_agg_df %>% 
  dplyr::select(year, index2) %>% 
  mutate(var = case_when(index2 > 0 ~ "Rev. Diversity"),
         region = case_when(index2 > 0 ~ "Northeast Shelf"),
         value = index2)

agg_trend_input <- bind_rows(lme_agg_vol, lme_agg_index) %>% 
  dplyr::select(- rev_cv, -index2)

lme_trend <-ggplot(agg_trend_input, aes(x = year, y = value)) + 
  geom_line(colour = "black", size = 1.2) + 
  geom_smooth(size = NA, method = 'loess') +
  scale_colour_identity() + 
  facet_grid(var ~ region, scales="free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank())



lme_df <- as.data.frame(lme_state_roll %>% 
                          mutate(Y = mega_subregion))


r_vol <- ggplot(data = lme_df, aes(x = year, y = rev_cv)) + 
  geom_line(data = transform(lme_df, mega_subregion = NULL), aes(group = Y)
            , alpha = 0.35) +   
  geom_line(aes(group = mega_subregion), colour = "black", size = 1.2) + 
    geom_smooth(size = NA, method = 'loess') +
      facet_grid(~mega_subregion) +
        theme_bw() +
          theme(axis.text.x = element_text(angle=45, hjust=1),
                legend.position = 'none',
                axis.title = element_blank()) +
                  scale_x_continuous(breaks = c(1990,2000,2010), limits = c(1985,2015)) +
                  scale_y_continuous(breaks = c(0,0.1,0.2,0.3), limits = c(0,0.3))

#looks good, but really diminshes lme scale increase! 

s_vol <- ggplot(lme_agg_df, aes(x = year, y = rev_cv)) + 
  geom_line(colour = "black", size = 1.2) + 
  geom_smooth(size = NA, method = 'loess') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank()) +
  scale_x_continuous(breaks = c(1990,2000,2010), limits = c(1985,2015)) +
  scale_y_continuous(breaks = c(0,0.05,0.1, 0.15), limits = c(0,0.15)) +
  labs(title = "Revenue Volatility")

s_index <- ggplot(lme_agg_df, aes(x = year, y = index2)) + 
  geom_line(colour = "black", size = 1.2) + 
  geom_smooth(size = NA, method = 'loess') +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank()) +
  scale_x_continuous(breaks = c(1990,2000,2010), limits = c(1985,2015)) +
  scale_y_continuous(breaks = seq(1, 17, by = 4), limits = c(1,17)) +
  labs(title = "Revenue Diversification")


lme <- plot_grid(s_vol, s_index, nrow = 1)


lme_vol <- lme_df %>% 
  dplyr::select(year, mega_subregion, rev_cv) %>% 
    mutate(var = case_when(rev_cv > 0 ~ "Rev. Volatility"),
           value = rev_cv)

lme_index <- lme_df %>% 
  dplyr::select(year, mega_subregion, index_roll) %>% 
  mutate(var = case_when(index_roll > 0 ~ "Rev. Diversity"),
         region = case_when(index_roll > 0 ~ "Northeast Shelf"),
         value = index_roll) 

trend_input <- bind_rows(lme_vol, lme_index) %>% dplyr::select(- rev_cv, -index_roll) %>% 
  mutate(Y = mega_subregion)


region_trend <- ggplot(trend_input, aes(x = year, y = value)) + 
  
  geom_line(data = transform(trend_input, mega_subregion = NULL), aes(group = Y)
            , alpha = 0.35) +   
  geom_line(aes(group = mega_subregion, colour = "black"), size = 1.2) + 
  geom_smooth(size = NA, method = 'loess') +
  scale_colour_identity() + 
  facet_grid(var ~ mega_subregion, scales="free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = 'none',
        axis.title = element_blank())


alt_trend_fig <- plot_grid(lme, region_trend, nrow = 2, labels = c("a", "b"),
                           rel_heights = 3)
s
ggsave("C:/Users/brian/Dropbox/COCA--diversity/figures/alt_trend_fig.jpg", alt_trend_fig,
       height = 192.75, width = 140, units = "mm")

#this is good first start, however I think i need to do it via 4
#seperate figures so I can get better control of over axis and titles
# ideally, lme and region have same axis values and they align 
#titles would have to be placed somewhere on the plot 
#overall, looks pretty good. huge improvement from previous plot

