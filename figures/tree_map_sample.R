


tree_input_port <- input_alt %>% 
  group_by(value_cat_port, mega_subregion) %>% 
    summarise(
      count= n_distinct(port_tidy),
      catch_div = round(mean(avg_index_boat), 2),
      rev_vol = round(mean(cv_revenue_boat), 2))

tree_input_boat <- input_alt %>% 
  group_by(value_cat, mega_subregion) %>% 
  summarise(
    count= n_distinct(HULLNUM),
    catch_div = round(mean(avg_index_boat), 2),
    rev_vol = round(mean(cv_revenue_boat), 2))

library(treemapify)
library(ggrepel)

g20 <- treemapify::G20

ggplot(G20, aes(area = gdp_mil_usd, fill = hdi, label = country,
                subgroup = region)) +
  geom_treemap() + 
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.5, colour =
                             "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T, grow = FALSE)

gear_spp_input

gear_spp_input <- read.csv("C:/Users/brian/Dropbox/COCA--diversity/Community data/data/gear_spp_input.csv")


ggplot(gear_spp_input, aes(area = value, fill = value, label = spp_top,
                           subgroup = gear_type)) + 
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "center", reflow = T) +
  geom_treemap_subgroup_border() + 
  geom_treemap_subgroup_text(place = "topleft", alpha = 0.5, colour =
                               "black", fontface = "italic") 

    geom_treemap()  

library(treemap)

treemap(tree_input_port, 
        index = c("mega_subregion", "value_cat_port"),
        vSize = "count",
        vColor = "catch_div",
        type = "value",
        palette = "RdYlGn",
        bg.labels = 0,
        align.labels=list(
          c("left", "top"), 
          c("center", "center")), 
        border.col = c("white", "white"),
        border.lwds=c(7,1))


treemap(tree_input_boat, 
        index = c("mega_subregion", "value_cat"),
        vSize = "count",
        vColor = "rev_vol",
        type = "value",
        palette = "YlGnBu",
        bg.labels = 0,
        align.labels=list(
          c("left", "top"), 
          c("center", "center")), 
        border.col = c("white", "white"),
        border.lwds=c(7,1))
