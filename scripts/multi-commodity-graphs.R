
library(WDI)
library(mapproj)
library(scales)
library(tidyverse)
library(forcats)
library(networkD3)
library(pandoc)
library(rmarkdown)
library(RColorBrewer)
library(ggsankeyfier)
library(sf)
library(units)
library(ggpubr)
library(rnaturalearthdata)
library(ggbreak)
library(scatterpie)
library(ggnewscale)

# Install ggsankey from GitHub
#devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# set working directory
setwd("~/projects/world-deforestation//")
setwd("~/deforestation_accounting/")

results_all <- readRDS("./output/global_results_v3.rds")
rm(list = setdiff(ls(), "results"))
gc()

# generating consistency for matching with world map data
#fc <- read_excel("./input/fabio-countries.xlsx")
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"
regions <- fread(file=paste0(input_path,"regions.csv"))

# worldbank population data
pop_d <- WDI(indicator = "SP.POP.TOTL", country = "all", start = 2010, end = 2022) %>%
  select(iso3c, year, SP.POP.TOTL)

# preliminary analysis

# setting the data
results_all <- results_all[, `:=` (continent_origin = fifelse(continent_origin == 'EU', 'EUR',continent_origin),
                                   continent_imd = fifelse(continent_imd == 'EU', 'EUR',continent_imd),
                                   continent_target = fifelse(continent_target == 'EU', 'EUR',continent_target))]

# I like this graph (INTEGRATE WITH WORLD MAP!)
results_all %>%
  filter(continent_target != "ROW") %>%
  mutate(trade = if_else(continent_origin == continent_target, "DOM", "TRADE")) %>%
  group_by(continent_target,consumption_category, trade) %>% 
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>%
  ggplot(aes(x = "", y = d_f_a, fill = interaction(consumption_category, trade))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  #geom_text(aes(y = ypos, label = paste0(round(prop), "%")), 
  #          color = "white", size = 5) +
  scale_fill_viridis_d(option = "H", direction = 1, begin = 0.4) +
  facet_wrap(~continent_target, scales = "free") +
  theme_void() +
  theme(legend.title = element_blank())

# net footprints per country
b <- results_all[, exp := fifelse(country_origin == country_target, "DOM", "EXP")][, .(d_f_a = sum(d_f_a, na.rm = T)), by = c("country_origin","exp")] %>% dcast(country_origin~exp, value.var = "d_f_a")
c <- results_all[, imp := fifelse(country_origin == country_target, "DOM", "IMP")][, .(d_f_a = sum(d_f_a, na.rm = T)), by = c("country_target","imp")] %>% dcast(country_target~imp, value.var = "d_f_a")
c1 <- c[country_target %in% setdiff(c$country_target,b$country_origin)] %>% rename(country = country_target)

a <- left_join(b, c[,c(1,3)], by = c("country_origin"="country_target")) %>% rename(country = country_origin)
a <- rbind(a,c1, fill = T)
a[is.na(a)] <- 0
a[, `:=` (NFP = DOM + IMP,
          type = fcase(IMP > DOM, "Importer",
                       EXP > DOM, "Exporter",
                       DOM > IMP, "Domestic",
                       default = NA_character_))]

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% select(admin, adm0_a3,geometry)
world <- world %>% left_join(a[,c(1,5,6)], by = c("adm0_a3"="country"))

# Scale NFP within each type
world <- world %>%
  group_by(type) %>%
  mutate(NFP_scaled = rescale(NFP)) %>%
  ungroup()

# Plot with fill by type and alpha by NFP within type
# ggplot(world) +
#   geom_sf(aes(fill = type, alpha = NFP_scaled)) +
#   scale_alpha(range = c(0.4, 1), guide = "none") +  # hide alpha legend
#   coord_sf(crs = "+proj=robin") +
#   theme_minimal() +
#   scale_fill_viridis_d(option = "H", begin = 0.1)
  

# World map with pie charts
pies <- results_all %>%
  filter(continent_target != "ROW") %>%
  mutate(trade = if_else(continent_origin == continent_target, "DOM", "TRADE")) %>%
  group_by(continent_target,consumption_category, trade) %>% 
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>%
  pivot_wider(names_from = c(consumption_category, trade), values_from = d_f_a) %>%
  mutate(lon = c(-10, 80, -30, -110, -150, 165),
         lat = c(-20, -20, 40, -10, 35, 10))

colnames(pies)[2:5] <- c("Food_DOM", "Food_TRADE", "Other_DOM", "Other_TRADE")

ggplot() +
  geom_sf(data = world,aes(fill = type, alpha = NFP_scaled)) +
  scale_alpha(range = c(0.4, 1), guide = "none") +  # hide alpha legend
  #coord_sf(crs = "+proj=robin") +
  coord_sf(xlim = c(-180, 200), ylim = c(-60, 80)) +
  scale_fill_viridis_d(name = "Footprint", option = "H", begin = 0.1) +
  new_scale_fill() +
  geom_scatterpie(
    aes(x = lon, y = lat, group = continent_target, r = 16),
    data = pies,
    cols = colnames(pies)[2:5],
    color = NA
  ) +
  scale_fill_viridis_d(name = "End use_Source", option = "H", direction = 1, begin = 0.4) +
  geom_text(
    data = pies,
    aes(x = lon, y = lat, label = continent_target),
    nudge_y = 19, size = 3
  ) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.background = element_rect(color = "black", size = 1),
        legend.spacing = unit(1, "cm"),
        legend.margin = margin(5, 5, 5, 5),
        plot.margin = margin(-3,-2,-3,-1,"cm")
        ) +
  guides(fill = guide_legend(nrow = 2))




ggsave(plot = last_plot(), bg = "#ffffff",
         filename = "./output/FIG1-net-footprints-trade.png",
         width = 240, height = 140, units = "mm", scale = 1)




# top crops
a <- results_all %>% group_by(crop) %>% summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% arrange(desc(d_f_a)) %>% print(n = 15)

a_l <- a$crop[1:9]

results_all %>% mutate(trade = if_else(continent_origin == continent_target, "DOM", "TRADE")) %>%
  group_by(crop, consumption_category,trade) %>% summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% arrange(desc(d_f_a)) %>%
  mutate(crop = if_else(crop %in% a_l, crop, "Other")) %>% group_by(crop, consumption_category,trade) %>%
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% print(n = 15) %>%
  mutate(crop = fct_reorder(crop, d_f_a, .fun = sum, .desc = FALSE)) %>%  # Orders by sum(d_f_a)
  mutate(crop = fct_relevel(crop, "Other", after = 0)) %>%  # Moves "Other" to the bottom
  ggplot(aes(x = crop, y = d_f_a, fill = interaction(consumption_category,trade))) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  #scale_y_break(c(600, 2000)) +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) +  # Apply viridis color scale
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-3)) + 
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 12),
        panel.spacing = unit(0.5, "cm"),
        legend.margin = margin(-25, 0, 0, 0),
        plot.margin = margin(10, 20, 10, 10))

ggsave(plot = last_plot(), bg = "#ffffff",
       filename = "./output/FIG1_2-global-def-fp-crops.png",
       width = 240, height = 140, units = "mm", scale = 1)





# top products
a <- results_all %>% group_by(group) %>% summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% arrange(desc(d_f_a)) %>% print(n = 15)

a_l <- a$group[1:9]

results_all %>% group_by(group, consumption_category) %>% summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% arrange(desc(d_f_a)) %>%
  mutate(group = if_else(group %in% a_l, group, "Other")) %>% group_by(group, consumption_category) %>%
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% print(n = 15) %>%
  mutate(group = if_else(group == "Vegetables, fruit, nuts, pulses, spices", "Fruits, etc.", group)) %>%
  mutate(group = fct_reorder(group, d_f_a, .fun = sum, .desc = FALSE)) %>%  # Orders by sum(d_f_a)
  mutate(group = fct_relevel(group, "Other", after = 0)) %>%  # Moves "Other" to the bottom
  ggplot(aes(x = group, y = d_f_a, fill = consumption_category)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) +  # Apply viridis color scale
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-3)) + 
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_linedraw() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 12),
        panel.spacing = unit(0.5, "cm"),
        legend.margin = margin(-25, 0, 0, 0),
        plot.margin = margin(10, 20, 10, 10))


ggsave(plot = last_plot(), bg = "#ffffff",
       filename = "./output/fig-22-global-def-fp-crops.png",
       width = 240, height = 140, units = "mm", scale = 1)




# Consuming side

# coloring each page
avg_dfa <- results_all %>%
  group_by(continent_target) %>%
  summarise(avg_dfa = mean(d_f_a, na.rm = TRUE)) %>%
  mutate(color = scales::rescale(avg_dfa, to = c(0, 1)))

results_all %>%
  filter(continent_target != "ROW") %>%
  mutate(continent_target = if_else(continent_target == "EU", "EUR", continent_target)) %>%
  mutate(imp = if_else(continent_origin == continent_target, "DOM", "IMP")) %>%
  group_by(continent_target, consumption_category, imp, group) %>%
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>%
  filter(!group %in% c("Fibre crops", "Tobacco, rubber", "Sugar crops", "Ethanol", "Alcohol", "Eggs", "Sugar, sweeteners")) %>%
  mutate(group = if_else(group == "Vegetables, fruit, nuts, pulses, spices", "Fruits, etc.", group)) %>%
  ggplot(aes(x = reorder(group, +d_f_a), y = d_f_a, fill = interaction(consumption_category, imp))) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) +  # Apply viridis color scale
  coord_flip() +
  facet_wrap(~continent_target, scales = "free_x", nrow = 2) +  # Facet by continent
  scale_y_continuous(labels = label_number(scale = 1e-3)) + 
  labs(x = "", y = "", fill = "") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 12),
        panel.spacing = unit(0.5, "cm"),
        legend.margin = margin(-10, 0, 0, 0),
        plot.margin = margin(10, 20, 10, 10))
  

ggsave(plot = last_plot(), bg = "#ffffff",
       filename = "./output/fig-19-continent-def-fp-consum-group.png",
       width = 240, height = 140, units = "mm", scale = 1)


# Producing side
results_all %>%
  mutate(continent_origin = if_else(continent_origin == "EU", "EUR", continent_origin)) %>%
  mutate(exp = if_else(continent_origin == continent_target, "DOM", "EXP")) %>%
  group_by(continent_origin, consumption_category, exp, crop, group) %>%
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>%
  filter(!group %in% c("Fibre crops", "Tobacco, rubber", "Sugar crops", "Ethanol", "Alcohol", "Eggs", "Sugar, sweeteners")) %>%
  mutate(group = if_else(group == "Vegetables, fruit, nuts, pulses, spices", "Fruits, etc.", group)) %>%
  ggplot(aes(x = reorder(group, +d_f_a), y = d_f_a, fill = interaction(consumption_category, exp))) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_viridis_d(option = "viridis", direction = -1, begin = 0, end = 1) +  # Apply viridis color scale
  coord_flip() +
  facet_wrap(~continent_origin, scales = "free_x", nrow = 2) +  # Facet by continent
  scale_y_continuous(labels = label_number(scale = 1e-3)) + 
  labs(
    x = "",
    y = "",
    fill = ""
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 12),
        panel.spacing = unit(1, "cm"),
        legend.margin = margin(-10, 0, 0, 0),
        plot.margin = margin(10, 20, 10, 10))


ggsave(plot = last_plot(), bg = "#ffffff",
       filename = "./output/fig-20-continent-def-fp-prod-group.png",
       width = 240, height = 140, units = "mm", scale = 1)


unimportant <- c("Fibre crops", "Tobacco, rubber", "Sugar crops", "Ethanol",
                 "Alcohol", "Eggs", "Sugar, sweeteners")


# analysis of Europe
# Identify the 7 most important crops and products
im_crop <- results_all[continent_target == "EU", .(d_f_a = sum(d_f_a)), by = crop][order(-d_f_a)][1:5, crop]
im_prod <- results_all[continent_target == "EU", .(d_f_a = sum(d_f_a)), by = product][order(-d_f_a)][1:5, product]

# Filter, transform, and summarize in one step
df <- results_all[continent_imd != "ROW" & continent_target %in% c("EU", "EUR"), 
                  .(d_f_a = sum(d_f_a)), 
                  by = .(continent_origin = fifelse(continent_origin == "EU", "EUR", continent_origin), 
                         crop = fifelse(crop %in% im_crop, crop, "Other"), 
                         #continent_imd = fifelse(continent_imd == "EU", "EUR", continent_imd), 
                         product = fifelse(product %in% im_prod, product, "Other"),
                         continent_target = fifelse(continent_target == "EU", "EUR", continent_target))]

long <- make_long(df,
                  c(continent_origin, crop, product, continent_target),
                  value =  d_f_a)

ggplot(long, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 fill = factor(node),
                 label = node,
                 value = value,
                 space = -3)) +
  geom_sankey(flow.alpha = .5,
              node.color = "gray30") +
  geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_blank())


ggplot(long, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 fill = factor(node),
                 label = node,
                 value = value)) +
  geom_alluvial(flow.alpha = .8) +
  geom_alluvial_text(size = 3, color = "black") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
