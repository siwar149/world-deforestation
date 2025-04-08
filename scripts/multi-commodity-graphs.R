
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

# Install ggsankey from GitHub
devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

# set working directory
setwd("~/projects/FAO-deforestation/")
setwd("~/deforestation_accounting/")

results_all <- readRDS("./output/global_results_v3.rds")
rm(list = setdiff(ls(), "results"))
gc()

# generating consistency for matching with world map data
fc <- read_excel("./input/fabio-countries.xlsx")
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"
regions <- fread(file=paste0(input_path,"regions.csv"))

# worldbank population data
pop_d <- WDI(indicator = "SP.POP.TOTL", country = "all", start = 2010, end = 2022) %>%
  select(iso3c, year, SP.POP.TOTL)

# preliminary analysis

# setting the data


# top crops
a <- results_all %>% group_by(crop) %>% summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% arrange(desc(d_f_a)) %>% print(n = 15)

a_l <- a$product[1:9]

results_all %>% group_by(crop, consumption_category) %>% summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% arrange(desc(d_f_a)) %>%
  mutate(crop = if_else(crop %in% a_l, crop, "Other")) %>% group_by(crop, consumption_category) %>%
  summarise(d_f_a = sum(d_f_a), .groups = "drop") %>% print(n = 15) %>%
  mutate(crop = fct_reorder(crop, d_f_a, .fun = sum, .desc = FALSE)) %>%  # Orders by sum(d_f_a)
  mutate(crop = fct_relevel(crop, "Other", after = 0)) %>%  # Moves "Other" to the bottom
  ggplot(aes(x = crop, y = d_f_a, fill = consumption_category)) +
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
       filename = "./output/fig-21-global-def-fp-crops.png",
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






# world maps
world_map <- map_data("world") %>% 
  filter(! long > 180)


gp <- results %>%
  group_by(item_final, country_target, year) %>%
  summarise(cbf_un = sum(cbf_un),
            cbf_a = sum(cbf_a), .groups = "drop") %>%
  filter(item_final == commodity) %>%
  left_join(regions[, c(1,3)], by = c("country_target"="iso3c")) %>%
  left_join(pop_d, by = c("country_target"="iso3c", "year"), relationship = "many-to-many") %>%
  left_join(fc, by = c("area"="fabio_c"), relationship = "many-to-many") %>%
  mutate(cbf_un_c = cbf_un / SP.POP.TOTL,
         cbf_a_c = cbf_a / SP.POP.TOTL) %>%
  filter(year == 2021) %>%
  select(item_final, country_target, WM, cbf_un_c, cbf_a_c)

map <- world_map %>%
  left_join(gp,by=c("region"="WM"), relationship = "many-to-many")

p <- map %>%
  ggplot(aes(map_id = region, fill = cbf_un_c)) +
  geom_map(map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               color = "black", fill = NA, size = 0.1) +
  labs(fill = "m2 / person", title = "") +  # Add title using labs()
  # scale_fill_viridis_c(end = 0.95, direction = -1,
  scale_fill_viridis_c(option = "magma", begin = 0.2, direction = -1,
                       labels = scales::label_number(scale = 1e+4)) +  # Use reversed color gradient
  # coord_map(projection = "moll") +
  coord_sf(crs = "+proj=robin") +
  theme_bw() +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.title.y = element_blank(),  # Remove y-axis label
        axis.text.x = element_blank(),   # Remove x-axis values
        axis.text.y = element_blank(),   # Remove y-axis values
        axis.ticks = element_blank(),    # Remove tick marks from axes
        panel.grid = element_blank(),    # Remove coordinate lines
        panel.border = element_rect(color = "black", size = 0),  # Thicker plot frame
        # panel.grid.major = element_line(color = "gray80", size = 0.5),  # Add graticule lines
        # panel.grid.minor = element_blank(),  # Remove minor grid lines
        plot.title = element_text(hjust = 0.5, face = "bold"))  # Center and bold title


ggplot2::ggsave(plot = p, bg = "#ffffff",
                filename = paste0("./output/fig-", commodity, ".png"),
                width = 240, height = 140, units = "mm", scale = 1)
