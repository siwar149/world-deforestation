# Load libraries ----------------------------------------------------------
library(readxl)
library(tidyverse)
library(scales)

#dir.create("./input", showWarnings = FALSE)
#dir.create("./output", showWarnings = FALSE)

# Scope of the study  -----------------------------------------------------
# amortization period (1/5/10/20 years) 
# land-use change intervals (time lag between deforestation and subsequent land use) (1/5/10 years)
# spatial resolution of LUC data (l1 = district/l2 = province/l3 = island/l4 = country) 
# direct and indirect deforestation attributed to palm oil
# deforestation footprints of China and the EU

# Load Data ---------------------------------------------------------------

rawdata <- read_excel("input/Statistics MB Indonesia Col2_Kabupaten_ID_Rev.xlsx", sheet = "TRANSITIONS")

conc_indo <- read_csv("./conc_indo.csv")

luc <- select(rawdata, island = "island_en", province = "prov_en", district = "distric_en",
              class_from, class_to, "2000-2001":"2021-2022") %>%
  left_join(conc_indo, by = c("class_from" = "id")) %>% 
  left_join(conc_indo, by = c("class_to" = "id")) %>% 
  select(-class_to, -class_from) %>%
  rename(class_from = class.x, class_to = class.y) %>%
  pivot_longer(cols = c(-island, -province, -district, -class_from, -class_to)) %>%
  separate(name, c("from","to"), sep = "-", convert = TRUE) %>% 
  group_by(island, province, district, class_from, class_to, from, to) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  mutate(timespan = to - from) %>% 
  filter(timespan %in% c(1,5,10), from %% timespan == 0, to < 2021) %>%
  mutate(trans = str_c(class_from,"_",class_to))
  

# indirect deforestation ------------------------------------------
# assumption: indirect deforestation of palm oil can be seen as the amount of land palm oil took form rice or other crops for which forests were converted
# spatial levels of analysis: 
# l1 = district
# l2 = province or island
# l3 = island
# l4 = country

data_l1 <- luc %>%
  select(-class_from, -class_to, -to) %>%
  filter(trans %in% c("Forest_Oil Palm", "Forest_Rice", "Forest_Other crops", "Rice_Oil Palm",
                      "Other crops_Oil Palm")) %>%
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>%
  mutate(direct = `Forest_Oil Palm`,
         indirect = ifelse(`Rice_Oil Palm` < `Forest_Rice`, `Rice_Oil Palm`, `Forest_Rice`) +
           ifelse(`Other crops_Oil Palm` < `Forest_Other crops`, `Other crops_Oil Palm`, `Forest_Other crops`)) %>%
  select(island, province, district, from, timespan, direct, indirect) %>%
  mutate(timespan = str_pad(timespan, width = 2, side = "left", pad = "0")) %>%
  pivot_wider(names_from = timespan, values_from = c(direct, indirect)) %>%
  group_by(island, province, district, cut5 = cut(from, seq(2000, 2020, 5), include.lowest = T, right = F)) %>%
  mutate(direct_05 = min(direct_05, na.rm = T) / n(),
         indirect_05 = min(indirect_05, na.rm = T) / n()) %>%
  ungroup() %>%
  group_by(island, province, district, cut10 = cut(from, seq(2000, 2020, 10), include.lowest = T, right = F)) %>%
  mutate(direct_10 = min(direct_10, na.rm = T) / n(),
         indirect_10 = min(indirect_10, na.rm = T) / n()) %>%
  ungroup() %>%
  select(-island, -province, - district, -cut5, -cut10) %>%
  group_by(from) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  pivot_longer(-from)


data_l2 <- luc %>%
  select(-class_from, -class_to, -to, -district) %>% 
  group_by(province, from, timespan, trans) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  filter(trans %in% c("Forest_Oil Palm", "Forest_Rice", "Forest_Other crops", "Rice_Oil Palm",
                      "Other crops_Oil Palm")) %>% 
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>% 
  mutate(direct = `Forest_Oil Palm`,
         indirect = ifelse(`Rice_Oil Palm` < `Forest_Rice`, `Rice_Oil Palm`, `Forest_Rice`) +
           ifelse(`Other crops_Oil Palm` < `Forest_Other crops`, `Other crops_Oil Palm`, `Forest_Other crops`)) %>% 
  select(province, from, timespan, direct, indirect) %>% 
  mutate(timespan = str_pad(timespan, width = 2, side = "left", pad = "0")) %>% 
  pivot_wider(names_from = timespan, values_from = c(direct, indirect)) %>% 
  group_by(province, cut5 = cut(from, seq(2000, 2020, 5), include.lowest = T, right = F)) %>% 
  mutate(direct_05 = min(direct_05, na.rm = TRUE) / n(), 
         indirect_05 = min(indirect_05, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  group_by(province, cut10 = cut(from, seq(2000, 2020, 10), include.lowest = T, right = F)) %>% 
  mutate(direct_10 = min(direct_10, na.rm = TRUE) / n(), 
         indirect_10 = min(indirect_10, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  select(-province, -cut5, -cut10) %>% 
  group_by(from) %>% 
  summarise(across(everything(), sum), .groups = "drop") %>% 
  pivot_longer(-from)



data_l3 <- luc %>%
  select(-class_from, -class_to, -to, -district, -province) %>% 
  group_by(island, from, timespan, trans) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  filter(trans %in% c("Forest_Oil Palm", "Forest_Rice", "Forest_Other crops", "Rice_Oil Palm",
                      "Other crops_Oil Palm")) %>% 
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>% 
  mutate(direct = `Forest_Oil Palm`,
         indirect = ifelse(`Rice_Oil Palm` < `Forest_Rice`, `Rice_Oil Palm`, `Forest_Rice`) +
           ifelse(`Other crops_Oil Palm` < `Forest_Other crops`, `Other crops_Oil Palm`, `Forest_Other crops`)) %>% 
  select(island, from, timespan, direct, indirect) %>% 
  mutate(timespan = str_pad(timespan, width = 2, side = "left", pad = "0")) %>% 
  pivot_wider(names_from = timespan, values_from = c(direct, indirect)) %>% 
  group_by(island, cut5 = cut(from, seq(2000, 2020, 5), include.lowest = T, right = F)) %>% 
  mutate(direct_05 = min(direct_05, na.rm = TRUE) / n(), 
         indirect_05 = min(indirect_05, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  group_by(island, cut10 = cut(from, seq(2000, 2020, 10), include.lowest = T, right = F)) %>% 
  mutate(direct_10 = min(direct_10, na.rm = TRUE) / n(), 
         indirect_10 = min(indirect_10, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  select(-island, -cut5, -cut10) %>% 
  group_by(from) %>% 
  summarise(across(everything(), sum), .groups = "drop") %>% 
  pivot_longer(-from)



data_l4 <- luc %>%
  select(-class_from, -class_to, -to, -district, -province, -island) %>% 
  group_by(from, timespan, trans) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  filter(trans %in% c("Forest_Oil Palm", "Forest_Rice", "Forest_Other crops", "Rice_Oil Palm",
                      "Other crops_Oil Palm")) %>% 
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>% 
  mutate(direct = `Forest_Oil Palm`,
         indirect = ifelse(`Rice_Oil Palm` < `Forest_Rice`, `Rice_Oil Palm`, `Forest_Rice`) +
           ifelse(`Other crops_Oil Palm` < `Forest_Other crops`, `Other crops_Oil Palm`, `Forest_Other crops`)) %>% 
  select(from, timespan, direct, indirect) %>% 
  mutate(timespan = str_pad(timespan, width = 2, side = "left", pad = "0")) %>% 
  pivot_wider(names_from = timespan, values_from = c(direct, indirect)) %>% 
  group_by(cut5 = cut(from, seq(1990, 2020, 5), include.lowest = T, right = F)) %>% 
  mutate(direct_05 = min(direct_05, na.rm = TRUE) / n(), 
         indirect_05 = min(indirect_05, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  group_by(cut10 = cut(from, seq(1990, 2020, 10), include.lowest = T, right = F)) %>% 
  mutate(direct_10 = min(direct_10, na.rm = TRUE) / n(), 
         indirect_10 = min(indirect_10, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  select(-cut5, -cut10) %>% 
  pivot_longer(-from)


mean_roll_05 <- rollify(mean, window = 5, unlist = TRUE, na_value = NULL)
mean_roll_10 <- rollify(mean, window = 10, unlist = TRUE, na_value = NULL)
mean_roll_20 <- rollify(mean, window = 20, unlist = TRUE, na_value = NULL)

data <- left_join(data_l1, data_l2, by = c("from" = "from", "name" = "name"), suffix = c("_l1", "_l2")) %>% 
  left_join(data_l3) %>% 
  rename(value_l3 = value) %>%
  left_join(data_l4) %>%
  rename(value_l4 = value) %>%
  pivot_wider(names_from = name, values_from = c(value_l1, value_l2, value_l3, value_l4)) %>% 
  arrange(from) %>% 
  mutate(across(starts_with("value_"), .fns = list(mean_05 = mean_roll_05, 
                                                   mean_10 = mean_roll_10, 
                                                   mean_20 = mean_roll_20))) %>% 
  pivot_longer(cols = -from) %>% 
  separate(name, into = c("aux1","spatial_level", "def_path","timelag","aux2","amortisation"), sep = "_", fill = "right") %>% 
  select(-aux1, -aux2) %>% 
  mutate(amortisation = ifelse(is.na(amortisation), "01", amortisation),
         spatial_level = factor(spatial_level, levels = c("l1", "l2", "l3", "l4"), labels = c("district", "province", "island", "country")),
         def_path = factor(def_path, levels = c("indirect", "direct"), labels = c("Indirect", "Direct"))) %>% 
  filter(!is.na(value)) %>% 
  rename(`Spatial level` = spatial_level)


################## DATA CHECK
# Direct deforestation
# All spatial levels must be identical
gp <- data %>% 
  filter(def_path == "Direct") %>% 
  ggplot(aes(x = from, y = value, colour = `Spatial level`)) + 
  theme_bw() + 
  facet_grid(vars(timelag), vars(amortisation), labeller = label_both) +
  geom_line() + 
  scale_color_viridis_d() + 
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.01)) + 
  xlab("Year") +
  ylab("Deforestation (M ha)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0),
        legend.margin = margin(-10,0,0,0))

ggplot2::ggsave(plot = gp, bg = "#ffffff",
                filename = "./output/fig-1-direct-soy-beans-deforestation.png",
                width = 160, height = 140, units = "mm", scale = 1)

# Indirect deforestation
# Indirect deforestation increases with the level of spatial aggregation
gp <- data %>% 
  filter(def_path == "Indirect") %>% 
  ggplot(aes(x = from, y = value, colour = `Spatial level`)) + 
  theme_bw() + 
  facet_grid(vars(timelag), vars(amortisation), labeller = label_both) +
  geom_line() + 
  scale_color_viridis_d() + 
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.01)) + 
  xlab("Year") +
  ylab("Deforestation (M ha)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0),
        legend.margin = margin(-10,0,0,0))

ggplot2::ggsave(plot = gp, bg = "#ffffff",
                filename = "./output/fig-2-indirect-soy-beans-deforestation.png",
                width = 160, height = 140, units = "mm", scale = 1)

# Total deforestation = direct + indirect
# Total deforestation increases with the level of spatial aggregation
gp <- data %>% 
  group_by(from, `Spatial level`, timelag, amortisation) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  ggplot(aes(x = from, y = value, colour = `Spatial level`)) + 
  theme_bw() + 
  facet_grid(vars(timelag), vars(amortisation), labeller = label_both) +
  geom_line() + 
  scale_color_viridis_d() + 
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.01)) + 
  xlab("Year") +
  ylab("Deforestation (M ha)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = -45, hjust = 0),
        legend.margin = margin(-10,0,0,0))

ggplot2::ggsave(plot = gp, bg = "#ffffff",
                filename = "./output/fig-3-total-soy-beans-deforestation.png",
                width = 160, height = 140, units = "mm", scale = 1)

# Total deforestation = direct + indirect
# 
Direct = viridis::viridis(2, begin = 0.3, end = 0.7)[1]
Indirect = viridis::viridis(2, begin = 0.3, end = 0.7)[2]

gp <- data %>% 
  ggplot() + 
  theme_bw() + 
  facet_grid(vars(amortisation), vars(timelag), labeller = label_both) +
  geom_bar(aes(x = from, y = value, group = `Spatial level`, fill = "Indirect", width = 0.5), 
           stat = "identity", show.legend = TRUE, position = "dodge", color = "black", linewidth = 0.1) + 
  geom_bar(data = filter(data, def_path == "Direct"),
           mapping = aes(x = from, y = value, group = `Spatial level`, fill = "Direct", width = 0.5), 
           stat = "identity", position = "dodge", color = "black", size = 0.1) + 
  scale_fill_viridis_d(begin = 0.6, end = 0.95) + 
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1)) + 
  xlab("Year") +
  ylab("Deforestation (M ha)") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = -45, hjust = 0),
        legend.margin = margin(-10,0,0,0))

ggplot2::ggsave(plot = gp, bg = "#ffffff",
                filename = "./output/fig-4-split-total-soy-beans-deforestation.png",
                width = 240, height = 140, units = "mm", scale = 1)
