# Load libraries ----------------------------------------------------------
library(readxl)
library(tidyverse)
library(tibbletime)
library(scales)

dir.create("./input", showWarnings = FALSE)
dir.create("./output", showWarnings = FALSE)

# Scope of the study  -----------------------------------------------------
# amortization period (1/5/10/20 years) 
# land-use change intervals (time lag between deforestation and subsequent land use) (1/5/10 years)
# spatial resolution of LUC data (l1 = municipality/l2 = state/l3 = country) 
# direct and indirect deforestation attributed to soy
# deforestation footprints of China and the EU

# Load Data ---------------------------------------------------------------

# url <- "https://mapbiomas-br-site.s3.amazonaws.com/Estat%C3%ADsticas/1_-_TABELA_GERAL_COL7_MAPBIOMAS_BIOMAS_UF_FINAL.xlsx"
# download.file(url, "./input/1_-_TABELA_GERAL_COL7_MAPBIOMAS_BIOMAS_UF_FINAL.xlsx")

# read land use change data
options(timeout = 1000)
url <- "https://storage.googleapis.com/mapbiomas-public/brasil/downloads/1-tabela-geral-col7-mapbiomas-biomas-municipio-final.xlsx"
download.file(url, "./input/1-tabela-geral-col7-mapbiomas-biomas-municipio-final.xlsx")
rawdata <- read_excel("./input/1-tabela-geral-col7-mapbiomas-biomas-municipio-final.xlsx",
                  sheet = "TRANSICAO_COL7")

# read soy production data per municipality
for(year in 1990:2020){
  url <- paste0("https://sidra.ibge.gov.br/geratabela?format=xlsx&name=tabela1612.xlsx&terr=N&rank=-&query=t/1612/n6/all/v/109,112,214,216/p/",year,"/c81/2713/l/v,c81,t%2Bp")
  download.file(url, paste0("./input/ibge_production_data_",year,".xlsx"))
}
options(timeout = 60)

conc <- read_csv("./conc.csv") %>% select(-level_4)

# check transitions from soybean
# select(rawdata, "to_level_4", "from_level_4", "1985-1986":"2020-2021") %>% 
#   pivot_longer(cols = c(-from_level_4, -to_level_4)) %>% 
#   separate(name, c("from","to"), sep = "-", convert = TRUE) %>% 
#   group_by(from_level_4, to_level_4, from, to) %>% 
#   summarise(value = sum(value, na.rm = TRUE)) %>% 
#   mutate(timespan = to - from) %>% 
#   filter(timespan %in% c(1,5,10), from >= 1990, from %% timespan == 0) %>% 
#   filter(from_level_4=="Soy Beans") %>% 
#   group_by(to_level_4) %>% 
#   summarise(value = sum(value, na.rm = T)) %>% 
#   View()

# Prepare data ---------------------------------------------

# prod <- lapply(1991:2020, function(year){
#   read_excel(paste0("./input/ibge_production_data_",year,".xlsx"), col_names = c("municipality", "year", "tonnes"), skip = 4,
#              sheet = "Quantidade produzida (Tonela...", na = c("-", "X", "..", "...")) %>% 
#     slice(-n()) %>% 
#     mutate(state = str_extract(municipality, "\\(\\s*(.*?)\\s*\\)"),
#            state = str_remove_all(state, "\\("),
#            state = str_remove_all(state, "\\)"),
#            municipality = str_remove_all(municipality, " \\(\\s*(.*?)\\s*\\)"),
#            tonnes = ifelse(is.na(tonnes), 0, tonnes),
#            year = as.integer(year),
#            timespan = 1)
# }) %>% bind_rows()
# 
# prod_5y <- prod %>% 
#   group_by(state, municipality, from = cut(year, seq(1990, 2020, 5))) %>% 
#   summarise(tonnes = sum(tonnes),
#             timespan = 5) %>% 
#   mutate(from = as.character(from),
#          from = str_extract(from, "\\(\\s*(.*?)\\s*,"),
#          from = str_remove_all(from, "\\("),
#          from = str_remove_all(from, ","),
#          from = as.integer(from),
#          to = from + 5)
# 
# prod_10y <- prod %>% 
#   group_by(state, municipality, from = cut(year, seq(1990, 2020, 10))) %>% 
#   summarise(tonnes = sum(tonnes),
#             timespan = 10) %>% 
#   mutate(from = as.character(from),
#          from = str_extract(from, "\\(\\s*(.*?)\\s*,"),
#          from = str_remove_all(from, "\\("),
#          from = str_remove_all(from, ","),
#          from = as.integer(from),
#          to = from + 10)
# 
# prod <- bind_rows(prod %>% mutate(to = year, from = year - 1) %>% select(-year), prod_5y, prod_10y)
# 
# 
# characters <- c("á", "Á", "é", "É", "í", "Í", "ó", "Ó", "ú", "Ú", "ã", "õ", "à", "À", "ò", "Ò", "è", "È", 
#                 "ì", "Ì", "ù", "Ù", "ç", "â", "Â", "ê", "Ê", "ô", "Ô")
# prod <- prod %>%
#   mutate(municipality = ifelse(municipality == "Pirajuí", "Pirajui", municipality))
# prod <- prod %>% 
#   mutate(municipality = str_remove_all(municipality, str_c(characters, collapse = "|")))


luc <- select(rawdata, state = "UF", "municipality", "from_class", "to_class", "1985-1986":"2020-2021") %>% 
  left_join(conc, by = c("from_class" = "level_id")) %>% 
  left_join(conc, by = c("to_class" = "level_id"), suffix = c("_from", "_to")) %>% 
  select(-to_class, -from_class) %>% 
  pivot_longer(cols = c(-state, -municipality, -class_to, -class_from)) %>% 
  separate(name, c("from","to"), sep = "-", convert = TRUE) %>% 
  group_by(state, municipality, class_from, class_to, from, to) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  mutate(timespan = to - from) %>% 
  filter(timespan %in% c(1,5,10), from >= 1990, from %% timespan == 0, to < 2021) %>% 
  filter(!municipality %in% c("Pirajui","Tapira","Curu")) %>% # these municipalities are ambiguous
  filter(!class_from %in% c("Non Observed", "Water"), 
         !class_to %in% c("Non Observed", "Water"),
         class_from != class_to) %>% 
  mutate(trans = str_c(class_from,"_",class_to))


# indirect deforestation ------------------------------------------
# assumption: indirect deforestation of soy can be seen as the amount of land soy took form pastures or other crops for which forests were converted
# spatial levels of analysis: 
# l1 = municipality
# l2 = state
# l3 = country
data_l1 <- luc %>%
  select(-class_from, -class_to, -to) %>% 
  filter(trans %in% c("Forest_Soy Beans", "Forest_Pasture", "Forest_Other Crops", "Pasture_Soy Beans", "Other Crops_Soy Beans")) %>% 
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>% 
  mutate(direct = `Forest_Soy Beans`,
         indirect = ifelse(`Pasture_Soy Beans` < Forest_Pasture, `Pasture_Soy Beans`, Forest_Pasture) + 
           ifelse(`Other Crops_Soy Beans` < `Forest_Other Crops`, `Other Crops_Soy Beans`, `Forest_Other Crops`)) %>% 
  select(state, municipality, from, timespan, direct, indirect) %>% 
  mutate(timespan = str_pad(timespan, width = 2, side = "left", pad = "0")) %>% 
  pivot_wider(names_from = timespan, values_from = c(direct, indirect)) %>% 
  group_by(state, municipality, cut5 = cut(from, seq(1990, 2020, 5), include.lowest = T, right = F)) %>% 
  mutate(direct_05 = min(direct_05, na.rm = TRUE) / n(), 
         indirect_05 = min(indirect_05, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  group_by(state, municipality, cut10 = cut(from, seq(1990, 2020, 10), include.lowest = T, right = F)) %>% 
  mutate(direct_10 = min(direct_10, na.rm = TRUE) / n(), 
         indirect_10 = min(indirect_10, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  select(-state, -municipality, -cut5, -cut10) %>% 
  group_by(from) %>% 
  summarise(across(everything(), sum), .groups = "drop") %>% 
  pivot_longer(-from)

data_l2 <- luc %>%
  select(-class_from, -class_to, -to, -municipality) %>% 
  group_by(state, from, timespan, trans) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  filter(trans %in% c("Forest_Soy Beans", "Forest_Pasture", "Forest_Other Crops", "Pasture_Soy Beans", "Other Crops_Soy Beans")) %>% 
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>% 
  mutate(direct = `Forest_Soy Beans`,
         indirect = ifelse(`Pasture_Soy Beans` < Forest_Pasture, `Pasture_Soy Beans`, Forest_Pasture) + 
           ifelse(`Other Crops_Soy Beans` < `Forest_Other Crops`, `Other Crops_Soy Beans`, `Forest_Other Crops`)) %>% 
  select(state, from, timespan, direct, indirect) %>% 
  mutate(timespan = str_pad(timespan, width = 2, side = "left", pad = "0")) %>% 
  pivot_wider(names_from = timespan, values_from = c(direct, indirect)) %>% 
  group_by(state, cut5 = cut(from, seq(1990, 2020, 5), include.lowest = T, right = F)) %>% 
  mutate(direct_05 = min(direct_05, na.rm = TRUE) / n(), 
         indirect_05 = min(indirect_05, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  group_by(state, cut10 = cut(from, seq(1990, 2020, 10), include.lowest = T, right = F)) %>% 
  mutate(direct_10 = min(direct_10, na.rm = TRUE) / n(), 
         indirect_10 = min(indirect_10, na.rm = TRUE) / n()) %>% 
  ungroup() %>% 
  select(-state, -cut5, -cut10) %>% 
  group_by(from) %>% 
  summarise(across(everything(), sum), .groups = "drop") %>% 
  pivot_longer(-from)

data_l3 <- luc %>%
  select(-class_from, -class_to, -to, -municipality, -state) %>% 
  group_by(from, timespan, trans) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>% 
  filter(trans %in% c("Forest_Soy Beans", "Forest_Pasture", "Forest_Other Crops", "Pasture_Soy Beans", "Other Crops_Soy Beans")) %>% 
  pivot_wider(names_from = trans, values_from = value, values_fill = 0) %>% 
  mutate(direct = `Forest_Soy Beans`,
         indirect = ifelse(`Pasture_Soy Beans` < Forest_Pasture, `Pasture_Soy Beans`, Forest_Pasture) + 
           ifelse(`Other Crops_Soy Beans` < `Forest_Other Crops`, `Other Crops_Soy Beans`, `Forest_Other Crops`)) %>% 
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
  pivot_wider(names_from = name, values_from = c(value_l1, value_l2, value_l3)) %>% 
  arrange(from) %>% 
  mutate(across(starts_with("value_"), .fns = list(mean_05 = mean_roll_05, 
                                                   mean_10 = mean_roll_10, 
                                                   mean_20 = mean_roll_20))) %>% 
  pivot_longer(cols = -from) %>% 
  separate(name, into = c("aux1","spatial_level", "def_path","timelag","aux2","amortisation"), sep = "_", fill = "right") %>% 
  select(-aux1, -aux2) %>% 
  mutate(amortisation = ifelse(is.na(amortisation), "01", amortisation),
         spatial_level = factor(spatial_level, levels = c("l1", "l2", "l3"), labels = c("municipality", "state", "country")),
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



