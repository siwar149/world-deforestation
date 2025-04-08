rm(list = ls())
gc()

library(tidyverse)

# Load data
cr <- as.data.table(readRDS("./output/deforestation_ctd.rds"))
results <- readRDS("./output/case_study_results.rds")

# Filter only desired observations
cr <- cr[`Spatial level` == "country" & from %between% c(2010, 2021)]

input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"
E <- readRDS(file=paste0(input_path,"E.rds"))

# settings
years <- 2010:2021
case_studies <- data.table(name = c("BRA_soy", "IDN_palm"), 
                           area = c("Brazil", "Indonesia"),
                           item = c("Soyabeans", "Oil, palm fruit"))

# Getting biomass production
prod <- list()

for (year in years) {
  print(year)
  
  for (case in case_studies$name) {
    
    a <- E[[as.character(year)]][area == case_studies[name == case, area] & item == case_studies[name == case, item], biomass]
    
    prod[[length(prod) + 1]] <- data.table(
      case = case,
      year = year,
      x = a
    )
  }
  
}

prod <- rbindlist(prod)
          
cr <- left_join(cr, prod, by = c("year", "case")) %>%
  mutate(e = value / x)

colnames(data)[2] <- "case"

cr <- cr %>%
  left_join(data, by = c("case", "year"), relationship = "many-to-many") %>%
  mutate(fp = e * value.y)


# direct
gp <- cr %>% 
  filter(def_path == "Direct" & case == "BRA_soy") %>% 
  ggplot(aes(x = year, y = fp, colour = continent_target)) + 
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

# indirect
gp <- cr %>% 
  filter(def_path == "Indirect" & case == "BRA_soy") %>% 
  ggplot(aes(x = year, y = fp, colour = continent_target)) + 
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


# general graph
gp <- cr %>% 
  filter(case == "BRA_soy") %>% 
  group_by(year, continent_target, timelag, amortisation) %>% 
  summarise(value = sum(fp), .groups = "drop") %>% 
  ggplot(aes(x = year, y = value, colour = continent_target)) + 
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


# graphs for policy brief
gp <- cr %>% 
  filter(case == "BRA_soy" & amortisation == "01" & timelag == "01" & continent_target != "ROW") %>% 
  group_by(year, continent_target, timelag, amortisation) %>% 
  summarise(value = sum(fp), .groups = "drop") %>% 
  ggplot(aes(x = year, y = value, colour = continent_target)) + 
  theme_bw() + 
  #facet_grid(vars(timelag), vars(amortisation), labeller = label_both) +
  geom_line(size = 1.2) +  # Increased line thickness
  scale_color_viridis_d(name = "Continents") +  # Changed legend title
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.01)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), labels = as.integer) + 
  xlab("Year") +
  ylab("Deforestation (M ha)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = -45, hjust = 0, color = "black", size = 14),  # Larger x-axis ticks
    axis.text.y = element_text(color = "black", size = 14),  # Larger y-axis ticks
    axis.title.x = element_text(face = "bold", size = 16),  # Bold, larger x-axis label
    axis.title.y = element_text(face = "bold", size = 16),  # Bold, larger y-axis label
    legend.text = element_text(size = 14),  # Larger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Larger, bold legend title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Larger plot title, centered
    plot.subtitle = element_text(size = 14, hjust = 0.5),  # Larger subtitle
    legend.margin = margin(-10, 0, 0, 0),
    panel.border = element_rect(size = 1.5, colour = "black")  # Thicker plot border
  )

ggplot2::ggsave(plot = gp, bg = "#ffffff",
                filename = "./output/fig-1-soy-deforestation-lag1-amort1.png",
                width = 240, height = 140, units = "mm", scale = 1)



gp <- cr %>% 
  filter(case == "BRA_soy" & amortisation == "20" & timelag == "10" & continent_target != "ROW") %>% 
  group_by(year, continent_target, timelag, amortisation) %>% 
  summarise(value = sum(fp), .groups = "drop") %>% 
  ggplot(aes(x = year, y = value, colour = continent_target)) + 
  theme_bw() + 
  #facet_grid(vars(timelag), vars(amortisation), labeller = label_both) +
  geom_line(size = 1.2) +  # Increased line thickness
  scale_color_viridis_d(name = "Continents") +  # Changed legend title
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.01)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10), labels = as.integer) + 
  xlab("Year") +
  ylab("Deforestation (M ha)") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = -45, hjust = 0, color = "black", size = 14),  # Larger x-axis ticks
    axis.text.y = element_text(color = "black", size = 14),  # Larger y-axis ticks
    axis.title.x = element_text(face = "bold", size = 16),  # Bold, larger x-axis label
    axis.title.y = element_text(face = "bold", size = 16),  # Bold, larger y-axis label
    legend.text = element_text(size = 14),  # Larger legend text
    legend.title = element_text(size = 16, face = "bold"),  # Larger, bold legend title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Larger plot title, centered
    plot.subtitle = element_text(size = 14, hjust = 0.5),  # Larger subtitle
    legend.margin = margin(-10, 0, 0, 0),
    panel.border = element_rect(size = 1.5, colour = "black")  # Thicker plot border
  )

ggplot2::ggsave(plot = gp, bg = "#ffffff",
                filename = "./output/fig-2-soy-deforestation-lag10-amort20.png",
                width = 240, height = 140, units = "mm", scale = 1)


# barplot
am <- c("01", "20"); tr <- c("01", "10")

cr %>% 
  filter(case == "BRA_soy" & amortisation %in% am & timelag %in% tr & year == 2019) %>% 
  group_by(continent_target, timelag, amortisation) %>% 
  summarise(value = sum(fp), .groups = "drop") %>%
  mutate(id = paste(timelag, amortisation)) %>%
  filter(id %in% c("01 01", "10 20") & continent_target != "ROW") %>%
  select(-id) %>%
  mutate(type = paste0("Amortization ", amortisation, "; Transiton lag ", timelag)) %>%
  ggplot(aes(x = continent_target, y = value, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.01)) + 
  labs(
    x = "Continent",
    y = "Deforestation (M ha)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text = element_text(colour = "black", size = 14),
        axis.title = element_text(colour = "black", size = 14),
        legend.text = element_text(colour = "black", size = 12))
  
ggsave(plot = last_plot(), bg = "#ffffff",
         filename = "./output/fig-3-comparison-deforestaion.png",
         width = 240, height = 140, units = "mm", scale = 1)



data %>%
  filter(year == 2019 & case == "IDN_palm") %>%
  ggplot(aes(x = continent_target, y = value)) +
  geom_bar(stat = "identity", position = "dodge")
