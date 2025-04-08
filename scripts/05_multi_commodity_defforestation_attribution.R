
library(readxl)
library(openxlsx)

# set working directory
setwd("~/projects/FAO-deforestation/")

agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# read deforestation data
def <- read_excel("input/DeDuCE_Deforestation_attribution_v1.0.1 (2001-2022).xlsx")
def <- def[, c(1:8)]


# Read labels of FABIO
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"
regions <- fread(file=paste0(input_path,"regions.csv"))
items <- fread(file=paste0(input_path,"items.csv"))
nrreg <- nrow(regions)
nrcom <- nrow(items)
io <- fread(paste0(input_path,"io_labels.csv"))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))

# Read FABIO data
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
Y <- readRDS(file=paste0(input_path,"losses/Y.rds"))
E <- readRDS(file=paste0(input_path,"E.rds"))


# finding intersections
#commodities_fabio <- items %>%
#  filter(group == "Primary crops") %>%
#  select(item) %>%
#  pull()


#commodities_ch <- unique(def$Commodity)

# commodity matches
commodities <- read_excel("input/deforestation_commodities.xlsx")

# keeping deforestation accounts of only specific commodities
def <- def %>%
  filter(Commodity %in% commodities$ch & Year == 2019)

# getting biomass production of selected products
# quick settings
years <- 2019


prod <- list()
for (year in years) {
  print(year)
  
  for (commodity in commodities$fabio) {
    a <- E[[as.character(year)]][item == commodity, biomass]
    region <- E[[as.character(year)]][item == commodity, area]
    
    prod[[length(prod) + 1]] <- data.table(
      year = year,
      region = region,
      commodity = commodity,
      output = a)
  }
}

prod <- rbindlist(prod)

prod <- prod %>%
  left_join(regions[, c(1,3)], by = c("region"="area"))


# now we get the intensities vector
def1 <- def %>%
  left_join(commodities, by = c("Commodity" = "ch")) %>%
  rename(iso3c = ISO,
         year = Year,
         commodity = fabio) %>%
  select(iso3c, year, commodity, `Deforestation attribution, unamortized (ha)`,
         `Deforestation risk, amortized (ha)`) %>%
  left_join(prod, by = c("iso3c", "year", "commodity"), relationship = "many-to-many") %>%
  mutate(e_un = if_else(output > 0, `Deforestation attribution, unamortized (ha)` / output, 0),
         e_a = if_else(output > 0, `Deforestation risk, amortized (ha)` / output, 0)) %>%
  filter(!is.na(output))



# Make settings for footprinting
extension <- "biomass"
allocation <- "value"
consumption <- c("food", "other")
years <- 2019
countries <- regions$iso3c
crops <- commodities$fabio



results_all <- data.table()
results_country <- list()

for (year in years) {
  print(year)
  L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
  Xi <- X[, as.character(year)]
  
  # Prepare extension and final demand
  ext <- as.numeric(unlist(E[[as.character(year)]][, ..extension])) / as.vector(Xi)
  ext[!is.finite(ext)] <- 0
  MP <- ext * L
  
  # Calculate footprints
    
  for(fd_type in consumption){
    
    Yi <- Y[[as.character(year)]][, fd$fd == fd_type]
    fd_subset <- fd[fd == fd_type,]
    
    for (country in countries) {
      print(paste(fd_type, country))
      FP <- t(t(MP) * Yi[, fd_subset$iso3c==country])
      colnames(FP) <- paste0(io$iso3c, "_", io$item)
      rownames(FP) <- paste0(io$iso3c, "_", io$item)
      FP <- as(FP, "TsparseMatrix")
      results <- data.table(source=rownames(FP)[FP@i + 1], target=colnames(FP)[FP@j + 1], value=FP@x)
      results[,`:=`(country_origin = substr(source,1,3),
                    crop = substr(source,5,100),
                    year = year,
                    country_imd = substr(target,1,3),
                    product = substr(target,5,100),
                    country_target = country,
                    consumption_category = fd_type)]
      
      results[,`:=`(group = items$comm_group[match(results$product,items$item)],
                    continent_origin = regions$continent[match(results$country_origin, regions$iso3c)],
                    continent_imd = regions$continent[match(results$country_imd, regions$iso3c)],
                    continent_target = regions$continent[match(results$country_target, regions$iso3c)])]
      
      results_country[[country]] <- results[round(value, 3) != 0]
              
    }
    
    results_all <- rbind(results_all, rbindlist(results_country))
  }
}

results_all <- results_all %>%
  select(country_origin, crop, country_imd, continent_final, product, group_final,
         country_target, continent_target, consumption_category, value)

colnames(results_all)[c(4,6)] <- c("continent_imd", "group")

#saveRDS(results_all, "./output/detailed-50-crop-def-fp.rds")
results_all <- readRDS("./output/detailed-60-crop-biomass-fp.rds")



def2 <- def1 %>% filter(year == 2019) %>% select(iso3c, commodity, e_un, e_a) %>%
  rename(country_origin = iso3c,
         crop = commodity)


results_all <- results_all %>%
  left_join(def2, by = c("country_origin", "crop"), relationship = "many-to-many") %>%
  mutate(d_f_a = value * e_a)

results_all <- results_all[(d_f_a) != 0 & !is.na(d_f_a) & is.finite(d_f_a)]
results_all <- results_all[round(d_f_a,4) != 0]
results_all[, c("value", "e_un", "e_a") := NULL]

head(results_all)

#saveRDS(results_all, file="./output/global_results_v2.rds")