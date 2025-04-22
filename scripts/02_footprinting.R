
# some labels and settings
allocation <- "value"
io <- fread(paste0(input_path,"io_labels.csv"))
items <- fread(file=paste0(input_path,"items.csv"))

# load data
Z <- readRDS(file=paste0(input_path,"Z_",allocation,".rds"))

for (i in c("Soyabeans", "Soyabean Oil", "Soyabean Cake")) {
  Zi <- cbind(io,Z[["2019"]][which(io$iso3c == "BRA" & io$item == i),])
  Zi <- Zi[,-c("area_code", "comm_code", "item_code")]
  print(sum(Zi[iso3c == 'BRA', V2]) / sum(Zi[, V2]))
}


### Ghosh model with gloria ### forward linkage connections
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v1.2/current/"

# useful function
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x));  return(x) }

# some labels
items <- fread(file=paste0(input_path,"items.csv"))
io <- fread(paste0(input_path,"io_labels.csv"))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))
regions <- fread(file=paste0(input_path,"regions.csv"))

# some settings
allocation <- "value"
years <- 2010:2021
year <- 2019


# Read data
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
Y <- readRDS(file=paste0(input_path,"losses/Y.rds"))

# TOTAL LINKAGE (remove row and column)
L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
Yi <- Y[[as.character(year)]]
colnames(Yi) <- fd$iso3c
Yi <- agg(Yi)

# original output per country
X0 <- L %*% Yi

# filling zeroes
L[, which(io$iso3c == "BRA" & io$item == "Soyabeans")] <- 0
L[which(io$iso3c == "BRA" & io$item == "Soyabeans"),] <- 0
L[which(io$iso3c == "BRA" & io$item == "Soyabeans"), which(io$iso3c == "BRA" & io$item == "Soyabeans")] <- 1
Yi[which(io$iso3c == "BRA" & io$item == "Soyabeans"),] <- 0

X1 <- L %*% Yi

dx <- (X1 - X0) / X0 * 100

df <- dx[which(io$iso3c %in% regions[EU27 == TRUE][, iso3c]), which(colnames(dx) %in% regions[EU27 == TRUE][, iso3c])]
df <- as.matrix(df)
df <- cbind(io[iso3c %in% regions[EU27 == TRUE][, iso3c]], df)
