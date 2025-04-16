
allocation <- "value"
io <- fread(paste0(input_path,"io_labels.csv"))
items <- fread(file=paste0(input_path,"items.csv"))
Z <- readRDS(file=paste0(input_path,"Z_",allocation,".rds"))

for (i in c("Soyabeans", "Soyabean Oil", "Soyabean Cake")) {
  Zi <- cbind(io,Z[["2019"]][which(io$iso3c == "BRA" & io$item == "Soyabean Oil"),])
  Zi <- Zi[,-c("area_code", "comm_code", "item_code")]
  print(sum(Zi[iso3c == 'BRA', V2]) / sum(Zi[, V2]))
}

