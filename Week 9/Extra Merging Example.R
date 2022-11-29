#EXTRA EXAMPLE OF MERGING

veg.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
veg.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Vegetation_transects")
veg <- as.data.frame(veg.tibble)
view(abiotic)
view(veg)

?merge
veg.abiotic <- merge(abiotic, veg, by = "Parcel", all = TRUE)
view(veg.abiotic)

?aggregate
ab.means <- aggregate(x = abiotic, by = list(abiotic$Parcel), FUN = "mean")
many2one <- merge(ab.means, veg, by.x = "Group.1", by.y = "Parcel", all = FALSE)


merged <- merged[,-2]
comp.merg <- complete.cases(merged)
lm(length ~ pH, data = merged[,-4])
, data = merged, norm = TRUE)
ord(merged)

#Everything has to match in an ordination.