library(xlsx)
library(readxl)

f.total <- read_xlsx("dataset_total.xlsx", 1)
training <- subset(f.total,year<2011)
