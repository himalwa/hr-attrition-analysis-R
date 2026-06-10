library(tidyverse)

hr <- read.csv("data/raw/train.csv")

head(hr)
glimpse(hr)
summary(hr)

# missval
colSums(is.na(hr))
#dups
sum(duplicated(hr))
#uniq
sapply(hr, function(x) length(unique(x)))
