setwd("C:/github/ufm") ## This is my WD in my local github desktop

library(tidyverse) ## Packages
library(readxl)

dat<-read_excel("data.xlsx",sheet="gabung")|>
  mutate(missing = if_else(if_any(everything(), is.na),1, 0))

dat|>filter(missing==1)|>print(n=300)

summary(dat$missing)

## There are around 48% missing, mostly in year 2022 (see the paper).

ea<-dat|>group_by(Province,Year)|>
  summarise(mean=mean(IDSD))