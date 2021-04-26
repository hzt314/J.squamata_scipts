rm(list = ls());
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(data.table)
cones<-fread('/Users/han/data/SCU/cmj_project/raw_data_cones_weight_001.csv',header = F)
cones[64:174,2]<-cones[64:174,2] / cones[64:174,5]
cones[64:174,3]<-cones[64:174,3] / cones[64:174,5]
col<-cones %>% count(cones$V4)
col<-rep(c('LT','ZG'),c(sum(col[1:3,2]),sum(col[4:6,2])))

ggplot(cones, aes(x=cones$V4,y=cones$V2, color = col)) +
  labs(x='plots', y='cones dry weight')+
  geom_boxplot()

ggplot(cones, aes(x=cones$V4,y=cones$V3, color = col)) +
  labs(x='plots', y='cones number')+
  geom_boxplot()

ggplot(cones, aes(x=cones$V4,y=cones$V2/cones$V3, color = col)) +
  labs(x='plots', y='averange dry weight of each cone')+
  geom_boxplot()
