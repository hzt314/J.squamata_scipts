rm(list = ls());
### coded by Han 
### reference https://github.com/bczernecki/climate
library(ggplot2)
library(tidyverse)
library(climate)
library(climatol)

lt<-fread('/Users/han/data/SCU/cmj_project/结果图表/new/climate/LT_plot_data.csv')
zg<-fread('/Users/han/data/SCU/cmj_project/结果图表/new/climate/ZG_plot_data.csv')


diagwl(lt[,2:13], mlab = "en", 
       est = "Litang", alt = 3948.9, pcol = '#7197F5', tcol = '#FF614A', sfcol = '#42BAFF',
       per = "1981-2010", p3line = FALSE)


diagwl(zg[,2:13], mlab = "en", 
       est = "Zuogong", alt = 3781.2, pcol = '#7197F5', tcol = '#FF614A', sfcol = '#42BAFF',
       per = "1981-2010", p3line = FALSE)


