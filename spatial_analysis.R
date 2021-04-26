# initialize environment
rm(list = ls());

library(data.table)
library(multiplex)
library(crayon)
library(magrittr)
library(dplyr)
library(poppr)
library(ggplot2)
library(adegenet)
library(tidyr)
library(ade4)
library(adegenet)
library(adespatial)
library(spdep)
library(showtext)
#library(adehabitat)

# readin data
LT_A <- read.genalex("/Users/han/data/SCU/cmj_project/LT/LT_A/Sheet1-Table 1_with_clone.csv", ploidy = 2,geo = TRUE)
LT_B <- read.genalex("/Users/han/data/SCU/cmj_project/LT/LT_B/Sheet1-Table 1_with_clone.csv", ploidy = 2,geo = TRUE)
LT_C <- read.genalex("/Users/han/data/SCU/cmj_project/LT/LT_C/Sheet1-Table 1.csv", ploidy = 2,geo = TRUE)
ZG_A <- read.genalex('/Users/han/data/SCU/cmj_project/ZG/ZG_A/Sheet1-Table 1_with_clone.csv', ploidy = 2, geo = TRUE)
ZG_B <- read.genalex('/Users/han/data/SCU/cmj_project/ZG/ZG_B/Sheet1-Table 1_with_clone.csv', ploidy = 2, geo = TRUE)
ZG_C <- read.genalex('/Users/han/data/SCU/cmj_project/ZG/ZG_C/Sheet1-Table 1_with_clone.csv', ploidy = 2, geo = TRUE)

LT_A0 <- fread("/Users/han/data/SCU/cmj_project/LT/LT_A/Sheet1-Table 1_with_clone.csv")
LT_B0 <- fread("/Users/han/data/SCU/cmj_project/LT/LT_B/Sheet1-Table 1_with_clone.csv")
LT_C0 <- fread("/Users/han/data/SCU/cmj_project/LT/LT_C/Sheet1-Table 1.csv")
ZG_A0 <- fread('/Users/han/data/SCU/cmj_project/ZG/ZG_A/Sheet1-Table 1_with_clone.csv')
ZG_B0 <- fread('/Users/han/data/SCU/cmj_project/ZG/ZG_B/Sheet1-Table 1_with_clone.csv')
ZG_C0 <- fread('/Users/han/data/SCU/cmj_project/ZG/ZG_C/Sheet1-Table 1_with_clone.csv')

#### draw distribution map
## draw LT_A
LT_A0<-LT_A0[4:nrow(LT_A0),]
# find mixed patches
aa<-LT_A0 %>% 
  group_by(LT_A0[,32:33]) %>% 
  mutate(mixed = n(),dup_id = row_number()) %>% 
  ungroup() 
# drop duplicated coordinates
aa<-filter(aa,aa[,35]==1)

# find clones
aa<-aa %>% 
  group_by(aa[,3:30]) %>% 
  mutate(clone = n(),clone_id = row_number()) %>% 
  ungroup() 

aa$clone_marker<-rep(c(0,1,0,2,0), c(1,3,20,3,30)) # manually add markers of clones
aa[, c(32:33)] <- sapply(aa[, c(32:33)], as.numeric)# turn coordinates into numeric type for plot

# set up fonts if you like, here I use Helvetica
font_add("Helvetica", '/System/Library/Fonts/Helvetica.ttc')


LT_A_P<-ggplot(aa,aes(V32,V33))+
  geom_point(aes(colour = factor(clone_marker),shape = factor(mixed)), size = 2.4, show.legend = T)+
  coord_fixed()+
  #scale_shape_manual(values = c(1,0,2,3,8))+
  scale_colour_manual(values = c("#6E6B6A", "#F5BC8E", "#DF8CC1"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(linetype = 'solid', fill = NA), 
        plot.title = element_text(colour = "black", family  = "Helvetica", size = 11), 
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
        text = element_text(family="Helvetica"))+
  expand_limits(y=c(0,30),x=c(0,100))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = '',y = '')+
  ggtitle('LT_A (4560m)')+
  guides(color=F, shape=guide_legend(title='x-mixed patch'))



## draw LT_B
LT_B0<-LT_B0[4:nrow(LT_B0),]
# find mixed patches
bb<-LT_B0 %>% 
  group_by(LT_B0[,32:33]) %>% 
  mutate(mixed = n(),dup_id = row_number()) %>% 
  ungroup() 
# drop duplicated coordinates
bb<-filter(bb,bb[,35]==1)

# find clones
bb<-bb %>% 
  group_by(bb[,3:30]) %>% 
  mutate(clone = n(),clone_id = row_number()) %>% 
  ungroup() 

bb$clone_marker<-rep(c(0,1,0), c(22,2,35)) # manually add markers of clones
bb[, c(32:33)] <- sapply(bb[, c(32:33)], as.numeric)# turn coordinates into numeric type for plot



LT_B_P<-ggplot(bb,aes(V32,V33))+
  geom_point(aes(colour = factor(clone_marker),shape = factor(mixed)), size = 2.4, show.legend = T)+
  coord_fixed()+
  scale_colour_manual(values = c("#6E6B6A", "#3DDAF5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(linetype = 'solid', fill = NA), 
        plot.title = element_text(colour = "black", family  = "Helvetica", size = 11), 
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
        text = element_text(family="Helvetica"))+
  expand_limits(y=c(0,30),x=c(0,100))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = '',y = '')+
  ggtitle('LT_B (4430m)')+
  guides(color=F, shape=guide_legend(title='x-mixed patch'))


## draw LT_C
LT_C0<-LT_C0[4:nrow(LT_C0),]
# find mixed patches
cc<-LT_C0 %>% 
  group_by(LT_C0[,32:33]) %>% 
  mutate(mixed = n(),dup_id = row_number()) %>% 
  ungroup() 
# drop duplicated coordinates
cc<-filter(cc,cc[,35]==1)

# find clones, since LT_C got no clone, I skipped this step
#cc<-cc %>% 
#  group_by(cc[,3:30]) %>% 
#  mutate(clone = n(),clone_id = row_number()) %>% 
#  ungroup() 

cc[, c(32:33)] <- sapply(cc[, c(32:33)], as.numeric)# turn coordinates into numeric type for plot



LT_C_P<-ggplot(cc,aes(V32,V33))+
  geom_point(aes(colour = factor(dup_id),shape = factor(mixed)), size = 2.4, show.legend = T)+
  coord_fixed()+
  scale_colour_manual(values = c("#6E6B6A"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(linetype = 'solid', fill = NA), 
        plot.title = element_text(colour = "black", family  = "Helvetica", size = 11), 
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
        text = element_text(family="Helvetica"))+
  expand_limits(y=c(0,30),x=c(0,100))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = '',y = '')+
  ggtitle('LT_C (4280m)')+
  guides(color=F, shape=guide_legend(title='x-mixed patch'))

## draw ZG_A
ZG_A0<-ZG_A0[4:nrow(ZG_A0),]
# find mixed patches
dd<-ZG_A0 %>% 
  group_by(ZG_A0[,32:33]) %>% 
  mutate(mixed = n(),dup_id = row_number()) %>% 
  ungroup() 
# drop duplicated coordinates
dd<-filter(dd,dd[,35]==1)

# find clones
dd<-dd %>% 
  group_by(dd[,3:30]) %>% 
  mutate(clone = n(),clone_id = row_number()) %>% 
  ungroup() 

dd$clone_marker<-rep(c(0,1,0,2,3,0,4,0,5,0,6,0,7,8,0,9,0), c(1,2,6,2,4,4,2,1,2,6,2,11,2,2,5,3,3)) # manually add markers of clones
dd[, c(32:33)] <- sapply(dd[, c(32:33)], as.numeric)# turn coordinates into numeric type for plot

ZG_A_P<-ggplot(dd,aes(V32,V33))+
  geom_point(aes(colour = factor(clone_marker),shape = factor(mixed)), size = 2.4, show.legend = T)+
  coord_fixed()+
  scale_colour_manual(values = c("#6E6B6A", "#FFE28F","#ABD99E","#B3E9F5","#B997DE","#FFC1B7","#FF5500","#FF1288","#DBD140","#263FE0"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(linetype = 'solid', fill = NA), 
        plot.title = element_text(colour = "black", family  = "Helvetica", size = 11), 
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
        text = element_text(family="Helvetica"))+
  expand_limits(y=c(0,30),x=c(0,100))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = '',y = '')+
  ggtitle('ZG_A (4550m)')+
  guides(color=F, shape=guide_legend(title='x-mixed patch'))



## draw ZG_B
ZG_B0<-ZG_B0[4:nrow(ZG_B0),]
# find mixed patches
ee<-ZG_B0 %>% 
  group_by(ZG_B0[,32:33]) %>% 
  mutate(mixed = n(),dup_id = row_number()) %>% 
  ungroup() 
# drop duplicated coordinates
ee<-filter(ee,ee[,35]==1)

# find clones
ee<-ee %>% 
  group_by(ee[,3:30]) %>% 
  mutate(clone = n(),clone_id = row_number()) %>% 
  ungroup() 

ee$clone_marker<-rep(c(0,1,0,2,0,3,0), c(10,2,6,2,1,2,20)) # manually add markers of clones
ee[, c(32:33)] <- sapply(ee[, c(32:33)], as.numeric)# turn coordinates into numeric type for plot

ZG_B_P<-ggplot(ee,aes(V32,V33))+
  geom_point(aes(colour = factor(clone_marker),shape = factor(mixed)), size = 2.4, show.legend = T)+
  coord_fixed()+
  scale_colour_manual(values = c("#6E6B6A", "#DB7976","#A29EFF","#B0FFCC"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(linetype = 'solid', fill = NA), 
        plot.title = element_text(colour = "black", family  = "Helvetica", size = 11), 
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
        text = element_text(family="Helvetica"))+
  expand_limits(y=c(0,30),x=c(0,100))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = '',y = '')+
  ggtitle('ZG_B (4420m)')+
  guides(color=F, shape=guide_legend(title='x-mixed patch'))


## draw ZG_C
ZG_C0<-ZG_C0[4:nrow(ZG_C0),]
# find mixed patches
ff<-ZG_C0 %>% 
  group_by(ZG_C0[,32:33]) %>% 
  mutate(mixed = n(),dup_id = row_number()) %>% 
  ungroup() 
# drop duplicated coordinates
ff<-filter(ff,ff[,35]==1)

# find clones
ff<-ff %>% 
  group_by(ff[,3:30]) %>% 
  mutate(clone = n(),clone_id = row_number()) %>% 
  ungroup() 

ff$clone_marker<-rep(c(0,1,0,2,0,3,0,4,0,5,0,6,0,7,0), c(1,2,2,3,5,2,1,2,12,2,9,2,1,2,10)) # manually add markers of clones
ff[, c(32:33)] <- sapply(ff[, c(32:33)], as.numeric)# turn coordinates into numeric type for plot

ZG_C_P<-ggplot(ff,aes(V32,V33))+
  geom_point(aes(colour = factor(clone_marker),shape = factor(mixed)), size = 2.4, show.legend = T)+
  coord_fixed()+
  scale_colour_manual(values = c("#6E6B6A", "#E06A41","#75DFFF","#4BDB44","#F5D238","#D63DFF","#D8FF92","#5800F5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        panel.border = element_rect(linetype = 'solid', fill = NA), 
        plot.title = element_text(colour = "black", family  = "Helvetica", size = 11), 
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
        text = element_text(family="Helvetica"))+
  expand_limits(y=c(0,30),x=c(0,100))+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  labs(x = '',y = '')+
  ggtitle('ZG_C (4270m)')+
  guides(color=F, shape=guide_legend(title='x-mixed patch'))

ZG_A_P

### spatial analysis
## take ZG_C as example
# basic info
gac <- genotype_curve(ZG_C, sample = 1000, quiet = TRUE) # The genotype accumulation curve
(pinflt <- locus_table(ZG_C)) # summary
info_table(ZG_C, type = "missing", plot = TRUE) # a look at missing data
poppr(ZG_C) # diversity summary
P.tab <- mlg.table(ZG_C)

# a look in spatial distribution
plot(ZG_C$other$xy)
s.kde2d(ZG_C$other$xy, add.plot = TRUE)

# try plot ob vs expected heterozygosity
ZG_C.smry<-summary(ZG_C)
plot(ZG_C.smry$Hexp, ZG_C.smry$Hobs, main = "Observed vs expected heterozygosity")
abline(0, 1, col = "red")
t.test(ZG_C.smry$Hexp, ZG_C.smry$Hobs, paired = TRUE, var.equal = TRUE)

# seek a global picture of the genetic diversity among genotypes using PCA
ZG_C.X <- scaleGen(ZG_C, NA.method = "zero") %>% as.data.frame()
ZG_C.pca1 <- dudi.pca(ZG_C.X, cent = FALSE, scale = FALSE, scannf = FALSE, nf = 2)
barplot(ZG_C.pca1$eig)
ZG_C.pca1
s.label(ZG_C.pca1$li) # add labels
s.kde2d(ZG_C.pca1$li, add.plot = TRUE, cpoint = 0)
add.scatter.eig(ZG_C.pca1$eig, 2, 1, 2)

# visualize the contribution of each allele
loadingplot(ZG_C.pca1$c1^2)
## to check whether the highlighted genotypes are uncommon, use truenames()
## X <- truenames(ZG_C)
### here I take cmj43.332 as an example
## cmj43.332 <-  X[, 'CMJ-43.332']
## table(cmj43.332)
## rownames(X)[cmj43.332 == t]  # here t == the value in table(cmj43.332)

# Mapping and testing PCA results
s.value(cbind(1:11, rep(1, 11)), -5:5, cleg = 0)
text(1:11, rep(1, 11), -5:5, col = "red", cex = 1.5)
plot(ZG_C$other$xy)
s.value(ZG_C$other$xy, ZG_C.pca1$li[, 1], add.p = TRUE, cleg = 0.5)
title("PCA - first PC", col.main = "yellow", line = -2, cex.main = 2)
plot(ZG_C$other$xy)
s.value(ZG_C$other$xy, ZG_C.pca1$li[, 2], add.plot = T, csize = 0.7)
title("PCA - second PC", col.main = "yellow", line = -2, cex.main = 2)
# create a connection network based on distance range
ZG_C.graph <- chooseCN(ZG_C$other$xy, type = 5, d1=0, d2=15, k=55, plot = FALSE, res = "listw")
ZG_C.graph
plot(ZG_C.graph, ZG_C$other$xy)
title("ZG_C.graph")

# Perform Moran’s test for the first two PCs, and plot the results
pc1.mctest <- moran.mc(ZG_C.pca1$li[, 1], ZG_C.graph, 999, zero.policy=TRUE)
plot(pc1.mctest) 
moran.plot(ZG_C.pca1$li[, 1], ZG_C.graph)
pc2.mctest <- moran.mc(ZG_C.pca1$li[, 2], ZG_C.graph, 999, zero.policy=TRUE)
plot(pc2.mctest)
moran.plot(ZG_C.pca1$li[, 2], ZG_C.graph)
# use Mantel test to test spatial structures in the whole data
mtest <- mantel.randtest(dist(ZG_C.X), dist(ZG_C$other$xy))
plot(mtest, nclass = 30)

# spacial PCA
ZG_C.spca1 <- multispati(ZG_C.pca1, ZG_C.graph, nfposi = 2, nfnega = 0)
barplot(ZG_C.spca1$eig, col = rep(c('red',"grey"), c(2,1000)))
ZG_C.spca1
plot(ZG_C$other$xy)
colorplot(ZG_C$other$xy, ZG_C.spca1$ls, axes = 1:2, transp = T, add = T, cex = 2)
