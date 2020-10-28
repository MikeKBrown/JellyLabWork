#Load Libraries
library(tidyverse)
library(vegan)
library(apcluster)
library(plyr)
library(scales)
library(ggplot2)
library(ggbiplot)
library(pairwiseAdonis)
library(ggord)
library(readr)

#Import Data
Raw_data <- read_csv()
Data <- Raw_data

#Numeric data conversions
#These need to be adjusted for each dataset.
Data$Locality <- as.character(Data$Locality) 
Data$f2 <- as.numeric(Data$f2)
Data$f3 <- as.numeric(Data$f3)
Data$f4 <- as.numeric(Data$f4)
Data$f5 <- as.numeric(Data$f5)
Data$f6 <- as.numeric(Data$f6)
Data$f7 <- as.numeric(Data$f7)
Data$f9 <- as.numeric(Data$f9)
Data$f10 <- as.numeric(Data$f10)
Data$f11 <- as.numeric(Data$f11)
Data$f12 <- as.numeric(Data$f12)
Data$f13 <- as.numeric(Data$f13)
Data$f19 <- as.numeric(Data$f19)
Data$f21 <- as.numeric(Data$f21)
Data$f22 <- as.numeric(Data$f22)
Data$f23 <- as.numeric(Data$f23)
Data$f24 <- as.numeric(Data$f24)
Data$f25 <- as.numeric(Data$f25)
Data$f26 <- as.numeric(Data$f26)
Data$f27 <- as.numeric(Data$f27)
Data$f28 <- as.numeric(Data$f28)
Data$f29 <- as.numeric(Data$f29)
Data$f30 <- as.numeric(Data$f30)

#Editing datasets
#Need to be adjusted for missing data per dataset.

Data1 <- Data %>% 
  select(1:24) 

#Normality test

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f1)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f2)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f3)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f5)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f6)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f7)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f9)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f10)[2]))

Data %>% 
  na.omit() %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f11)[2]))

Data %>% 
  group_by(Locality) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(f12)[2]))

#Distance matrix
Aurelia1 <- vegdist(Data1$f1, method = "euclidean", na.rm = T)
Aurelia2 <- vegdist(Data1$f2, method = "euclidean", na.rm = T)
Aurelia3 <- vegdist(Data1$f3, method = "euclidean", na.rm = T)
Aurelia5 <- vegdist(Data1$f5, method = "euclidean", na.rm = T)
Aurelia6 <- vegdist(Data1$f6, method = "euclidean", na.rm = T)
Aurelia7 <- vegdist(Data1$f7, method = "euclidean", na.rm = T)
Aurelia9 <- vegdist(Data1$f9, method = "euclidean", na.rm = T)
Aurelia10 <- vegdist(Data1$f10, method = "euclidean", na.rm = T)
Aurelia11 <- vegdist(Data1$f11, method = "euclidean", na.rm = T)
Aurelia12 <- vegdist(Data1$f12, method = "euclidean", na.rm = T)
Aurelia13 <- vegdist(Data1$f13, method = "euclidean", na.rm = T)
Aurelia21 <- vegdist(Data1$f21, method = "euclidean", na.rm = T)
Aurelia22 <- vegdist(Data1$f22, method = "euclidean", na.rm = T)
Aurelia23 <- vegdist(Data1$f23, method = "euclidean", na.rm = T)
Aurelia24 <- vegdist(Data1$f24, method = "euclidean", na.rm = T)
Aurelia25 <- vegdist(Data1$f25, method = "euclidean", na.rm = T)
Aurelia26 <- vegdist(Data1$f26, method = "euclidean", na.rm = T)
Aurelia27 <- vegdist(Data1$f27, method = "euclidean", na.rm = T)
Aurelia28 <- vegdist(Data1$f28, method = "euclidean", na.rm = T)
Aurelia29 <- vegdist(Data1$f29, method = "euclidean", na.rm = T)
Aurelia30 <- vegdist(Data1$f30, method = "euclidean", na.rm = T)

#Permanova Test
Permanova <- adonis(Aurelia~Locality, data = Data1, method = "euclidean")
summary(Permanova)

#Pairwise permanova tests
pairwise.adonis(Aurelia1, factors = Data1$Locality, sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia2, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia3, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia5, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia6, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia7, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia9, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia10, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia11, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia12, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia13, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia21, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia22, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia23, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia24, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia25, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia26, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia27, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia28, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia29, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")
pairwise.adonis(Aurelia30, factors = Data1$Locality,  sim.function = "vegdist", sim.method = "euclidean")

#Cap Analysis
CAP <- capscale(Aurelia ~ f2 + f3 + f5 + f6 + f7 + f9 + f10 + f19 + 
                  f21 + f22 + f23 + f24 + f25 + f26 + f27 + f28, Data1)
CAP1 <- ggord(CAP, Data1$Locality, ellipse = F) +
  scale_shape_discrete('Groups') + scale_fill_discrete('Groups')
CAP1


