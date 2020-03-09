

# https://chrischizinski.github.io/rstats/vegan-ggplot2/

library(ggplot2)
library(vegan)
library(tidyverse)

setwd("D:/Curriculum/14_ Colaboracion/2018 Moth Project/Statistical Analisis")


Moth.frm  <- read.csv("fullmatrix.csv")
attach(Moth.frm)
Moth.frm


##### nMDS #####

moth <- select(Moth.frm, M1:A248)
set.seed(1)
moth.mds <- metaMDS(moth, distance = "bray", k = 2,trymax=100)  #using all the defaults
moth.mds


Habitat <- select(Moth.frm, Habitat)
Site <- select(Moth.frm, Site)
Condition <- select(Moth.frm, Hurricane)

data.scores <- as.data.frame(scores(moth.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores

data.scores$Site <- unlist(Site)  #   create a column of site names
data.scores$Condition <- unlist(Condition)  
data.scores$Habitat <- unlist(Habitat)
head(data.scores) #look at the data

species.scores <- as.data.frame(scores(moth.mds, "species"))
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


####### nMDS Graphs ######

p <- ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5) # add the species labels
p
p1 <- p + geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Condition,colour=Condition),size=3) # add the point markers
p1  

p2 <- p1 + geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=Habitat),size=6,vjust=0)  # add the site labels
p2  

p3 <- p2 + coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
p3


#### PERMANOVA ####

adonis(Moth.frm$Abundance ~ Moth.frm$Hurricane*Moth.frm$Habitat, permutations=999, method = "euclidean") ## generación de permanova con euclideana y un alfa de 0.001


