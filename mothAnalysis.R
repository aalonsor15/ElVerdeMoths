# Puerto Rico Moth Hurricane Project
# AAR and PGF


# https://chrischizinski.github.io/rstats/vegan-ggplot2/

library(ggplot2)
library(vegan)
library(tidyverse)

# setwd("D:/Curriculum/14_ Colaboracion/2018 Moth Project/Statistical Analisis")

# -------------------------------------------------------------------------



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
Period <- select(Moth.frm, Hurricane)

data.scores <- as.data.frame(scores(moth.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores

data.scores$Site <- unlist(Site)  #   create a column of site names
data.scores$Period <- unlist(Period)  
data.scores$Habitat <- unlist(Habitat)
head(data.scores) #look at the data

species.scores <- as.data.frame(scores(moth.mds, "species"))
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


####### nMDS Graphs ######

p <- ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2, label = "*"),alpha=0.5) # add the species labels
p
p1 <- p + geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=Period,colour=Habitat),size=4) # add the point markers
p1  

p2 <- p1 + geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=""),size=4,vjust=0)  # add the site labels
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


###### abundance and richness boxplots ######

library("ggpubr")

# boxplots based on full matrix (taking all dates and site*period combo as replicates)

ggboxplot(Moth.frm, x = "Hurricane", y = "Abundance", color = "Habitat",width = 0.7,
          palette = c("#00AFBB","#FC4E07"),size =1,
          order = c("Pre", "Post"),legend = "right",
          font.legend = c(15, "plain", "black"),
          font.x = c(20, "bold", "black"),font.y = c(20, "bold", "black"),
          font.xtickslab= c(15, "bold", "black"), font.ytickslab= c(15, "bold", "black"))

ggboxplot(Moth.frm, x = "Hurricane", y = "Richness", color = "Habitat",width = 0.7,
          palette = c("#00AFBB","#FC4E07"),size =1,
          order = c("Pre", "Post"),legend = "right",
          font.legend = c(15, "plain", "black"),
          font.x = c(20, "bold", "black"),font.y = c(20, "bold", "black"),
          font.xtickslab= c(15, "bold", "black"), font.ytickslab= c(15, "bold", "black"))

# calculating total richness and abundance per site per period


# moth2 <- Moth.frm %>%
#   group_by(Site, Hurricane) %>%
#   summarize(Abundance = sum(M1:A248))



#### PERMANOVA ####

adonis(Moth.frm$Abundance ~ Moth.frm$Hurricane * Moth.frm$Habitat, 
       permutations=999, method = "euclidean") ## generaci?n de permanova con euclideana y un alfa de 0.001



aov2.abun <- aov(Abundance ~ Habitat * Hurricane, data = Moth.frm)
summary(aov2.abun)
TukeyHSD(aov2.abun)

aov2.rich <- aov(Richness ~ Habitat * Hurricane, data = Moth.frm)
summary(aov2.rich)
TukeyHSD(aov2.rich)

aov2.out <- group_by(Moth.frm, Habitat, Hurricane) %>%
  summarise(
    count = n(),
    Abundance_mean = mean(Abundance, na.rm = TRUE),
    Abundance_sd = sd(Abundance, na.rm = TRUE),
    Richness_mean = mean(Richness, na.rm = TRUE),
    Richness_sd = sd(Richness, na.rm = TRUE),
  )

aov2.out


################## ANOSIM ###################

moth.dist <- vegdist(moth,method="bray")
moth.ano <- anosim(moth.dist, Moth.frm$Hurricane)
summary(moth.ano)
plot(moth.ano)

################ SIMPER ######################

(sim <- with(Moth.frm, simper(moth, Hurricane)))
summary(sim)

(sim <- with(Moth.frm, simper(moth, Habitat)))
summary(sim)


################ Indicator Value ###############

install.packages("labdsv")
install.packages("indicspecies")
library("indicspecies")
library("labdsv")

library(vegan)
library(MASS)
library(labdsv)
library(cluster)
library(indicspecies)

ind_species<-multipatt(moth,Moth.frm$Hurricane,max.order=2,
                       duleg=TRUE,func="IndVal.g",control=how(nperm=5000))
ind_species
summary(ind_species)

