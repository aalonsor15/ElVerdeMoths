# Puerto Rico Moth Hurricane Project
# AAR and PGF


# https://chrischizinski.github.io/rstats/vegan-ggplot2/

library(ggplot2)
library(vegan)
library(tidyverse)
library(ggpubr)
library(patchwork)

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
Period <- select(Moth.frm, Period)

data.scores <- as.data.frame(scores(moth.mds))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores

data.scores$Site <- unlist(Site)  #   create a column of site names
data.scores$Period <- unlist(Period)  
data.scores$Habitat <- unlist(Habitat)
head(data.scores) #look at the data

species.scores <- as.data.frame(scores(moth.mds, "species"))
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data

data.scores$Period <- factor(data.scores$Period,
                          levels = c("Pre-Hurricane","Post-Hurricane"), ordered = TRUE)


####### nMDS Graphs ######

p <- ggplot() + 
  geom_text(data=species.scores,aes(x=NMDS1,y=NMDS2, label = "*"),size=7, alpha=0.5) # add the species labels
p
p1 <- p + geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,
                                          shape=Period,colour=Habitat),size=5) + # add the point markers
        scale_color_manual(values=c("#00AFBB", "#FC4E07")) + 
        scale_shape_manual(values=c(15, 17))
p1  

p2 <- p1 + geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=""),size=6,vjust=0)  # add the site labels
p2  

p3 <- p2 + coord_equal() +
  theme_bw() + 
  theme(legend.title = element_text(size=12),
        axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=16), # remove x-axis labels
        axis.title.y = element_text(size=16), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()) + 
  guides(color = guide_legend(override.aes = list(shape = 16, size = 4)))
p3


# time series graph -------------------------------------------------------

# install.packages("ggthemes")
library(ggthemes)

by.date <- read.csv('summary.date.csv')

c <- ggplot(by.date, aes(Date, Abundance, group=Habitat, colour=Habitat)) +
  geom_line(lwd=1.0)+xlab("")+ylab(label='Abundance') +
  theme(axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        panel.background = element_blank()) +
  theme_classic() + 
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) 

d <- ggplot(by.date, aes(Date, Richness, group=Habitat, colour=Habitat)) +
  geom_line(lwd=1.0)+xlab(label='Sampling Month')+ylab(label='Richness') +
  theme(axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        panel.background = element_blank()) +
  theme_classic() + 
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) 


cd <- ggarrange(c + rremove("x.text") , d , align = "hv",
                labels = c("A", "B"),font.label = list(size = 12, color = "black"),
                ncol = 1, nrow = 2, 
                common.legend = TRUE, legend = "top")
cd

e <- ggplot(data = by.date, 
            mapping = aes(x=Date, y=Abundance, group=Habitat, colour=Habitat)) +
  geom_point(size=4) + geom_line(linetype=2) +
  xlab("") +
  theme(axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        panel.background = element_blank()) +
  theme_classic() + 
  theme(legend.position = "top") +
  scale_color_manual(values=c("#00AFBB", "#FC4E07")) 
  

f <- ggplot(data = by.date, 
            mapping = aes(x=Date, y=Richness, group=Habitat, colour=Habitat)) +
  geom_point(size=4) + geom_line(linetype=2) +
  xlab(label='Sampling Month') +
  theme(axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        panel.background = element_blank()) +
  theme_classic() + 
  theme(legend.position = "none") +
  scale_color_manual(values=c("#00AFBB", "#FC4E07"))


e/f

# Can we do a mann kendal analysis with this data?

###### abundance and richness boxplots (also dominance and diversity) ######

# boxplots based on full matrix

ggboxplot(Moth.frm, x = "Period", y = "Abundance", color = "Habitat",width = 0.7,
          palette = c("#00AFBB","#FC4E07"),size =1,
          order = c("Pre-Hurricane", "Post-Hurricane"),legend = "right",
          font.legend = c(15, "plain", "black"),
          font.x = c(20, "bold", "black"),font.y = c(20, "bold", "black"),
          font.xtickslab= c(15, "bold", "black"), font.ytickslab= c(15, "bold", "black"))

ggboxplot(Moth.frm, x = "Period", y = "Richness", color = "Habitat",width = 0.7,
          palette = c("#00AFBB","#FC4E07"),size =1,
          order = c("Pre-Hurricane", "Post-Hurricane"),legend = "right",
          font.legend = c(15, "plain", "black"),
          font.x = c(20, "bold", "black"),font.y = c(20, "bold", "black"),
          font.xtickslab= c(15, "bold", "black"), font.ytickslab= c(15, "bold", "black"))

# calculating total richness and abundance per site per period
# *** I should learn how to do this in R!

# moth2 <- Moth.frm %>%
#   group_by(Site, Hurricane) %>%
#   summarize(Abundance = sum(M1:A248))


# boxplots based on summarized data by site and per period (pre and post)

summary <- read.csv('summary.csv')

a <- ggboxplot(summary, x = "Period", y = "Abundance", width = 0.7,
          palette = c("#00AFBB","#FC4E07"),size =0.5, fill= "Habitat", 
          order = c("Pre-Hurricane", "Post-Hurricane"),legend = "right",
          font.legend = c(10, "plain", "black"),
          font.y = c(12, "black"),
          font.ytickslab= c(8, "black")) + xlab("")
      

b <- ggboxplot(summary, x = "Period", y = "Richness", width = 0.7,
          palette = c("#00AFBB","#FC4E07"),size =0.5, fill = "Habitat",
          order = c("Pre-Hurricane", "Post-Hurricane"),legend = "right",
          font.legend = c(10, "plain", "black"),
          font.x = c(12, "black"),font.y = c(12, "black"),
          font.xtickslab= c(10, "black"), font.ytickslab= c(8, "black"))


ab <- ggarrange(a + rremove("x.text") , b, align = "hv",
                labels = c("A", "B"),font.label = list(size = 12, color = "black"),
                ncol = 1, nrow = 2,
                common.legend = TRUE, legend = "top")
ab

# Berger-Parker dominance boxplot

bp <- ggboxplot(summary, x = "Period", y = "Dominance", width = 0.7,
                palette = c("#00AFBB","#FC4E07"),size =0.5, fill = "Habitat",
                order = c("Pre-Hurricane", "Post-Hurricane"),legend = "right",
                font.legend = c(10, "plain", "black"),
                font.x = c(12, "black"),font.y = c(12, "black"), 
                font.xtickslab= c(10, "black"), font.ytickslab= c(8, "black")) + xlab("")

# Fisher's alpha diversity 

library(vegan)

short_matrix <- read.csv('site.by.period.matrix.csv')
F.Diversity <- fisher.alpha(short_matrix) 
F.Diversity <- as.data.frame(F.Diversity)
summary <- cbind(summary, F.Diversity)


div <- ggboxplot(summary, x = "Period", y = "F.Diversity", width = 0.7,
               palette = c("#00AFBB","#FC4E07"),size =0.5, fill= "Habitat", 
               order = c("Pre-Hurricane", "Post-Hurricane"),legend = "top",
               font.x = c(12, "black"),font.y = c(12, "black"),
               font.ytickslab= c(8, "black"), font.xtickslab= c(10, "black")) + 
              xlab("Period") + ylab("Diversity")
div


dbp <- ggarrange(bp + rremove("x.text") , div, align = "hv",
                labels = c("C", "D"),font.label = list(size = 12, color = "black"),
                ncol = 1, nrow = 2,
                common.legend = TRUE, legend = "top")
dbp


#### PERMANOVA ####

adonis(Moth.frm$Abundance ~ Moth.frm$Hurricane * Moth.frm$Habitat, 
       permutations=999, method = "euclidean") ## generaci?n de permanova con euclideana y un alfa de 0.001

# Two Way Anova with full matrix 

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

# Two Way Anova based on summarized data by site and per period (pre and post)

aov2.abun.short <- aov(Abundance ~ Habitat * Period, data = summary)
summary(aov2.abun.short)
TukeyHSD(aov2.abun.short)

aov2.rich.short <- aov(Richness ~ Habitat * Period, data = summary)
summary(aov2.rich.short)
TukeyHSD(aov2.rich.short)

aov2.div.short <- aov(F.Diversity ~ Habitat * Period, data = summary)
summary(aov2.div.short)
TukeyHSD(aov2.div.short)

aov2.dom.short <- aov(Dominance ~ Habitat * Period, data = summary)
summary(aov2.dom.short)
TukeyHSD(aov2.dom.short)


################## ANOSIM ###################

moth.dist <- vegdist(moth,method="bray")
moth.ano <- anosim(moth.dist, Moth.frm$Period)
summary(moth.ano)
plot(moth.ano)

################ SIMPER ######################

(sim <- with(Moth.frm, simper(moth, Period)))
summary(sim)

(sim <- with(Moth.frm, simper(moth, Period)))
summary(sim)


################ Indicator Value ###############

#install.packages("labdsv")
#install.packages("indicspecies")
library("indicspecies")
library("labdsv")

library(vegan)
library(MASS)
library(labdsv)
library(cluster)
library(indicspecies)

ind_species<-multipatt(moth,Moth.frm$Period,max.order=2,
                       duleg=TRUE,func="IndVal.g",control=how(nperm=5000))
ind_species
summary(ind_species)



# other things to do
## Berger-Parker dominance
## graficos de canopy cover
## add symbols in box plots to represent significant differences, as in Alonso_rodz et al
## linear models with Site, Habitat, Period, Moonlight



# boxplot for canopy cover --------------------------------------

canopy <- read.csv('canopy.csv')

cc1 <- ggboxplot(canopy, x = "Date", y = "Canopy_Openness", width = 0.7,
               palette = c("#00AFBB","#FC4E07"),size =0.5, fill= "Habitat", 
               order = c("2017/09/06", "2017/09/10", "2017/10/04",
                         "2017/11/30", "2018/01/25", "2018/02/28",
                         "2018/04/03", "2018/08/15", "2019/08/08"),
               legend = "top",
               font.legend = c(10, "plain", "black"), 
               font.x = c(12, "black"), font.y = c(12, "black"), 
               ylab = "Canopy Openness (%)", xlab = "Date",
               font.xtickslab= c(8, "black"),
               font.ytickslab= c(8, "black")) 
cc1

