

                              #**BASIC DIVERSITY INDECES**#
                             
specaccum(Spider.20.wide[,2:163])
Species Accumulation Curve
Accumulation method: exact
Call: specaccum(comm = Spider.data.a[, 2:163]) 

sac<-specaccum(Spider.20.wide[,2:163])
plot(sac)

diversity(Spider.dat[a.a[,2:136], index = "shannon")

SPdiv<- diversity(Spider.combined.wide[,2:254], index = "simpson")
Antdiv<- diversity(Ants.combined.wide[,2:132], index= "simpson")

SPdiv<- diversity(Spider.combined.wide[,2:254], index = "shannon")
Antdiv<- diversity(Ants.combined.wide[,2:132], index= "simpson")

decorana(Spider.data.a[,2:136])
deco<-decorana(Spider.data.a[,2:136])
plot(deco)

 
rda(Spider.data.a[,2:136])
pca<-rda(Spider.data.a[,2:136])
plot(pca)


txt2<-c("CBE"="Cleared bush encroachment", "CC"="Conservancy","CL"="Crop land", "GR"="Game range", 
"HS"="Human settlememt", "RF"="Riverine forest","WG"="Wildlife grazing")


                          **Evenness**
#set data with species as columns and sites as rows
View(Spider.combined.wide)
library(vegan)
Ev<-apply(Spider.combined.wide[,-1]>0,1, sum)
diversity(Spider.combined.wide[-1], index = "simpson")/log(Ev)

                            **Anova**
 
anno<-aov(Abundance~Season*Land.use.type*Site, data=Spider.date.combined.)
summary(anno)

                      T-test (2samples)
#check for significant difference between richness and abundance in both seasons


                           **NMDS**

  #NMDS
  
#relative abundance
  relabu<-decostand(Spider.combined.wide[,4:199], method = "total")
  relabu2<-decostand(Ants.combined.wide[,4:131], method = "total")
# Calculate distance matrix
dismatrix<-vegdist(relabu, method = "bray")
dismatrix2<-vegdist(relabu2, method = "bray")

# Creating easy to view matrix and writing .csv
dismatrix <- as.matrix(dismatrix, labels = T)
write.csv(dismatrix, "dismatrix")

# Spider NMDS 
Spidernmds=metaMDS(dismatrix,distance = "bray", k=2,
maxit = 999, rymax = 500,wascores = TRUE)

plot(Spidernmds$points, pch = shape2[Spider.combined.wide$Elevation],
col= coo2,cex=1,
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-0.7,0.7), 
ylim = c(-0.7,0.7))


# Plotting points in ordination space
plot(spiderNMDS, "sites")   # Produces distance 
orditorp(spiderNMDS, "sites")   # Gives points labels

#Colouring and shape
colvec <- c("gray0", "gray0", "gray49", "gray49")   # Identifies colors for group assignments
pchvec <- c(21, 21, 22, 22)   # Identifies character symbols for group assignments

#plot
plot(spiderNMDS)
with(SpiGroup, points(spider19NMDS,display = "sites",
col = "black", environment(fun = NULL),pch = pchvec[habitat],
bg = colvec[habitat])
     
# Shepards test/goodness of fit
     
goodness(spiderNMDS) # Produces a results of test statistics for goodness of fit for each point
     
stressplot(spiderNMDS) # Produces a Shepards diagram 
     

#plot nmds without points or line (using "type" function)
plot(nmdS$points, type="n")

#show site names on actual nmds plot, "txt" is a created object with site names
orditorp(nmdS,display= "sites",txt, cex=0.80,air=0.01)



                       #################NMDS VECTORS#######################

Sitenames<-c("08NA", "08NB", "CL", "BE", "CBE","10N", "10S", "12N", 
       "12SA", "12SB" ,"14NA", "14NB","14SA", "BP", "MO", "16S", "17N", "VIL")

Sitenamesx2<-c("08NA", "08NB", "CL", "BE", "CBE","10N", "10S", "12N", 
        "12SA", "12SB" ,"14NA", "14NB","14SA", "BP", "MO", "16S", "17N", "VIL",
        "08NA", "08NB", "CL", "BE", "CBE","10N", "10S", "12N", 
        "12SA", "12SB" ,"14NA", "14NB", "14SA", "BP", "MO", "16S", "17N", "VIL")

co=c("Game range"="black","Crop land"="red", "Conservancy"="green",
     "Wildlife grazing"="yellow", "Cattle grazing"="blue",
     "Human settlement"="orange", "Riverine Forest"="brown",
     "Cleared bush encroachment"="purple")

Landuseshape=c("GR"=15,"CL"=16, "CC"=2, "WG"=5,"10S"=6,"HS"=8,"RF"=0,"CBE"=17)

txt1<- c("GR","CL", "CC", "WG", "10S","HS","RF","CBE")

Seasonnames<- c("hotdry", "hotwet")

Seasonshape= c("hotdry"=15, "hotwet"=16)

Seasoncol= c("hotdry"="red","hotwet"="blue")

Seasoncol1= c("red","blue")

Elevnames= c("Low", "Mid", "High")

Elevshape= c("Low"=15, "Mid"=16, "High"=17)

Elevcol= c("Low"="black", "Mid"="green", "High"="red")

col= Seasoncol[SpiderNMDS_Season$Season]

Aspectshape= c("North"=15, "South"=16)
Aspectcol= c("North"="red","South"="blue")
Aspectnames= c("North", "South")

                        #*****ASPECT NMDS****#
  
  #relative abundance
relabu<-decostand(Spider.combined.wide[,5:200], method = "total")
relabu2<-decostand(Ants.combined.wide[,6:133], method = "total")
# Calculate distance matrix
dismatrix<-vegdist(relabu, method = "bray")
dismatrix2<-vegdist(relabu2, method = "bray")

  mn
  
  
AntnmdsA<- metaMDS(dismatrix2,distance = "bray", k=2,
                      maxit = 999, rymax = 500,wascores = TRUE)
plot(AntnmdsA$points, pch = Aspectshape[Ants.combined.wide$Aspect], 
cex=0.75, col= Aspectcol[Ants.combined.wide$Aspect],
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-0.43,0.45), ylim = c(-0.48,0.50))


orditorp(AntnmdsA, display = "sites",Sitenames, pos= 3, cex = 0.50)

legend('bottomright', title = "Aspect",Aspectnames,col=Aspectcol,cex=0.6,
       pch=Aspectshape)  

  

                      #***ELEVATIONAL NMDS***#
  
SpidernmdsE<- metaMDS(Spider.combined.wide[,5:200], distance = "bray", k=2,
                      maxit = 999, rymax = 500,wascores = TRUE)

plot(SpidernmdsE$points, pch = Elevshape[Spider.combined.wide$Elevation],
     cex=1,col= Elevcol[Spider.combined.wide$Elevation],
     xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-1,1.3), 
     ylim = c(-1,1))

orditorp(SpidernmdsE, display = "sites",Sitenames, pos= 3, cex = 0.55)

legend('bottomright', title = "Elevation",Elevnames,col= Elevcol, cex=0.6,
pch=Elevshape)

AntnmdsE<-metaMDS(Ants.combined.wide[,6:133], distance = "bray", k=2,
maxit = 999, rymax = 500,wascores = TRUE)

plot(AntnmdsE$points, pch = Elevshape[Ants.combined.wide$Elevation],
cex=1,col= Elevcol[Ants.combined.wide$Elevation],
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-1.3,1.3), 
ylim = c(-1.0,1.0))

orditorp(AntnmdsE, display = "sites",Sitenames, pos= 3, cex = 0.55)

legend('bottomright', title = "Elevation",Elevnames,col=Elevcol,cex=0.6,
pch=Elevshape)

                       **SEASONAL NMDS**
  
  
  
  #relative abundance
relabu<-decostand(SpiderNMDS_Season[,3:198], method = "total")
relabu2<-decostand(Ants.Season.NMDS[,3:130], method = "total")
# Calculate distance matrix
dismatrix<-vegdist(relabu, method = "bray")
dismatrix2<-vegdist(relabu2, method = "bray")

# Creating easy to view matrix and writing .csv
dismatrix <- as.matrix(dismatrix, labels = T)
write.csv(dismatrix, "dismatrix")


# Spider NMDS 
SpiderDS=metaMDS(dismatrix,distance = "bray", k=2,
maxit = 999, rymax = 500,wascores = TRUE)
plot(SpiderDS$points, pch = Seasonshape[SpiderNMDS_Season$Season], 
cex=0.8, col= Seasoncol[SpiderNMDS_Season$Season],
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-0.35,0.63), ylim = c(-0.50,0.51))
library(vegan)
  

orditorp(SpiderDS, display = "sites",Sitenamesx2,pos=4, cex = 0.45, 
         air = 0.0000001)

legend('bottomright', title = "Season",Seasonnames,cex=0.6,col=Seasoncol,
       pch=Seasonshape)  


#Ant NMDS 

AntnmdsS=metaMDS(dismatrix2,distance = "bray", k=2)

  
#Spider
as.factor(SpiderNMDS_Season$Sites)
as.factor(SpiderNMDS_Season$Season)

SpidernmdsS=metaMDS(SpiderNMDS_Season[,3:198],distance = "bray", k=2)

plot(SpidernmdsS$points, pch = Seasonshape[SpiderNMDS_Season$Season], 
cex=0.8, col= Seasoncol[SpiderNMDS_Season$Season],
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-0.58,0.55), ylim = c(-0.45,0.46))

plot(SpidernmdsS$points)

plot(SpidernmdsS$points, pch = Seasonshape[Spider.Season.NMDS$Season], 
cex=0.8, col= Seasoncol[Spider.Season.NMDS$Season],
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-0.58,0.55), ylim = c(-0.45,0.46))

orditorp(SpidernmdsS, display = "sites",Sitenamesx2,pos=4, cex = 0.45, 
         air = -0.001)

legend('bottomleft', title = "Season",Seasonnames,cex=0.6,col=Seasoncol,
pch=Seasonshape)

#Ants
as.factor(Ants.Season.NMDS$Sites)
as.factor(Ants.Season.NMDS$Season)

AntnmdsS=metaMDS(Ants.Season.NMDS[,3:130],distance = "bray", k=2)

plot(AntnmdsS$points, pch = Seasonshape[Ants.Season.NMDS$Season],
cex=0.8,col=Seasoncol[Ants.Season.NMDS$Season],
xlab= "NMDS 1", ylab= "NMDS 2", xlim = c(-1.,0.85), ylim = c(-0.8,0.8))

orditorp(AntnmdsS, display = "sites",Sitenamesx2,pos=4, cex = 0.50)

legend('bottomleft', title = "Season",Seasonnames,cex=0.6,col= Seasoncol,
pch=Seasonshape)

                  ##!!!!!!!*******ANOSIM******!!!!!!!!##
library(vegan)

##Ants
ano1<- anosim(Ants.combined.wide[,5:132], Ants.combined.wide$Elevation,
permutations=999, distance= "bray")
summary(ano1)
plot(ano1)

ano2<- anosim(Ants.Season.NMDS[,3:130], Ants.Season.NMDS$Season,
permutations=999, distance= "bray")
summary(ano2)
plot(ano2)

ano3<- anosim(Ants.Season.NMDS[,3:130], Ants.Season.NMDS$Sites,
permutations=999, distance= "bray")
summary(ano3)
plot(ano3)

                                 OR
                                 
ano3<- anosim(Ants.combined.wide[,5:132], Ants.combined.wide$Landuse_type,
permutations=999, distance= "bray")
summary(ano3)
plot(ano3)

##Spiders

ano4<- anosim(Spider.combined.wide[,5:200], Spider.combined.wide$Elevation,
permutations=999, distance= "bray")
summary(ano4)
plot(ano4)

ano5<- anosim(Spider.Season.NMDS[,3:198], Spider.Season.NMDS$Season,
permutations=999, distance= "bray")
summary(ano5)
plot(ano5)

ano6<- anosim(Spider.Season.NMDS[,3:198], Spider.Season.NMDS$Sites,
permutations=999, distance= "bray")
summary(ano6)
plot(ano6)

    

                        OR
      
ano6<- anosim(Spider.combined.wide[,5:200], Spider.combined.wide$Landuse_type,
permutations=999, distance= "bray")
summary(ano6)
plot(ano6)

#aspect
ano7<- anosim(Ants.combined.wide[,6:133], Ants.combined.wide$Aspect,
permutations=999, distance= "bray")
summary(ano7)
plot(ano6)

ANOSIM statistic R: 0.1805 
Significance: 0.042

ano8<- anosim(Spider.combined.wide[,5:200], Spider.combined.wide$Aspect,
permutations=999, distance= "bray")
summary(ano8)
plot(ano6)

ANOSIM statistic R: 0.2192 
Significance: 0.019                            
                     ##*********PERMANOVA**********##
#Ants      
perm1<- adonis(Ants.combined.wide[,5:132]~Elevation, data = Ants.combined.wide, 
permutations = 999, method = "bray")              
perm1
                 
perm2<- adonis(Ants.Season.NMDS[,3:130]~Season, data = Ants.Season.NMDS, 
permutations = 999, method = "bray")              
perm2                  

perm3<- adonis(Ants.combined.wide[,5:132]~Landuse_type, data = Ants.combined.wide, 
permutations = 999, method = "bray")
perm3

perm4<- adonis(Spider.combined.wide[,5:200]~Elevation, data = Spider.combined.wide, 
permutations = 999, method = "bray")              
perm4

perm5<- adonis(Spider.Season.NMDS[,3:198]~Season, data = Spider.Season.NMDS, 
permutations = 999, method = "bray")              
perm5  

perm6<- adonis(Spider.combined.wide[,5:200]~Landuse_type, data = Spider.combined.wide, 
permutations = 999, method = "bray")
perm6

#aspect
perm7<- adonis(Ants.combined.wide[,6:133]~Aspect, data = Ants.combined.wide, 
permutations = 999, method = "bray")
perm7

1    0.6714 0.67140   2.105 0.11627  0.014 *

perm8<- adonis(Spider.combined.wide[,5:200]~Aspect, data = Spider.combined.wide, 
permutations = 999, method = "bray")
perm8

1    0.8099 0.80993  2.5523 0.13757  0.003 **
  
  
  
  

                           #######**PROF. FOORD BOXPLOTS**#########


ant_univariate_data$Sites<-ordered(ant_univariate_data$Sites)
as.factor(ant_univariate_data$Sites)
Siteorder <- ordered(ant_univariate_data$Sites,levels=c( "VIL","CL",
"BE", "CBE", "10S","12SA", "12SB",
"14SA" ,"BP" ,"MO" ,"16S" ,"17N" , "14NB", "14NA",
"12N","10N","08NA", "08NB"))

ant_univariate_data$Sites <- ordered(ant_univariate_data$Sites,
levels =  c("VIL","CL","BE", "CBE", "10S","12SA", "12SB",
"14SA","BP" ,"MO" ,"16S" ,"17N" , "14NB", "14NA","12N","10N"
,"08NA", "08NB"))

as.factor(BACT_ARCH_amoA_gene_qPCR_validation$`Gene copies ?l -1`)
BACT_ARCH_amoA_gene_qPCR_validation$`Gene copies ?l -1`<- ordered(BACT_ARCH_amoA_gene_qPCR_validation$`Gene copies ?l -1`,
levels= c("5.84x10^8","5.84x10^7", "5.84x10^6", "5.84x10^5", "5.84x10^4", "5.84x10^3","5.84x10^2")

G<-ggplot(BACT_ARCH_amoA_gene_qPCR_validation,aes(BACT_ARCH_amoA_gene_qPCR_validation$`Gene copies ?l -1`,
BACT_ARCH_amoA_gene_qPCR_validation$`Cycle threshold (Ct)`)) + 
 geom_boxplot(outlier.shape = NA) + 
 theme_classic() + 
 theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1) ) +
 facet_grid(.~Essay) 
G+ylim=rev(range(y))
g1<-ggplot(ant_univariate_data,aes(Sites, Species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1) ) +
  facet_grid(.~Season)
g1+ xlab("Sites") + ylab("Species richness")

coord_cartesian(ylim = c(0, 100))

g1+scale_x_discrete(limits =c( "VIL","CL" , "BE", "CBE", "10S","12SA", "12SB",
                              "14SA" ,"BP" ,"MO" ,"16S" ,"17N" , "14NB", 
                              "14NA" , "12N","10N","08NA", "08NB")) 

g2<-ggplot(ant_univariate_data,aes(Sites, Activity)) + 
  geom_boxplot(outlier.shape = NA) + theme_classic() + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1) ) +
  facet_grid(.~Season)+ scale_y_continuous(trans ='log2')

g2+scale_x_discrete(limits =c( "VIL","CL" , "BE", "CBE", "10S","12SA", "12SB",
                               "14SA" ,"BP" ,"MO" ,"16S" ,"17N" , "14NB", 
                               "14NA" , "12N","10N","08NA", "08NB"))

ggplot(ant_univariate_data,aes(Landuse_type, Species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1) ) +
  facet_grid(.~Season)





spider_univariate_data$Sites<-ordered(spider_univariate_data$Sites,
levels =  c("VIL","CL" , "BE", "CBE", "10S","12SA", "12SB",
"14SA" ,"BP" ,"MO" ,"16S" ,"17N" , "14NB", 
"14NA" , "12N","10N","08NA", "08NB"))
  
g3<-ggplot(spider_univariate_data,aes(Sites, Species_richness)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1) ) +
  facet_grid(.~Season)

g3+ xlab("Sites") + ylab("Species richness")

g3+scale_x_discrete(limits =c( "VIL","CL" , "BE", "CBE", "10S","12SA", "12SB",
                               "14SA" ,"BP" ,"MO" ,"16S" ,"17N" , "14NB", 
                               "14NA" , "12N","10N","08NA", "08NB"))

g4<-ggplot(spider_univariate_data,aes(Sites,Activity)) + 
  geom_boxplot(outlier.shape = NA) +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 75, vjust = 1, hjust = 1) ) +
  facet_grid(.~Season) + scale_y_continuous(trans ='log2') 

g4+scale_x_discrete(limits =c( "VIL","CL" , "BE", "CBE", "10S","12SA", "12SB",
                               "14SA" ,"BP" ,"MO" ,"16S" ,"17N" , "14NB", 
                               "14NA" , "12N","10N","08NA", "08NB"))





                          **INDICATOR SPECIES**
library(indicspecies)
library(labdsv)
library(vegan)


groups= c( "Vil","08S" , "09SA", "09SB", "10S","12SB", "12SA",
"14SA" ,"14SB" ,"14SC" ,"16S" ,"17N" , "14NB", "14NA" , "12N",
"10N","08NA", "08NB")

groups<- c("GR","CL", "CC", "WG", "10S","HS","RF","CBE")



                   ##Sites grouped by land use vector



#ants 
abund = Ants.combined.wide[,5:132]
land= Ants.combined.wide$Landuse_type
indval = multipatt(abund, land, func = "r.g",  control = how(nperm=9999))
summary(indval)


#Spider
abund2 = Spider.combined.wide[,5:200]
land2= Spider.combined.wide$Landuse_type
indval2 = multipatt(abund2, land2, func = "r.g",  control = how(nperm=9999))
summary(indval2)

#Spider
abund2 = INDISPECIES_data_spider[,3:160]
land2= INDISPECIES_data_spider$Landuse_type
indval2 = multipatt(abund2, land2, func = "r.g",  control = how(nperm=9999))
summary(indval2)








                     ##Sites grouped by elevational zones

#ants 
abund = Ants.combined.wide[,6:ncol(Ants.combined.wide)]
elev= Ants.combined.wide$Elevation
indval = multipatt(abund, elev, func = "r.g",  control = how(nperm=9999))
summary(indval)

#Spider
        
abund2 = Spider.combined.wide[,5:ncol(Spider.combined.wide)]
elev= Spider.combined.wide$Elevation
indval2 = multipatt(abund2, elev, func = "r.g",  control = how(nperm=9999))
+Figure 3.2: Non-metric multidimensional scaling (NMDS) plots of spider (A) and ant (B) community similarity across the transect between the dry and wet season. The plots were based on activity data and Bray-Curtis similarity was used on the two-dimensional plots. VIL: Village, CL: Cropland, CBE: Cleared Bush Encroachment, BE: Bush Encroachment, BP: Bluegum Plantation, MO: Macademia Orchard        


  
                       ###Sites grouped by aspects###
  
  #Ants
abund = Ants.combined.wide[,4:ncol(Ants.combined.wide)]
asp= Ants.combined.wide$Aspect
indval = multipatt(Ants.combined.wide[,5:132], asp, func = "r.g",
control = how(nperm=9999))
summary(indval)     
  
  
 #Spiders 
abund2 = Spider.combined.wide[,4:ncol(Spider.combined.wide)]
asp2 = Spider.combined.wide$Aspect
indval2 = multipatt(Spider.combined.wide[,5:132], asp, func = "r.g",
control = how(nperm=9999))
summary(indval2)   

                             
  



                 ##### ##########******GLMM*******########## ####



library(nlme)  
library(lme4)                              
library(vegan)

#trail
model1<- lme(Species_richness ~ Landuse_type * Season * elevation,
data = ant_univariate_data, random = ~1|categories)
summary(model1)$coefficient

model2= glmer(Species_richness~Season+Landuse_type+aspect+habitat+
(1|categories) + (1|elevation),data = ant_univariate_data, 
weight=cov, family=poisson)
summary(model2)$coefficient

#ANTS
  
mod <- glmer(Species_richness ~ Season + Landuse_type + Elevational_zone
+ aspect + habitat +(1|categories) + (1|elevation),
data = ant_univariate_data, weight = cov, family = poisson)
summary(mod)$coefficient            

Fixed effects:
Estimate Std. Error z value Pr(>|z|)    
(Intercept)                            2.43579    0.16815  14.486  < 2e-16 ***
Seasonhotwet                           0.28848    0.04762   6.057 1.38e-09 ***
Landuse_typeCleared bush encroachment  0.23632    0.12319   1.918 0.055063 .  
Landuse_typeCrop land                  0.18167    0.12419   1.463 0.143489    
Landuse_typeEucalyptus plantation     -0.77320    0.18930  -4.085 4.42e-05 ***
Landuse_typeHuman settlement           0.09485    0.12682   0.748 0.454536    
Landuse_typeMacadamia orchard         -0.18406    0.16462  -1.118 0.263522    
Landuse_typeNatural                    0.04218    0.12896   0.327 0.743592    
Elevational_zoneLow                    0.03704    0.09077   0.408 0.683204    
Elevational_zoneMid                    0.16869    0.09412   1.792 0.073075 .  
aspectSouth                            0.08786    0.08051   1.091 0.275153    
habitatforest                         -0.56778    0.15256  -3.722 0.000198 ***
habitatopen woodland                  -0.10183    0.13385  -0.761 0.446823    
habitatsedgeland                      -0.35161    0.16143  -2.178 0.029403 *  


mod2<- glmer(Activity ~ Season + Landuse_type + Elevational_zone
+ aspect + habitat + (1|elevation)+ (1|categories),
data = ant_univariate_data, weight = cov, family = poisson)
summary(mod2)


Estimate Std. Error z value Pr(>|z|)    
(Intercept)                            4.10399    0.22189  18.496  < 2e-16 ***
Seasonhotwet                           0.91721    0.01001  91.640  < 2e-16 ***
Landuse_typeCleared bush encroachment  0.37615    0.03358  11.202  < 2e-16 ***
Landuse_typeCrop land                  2.27066    0.42664   5.322 1.03e-07 ***
Landuse_typeEucalyptus plantation     -0.76194    0.31479  -2.421  0.01550 *  
Landuse_typeHuman settlement           0.94159    0.32571   2.891  0.00384 ** 
Landuse_typeMacadamia orchard          0.84433    0.31516   2.679  0.00738 ** 
Landuse_typeNatural                    0.25387    0.36318   0.699  0.48454    
Elevational_zoneLow                   -0.06457    0.26681  -0.242  0.80879    
Elevational_zoneMid                    0.23163    0.27756   0.835  0.40400    
aspectSouth                            0.49743    0.04293  11.586  < 2e-16 ***
habitatforest                          0.01161    0.45494   0.026  0.97964    
habitatopen woodland                   0.02636    0.36393   0.072  0.94226    
habitatsedgeland                       0.36478    0.46127   0.791  0.42905 


#SPIDER

mod3 <- glmer(Species_richness ~ Season + Landuse_type + Elevational_zone
+ habitat + aspect +(1|categories) + (1| elevation) + (1|replicate),
data= spider_univariate_data, weight = cov, family = poisson)
summary(mod3)



Fixed effects:
Estimate Std. Error z value Pr(>|z|)    
(Intercept)                            0.38882    0.37444   1.038  0.29908    
Seasonhotwet                          -0.26740    0.09466  -2.825  0.00473 ** 
Landuse_typeCleared bush encroachment  0.37852    0.31915   1.186  0.23561    
Landuse_typeCrop land                  0.91443    0.28570   3.201  0.00137 ** 
Landuse_typeEucalyptus plantation      0.41899    0.39504   1.061  0.28886    
Landuse_typeHuman settlement           0.59565    0.31476   1.892  0.05844 .  
Landuse_typeMacadamia orchard          1.57537    0.33388   4.718 2.38e-06 ***
Landuse_typeNatural                    0.41729    0.35191   1.186  0.23571    
Elevational_zoneLow                    0.36690    0.17858   2.055  0.03992 *  
Elevational_zoneMid                    0.02949    0.20112   0.147  0.88341    
habitatforest                          0.40621    0.35825   1.134  0.25685    
habitatopen woodland                   0.55462    0.32113   1.727  0.08416 .  
habitatsedgeland                       0.67863    0.35958   1.887  0.05912 .  
aspectSouth                            0.19547    0.17012   1.149  0.25054 



mod4 <-glmer(Activity ~ Season + Landuse_type + Elevational_zone
+ aspect + habitat + (1|elevation)+ (1|categories),
data = spider_univariate_data, weight = cov, family = poisson)
summary(mod4)    



Fixed effects:
Estimate Std. Error z value Pr(>|z|)    
(Intercept)                           -0.04441    0.42200  -0.105  0.91619    
Seasonhotwet                          -0.45227    0.05098  -8.872  < 2e-16 ***
Landuse_typeCleared bush encroachment  2.08682    0.19577  10.660  < 2e-16 ***
Landuse_typeCrop land                  2.94392    0.15450  19.054  < 2e-16 ***
Landuse_typeEucalyptus plantation      1.22349    0.42867   2.854  0.00431 ** 
Landuse_typeHuman settlement           1.86333    0.34939   5.333 9.66e-08 ***
Landuse_typeMacadamia orchard          3.10879    0.40519   7.672 1.69e-14 ***
Landuse_typeNatural                    0.64651    0.46235   1.398  0.16202    
Elevational_zoneLow                    0.65791    0.25524   2.578  0.00995 ** 
Elevational_zoneMid                   -0.09365    0.22942  -0.408  0.68314    
aspectSouth                            0.46197    0.15782   2.927  0.00342 ** 
habitatforest                          1.32221    0.47452   2.786  0.00533 ** 
habitatopen woodland                   1.17767    0.45127   2.610  0.00906 ** 
habitatsedgeland                       1.71759    0.52152   3.293  0.00099 ***

  
                         ####Transformation GLMM
  
#Ants
trans <- glmer(Species_richness ~ Transformation +aspect+(Season*Elevational_zone)
+(1|elevation)+ (1|categories),data = ant_univariate_data, weight = cov, family = poisson)
summary(trans)   

#Fixed effects:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                       1.85138    0.19885   9.310  < 2e-16 ***
  TransformationUntransformed       0.21204    0.16590   1.278 0.201200    
aspectSouth                       0.12819    0.11885   1.079 0.280772    
Seasonhotwet                      0.44047    0.08529   5.165 2.41e-07 ***
  Elevational_zoneLow               0.50284    0.13613   3.694 0.000221 ***
  Elevational_zoneMid               0.22265    0.17730   1.256 0.209210    
Seasonhotwet:Elevational_zoneLow -0.22925    0.10698  -2.143 0.032116 *  
  Seasonhotwet:Elevational_zoneMid -0.18127    0.15154  -1.196 0.231623


trans2 <- glmer(Activity ~ Transformation +aspect+(Season*Elevational_zone)
+(1|elevation)+ (1|categories),data = ant_univariate_data, weight = cov, family = poisson)
Summary(trans2)




#Spider
trans3 <- glmer(Species_richness ~ Transformation +aspect+(Season*Elevational_zone)
 + (1|elevation)+(1|categories),data = spider_univariate_data, weight = cov, family = poisson)
summary(trans3) 

Fixed effects:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                       1.55232    0.20048   7.743  9.7e-15 ***
  TransformationUntransformed      -0.01481    0.15253  -0.097    0.923    
aspectSouth                      -0.01923    0.13715  -0.140    0.889    
Seasonhotwet                     -0.19605    0.15603  -1.256    0.209    
Elevational_zoneLow               0.04059    0.14961   0.271    0.786    
Elevational_zoneMid              -0.03052    0.20268  -0.151    0.880    
Seasonhotwet:Elevational_zoneLow -0.04819    0.20546  -0.235    0.815    
Seasonhotwet:Elevational_zoneMid -0.28200    0.30965  -0.911    0.362  


trans4 <- glmer(Activity ~ Transformation +aspect+(Season*Elevational_zone)
+(1|elevation)+ (1|categories),data = spider_univariate_data, weight = cov, family = poisson)
Summary(trans4)




###### GLM FINAL

#Ants
mod <- glmer(Species_richness ~ Season + Landuse_type + Elevational_zone
+ aspect + habitat +(1|categories) + (1|elevation),
data = ant_univariate_data, weight = cov, family = poisson)
summary(mod)


(Intercept)                            2.43579    0.16815  14.486  < 2e-16 ***
Seasonhotwet                           0.28848    0.04762   6.057 1.38e-09 ***
Landuse_typeCleared bush encroachment  0.23632    0.12319   1.918 0.055063 .  
Landuse_typeCrop land                  0.18167    0.12419   1.463 0.143489    
Landuse_typeEucalyptus plantation     -0.77320    0.18930  -4.085 4.42e-05 ***
Landuse_typeHuman settlement           0.09485    0.12682   0.748 0.454536 
Landuse_typeMacadamia orchard         -0.18406    0.16462  -1.118 0.263522    
Landuse_typeNatural                    0.04218    0.12896   0.327 0.743592    
Elevational_zoneLow                    0.03704    0.09077   0.408 0.683204    
Elevational_zoneMid                    0.16869    0.09412   1.792 0.073075 .  
aspectSouth                            0.08786    0.08051   1.091 0.275153    
habitatforest                         -0.56778    0.15256  -3.722 0.000198 ***
habitatopen woodland                  -0.10183    0.13385  -0.761 0.446823    
habitatsedgeland                      -0.35161    0.16143  -2.178 0.029403 * 

  
mod1 <- glmer(Activity ~ Season + Landuse_type + Elevational_zone
+ aspect  +(1|categories) + (1|elevation),
data = ant_univariate_data, weight = cov, family = poisson)
summary(mod1)  

(Intercept)                            4.24974    0.15717  27.039  < 2e-16 ***
Seasonhotwet                           0.91721    0.01001  91.641  < 2e-16 ***
Landuse_typeCleared bush encroachment  0.37612    0.03358  11.201  < 2e-16 ***
Landuse_typeCrop land                  2.27660    0.42689   5.333 9.66e-08 ***
Landuse_typeEucalyptus plantation     -0.90871    0.27552  -3.298 0.000973 ***
Landuse_typeHuman settlement           0.94762    0.32214   2.942 0.003265 ** 
Landuse_typeMacadamia orchard          0.69712    0.27596   2.526 0.011531 *  
Landuse_typeNatural                    0.28142    0.07228   3.893 9.89e-05 ***
Elevational_zoneLow                   -0.21756    0.20165  -1.079 0.280627    
Elevational_zoneMid                    0.07549    0.25094   0.301 0.763542    
aspectSouth                            0.49863    0.04264  11.694  < 2e-16 ***
  
  
#Spider  
  drop1(mod3, test = "Chisq")

mod3 <- glmer(Species_richness ~ Season + Landuse_type + Elevational_zone
+ habitat + aspect +(1|categories) + (1| elevation),
data= spider_univariate_data, weight = cov, family = poisson)
summary(mod3)  

Estimate Std. Error z value Pr(>|z|)    
(Intercept)                            0.38882    0.37444   1.038  0.29908    
Seasonhotwet                          -0.26740    0.09466  -2.825  0.00473 ** 
Landuse_typeCleared bush encroachment  0.37852    0.31915   1.186  0.23561    
Landuse_typeCrop land                  0.91443    0.28570   3.201  0.00137 ** 
Landuse_typeEucalyptus plantation      0.41899    0.39504   1.061  0.28886    
Landuse_typeHuman settlement           0.59565    0.31476   1.892  0.05844 .  
Landuse_typeMacadamia orchard          1.57537    0.33388   4.718 2.38e-06 ***
Landuse_typeNatural                    0.41729    0.35191   1.186  0.23571    
Elevational_zoneLow                    0.36690    0.17858   2.055  0.03992 *  
Elevational_zoneMid                    0.02949    0.20112   0.147  0.88341    
habitatforest                          0.40621    0.35825   1.134  0.25685   
habitatopen woodland                   0.55462    0.32113   1.727  0.08416 .  
habitatsedgeland                       0.67863    0.35958   1.887  0.05912 .  
aspectSouth                            0.19547    0.17012   1.149  0.25054

mod4 <- glmer(Activity ~ Season + Landuse_type + Elevational_zone
+ habitat + aspect +(1|categories) + (1| elevation),
data= spider_univariate_data, weight = cov, family = poisson)
summary(mod4) 
