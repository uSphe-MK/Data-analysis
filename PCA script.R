
##########################Principal Component Analysis#########################

library(factoextra)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(grid)
library(vegan)
library(cluster)
library(ggbiplot)

#Create matrix
data<-as.matrix(Elemental_analysis_23_24[,5:20])
data<-as.matrix(Form.from.abund[,3:18])
data<-as.matrix(Soil.and.plant.nutrients_NAs[,4:17])
data<-as.matrix(NWP_Analysis[,5:11])
data<-as.matrix(PCA_data_Nov23[,4:16])
data<-as.matrix(PCA.data.Feb24_mean[,3:15])
data<-as.matrix(New_PCA_Data_1_[,5:16])
data<-as.matrix(Fertility[,6:20])
data<-New_PCA_Data_1_[,5:16]
data<-N_and_K_Fertigation_Yield_Data_2024[,5:22]

#Try bray curtis matrix also
db2<-vegdist(Soil.texture.Dbase[,2:13], method="bray")
db3<-vegdist(Soil.Texture[,15:26], method = "bray")
db4<-vegdist(PCA_data_Oct23b[,4:117], method = "bray")
db5<-vegdist(Cover[,4:117], method = "bray")
db6<-vegdist(rel7, method = "bray")

#Run pca
pca<-prcomp(data, center = TRUE, scale= TRUE)
pca1<- prcomp(data1, center=TRUE, scale=TRUE)
pca6<-prcomp(data7, center = TRUE, scale= TRUE)
pca7<-princomp(data, center = TRUE, scale= TRUE)
pca<-prcomp(db, center = TRUE, scale= TRUE)
pca<-rda(data, center=TRUE, scale=TRUE)
pca<-prcomp(db6, center = TRUE, scale= TRUE)

#Variation explained by components
var1<-round(pca$sdev[1]^2/sum(pca$sdev^2)*100,2)
var2<-round(pca$sdev[2]^2/sum(pca$sdev^2)*100,2)

#Results
summary(pca)
print(pca)
summary(pca7)
print(pca7)

#Visualize pca results

#Scree plot
screeplot(pca,type = "line", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue= 1"), col=c("red"),lty=5, cex=1)

fviz_eig(pca)

cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance",
     main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"), col=c("blue"), lty=5, cex=0.6)

#PCA plots
#cos2- Color by the quality of representation
#repel- Avoid text overlapping
#Graph of variables- Positive correlated variables point to the same side of the plot. 
#Negative correlated variables point to opposite sides of the graph.


###Groups and vectors
group<-as.factor(SPHE_Makungo23_00000003_Final12$`Cultivar name`)
group<-as.factor(PCA_data_Oct23b$`Fire treatment`)
group<-as.factor(PCA_data_Oct23b$Type)
group<-as.factor(Form.from.abund$Treatment)
group<-as.factor(NWP_Analysis$Location)
groupG<-as.factor(Data_Tuber$Genotypes)
groupP<-as.factor(Data_Tuber$Date)



pshapet<-c("Grass"=22,"Forb"=25)
pshape<-c("AA"=15,"As"=8,"BA"=17,"BS"=9,"IB"=19)
pcol<-c("AA"="red","AS"="black","BA"="blue","BS"= "darkgreen","IB"= "darkorange") 
treatnames<- c("AA","As","BA", "BS", "IB")

pcol<-c("F.AA"="black","F.As"="blue","F.BA"="red","F.BA"="darkgreen","F.BS"="navy", 
"F.IB"="darkorange","G.AA"="yellow","G.As"="orange","G.BA"="lightgreen",
"G.BA"="lightblue","G.BS"="lightgrey", "G.IB"="lightpink")

fill.palette = c("AA"="pink","AS"="grey","BA"="lightblue","BS"= "lightgreen","IB"= "yellow")

covernames= c("Bo","Bo+V","C bare","C weeds","Sr","Sr + V","V","Wc", "Wc + Bo")
         
covershape=c("Bo"=15,"Bo+V"=16, "C bare"=2, "C weeds"=5,
"Sr"=6,"Sr + V"=8,"V"=0,"Wc"=17, "Wc + Bo"= 14)
         
colind<- c("Combined"= "lightgrey", "Control"= "lightgreen", 
           "Inorganic Fertilizer" = "lightblue", "Organic Fertilizer"= "lightpink")

shapind<- c("Combined"= 16, "Control"= 17, 
            "Inorganic Fertilizer" = 18, "Organic Fertilizer"= 19)


                         #####Basic PCA graphs#####

fviz_pca_ind(pca, col.ind="cos2", 
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"),repel=TRUE)

fviz_pca_var(pca1, col.var="contrib", 
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"),repel=TRUE)

#OR

b=fviz_pca_biplot(pca,xlab="PC1 (39.18%)", ylab= "PC2 (19.92%)",
geom=c("text", "point"),col.var="black",col.ind="black", label = "all", 
geom.ind = c("label"), 
geom.var = c("text", "arrow"),
addEllipses=TRUE,ellipse.type= "convex",,
col.ind= PCA.data.Feb24_mean$Type,repel = TRUE)+ theme_set(theme_bw()) + 
  theme( plot.background = element_blank(), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank())
b+geom_point(aes(color = PCA.data.Feb24_mean$Fire.treatment,
shape=PCA.data.Feb24_mean$Type)) +
coord_fixed() +
labs(colour= "Fire treatment", shape="Functional type") 

###Using ggplot

ggplot_pca(pca,
choices = 1:2,
scale = 1,
pc.biplot = TRUE,
labels = NULL,
labels_textsize = 3,
labels_text_placement = 1.5,
groups = PCA.data.Feb24_mean$Fire.treatment,
addEllipse = TRUE,ellipse_type="norm", ellipse_prob = 0.68,
points_size = 2,
points_alpha = 0.25,repel=TRUE,
arrows = TRUE,
arrows_colour = "black",
arrows_size = 0.5,
arrows_textsize = 3,
arrows_textangled = TRUE,
arrows_alpha = 0.75,
base_textsize = 10)


####*** for grouping by 2 explanatory variables

fviz_pca_biplot(pca,xlab="PC1 (40.49%)", ylab= "PC2 (32.11%)",
geom=c("text"),
col.var="black",
fill.ind = PCA.data.Feb24_mean$Type, addEllipses = TRUE,
ellipse.type= "convex", ellipse.level = 0.90,
repel = TRUE,
label = "all") + theme_set(theme_bw()) + 
theme(plot.background = element_blank(), 
panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + 
geom_point(aes(color = PCA.data.Feb24_mean$Fire.treatment,
shape=PCA.data.Feb24_mean$Type)) +
coord_fixed() + labs(colour= "Fire treatment", shape="Functional type")  


#lwando
PD<-c("December"= "black",
             "February"= "grey",
             "January"= "red",
             "March"= "green",
             "May"= "blue")
PD<-c("December"= "black",
      "February"= "grey",
      "January"= "red",
      "March"= "green",
      "May"= "blue")
GT<-c("Endurance"="pink","Line 2"="beige",
      "Nooitgedacht"="skyblue")

GT<-c("Endurance"="18","Line 2"="23",
      "Nooitgedacht"="15")

###best
fviz_pca_biplot(pca,xlab="PC1 (31.91%)", ylab= "PC2 (15.93%)",
                     geom.var = c("text", "arrow"),
                     geom.ind = c("point"),alpha.ind=0,
                     pointsize=0,alpha.var=0.5,
                     fill.ind = N_and_K_Fertigation_Yield_Data_2024$Potassium,
                     col.var="black",
                     addEllipses = TRUE,
                     ellipse.type= "confidence", 
                     ellipse.level = 0.80,
                     repel = TRUE,
                     label = c("all")) 


+ 
                     geom_point(aes(size=0.2,
                     shape=N_and_K_Fertigation_Yield_Data_2024$Potassium),
                     alpha=0.3) + coord_fixed() + 
                    labs(colour="Nitrogen",shape="Potassium")
                    


#scale_shape_manual(values=c(2, 8, 11))- change shapes
#geom_text_repel(aes(label = name))


# Assume you have a grouping variable, e.g., "Year"
g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, 
              groups = c(Data_Leaf$Genotypes), # Change to your grouping variable
              ellipse = TRUE,  # Add ellipses around groups
              circle = TRUE)   # Add a unit circle

# Customize the plot
g <- g + scale_color_discrete(name = 'Year') + 
  theme_minimal() + 
  theme(legend.position = 'bottom')

# Display the plot
print(g)


#
fviz_pca_biplot(pca,xlab="PC1 (25.58%)", ylab= "PC2 (22.12%)",
                geom.var = c("text", "arrow"),
                geom.ind = c("point"),alpha.ind=5,
                pointsize=2,alpha.var=0.4,
                fill.ind = New_PCA_Data_1_$Treatment,
                col.var="black",col.ind = New_PCA_Data_1_$Treatment,
                addEllipses = TRUE,
                ellipse.type= "confidence", 
                ellipse.level =0.99,
                repel = TRUE,
                label = c("all")) +  
  labs(colour="Land-use",shape="Fertilizer rate") + 
  theme(plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_point(aes(size=0.0),
  alpha=0.0) + coord_fixed() + 
  labs(colour="Tillage",shape="Fertilizer rate") + 
  theme(plot.background = element_blank(), 
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) 
                

####***Reminders

#palette-for colors
#habillage- color and shape
#alpha-transparency
#scale_shape_manual(values=c(2, 8, 11))- change shapes
#geom_point(col = dfekm$rgb, size = 4) +
#geom_text_repel(aes(label = name))
#+ coord_fixed()

#
fviz_pca_biplot(pca,xlab="PC1 (44.06%)", ylab= "PC2 (30.63%)",
geom.var =  c("arrow","text"),
col.var= "black",
geom.ind = c("point"),
fill.ind = PCA.data.Feb24_mean$Fire.treatment,
habillage = PCA.data.Feb24_mean$Type,
palette = c("pink","grey","lightblue", "lightgreen", "yellow"),
addEllipses= TRUE, ellipse.type= "convex", ellipse.level = 0.90,
labelsize=4,
repel = TRUE, 
legend.title= "species")+
labs(fill.ind=PCA.data.Feb24_mean$Fire.treatment, habillage=PCA.data.Feb24_mean$Type)+
theme(plot.background = element_blank(), 
panel.grid.major = element_blank(),
panel.grid.minor = element_blank())


#
fviz_pca_biplot(pca1,
geom.ind="point",
fill.ind=PCA_data_Oct23b$`Fire treatment`,
col.ind="cos2",
pointshape=21,pointsize=2,
mean.point=FALSE,
alpha.var="contrib",col.var="contrib",
gradient.cols=c("#00AFBB","#E7B800","#FC4E07"),
labelsize=5,
repel=TRUE,
xlab="PC 1",
ylab="PC 2")+
labs(fill="Uso", color="Contrib",alpha="Contrib")+
ggpubr::fill_palette(c("green","yellow","red", "black", "blue"))+
theme(text=element_text(size=9),
axis.title=element_text(size=15),
axis.text=element_text(size=12))


#  
fviz_pca_biplot(pca1,xlab="PC1 (72.88%)", ylab= "PC2 (21.39%)",
col.var= "black", geom= c("point", "text"),
pointshape= 25,pointsize=3,
geom.ind = c("point", "label"),
addEllipses= TRUE, 
ellipse.type= "norm", ellipse.level = 0.90,
geom.var =  c("arrow","text"),labelsize=4,
repel = TRUE, legend.title= "Species type") +
theme(plot.background = element_blank(), 
panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) 

#  
fviz_pca_biplot(pca,xlab="PC1 (29.79%)", ylab= "PC2 (20.04%)",
col.var= "black", 
geom= c("point", "text"),
pointshape= 21,pointsize=2,
fill.ind = group, 
geom.ind = c("point"),
addEllipses= TRUE,
ellipse.type= "norm", ellipse.level = 0.90,
geom.var =  c("arrow","text"),
repel = TRUE, 
legend.title= "Treatment") +
theme(plot.background = element_blank(), 
panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) 


#####for an rda pca
biplot(pca, scaling = 1, labSize=2,lab=rownames(PCA_data_Oct23$Treatment),
drawConnectors = TRUE,type = "text", xlab= "PC1 (43.55%)", ylab="PC2 (30.77%)")

biplot(pca1, colby = groups, colkey = groups,
       shape = groups, shapekey = c(A=10, B=21), legendPosition = 'bottom')

biplot(pca, axes = c(1, 2),
  type = c("form", "covariance"),
  active = TRUE,
  sup = TRUE,
  labels = "variables",
  col.rows = "#004488",
  col.columns = "#BB5566",
  pch.rows = 16,
  pch.columns = 17,
  lty = "solid",
  lwd = 2,
  main = NULL,
  sub = NULL)


#
fviz_pca_biplot(pca,xlab="PC1 (41.09%)", ylab= "PC2 (29.87%)",
col.var= "black", geom= c("point", "label"),pointshape= 21,pointsize=1,
fill.ind = groups, geom.ind = c("point", "label"),
addEllipses= TRUE, ellipse.type= FALSE, ellipse.level = 0.90,
geom.var =  c("arrow","text"),
repel = TRUE, legend.title= "Treatment") +
theme(plot.background = element_blank(), 
panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) 


#
fviz_pca_biplot(pca4,xlab="PC1 (22.9%)", ylab= "PC2 (21.8%)",
col.var= "black", geom= c("point", "label"),pointshape= 21,pointsize=2,
fill.ind= groups, 
labelsize = 5, arrowsize = 0.5,
addEllipses=TRUE , ellipse.type= "norm", ellipse.level = 0.95,
geom.var =  c("arrow", "text"),
repel = TRUE, legend.title= "Treatment Class") + 
theme(plot.background = element_blank(), 
panel.grid.major = element_blank(),
panel.grid.minor = element_blank()) 


#
fviz_pca_biplot(pca, xlab="PC1 (22.9%)", ylab= "PC2 (21.8%)",
col.var= "black", 
geom= c("point", "label"),pointshape= 21,pointsize=2,
fill.ind = groupG, 
labelsize = 5, 
arrowsize = 0.5,
addEllipses=TRUE , 
ellipse.type= "norm", ellipse.level = 0.95,
geom.var =  c("arrow", "text"),
repel = TRUE, 
legend.title= "Treatment Class") +
scale_x_continuous(breaks= seq(-4,4,1), limits = c(-4,4)) +
theme_grey(base_size=16, base_family="serif")+
theme(axis.line= element_line(size=0.5, colour="black"),
axis.title.y= element_text (margin = margin(t=0, r=0, b=0, l=0)),
legend.position = c(0.9,0.1),
legend.title = element_blank(),
legend.key = element_rect(color = "white", fill = "white"),
legend.background= element_rect(fill= alpha("grey",.05)),
plot.margin= margin(-0.5,0.5,0,0.3,"cm"))+
windows(width=6, height=5.5)


