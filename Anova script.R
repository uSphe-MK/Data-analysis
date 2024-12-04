.

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(afex)
library(emmeans)
library(multcomp)

#################################ANOVA TEST#################################

#Load data
datafile<- read.csv(D:\Brotherton.soil.MS.All_treatAP, header = TRUE,
colClasses = c("factor", "factor", "factor", "numeric"))
datafile<-readxl::E:\Work\Data Analysis\Clint Carbutt\Biomass data 19-24 Genstat



anova<-aov(Exch_acid~Burn+Depth+Rep+Burn*Depth, data=Brotherton.soil.MS.All_treatAP)

summary(anova)
print(anova)

anova1<- aov(A260.A280~ amount.of.soil+with.or.without.incubation.in.CaCO3+
incubation.time, data = DNA.extraction.method.optimisation)

summary(anova1)
print(anova1)

anova2<- aov(A260.A230~ amount.of.soil+
with.or.without.incubation.in.CaCO3+
incubation.time, data = DNA.extraction.method.optimisation)

summary(anova2)
print(anova2)


anova3<-aov(Exch_acid~Burn+Depth+Rep+Burn*Depth+ Burn*Depth*Rep,
            data=Brotherton.soil.MS.All_treatAP)

summary(anova3)
print(anova3)


#
anov<-aov(`Mycellium growth (mm)`~Treatment+Time+Rep+Treatment*Time, 
          data =Mycelium_growth_rate_averages )
summary(anov)
print(anov)

a<-kruskal.test(`Mycellium growth (mm)`~Treatment, 
                data =Mycelium_growth_rate_averages)

kruskal.test(`Mycellium growth (mm)`~Time, 
             data =Mycelium_growth_rate_averages)


# To find best model use AIC model selection:
library(AICcmodavg)
model.set<- list(anova, anova2)
model.names<- c("anova", "anova2")
aictab(model.set, modnames= model.names)

Model selection based on AICc:
  K  AICc Delta_AICc AICcWt Cum.Wt    LL
anova  15 33.26        Inf    NaN    NaN 10.37
anova2 37  -Inf        NaN    NaN    NaN   Inf

#The model with the lowest AIC score (listed first in the table) is 
#the best fit for the data

#NOTES
#1-Way
one.way <- aov(yield ~ fertilizer, data = crop.data)
#2-way
two.way <- aov(yield ~ fertilizer + density, data = crop.data)
#Interaction
interaction <- aov(yield ~ fertilizer*density, data = crop.data)
#Block
blocking <- aov(yield ~ fertilizer + density + block, data = crop.data)


Method3.extraction$DNA.yield..ng.ul.
Method3.extraction$dna.extraction.efficiency..ug.g.wt.soil.


one.way <- aov(DNA.yield..ng.ul. ~ extraction.method, data = Method3.extraction)
summary(one.way)


Y<-Mr.Zulu.cultivar.trail$Yield..t.ha.
ggdensity(my_data$len, 
          main = "Yield (t/ha)",
          xlab = "")

qqPlot(Mr.Zulu.cultivar.trail$Yield..t.ha.)

shapiro.test(Mr.Zulu.cultivar.trail$Yield..t.ha.)

#From the output, the p-value > 0.05 implying that the distribution of the data 
#are not significantly different from normal distribution. 
#In other words, we can assume the normality.

Mr.Zulu.cultivar.trail$
ano<- aov(Yield..t.ha. ~ Cultivar*Season*Treatment*Rep, 
          data = Mr.Zulu.cultivar.trail)
summary(ano)


kruskal.test(Yield..t.ha. ~ Cultivar * Season * Treatment, 
            data = Mr.Zulu.cultivar.trail)

aov <- aov(Yield..t.ha.~ Cultivar * Season * Treatment, 
                data = Mr.Zulu.cultivar.trail)
summary(aov2)



#1 check normality (if p-value is less than or equal to 0.05 then NOT NORMAL)
shapiro.test(Dundee.Sphe.final.xlsx$Yield..t.ha.)
shapiro.test(Dundee.Sphe.final.xlsx$MOISTURE.)

shapiro.test(soil_moisture$`10cm`)
shapiro.test(soil_moisture$`20cm`)


anova <- aov(10cm ~ Days after Planting + Location, data = soil_moisture)

summary(anova)

aov <- aov(Yield..t.ha.~ CV..NAME, data = Dundee.Sphe.final1)
summary(aov)
plot(anova)
TukeyHSD(aov)
aov
tukey.plot.aov<-aov(Yield..t.ha.~  CV..NAME, data=Dundee.Sphe.final1)

tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

pairwise.t.test(Dundee.Sphe.final1$Yield..t.ha.,
                Dundee.Sphe.final1$CV..NAME, p.adj="none")

aov <- aov(MOISTURE.~ CV..NAME, 
           data = Dundee.Sphe.final.xlsx)

#calculate CV%
sapply(Dundee.Sphe.final.xlsx, function(Yield..t.ha.) sd(Yield..t.ha., na.rm=T) / 
         mean(Yield..t.ha., na.rm=T) * 100)

#anova table
ano<-aov(Seed_density ~ `Slope!` + `SR!` + `Camp!` +`Type!`, 
         data= Univariate_soil_seed_bank_data_Copy)
summary(ano)

#table of means
aggregate(Univariate_soil_seed_bank_data_Copy$Seed_density, 
          list(Univariate_soil_seed_bank_data_Copy$`Camp!`), FUN=mean)

#calculate CV%
sapply(Univariate_soil_seed_bank_data_Copy, function(Seed_density) sd(Seed_density, na.rm=T) / 
         mean(Seed_density, na.rm=T) * 100)

#Post Hoc


data<-kokstad_feeding_trial_11_01_24_to_15_04_24
# Fit the Repeated Measures ANOVA model
model <- aov_ez(id =data$`TAG NUMBER`, dv = data$WEIGHT, within = data$DATE, data = data)
summary(model)


### Dr. Manson
library(readxl)
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)
library(emmeans)
options(max.print = .Machine$integer.max)

# Load the data
data<-Biomass_data_19_24_Genstat

#ANOVA test
anova<- aov(`Dry biomass (g)`~Treatment*`Functional group`*Year, 
            data = data)
Error(`Plot No.`)
#Results
print(anova)
summary(anova)

#standard error function
se <- function(x) {sd(x) / sqrt(length(x))}

#summary table
summary <- group_by(data,Year,Treatment,`Functional group`) %>%
  summarise(mean=mean(`Dry biomass (g)`), 
            se=se(`Dry biomass (g)`)) %>%
  arrange(desc(Year))

print(summary,n=Inf)

#Post hoc

#Treatment
emmT <- emmeans(anova, ~ Treatment)
tukeyT<- pairs(emmT, adjust = "tukey")
print(tukeyT)

cldT <- multcomp::cld(emmT, Letters =letters)%>%
  arrange(desc(emmean))
print(cldT)

#Function group
emmFG <- emmeans(anova, ~ `Functional group`)
tukeyFG<- pairs(emmFG, adjust = "tukey")
print(tukeyFG)

cldFG <- multcomp::cld(emmFG, Letters =letters)%>%
  arrange(desc(emmean))
print(cldFG)

#Year
emmY <- emmeans(anova, ~ Year)
tukeyY<- pairs(emmY, adjust = "tukey")
print(tukeyY)

cldY <- multcomp::cld(emmY, Letters =letters) %>%
  arrange(desc(emmean))
print(cldY)

#Treatment*Functional Group
emmTF <- emmeans(anova, ~ Treatment|`Functional group`)
tukeyTF<- pairs(emmTF, adjust = "tukey")
print(tukeyTF)

cldTF <- multcomp::cld(emmTF, Letters =letters)%>%
  arrange(desc(emmean))
print(cldTF)

#Year*Functional Group
emmYF <- emmeans(anova, ~ Year|`Functional group`)
tukeyYF<- pairs(emmYF, adjust = "tukey")
print(tukeyYF)

cldYF <- multcomp::cld(emmYF, Letters =letters)%>%
  arrange(desc(emmean))
print(cldYF)

#Plot significant interactions
#Treatment*Functional group

ggplot(data,aes(Treatment,`Dry biomass (g)`)) + 
  geom_boxplot() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1)) +
  facet_grid(cols = vars(`Functional group`), scales = "free_y")

#Year*Functional group
ggplot(data,aes(Year,`Dry biomass (g)`)) + 
  geom_boxplot() +
  theme_classic() + 
  theme(axis.text.x = element_text(angle = 0, vjust = 1, hjust = 1) ) +
  facet_grid(.~`Functional group`)
