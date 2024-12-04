       ########################Linear Regression#######################

library(ggplot2)
library(dplyr)
library(broom)
library(tidyverse)
library(lme4)  
library(ggpubr)
library(rstatix)
library(caret)
library(ggResidpanel)
library(readxl)
library(ggpmisc)
library(gridExtra)
library(cowplot)
library(flexplot)  
library(Kendall)
library(zyp)  
library(agricolae)
library(ggdendro)
library(stats)   
library(dendextend)       
library(factoextra)
library(ggrepel)       
 
install.packages("devtools")
# install the stable version
devtools::install_github("dustinfife/flexplot")
# install the development version
devtools::install_github("dustinfife/flexplot", ref="development")
    

#unicode for ^3
U+00B3 = ^3

#scale_linetype_manual() : to change line types
#scale_color_manual() : to change line colors
#scale_size_manual() : to change the size of lines




                          #######check assumptions######
#1 Normality
hist(Ratings$Yield)
hist(soil_moisture$Soil_moisture_content)
hist(Book_2_for_Sphe$`Total fruits yield per plot (Kg)`)
hist(Genstat$`P. density`)
hist(Lwando_Regression$DE)

#2 Linearity
plot(Yield ~ DSI, data = Ratings)
abline(lm(Yield ~ DSI)) #add fitted line

plot(DSI ~ Final.Disease.Severity, data= Ratings)
abline(lm(DSI ~ Final.Disease.Severity))

plot(Soil_moisture_content~ Days_after_Planting, data= soil_moisture)
abline(lm(soil_moisture$Soil_moisture_content~soil_moisture$Days_after_Planting))


#Check correlation

cor(Ratings$DSI, Ratings$Final.Disease.Severity)
cor(Ratings$Yield, Ratings$Final.Disease.Severity)

cor(soil_moisture$Soil_moisture_content, soil_moisture$Depth)
cor(soil_moisture$Soil_moisture_content, soil_moisture$Days_after_Planting)


#or get specific coefficients
a <- coef(model)[1]
print(a)
Cover <- coef(model)[2]
print(Cover)
XN.rate <- coef(model)[3]
print(N.rate)


#
model<- lm(CCI.N.potato.trial$X38DAS~CCI.N.potato.trial$N.Fert, 
data = CCI.N.potato.trial)
summary(model)
print(model)
plot(model)
plot(df$x, df$y)
abline(model)
predict(model, newdata = CCI.N.potato.trial)


modelA<-lm(C..N.leafy~ C.leaf,
data = Genstat.sheet_Mkhonza.Feb.2024_.Cover.crop.stochiometry.study)
summary(modelA)
print(modelA)
plot(modelA)


modA<-ggplot(data=Genstat.sheet_Mkhonza.Feb.2024_.Cover.crop.stochiometry.study, 
  aes(x=C.leaf, y=C..N.leafy)) +
  geom_smooth(method="lm") +
  geom_point() + theme_classic() +
  xlab("Leaf carbon (%)") + ylab("Leaf C:N ratio") +
  annotate("text", x=c(50.3,50.3,50.3), y=c(35, 33, 31),
  label= c("y = -3.661x + 201.981",
  "R?: 0.1801 ",
  "p: 0.001"), cex=5)

modA + theme(text = element_text(size = 14))   


#create plot with regression line, regression equation, and R-squared
ggplot(data=Genstat.sheet_Mkhonza.Feb.2024_.Cover.crop.stochiometry.study, 
  aes(x=N.leaf, y=C..N.leafy)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=3, label.y=27) +
  stat_cor(aes(label=..rr.label..), label.x=3, label.y=26) + 
  xlab("Leaf nitrogen") + ylab("Leaf C:N ratio") + 
  stat_pvalue_manual(data, label = "p=")
  
#
model1<- lm(soil_moisture$Soil_moisture_content~ soil_moisture$Depth,
data=soil_moisture)
summary(model)
print(model)

plot(model)
plot(df$x, df$y)
abline(model)
predict(model, newdata =soil_moisture)

#
ggplot(data=Genstat, aes(x=N_Fert, y=`Haulms/plant`)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=240, label.y=7.5) +
  stat_cor(aes(label=..rr.label..), label.x=240, label.y=7.3)



#
model<-lm(Genstat$`Days_to_75%_emerg`~ Genstat$N_Fert, data = Genstat)
summary(model)
print(model)
plot(df$x, df$y)
abline(model)
predict(model, newdata =soil_moisture)

#
ggplot(data=Genstat, aes(x=N_Fert, y=`Days_to_75%_emerg`)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=240, label.y=16) +
  stat_cor(aes(label=..rr.label..), label.x=240, label.y=17)

ggplot(data=Genstat, aes(x=N_Fert, y=`Days_to_75%_emerg`)) +
  geom_point() +
  stat_smooth(method="lm", formula = y~x) +
  stat_regline_equation(label.x=240, label.y=16) +
  stat_cor(aes(label=..rr.label..), label.x=240, label.y=17)


#
model1<-lm(Genstat$`P. density`~Genstat$N_Fert, data = Genstat)
summary(model1)
print(model1)
plot(model)
plot(df$x, df$y)
abline(model1)
predict(model1, newdata =Genstat)

#create plot with regression line, regression equation, and R-squared
ggplot(data=Genstat, aes(x=N_Fert, y=`P. density`)) +
  geom_smooth(method="lm") +
  geom_point() +
  stat_regline_equation(label.x=240, label.y=3000) +
  stat_cor(aes(label=..rr.label..), label.x=240, label.y=3001)


### 
modelADF<-lm(ADF ~ `Fodder radish (%)`, data = Regression) 
summary(modelADF)
print(modelADF)
AIC(modelADF)

#Plot
modADF<-ggplot(Regression,
  aes(`Fodder radish (%)`, ADF)) +
  geom_point() + theme_classic() + 
  geom_smooth(method = lm, formula = y ~ x) +
  xlab("Fodder radish (%)") + ylab("Acid detergent fiber (%)") +
  annotate("text", x=c(25,25,25), y=c(44.5, 43,41.5),
  label= c("y= 44.4206 - 0.2838x",
  "R?: 0.7616",
  "p: <0.001"),cex=5)

modADF + theme(text = element_text(size = 14))


### 
modelCP1<-lm(CP ~ `Fodder radish (%)`, data = Regression) 
summary(modelCP1)
print(modelCP1)

#Plot
modCP1<-ggplot(Regression,
                 aes(`Fodder radish (%)`, CP)) +
  geom_point() + theme_classic() + 
  geom_smooth(method = lm, formula = y ~ x) +
  xlab("Fodder radish (%)") + ylab("Crude protein (%)") +
  annotate("text", x=c(15,15,15), y=c(12,11.2,10.4),
           label= c("y= 5.2104 - 0.1421x",
                    "R?: 0.9377",
                    "p: <0.001"),cex=5)

modCP1 + theme(text = element_text(size = 14))

#assumptions:
plot(DMI ~ `Fodder radish (%)`, data = Regression)

abline(lm(Regression$DMI  ~ Regression$`Fodder radish (%)`))

#Correlation:
corD<-cor.test(Regression$DMI ~ Regression$`Fodder radish (%)`,
               method = c("spearman"))

corplot<- ggscatter(Regression,
                    x = Regression$`Fodder radish (%)`, y = Regression$DMI 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = "spearman",
                    xlab = "FR(%)", ylab = "DMI")


####GROUPED DATA####

#Hauls
plotHauls<-ggplot(Genstat_ready,aes(x = Fert, y = `Hauls/plant`, colour = Time)) +
geom_point() + theme_classic() +
geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
scale_color_brewer(palette = "Dark2") +
labs(x = "Nitrogen fertilizer (kg/ha)", y = "Hauls/plant") +
annotate("text", x=c(200,200,200), y=c(10,9.5,9),
           label= c("y= 6.9280 + 0.0453x",
                    "R?: 0.6455",
                    "p: <0.001"),cex=3)

anovaHauls<-anova(lm(`Hauls/plant`~ Fert*Time, data = Genstat_ready))

# filter the data and fit a linear model
Genstat_ready %>% 
  filter(Time == "40DAS") %>% 
  lm(`Hauls/plant`~ Fert, data = .)

modHauls<- lm(`Hauls/plant`~ Fert*Time, data = Genstat_ready) 
summary(modHauls)
print(modHauls)


#charmaine
#overall
data<-Regretion_for_yield_and_rates
mod<-lm(`Yield (t/ha)`~ poly(Nitrogen,2) , data = data)
mod1<-lm(`Yield (t/ha)`~ Nitrogen,data = data)
summary(mod)
print(mod)
AIC(mod)

y22<- data %>%
  filter(Year == "2022")
y23<- data %>%
  filter(Year == "2023")
y24<- data %>%
  filter(Year == "2024")

#outliers



mod22<-lm(`Yield (t/ha)`~ poly(Nitrogen,2, raw=TRUE) , data = y22)
mod22b<-lm(`Relative Yield (%)`~ Nitrogen,data = y22)
summary(mod22)
print(mod22)
AIC(mod22)
plot(mod22)

# Extract coefficients from the model
coefficients22 <- coef(mod22)
b0.22 <- coefficients22[1]  # c Intercept (constant term)
b1.22 <- coefficients22[2]  # b Linear term (N)
b2.22 <- coefficients22[3]  # a Quadratic term (N^2)

# Calculate the optimum Nitrogen rate
N_opt22 <- -b1.22 / (2 * b2.22)

# Print the optimum Nitrogen rate
cat("The optimum Nitrogen rate for maximum yield in 2022 is:", N_opt22, "kg/ha\n")

# Create a sequence of Nitrogen rates for prediction
nitrogen_seq <- seq(min(data$Nitrogen), max(data$Nitrogen), length.out = 100)

# Predict yield for the sequence of Nitrogen rates
predicted_yield <- predict(mod22, newdata = data.frame(Nitrogen = nitrogen_seq))

# Plot the original data
plot(data$Nitrogen, data$`Yield (t/ha)`, pch = 19, xlab = "Nitrogen rate (kg/ha)", 
     ylab = "Yield (kg/ha)", cex=0.5,
     main = "Yield vs. Nitrogen Rate")


yr<- c("2022"= 16, "2023"=17, "2024"=18)

# Highlight point
highlight_data <- data.frame(Nitrogen = c(194,170,10),   # X-values to highlight
Yield = c(12.55, 4.55,2.0))

#best
ggplot(data, 
  aes(Nitrogen,`Yield (t/ha)`, colour = Year)) +
  geom_jitter(size=0.2) + 
  theme_classic() + 
  geom_point(data = highlight_data, aes(x = Nitrogen, y = Yield), 
  color = c('black', 'darkgrey', 'red'), 
  size = 4, shape = 18, 
  fill = c('black', 'darkgrey', 'red')) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE) +
  scale_color_manual(values= c('black', 'darkgrey', 'red')) +
  xlab("Nitrogen rate (kg/ha)") + 
  ylab("Yield (kg/ha)") + 
  geom_vline(xintercept = c(194, 170,10), 
  linetype = "dashed", color = c('black', 'darkgrey', 'red'),alpha=0.4,size = 1)+
  annotate("text", x=c(170.5,170.5,170,170.5,178,178.5), 
           y=c(13.5,13,5.7,5.1,3.8,3.3),
           label= c("R²  = 0.84","p= < 0.001",
                    "R² = 0.29","p= < 0.001",
                    "R²  = 0.45","p= < 0.001"),cex=3.5)

# Add the regression curve
lines(nitrogen_seq, predicted_yield, col = "blue", lwd = 2)

# Add a vertical line at the optimum Nitrogen rate
abline(v = N_opt22, col = "red", lty = 2)

# Add a point for the optimum Nitrogen rate on the curve
opt_yield22 <- predict(mod22, newdata = data.frame(Nitrogen = N_opt22))
points(N_opt22, opt_yield, col = "red", pch = 19, cex = 1.5)

# Annotate the optimum point
text(N_opt22, opt_yield22, 
labels = paste("Optimum N =", 
round(N_opt22, 2), "kg/ha"), pos = 1)


mod23<-lm(`Relative Yield (%)`~ poly(Nitrogen,2, raw=TRUE) , data = y23)
mod23b<-lm(`Relative Yield (%)`~ Nitrogen,data = y23)
summary(mod23)
print(mod23)
AIC(mod)

# Extract coefficients from the model
coefficients23 <- coef(mod23)
b0.23 <- coefficients23[1]  # Intercept (constant term)
b1.23 <- coefficients23[2]  # Linear term (N)
b2.23 <- coefficients23[3]  # Quadratic term (N^2)

# Calculate the optimum Nitrogen rate
N_opt23 <- 9.420-b1.23 / (2 * b2.23)

# Print the optimum Nitrogen rate
cat("The optimum Nitrogen rate for maximum yield in 2023 is:", N_opt23, "kg/ha\n")


# Create a sequence of Nitrogen rates for prediction
nitrogen_seq23 <- seq(min(data$Nitrogen), max(data$Nitrogen), length.out = 100)

# Predict yield for the sequence of Nitrogen rates
predicted_yield23 <- predict(mod23, newdata = data.frame(Nitrogen = nitrogen_seq))

# Plot the original data
plot(data$Nitrogen, data$`Yield (t/ha)`, pch = 19, xlab = "Nitrogen rate (kg/ha)", 
     ylab = "Yield (tons/ha)", cex=0.5,
     main = "Yield vs. Nitrogen Rate")

# Add the regression curve
lines(nitrogen_seq, predicted_yield, col = "blue", lwd = 2)

# Add a vertical line at the optimum Nitrogen rate
abline(v = N_opt23, col = "red", lty = 2)

# Add a point for the optimum Nitrogen rate on the curve
opt_yield <- predict(mod23, newdata = data.frame(Nitrogen = N_opt23))
points(N_opt23, opt_yield, col = "red", pch = 19, cex = 1.5)

# Annotate the optimum point
text(N_opt23, opt_yield23, 
     labels = paste("Optimum N =", round(N_opt23, 2), "kg/ha"), pos = 4)


#2024
mod24<-lm(`Yield (t/ha)`~ poly(Nitrogen,2) , data = y24)
mod24b<-lm(`Yield (t/ha)`~ Nitrogen,data = y24)
summary(mod24)
print(mod24)
AIC(mod24)

# Extract coefficients from the model
coefficients24 <- coef(mod24)
b0.24 <- coefficients24[1]  # Intercept (constant term)
b1.24 <- coefficients24[2]  # Linear term (N)
b2.24 <- coefficients24[3]  # Quadratic term (N^2)

# Calculate the optimum Nitrogen rate
N_opt24 <- 9.420-b1.24 / (2 * b2.24)

N_opt24a<- -0.5665/(2*-0.0618)
# Print the optimum Nitrogen rate
cat("The optimum Nitrogen rate for maximum yield in 2024 is:", N_opt24, "kg/ha\n")

scale_linetype_manual(values = c("Spring" = "solid", "Fall" = "dashed"))
linetype = groupY


g<-ggplot(y24, 
  aes(Nitrogen,`Yield (t/ha)`)) +
  geom_jitter(size=3.3, shape=20, color="darkgrey") + 
  theme_classic() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), 
  se=FALSE, size= 1,color="black") +
  geom_point(data = highlight_data, aes(x = Nitrogen, y = Yield), 
  color = c('red'), size = 4.5, shape = 18, fill = c('red')) +
  geom_vline(xintercept = c(132), 
  linetype = "dashed", color = c('red'),alpha=0.7,linewidth = 0.7)+
  scale_color_manual(values=c('red')) +
  xlab("Nitrogen rate (kg/ha)") + 
  ylab("Yield (t/ha)") + 
  annotate("text", x=c(186,187), y=c(2.8,2.72),
  label= c("R² = 0.45","p  = <0.001"),cex=3.8)+
  annotate("text", x= c(67,93,111), y=c(2.75,2.75,2.75),
  label=c("Optimum N =", round(132), "kg/ha"),
  cex=3.8)

g+scale_x_continuous(limits =c(0,230), breaks = c(0,30,60,90,150,210,250))

  scale_x_discrete(limits =c(0,30,60,90,150,210,250)) 
 
 nrate= c(0,30,60,90,150,210)
 
 scale_x_continuous(
   limits = c(0, 250),
   breaks = c(0,30, 60, 90, 150, 210), 
   labels = c("0", "30", "60", "90", "150","210")

highlight_data <- data.frame(Nitrogen = c(132),Yield = c(2.718))
text(N_opt23, opt_yield23, 
     labels = paste("Optimum N =", round(N_opt23, 2), "kg/ha"), pos = 4)

model <- lm(`Yield (t/ha)` ~ log(Nitrogen), data = y24)
summary(model)
sum(is.infinite(y24$Nitrogen))

# Create a sequence of Nitrogen rates for prediction
nitrogen_seq24 <- seq(min(data$Nitrogen), max(data$Nitrogen), length.out = 100)

# Predict yield for the sequence of Nitrogen rates
predicted_yield23 <- predict(mod23, newdata = data.frame(Nitrogen = nitrogen_seq))

# Plot the original data
plot(data$Nitrogen, data$`Yield (t/ha)`, pch = 19, xlab = "Nitrogen rate (kg/ha)", 
     ylab = "Yield (tons/ha)", cex=0.5,
     main = "Yield vs. Nitrogen Rate")


ano<-aov(`Yield (t/ha)`~ Covers*Year, data=data)
summary(ano)
emmeans::add_grouping(data,Covers)
as.factor(data$Covers)
C<- data %>%
  filter(Covers == "C")
S<- data %>%
  filter(Covers == "S")
SV<- data %>%
  filter(Covers == "SV")

ggplot(data, 
       aes(Nitrogen, `Yield (t/ha)`, color= Covers)) +
  geom_jitter(size=1) + 
  theme_classic() + 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se=FALSE) +
  scale_color_manual(values=c('black', 'darkgrey', 'red'))+
  xlab("Nitrogen rate (kg/ha)") + 
  ylab("Yield (t/ha)") + 
  annotate("text", x=c(179,179,179,179,179,179), y=c(13.5,13,5.5,5,3.6,3.1),
           label= c("R²  = 0.84","p= < 0.001",
                    "R² = 0.45","p= < 0.001",
                    "R²  = 0.29","p= < 0.001"),cex=3.5)


#SQUARE ROOT model
mod24a <- nls(`Yield (t/ha)` ~ theta1 * (1 - exp(-theta2 * sqrt(Nitrogen))),
  data = y24,
  start = list(theta1 = max(y24$`Yield (t/ha)` ), theta2 = 0.10))
summary(mod24a)
print(mod22)

# Fitting the model
mod <- nls(`Relative Yield (%)` ~ theta1 * (1 - exp(-theta2 * sqrt(Nitrogen))),
           data = data,
           start = list(theta1 = max(y22$`Relative Yield (%)`), theta2 = 0.10))

# Predicted values from the model
predicted_values <- predict(mod24a)

# Observed values
observed_values <- data$`Yield (t/ha)`

# Residual Sum of Squares (RSS)
RSS <- sum((observed_values - predicted_values)^2)

# Total Sum of Squares (TSS)
TSS <- sum((observed_values - mean(observed_values))^2)

# R-squared calculation
R_squared <- 1 - (RSS / TSS)

# Output R-squared
R_squared


# Create new data for nitrogen to predict the fitted curve
nitrogen_values <- data.frame(Nitrogen = seq(min(y22$Nitrogen), 
                            max(y22$Nitrogen), length.out = 100))

# Generate predicted yield values using the fitted model
predicted_values <- predict(mod22, newdata = nitrogen_values)

# Create a data frame with predicted values for plotting
pred_df <- data.frame(Nitrogen = nitrogen_values$Nitrogen,
                      `Relative Yield (%)` = predicted_values)

# Plot the data and the fitted model
ggplot(data, aes(x = Nitrogen, y =`Relative Yield (%)`)) +
  geom_point() +  # Original data points
  geom_line(data = pred_df, aes(x = Nitrogen, y = `Relative Yield (%)`), 
  color = "red", size = 1) +  # Fitted curve
  theme_minimal() +
  labs(x = "Nitrogen Rate", y = "Relative Yield (%)") +
  theme(plot.title = element_text(hjust = 0.5))
                                              

mod22b<-lm(`Yield (t/ha)`~ Nitrogen,data = y22)
summary(mod22)
print(mod)
AIC(mod)

mod23<-lm(`Yield (t/ha)`~ poly(Nitrogen,2) , data = y23)
mod23b<-lm(`Yield (t/ha)`~ Nitrogen,data = y23)
summary(mod23)
print(mod23)
AIC(mod)

mod24<-lm(`Yield (t/ha)`~ poly(Nitrogen,2) , data = y24)
mod24b<-lm(`Yield (t/ha)`~ Nitrogen,data = y24)
summary(mod24)
print(mod24)
AIC(mod)


# Sample data
data <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  y = c(2, 3, 5, 7, 8, 9, 9.5, 10, 10, 10))

# Fit the square root plateau model
model <- nls(`Relative Yield (%)`~ theta1 * (1 - exp(-theta2 * sqrt(Nitrogen))),
             data = data,
             start = list(theta1 = max(data$`Relative Yield (%)`, theta2 = 0.1))

# Summary of the model fit
summary(model)

# Predictions from the model
x_new <- seq(min(data$Nitrogen), max(data$Nitrogen), length.out = 100)
predictions <- predict(model, newdata = list(Nitrogen = x_new))

# Plot
plot(data$Nitrogen, data$`Relative Yield (%)`,
     xlab = "Predictor (x)", ylab = "Response (y)", pch = 19)
lines(x_new, predictions, col = "blue")



#####Another way of plotting 
sp <- ggscatter(mtcars, x = "wt", y = "mpg",
                add = "reg.line",               # Add regression line
                conf.int = TRUE,                # Add confidence interval
                color = "cyl", palette = "jco", # Color by groups "cyl"
                shape = "cyl"                   # Change point shape by groups "cyl"
)+
  stat_cor(aes(color = cyl), label.x = 3)



#for ggplot plots library(agricolae)

ggarrange(a, b, c, d, e,
          labels = c("Clay type 1", "Clay type 2", 
          "Clay type 3", "Clay type 4", "Clay type5"),cex=5,
          ncol = 2, nrow = 3, font.label = list(size = 5, color = "black"))

ggdraw()+
  draw_plot(a, x = 0.3, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(b, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(c, x = 0, y = 0, width = 1, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C"), 
  size = 15, x = c(0,0.5,0), y = c(1, 1,0.5))


######

modCL1<-lm(`Non-ex K_extracted_mg/kg` ~ poly(`K added`,3, raw = TRUE), 
           data = Pottrial_new_stats_final_1_Feb_2024) 
summary(modCL1)
print(modCL1)
AIC(modCL1)
BIC(modCL1)


modCL1<-ggplot(Pottrial_new_stats_final_1_Feb_2024,
  aes(`K added`,`Non-ex K_extracted_mg/kg`)) +
  geom_point() + theme_classic() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  xlab("Potassium added (mg K/kg soil)") + 
  ylab("Non-exchangable potassium extracted (mg/kg)") +
  annotate("text", x=c(150,150,150), y=c(26, 24, 22),
  label= c("y = 23.23 - 0.3925x + 0.0021x? - 0.000004x\u00B3",
  "Adj.R? = 0.9329","p= < 0.001"),cex=5)   

a<-modCL1 + theme(text = element_text(size = 10))


ggplot(Pottrial_new_stats_final_1_Feb_2024, 
  aes(`K added`, `Non-ex K_extracted_mg/kg`, color= group)) +
  geom_jitter(size=2) + theme_classic() + 
  geom_smooth(method = "loess", fill=NA)+
  scale_color_manual(values=c('black', 'darkgrey', 'red','orange', 'blue'))+
  xlab("Potassium added (mg K/kg soil)") + 
  ylab("Non-exchangable potassium extracted (mg/kg)") + 
  annotate("text", x=c(1,1,-12,-12,-12,-12,26,26,5,5), 
           y=c(8,6,20,18,28,26,40,38,51,49),
  label= c("Adj.R? = 0.7271","p= < 0.001",
           "Adj.R? = 0.9329","p= < 0.001",
           "Adj.R? = 0.8509","p= < 0.001",
           "Adj.R? = 0.8950","p= < 0.001",
           "Adj.R? = 0.8523","p= < 0.001"),cex=2.6) 
 


# Fit separate regression models for each group
models <- data %>%
  group_by(`Clay type`) %>%
  do(model = lm(`mg/kg`~ `K added(mg K/kg soil), data = data))
     
summary(models)
     
     
     

# Example: Subset data for females aged 30 and above
subset_df <- subset(df, Gender == "Female" & Age >= 30)

# Using dplyr:
subset_df <- df %>%
  filter(Gender == "Female" & Age >= 30)


####Post hoc for GLM
library(lme4)
library(emmeans)
library(multcompView)

# Fit the GLM
glm_model <- glm(BW ~ Treatment*`Production activity`,
                 family = poisson,
                   data = Copy_of_kraal_trial_cow_calf_performance_Final_2024_ammended)

# Perform pairwise comparisons
pairwise_comparisons <- emmeans(glm_model, pairwise ~ Treatment*`Production activity`)

# Generate the compact letter display
posthoc_letters <- multcomp::cld(pairwise_comparisons$emmeans, Letters = letters)

# Print the results
print(posthoc_letters)





##############Correlation######################
library(ltm)

data<-Temperament_scores_cortisol_levels_2020_2021_Final1
biserial.cor(data$Grazing, data$Vocalisation)

cor(data$Vocalisation, data$Walking, method = "pearson")


######COMPARING SLOPES FROM DIFFERENT REGRESSION TESTS######
