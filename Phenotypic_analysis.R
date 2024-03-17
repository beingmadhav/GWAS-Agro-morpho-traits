#Required package installation
install.packages("agricolae")
install.packages("lmerTest") 
install.packages("lme4")
install.packages("lsmeans")
install.packages("emmeans") 
install.packages("ggplot2")
install.packages("psych") 
install.packages("car")
install.packages("gvlma")
install.packages("reshape2")
install.packages("dplyr")

#loading packages for use
library(agricolae)
library(lmerTest) 
library(lme4)
library(lsmeans)
library(emmeans) 
library(ggplot2)
library(psych)
library(car)
library(gvlma)
library(reshape2) 
library(dplyr)
library(multcompView)

########################################################## PHENOTYPIC ANALYSIS ##################################################
#Set working directory
setwd("C:/Users/MadhavSubedi/Agronomic_data")

# Importing data
mydata= read.csv("C:/Users/MadhavSubedi/Agronomic_data/Phenotype/Agronomicdata_alltait.csv") 
head(mydata)

# Structure of the data
str(mydata)
dim(mydata)
mydata$Rep=as.factor(mydata$Rep) #Change variable 'Rep' to factor
mydata$Cultivar=as.factor(mydata$Cultivar) #Change variable 'Cultivar' to factor
mydata$ID=as.factor(mydata$ID) #Change ID # to factor
table(mydata$ID, mydata$Rep) #to cross check the data structure and mis-tabulation
table(mydata$Cultivar,mydata$Rep)


# Descriptive statistics of the data
mean= aggregate(x=mydata[4:70],        # specify data column whose mean is being computed
                by= list(mydata$Cultivar) , 
FUN="mean") #specify function (i.e. mean)
dstat= describe(mean)   
head(dstat)
write.csv(dstat, file='Descriptive_Staistics_agromorphotraits.csv') 


#Subsetting heading data
Heading_data <- mydata[, c("Cultivar","ID","Rep","HD_PL23","HD_GRF23","HD_PL22", "HD_GRF22", "HD_PL21", "HD_GRF21" )] #For All-Combined dataset
Heading_data_griffin <- mydata[, c("Cultivar","ID","Rep", "HD_GRF23", "HD_GRF22", "HD_GRF21" )] #For Griffin-Combined dataset
Heading_data_plains <- mydata[, c("Cultivar","ID","Rep", "HD_PL23", "HD_PL22", "HD_PL21" )] #For Plains-Combined dataset


#Melt the data frame from wide to long format for analysis
Heading_melt_data <- reshape2::melt(Heading_data, varnames = c("Cultivar", "Rep")) 
Heading_melt_plains <- reshape2::melt(Heading_data_plains, varnames = c("Cultivar", "Rep"))
Heading_melt_griffin <- reshape2::melt(Heading_data_griffin, varnames = c("Cultivar", "Rep"))


# Simple plot for checking data. If there are outliers, they can be identified through the plot. 

plot(Heading_data$HD_PL23, col=mydata$Rep,xlab = "Lines", ylab = "Heading date (DOS) for PL23 ", ylim=c(120, 150))
plot(Heading_data$HD_GRF23, col=mydata$Rep,xlab = "Lines", ylab = "Heading date (DOS) for PL23 ", ylim=c(135, 160))
plot(Heading_data$HD_PL22, col=mydata$Rep,xlab = "Lines", ylab = "Heading date (DOS) for PL22 ", ylim=c(120, 150))
plot(Heading_data$HD_GRF22,col=mydata$Rep,xlab = "Lines", ylab = "Heading date (DOS) for GRF22",ylim=c(155, 180))
plot(Heading_data$HD_PL21,col=mydata$Rep,xlab = "Lines", ylab = "Heading date (DOS) for PL21",ylim=c(120, 145))            
plot(Heading_data$HD_GRF21,col=mydata$Rep,xlab = "Lines", ylab = "Heading date (DOS) for GRF21",ylim=c(140, 165))          


# Violin Plot

# Set color palette
my_palette <- c("skyblue", "orange", "skyblue", "orange","skyblue", "orange")
 # set color for the graph, here two repeated color represent two different field locations totalling to six environments

#Plotting the heading date data.This displays the heading date for all six individual environments
ggplot(Heading_graph_melt, aes(x = variable, y = value, fill = factor(variable))) +
  geom_violin(trim = FALSE, adjust = 2, alpha = 0.5) +
  geom_boxplot(width = 0.2, color = "black", outlier.color = "black", fill = "white") +
  
  # Set fill colors using the palette
  scale_fill_manual(values = my_palette) +
  
  # Set the title, axis labels, and limits
  labs(title = NULL,
       x = NULL, y = "HD (DAS)",
       fill = "Environment") +
  ylim(120, 180) +
  
  # Customize the theme
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle =45, hjust=1, vjust = 1, size = 16, color="white" ),
    axis.text.y = element_text(size = 16,hjust = 1, vjust =2, color="black"),
    legend.position = "NULL",
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 16, face="bold"),
    panel.border = element_rect(color = "black", fill = NA)))


#levene's test for checking homogeneity of variance
leveneTest(value~variable,Heading_melt_plains,center="mean")$`Pr(>F)` 
 #testing if the each enviornmnet data in plains have equal variance or not
leveneTest(value~variable,Heading_melt_griffin,center="mean")$`Pr(>F)`
 #testing if the each enviornmnet data in Griffin have equal variance or not


# Normality testing
shapiro.test(Heading_data$HD_PL23) #individual environment
shapiro.test(Heading_data$HD_PL22)
shapiro.test(Heading_data$HD_PL21)
shapiro.test(Heading_data$HD_GRF23)
shapiro.test(Heading_data$HD_GRF22)
shapiro.test(Heading_data$HD_GRF21)


################################################################# Mixed model  #####################################################
# Combined environment for GXE interaction effect 
# All-Combined data
model2_heading=lmer(Heading_melt_data$value~(1|ID)+(1|Rep)+(1|variable)+(1|ID:variable), data=Heading_melt_data) 
#Here variable is the enviornments
summary(model2_heading)
ranova(model2_heading)$Pr

# Plains-Combine data
model2_heading_plains=lmer(Heading_melt_plains$value~(1|ID)+(1|Rep)+(1|variable)+(1|ID:variable),data=Heading_melt_plains)
summary(model2_heading_plains)
ranova(model2_heading_plains)$Pr

# Griffin-Combined data
model2_heading_grf=lmer(Heading_melt_griffin$value~(1|ID)+(1|Rep)+(1|variable)+(1|ID:variable),data=Heading_melt_griffin)
summary(model2_heading_grf)
ranova(model2_heading_grf)$Pr

#Single environments
# HD_PL23 (Heading date Plains 2023)
model2_HD_PL23=lmer(mydata$HD_PL23~(1|ID)+(1|Rep), data=mydata) 
hist(residuals(model2_HD_PL23), col="light Blue") 
plot(model2_HD_PL23)              #check residuals for any problematic data i.e diagnostic plot
hist(residuals(model2_HD_PL23))   #should look like normal distribution
summary(model2_HD_PL23)  #shows output for coefficients of random effects, ID which is the BLUP of individual ID
ranova(model2_HD_PL23)$Pr[2]   #for random effect

# HD_GRF23 
model2_HD_GRF23=lmer(mydata$HD_GRF23~(1|ID)+(1|Rep), data=mydata) 
hist(residuals(model2_HD_GRF23), col="light Blue") #looks normally distribution for HD_PL22
plot(model2_HD_GRF23)              
boxplot(residuals(model2_HD_GRF23))
summary(model2_HD_GRF23)
ranova(model2_HD_GRF23)$Pr[2]      

# HD_PL22 
model2_HD_PL22=lmer(mydata$HD_PL22~(1|ID)+(1|Rep), data=mydata) #removed rep
hist(residuals(model2_HD_PL22), col="light Blue") #looks normally distribution for HD_PL22
plot(model2_HD_PL22)            
summary(model2_HD_PL22)
ranova(model2_HD_PL22)$Pr[2]   
 
 #Similarly, other individual environments can be modeled

######################################################## BLUP estimation ####################################################

# All-Combined data 
fixef(model2_heading)
ranef(model2_heading)$ID 
# this is the intercept which is basically applied to each lines. when this is added to ranef we get normal blup estimates. 

BLUP_normal_heading= fixef(model2_heading)+ranef(model2_heading)$ID  
#Extracting BLUP values and converting to normal scale. Adding intercept value of fixed effect to correct BLUP to normal scale

tail(BLUP_normal_HD_PL23)
write.csv(BLUP_normal_heading, file="BLUP_Heading_Allcomb.csv") 
hist(BLUP_normal_heading$"(Intercept)", col="light Blue", labels=T) # Histogram of BLUP


# Griffin-Combined data
fixef(model2_heading_grf)
ranef(model2_heading_grf)$ID
BLUP_normal_heading_grf= fixef(model2_heading_grf)+ranef(model2_heading_grf)$ID
tail(BLUP_normal_heading_grf)
write.csv(BLUP_normal_heading_grf, file="BLUP_Heading_GRFcomb.csv") 
hist(BLUP_normal_heading_grf$"(Intercept)", col="light Blue", labels=T) # Histogram of BLUP


# Plains-Combined data
fixef(model2_heading_plains)
ranef(model2_heading_plains)$ID
BLUP_normal_heading_plains= fixef(model2_heading_plains)+ranef(model2_heading_plains)$ID
tail(BLUP_normal_heading_plains)
write.csv(BLUP_normal_heading_plains, file="BLUP_Heading_PLcomb.csv") 
hist(BLUP_normal_heading_plains$"(Intercept)", col="light Blue", labels=T) # Histogram of BLUP


# HD_PL23
BLUP_HD_PL23=ranef(model2_HD_PL23)$ID
head(BLUP_HD_PL23)
fixef(model2_HD_PL23) 
BLUP_normal_HD_PL23=fixef(model2_HD_PL23)+BLUP_HD_PL23 
write.csv(BLUP_normal_HD_PL23, file="BLUP_HD_PL23.csv") 
hist(BLUP_normal_HD_PL23$"(Intercept)", col="light Blue", labels=T) # Histogram of BLUP

#Similarly BLUPs can be calculated for other environments and traits
