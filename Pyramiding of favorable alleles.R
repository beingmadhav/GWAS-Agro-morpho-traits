##################################################### Pyramiding effect of Favorable alleles  ####################################

library(agricolae)
library(lme4)
library(lsmeans)
library(lmerTest)
library(emmeans)
library(multcompView)

FLW_data=read.csv("Pyramiding_data_FLW.csv")
#This file contains Taxa in first  column, number of favorable alleles (FAVA_NUM) for Flag leaf width (FLW) for each lines in second column, and BLUP values of FLW in third column

head(FLW_data)
FLW_data$FAVA_NUM_FLW=factor(FLW_data$FAVA_NUM_FLW) #Change number of favourable alleles variable to factor
glimpse(FLW_data)


describeBy(
  FLW_data,
  FLW_data$FAVA_NUM_PHT # grouping variable
)

# ANOVA test for variance by number of favourable alleles
Anova_FLW=aov(BLUP_FLW~FAVA_NUM_FLW, data=FLW_data)   
summary(Anova_FLW)

#TUKEY's test
Tukey_FLW= TukeyHSD(Anova_FLW)
Tukey_FLW


#Plotting the data

ggplot(FLW_DATA) +
  aes(x = FAVA_NUM_FLW, y = BLUP_FLW) +
  geom_point(size=2) +
  geom_smooth(method = "lm", se=T, colour="blue", size=1, linetype="dotted")+
  theme_classic()+
  theme(axis.text.x = element_text(color = "black", size = 12, face="bold"),
        axis.text.y = element_text(color = "black", size = 12, face="bold"),  
        axis.title.x = element_text(color = "black", size = 16, face="bold"),
        axis.title.y = element_text(color = "black", size = 16, face="bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0),
        plot.title = element_text(color = "black", size = 18, face = "bold.italic", hjust = 0.5))+
  xlab("Number of favorable alleles")+ylab("FLW (cm)")+
  stat_cor(label.x = 2.2, label.y = 1.65, aes(label = paste(..rr.label.., ..p.label.., sep = "~~~~")), colour="blue")+
  stat_regline_equation(label.x = 2.2, label.y =1.6, colour="blue" )

