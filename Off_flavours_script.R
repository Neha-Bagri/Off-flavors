#######Library##############
getwd()
library(readxl)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(purrr)

######Import Data frame (Excel)#######
Off_flavour_Data <- read_excel("Off_flavour_Data.xlsx")

#strings
Off_flavour_Data$Enzyme <- as.factor(Off_flavour_Data$Enzyme)
Off_flavour_Data$Fat <- as.factor(Off_flavour_Data$Fat)
Off_flavour_Data$Fermentation <- as.factor(Off_flavour_Data$Fermentation)
Off_flavour_Data$Intensity_mean <- as.numeric(Off_flavour_Data$Intensity_mean)
Off_flavour_Data$Dosage <- as.factor(Off_flavour_Data$Dosage)
Off_flavour_Data$Panelist <- as.factor(Off_flavour_Data$Panelist)

######Data Visualization (1)#######

###660
Off_flavour_660 <- Off_flavour_Data %>% filter(Dosage == 660) 
ggline(Off_flavour_660, x="Enzyme", y="Intensity",
       add = "mean_se",
       color ="Fat",
       point.size = 2,
       facet.by ="Fermentation",
       plot_type = "p",
       legend = "right",
       position = position_dodge(w=0.15))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))
       


###220
Off_flavour_220 <- Off_flavour_Data %>% filter(Dosage == 220)
ggline(Off_flavour_220, x="Enzyme", y="Intensity",
       add = "mean_se",
       color ="Fat",
       point.size = 2,
       facet.by ="Fermentation",
       plot_type = "p",
       legend = "right",
       position = position_dodge(w=0.15))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))


######Data Visualization (2)#######

###BUTTER
Off_flavour_Butter <- Off_flavour_Data %>% filter(Fat == "Butter")
ggline(Off_flavour_Butter, x="Enzyme", y="Intensity",
       add = "mean_se",
       color ="Dosage",palette = c("#F5AAB0","#D36069"),
       point.size = 3,
       shape = 18,
       facet.by ="Fermentation",
       plot_type = "p",
       legend = "right",
       position = position_dodge(w=0.15))+
  labs(colour = "Dosage (ALU/kg)")+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))

###Palm
Off_flavour_Palm <- Off_flavour_Data %>% filter(Fat == "Palm")
ggline(Off_flavour_Palm, x="Enzyme", y="Intensity",
       add = "mean_se",
       color ="Dosage",palette = c("#7BB2C4","#13586F"),
       point.size = 3,
       facet.by ="Fermentation",
       plot_type = "p",
       legend = "right",
       position = position_dodge(w=0.15))+
  labs(colour = "Dosage (ALU/kg)")+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))

###Sunflower
Off_flavour_Sunflower <- Off_flavour_Data %>% filter(Fat == "Sunflower")
ggline(Off_flavour_Sunflower, x="Enzyme", y="Intensity",
       add = "mean_se",
       color ="Dosage",palette = c("#E190E4","#AE18B3"),
       point.size = 3,
       shape = 18,
       facet.by ="Fermentation",
       plot_type = "p",
       legend = "right",
       position = position_dodge(w=0.15))+
  labs(colour = "Dosage (ALU/kg)")+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))

###Coconut
Off_flavour_Coconut <- Off_flavour_Data %>% filter(Fat == "Coconut")
ggline(Off_flavour_Coconut, x="Enzyme", y="Intensity",
       add = "mean_se",
       color ="Dosage",palette = c("#A6D26D","#45710D"),
       point.size = 3,
       shape = 18,
       facet.by ="Fermentation",
       plot_type = "p",
       legend = "right",
       position = position_dodge(w=0.15))+
  labs(colour = "Dosage (ALU/kg)")+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))



######ANOVA & T.test b/w Rounds of Panelists############

Off_flavour_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Butter_panel_df")
Off_flavour_panel$Enzyme.2 <- as.factor(Off_flavour_panel$Enzyme.2)
Off_flavour_panel$Round <- as.factor(Off_flavour_panel$Round)

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "anova")

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "t.test")

#Interpretation: There was no significant difference between average observations of the panelists in different rounds (p=0.52, ANOVA).
#Interpretation: The average observations of panelists showed no significant difference between round1 and round2 (p= 0.33), round1 and round3 (p= 0.82), and round2 and round3 (p= 0.43).

############Anova butter b/w rounds of panel df###################


Off_flavour_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Butter_panel_of")
Off_flavour_panel$Enzyme.2 <- as.factor(Off_flavour_panel$Enzyme.2)
Off_flavour_panel$Round <- as.factor(Off_flavour_panel$Round)

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "anova")

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "t.test")

##########3Anova palm b/w rounds of panel df##############
Off_flavour_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Palm_panel_df")
Off_flavour_panel$Enzyme.2 <- as.factor(Off_flavour_panel$Enzyme.2)
Off_flavour_panel$Round <- as.factor(Off_flavour_panel$Round)

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "anova")

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "t.test")

##########3Anova palm b/w rounds of panel 0f##############
Off_flavour_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Palm_panel_of")
Off_flavour_panel$Enzyme.2 <- as.factor(Off_flavour_panel$Enzyme.2)
Off_flavour_panel$Round <- as.factor(Off_flavour_panel$Round)

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "anova")

compare_means(Intensity.avg~Round,
              data=Off_flavour_panel,
              method = "t.test")

