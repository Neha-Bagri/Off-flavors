#######Library##############
getwd()
library(readxl)
library(readr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(purrr)
library(Rmisc)
library(dplyr)

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

###220
Off_flavour_220 <- Off_flavour_Data %>% filter(Dosage == 220)
ggplot(Off_flavour_220,aes(Enzyme,Intensity,colour=Fat))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  facet_grid(cols = vars(Fermentation))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  ylim(-0.1,4.01) +  theme_bw()

###660
Off_flavour_660 <- Off_flavour_Data %>% filter(Dosage == 660)
ggplot(Off_flavour_660,aes(Enzyme,Intensity,colour=Fat))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  facet_grid(cols = vars(Fermentation))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  ylim(-0.1,4.01) +  theme_bw()

######Data Visualization (2)#######

###BUTTER
Off_flavour_Butter <- Off_flavour_Data %>% filter(Fat == "Butter")
ggplot(Off_flavour_Butter,aes(Enzyme,Intensity,colour=Dosage))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  scale_colour_manual(values=c("#F5AAB0","#D36069"))+
  labs(colour = "Dosage (ALU/kg)")+
  facet_grid(cols = vars(Fermentation))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  ylim(-0.1,4.01) +  theme_bw()

###Palm
Off_flavour_Palm <- Off_flavour_Data %>% filter(Fat == "Palm")
ggplot(Off_flavour_Palm,aes(Enzyme,Intensity,colour=Dosage))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  scale_colour_manual(values=c("#7BB2C4","#13586F"))+
  labs(colour = "Dosage (ALU/kg)")+
  facet_grid(cols = vars(Fermentation))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  ylim(-0.1,4.01) +  theme_bw()


###Sunflower
Off_flavour_Sunflower <- Off_flavour_Data %>% filter(Fat == "Sunflower")
ggplot(Off_flavour_Sunflower,aes(Enzyme,Intensity,colour=Dosage))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  scale_colour_manual(values=c("#E190E4","#AE18B3"))+
  labs(colour = "Dosage (ALU/kg)")+
  facet_grid(cols = vars(Fermentation))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  ylim(-0.1,4.01) +  theme_bw()

###Coconut
Off_flavour_Coconut <- Off_flavour_Data %>% filter(Fat == "Coconut")
ggplot(Off_flavour_Coconut,aes(Enzyme,Intensity,colour=Dosage))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  scale_colour_manual(values=c("#A6D26D","#45710D"))+
  labs(colour = "Dosage (ALU/kg)")+
  facet_grid(cols = vars(Fermentation))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  ylim(-0.1,4.01) +  theme_bw()



######ANOVA & T.test b/w Rounds of Panelists############

Off_flavour_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Palm_panel_220")
Off_flavour_panel$Enzyme <- as.factor(Off_flavour_panel$Enzyme)
Off_flavour_panel$Round <- as.factor(Off_flavour_panel$Round)

compare_means(Intensity~Round,
              data=Off_flavour_panel,
              method = "anova")

compare_means(Intensity~Round,
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

######Data Visualization (3)#######

#Butter_panel
butter_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Butter_panel_220")

#strings
butter_panel$Enzyme <- as.factor(butter_panel$Enzyme)
butter_panel$Fermentation <- as.factor(butter_panel$Fermentation)
butter_panel$Intensity <- as.numeric(butter_panel$Intensity)
butter_panel$Panelist <- as.factor(butter_panel$Panelist)

ggplot(butter_panel,aes(Enzyme,Intensity,colour=Fermentation))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4, shape = 18)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  scale_colour_manual(values=c("#F5AAB0","#D36069"))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  labs(colour = "Fermentation at dosage 220 ALU/kg")+
  ylim(-0.1,4.01) +  theme_bw()


#Palm_panel
Palm_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Palm_panel_220")

#strings
Palm_panel$Enzyme <- as.factor(Palm_panel$Enzyme)
Palm_panel$Fermentation <- as.factor(Palm_panel$Fermentation)
Palm_panel$Intensity <- as.numeric(Palm_panel$Intensity)
Palm_panel$Panelist <- as.factor(Palm_panel$Panelist)

ggplot(Palm_panel,aes(Enzyme,Intensity,colour=Fermentation))+
  geom_point(stat="summary",fun="mean",position=position_dodge(w=0.2), size = 4, shape = 18)+
  stat_summary(geom="errorbar",fun.data = mean_se,position=position_dodge(width=0.2),width=0.5)+
  scale_colour_manual(values=c("#7BB2C4","#13586F"))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))+
  labs(colour = "Fermentation at dosage 220 ALU/kg")+
  ylim(-0.1,4.01) +  theme_bw()

