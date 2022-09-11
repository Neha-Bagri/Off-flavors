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
Off_flavour_Data <- select(Off_flavour_Data,-Intensity_P1,-Intensity_P2,-Intensity_P3,
                           -Quantity_1,-Quantity_2)

#strings
Off_flavour_Data$Enzyme <- as.factor(Off_flavour_Data$Enzyme)
Off_flavour_Data$Fat <- as.factor(Off_flavour_Data$Fat)
Off_flavour_Data$Fermentation <- as.factor(Off_flavour_Data$Fermentation)
Off_flavour_Data$Intensity_mean <- as.numeric(Off_flavour_Data$Intensity_mean)
Off_flavour_Data$Dosage <- as.factor(Off_flavour_Data$Dosage)


######Data Visualization (1)#######

###660
Off_flavour_660 <- Off_flavour_Data %>% filter(Dosage == 660) 
ggplot(Off_flavour_660, aes(x=Enzyme, y=Intensity_mean, colour=Fat))+        
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  ylab("Off-flavour Intensity")+
  facet_grid(cols = vars(Fermentation))+ 
  ggtitle("Off flavour Intensity of different Enzyme at dosage of 660 ALU/kg")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))


###220
Off_flavour_220 <- Off_flavour_Data %>% filter(Dosage == 220)
ggplot(Off_flavour_220, aes(x=Enzyme, y=Intensity_mean, colour=Fat))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  ylab("Off-flavour Intensity")+
  facet_grid(cols = vars(Fermentation))+
  ggtitle("Off flavour Intensity of different Enzyme at dosage of 220 ALU/kg")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))



######Data Visualization (2)#######

###BUTTER
Off_flavour_Butter <- Off_flavour_Data %>% filter(Fat == "Butter")
ggplot(Off_flavour_Butter, aes(x=Enzyme, y=Intensity_mean, colour=Dosage))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  facet_grid(cols = vars(Fermentation))+
  scale_colour_manual(values=c("#F5AAB0","#D36069"))+ 
  ylab("Off-flavour Intensity")+
  ggtitle("Off flavour Intensity of different Enzyme in Butter")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))

###Palm
Off_flavour_Palm <- Off_flavour_Data %>% filter(Fat == "Palm")
ggplot(Off_flavour_Palm, aes(x=Enzyme, y=Intensity_mean, colour=Dosage))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  facet_grid(cols = vars(Fermentation))+
  scale_colour_manual(values=c("#7BB2C4","#13586F"))+ 
  ylab("Off-flavour Intensity")+
  ggtitle("Off flavour Intensity of different Enzyme in Palm")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))


###Sunflower
Off_flavour_Sunflower <- Off_flavour_Data %>% filter(Fat == "Sunflower")
ggplot(Off_flavour_Sunflower, aes(x=Enzyme, y=Intensity_mean, colour=Dosage))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  facet_grid(cols = vars(Fermentation))+
  scale_colour_manual(values=c("#E190E4","#AE18B3"))+
  ylab("Off-flavour Intensity")+
  ggtitle("Off flavour Intensity of different Enzyme in Sunflower")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))


###Coconut
Off_flavour_Coconut <- Off_flavour_Data %>% filter(Fat == "Coconut")
ggplot(Off_flavour_Coconut, aes(x=Enzyme, y=Intensity_mean, colour=Dosage))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  facet_grid(cols = vars(Fermentation))+
  scale_colour_manual(values=c("#A6D26D","#45710D"))+ 
  ylab("Off-flavour Intensity")+
  ggtitle("Off flavour Intensity of different Enzyme in Coconut")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E"))



######ANOVA & T.test b/w Rounds of Panelists############

Off_flavour_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Butter_panel_of")
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


######Data Visualization (3)#######

#Butter_panel
butter_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Off_Intensity_panel")
butter_panel <- butter_panel %>% filter(Fat == "Butter")


#strings
butter_panel$Enzyme <- as.factor(butter_panel$Enzyme)
butter_panel$Fat <- as.factor(butter_panel$Fat)
butter_panel$Fermentation <- as.factor(butter_panel$Fermentation)
butter_panel$Intensity_mean <- as.numeric(butter_panel$Intensity_mean)
butter_panel$Dosage <- as.numeric(butter_panel$Dosage)


ggplot(butter_panel, aes(x=Enzyme, y=Intensity_mean, colour=Fermentation))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  scale_colour_manual(values=c("#F5AAB0","#D36069"))+ 
  ylab("Off-flavour Intensity")+
  ggtitle("Off flavour Intensity of Butter by Trained Panelist (220ALU/kg)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E","Ref-Ref"))


#Palm_panel
palm_panel <- read_excel("Off_flavour_Data.xlsx", sheet = "Off_Intensity_panel")
palm_panel <- palm_panel %>% filter(Fat == "Palm")


#strings
palm_panel$Enzyme <- as.factor(palm_panel$Enzyme)
palm_panel$Fat <- as.factor(palm_panel$Fat)
palm_panel$Fermentation <- as.factor(palm_panel$Fermentation)
palm_panel$Intensity_mean <- as.numeric(palm_panel$Intensity_mean)
palm_panel$Dosage <- as.numeric(palm_panel$Dosage)


ggplot(palm_panel, aes(x=Enzyme, y=Intensity_mean, colour=Fermentation))+
  geom_point(position=position_jitter(h=0.01, w=0.1), size = 4)+
  scale_colour_manual(values=c("#7BB2C4","#13586F"))+ 
  ylab("Off-flavour Intensity")+
  ggtitle("Off flavour Intensity of Palm by Trained Panelist (220ALU/kg)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c("Ref","A","B","C","D","E","Ref-Ref"))
