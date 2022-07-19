# run once to install required libraries #####
required_libaries <- c("tidyverse","rstudioapi","scales","lubridate")
install.packages(required_libaries)

# run each time ####
# inport libaries
library("tidyverse")
library("rstudioapi")
library("scales")
library("lubridate")

# custom ggplot themes
theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "none",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}


# import data
# Load data ------------------------------------------------------
data <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"sensitivity_by_region.csv", sep="/"))
data$Region <- factor(data$Region, levels = c("NW","GS","EC","BS","SW"))

# Calculate PPT sensitivity 
PPTnegativeAnomalies <- data %>% 
                          select(c(ppt2015_mean, SIF2015_mean, Region)) %>% 
                          filter(ppt2015_mean <= 0)

PPTnegativeAnomalies <- data.frame(PPTnegativeAnomalies)
PPTnegativeAnomalies$PPTSensitivity <- PPTnegativeAnomalies$SIF2015_mean / PPTnegativeAnomalies$ppt2015_mean
PPTnegativeAnomalies$LoggedPPTSensitivity <- if_else(PPTnegativeAnomalies$PPTSensitivity >= 0, log(PPTnegativeAnomalies$PPTSensitivity + 1), log((PPTnegativeAnomalies$PPTSensitivity - 1)*-1)*-1)

ggplot(data=PPTnegativeAnomalies, aes(y=LoggedPPTSensitivity, x=Region, fill=Region)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(y= "SIF Sensitivity to PPT (Logged)", x = "Region") +
  ylim(-2,4) +
  theme_Publication()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22,face="bold"),legend.position = "none")


# Calculate Temp sensitivity 
TempnegativeAnomalies <- data %>% 
  select(c(temp2015_mean, SIF2015_mean, Region)) %>% 
  filter(temp2015_mean >= 0)

TempnegativeAnomalies <- data.frame(TempnegativeAnomalies)
TempnegativeAnomalies$TempSensitivity <- (TempnegativeAnomalies$SIF2015_mean / TempnegativeAnomalies$temp2015_mean)*-1
TempnegativeAnomalies$LoggedTempSensitivity <- if_else(TempnegativeAnomalies$TempSensitivity >= 0, log(TempnegativeAnomalies$TempSensitivity + 1), log((TempnegativeAnomalies$TempSensitivity - 1)*-1)*-1)

ggplot(data=TempnegativeAnomalies, aes(y=LoggedTempSensitivity, x=Region, fill=Region)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +
  labs(y= "SIF Sensitivity to Temp (Logged)", x = "Region") +
  ylim(-2,4) +
  theme_Publication()+
  theme(axis.text=element_text(size=20),axis.title=element_text(size=22,face="bold"),legend.position = "none")





