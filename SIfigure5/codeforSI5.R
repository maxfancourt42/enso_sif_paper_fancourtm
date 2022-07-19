# run once to install required libraries #####
required_libaries <- c("tidyverse","rstudioapi","lubridate")
install.packages(required_libaries)

# run each time ####
# inport libaries
library("tidyverse")
library("rstudioapi")
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
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

# read in csv
data <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"SI5Data.csv", sep="/"))
data$Date <- as.Date(data$date, format="%d/%m/%Y")
data$type <- as.factor(data$type)

ggplot(data, aes(x = Date, y = value)) +
  geom_vline(aes(xintercept = 16723), col = "#000000", linetype = "dashed",size = 1) +
  geom_vline(aes(xintercept = 16784), col = "#000000", linetype = "dashed",size = 1) +
  geom_line(aes(color = type),size = 1.5)+
  scale_color_manual(values = c("#e41a1c", "#377eb8"))+
  scale_linetype_manual(name = "Linetype", values = c("no correction" = 2, "with correction" = 1))+
  theme_Publication() +
  theme(axis.text=element_text(size=25),axis.title=element_text(size=26,face="bold"), axis.text.x = element_text(angle = 0)) +
  labs(y="SIF (mW m-² sr???¹ nm???¹)", x = "Year") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year")+ 
  coord_cartesian(ylim=c(0.5,1.75)) + 
  theme(legend.position="none")
