# run once to install required libraries #####
required_libaries <- c("ggplot2", "dplyr","lubridate", "scales","rstudioapi")
install.packages(required_libaries)


# run each time ####
# inport libaries
library("ggplot2")
library("dplyr")
library("lubridate")
library("scales")
library("rstudioapi")

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
            axis.ticks = element_line(size = 1),
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

# code to reproduce figure 1 #####
# import data
data <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"region_time_series_data.csv", sep="/"))

# convert date column to readable date format
data$Date <- as.Date(data$date, format="%d/%m/%Y")
data <- data %>% filter(Date < as.Date("2017-01-01"))

# plot NW
toplot <- data %>% filter(Region == "NW")
ggplot(data=toplot, aes(x=Date, y=sif)) +
  theme_Publication() +
  geom_line(color="#e41a1c", size=1.25) +
  theme(axis.text=element_text(size=25),axis.title=element_text(size=26,face="bold"), axis.text.x = element_text(angle = 0)) +
  labs(y="SIF Anomaly", x = "Year") +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
  geom_vline(aes(xintercept = as.Date("2015-10-01")), col = "#000000", linetype = "dashed") +
  geom_vline(aes(xintercept = as.Date("2015-12-31")), col = "#000000", linetype = "dashed") + 
  coord_cartesian( ylim=c(-3,1.5))
  

# plot GS
toplot <- data %>% filter(Region == "GS")
ggplot(data=toplot, aes(x=Date, y=sif)) +
  theme_Publication() +
  geom_line(color="#377eb8", size=1.25) +
  theme(axis.text=element_text(size=25),axis.title=element_text(size=26,face="bold"), axis.text.x = element_text(angle = 0)) +
  labs(y="SIF Anomaly", x = "Year") +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
  geom_vline(aes(xintercept = 16723), col = "#000000", linetype = "dashed") +
  geom_vline(aes(xintercept = 16784), col = "#000000", linetype = "dashed") +
  coord_cartesian( ylim=c(-3,1.5))

# plot EC
toplot <- data %>% filter(Region == "EC")
ggplot(data=toplot, aes(x=Date, y=sif)) +
  theme_Publication() +
  geom_line(color="#4daf4a", size=1.25) +
  theme(axis.text=element_text(size=25),axis.title=element_text(size=26,face="bold"), axis.text.x = element_text(angle = 0)) +
  labs(y="SIF Anomaly", x = "Year") +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
  geom_vline(aes(xintercept = 16723), col = "#000000", linetype = "dashed") +
  geom_vline(aes(xintercept = 16784), col = "#000000", linetype = "dashed") +
  ylim(-3,1.5)

# plot BS
toplot <- data %>% filter(Region == "BS")
ggplot(data=toplot, aes(x=Date, y=sif)) +
  theme_Publication() +
  geom_line(color="#984ea3", size=1.25) +
  theme(axis.text=element_text(size=25),axis.title=element_text(size=26,face="bold"), axis.text.x = element_text(angle = 0)) +
  labs(y="SIF Anomaly", x = "Year") +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
  geom_vline(aes(xintercept = 16723), col = "#000000", linetype = "dashed") +
  geom_vline(aes(xintercept = 16784), col = "#000000", linetype = "dashed") +
  coord_cartesian( ylim=c(-3,1.5))

# plot SW
toplot <- data %>% filter(Region == "SW")
ggplot(data=toplot, aes(x=Date, y=sif)) +
  theme_Publication() +
  geom_line(color="#ff7f00", size=1.25) +
  theme(axis.text=element_text(size=25),axis.title=element_text(size=26,face="bold"), axis.text.x = element_text(angle = 0)) +
  labs(y="SIF Anomaly", x = "Year") +
  geom_hline(yintercept=0, linetype = "dashed") +
  scale_x_date(date_labels = "%y", date_breaks = "1 year") + 
  geom_vline(aes(xintercept = 16723), col = "#000000", linetype = "dashed") +
  geom_vline(aes(xintercept = 16784), col = "#000000", linetype = "dashed") +
  coord_cartesian( ylim=c(-3,1.5))

