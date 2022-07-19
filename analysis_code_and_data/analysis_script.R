# run once to install required libraries #####
required_libaries <- c("tidyverse","rstudioapi","lubridate","RColorBrewer")
install.packages(required_libaries)

# run each time ####
# inport libaries
library("tidyverse")
library("rstudioapi")
library("lubridate")
library("RColorBrewer")

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

# Load data ------------------------------------------------------
# Path to csv export from earth engine
data <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"PublishedScriptData.csv", sep="/"))


# calculate the linear and quadratic terms for temp and add back to the dataframe
tempLinearTerm <-poly(data$temp2015_mean, 2, raw=F)[,1]
tempQuadraticTerm <-poly(data$temp2015_mean, 2, raw=F)[,2]

data$TempLinear <- tempLinearTerm
data$TempQuad <- tempQuadraticTerm

data$MAPCoV <- data$MAPVariance/data$MAP
data$MATMaxCoV <- data$MatMaxVar/data$MATMax

# Define the full model formula
FullModelFormula <- formula(scale(SIF2015_mean) ~ 
                              scale(ppt2015_mean) + 
                              scale(TempLinear) + 
                              scale(TempQuad) + 
                              scale(mcwd2015) +
                              scale(MAP) +
                              scale(MAPCoV) +
                              scale(PPTSeasonality) +
                              scale(MATMax) +
                              scale(MATMaxCoV) +
                              scale(IntraVarMaxTemp) +
                              scale(CEC) +
                              scale(Sand) +
                              scale(WaterTableDepth)
)



# Estimate full model ----------------------------------------------
full_mod <- lm(FullModelFormula, data = data)
summary(full_mod)

# Stepwise variable elimatination ---------------------------------------------------
sel_mod <- step(lm(FullModelFormula, data = data), 
                direction = "backward",
                scope = list(upper = FullModelFormula, 
                             lower = formula(scale(SIF2015_mean)~scale(ppt2015_mean) + scale(TempQuad))),
                trace = 0)

summary(sel_mod)
