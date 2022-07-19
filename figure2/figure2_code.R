# run once to install required libraries #####
required_libaries <- c("tidyverse","rstudioapi","RColorBrewer")
install.packages(required_libaries)
remotes::install_github("")
# run each time ####
# inport libaries
library("tidyverse")
library("rstudioapi")
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

# code to reproduce figure 2 #####
# import data
data <- read.csv(paste(dirname(rstudioapi::getSourceEditorContext()$path),"model_bootstrapping_results.csv", sep="/"))
rownames(data) <- data[,1]
data <- data[ , !(names(data) %in% c("X.1"))]

myColors <- brewer.pal(4,"RdYlBu")
names(myColors) <- levels(data$Group)

# Create the graph
ggplot(data, aes(x=X, y=sel_est, group=X)) + 
  geom_segment(size=5, alpha = 0.5, aes(x=X,xend=X, y=boot_025per,yend=boot_975per, colour=Group), guide = FALSE) +
  scale_color_manual(values=myColors, guide = FALSE) +
  geom_pointrange(aes(ymin=sel_est-sel_se, ymax=sel_est+sel_se), size=0.75, width=0.75) +
  theme_Publication() +
  scale_y_continuous(name="Scaled Regression Coefficient", limits = c(-0.5, 0.5), breaks=c(seq(-0.5,0.5,0.1))) +
  scale_x_continuous("", labels = function(x) str_wrap(as.character(row.names(data)), width = 15), breaks = data$X) + 
  geom_hline(yintercept=0, linetype="dashed") +
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        axis.text.y = element_text(angle=90, hjust=0.5))

