library(ggplot2)
library(ggradar)
library(dplyr)
library(data.table)


enzo <- read.csv("enzolefee.csv")
chosen <- c("npxG + xA",
            "Shot-Creating Actions",
            "Pressures",
            "Tackles",
            "Carries into Penalty Area",
            "Progressive Carries",
            "Key Passes")

enzo <- enzo %>% 
  filter(Statistic %in% chosen) %>% 
  transpose()
colnames <- enzo[1,]
enzo <- as.data.frame(lapply(enzo[-1,], as.numeric))
colnames(enzo) <- colnames

enzo <- cbind(rep("Enzo Le Fée", nrow(enzo)), enzo)


ggradar(plot.data=enzo[2,],
        #plot.title="Percentile comparison to all midfielders in top 5 European leagues",
        base.size=10,
        axis.labels = paste(colnames(enzo)[2:8],
                            " (", as.character(enzo[2,2:8]), "th)",
                            sep=""),
        values.radar = c(0, 50, 100),
        grid.min=0,
        grid.mid=50,
        grid.max=100,
        group.colours = "orange",
        fill = TRUE,
        fill.alpha = 0.25,
        axis.label.size=5,
        background.circle.transparency = 0.1,
        plot.extent.x.sf = 2.22) +
  labs(caption= "") +
  theme(panel.background = element_rect(fill = "transparent", colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = NA)
        ) 
  
  ggsave(filename = "CANVAenzo.png", plot=last_plot(), bg='transparent', width=10.5, height=6)
  