library(dplyr)
library(ggplot2)
df <- data.frame(antibodies = c("GA", "MG", "MGA"),
                 samples = c(12, 1, 28),
                 accuracy = c(10, 1, 28),
                 percentageSamples = c("29.27", "2.44", "68.29"),
                 percentageAccuracy = c("83.33", "100", "100"))
View(df)
chart <- df %>% 
  ggplot(aes(x=reorder(antibodies, -samples), y=samples, fill=percentageAccuracy))+
  labs(subtitle = "Accuracy among sets of antibodies \nagainst COVID-19",
       x = "Antibodies (Ig)",
       y = "Samples")+
  theme_classic()+
  geom_col(alpha = 0.7)+
  theme(legend.background = element_rect(fill="#a3d1b1",
                                         size=0.5, linetype="solid", 
                                         colour ="darkblue"))+
  guides(fill=guide_legend(title="Percentage Accuracy"))
chart
