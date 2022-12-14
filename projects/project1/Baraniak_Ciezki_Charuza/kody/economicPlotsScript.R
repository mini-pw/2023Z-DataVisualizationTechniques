install.packages("ggplot2")
library(ggplot2) 
library(ggrepel)
library(patchwork)
options(scipen=999)

countries_summary$Population <- countries_summary$Population / 1000000
countries_summary$average_GDP <- countries_summary$average_GDP / 1000000000
colnames(countries_summary)[18] <- "PKB"
countries_summary$Industry = as.double(gsub(",", ".", countries_summary$Industry))
countries_summary$Agriculture = as.double(gsub(",", ".", countries_summary$Agriculture))
countries_summary$Service = as.double(gsub(",", ".", countries_summary$Service))
athlete_countries$Industry = as.double(gsub(",", ".", athlete_countries$Industry))
athlete_countries$Agriculture = as.double(gsub(",", ".", athlete_countries$Agriculture))
athlete_countries$Service = as.double(gsub(",", ".", athlete_countries$Service))

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}


PopulationPlot <- ggplot(countries_summary, 
       mapping = aes(x=Population, y=n_medals_per_year))+
  geom_point() +
  scale_x_continuous(trans='log10') +
  theme_light() +
  labs(title = expression(atop("Zależność śreniej ilości zdobywanych medali od populacji kraju i jego PKP")),
       y = expression(atop("Średnia ilość rocznie zdobywanych medali")), 
       x = expression(atop("Populacja",atop("w milionach")))) +
  geom_label_repel(aes(label=ifelse(n_medals_per_year > 45 |
                                      Team == "Poland",
                                    Team, "")),
                   box.padding   = 1,
                   point.padding = 0.5) +
  theme_light() +
  coord_flip()

PkbPlot <- ggplot(countries_summary, 
       mapping = aes(x = average_GDP, y = n_medals_per_year))+
  geom_point() +
  scale_x_continuous(trans=reverselog_trans(10)) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  labs(title = "",
       x = expression(atop("PKB",atop("w miliardach dolarów"))),
       y = "") +
  theme_light()

PopulationPlot / PkbPlot

SectorPlot <- ggplot(athlete_countries, 
                     mapping = aes(x = Service, fill = "Usługi"))+
  geom_density(adjust = 3, alpha=0.5) +
  geom_density(aes(x = Agriculture, fill = "Rolnictwo"), adjust = 3, alpha=0.5) +
  geom_density(aes(x = Industry, fill = "Przemysł"), adjust = 3, alpha=0.5) +
  theme_light() +
  scale_color_manual(values = c( "gray", "brown", "gold")) +
  labs(title = "Zależność ilości zdobywanych medali w zależności od wielkości sektorów gospodarki",
       x = expression(atop("Wielkość Sektora")),
       y = "Średnia ilość zdobywanych medali") +
  scale_x_continuous(labels = scales::percent) +
  guides(fill=guide_legend(title="Sektory"))
