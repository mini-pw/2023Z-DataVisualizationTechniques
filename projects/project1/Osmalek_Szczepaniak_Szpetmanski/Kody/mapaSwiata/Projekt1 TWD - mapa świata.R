library(XML)
library(RCurl)
library(dplyr)
link <- "https://www.worldathletics.org/records/by-category/world-records"
xData <- getURL(link)
tabele <- readHTMLTable(xData, stringsAsFactors = FALSE)
length(tabele)
tabele[[1]] -> tabela1
tabele[[2]] -> tabela2
tabele[[3]] -> tabela3
tabele[[4]] -> tabela4
tabele[[5]] -> tabela5

rekordy <- rbind(tabela1,tabela2,tabela3,tabela4,tabela5)

rekordy[7] -> kraje
kraje %>% group_by(kraje[[1]]) %>% 
  summarise(total_count=n(),.groups = 'drop') %>%
  as.data.frame() -> kraje


colnames(kraje)[1] ="region"
colnames(kraje)[2] ="value"



kraje[kraje == 'AUS'] <- 'Australia'
kraje[kraje == 'BRA'] <- 'Brazil'
kraje[kraje == 'CAN'] <- 'Canada'
kraje[kraje == 'CHN'] <- 'China'
kraje[kraje == 'CUB'] <- 'Cuba'
kraje[kraje == 'ETH'] <- 'Ethiopia'
kraje[kraje == 'FRA'] <- 'France'
kraje[kraje == 'GDR'] <- 'Germany'
kraje[kraje == 'JAM'] <- 'Jamaica'
kraje[kraje == 'KEN'] <- 'Kenya'
kraje[kraje == 'LTU'] <- 'Lithuania'
kraje[kraje == 'NOR'] <- 'Norway'
kraje[kraje == 'POL'] <- 'Poland'
kraje[kraje == 'RUS'] <- 'Russia'
kraje[kraje == 'VEN'] <- 'Venezuela'
kraje[kraje == 'BDI'] <- 'Burundi'
kraje[kraje == 'BRN'] <- 'Brunei'
kraje[kraje == 'BUL'] <- 'Bulgaria'
kraje[kraje == 'DJI'] <- 'Djibouti'
kraje[kraje == 'VEN'] <- 'Venezuela'
kraje[kraje == 'ITA'] <- 'Italy'
kraje[kraje == 'MEX'] <- 'Mexico'
kraje[kraje == 'MOZ'] <- 'Mozambique'
kraje[kraje == 'NAM'] <- 'Namibia'
kraje[kraje == 'QAT'] <- 'Qatar'
kraje[kraje == 'UKR'] <- 'Ukraine'
kraje[kraje == 'CZE'] <- 'Czech Republic'
kraje[kraje == 'NOR'] <- 'Norway'
kraje[kraje == 'GBR'] <- 'United Kingdom'
kraje[kraje == 'JPN'] <- 'Japan'
kraje[kraje == 'MAR'] <- 'Marocco'
kraje[kraje == 'UGA'] <- 'Uganda'
kraje[kraje == 'CHN'] <- 'China'
kraje[kraje == 'SWE'] <- 'Sweden'
kraje[kraje == 'VEN'] <- 'Venezuela'




library(dplyr)
library(ggplot2)
library(maps)

worldmap = map_data("world")
merged_data <- merge(x = worldmap, y = kraje, by.x = "region", by.y = "region", all.x = TRUE) %>% arrange(order)

merged_data %>% mutate(value_cut = cut(value, breaks=c(0,2,4,5,10,20,30))) %>% ggplot() -> gg 
gg <- gg + geom_polygon(aes(x = long, y = lat, group = group,
                            fill = value_cut),
                        size = 0.25, color = '#A9A9A9')
cols <- c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177","#49006a")
gg <- gg + scale_fill_manual(values = cols, na.value = '#46386E') +
  theme(rect = element_blank(),axis.title.x = element_blank(),
        axis.title.y=element_blank(),axis.ticks = element_blank(),
        axis.text.x =element_blank(),axis.text.y = element_blank(),
        legend.key.size = unit(10, 'mm'))+
  labs(fill='World Records in athletics')
  
print(gg)


  


























