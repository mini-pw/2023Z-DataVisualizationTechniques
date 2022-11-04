library(dplyr)
library(ggplot2)


#wykres 1
odpowiedz <- c("zdecydowanie zmniejszy", "raczej zmniejszy", "zdecydowanie zwiększy", "raczej zwiększy", "będzie bez wpływu", "nie wiem")
odpowiedz <- factor(odpowiedz, levels = unique(odpowiedz))
ile_odp1 <- c(277, 296, 83, 98, 154, 92)
procent_odp1 <- ile_odp1/sum(ile_odp1)*100
grupy_odpowiedzi1 <- c("nie", "nie", "tak", "tak", "bez wplywu", "nie wiem")

df1 <- data.frame(odpowiedz, procent_odp1, grupy_odpowiedzi1)


plot1 <- ggplot(df1, mapping =  aes(x = odpowiedz, y = procent_odp1, fill = grupy_odpowiedzi1)) + geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("azure4", "brown1", "darkgrey", "cadetblue4")) + 
  labs(title = "W jaki sposób ułatwienie dostępu do broni palnej w Polsce wpłynie na Pani/Pana \nbezpieczeństwo?", x = "Odpowiedź", y = "Procent odpowiedzi") +
  geom_text(aes(label = procent_odp1), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_y_continuous(limits = c(0,32), expand = c(0, 0), breaks = seq(0,32,5)) +
  theme_bw() +
  theme(panel.grid = element_blank())
plot1

#wykres 2
plec <- c(rep("kobieta", 6), rep("mężczyzna", 6))
procent_odp2 <- c(34, 34, 6, 1, 14,11, 21, 25, 11, 20, 17,6)

df2 <- data.frame(odpowiedz, procent_odp2, plec)
df2 <- df2 %>% group_by(odpowiedz)

plot2 <- ggplot(df2, mapping =  aes(x = odpowiedz, y = procent_odp2, fill = plec)) + geom_bar(position = "dodge", stat = "identity") +
  labs(title = "W jaki sposób ułatwienie dostępu do broni palnej w Polsce wpłynie na Pani/Pana \nbezpieczeństwo? (w zależności od płci)", x = "Odpowiedź", y = "Procent odpowiedzi") +
  guides(fill=guide_legend(title="Płeć")) +
  geom_text(aes(label = procent_odp2), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_manual(values = c("darkgoldenrod1", "black")) +
  scale_y_continuous(limits = c(0,38), expand = c(0, 0), breaks = seq(0, 28, 5)) +
  theme_bw() +
  theme(panel.grid = element_blank())
plot2


