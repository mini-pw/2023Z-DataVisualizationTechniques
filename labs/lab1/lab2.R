###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################
install.packages('dplyr')
library(dplyr) # https://dplyr.tidyverse.org/

# Dane
starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych
str(starwars_new)

# Podglad tabeli
View(starwars_new)

# Okreslamy typy zmiennych:
# name - jakosciowa, nominalna
# height - ilosciowa, ilorazowa
# mass - ilosciowa, ilorazowa
# hair_color - jakosciowa, nominalna
# skin_color - jakosciowa, nominalna
# birth_year - ilosciowa, przedzialowa
# sex - jakosciowa, binarna/nominalna

# 1) Wybór wierszy i kolumn w dplyr


# a) wybór kolumn ---> select()
select(starwars_new, name)

# b) wybór wierszy ---> filter()
filter(starwars, eye_color == "blue")
filter(starwars, eye_color == "blue" & hair_color == "blond")

# 2) pipes %>% (skrót Ctrl + Shift + m)
starwars %>%
  filter(eye_color == "blue") %>%
  select(name) %>%
  tail()

# Zadanie 1
# Uzywajac funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokosc jest wieksza niz 100.
View(starwars_new)
filter(starwars, species == "Droid" & height > 100) %>%
  select(name)


# Zadanie 2 
# Uzywajac funkcji z pakietu dplyr() wybierz te postacie, które nie maja okreslonego koloru wlosów.
filter(starwars, hair_color == NA | hair_color == "none" | hair_color == "unknown")


# c) sortowanie wierszy ---> arrange()
arrange(starwars, -height)


# Zadanie 3
# Uzywajac funkcji z pakietu dplyr() wybierz postac o najwiekszej masie.
starwars %>%
  arrange(-mass) %>%
  head(1)

# d) transformacja zmiennych ---> mutate()
starwars %>%
  mutate(height_m = height/100) %>%
  select(name, height, height_m)

# e) transformacja zmiennych ---> transmute()
starwars %>%
  transmute(height_m = height/100)

# Zadanie 4
# Uzywajac funkcji z pakietu dplyr() wylicz wskaznik BMI (kg/m^2) i wskaz postac, która ma najwiekszy wskaznik.
starwars %>%
  transmute(BMI = mass/((height/100)**2)) %>%
  top_n(1, BMI)

# f) kolejnosc kolumn ---> relocate()
starwars %>%
  relocate(sex:homeworld, .before = height)

starwars %>%
  relocate(where(is.numeric), .after = where(is.character))

# g) dyskretyzacja ---> ifelse(), case_when()
starwars %>%
  mutate(is_human.or_droid = ifelse(species == "Human", "Human", "Other")) %>%
  select(name, species, is_human)

# h) funkcje agregujace ---> summarise(), n(), mean, median, min, max, sum, sd, quantile
summary(starwars$height)
starwars %>%
  summarise(mean_mass = mean(mass, na.rm = TRUE))

starwars %>%
  filter(hair_color == "blond") %>%
  summarise(n = n())
  

# i) grupowanie ---> group_by() + summarise()
starwars %>%
  group_by(species) %>%
  summarise(med = median(mass, na.rm = T))

starwars %>%
  group_by(skin_color, eye_color) %>%
  summarise(n = n())


# 3) Przeksztalcenie ramki danych w tidyr
install.packages("tidyr")
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count")

# k) pivot_wider()

fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)

# 4) Praca z faktorami (szczególnie w wizualizacji)
install.packages("forcats")
install.packages("ggplot2")
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org

starwars %>%
  ggplot(aes(x = eye_color)) +
  geom_bar() +
  coord_flip()


# l) kolejnosc poziomów ---> fct_infreq()
starwars %>%
  mutate(eye_color = fct_infreq(eye_color)) %>%
  ggplot(aes(x = eye_color)) +
  geom_bar() +
  coord_flip()


# m) scalanie poziomów ---> fct_lump()
starwars %>%
  mutate(eye_color = fct_infreq(fct_lump(eye_color, n = 5))) %>%
  ggplot(aes(x = eye_color)) +
  geom_bar() +
  coord_flip()

starwars %>%
  mutate(eye_color = fct_infreq(fct_lump(eye_color, prop = 0.05))) %>%
  ggplot(aes(x = eye_color)) +
  geom_bar() +
  coord_flip()

# n) kolejnosc poziomów na podstawie innej zmiennej ---> fct_reorder()
iris %>%
  mutate(Species = fct_reorder(Species, Sepal.Width, .fun = mean, .desc = T))
  ggplot(aes(x = Species, y = Sepal.Width)) + 
  geom_boxplot()

# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
install.packages("stringr")
library(stringr) # https://stringr.tidyverse.org


x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)

# o) podzial stringów ---> str_split()
str_split(x, pattern = "__", simplify = T)
str_split(x, pattern = "_", n=2, simplify = T)

# p) usuniecie/zastapienie znaków ---> str_remove(), str_replace()
str_remove(x, "=")
str_remove_all(x, "=")
str_replace_all(x, "_", "+")

# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org