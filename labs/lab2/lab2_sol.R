###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################
library(dplyr) # https://dplyr.tidyverse.org/

# Dane
starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych
str(starwars_new)

# Podgląd tabeli
View(starwars_new)

# Określamy typy zmiennych:
# name - jakościowa, nominalna
# height - ilościowa, ilorazowa
# mass - ilościowa, ilorazowa
# hair_color - jakościowa, nominalna 
# skin_color - jakościowa, nominalna
# birth_year - ilościowa, przedziałowa
# sex - jakościowa, binarna/nominalna

# 1) Wybór wierszy i kolumn w dplyr

# a) wybór kolumn ---> select()

select(starwars, name)
select(starwars, name, gender, mass)
select(starwars, gender, name, mass)

select(starwars, -name)
select(starwars, -name, -skin_color, -species)
select(starwars, -c(name, skin_color, species))

wybierz <- c("name", "gender")
select(starwars, wybierz)

wybierz <- c(1,2,3,4)
select(starwars, wybierz)

any_of()
select(starwars, any_of(wybierz))

all_of()
select(starwars, all_of(wybierz))

# b) wybór wierszy ---> filter()

filter(starwars, eye_color == "blue")
filter(starwars, eye_color == "blue" & hair_color == "blond")
filter(starwars, eye_color == "blue", hair_color == "blond")

filter(starwars, eye_color == "blue" | hair_color == "blond")

# 2) pipes %>% (skrót Ctrl + Shift + m)

starwars %>%
  filter(eye_color == "blue") %>% 
  select(name) %>% 
  head()

# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.

starwars %>% 
  filter(species == "Droid", height > 100) %>% 
  select(name)

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.

starwars %>% 
  filter(is.na(hair_color)) %>% 
  select(name)

# c) sortowanie wierszy ---> arrange()

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(height)

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(-height)

starwars %>% 
  filter(is.na(hair_color)) %>% 
  arrange(desc(height))

# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.

starwars %>% 
  arrange(desc(mass)) %>% 
  head(1)

starwars %>% 
  top_n(1, mass)

# d) transformacja zmiennych ---> mutate()

starwars %>% 
  mutate(height_m = height/100) %>% 
  View()

# e) transformacja zmiennych ---> transmute()

starwars %>% 
  transmute(height_m = height/100)

# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.

starwars %>% 
  mutate(height_m = height/100,
         BMI = mass/height_m^2) %>% 
  top_n(1, BMI) %>% 
  select(name)

# f) kolejność kolumn ---> relocate()

starwars %>% 
  relocate(sex:homeworld, .before = height)

starwars %>% 
  relocate(sex:homeworld, .after = height)

starwars %>% 
  relocate(where(is.numeric), .after = where(is.character))

# g) dyskretyzacja ---> ifelse(), case_when()

starwars %>% 
  mutate(species_new = ifelse(species == "Human", "Human", "Other")) %>% 
  select(name, species, species_new)

starwars %>% 
  mutate(species_new = case_when(species == "Human" ~ "Human",
                                 species == "Droid" ~ "Droid",
                                 TRUE ~ "Other")) %>% 
  select(name, species, species_new) %>% 
  tail()

# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile

summary(starwars$height)

starwars %>% 
  summarise(mean_mass = mean(mass))

starwars %>% 
  summarise(mean_mass = mean(mass, na.rm = TRUE))

starwars %>% 
  filter(hair_color == "blond") %>% 
  summarise(n = n())

# i) grupowanie ---> group_by() + summarise()

starwars %>% 
  group_by(species) %>% 
  summarise(mediana = median(mass, na.rm = TRUE))

starwars %>% 
  group_by(skin_color, eye_color) %>% 
  summarise(n = n())


# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

?relig_income

relig_income %>% 
  pivot_longer(!religion, names_to = "income", values_to = "count")

# k) pivot_wider()

?fish_encounters

fish_encounters %>% 
  pivot_wider(names_from = station, values_from = seen)

fish_encounters %>% 
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)


# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org

starwars %>%
  ggplot(aes(x = eye_color)) + 
  geom_bar() + 
  coord_flip()

starwars %>% 
  select(eye_color) %>%
  pull %>%
  table %>%
  sort

# l) kolejność poziomów ---> fct_infreq()

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
  mutate(eye_color = fct_infreq(fct_lump(eye_color, prop = 0.2, other_level = "OTHER"))) %>%
  ggplot(aes(x = eye_color)) + 
  geom_bar() + 
  coord_flip()

# n) kolejność poziomów na podstawie innej zmiennej ---> fct_reorder()

iris %>%
  # mutate(Species = fct_reorder(Species, Sepal.Width, .fun = mean, .desc = TRUE)) %>%
  ggplot(aes(x = Species, y = Sepal.Width)) +
  geom_boxplot()


# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
library(stringr) # https://stringr.tidyverse.org

x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)

# o) podział stringów ---> str_split()

str_split(x, pattern = "__")
str_split(x, pattern = "__", simplify = TRUE)
str_split(x, pattern = "_", n = 1, simplify = TRUE)
str_split(x, pattern = "_", n = 2, simplify = TRUE)
str_split(x, pattern = "_", n = 3, simplify = TRUE)

# p) usunięcie/zastąpienie znaków ---> str_remove(), str_replace()

str_remove(x, "=")
str_remove_all(x, "=")

str_replace(x, "_", "+")
str_replace_all(x, "_", " ")


# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org