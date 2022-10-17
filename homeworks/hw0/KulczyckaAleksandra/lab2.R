###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################
library("dplyr") # https://dplyr.tidyverse.org/

# Dane
starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych


# Podgląd tabeli


# Określamy typy zmiennych:
# name - jakościowa, nominalna
# height - ilościowa, ilorazowa
# mass - ilościowa, ilorazowa
# hair_color - jakościowa, nominalna
# skin_color - jakościowa, nominalna
# birth_year - ilościowa, przedziałowa
# sex - jakościowa, nominalna/binarna

# 1) Wybór wierszy i kolumn w dplyr
?select


# a) wybór kolumn ---> select()

select(starwars,name)
select(starwars,1:3)
select(starwars, gender, mass, name) #zmienia sie kolejnosc
select(starwars, gender, mass:name) #wszystko pomiedzy sie wyswietla
select(starwars, -c(gender, mass, name)) #wyswietla wszystko procz tego wektora kolumn
temp <- c("name","gender")
select(starwars, temp)
?any_of
?all_of

temp2 <- c("name","gender","smile") #jedna z nich nie istnieje w tabeli
select(starwars, temp2)
select(starwars, any_of(temp2)) #wyswietla te ktore istnieja z wektora 
select(starwars, all_of(temp2)) #wyswietla błąd


# b) wybór wierszy ---> filter()

filter(starwars, eye_color == "blue")
filter(starwars, eye_color == "blue" & hair_color == "blond")
filter(starwars, eye_color == "blue", hair_color == "blond") # , i & są równoważne w warunku przecięcia zbioróW

filter(starwars, eye_color == "blue" | hair_color == "blond") #lub to |


# 2) pipes %>% (skrót Ctrl + Shift + m)

starwars %>% #jako pierwszy argument w kolejnej komendzie przechodzi to co wczesniej bylo przefiltrowane
  filter(eye_color == "blue") %>%
 select(name) %>%
   tail()

# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.

filter(starwars,species == "Droid", height > 100)

# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.

filter(starwars, is.na(hair_color) | hair_color == "none")

# c) sortowanie wierszy ---> arrange()
?arrange
filter(starwars, is.na(hair_color) | hair_color == "none") %>%
arrange(height) #rosnąco

filter(starwars, is.na(hair_color) | hair_color == "none") %>%
arrange(-height) #malejąco, po ujemnej zmiennej

# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.

starwars %>%
  arrange(-mass)%>%
  top_n(1,mass)

# d) transformacja zmiennych ---> mutate()

starwars %>%
  mutate(height_m = height/100)%>%
  select(name,height,height_m)

# e) transformacja zmiennych ---> transmute()

starwars %>% 
  transmute(height_m = height/100) #transmute drops existing ones 


# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.

starwars%>%
  mutate(height_m = height/100,bmi = mass/height_m^2)%>%
  select(name,mass,height_m,bmi)%>%
  top_n(1,bmi)
  

# f) kolejność kolumn ---> relocate()

starwars%>%
  relocate(sex:homeworld,.before = height)%>%
  relocate(sex:homeworld,.after = height)
# g) dyskretyzacja ---> ifelse(), case_when()

starwars%>%
  mutate(is_human = ifelse(species == "Human", "Human", "Other"))%>%
  select(name,species,is_human)

starwars%>%
  mutate(is_human_or_droid = case_when(species == "Human" = "Human",species == "Droid" = "Droid"))%>%
  select(name,species,is_human_or_droid)
# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile

summary(starwars$height)
starwars%>%
  summarise(mean_mass = mean(mass,na.rm = T))
# i) grupowanie ---> group_by() + summarise()
starwars%>%
  group_by(species)%>%
  summarise(med = median(mass, na.rm = T))

starwars%>%
  group_by(skin_color,eye_color)%>%
  summarise(n = n())


  
# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

?relig_income

relig_income%>%
  pivot_longer(!religion,names_to = "income")

# k) pivot_wider()
fish_encounters%>%
pivot_wider(names_from = station, values_from = seen,values_fill = 0)

?fish_encounters

# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org

vec <- c(1,2,3)
as.factor(vec)


# l) kolejność poziomów ---> fct_infreq()
starwars%>%
  mutate(eye_color = fct_infreq((eye_color))%>%
           ggplot(aes(x = eye_color))+
           geom_bar()+
           coord_flip()

# m) scalanie poziomów ---> fct_lump()


# n) kolejność poziomów na podstawie innej zmiennej ---> fct_reorder()


# 4) Praca z stringami
# Zaawansowane: https://stringi.gagolewski.com
library(stringr) # https://stringr.tidyverse.org

x <- paste0(letters[1:5], "=", 1:5, "__", letters[6:10], "=", 6:10)

# o) podział stringów ---> str_split()


# p) usunięcie/zastąpienie znaków ---> str_remove(), str_replace()


# 5) https://www.tidyverse.org/packages
# Bezpieczne wczytywanie danych w R https://readr.tidyverse.org