###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 2            ###
###########################################
library(dplyr) # https://dplyr.tidyverse.org/

# Dane
starwars
starwars_new <- starwars[, c(1, 2, 3, 4, 5, 7, 8)]

# Informacje o zbiorze danych


# Podgląd tabeli


# Określamy typy zmiennych:
# name - 
# height - 
# mass -
# hair_color - 
# skin_color - 
# birth_year - 
# sex - 

# 1) Wybór wierszy i kolumn w dplyr


# a) wybór kolumn ---> select()


# b) wybór wierszy ---> filter()


# 2) pipes %>% (skrót Ctrl + Shift + m)


# Zadanie 1
# Używając funkcji z pakietu dplyr() wybierz te postacie, których gatunek to Droid, 
# a ich wysokość jest większa niż 100.


# Zadanie 2 
# Używając funkcji z pakietu dplyr() wybierz te postacie, które nie mają określonego koloru włosów.


# c) sortowanie wierszy ---> arrange()


# Zadanie 3
# Używając funkcji z pakietu dplyr() wybierz postać o największej masie.


# d) transformacja zmiennych ---> mutate()


# e) transformacja zmiennych ---> transmute()


# Zadanie 4
# Używając funkcji z pakietu dplyr() wylicz wskaźnik BMI (kg/m^2) i wskaż postać, która ma największy wskaźnik.


# f) kolejność kolumn ---> relocate()


# g) dyskretyzacja ---> ifelse(), case_when()


# h) funkcje agregujące ---> summarise(), n(), mean, median, min, max, sum, sd, quantile


# i) grupowanie ---> group_by() + summarise()


# 3) Przekształcenie ramki danych w tidyr
library(tidyr) # https://tidyr.tidyverse.org

# j) pivot_longer()

?relig_income

# k) pivot_wider()

?fish_encounters

# 4) Praca z faktorami (szczególnie w wizualizacji)
library(forcats) # https://forcats.tidyverse.org
library(ggplot2) # https://ggplot2.tidyverse.org


# l) kolejność poziomów ---> fct_infreq()


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
