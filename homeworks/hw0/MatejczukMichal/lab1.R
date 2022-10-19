###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzacy
# Anna Kozak/Mateusz Krzyzinski/Hubert Ruczynski/Mikolaj Spytek 
# Kontakt: MS Teams lub mail
# anna.kozak@pw.edu.pl/mateusz.krzyzinski.stud@pw.edu.pl/hruczynski21@interia.pl/mikolaj.spytek.stud@pw.edu.pl

## 1) Sposób pracy na zajeciach laboratoryjnych
# a) pracujemy w R (wiekszosc semestru) i Python
# b) pracujemy na przygotowanych plikach, które beda na repozytorium przedmiotu
# c) podczas zajec prowadzacy bedzie wprowadzal zagdanienia, a nastepnie bedzie rozwiazywanie zadan w celu utrwalenia wiadomosci
# d) kolejna porcja materialu bedzie omawiana jezeli wiekszosc grupy wykona zadane zadanie 
# e) wszelkie pytania czy to zwiazane z kodem, praca domowa czy kwestie teoretyczne prosze smialo zglaszac prowadzacemu 

## 2) Materialy
# Repozytorium na GitHub
# https://github.com/mini-pw/2023Z-DataVisualizationTechniques 

## 3) Jak dziala GitHub?
# Jak zglosic prace domowa/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 4) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?

# Jak wybieramy kolumny po nazwach? 

# Wszystkie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?

# Jak wybierac jedna kolumne?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

# 2. Jakie sa typy zmiennych?

# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?

# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?

# 5. Jakie sa unikalne wartosci zmiennej "am" i jaki jest ich rozklad (liczba wystapien)? 

# Prosty wykres

# Zaleznosc "mpg" i "hp" - scatter plot



# Zmienna "cyl" - barplot


## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees[employees$surname == "Insecure",]
proton(action = "login", login="johnins")

for(i in top1000passwords){
  if(proton(action = "login", login="johnins", password=i) == "Success! User is logged in!"){
    break
  }
}

install.packages("dplyr")
library(dplyr)

employees[employees$surname == "Pietraszko",]
tmp <- logs[logs$login == "slap",]
tmp2 <- tmp %>% group_by(host) %>% count()
tmp2
proton(action = "server", host="194.29.178.16")

split(bash_history, " ") -> tmp

for(j in tmp){
  proton(action = "login", login="johnins", password=j)
}



## 5) Umieszczamy rozwiazanie na repozytorium.