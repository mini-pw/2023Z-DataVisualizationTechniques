###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) ProwadzDcy
# Anna Kozak/Mateusz KrzyziEski/Hubert RuczyEski/MikoEaj Spytek 
# Kontakt: MS Teams lub mail
# anna.kozak@pw.edu.pl/mateusz.krzyzinski.stud@pw.edu.pl/hruczynski21@interia.pl/mikolaj.spytek.stud@pw.edu.pl

## 1) SposC3b pracy na zajDciach laboratoryjnych
# a) pracujemy w R (wiDkszoED semestru) i Python
# b) pracujemy na przygotowanych plikach, ktC3re bDdD na repozytorium przedmiotu
# c) podczas zajDD prowadzDcy bDdzie wprowadzaE zagdanienia, a nastDpnie bDdzie rozwiDzywanie zadaE w celu utrwalenia wiadomoEci
# d) kolejna porcja materiaEu bDdzie omawiana jeE<eli wiDkszoED grupy wykona zadane zadanie 
# e) wszelkie pytania czy to zwiDzane z kodem, pracD domowD czy kwestie teoretyczne proszD EmiaEo zgEaszaD prowadzDcemu 

## 2) MateriaEy
# Repozytorium na GitHub
# https://github.com/mini-pw/2023Z-DataVisualizationTechniques 

## 3) Jak dziaEa GitHub?
# Jak zgEosiD pracD domowD/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 4) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

 ?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?

# Jak wybieramy kolumny po nazwach? 

# Wszystkie wiersze i kolumny w kolejnoEci "am", "wt", "mpg"?

# Jak wybieraD jednD kolumnD?

# Uwaga na przecinek i wybC3r kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

# 2. Jakie sD typy zmiennych?

# 3. Ile jest unikalnych wartoEci zmiennej "cyl" i jakie to sD wartoEci?

# 4. Jaka jest Erednia wartoED zmiennej "drat" dla samochodC3w o wartoEci zmiennej "cyl" rC3wnej 4?

# 5. Jakie sD unikalne wartoEci zmiennej "am" i jaki jest ich rozkEad (liczba wystDpieE)? 

# Prosty wykres

# ZaleE<noED "mpg" i "hp" - scatter plot



# Zmienna "cyl" - barplot


## 4) Gra proton, naleE<y stworzyD plik R z kodami do rozwiDzania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


## 5) Umieszczamy rozwiDzanie na repozytorium.
dim(employees)
head(employees)
employees[employees$name == 'John',]

proton(action = "login", login="johnins")

head(top1000passwords)

for(x in top1000passwords) {
  proton(action = "login", login="johnins", password=x)
}

head(logs)
employees[employees$name == 'Slawomir',]
table(logs[logs$login == "slap",]$host) -> t


