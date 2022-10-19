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

tail(mtcars, 5)

dim(mtcars)

nrow(mtcars)

ncol(mtcars)

typeof(mtcars)


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[c(2,3),c(4,5)]

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10,c(2,3)]

# Jak wybieramy kolumny po nazwach? 

mtcars[c("mpg","am")]

mtcars[,"mpg"]

mtcars$mpg

# Wszystkie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?

mtcars[c("am","wt","mpg")]

mtcars[,c("am","wt","mpg")]

# Jak wybierac jedna kolumne?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie sa typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?

unique(mtcars$cyl)

length(unique(mtcars$cyl))

# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?

mtcars$cyl== 4

mtcars[mtcars$cyl == 4, ]

mtcars[mtcars$cyl == 4, "drat"]

mean(mtcars[mtcars$cyl == 4, "drat"])

median(mtcars[mtcars$cyl == 4, "drat"])

# 5. Jakie sa unikalne wartosci zmiennej "am" i jaki jest ich rozklad (liczba wystapien)? 

unique(mtcars$am)

table(mtcars$am)

# Prosty wykres

plot(mtcars$mpg, mtcars$hp)

# Zaleznosc "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)

barplot(table(mtcars$cyl))

# wykres kolowy (UWAGA!!!)

pie(mtcars$cyl)

pie(table(mtcars$cyl))

hist(mtcars$mpg)

boxplot(mtcars$mpg, mtcars$hp)

max(mtcars$mpg)


## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

#ETAP 1
employees[employees$surname == "Insecure",]
proton(action = "login", login="johnins")

#ETAP 2
for (x in top1000passwords){
  proton(action = "login", login="johnins", password=x)
}

#ETAP 3
logs[logs$login=="pietraszko",]

## 5) Umieszczamy rozwiazanie na repozytorium.
