###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzący
# Anna Kozak/Mateusz Krzyziński/Hubert Ruczyński/Mikołaj Spytek 
# Kontakt: MS Teams lub mail
# anna.kozak@pw.edu.pl/mateusz.krzyzinski.stud@pw.edu.pl/hruczynski21@interia.pl/mikolaj.spytek.stud@pw.edu.pl

## 1) Sposób pracy na zajęciach laboratoryjnych
# a) pracujemy w R (większość semestru) i Python
# b) pracujemy na przygotowanych plikach, które będą na repozytorium przedmiotu
# c) podczas zajęć prowadzący będzie wprowadzał zagdanienia, a następnie będzie rozwiązywanie zadań w celu utrwalenia wiadomości
# d) kolejna porcja materiału będzie omawiana jeżeli większość grupy wykona zadane zadanie 
# e) wszelkie pytania czy to związane z kodem, pracą domową czy kwestie teoretyczne proszę śmiało zgłaszać prowadzącemu 

## 2) Materiały
# Repozytorium na GitHub
# https://github.com/mini-pw/2023Z-DataVisualizationTechniques 

## 3) Jak działa GitHub?
# Jak zgłosić pracę domową/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 4) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars

dim(mtcars)
nrow(mtcars)
ncol(mtcars)

str(mtcars)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[2:3,]
mtcars[2:3, 4:5]

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]
head(mtcars[,2:3], 10)

# Jak wybieramy kolumny po nazwach?

mtcars[,c("mpg","cyl")] #możemy ustawić kolejność
mtcars[c("mpg","cyl")] # R wybiera domyślnie kolumny

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[,c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

mtcars$mpg # dostajemy wektor
mtcars["mpg"] # ramka danych
mtcars[,"mpg"] # wektor

# Uwaga na przecinek i wybór kolumn poprzez indeksy

mtcars[,c(1,2)]


# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars$cyl))
unique(mtcars$cyl)


# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?


temp <- mtcars$cyl == 4
mean(mtcars[temp, "drat"])
mean(mtcars[mtcars$cyl == 4, "drat"]) # median sd min max

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am)
table(mtcars$am) # zero 19 razy i jeden 13 razy

# Prosty wykres



# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl)
barplot(table(mtcars$cyl))



## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
employees

for(passwd in top1000passwords){     
  proton(action = "login", login="johnins", password=passwd)
  }

## 5) Umieszczamy rozwiązanie na repozytorium.