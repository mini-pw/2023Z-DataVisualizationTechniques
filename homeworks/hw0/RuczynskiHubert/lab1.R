###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadzący
# Anna Kozak/Mateusz Krzyziński/Hubert Ruczyński/Mikołaj Spytek 
# Kontakt: MS Teams lub mail
# anna.kozak@pw.edu.pl/mail/mail/mail

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
head(mtcars, 10)
tail(mtcars, 10)
?mtcars
dim(mtcars)
class(mtcars)
type(mtcars)
str(mtcars)

mtcars$mpg
# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?
mtcars[c(1, 2),  4:10]
mtcars[c(1, 2)]
mtcars[5, ]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1,1]
# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[1:10, 2:3]
# Jak wybierać jedną kolumnę?
mtcars[, c("am", "wt", "mpg")]
# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)
typeof(mtcars$mpg)
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, c('drat')])
median(mtcars[mtcars$cyl == 4, c('drat')])

# Jakie są unikalne wartości zmiennej am i jaki jest jej rozkład / liczba wystąpień

mtcars$am
unique(mtcars$am)
table(mtcars$am)
factor(mtcars$am)

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$mpg)


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


## 5) Umieszczamy rozwiązanie na repozytorium.