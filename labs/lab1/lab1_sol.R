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

?mtcars
mtcars
data(mtcars)
head(mtcars, 10)
tail(mtcars, 10)
dim(mtcars)
str(mtcars)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[2:3,4:5]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
head(mtcars[,2:3], 10)

# Jak wybieramy kolumny po nazwach?
mtcars[, c("mpg", "cyl")]
mtcars[, c("cyl", "mpg")]
mtcars[c("cyl", "mpg")]
# Bez przecinka - wybór kolumn

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$mpg
mtcars["mpg"]
# Jaka jest różnica?

# $ nie można użyć, gdy nazwę kolumny mamy w zmiennej, np. w pętli
var <- "mpg"
# NIE mtcars$var
mtcars[,var]

# Uwaga na przecinek i wybór kolumn poprzez indeksy
mtcars[c(1,2)]
mtcars[1,2]


# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars$cyl)
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mtcars$cyl == 4
mean(mtcars[mtcars$cyl == 4, c("drat")])
median(mtcars[mtcars$cyl == 4, c("drat")])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)?
mtcars$am
table(mtcars$am)

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

# Przykładowe rozwiązanie

## Etap 1

employees[employees$surname == "Insecure", ]

proton(action = "login", login = "johnins")

## Etap 2

top1000passwords #wektor

for (pass in top1000passwords) {
  response <- proton(action = "login", login = "johnins", password = pass)
  if (response == 'Success! User is logged in!'){
    cat(pass)
  }
}

## Etap 3

employees[employees$surname == "Pietraszko",]

table(logs[logs$login == "slap", c("host") ]) -> tmp
data.frame(tmp)

proton(action = "server", host = "194.29.178.16")


## Etap 4

bash_history #lista

split_bash_history <- strsplit(bash_history, " ") # rozdziela napisy względem podanego znaku, tutaj względem spacji " "

commands <-  c()

for (x in split_bash_history){ # wybieramy pierwszy napis po podzieleniu komendy z bash_history
  commands <- c(commands, x[[1]])
}

for (command in unique(commands)){ # wykonujemy w pętli logowanie używając po kolei wartości commands
  proton(action = "login", login = "slap", password = command)
}

# Inna opcja rozwiązania, nie brute-force
table(commands) # sprawdzamy stringa, który wygląda na hasło (użyty tylko 1 raz)
proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")


## 5) Umieszczamy rozwiązanie na repozytorium.
