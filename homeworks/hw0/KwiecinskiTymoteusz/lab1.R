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
table(mtcars)
?mtcars

typeof(mtcars)
str(mtcars)
class(mtcars)
mtcars$mpg

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?


mtcars[c(1,3), 4:6]

mtcars[, 5]
mtcars[5, ]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, c(2,3)]

# Jak wybieramy kolumny po nazwach? 

mtcars[, c("am", "wt", "mpg")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars$cyl))
unique(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

median(mtcars[mtcars$cyl==4, c("drat")])
mean(mtcars[mtcars$cyl==4, c("drat")])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

mtcars$am

table(mtcars$am)
factor(mtcars$hp)

# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(mtcars$cyl, mtcars$hp)

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


## 5) Umieszczamy rozwiązanie na repozytorium.



employees[employees$name == "John" & employees$surname == "Insecure", ]
proton(action = "login", login="johnins")


for (pass in top1000passwords) {
  proton(action = "login", login="johnins", password=pass)
}


head(logs)
employees[employees$surname == "Pietraszko", ]

interesting
interesting = logs[logs$login == "slap", ]
table(interesting)
head(interesting)

res = aggregate(x = interesting, list(interesting$host), FUN = length)
head(res[order(res$login, decreasing=TRUE), ], 1) ## tutaj mamy wynik


proton(action = "server", host="194.29.178.16")


head(bash_history)
class(bash_history)
length(bash_history)
passwords = sub(" ", "", bash_history)

for(pass in passwords) {
  proton(action = "login", login="slap", password=pass)
}
