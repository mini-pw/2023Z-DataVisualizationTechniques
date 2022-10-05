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
tail(mtcars)

dim(mtcars)
class(mtcars)
str(mtcars)

mtcars$mpg

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[c(1, 2), 4:6]
mtcars[c(1, 2)]
mtcars[5, ]
# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 

mtcars$disp

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy


# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)
typeof(mtcars$mpg)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

?unique

length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4, "drat"])
median(mtcars[mtcars$cyl == 4, "drat"])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am)
?table

table(mtcars$am)


# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot

barplot(mtcars$cyl)


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

?employees

head(employees)

employees[employees$surname == "Insecure", "login"]

proton(action = "login", login="johnins")

top1000passwords[1]

?top1000passwords

for(i in 1:length(top1000passwords)){
  proton(action = "login", login = "johnins", password = top1000passwords[i])
}


# 3
head(logs)
?aggregate
?which.max


employees[employees$surname == "Pietraszko", "login"]



which.max(table(logs[logs$login == "slap", ]$host))

proton(action = "server", host="194.29.178.16")


# 4

head(bash_history)
?bash_history

?cat
cat(bash_history, )

?paste

?substr

substr(bash_history[1], stop = " ")
?sub

for( i in 1: length(bash_history)){
  sub(" .*","",bash_history[i])
  proton(action = "login", login="slap", password=sub(" .*","",bash_history[i]))
}
sub(" .*","",bash_history[3])


## 5) Umieszczamy rozwiązanie na repozytorium.