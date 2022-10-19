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

mtcars[,3]
typeof(mtcars)
str(mtcars)
mtcars[2:3,4:5]
mtcars[c(2,3), c(4,5)]
mtcars[1:10,c(2,3)]
mtcars[,"mpg"]

mtcars[,c("mpg","disp")]
mtcars[c("disp", "mpg")]


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?


# 10 pierszych wierszy, 2 i 3 kolumna?

# Jak wybieramy kolumny po nazwach? 

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[,c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

mtcars$mpg

# Uwaga na przecinek i wybór kolumn poprzez indeksy
mtcars[1,2]
mtcars[c(1,2)]

mean(mtcars[,1])

str(mtcars)

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
mtcars[mtcars$cyl == 4,]
mean(mtcars[mtcars$cyl == 4,]$drat)
median(mtcars[mtcars$cyl == 4,]$dra)
mtcars$drat

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am)
table(mtcars$am)

# Prosty wykres


# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)


# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl))


# wykres kolowy
pie(table(mtcars$cyl))


# histogram
hist(mtcars$mpg)
?hist
hist(mtcars$mpg, breaks = 10)

#box plot
boxplot(mtcars$mpg)

max(mtcars$mpg)
min(mtcars$mpg)

boxplot(mtcars$hp, mtcars$mpg)



## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

employees

dim(employees)
head(employees)
employees[employees$surname == "Pietraszko",]

employees[employees$surname == "Insecure",]

proton(action = "login", login = "johnins")

head(top1000passwords)
dim(top1000passwords)
typeof(top1000passwords)
length(unique(top1000passwords))
top1000passwords[1]
head(top1000passwords)


proton(action = "login", login = "johnins", password = "qwerty")

for (pass in 1:1000) {
  proton(action = "login", login = "johnins", password = top1000passwords[pass])
}

head(logs)

proton()
head(employees)
employees[employees$surname == "Pietraszko",]

head(logs)
logs[logs$login == "slap",]
unique(logs[logs$login == "slap",]$host)
table(logs[logs$login == "slap",]$host)
max(table(logs[logs$login == "slap",]$host))
which.max(table(logs[logs$login == "slap",]$host))

proton(action = "server", host="194.29.178.16")

head(bash_history)
length(bash_history)
typeof(bash_history)



#194.29.178.16 
## 5) Umieszczamy rozwiązanie na repozytorium.