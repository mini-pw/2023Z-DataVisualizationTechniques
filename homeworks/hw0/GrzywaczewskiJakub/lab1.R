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
?mtcars

# Infomation
head(mtcars)
tail(mtcars, 5)
dim(mtcars)
# Indeksowanie
mtcars[c(2,3), c(4,5)]
mtcars[1, 1]
mtcars[1:10, 2:3]

mtcars["mpg"]
mtcars[, "mpg"]
mtcars[c("mpg", "cyl")]
mtcars[, c("mpg", "cyl")]
mtcars[, c("cyl", "mpg")] # odwrocona kolejnosc


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?

# Jak wybieramy kolumny po nazwach? 

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

# Jak wybierać jedną kolumnę?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mtcars[mtcars$cyl == 4, ]
mtcars[mtcars$cyl == 4, "drat"]
mean(mtcars[mtcars$cyl == 4, "drat"])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
unique(mtcars$am)
table(unique(mtcars$am))

# Prosty wykres
# Zależność "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot
barplot(mtcars$cyl)
barplot(table(mtcars$cyl))

# Wykres kołowy
pie(mtcars$cyl)
pie(table(mtcars$cyl))

# Wykres liniony (ale to powinny byc posortowane dane)
plot(mtcars$mpg, mtcars$hp, type="l")

# Histogram
hist(mtcars$mpg)
?hist
hist(mtcars$mpg, breaks = 10)

# Box plot
boxplot(mtcars$mpg)
max(mtcars$mpg)

boxplot(mtcars$hp)

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

## 5) Umieszczamy rozwiązanie na repozytorium.

# Task 1 Find login
data("employees")
head(employees)
employees[employees$name == "John", ]
# login = johnins
proton(action = "login", login = "johnins")

# Task 2 Find password
data("top1000passwords")
str(top1000passwords)
for (pass in top1000passwords){
  res = proton(action = "login", login = "johnins", password = pass)
  if (res != "Password or login is incorrect") {
    print(pass)
  }
}
password_list <- lapply(top1000passwords, function (x) proton(action = "login", login = "johnins", password = x))
wrong <- password_list[1]
password_list[3]
password_list[password_list != "Password or login is incorrect"]
# Found password = q1w2e3r4t5
proton(action = "login", login = "johnins", password = "q1w2e3r4t5")

# Logs
data(logs)
head(logs)
employees[employees$surname == "Pietraszko", ]
logs[logs$login == "slap", ]
slap_logs <- logs[logs$login == "slap", ]

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(slap_logs$host)
# Najczesciej z 194.29.178.16
proton(action = "server", host = "194.29.178.16")

# Find creds
data("bash_history")
head(bash_history)
str(bash_history)
install.packages("strings")

library(strings)
firsts <- lapply(bash_history, function (x) strsplit(x, " ")[[1]])
unique(firsts)
str(strsplit(bash_history, " "))
# Found pass = DHbb7QXppuHnaXGN
proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")
