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
head(mtcars, 15)
table(head(mtcars))
tail(mtcars, 10)
?mtcars
dim(mtcars)
class(mtcars)
str(mtcars)
mtcars$mpg
mtcars[c(1, 2), 4:5]
mtcars[c(1, 2)]
mtcars[, 5]
mtcars[1:10, 2:3]
mtcars[, c("am", "wt", "mpg")]
# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?

# 10 pierszych wierszy, 2 i 3 kolumna?

# Jak wybieramy kolumny po nazwach? 

# Wszystkie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?

# Jak wybierac jedna kolumne?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
class(mtcars)
dim(mtcars)
# 2. Jakie sa typy zmiennych?
str(mtcars)
typeof(mtcars$mpg)
# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl == 4, c('drat')])
median(mtcars[mtcars$cyl == 4, c('drat')])
# 5. Jakie sa unikalne wartosci zmiennej "am" i jaki jest ich rozklad (liczba wystapien)? 
unique(mtcars$am)
table(mtcars$am)
factor(mtcars$am)
# Prosty wykres

# Zaleznosc "mpg" i "hp" - scatter plot
plot(mtcars$mpg, mtcars$hp)
barplot(mtcars$mpg)
# Zmienna "cyl" - barplot


## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
head(employees)
employees[employees$name == 'John' & employees$surname == 'Insecure', c('login')]
proton(action = "login", login="johnins")
head(top1000passwords)
for (x in 1:1000){
  
  proton(action = "login", login="johnins", password=top1000passwords[x])
  
}

head(logs)
?aggregate
table(logs$host)
employees[employees$name == 'Slawomir' & employees$surname == 'Pietraszko', c('login')]
a = logs[logs$login == "slap", ]
table(a$host)
194.29.178.16
proton(action = "server", host="194.29.178.16")


head(bash_history)
bash_history
bash_history = strsplit(bash_history, ' ')
head(bash_history)


for (x in 1:length(bash_history)){
  proton(action = "login", login="slap", password=bash_history[[x]][1])
}

length(bash_history)
## 5) Umieszczamy rozwiazanie na repozytorium.