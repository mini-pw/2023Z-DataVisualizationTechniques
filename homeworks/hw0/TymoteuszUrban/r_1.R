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
tail(mtcars)

?mtcars

dim(mtcars)

mtcars[c(1,2), 4:5]

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

# Pierwszy wiersz, pierwsza kolumna?

mtcars[1,1]

# 10 pierszych wierszy, 2 i 3 kolumna?

head(mtcars, 10)[2:3]

# Jak wybieramy kolumny po nazwach? 

mtcars$mpg

# Wszystkie wiersze i kolumny w kolejnosci "am", "wt", "mpg"?

mtcars[,c("am","wt")]



# Jak wybierac jedna kolumne?

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)
# 32 x 11

# 2. Jakie sa typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartosci zmiennej "cyl" i jakie to sa wartosci?

length(unique(mtcars$cyl))
# 4, 6, 8

# 4. Jaka jest srednia wartosc zmiennej "drat" dla samochodów o wartosci zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4, c('drat')])

# 5. Jakie sa unikalne wartosci zmiennej "am" i jaki jest ich rozklad (liczba wystapien)? 

mtcars$am
unique(mtcars$am)
table(mtcars$am)


# Prosty wykres

# Zaleznosc "mpg" i "hp" - scatter plot

plot( mtcars$hp, mtcars$mpg)


# Zmienna "cyl" - barplot

barplot(mtcars$cyl)

## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()

data("employees")
head(employees)
employees[employees$surname == "Insecure",c(1:3)]

data("top1000passwords")
head(top1000passwords)
unique(top1000passwords)

for (pass in top1000passwords)
{
  proton(action = "login", login="johnins", password=pass)
}

data(logs)
head(logs)
table(logs)

employees[employees$surname == "Pietraszko",c(1:3)]

logi <- logs[logs$login == "slap" ,1:3]
head(logi)
tabelka <- factor(logi$host)
tabelka <- as.data.frame(tabelka)
table(tabelka)

data(bash_history)
head(bash_history)
bash <- as.data.frame(bash_history)
bash[bash$bash_history=="slap",1]
bash[,1]
head(bash)

for (pas in bash_history)
{
  proton(action = "login", login="slap", password=pas)
}
#proton(action = "server", host="XYZ")
#slawomir pietraszko
## 5) Umiszczamy rozwiazanie na repozytorium.