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


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

head(mtcars, 15)
tail(mtcars)
dim(mtcars) # wymiary
nrow(mtcars)
ncol(mtcars)
str(mtcars)

# Pierwszy wiersz, pierwsza kolumna?

mtcars[2:3, 4:5]
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]

# Jak wybieramy kolumny po nazwach? 

mtcars[, c("mpg", "cyl")]
mtcars[, c("cyl", "mpg")]
mtcars[c("cyl", "mpg")]

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

mtcars[, c("am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?

mtcars$mpg #wektor
mtcars["mpg"] #ramka
mtcars[, "mpg"] #wektor
var <- "mpg"
mtcars[var]


# Uwaga na przecinek i wybór kolumn poprzez indeksy

mtcars[c(1,2)] #1 i 2 kolumna
mtcars[1,2]

# Pytania

# 1. Wymiar ramki danych

dim(mtcars)

# 2. Jakie są typy zmiennych?

str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?

length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?

mean(mtcars[mtcars$cyl == 4,"drat"])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 

unique(mtcars$am)
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

str(employees)
head(employees)
employees[employees$name == "John",]

proton(action = "login", login = "johnins")

class(top1000passwords)
head(top1000passwords)
top1000passwords[[1]]
for (i in 1:1000){
  print(i)
  proton(action = "login", login = "johnins", password = top1000passwords[i])
  
}

password <- top1000passwords[120]

proton(action = "login", login = "johnins", password = password)

str(logs)
head(logs)

logs[logs$login == "t.pietraszko",]
length(letters)

for (i in 1:26){
  logs[logs$login == as.character(letters[i]) + ".pietraszko",]
}
?as.character


## 5) Umieszczamy rozwiązanie na repozytorium.














