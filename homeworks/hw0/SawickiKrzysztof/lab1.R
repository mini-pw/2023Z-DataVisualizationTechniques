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
dim(mtcars)
nrow(mtcars)
ncol(mtcars)
str(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[2:3,4:5]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
head(mtcars[,2:3],10)
# Jak wybieramy kolumny po nazwach? 

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?

# Jak wybierać jedną kolumnę?
mtcars$mpg

# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych

  dim(mtcars)

# 2. Jakie są typy zmiennych?

  ?typeof
  numeric, string, nan
  str(mtcars)
  
# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
  
  unique(mtcars$cyl)

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
  
  mean(mtcars[mtcars$cyl == 4, c("drat")])
  
# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)?
  
  unique(mtcars$am)
  table(mtcars$am)
  
# Prosty wykres

# Zależność "mpg" i "hp" - scatter plot

plot(mtcars$mpg, mtcars$hp)

# Zmienna "cyl" - barplot

barplot(table(mtcars$cyl))
hist(mtcars$cyl)

## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()


data(employees)
employees[employees$name == "John",]

proton(action = "login", login="johnins")


data(top1000passwords)
head(top1000passwords)
length(top1000passwords)
unique(top1000passwords)
top1000passwords[217]

proton(action = "login", login="johnins", password="john")

for(i in 1:1000){
  if (proton(action = "login", login="johnins", password= top1000passwords[i]) == `Success! User is logged in!`){
    print(5)
  }
  
}

for(i in 1:1000){proton(action = "login", login="johnins", password= top1000passwords[i])}

data(logs)
head(logs)
employees[employees$surname == "Pietraszko",2]
slap
head(logs[logs$login == "slap",2])
head(table(as.character(logs[logs$login == "slap",2])))
proton(action = "server", host= 194.29.178.16)

## 5) Umieszczamy rozwiązanie na repozytorium.