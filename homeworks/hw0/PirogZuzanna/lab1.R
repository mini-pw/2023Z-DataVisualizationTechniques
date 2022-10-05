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

data(mtcars) #wczytujemy sobie fajnie
head(mtcars)

?mtcars

head(mtcars, 5) # ile wierszy chcemy
dim(mtcars)
nrow(mtcars)
ncol(mtcars)
str(mtcars)

# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?

mtcars[2:3, 4:5]

# Pierwszy wiersz, pierwsza kolumna?
mtcars[1, 1]

# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10, 2:3]
head(mtcars[,2:3])

# Jak wybieramy kolumny po nazwach? 

mtcars[,c("cyl", "mpg")]
mtcars[,c( "mpg", "cyl")] #kolejnosc ma znaczenie
mtcars[c( "mpg", "cyl")] #nie musi byc przecinka

# Wszystkie wiersze i kolumny w kolejności "am", "wt", "mpg"?
mtcars[,c( "am", "wt", "mpg")]

# Jak wybierać jedną kolumnę?
mtcars$mpg #otrzymujemy jako wektor
mtcars[,"mpg"] #otrzymujemy jako wektor
#!!!!!! UWAGA !!!!!!!
mtcars["mpg"] #otrzymujemy ramke danych

temp <- "mpg" # do temp nie odniesiemy sie po dolarze, tylko tak
mtcars[,temp]

# Uwaga na przecinek i wybór kolumn poprzez indeksy




# Pytania

# 1. Wymiar ramki danych
dim(mtcars)

# 2. Jakie są typy zmiennych?
str(mtcars)

# 3. Ile jest unikalnych wartości zmiennej "cyl" i jakie to są wartości?
unique(mtcars["cyl"])
unique(mtcars$cyl)
#ile ich jest
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mask <- mtcars$cyl == 4
mean(mtcars[mask, "drat"])
# mean, median, sd - odchylenie standardowe, min, max

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
unique(mtcars$am)
table(mtcars$am) #ile razy dana rzecz wystapila 

# Prosty wykres
plot(mtcars$mgp, mtcars$hp)

# Zależność "mpg" i "hp" - scatter plot


# Zmienna "cyl" - barplot
barplot(table(mtcars$cyl))
hist(table(mtcars$cyl))
## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
data("employees")
head(employees)
employees[employees$surname == "Insecure", "login"]



proton(action = "login", login="johnins")
data("top1000passwords")
top1000passwords
for (i in 1 : length(top1000passwords)) {
  if((proton(action = "login", login="johnins", password= "")) == "Success! User is logged in!"){
    print()
  }
}
for (i in 1 : length(top1000passwords)){
  proton(action = "login", login="XYZ", password=top1000passwords[i])
}

## 5) Umieszczamy rozwiązanie na repozytorium.