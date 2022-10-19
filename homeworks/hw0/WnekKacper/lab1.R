###########################################
###    TECHNIKI WIZUALIZACJI DANYCH     ###
###           LABORATORIUM 1            ###
###########################################

## 0) Prowadz¹cy
# Anna Kozak/Mateusz Krzyziñski/Hubert Ruczyñski/Miko³aj Spytek 
# Kontakt: MS Teams lub mail
# anna.kozak@pw.edu.pl/mateusz.krzyzinski.stud@pw.edu.pl/hruczynski21@interia.pl/mikolaj.spytek.stud@pw.edu.pl

## 1) Sposób pracy na zajêciach laboratoryjnych
# a) pracujemy w R (wiêkszoœæ semestru) i Python
# b) pracujemy na przygotowanych plikach, które bêd¹ na repozytorium przedmiotu
# c) podczas zajêæ prowadz¹cy bêdzie wprowadza³ zagdanienia, a nastêpnie bêdzie rozwi¹zywanie zadañ w celu utrwalenia wiadomoœci
# d) kolejna porcja materia³u bêdzie omawiana je¿eli wiêkszoœæ grupy wykona zadane zadanie 
# e) wszelkie pytania czy to zwi¹zane z kodem, prac¹ domow¹ czy kwestie teoretyczne proszê œmia³o zg³aszaæ prowadz¹cemu 

## 2) Materia³y
# Repozytorium na GitHub
# https://github.com/mini-pw/2023Z-DataVisualizationTechniques 

## 3) Jak dzia³a GitHub?
# Jak zg³osiæ pracê domow¹/projekt? (fork, commit, pull request)
# https://rogerdudler.github.io/git-guide/

## 4) Podstawy R - rozgrzewka 

data(mtcars)
head(mtcars)

?mtcars


# Jak wybieramy wiersze (obserwacje) oraz kolumny (zmienne)?
mtcars[c(2,3),c(4,5)]
mtcars[2:3,4:5]
# Pierwszy wiersz, pierwsza kolumna?
mtcars[1,1]
# 10 pierszych wierszy, 2 i 3 kolumna?
mtcars[1:10,2:3]
# Jak wybieramy kolumny po nazwach? 
mtcars['mpg'] #ramka danych
mtcars[,'mpg'] #wektor
mtcars[,c('mpg','cyl')]
mtcars[c('mpg','cyl')]
mtcars[,c('cyl','mpg')]
# Wszystkie wiersze i kolumny w kolejnoœci "am", "wt", "mpg"?
mtcars[,c('am','wt','mpg')]
# Jak wybieraæ jedn¹ kolumnê?
mtcars$mpg #wektor
# Uwaga na przecinek i wybór kolumn poprzez indeksy

# Pytania

# 1. Wymiar ramki danych
dim(mtcars)
# 2. Jakie s¹ typy zmiennych?
typeof(mtcars)
# 3. Ile jest unikalnych wartoœci zmiennej "cyl" i jakie to s¹ wartoœci?
unique(mtcars$cyl)
length(unique(mtcars$cyl))
# 4. Jaka jest œrednia wartoœæ zmiennej "drat" dla samochodów o wartoœci zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4, c("drat")])
median(mtcars[mtcars$cyl==4, c("drat")])
# 5. Jakie s¹ unikalne wartoœci zmiennej "am" i jaki jest ich rozk³ad (liczba wyst¹pieñ)? 
unique(mtcars$am)
# Prosty wykres

# Zale¿noœæ "mpg" i "hp" - scatter plot



# Zmienna "cyl" - barplot


## 4) Gra proton, nale¿y stworzyæ plik R z kodami do rozwi¹zania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
data.frame(employees)
employees[employees$surname =='Insecure',c('login')]
proton()
proton(action="login", login='johnins')
top1000passwords
for(i in 1:1000){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
logs
logs[logs$login=='s.pietraszko', c('host')]
unique(logs[logs$login=='s.pietraszko', c('host')])
x<-table(unique(logs[logs$login=='s.pietraszko', c('host')]))
for(i in 1:312){
  proton(action = "server", host=x[i])
}
## 5) Umieszczamy rozwi¹zanie na repozytorium.
