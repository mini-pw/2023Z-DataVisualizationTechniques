data(mtcars) #ładowanie ramki danych
head(mtcars)

?mtcars
dim(mtcars)
class(mtcars)
mode(mtcars)
str(mtcars)

#wiersze i kolumny
mtcars[c(1,2),4:5]
mtcars[, 5] # tylko 5 kolumna

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
length(unique(mtcars$cyl))

# 4. Jaka jest średnia wartość zmiennej "drat" dla samochodów o wartości zmiennej "cyl" równej 4?
mean(mtcars[mtcars$cyl==4, c('drat')])

# 5. Jakie są unikalne wartości zmiennej "am" i jaki jest ich rozkład (liczba wystąpień)? 
unique(mtcars$am)
table(mtcars$am)

# Prosty wykres
plot(mtcars$mpg, mtcars$hp)

# Zależność "mpg" i "hp" - scatter plot


# Zmienna "cyl" - barplot
barplot(mtcars$mpg)


## 4) Gra proton, należy stworzyć plik R z kodami do rozwiązania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
data("employees")
employees
#1
employees[employees$name == "John" & employees$surname =="Insecure", c('login')]
proton(action = "login", login="johnins")
top1000passwords[2]

#2
for (i in 1: 1000)
{
  proton(action = "login", login="johnins", password=top1000passwords[i])
}

#3
logs
employees[employees$surname =="Pietraszko", c('login')]
logs[logs$login=="slap",]
logs[logs$login=="slap",]$host

sort(table(logs[logs$login=="slap",]$host),decreasing=TRUE)[1]
proton(action = "server", host="194.29.178.16")
head(bash_history)
#4
for (i in bash_history){
  pass = strsplit(i," ")[[1]][1]
  proton(action = "login", login="slap", password = pass)
}
bash_history[grepl(x=bash_history, pattern = "\w")]
strsplit(bash_history[[1]]," ")[[1]][1]
