install.packages("proton")
library(proton)
proton()

employees[employees$name=='John',]
View(employees)

proton(action='login', login='johnins')

for (x in top1000passwords) {
  proton(action = "login", login="johnins", password=x)
}

View(table(logs[logs$login=='slap','host']))


proton(action = "server", host="194.29.178.16")

View(bash_history)


x <- strsplit(bash_history, ' ')
commands <- sapply( x, "[", 1)
print(unique(commands))


DHbb7QXppuHnaXGN


proton(action = "login", login="slap", password='DHbb7QXppuHnaXGN')
