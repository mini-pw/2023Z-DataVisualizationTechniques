install.packages("proton")
library(proton)
proton()

# zadanie 1
data("employees")
employees[employees$name == "John", "login"]

proton(action = "login", login="johnins")


# zadanie 2
for(i in 1:1000) {
  
  proton(action = "login", login="johnins", password=top1000passwords[i])
  
}

# zadanie 3
data(logs)
employees[employees$surname == "Pietraszko", "login"]
table(logs[logs$login == "slap", "host"])
proton(action = "server", host="194.29.178.16")


# zadanie 4
bash_history

for(i in 1:length(bash_history)) {
  
  proton(action = "login", login="slap", password = strsplit(bash_history, " ")[[i]][1])
  
}


