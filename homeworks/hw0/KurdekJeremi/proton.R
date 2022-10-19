install.packages("proton")
library(proton)
proton()

head(employees)

employees[employees$surname == "Insecure", ]

proton(action = "login", login="johnins")

for (pass in top1000passwords){
  proton(action = "login", login="johnins", password=pass)
}

employees[employees$surname == "Pietraszko",]

head(logs)
sort(table(logs[logs$login == "slap", "host"]), decreasing=TRUE)[1]

proton(action = "server", host="194.29.178.16")

for(command in bash_history){
  zx<-strsplit(command, " ")
  proton(action = "login", login="slap", password=zx[[1]][1])
}