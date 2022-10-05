## 4) Gra proton, nalezy stworzyc plik R z kodami do rozwiazania gry (do 20 minut).

install.packages("proton")
library(proton)
proton()
head(employees)
emp <- employees

employees[employees$name == c("John"),]
proton(action = "login", login="johnins")
pass <- top1000passwords
for(i in 1:1000){
  proton(action = "login", login="johnins", password=pass[[i]])
}
proton(action = "login", login="johnins", password="password")


employees[employees$surname == c("Pietraszko"),]
logs[logs$login==c('slap'),c("host")]
proton(action = "server", host="194.29.178.16")


head(bash_history)
bh <- bash_history
k <-strsplit(bh, " ")
for(i in 1:length(k)){
  proton(action = "login", login="slap", password=k[[i]][[1]])
}
