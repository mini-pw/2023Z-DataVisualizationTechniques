install.packages("proton")
library(proton)
proton()

employees[employees$surname == "Insecure", ]$login
proton(action = "login", login=employees[employees$surname == "Insecure", ]$login)

login=employees[employees$surname == "Insecure", ]$login -> J_login

for(i in 1:1000){
  proton(action = "login", login=J_login, password=top1000passwords[i])
  
}


employees[employees$surname == "Pietraszko", ]$login -> P_login

data.frame(table(logs[logs$login == P_login,]$host)) -> df

proton(action = "server", host="194.29.178.16")

bash_history
strsplit(bash_history," ") -> bash

length(bash[[3]])      
head(bash)
length(bash[[3]])
bash[[3]]

for(i in 1:19913){
  
   if(length(bash[[i]]) == 1){
     
     proton(action = "login", login=P_login, password = bash[[i]])
   }
  
}
