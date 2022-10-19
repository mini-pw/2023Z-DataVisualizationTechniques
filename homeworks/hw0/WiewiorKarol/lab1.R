View(employees)
employees[ employees$surname == "Insecure",]
for(i in 1:length(top1000passwords)){
  proton(action = "login", login="johnins", password=top1000passwords[i])
}
employees[ employees$surname == "Pietraszko",]
data.frame(table(logs[ logs$login == "slap",]$host))
proton(action = "server", host="194.29.178.16")
View(bash_history)
for(i in 1:length(bash_history)){
  if(length(strsplit(bash_history[i]," ")[[1]]) == 1){
    proton(action="login", login="slap", password=bash_history[i][1])
  }
}
