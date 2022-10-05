head(employees)
employees[employees$name=='John',]
proton(action="login", login = "johnins")

####

head(top1000passwords)
proton(action="login", login = "johnins", password = top1000passwords)

####

for(i in 1:1000){
  proton(action="login", login = "johnins", password = top1000passwords[i])
}

####
head(logs)
employees[employees$surname == "Pietraszko",]
temp <- data.frame(table(logs[logs$login == 'slap',]$host))
head(temp[temp$Freq != 0,])

proton(action = 'server', host="194.29.178.16")

####

head(bash_history)

temp2 <- strsplit(bash_history," ")
for (i in 1:length(temp2)) {
  proton(action="login",login="slap",password=temp2[[i]][[1]])
}


