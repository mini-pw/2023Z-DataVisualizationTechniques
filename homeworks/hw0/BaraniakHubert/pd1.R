employees[employees$surname == "Insecure","login"]
proton(action = "login", login="johnins")

for(passwd in top1000passwords){
  if(proton(action = "login", login="johnins", password=passwd) == "Success! User is logged in!")
    passwd
}

head(logs)
suspected_hosts = (logs[logs$login == employees[employees$surname == "Pietraszko", "login"],"host"])
hosts = data.frame(table(suspected_hosts))
proton(action = "server", host="194.29.178.16")
head(bash_history)
proton(action = "login", login="slap", password="lighttpd")
