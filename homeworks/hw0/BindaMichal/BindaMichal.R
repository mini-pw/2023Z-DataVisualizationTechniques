#PROBLEM 1 Find Login
length(employees$name)

for (i in 1:541){
  if (employees[i,"name"] == "John"){
    if (employees[i,"surname"] == "Insecure"){
      x = i
    }
  }
}
x # John jest w rzedzie 217
#John's login:
employees[217,"login"]
proton(action = "login", login="johnins")


#PROBLEM 2 Find Password

for(i in 1:1000){
  
  if(proton(action = "login", login="johnins", password = top1000passwords[i]) == "Success! User is logged in!"){
    y = i
  }
}
top1000passwords[y]
proton(action = "login", login="johnins", password = "q1w2e3r4t5")

#PROBLEM 3 Check from which server Pietraszko logs into the Proton server most often.
View(logs)
for (i in 1:541){
  if (employees[i,"surname"] == "Pietraszko"){
    x = i
  }
}
x
employees[477,]
#login Pietraszko to "slap"
logs[1,3]
logs[58922,]
length(logs$login)
View(logs)
for (i in 1:59366){
  if(logs[i,1] == "slap"){
    var = logs[i,]
    x = i
  } 
} 
x
View(var)
# host to "193.0.96.13.20"
proton(action = "server", host="193.0.96.13.20")

