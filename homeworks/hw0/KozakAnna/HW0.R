# Przykładowe rozwiązanie

## Etap 1

employees[employees$surname == "Insecure", ]

proton(action = "login", login = "johnins")

## Etap 2

top1000passwords #wektor

for (pass in top1000passwords) {
  response <- proton(action = "login", login = "johnins", password = pass)
  if (response == 'Success! User is logged in!'){
    cat(pass)
  }
}

## Etap 3

employees[employees$surname == "Pietraszko",]

table(logs[logs$login == "slap", c("host") ]) -> tmp
data.frame(tmp)

proton(action = "server", host = "194.29.178.16")


## Etap 4

bash_history #lista

split_bash_history <- strsplit(bash_history, " ") # rozdziela napisy względem podanego znaku, tutaj względem spacji " "

commands <-  c()

for (x in split_bash_history){ # wybieramy pierwszy napis po podzieleniu komendy z bash_history
  commands <- c(commands, x[[1]])
}

for (command in unique(commands)){ # wykonujemy w pętli logowanie używając po kolei wartości commands
  proton(action = "login", login = "slap", password = command)
}

# Inna opcja rozwiązania, nie brute-force
table(commands) # sprawdzamy stringa, który wygląda na hasło (użyty tylko 1 raz)
proton(action = "login", login = "slap", password = "DHbb7QXppuHnaXGN")
