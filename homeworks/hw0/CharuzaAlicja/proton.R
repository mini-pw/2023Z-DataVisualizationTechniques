install.packages("proton")
library(proton)
proton()
data("employees")

employees[employees$name == "John" & employees$surname == "Insecure", "login"]
proton(action = "login", login = "johnins")

data("top1000passwords")
top1000passwords

fun <- function(password_check) {
  proton(action ="login", login = "johnins", password = password_check)
}
lapply(top1000passwords, fun)

data(logs)
employees[employees$surname == "Pietraszko", "login"]
table(logs[logs$login == "slap", "host"]) -> hosts
hosts <- as.data.frame(hosts)
hosts <- hosts[order(hosts$Freq, decreasing = TRUE), ]