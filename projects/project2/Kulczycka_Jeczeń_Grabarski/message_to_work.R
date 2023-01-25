merged_message <- read.csv("Magdalena Jeczeń_merged_message.csv", sep = ",") # tutaj dla kazdej z 3 ramek to zrobisz


merged_message %>% 
  filter(participants > 2) %>% 
  group_by(file) %>% 
  mutate(participants_grouped = participants) %>% 
  summarise(p_of_my_mess = (sum(my_messages)/sum(all_messages))*100, participants, participants_grouped) %>% 
  head(20)

merged_message_to_work <- merged_message
merged_message_to_work <- merged_message_to_work %>% 
  filter(participants != 1) %>% 
  mutate(participants_grouped = participants) 
S

merged_message_to_work$participants <- as.numeric(merged_message_to_work$participants)
merged_message_to_work$participants <- as.integer(merged_message_to_work$participants)
typeof(merged_message_to_work$participants_grouped)
merged_message_to_work[merged_message_to_work$participants_grouped == 2, 'participants_grouped'] <- 2
merged_message_to_work[merged_message_to_work$participants_grouped >2 & merged_message_to_work$participants_grouped <=5, 'participants_grouped'] <- 3
merged_message_to_work[merged_message_to_work$participants_grouped >5 & merged_message_to_work$participants_grouped <=10, 'participants_grouped'] <- 6
merged_message_to_work[merged_message_to_work$participants_grouped >10 & merged_message_to_work$participants_grouped <=20, 'participants_grouped'] <- 10
merged_message_to_work[merged_message_to_work$participants_grouped >20 , 'participants_grouped'] <- 20

merged_message_to_work$participants_grouped <- as.character(merged_message_to_work$participants_grouped)
typeof(merged_message_to_work$participants_grouped)

merged_message_to_work[merged_message_to_work$participants_grouped == "2", 'participants_grouped'] <- "2"
merged_message_to_work[merged_message_to_work$participants_grouped == "3", 'participants_grouped'] <- "[3,5]"
merged_message_to_work[merged_message_to_work$participants_grouped == "6", 'participants_grouped'] <- "[6,10]"
merged_message_to_work[merged_message_to_work$participants_grouped == "10", 'participants_grouped'] <- "[11,20]"
merged_message_to_work[merged_message_to_work$participants_grouped == "20", 'participants_grouped'] <- ">20"
write.csv(merged_message_to_work, "D:\\IAD\\sem3\\wizualizacja danych\\projekt2\\magda_merged_message_to_work.csv", row.names=FALSE)
S