library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartosci NA w zadnej kolumnie

# 1. Jaka jest srednia cena nieruchomosci z liczba lazienek powyzej mediany 
# i polozonych na wschod od poludnika 122W?


df %>% 
  filter(bathrooms > median(bathrooms), long > -122) %>%
  summarise(mean_price = mean(price))

# Odp: 625499.4

# 2. W którym roku zbudowano najwiêcej nieruchomoœci?


df %>% 
  select(yr_built) %>%
  count(yr_built) %>%
  arrange(-n) %>%
  top_n(1)

# Odp: 2014


# 3. O ile procent wiêksza jest mediana ceny budynków po³o¿onych nad wod¹ w porównaniu z tymi po³o¿onymi nie nad wod¹?


df %>%
  group_by(waterfront) %>%
  summarize(median_price = median(price)) %>%
  mutate(percentage = (median_price/min(median_price) - 1)*100) %>%
  top_n(1, percentage)

# Odp: o 211%

# 4. Jaka jest œrednia powierzchnia wnêtrza mieszkania dla najtañszych nieruchomoœci posiadaj¹cych 1 piêtro (tylko parter) wybudowanych w ka¿dym roku?


df %>%
  filter(floors == 1) %>%
  select(sqft_living, yr_built, price) %>%
  group_by(yr_built) %>% 
  filter(price == min(price)) %>%
  ungroup() %>% 
  summarise(mean_sqft= mean(sqft_living))
  

# Odp: 1030
  

# 5. Czy jest ró¿nica w wartoœci pierwszego i trzeciego kwartyla jakoœci wykoñczenia pomieszczeñ pomiêdzy nieruchomoœciami z jedn¹ i dwoma ³azienkami?
# Jeœli tak, to jak ró¿ni siê Q1, a jak Q3 dla tych typów nieruchomoœci

df %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>%
  select(bathrooms, grade) %>%
  group_by(bathrooms) %>%
  summarize(quant1 = quantile(grade, probs = .25),
            quant3 = quantile(grade, probs = .75))

  
# Odp: Zarowno dla pierwszego jak i trzeciego kwantyla wartosc jest o 1 wyzsza dla nieruchomoci z dwiema lazienkami


# 6.  Jaki jest odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy a jaki tych na po³udniu? (Pó³noc i po³udnie definiujemy jako po³o¿enie odpowiednio powy¿ej i poni¿ej punktu 
# znajdujacego sie w polowie miedzy najmniejsza i najwieksza szerokoscia geograficzna w zbiorze danych)

df %>%
  select(price, lat) %>%
  mutate(polozenie = ifelse(lat <= ((max(lat)+min(lat))/2), "S", "N")) %>%
  group_by(polozenie) %>%
  summarize(quant1 = quantile(price, probs = .25),
            quant3 = quantile(price, probs = .75),
            diff = quant3 - quant1)
  

# Odp: Odstep miedzykwartylowy dla mieszka na polnocy: 321000, na poludniu: 122500

# 7. Jaka liczba ³azienek wystêpuje najczêœciej i najrzadziej w nieruchomoœciach niepo³o¿onych nad wod¹, których powierzchnia wewnêtrzna na kondygnacjê nie przekracza 1800 sqft?

df %>%
  filter(waterfront == 0, sqft_living/floors <= 1800) %>%
  select(bathrooms) %>%
  group_by(bathrooms) %>%
  summarize(n = n()) %>%
  arrange(n) %>%
  filter(row_number() == 1 | row_number() == n())
  
  
  


# Odp: Najrzadziej: 4.75, najczesciej: 2.5

# 8. ZnajdŸ kody pocztowe, w których znajduje siê ponad 550 nieruchomoœci. Dla ka¿dego z nich podaj odchylenie standardowe powierzchni dzia³ki oraz najpopularniejsz¹ liczbê ³azienek

df %>%
  select(zipcode, bathrooms, sqft_lot) %>%
  group_by(zipcode) %>%
  mutate(n = n())%>%
  filter(n > 550) %>% 
  summarise(sd_sqft = sd(sqft_lot),
            common_bath = names(which.max(table(bathrooms))))
  

  
      

# Odp: 
# kod, odchylenie, lazienki,
#1   98038  63111. 2.5        
#2   98052  10276. 2.5        
#3   98103   1832. 1          
#4   98115   2675. 1          
#5   98117   2319. 1  

# 9. Porównaj œredni¹ oraz medianê ceny nieruchomoœci, których powierzchnia mieszkalna znajduje siê w przedzia³ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj¹cych siê przy wodzie.

df %>%
  filter(waterfront == 0) %>%
  select(sqft_living, price) %>%
  mutate(size = case_when(sqft_living <= 2000 ~ "Small",
                          sqft_living > 4000 ~ "Big",
                          TRUE ~ "Medium")) %>%
  group_by(size) %>%
  summarize(mean = mean(price),
            median = median(price))
  


# Odp: Dla mieszkan (0, 2000]: srednia - 385084, Mediana - 359000
# Dla mieszkan (2000,4000]: srednia - 645419, Mediana - 595000
# Dla mieszkan (4000, +Inf): srednia - 1448119, Mediana - 1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomoœci? (bierzemy pod uwagê tylko powierzchniê wewn¹trz mieszkania)

df %>%
  select(price, sqft_living) %>%
  mutate(sqm = sqft_living*0.09290304) %>%
  summarize(min_price = min(price/sqm))
  
# Odp: 942.7919