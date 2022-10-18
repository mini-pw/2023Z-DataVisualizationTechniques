library(dplyr)
library(DescTools) 


df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartoœci NA w ¿adnej kolumnie
View(df)
# 1. Jaka jest œrednia cena nieruchomoœci z liczb¹ ³azienek powy¿ej mediany i po³o¿onych na wschód od po³udnika 122W?
df %>% 
  filter(bathrooms > median(bathrooms), long > -122) %>% 
  summarise(mean_price = mean(price))


# Odp: 625499.4

# 2. W którym roku zbudowano najwiêcej nieruchomoœci?
df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  select(yr_built) %>% 
  head(1)


# Odp: 2014

# 3. O ile procent wiêksza jest mediana ceny budynków po³o¿onych nad wod¹ w porównaniu z tymi po³o¿onymi nie nad wod¹?
df %>% 
  group_by(waterfront) %>% 
  summarise(median_price = median(price)) 
(1400000 - 450000)/450000 * 100
  
  

# Odp: 211.1111%

# 4. Jaka jest œrednia powierzchnia wnêtrza mieszkania dla najtañszych nieruchomoœci posiadaj¹cych 1 piêtro (tylko parter) wybudowanych w ka¿dym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  summarise(mean_sqft = mean(sqft_living))


# Odp: 1030

# 5. Czy jest ró¿nica w wartoœci pierwszego i trzeciego kwartyla jakoœci wykoñczenia pomieszczeñ pomiêdzy nieruchomoœciami z jedn¹ i dwoma ³azienkami? Jeœli tak, to jak ró¿ni siê Q1, a jak Q3 dla tych typów nieruchomoœci?
df %>% 
  filter(bathrooms %in% c(1, 2)) %>% 
  group_by(bathrooms) %>% 
  summarise(first = quantile(grade,0.25), third = quantile(grade,0.75))

# Odp: Jest Q1 ró¿ni siê o 1 i Q3 równie¿ o 1

# 6. Jaki jest odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy a jaki tych na po³udniu? (Pó³noc i po³udnie definiujemy jako po³o¿enie odpowiednio powy¿ej i poni¿ej punktu znajduj¹cego siê w po³owie miêdzy najmniejsz¹ i najwiêksz¹ szerokoœci¹ geograficzn¹ w zbiorze danych)

df %>% 
  mutate(north = ifelse(lat > (max(df$lat)+min(df$lat))/2, T, F)) %>% 
  group_by(north) %>% 
  summarise(interquartile_range = IQR(price))
  

# Odp: Po³udnie: 122500, Pó³noc: 321000 

# 7. Jaka liczba ³azienek wystêpuje najczêœciej i najrzadziej w nieruchomoœciach niepo³o¿onych nad wod¹, których powierzchnia wewnêtrzna na kondygnacjê nie przekracza 1800 sqft?
df %>% 
  filter(waterfront == 0, sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  View()

# Odp: Najrzadziej: 4.75, Najczêsciej: 2.5

# 8. ZnajdŸ kody pocztowe, w których znajduje siê ponad 550 nieruchomoœci. Dla ka¿dego z nich podaj odchylenie standardowe powierzchni dzia³ki oraz najpopularniejsz¹ liczbê ³azienek


df %>% 
  group_by(zipcode) %>% 
  summarise(n = n(),
            standard_deviation = sd(sqft_lot),
            mode_bathrooms = Mode(bathrooms)) %>% 
  filter(n > 550) %>% 
  arrange(n) %>% 
  select(-n) %>% 
  View()
  

# Odp: zipcode  odchylenie  moda 
#   1   98117   2318.662    1.0
#   2   98052   10276.188   2.5
#   3   98115   2675.302    1.0
#   4   98038   63111.112   2.5
#   5   98103   1832.009    1.0

# 9. Porównaj œredni¹ oraz medianê ceny nieruchomoœci, których powierzchnia mieszkalna znajduje siê w przedzia³ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj¹cych siê przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  mutate(size = case_when(sqft_living <= 2000 ~"small",
                          sqft_living <= 4000 ~"medium",
                          TRUE ~"big")) %>% 
  group_by(size) %>% 
  summarise(mean_price = mean(price), median_price = median(price)) %>% 
  View()

# Odp:  Rozmiar   Œrednia     Mediana
#   1   Du¿e      1448118.8   1262750
#   2   Œrednie   645419.0    595000
#   3   Ma³e      385084.3    359000

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomoœci? (bierzemy pod uwagê tylko powierzchniê wewn¹trz mieszkania)
df %>% 
  summarise(min_price = min(price/(sqft_living * 0.09290304)))

# Odp: 942.7919
