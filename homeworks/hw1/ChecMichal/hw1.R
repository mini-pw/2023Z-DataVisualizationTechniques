library(dplyr)
library(tidyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartoœci NA w ¿adnej kolumnie

# 1. Jaka jest œrednia cena nieruchomoœci z liczb¹ ³azienek powy¿ej mediany i po³o¿onych na wschód od po³udnika 122W?


# Odp:

temp_1 <- df %>% 
  filter(bathrooms > median(df$bathrooms), long > -122) %>% 
  select(price)
mean(temp_1$price)

# 625499.4$ 

# 2. W którym roku zbudowano najwiêcej nieruchomoœci?


# Odp:

df %>% 
  group_by(yr_built) %>% 
  count() %>% 
  arrange(-n) %>% 
  head(1)

# W 2014 - zbudowano wtedy 559

# 3. O ile procent wiêksza jest mediana ceny budynków po³o¿onych nad wod¹ w porównaniu z tymi po³o¿onymi nie nad wod¹?


# Odp:
  
water <- df %>% 
  filter(waterfront == 1) %>% 
  select(price)

no_water <- df %>% 
  filter(waterfront == 0) %>% 
  select(price)

median(water$price)/median(no_water$price)*100 - 100

# wzros³o o 211,11%

# 4. Jaka jest œrednia powierzchnia wnêtrza mieszkania dla najtañszych nieruchomoœci 
# posiadaj¹cych 1 piêtro (tylko parter) wybudowanych w ka¿dym roku?


# Odp:


temp1 <- df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  top_n(1 ,-price) 

mean(temp1$sqft_living)  
  
  
  
# 1030.42



# 5. Czy jest ró¿nica w wartoœci pierwszego i trzeciego kwartyla jakoœci 
# wykoñczenia pomieszczeñ pomiêdzy nieruchomoœciami z jedn¹ i dwoma ³azienkami? 
# Jeœli tak, to jak ró¿ni siê Q1, a jak Q3 dla tych typów nieruchomoœci?

# Odp:

omg <- df %>% 
  filter(bathrooms == 1) %>% 
  select(grade) 

quantile(omg$grade)


omg2 <- df %>% 
  filter(bathrooms == 2) %>% 
  select(grade) 

quantile(omg2$grade)

# Jest ró¿nica w Q1 i Q3, dwu³azienkowe nieruchomoœci maj¹ wiêksz¹ wartoœæ jakoœci o 1 w obu przypadkach



# 6. Jaki jest odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy a jaki tych na po³udniu? 
# (Pó³noc i po³udnie definiujemy jako po³o¿enie odpowiednio powy¿ej i poni¿ej punktu znajduj¹cego siê w po³owie miêdzy najmniejsz¹ i najwiêksz¹ szerokoœci¹ geograficzn¹ w zbiorze danych)


# Odp:

mid = abs(min(df$lat)/2 + max(df$lat)/2)

north <- df %>% 
  filter(lat > mid) %>% 
  select(price)

# Pó³noc 321000
IQR(north$price)

south <- df %>% 
  filter(lat < mid) %>% 
  select(price)

# Po³udnie 122500
IQR(south$price)


# 7. Jaka liczba ³azienek wystêpuje najczêœciej i najrzadziej w nieruchomoœciach 
#niepo³o¿onych nad wod¹, których powierzchnia wewnêtrzna na kondygnacjê nie przekracza 1800 sqft?

# Odp:


df %>% 
  filter(waterfront == 0, sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  count() %>% 
  arrange(-n) %>% 
  select(bathrooms) %>% 
  head(1)

# najczêœciej 2.5


df %>% 
  filter(waterfront == 0, sqft_living/floors <= 1800) %>% 
  group_by(bathrooms) %>% 
  count() %>% 
  arrange(n) %>% 
  select(bathrooms) %>% 
  head(1)

# najrzadziej 4.75



# 8. ZnajdŸ kody pocztowe, w których znajduje siê ponad 550 nieruchomoœci. 
#Dla ka¿dego z nich podaj odchylenie standardowe powierzchni dzia³ki oraz najpopularniejsz¹ 
#liczbê ³azienek

# Odp:

temp3 <- df %>% 
  group_by(zipcode) %>% 
  count() %>% 
  filter(n > 550) 

odchylenie_standardowe <- df %>% 
  filter(zipcode == temp3$zipcode) %>% 
  group_by(zipcode) %>% 
  summarise(sd(sqft_lot))


temp4 <- df %>% 
  filter(zipcode == temp3$zipcode) %>% 
  select(zipcode, bathrooms) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(n()) %>%
  arrange(-`n()`) %>% 
  select(zipcode, bathrooms) %>% 
  head(5)
  

colnames(temp4) <- c("zipcode", "max_bathrooms")


temp4 
odchylenie_standardowe
# zipcode max_bathrooms

# 98038           2.5
# 98117           1  
# 98052           2.5
# 98115           1  
# 98103           1    

#zipcode  odchylenie standardowe

# 98038         43221.
# 98052          6396.
# 98103          1749.
# 98115          2787.
# 98117          1789.





# 9. Porównaj œredni¹ oraz medianê ceny nieruchomoœci, których powierzchnia mieszkalna 
# znajduje siê w przedzia³ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, 
# nieznajduj¹cych siê przy wodzie.

# Odp:


to_2k <- df %>% 
  filter(sqft_living <= 2000, waterfront == 0) %>% 
  select(price)

from_2k_to_4k <- df %>% 
  filter(sqft_living <= 4000, sqft_living > 2000, waterfront == 0) %>% 
  select(price)

from_4k <- df %>% 
  filter(sqft_living > 4000,waterfront == 0) %>% 
  select(price)

res <- data.frame()
res_2 <- rbind(res,c("0-2000", mean(to_2k$price), median(to_2k$price)))
res_3 <- rbind(res_2, c("2000-4000", mean(from_2k_to_4k$price), median(from_2k_to_4k$price)))
res_final <- rbind(res_3, c("4000-inf", mean(from_4k$price), median(from_4k$price)))

colnames(res_final) <- c("rozmiar", "srednia", "mediana")
res_final

# rozmiar      srednia    mediana
# 1 0-2000     385084.32      359000
# 2 2000-4000  645419.04    595000
# 3 4000-inf   1448118.757  1262750

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomoœci? 
#(bierzemy pod uwagê tylko powierzchniê wewn¹trz mieszkania)


# Odp:

df %>% 
  mutate(cena_za_metr = price/(sqft_living*0.09290304)) %>% 
  select(cena_za_metr) %>% 
  arrange(cena_za_metr) %>% 
  head(1)

# najmniejsza cena to 942.79 $/m^2






