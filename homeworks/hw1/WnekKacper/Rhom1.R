library(dplyr)
getwd()
df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartoœci NA w ¿adnej kolumnie

# 1. Jaka jest œrednia cena nieruchomoœci z liczb¹ ³azienek powy¿ej mediany i po³o¿onych na wschód od po³udnika 122W?
df%>%
  filter(bathrooms> median(bathrooms, na.rm=T) & long>-122)%>%
  select(price)%>%
  summarise(cena=mean(price))
  


# Odp: 625499.4

# 2. W którym roku zbudowano najwiêcej nieruchomoœci?
df %>% 
  group_by(yr_built) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  select(yr_built) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent wiêksza jest mediana ceny budynków po³o¿onych nad wod¹ w porównaniu z tymi po³o¿onymi nie nad wod¹?
df %>% 
  filter(waterfront==1) %>% 
  summarise(median_waterfront=median(price))-> median_waterfront

df %>% 
  filter(waterfront==0) %>% 
  summarise(median_waterfront0=median(price))-> median_waterfront0
# Odp:
((median_waterfront-median_waterfront0)/median_waterfront0)*100

# 4. Jaka jest œrednia powierzchnia wnêtrza mieszkania dla najtañszych nieruchomoœci posiadaj¹cych 1 piêtro (tylko parter) wybudowanych w ka¿dym roku?
df %>%
  filter(floors == 1) %>%
  select(price, yr_built, sqft_living) %>%
  group_by(yr_built) %>%
  top_n(-1, price) %>% 
  ungroup() %>%
  summarise(var_4 = mean(sqft_living))
  

  

# Odp: 1030

# 5. Czy jest ró¿nica w wartoœci pierwszego i trzeciego kwartyla jakoœci wykoñczenia pomieszczeñ pomiêdzy nieruchomoœciami z jedn¹ i dwoma ³azienkami? Jeœli tak, to jak ró¿ni siê Q1, a jak Q3 dla tych typów nieruchomoœci?
df %>% 
  filter(bathrooms==1 | bathrooms==2) %>% 
  arrange(grade) %>% 
  summarise(var=quantile(grade, c(0.25, 0.75)))-> var_1
  names(var_1$var) <- NA
  var_1$var[2]-var_1$var[1]


# Odp: Ro¿ni siê o 1

# 6. Jaki jest odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy a jaki tych na po³udniu? (Pó³noc i po³udnie definiujemy jako po³o¿enie odpowiednio powy¿ej i poni¿ej punktu znajduj¹cego siê w po³owie miêdzy najmniejsz¹ i najwiêksz¹ szerokoœci¹ geograficzn¹ w zbiorze danych)
df %>% 
  arrange(lat) %>% 
  select(lat) %>% 
  head(1)->var_top 

df %>% 
  arrange(lat) %>% 
  select(lat) %>% 
  tail(1)-> var_bottom

var_middle<- (var_top+var_bottom)/2

df %>% 
  select(price, lat) %>% 
  filter(lat>=var_middle$lat) %>% 
  summarise(varN=quantile(price, c(0.25, 0.75)))->varN

varN$varN[2]-varN$varN[1]

df %>% 
  select(price, lat) %>% 
  filter(lat<=var_middle$lat) %>% 
  summarise(varS=quantile(price, c(0.25, 0.75)))->varS
  
varS$varS[2]-varS$varS[1]
# Odp: dla polnocy 321000  dla poludnia 122500

# 7. Jaka liczba ³azienek wystêpuje najczêœciej i najrzadziej w nieruchomoœciach niepo³o¿onych nad wod¹, których powierzchnia wewnêtrzna na kondygnacjê nie przekracza 1800 sqft?
df %>% 
  filter(waterfront==0 & sqft_living/floors<=1800) %>% 
  select(bathrooms) %>% 
  group_by(bathrooms) %>% 
  summarise(var100=n()) %>% 
  arrange(var100)->varb
  head(varb$bathrooms,1) 
  tail(varb$bathrooms,1)
  
  



# Odp: najczesciej 2.5 a najrzadziej 4,75

# 8. ZnajdŸ kody pocztowe, w których znajduje siê ponad 550 nieruchomoœci. Dla ka¿dego z nich podaj odchylenie standardowe powierzchni dzia³ki oraz najpopularniejsz¹ liczbê ³azienek
  df %>% 
    group_by(zipcode) %>% 
    summarise(n=n()) %>% 
    filter(n>550) %>% 
    select(zipcode) -> var_codes
  
  var_codes<- var_codes$zipcode
  
  
  df %>% 
    filter(zipcode %in% (var_codes)) %>% 
    group_by(zipcode, bathrooms) %>% 
    summarise(n=n()) ->r1
  
  
  df %>% 
    filter(zipcode %in% (var_codes)) %>% 
    group_by(zipcode, bathrooms) %>% 
    summarise(n=n()) %>% 
    summarise(n= max(n)) ->r2
  
  
  inner_join(r1,r2)
  
  df %>% 
    filter(zipcode %in% (var_codes)) %>% 
    group_by(zipcode) %>% 
    select(zipcode, sqft_lot) %>% 
    summarise(sd(sqft_lot))
# Odp:

# 9. Porównaj œredni¹ oraz medianê ceny nieruchomoœci, których powierzchnia mieszkalna znajduje siê w przedzia³ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj¹cych siê przy wodzie.
df %>% 
  filter(waterfront == 0) %>% 
  mutate( var_9 = case_when(
    sqft_living <= 2000 ~ "(0, 2000]",
    sqft_living <= 4000 ~ "(2000, 4000]",
    TRUE ~ "(4000, Inf)"
  )) %>% 
  group_by(var_9) %>% 
  summarize(var_9srednia = mean(price), var_9mediana = median(price))

# Odp:

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomoœci? (bierzemy pod uwagê tylko powierzchniê wewn¹trz mieszkania)
df %>% 
  mutate(sqm_living = sqft_living * 0.09290304) %>% 
  mutate(var_10 = price / sqm_living) %>% 
  top_n(var_10, n = -1) %>% 
  select(var_10)

# Odp: 942.79