library(dplyr)
getwd()
df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma warto�ci NA w �adnej kolumnie

# 1. Jaka jest �rednia cena nieruchomo�ci z liczb� �azienek powy�ej mediany i po�o�onych na wsch�d od po�udnika 122W?
df%>%
  filter(bathrooms> median(bathrooms, na.rm=T) & long>-122)%>%
  select(price)%>%
  summarise(cena=mean(price))
  


# Odp: 625499.4

# 2. W kt�rym roku zbudowano najwi�cej nieruchomo�ci?
df %>% 
  group_by(yr_built) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  select(yr_built) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent wi�ksza jest mediana ceny budynk�w po�o�onych nad wod� w por�wnaniu z tymi po�o�onymi nie nad wod�?
df %>% 
  filter(waterfront==1) %>% 
  summarise(median_waterfront=median(price))-> median_waterfront

df %>% 
  filter(waterfront==0) %>% 
  summarise(median_waterfront0=median(price))-> median_waterfront0
# Odp:
((median_waterfront-median_waterfront0)/median_waterfront0)*100

# 4. Jaka jest �rednia powierzchnia wn�trza mieszkania dla najta�szych nieruchomo�ci posiadaj�cych 1 pi�tro (tylko parter) wybudowanych w ka�dym roku?
df %>%
  filter(floors == 1) %>%
  select(price, yr_built, sqft_living) %>%
  group_by(yr_built) %>%
  top_n(-1, price) %>% 
  ungroup() %>%
  summarise(var_4 = mean(sqft_living))
  

  

# Odp: 1030

# 5. Czy jest r�nica w warto�ci pierwszego i trzeciego kwartyla jako�ci wyko�czenia pomieszcze� pomi�dzy nieruchomo�ciami z jedn� i dwoma �azienkami? Je�li tak, to jak r�ni si� Q1, a jak Q3 dla tych typ�w nieruchomo�ci?
df %>% 
  filter(bathrooms==1 | bathrooms==2) %>% 
  arrange(grade) %>% 
  summarise(var=quantile(grade, c(0.25, 0.75)))-> var_1
  names(var_1$var) <- NA
  var_1$var[2]-var_1$var[1]


# Odp: Ro�ni si� o 1

# 6. Jaki jest odst�p mi�dzykwartylowy ceny mieszka� po�o�onych na p�nocy a jaki tych na po�udniu? (P�noc i po�udnie definiujemy jako po�o�enie odpowiednio powy�ej i poni�ej punktu znajduj�cego si� w po�owie mi�dzy najmniejsz� i najwi�ksz� szeroko�ci� geograficzn� w zbiorze danych)
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

# 7. Jaka liczba �azienek wyst�puje najcz�ciej i najrzadziej w nieruchomo�ciach niepo�o�onych nad wod�, kt�rych powierzchnia wewn�trzna na kondygnacj� nie przekracza 1800 sqft?
df %>% 
  filter(waterfront==0 & sqft_living/floors<=1800) %>% 
  select(bathrooms) %>% 
  group_by(bathrooms) %>% 
  summarise(var100=n()) %>% 
  arrange(var100)->varb
  head(varb$bathrooms,1) 
  tail(varb$bathrooms,1)
  
  



# Odp: najczesciej 2.5 a najrzadziej 4,75

# 8. Znajd� kody pocztowe, w kt�rych znajduje si� ponad 550 nieruchomo�ci. Dla ka�dego z nich podaj odchylenie standardowe powierzchni dzia�ki oraz najpopularniejsz� liczb� �azienek
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

# 9. Por�wnaj �redni� oraz median� ceny nieruchomo�ci, kt�rych powierzchnia mieszkalna znajduje si� w przedzia�ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj�cych si� przy wodzie.
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

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomo�ci? (bierzemy pod uwag� tylko powierzchni� wewn�trz mieszkania)
df %>% 
  mutate(sqm_living = sqft_living * 0.09290304) %>% 
  mutate(var_10 = price / sqm_living) %>% 
  top_n(var_10, n = -1) %>% 
  select(var_10)

# Odp: 942.79