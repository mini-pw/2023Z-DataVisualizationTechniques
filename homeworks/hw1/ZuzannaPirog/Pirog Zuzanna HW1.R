# Rozwi¹zanie: Zuzanna Piróg

library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartoœci NA w ¿adnej kolumnie

# 1. Jaka jest œrednia cena nieruchomoœci z liczb¹ ³azienek powy¿ej mediany i po³o¿onych na wschód od po³udnika 122W?
df %>% 
  filter(long > -122 & bathrooms > median(bathrooms)) %>% 
  summarise(srednia = mean(price))

# Odp:625499.4

# 2. W którym roku zbudowano najwiêcej nieruchomoœci?
df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  select(yr_built) %>% 
  head(1)

# Odp: 2014

# 3. O ile procent wiêksza jest mediana ceny budynków po³o¿onych nad wod¹ w porównaniu z tymi po³o¿onymi nie nad wod¹?
df %>% 
  filter(waterfront == 1) %>% 
  summarise(mediana = median(price)) -> mediana_woda
df %>% 
  filter(waterfront == 0) %>% 
  summarise(mediana = median(price)) -> mediana_nie_woda
100 *(mediana_woda[1] - mediana_nie_woda[1])/ mediana_nie_woda[1]

# Odp: 211.1111 %

# 4. Jaka jest œrednia powierzchnia wnêtrza mieszkania dla najtañszych nieruchomoœci posiadaj¹cych 1 piêtro (tylko parter) wybudowanych w ka¿dym roku?
df %>% 
  filter(floors == 1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  summarise(wynik = mean(sqft_living))

# Odp:1030

# 5. Czy jest ró¿nica w wartoœci pierwszego i trzeciego kwartyla jakoœci wykoñczenia pomieszczeñ pomiêdzy nieruchomoœciami z jedn¹ i dwoma ³azienkami? Jeœli tak, to jak ró¿ni siê Q1, a jak Q3 dla tych typów nieruchomoœci?
#nieruchomosci z jedn¹ ³azienk¹
df %>% 
  filter(bathrooms == 1 | bathrooms == 2) %>% 
  select(grade) -> df_grade
df_grade <- df_grade$grade
quantile(df_grade, probs = c(0.25, 0.75))

# Odp: tak, jest ró¿nica, pierwszy kwartyl wynosi 6 a trzeci wynosi 7

# 6. Jaki jest odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy a jaki tych na po³udniu? (Pó³noc i po³udnie definiujemy jako po³o¿enie odpowiednio powy¿ej i poni¿ej punktu znajduj¹cego siê w po³owie miêdzy najmniejsz¹ i najwiêksz¹ szerokoœci¹ geograficzn¹ w zbiorze danych)
df %>% 
  arrange(lat) %>% 
  select(lat) %>% 
  head(1)-> min
df %>% 
  arrange(-lat) %>% 
  select(lat) %>% 
  head(1)-> max
min + (max - min)/2 -> center_df
center <- center_df[1,1]
center


#mieszkania na pó³nocy

polnoc <- df %>% 
  filter(lat > center) %>% 
  select(price)
polnoc <- polnoc$price

polnoc <- quantile(polnoc, prob=c(0.25, 0.75))
wynik_n <- polnoc[2] - polnoc[1]
wynik_n


#mieszkania na po³udnie

poludnie <- df %>% 
  filter(lat < center) %>% 
  select(price)
poludnie <- poludnie$price

poludnie <- quantile(poludnie, prob=c(0.25, 0.75))
wynik_s <- poludnie[2] - poludnie[1]
wynik_s


# Odp: Na pó³nocy odstêp miêdzykwartylowy: 321000. Na po³udniu odstêp miêdzykwartylowy: 122500

# 7. Jaka liczba ³azienek wystêpuje najczêœciej i najrzadziej w nieruchomoœciach niepo³o¿onych nad wod¹, których powierzchnia wewnêtrzna na kondygnacjê nie przekracza 1800 sqft?
df %>% 
  filter(waterfront == 0) %>% 
  mutate(pow_kondyg = sqft_living/floors) %>% 
  filter(pow_kondyg <= 1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n = n()) %>% 
  arrange(n) -> bathroom_count
head(bathroom_count, 1) #najrzadziej ³azienka

bathroom_count %>% 
  arrange(-n) %>% 
  head(1) #najczêœciej ³azienka


# Odp:Najczêœciej wystêpuje 2.5 ³azienek, a najrzadziej 4.75

# 8. ZnajdŸ kody pocztowe, w których znajduje siê ponad 550 nieruchomoœci. Dla ka¿dego z nich podaj odchylenie standardowe powierzchni dzia³ki oraz najpopularniejsz¹ liczbê ³azienek
df %>% 
  group_by(zipcode) %>% 
  summarise(n = n()) %>% 
  filter(n > 550) %>% 
  select(zipcode) -> kody 
kody_w <- kody[['zipcode']]
kody_w

df %>% 
  filter(zipcode %in% kody_w) %>% 
  group_by(zipcode) %>% 
  summarise(odchylenie = sd(sqft_lot)) -> odchylenie_pow_dzialki
odchylenie_pow_dzialki

df %>% 
  filter(zipcode %in% kody_w) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(n = n()) %>% 
  top_n(1, n) %>% 
  select(zipcode, bathrooms)

# Odp:Odchylenie standardowe dla ka¿dego zipkodu - 98038: 63111; 98052:10276;  98103:1832;  98115:2675;  98117: 2319
    # Najpopularniejsza liczba ³azienek: 98038: 2.5 ; 98052: 2.5 ; 98103: 1 ;  98115:1 ; 98117:1

# 9. Porównaj œredni¹ oraz medianê ceny nieruchomoœci, których powierzchnia mieszkalna znajduje siê w przedzia³ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj¹cych siê przy wodzie.
#(0, 2000]
df %>% 
  filter(waterfront == 0) -> df_no_water

df_no_water %>% 
  filter(sqft_living <= 2000) %>% 
  summarise(mediana = median(price),
            srednia = mean(price)) ->df_0_2000
df_0_2000

#(2000, 4000]
df_no_water %>% 
  filter(sqft_living > 2000 & sqft_living <= 4000) %>% 
  summarise(mediana = median(price),
            srednia = mean(price)) ->df_2000_4000
df_2000_4000

df_no_water %>% 
  filter(sqft_living > 4000) %>% 
  summarise(mediana = median(price),
            srednia = mean(price)) ->df_2000_4000
df_2000_4000

# Odp: Najwiêksza œrendia i mediana znajduje siê dla nieruchomoœci o powierzchni mieszkalnej wiêKszej ni¿ 4000 sqft, nastêpnie dla przedzia³u (2000,4000], a najni¿sze ceny dla (0, 2000]
#      W ka¿dym z przedzia³ów œrednia jest wy¿sza od mediany

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomoœci? (bierzemy pod uwagê tylko powierzchniê wewn¹trz mieszkania)

df %>% 
  mutate(metr_kw = price/(sqft_living/10.76)) %>% 
  select(metr_kw) %>% 
  arrange(metr_kw) %>% 
  head(1)

# Odp:942.4494
