library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartoœci NA w ¿adnej kolumnie

# 1. Jaka jest œrednia cena nieruchomoœci z liczb¹ ³azienek powy¿ej mediany i po³o¿onych na wschód od po³udnika 122W?
mediana_³az = median(df$bathrooms)
df %>% 
  filter(bathrooms>mediana_³az & long>(-122)  & long<180) %>% 
  select(price) %>% 
  summarise(mean=mean(price))

# Odp: 625499.4 USD

# 2. W którym roku zbudowano najwiêcej nieruchomoœci?

df %>% 
  group_by(yr_built) %>% 
  summarise(n = n()) %>% 
  top_n(1, n)


# Odp: 2014

# 3. O ile procent wiêksza jest mediana ceny budynków po³o¿onych nad wod¹ w porównaniu z tymi po³o¿onymi nie nad wod¹?

med_nad_wod=df %>% 
  filter(waterfront==1) %>% 
  select(price) %>% 
  summarise(median=median(price))

med_nad_wod

med_nienad_wod=df %>% 
  filter(waterfront==0) %>% 
  select(price) %>% 
  summarise(median=median(price))

med_nienad_wod

med_nad_wod/med_nienad_wod*100-100

# Odp: Wiêksza o 211,1111 procent

# 4. Jaka jest œrednia powierzchnia wnêtrza mieszkania dla najtañszych nieruchomoœci posiadaj¹cych 1 piêtro (tylko parter) wybudowanych w ka¿dym roku?
df %>% 
  filter(floors==1) %>% 
  group_by(yr_built) %>% 
  filter(price == min(price)) %>% 
  ungroup() %>% 
  summarise(mean(sqft_living))
  
  
  

# Odp: 1030

# 5. Czy jest ró¿nica w wartoœci pierwszego i trzeciego kwartyla jakoœci wykoñczenia pomieszczeñ pomiêdzy nieruchomoœciami z jedn¹ i dwoma ³azienkami? Jeœli tak, to jak ró¿ni siê Q1, a jak Q3 dla tych typów nieruchomoœci?
x=df %>% 
  filter(bathrooms==1 | bathrooms==2) %>% 
  select(grade)

x=x$grade

quantile(x, prob=c(0.25, 0.75))


# Odp: Jest ró¿nica, Q1=6, Q3=7

# 6. Jaki jest odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy a jaki tych na po³udniu? (Pó³noc i po³udnie definiujemy jako po³o¿enie odpowiednio powy¿ej i poni¿ej punktu znajduj¹cego siê w po³owie miêdzy najmniejsz¹ i najwiêksz¹ szerokoœci¹ geograficzn¹ w zbiorze danych)
min=df %>% 
  select(lat) %>% 
  arrange(lat) %>%
  head(1)

max=df %>% 
  select(lat) %>% 
  arrange(-lat) %>%
  head(1)

max
min
sr=min+(max-min)/2
sr=sr$lat
sr

#poludnie

poludnie=df %>%
  filter(lat<sr) %>% 
  select(price)
poludnie=poludnie$price
poludnie
pom=unname(quantile(poludnie, prob=c(0.25, 0.75)))
pom[2]-pom[1]

#polnoc

polnoc=df %>%
  filter(lat>sr) %>% 
  select(price)
polnoc=polnoc$price
polnoc
pom2=unname(quantile(polnoc, prob=c(0.25, 0.75)))
pom2
pom2[2]-pom2[1]

# Odp: Odstêp miêdzykwartylowy ceny mieszkañ po³o¿onych na pó³nocy wynosi 321000 USD a tych na po³udniu 122500 USD

# 7. Jaka liczba ³azienek wystêpuje najczêœciej i najrzadziej w nieruchomoœciach niepo³o¿onych nad wod¹, których powierzchnia wewnêtrzna na kondygnacjê nie przekracza 1800 sqft?
bathdane=df %>% 
  filter(waterfront == 0) %>% 
  mutate(pow_na_kon=sqft_living/floors) %>% 
  filter(pow_na_kon<=1800) %>% 
  group_by(bathrooms) %>% 
  summarise(n=n()) %>% 
  arrange(n)

#najrzadziej
head(bathdane, 1)

#najczesciej
bathdane %>% 
  arrange(-n) %>% 
  head(1)

# Odp: Najczêœiej wystêpuje 2.5 ³azienek a najrzadziej 4.75 ³azienek

# 8. ZnajdŸ kody pocztowe, w których znajduje siê ponad 550 nieruchomoœci. Dla ka¿dego z nich podaj odchylenie standardowe powierzchni dzia³ki oraz najpopularniejsz¹ liczbê ³azienek
kody=df %>% 
  group_by(zipcode) %>% 
  summarise(n=n()) %>% 
  filter(n>550)
kody=kody$zipcode
kody

df %>% 
  filter(zipcode %in% kody) %>% 
  group_by(zipcode) %>% 
  summarise(od_stan_pow=sd(sqft_lot))


df %>% 
  filter(zipcode %in% kody) %>% 
  group_by(zipcode, bathrooms) %>% 
  summarise(l_laz=n()) %>% 
  top_n(1, l_laz)
  
# Odp: Odchylenia standardowe dla kolejnych zipcode [98038, 98052, 98103, 98115, 98117] to [63111, 10276, 1832, 2675, 2319];
       #Najpopularniejsze liczby ³azienek to kolejno [2.5, 2.5, 1, 1, 1]

# 9. Porównaj œredni¹ oraz medianê ceny nieruchomoœci, których powierzchnia mieszkalna znajduje siê w przedzia³ach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajduj¹cych siê przy wodzie.
df_nie_woda=df %>% 
  filter(waterfront == 0)

#0-2000
df_nie_woda %>% 
  filter(sqft_living<=2000) %>% 
  summarise(median(price),
            mean(price))

#2000-4000
df_nie_woda %>% 
  filter(sqft_living>2000 & sqft_living<=4000) %>% 
  summarise(median(price),
            mean(price))

#4000+

df_nie_woda %>% 
  filter(sqft_living>4000) %>% 
  summarise(median(price),
            mean(price))
  

# Odp: W ka¿dym z przedzia³ów œrednia jest wy¿sza od mediany. Ceny œrednie i mediany rosn¹ wraz ze wzrostem powierzchni mieszkalnej.
#Œrednie i mediany dla przedzia³ów (0, 2000], (2000, 4000], (4000, Inf) wynosz¹ kolejno (w USD) [359000, 385084.3], [595000, 645419], [1262750, 1448119].

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomoœci? (bierzemy pod uwagê tylko powierzchniê wewn¹trz mieszkania)
#metr kwadratowy jest w przybli¿eniu równy 10.76 stopom kwadratowym
df %>% 
  mutate(cena_za_metr_2=price/(sqft_living/10.76)) %>% 
  select(cena_za_metr_2) %>% 
  arrange(cena_za_metr_2) %>% 
  head(1)

# Odp: 942.4494 USD

