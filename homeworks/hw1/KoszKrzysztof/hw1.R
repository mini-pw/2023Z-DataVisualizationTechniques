library(dplyr)
library(tidyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?

df%>%
  select(price, bathrooms, long)%>%
  filter(bathrooms>median(df$bathrooms), long>-122)%>%
  summarise(mean_price=mean(price))
  

# Odp:625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?

df%>%
  select(id, yr_built)%>%
  group_by(yr_built)%>%
  summarise(n=n())%>%
  arrange(desc(n))%>%
  head(1)%>%
  select(yr_built)

# Odp:2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?

x<-df%>%
  group_by(waterfront)%>%
  summarise(median=median(price))%>%
  arrange(waterfront)
x
odp<-x[2,2]/x[1,2]*100-100
odp
# Odp:211,111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?

df %>%
  filter(floors == 1)%>% 
  group_by(yr_built)%>% 
  filter(price == min(price))%>% 
  ungroup()%>% 
  summarise(mean = mean(sqft_living))

# Odp:1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?

df%>%
  filter(bathrooms==1 | bathrooms==2)%>%
  select(grade, bathrooms)%>%
  group_by(bathrooms)%>%
  summarise(q1 = quantile(grade, probs = .25), q3 = quantile(grade, probs = .75))

 
# Odp:Tak, jest roznica. dla 2 lazieniek i pierwszy i trzeci kwartyl jest wiekszy o 1 (dla 1 lazienki Q1=6, Q3=7, a dla 2 lazienek Q1=7, Q2=8)

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

mid=(max(df$lat)+min(df$lat))/2

df%>%
  select(lat, price)%>%
  mutate(polkula=ifelse(lat>mid, "N", "S"))%>%
  group_by(polkula)%>%
  summarise(miedzykwart=IQR(price))

# Odp:Odstep dla mieszkan polozonych na polnocy to 321000, a na poludniu to 122500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?

helper<-df%>%
  select(floors, sqft_living, bathrooms, waterfront)%>%
  filter(waterfront==0, sqft_living/floors<=1800)%>%
  group_by(bathrooms)%>%
  summarise(n=n())

mostly<-helper%>%
  arrange(desc(n))%>%
  head(1)
mostly[1,1]

rarely<-helper%>%
  arrange(n)%>%
  head(1)
rarely[1,1]

# Odp:Najczesciej 2,5, najrzadziej 4,75 lazienek

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df%>%
  select(zipcode, sqft_lot, bathrooms)%>%
  group_by(zipcode)%>%
  summarise(ilosc=n(), odch_stand=sd(sqft_lot), maxi=getmode(bathrooms))%>%
  filter(ilosc>550)


# Odp: Format: kod pocztowy - odchylenie standardowe powierzchni dzialki - najczesciej wystepujaca liczba lazienek
#98038 - 63111 - 2.5
#98052 - 10276 - 2.5
#98103 - 1832 - 1
#98115 - 2675 - 1
#98117 - 2319 - 1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.

df%>%
  select(price, sqft_living, waterfront)%>%
  filter(waterfront==0, sqft_living<=2000)%>%
  summarise(mean=mean(price), median=median(price))%>%
  mutate(cowieksze=ifelse(mean>median, "srednia", "mediana"))

df%>%
  select(price, sqft_living, waterfront)%>%
  filter(waterfront==0, sqft_living<=4000, sqft_living>2000)%>%
  summarise(mean=mean(price), median=median(price))%>%
  mutate(cowieksze=ifelse(mean>median, "srednia", "mediana"))

df%>%
  select(price, sqft_living, waterfront)%>%
  filter(waterfront==0, sqft_living>4000)%>%
  summarise(mean=mean(price), median=median(price))%>%
  mutate(cowieksze=ifelse(mean>median, "srednia", "mediana"))

# Odp:(0,2000] srednia>mediana, (2000,4000] srednia>mediana, (4000, +Inf) srednia>mediana

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)

df%>%
  select(price, sqft_living)%>%
  mutate(sqm=0.092903411613275*sqft_living)%>%
  mutate(price_per_sqm=price/sqm)%>%
  arrange(price_per_sqm)%>%
  head(1)


# Odp:942,7881