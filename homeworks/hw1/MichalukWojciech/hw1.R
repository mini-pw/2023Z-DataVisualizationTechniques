library(dplyr)

df <- read.csv("house_data.csv")

colnames(df)
dim(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie
View(df)

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df %>% 
  select(price, bathrooms, long) %>% 
  filter(bathrooms>median(bathrooms), long>-122) %>% 
  summarise(meanPrice = mean(price))

# Odp: meanPrice 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
df %>% 
  group_by(yr_built) %>%  
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  head(1) %>% 
  select(yr_built)

# Odp: yr_built 2014

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
resultM <- df %>% 
  select(price, waterfront) %>% 
  group_by(waterfront) %>% 
  summarise(median = median(price))

wf<- resultM %>% filter(waterfront == 1) %>% select(median)
nwf<- resultM %>% filter(waterfront == 0) %>% select(median)
((100 * wf)/nwf)-100
# Odp: 211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
df %>% 
  select(sqft_living, price, floors, yr_built) %>% 
  filter(floors==1) %>%
  group_by(yr_built) %>% 
  filter(price==min(price)) %>% 
  ungroup() %>% 
  summarise(medianSurface = mean(sqft_living))
  

# Odp: 1030

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwoma łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
df %>%
  select(grade, bathrooms) %>% 
  filter(bathrooms==2 | bathrooms==1) %>% 
  group_by(bathrooms) %>% 
  summarise(Q1 = quantile(grade, probs= c(0.25)), Q3 = quantile(grade, probs= c(0.75)))

# Odp: Istnieje różnica, dla mieszkań z 1 łazienką Q1 = 6, Q3=7. Dla mieszkań z 2 łazienkami Q1=7, Q3=8.

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)

medium <- (max(df$lat)+min(df$lat))/2

df %>% 
  select(price, lat) %>% 
  mutate(position = if_else(lat>medium, "N", "S" )) %>% 
  group_by(position) %>% 
  summarise(IQR = IQR(price))

# Odp: dla tych na północy 321 000, na południu 122 500

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
res <- df %>% 
  select(bathrooms, waterfront, sqft_living, floors) %>% 
  filter(waterfront == 0, (sqft_living/floors)<1800) %>% 
  group_by(bathrooms) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

max <- res %>% head(1)
min <- res %>% tail(1)


# Odp: najczesciej 2,5 łazienki; najrzadziej 4,5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek
getDominant <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

prepData <- df %>% 
  select(zipcode, sqft_lot, bathrooms) %>% 
  group_by(zipcode) %>% 
  mutate(amount = n()) %>% 
  filter(amount > 550)
  
prepData %>% 
  group_by(zipcode) %>% 
  summarise(sdSurface = sd(sqft_lot),
            bathNum = getDominant(bathrooms))
# Odp:
# zipcode sdSurface bathNum
# 1   98038    63111.     2.5
# 2   98052    10276.     2.5
# 3   98103     1832.     1  
# 4   98115     2675.     1  
# 5   98117     2319.     1  

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
df %>% 
  select(price, sqft_living, waterfront) %>% 
  filter(waterfront == 0) %>% 
  mutate(range = if_else(sqft_living %in% 0:2000, "1",
                if_else(sqft_living %in% 2001:4000, "2", "3"))) %>% 
  group_by(range) %>% 
  summarise(meanPrice = mean(price),
            medianPice = median(price),
            greaterValue = if_else(meanPrice > medianPice, "meanPrice", "medianPice"))
  

#Odp;
# range meanPrice medianPice greaterValue
# 1        385084.     359000 meanPrice   
# 2        645419.     595000 meanPrice   
# 3       1448119.    1262750 meanPrice 

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df%>%
  select(price, sqft_living)%>%
  mutate(sgmtLiving=0.092903411613275*sqft_living)%>%
  mutate(pricePerSqmt=price/sgmtLiving)%>%
  arrange(pricePerSqmt)%>%
  head(1) %>% 
  select(pricePerSqmt)

# Odp: pricePerSqmt 942.7881

