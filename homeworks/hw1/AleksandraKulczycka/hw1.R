library(dplyr)

df <- read.csv("https://raw.githubusercontent.com/MI2-Education/2023Z-DataVisualizationTechniques/main/homeworks/hw1/house_data.csv")

colnames(df)
dim(df)
head(df)
apply(df, 2, function(x) sum(is.na(x))) # nie ma wartości NA w żadnej kolumnie

# 1. Jaka jest średnia cena nieruchomości z liczbą łazienek powyżej mediany i położonych na wschód od południka 122W?
df%>%
  filter(bathrooms>median(bathrooms,na.rm=T))%>%
  filter(long>-122)%>%
  summarise(mean_price = mean(price,na.rm = T))

# Odp: 625499.4

# 2. W którym roku zbudowano najwięcej nieruchomości?
df%>%
  group_by(yr_built)%>%
  summarise(n=n())%>%
  top_n(1,n)
  
# Odp:2014 

# 3. O ile procent większa jest mediana ceny budynków położonych nad wodą w porównaniu z tymi położonymi nie nad wodą?
 x<-  df%>%
  filter(waterfront == 1)%>%
  summarise(median(price))
 x
 y<- df%>%
  filter(waterfront == 0)%>%
  summarise(median(price))
 y
 
 z <- (x-y)/y *100
 z
# Odp:211.1111%

# 4. Jaka jest średnia powierzchnia wnętrza mieszkania dla najtańszych nieruchomości posiadających 1 piętro (tylko parter) wybudowanych w każdym roku?
a <- df%>%
  filter(floors == 1)%>%
  group_by(yr_built)%>%
  top_n(-10,price)%>%
  summarise(mean(sqft_living))
a
# Odp:
#1030.422 
  

# 5. Czy jest różnica w wartości pierwszego i trzeciego kwartyla jakości wykończenia pomieszczeń pomiędzy nieruchomościami z jedną i dwiema łazienkami? Jeśli tak, to jak różni się Q1, a jak Q3 dla tych typów nieruchomości?
x <- df%>%
       filter(bathrooms ==1 | bathrooms ==2)
  quantile(x$grade, digits = 4)
?quantile
# Odp: Q1 = 6, Q3=7

# 6. Jaki jest odstęp międzykwartylowy ceny mieszkań położonych na północy a jaki tych na południu? (Północ i południe definiujemy jako położenie odpowiednio powyżej i poniżej punktu znajdującego się w połowie między najmniejszą i największą szerokością geograficzną w zbiorze danych)
  lat_min <- min(df$lat)
  lat_max <- max(df$lat)
  center <- (lat_max+lat_min)/2
  
   x <-df%>%
    filter(lat>center)%>%
     summarise(Qa = IQR(price))
    
   y<- df%>%
     filter(lat<center)%>%
     summarise(Qb = IQR(price))
    x
    y
    

   # Odp: dla tych północnych - 321000, a dla tych południowych - 12250

# 7. Jaka liczba łazienek występuje najczęściej i najrzadziej w nieruchomościach niepołożonych nad wodą, których powierzchnia wewnętrzna na kondygnację nie przekracza 1800 sqft?
     df%>%
        filter(waterfront ==0 & sqft_living/floors <=1800)%>% #rozważamy konkretne kondygnacje
        group_by(bathrooms)%>%
        summarise(n = n())%>%
        filter(n ==max(n)|n ==min(n))
   

# Odp: najrzadziej występująca to 4.75, najczęściej występująca to 2.5

# 8. Znajdź kody pocztowe, w których znajduje się ponad 550 nieruchomości. Dla każdego z nich podaj odchylenie standardowe powierzchni działki oraz najpopularniejszą liczbę łazienek

df1 <- df%>%
  group_by(zipcode)%>%
  summarise(sum = n())%>%
  filter(sum>550)
   
   c <- c(df1$zipcode) #przypisujemy kody
   

  df%>%
    filter(zipcode %in% c)%>%
    group_by(zipcode)%>%
    summarise(sd=sd(sqft_lot)) #sd
  

  df%>%
    filter(zipcode %in% c)%>%
    group_by(zipcode,bathrooms)%>%
    summarise(n = n())%>%
    top_n(1,n) #lazienki
    

# Odp: 
  #nr   zipcode amount   sd           bathrooms
  # 1   98038   590      63111.11     2.5
  # 2   98052   574      10276.19     2.5
  # 3   98103   602      1832.009     1
  # 4   98115   583      2675.302     1
  # 5   98117   553      2318.662     1

# 9. Porównaj średnią oraz medianę ceny nieruchomości, których powierzchnia mieszkalna znajduje się w przedziałach (0, 2000], (2000,4000] oraz (4000, +Inf) sqft, nieznajdujących się przy wodzie.
x <-df%>%
  filter(sqft_living<=2000, waterfront == 0)%>%
summarise(median(price), mean(price))
x
y<- df%>%
  filter(sqft_living<=4000,sqft_living>2000, waterfront == 0)%>%
  summarise(median(price), mean(price))
y
z<- df%>%
  filter(sqft_living>4000, waterfront == 0)%>%
  summarise(median(price), mean(price))
z

# Odp:
# Range      Median      Mean          tzn, że wraz ze wzrostem cen, rośnie różnica pomiędzy medianą a średnią
#(0,200]     359000      385084.3
#(200,400]   595000      645419
#(400,inf)   1262750     1448119

# 10. Jaka jest najmniejsza cena za metr kwadratowy nieruchomości? (bierzemy pod uwagę tylko powierzchnię wewnątrz mieszkania)
df%>%
  mutate(price_m = price/(sqft_living*0.09290304))%>% #konieczna była zmiana miary w stopach, na metry 
  top_n(1,-price_m)%>%
  select(price_m)
  
# Odp:942.7919

