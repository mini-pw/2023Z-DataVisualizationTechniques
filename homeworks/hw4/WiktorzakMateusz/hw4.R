
library(plotly)
df <- read.csv("Properties_philly_Kraggle_v2.csv")


colnames(df)

df1 <- df %>% 
  mutate(Sale.Price.bid.price = as.numeric(gsub(",", "", substring(Sale.Price.bid.price, 2)))) %>%
  mutate(bedrooms = as.numeric(bedrooms)) %>% 
  filter(!is.na(Sale.Price.bid.price) & !is.na(bedrooms) & yearBuilt != 0)

plot_ly(
  data = df1,
  x = ~yearBuilt
) %>% 
  add_markers(y = ~Sale.Price.bid.price, name = "Price") %>%
  add_markers(y = ~finished...SqFt., name = "SqFt", visible = FALSE) %>% 
  layout(
    title = "Ratio of price/sqft to year",
    yaxis = list(title = "Price"),
    showlegend = FALSE,
    updatemenus = list(
      list(
        buttons = list(
          list(method = "update",
               args = list(list(visible = list(TRUE, FALSE)),
                           list(yaxis = list(title = "Price"))),
               label = "Price"),
          
          list(method = "update",
               args = list(list(visible =  list(FALSE, TRUE)),
                           list(yaxis = list(title = "SqFt"))),
               label = "SqFt"))
      )
      
    )
    
  )

