library(plotly)
library(dplyr)

df <- read.csv("Properties_philly_Kraggle_v2.csv")

df %>%
  select(Rent.Estimate, PropType, yearBuilt) %>%
  mutate(Rent.Estimate = as.numeric(gsub(",", "", df$Rent.Estimate))) %>%
  filter(yearBuilt > 0) %>%
  mutate(
    yearBuilt = case_when(
      yearBuilt <= 1905 ~ "1875-1905",
      yearBuilt <= 1925 ~ "1905-1925",
      yearBuilt <= 1945 ~ "1925-1945",
      yearBuilt <= 1965 ~ "1945-1965",
      yearBuilt <= 1985 ~ "1965-1985",
      TRUE ~ "1985-2006"
    )
  ) -> df1

plot_ly(
  df1,
  x = ~ PropType,
  y = ~ Rent.Estimate,
  frame = ~ yearBuilt,
  type = "box"
) %>%
  layout(
    title = "Estimated rent cost of different house types",
    xaxis = list(title = "House type"),
    yaxis = list(title = "Estimated rent [$]"),
    updatemenus = list(list(
      x = 1,
      y = 1,
      buttons = list(
        list(
          method = "restyle",
          args = list("type", "box"),
          label = "Box plot"
        ),
        list(
          method = "restyle",
          args = list("type", "violin"),
          label = "Violin plot"
        )
      )
    ))
  ) %>%
  animation_slider(currentvalue = list(prefix = "Year built: "))
