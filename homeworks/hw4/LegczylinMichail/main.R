library(dplyr)
library(plotly)

df <-
  na.omit(as.data.frame(read.csv("Properties_philly_Kraggle_v2.csv")))

steps <- list(
  list(
    args = list(list(
      "marker.color" = list(""),
      "marker.symbol" = list("circle")
    )),
    label = "Default",
    method = "restyle",
    value = "1"
  ),
  list(
    args = list(list(
      "marker.color" = list("darkgreen"),
      "marker.symbol" = list("x")
    )),
    label = "Darkgreen X",
    method = "restyle",
    value = "2"
  ),
  list(
    args = list(list(
      "marker.color" = list("DarkSlateGrey"),
      "marker.symbol" = list("star")
    )),
    label = "Dark Slate Grey Star",
    method = "restyle",
    value = "3"
  ),
  list(
    args = list(list(
      "marker.color" = list("pink"),
      "marker.symbol" = list("star-diamond")
    )),
    label = "Pink Star Diamond",
    method = "restyle",
    value = "4"
  )
)

plot_ly(
  data = df,
  x = ~ bathrooms,
  y = ~ Water,
  color = ~ bedrooms,
  colors = "Set1",
  type = "box",
  marker = list(size = 10)
) %>%
  layout(
    title = "Bathrooms count corelation to water discharge",
    
    sliders = list(list(
      active = 0,
      currentvalue = list(prefix = "Mark style: "),
      pad = list(t = 60),
      steps = steps
    )),
    
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
        ),
        list(
          method = "restyle",
          args = list("type", "bar"),
          label = "Bar plot"
        )
      )
    ))
  )