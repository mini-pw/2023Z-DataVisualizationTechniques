library(shiny)
library(dplyr)
library(bslib)
library(ggplot2)
library(stringr)
library(plotly)

major_data <- read.csv("graduates-major-data.csv",
    sep = ";",
    fileEncoding = "utf-8"
)

filteredData <- major_data %>%
    select(P_NAZWA_UCZELNI,
        P_N,
        P_KIERUNEK_NAZWA,
        P_ROKDYP,
        P_WWZ_MIES_1:P_WWZ_MIES_60,
        P_WWB_MIES_1:P_WWB_MIES_60,
        P_E_ZAR,
        P_N_SEMESTR
    ) %>%
    tidyr::drop_na(
        P_NAZWA_UCZELNI,
        P_KIERUNEK_NAZWA,
        P_E_ZAR
    ) %>%
    filter(P_E_ZAR != "") %>%
    rename(
        uczelnia = P_NAZWA_UCZELNI,
        liczbaAbso = P_N,
        kierunek = P_KIERUNEK_NAZWA,
        rokUkon = P_ROKDYP,
        zarobek = P_E_ZAR,
        semestr = P_N_SEMESTR
    )

kierunki <- unique(filteredData$kierunek)
kierunki[1:100]

ui <- fluidPage(
  theme = bs_theme(bootswatch = "sandstone"),
  titlePanel("Ranking zarobokowy uczelni z podziałem na kierunki"),
  fluidRow(
    column(6,
            sliderInput("months",
                "Przedział miesiecy po ukoczeniu",
                value = c(1, 60),
                min = 1,
                max = 60,
                step = 1,
                width = "90%"
            )
    ),
    column(3,
            selectInput(
                "wybranyKierunek",
                "Wbierz kierunek",
                unique(filteredData$kierunek)
            ),
    ),
    column(3,
            textInput(
                "grepNazwy",
                "Przeszukaj uczelnie",
                "",
                placeholder = "np. Politechnika War"
            ),
    ),
  ),
  br(),
  fluidRow(
    column(6,
            plotlyOutput("pointPlot")
    ),
    column(6,
            plotlyOutput("linePlot")
    )
  )
)

server <- function(input, output) {
  output$pointPlot <- renderPlotly({
    WWZ_range <- c(
        paste0("P_WWZ_MIES_", input$months[1]),
        paste0("P_WWZ_MIES_", input$months[2])
    )
    WWB_range <- c(
        paste0("P_WWB_MIES_", input$months[1]),
        paste0("P_WWB_MIES_", input$months[2])
    )

    kierunekRestrictedData <- filteredData %>%
       filter(kierunek == input$wybranyKierunek)

    dataForPointPlot <- kierunekRestrictedData %>%
        mutate(
            mean_wwz = rowMeans(
                select(kierunekRestrictedData,
                    WWZ_range[1]:WWZ_range[2]
                ),
                na.rm = TRUE
            ),
            mean_wwb = rowMeans(
                select(kierunekRestrictedData,
                    WWB_range[1]:WWB_range[2]
                ),
                na.rm = TRUE
            ),
            semestr = as.character(semestr)
        ) %>%
        select(uczelnia, semestr, rokUkon, liczbaAbso, mean_wwz, mean_wwb, kierunek) %>%
        tidyr::drop_na(mean_wwz, mean_wwb, rokUkon, liczbaAbso) %>%
        filter(mean_wwz != 0) %>%
        filter(grepl(input$grepNazwy, uczelnia))

    plot_ly(dataForPointPlot,
            x = ~mean_wwz,
            y = ~mean_wwb,
            color = ~semestr,
            colors = "Set1",
            text = paste(
                dataForPointPlot$uczelnia,
                dataForPointPlot$rokUkon
            )
        ) %>%
        layout(
            title = paste("Zalezność miedzy bezrobociem, a zarobkami absolwentów kierunku",
                    input$wybranyKierunek),
            xaxis = list(
                title = "Względny Wskaźnik Zarobków [wiecej lepiej]",
                range = c(0, 1.1 * max(dataForPointPlot$mean_wwz))
            ),
            yaxis = list(
                title = "Względny Wskaźnik Bezrobocia [mniej lepiej]",
                range = c(0, 1.1 * max(dataForPointPlot$mean_wwb))
            ),
            legend = list(title = list(text = "<b> Długość programu\nw semestrah </b>"))
        )

  })
  output$linePlot <- renderPlotly({

    WWZ_range <- c(
        paste0("P_WWZ_MIES_", input$months[1]),
        paste0("P_WWZ_MIES_", input$months[2])
    )
    WWB_range <- c(
        paste0("P_WWB_MIES_", input$months[1]),
        paste0("P_WWB_MIES_", input$months[2])
    )

    kierunekRestrictedData <- filteredData %>%
       filter(kierunek == input$wybranyKierunek)

    fullyRestrictedData <- kierunekRestrictedData %>%
        select(uczelnia, rokUkon, liczbaAbso, zarobek) %>%
        tidyr::drop_na(rokUkon, zarobek) %>%
        filter(zarobek != 0) %>%
        filter(grepl(input$grepNazwy, uczelnia))


    dataForLinePlot <- fullyRestrictedData %>%
        group_by(rokUkon) %>%
        summarise(
            sredniZarobek = sum(liczbaAbso * zarobek) / sum(liczbaAbso)
        )

    plot_ly(
        dataForLinePlot,
        x = ~rokUkon,
        y = ~sredniZarobek,
        type = "scatter",
        mode = "lines",
        text = paste(
            "Rok",
            dataForLinePlot$rokUkon,
            "zarabia średnio",
            round(dataForLinePlot$sredniZarobek, 2),
            "zł"
        )) %>%
        layout(
            title = paste("Średnie zarobiki po kierunku",
                    input$wybranyKierunek,
                    "w wybranych",
                    length(unique(fullyRestrictedData$uczelnia)),
                    "uczelniach"
                ),
            xaxis = list(
                title = "Rok ukończenia",
                range = c(
                    min(dataForLinePlot$rokUkon) - 0.1,
                    max(dataForLinePlot$rokUkon) + 0.1
                )
            ),
            yaxis = list(
                title = "Średnie zarobiki [w zł]",
                range = c(0, 1.2 * max(dataForLinePlot$sredniZarobek))
            )
        )

  })
}

shinyApp(ui, server)

