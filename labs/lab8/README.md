### Zadanie 1 
 
Używając funkcji `textOutput()` oraz `renderText()` przygotuj krótki tekst, który będzie zamieszczony pod tytułem aplikacji.
Tekst jest poniżej, lecz zawiera pewne braki, które są oznaczone jako **XXX**. Uzupełnij odpowiednimi funkcjami.

"Aplikacja zawiera wstępną analizę zbioru danych o pingwinach.", 
"W zbiorze danych jest", 
**XXX**, 
"pingwinów.",
"Najmniejszy waży",
**XXX**, 
"gramów!"


### Zadanie 2

Używając funkcji `checkboxGroupInput()` przygotuj możliwość zaznaczenia odpowiedzi na pytanie *"Który gatunek pingwinów wybierasz?"*. Można wybrać jedną, dwie lub trzy kategorie.


### Zadanie 3

Używając funkcji `sliderInput()` przygotuj możliwość wyboru zakresu zmiennej `Year` (na przykład możliwość wyboru lat 2007-2009). Przyjmuje ona trzy wartości, zadbaj o możliwość wyboru tylko tych wartości.


### Zadanie 4

Przygotuj wykres przy użyciu pakietu `plotly` pod tytułem "Zależność między długością skrzydła a masą ciała". Kolorem oznacz gatunek pingwinów. Zadbaj, aby na wykresie niezależnie od wybranego gatunku pingwinów skala osi x i y była taka sama. Dodatkowo ogranicz dane z zależności od `silderInput()`z Zadania 3.

**Uwaga!** Dobrze wykonany wybór roku zwraca wektor o dwóch elementach. Można się do wybranych wartości odwołać przez `input$nazwa[1]` oraz `input$nazwa[2]`.


### Zadanie 5

Używając funkcji `selectInput()`przygotuj możliwość wybrania jednej z czterech nazw kolumn ("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g").


### Zadanie 6

Przygotuj drugi wykres używając `ggplot()`, a następnie `ggplotly()`, który będzie obrazował rozkład wybranej zmiennej z Zadania 5 w podziale na gatunek pingwina.

### Zadanie 7

Używając funkcji `dataTableOutput()` oraz `renderDataTable()` przygotuj tablę, która będzie zwierała informację o średniej dla wszystkich zmiennych ilościowych ze zbioru danych w podziale na gatunek oraz wyspę z której pochodzą pingwiny.

### Zadanie 8

Używając funkcji `renderUI()` przygotuj drugą rozwijaną listę (jak w Zadaniu 5). Bazując na tych dwóch wyborach zmiennych przygotuj wykres punktowy ukazujący zależność pomiędzy wybranymi zmiennymi.

### Zadanie 9

Korzystając z https://rstudio.github.io/bslib/ oraz https://rstudio.github.io/bslib/articles/bslib.html zapoznaj się ze sposobem modyfiakcji stylów w R Shiny i wybierając jeden z nich zmodyfikuj swoją aplikację.

### Zadanie (*)

Bazując na wykonanej aplikacji przygotuj R Shiny Dashboard https://rstudio.github.io/shinydashboard/. Każda z dwóch kolumn niech będzie jedną z dwóch zakładek.


### Przykładowy wygląd aplikacji 

![plot](app.png)