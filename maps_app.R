ui <- fluidPage(
  titlePanel("Frekwencja wyborcza w wojewodztwach"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("wybor_wykresu", 
                   label = "Wybierz mapę:",
                   choices = c("Ogolem", "Wies", "Miasto"),
                   selected = "Ogolem")
    ),
    
    mainPanel(
      plotOutput("wykres")
    )
  )
)

# Definiowanie logiki aplikacji w Shiny
server <- function(input, output) {
  
  # Wybór wykresu na podstawie przycisków radiowych
  wybrany_wykres <- reactive({
    switch(input$wybor_wykresu,
           "Ogolem" = frekwencja_ogolna,
           "Wies" = frekwencja_wies,
           "Miasto" = frekwencja_miasto)
  })
  
  # Wyświetlanie wybranego wykresu
  output$wykres <- renderPlot({
    wybrany_wykres()
  })
}

# Uruchamianie aplikacji Shiny
shinyApp(ui = ui, server = server)