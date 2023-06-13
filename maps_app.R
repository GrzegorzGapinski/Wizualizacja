ui <- fluidPage(
  titlePanel("Frekwencja wyborcza w wojewodztwach"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("obszar", 
                   label = "Wybierz Typ Obszaru:",
                   choices = c("Razem" ="razem", "Wieś"="wieś", "Miasto"= "miasto"),
                   selected = "razem"),
    selectInput(inputId = 'party',
                label = "Wybierz Partię",
                choices = c("Koalicja obywatelska" = "po",
                            "Prawo i Sprawiedliwość" = "pis",
                            "Konfederacja" = "konf",
                            "Lewica Razem" = "lew",
                            "Wiosna" = "wiosna",
                            "Kukiz15" = "kukiz"))
                # choices = c('pis','po'))
    ),
    mainPanel(
      tabsetPanel(tabPanel('Frekwencja w Województwach', 
                           plotOutput('frekwencje', height = "800px" )),
                  tabPanel('Popracia dla Partii', plotOutput('wykres', height = "800px")),
                  tabPanel('Dane dla Województw', DT::dataTableOutput('tabela_wojewodztwa'))
      )
    )
  )
)


# Definiowanie logiki aplikacji w Shiny
server <- function(input, output) {
  
  # Wybór wykresu na podstawie przycisków radiowych
  
  
  output$wykres <- renderPlot({
      ggplot(data = powiaty_shiny, aes(geometry = geometry, fill = get(input$party))) + 
      geom_sf() +
      scale_fill_viridis_c(option = "G", alpha = .6, direction = -1) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
      ) +
      labs(fill = "Poparcie")
  })
  output$frekwencje <- renderPlot({
    entire_data %>%
    {if(input$obszar != "razem") filter(., Typ.obszaru == input$obszar) else .} %>% #not sure why this ifelse dont work
    group_by(Województwo) %>%
    summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
    uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
    .groups='drop') %>%
    as.data.frame() %>%
    merge(x=., y=wojewodztwa[c('JPT_NAZWA_', 'geometry','X','Y')], by.x="Województwo", by.y="JPT_NAZWA_") %>% 
    transform(., frekwencja=round(oddane_glosy/uprawnieni_do_glosowania, digits = 3)*100) %>% 
    transform(., Y_woj=Y-20000) %>%
      ggplot(data = ., aes(geometry = geometry)) +
      geom_sf(aes(fill = frekwencja)) +
      geom_text(aes(x=X, y=Y, label=paste(frekwencja, "%", sep='')), position = position_dodge(0.5), size=6.2) +
      geom_text(aes(x=X, y=Y_woj, label=Województwo), position= position_dodge(0.9), size=3.8) +
      scale_fill_viridis_c(option = "D",trans = "sqrt", alpha = .4) +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  output$tabela_wojewodztwa <- DT::renderDataTable({
    entire_data %>%
      {if(input$obszar != "razem") filter(., Typ.obszaru == input$obszar) else .} %>% #not sure why this ifelse dont work
      group_by(Województwo) %>%
      summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
      uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
      .groups='drop') %>%
      as.data.frame() %>% DT::datatable()
  })
  # Wyświetlanie wybranego wykresu
}

# Uruchamianie aplikacji Shiny
shinyApp(ui = ui, server = server)

