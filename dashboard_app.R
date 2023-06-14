ui <- fluidPage(
  titlePanel("Wybory do Parlamentu Europejskiego 2019 w Polsce"),
  sidebarLayout(
    tabsetPanel(
      tabPanel("Frekwencja w województwach",
        sidebarPanel(
          radioButtons("obszar",  
                       label = "Wybierz Typ Obszaru:",
                       choices = c("Razem" ="razem", "Wieś"="wieś", "Miasto"= "miasto"),
                       selected = "razem"),
          ),
        mainPanel(plotOutput('frekwencje', height = "800px" ), DT::dataTableOutput('tabela_wojewodztwa'))
      ),
      tabPanel("Poparcie dla partii",
        sidebarPanel(
          selectInput(inputId = 'party',
                      label = "Wybierz Partię",
                      choices = c("Koalicja obywatelska" = "po",
                                  "Prawo i Sprawiedliwość" = "pis",
                                  "Konfederacja" = "konf",
                                  "Lewica Razem" = "lew",
                                  "Wiosna" = "wiosna",
                                  "Kukiz15" = "kukiz")),
          # choices = c('pis','po'))
          radioButtons("jednostka",
                       label = "Wybierz jednostkę administracyjną:",
                       choices = c("Województwa" = "wojewodztwa", "Powiaty" = "powiaty"),
                       selected = "wojewodztwa")
        ),
        mainPanel(plotOutput('wykres', height = "800px"))
               ),
      tabPanel("Zwycięzcy w powiatach", leafletOutput('zwyciezcy_powiaty', height = "800px")),
      tabPanel("Wyniki w województwach",
               sidebarPanel(
                 selectInput(inputId = 'woj',
                             label = "Wybierz województwo",
                             choices = c("dolnośląskie", "kujawsko-pomorskie",
                                         "lubelskie", "lubuskie",
                                         "łódzkie", "małopolskie",
                                         "mazowieckie", "opolskie",
                                         "podkarpackie", "podlaskie",
                                         "pomorskie", "śląskie",
                                         "świętokrzyskie", "warmińsko-mazurskie",
                                         "wielkopolskie", "zachodniopomorskie"))
               ),
               mainPanel(plotOutput('wyniki', height = "800px" ))
               ),
      tabPanel("Pełne dane",
               mainPanel(DT::dataTableOutput('tabela_ogolna'))
               )
    ),
    mainPanel = NaN
  )
)

# Definiowanie logiki aplikacji w Shiny
server <- function(input, output) {
  
  # Wybór wykresu na podstawie przycisków radiowych
  
  output$wykres <- renderPlot({
    {if(input$jednostka == "wojewodztwa")
      ggplot(data = wojewodztwa_shiny, aes(geometry = geometry, fill = get(input$party))) + 
        geom_sf() +
        #geom_text(aes(geometry = geometry, label=paste("po", "%", sep='')), position = position_dodge(0.5), size=6.2) +
        #geom_text(aes(geometry = geometry, label=Województwo), position= position_dodge(0.9), size=3.8) +
        scale_fill_viridis_c(option = "G", alpha = .6, direction = -1) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
        ) +
        labs(fill = "Poparcie")
      else
        ggplot(data = powiaty_shiny, aes(geometry = geometry, fill = get(input$party))) + 
        geom_sf() +
        scale_fill_viridis_c(option = "G", alpha = .6, direction = -1) +
        geom_sf(data = wojewodztwa, fill = NA, color = "black", linetype = "solid", size = 10.0) +
        theme(axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
        ) +
        labs(fill = "Poparcie")}
  })
  
  output$wyniki <- renderPlot({
    wyniki_woj <- filter(wojewodztwa_shiny, Województwo == input$woj)
    wyniki <- data.frame(
      Partie = c("pis", "po", "konf", "wiosna", "lew", "kukiz"),
      Poparcie = as.numeric(wyniki_woj[, c("pis", "po", "konf", "wiosna", "lew", "kukiz")])
    )
    ggplot(data = wyniki, aes(x = Partie, y = Poparcie, fill = Partie)) +
      geom_bar(stat = "identity") +
      labs(x = "Partie", y = "Poparcie", title = paste("Poparcie partii w", input$woj)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
      {if(input$obszar != "razem") filter(., Typ.obszaru == input$obszar) %>% 
      group_by(Województwo) %>%
      summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
                uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
                frekwencja_procentowa=round(oddane_glosy/uprawnieni_do_glosowania, digits = 4)*100,
                .groups='drop') %>%
      as.data.frame() %>% DT::datatable() 
        else 
          entire_data %>% group_by(entire_data$Województwo) %>%
        summarise(oddane_glosy=sum(Liczba.kart.wyjętych.z.urny),
                  uprawnieni_do_glosowania=sum(Liczba.wyborców.uprawnionych.do.głosowania),
                  frekwencja_procentowa=round(oddane_glosy/uprawnieni_do_glosowania, digits = 4)*100,
                  .groups='drop') %>%
        as.data.frame() %>% DT::datatable()
        }
  })
  
  output$zwyciezcy_powiaty <- renderLeaflet(mymap)
  
  output$tabela_ogolna <- DT::renderDataTable({
    entire_data
  })
  # Wyświetlanie wybranego wykresu
}

# Uruchamianie aplikacji Shiny
shinyApp(ui = ui, server = server)