# LIBRARIES ----
library(shiny)
library(shinythemes)
library(plotly)
library(ggwordcloud)
library(tidyverse)
library(tidyquant)
library(shinydashboard)
library(bs4Dash)
library(lubridate)
library(gridExtra)
library(scales)
library(igraph)
library(ggraph)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(textdata)
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(tidytext) #text mining
library(readxl)
library(purrr)
library(stringi)
library(stringr)

Bad_bunny_canciones <- read_excel("C:/Users/ferna/Desktop/Bad bunny canciones.xlsx")

glimpse(Bad_bunny_canciones)

###TEXT MINING###
limpiar_tokenizar <- function(texto){
    # El orden de la limpieza no es arbitrario
    # Se convierte todo el texto a minúsculas
    nuevo_texto <- tolower(texto)
    # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
    # de cualquier cosa que no sea un espacio)
    nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
    # Eliminación de signos de puntuación
    nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
    # Eliminación de números
    nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
    # Eliminación de espacios en blanco múltiples
    nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
    # Tokenización por palabras individuales
    nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
    #eliminar tilde 
    nuevo_texto <- stri_trans_general(nuevo_texto,"Latin-ASCII")
    # Eliminación de tokens con una longitud < 2
    nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 2})
    return(nuevo_texto)
}


# Se aplica la función de limpieza y tokenización a cada tweet
Bad_bunny_canciones <- Bad_bunny_canciones %>% mutate(texto_tokenizado = map(.x = letra,
                                                                             .f = limpiar_tokenizar))

#Se tokeniza los tweets extraídos
BB_canciones = Bad_bunny_canciones %>% select(-letra) %>% unnest()
BB_canciones <- BB_canciones %>% rename(token = texto_tokenizado) 

BB_canciones$token2 = removeWords(BB_canciones$token, words = stopwords("spanish"))
BB_canciones2 = BB_canciones %>% filter(token2 != "")


### BIGRAMAS ###
### Relación entre palabras
limpiar <- function(texto){
    # El orden de la limpieza no es arbitrario
    # Se convierte todo el texto a minúsculas
    nuevo_texto <- tolower(texto)
    # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
    # de cualquier cosa que no sea un espacio)
    nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
    # Eliminación de signos de puntuación
    nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
    # Eliminación de números
    nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
    # Eliminación de espacios en blanco múltiples
    nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
    # Eliminación de tokens con una longitud < 2
    nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 2})
    return(nuevo_texto)
}


albums123 = list("Un verano sin ti" = "un verano sin ti",
    "El último tour" = "el ultimo tour",
    "Las que no iban a salir" = "Las que no iban a salir",
    "YHLQMDLG" = "YHLQMDLG",
    "Oasis" = "oasis",
    "x100pre" = "x100pre",
    "Colaboración / Remix" = "Colaboracion/remix",
    "Sencillo" = "Sencillo")

#### APP ####
ui <- navbarPage(
    title = "APRENDIENDO VISUALIZACIÓN CON BAD BUNNY",
    collapsible = TRUE,
    inverse     = TRUE,
    theme       = shinytheme("simplex"),
    
    shiny::tabPanel(title = "STATS",
                    fluid = T,
                    icon = icon("globe-americas"),
                    sidebarLayout(
                        sidebarPanel(
                            img(src= "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQtffE_egpIVQcGprMp4n4s-Vzf-DObjYzN4n6bP0D11QCKaE8MQDdwRGxL3ukLJjwtXnA&usqp=CAU", width = 280),
                            hr(),
                            h5(strong("Selección:")),
                            checkboxGroupInput(inputId = "albumsID",
                                               label = strong("ALBUMS:"),
                                               choices = c("Un verano sin ti" = "un verano sin ti",
                                                           "El último tour" = "el ultimo tour",
                                                           "Las que no iban a salir" = "Las que no iban a salir",
                                                           "YHLQMDLG" = "YHLQMDLG",
                                                           "Oasis" = "oasis",
                                                           "x100pre" = "x100pre",
                                                           "Colaboración / Remix" = "Colaboracion/remix",
                                                           "Sencillo" = "Sencillo"),
                                               selected = c("Un verano sin ti" = "un verano sin ti",
                                                            "El último tour" = "el ultimo tour",
                                                            "Las que no iban a salir" = "Las que no iban a salir",
                                                            "YHLQMDLG" = "YHLQMDLG",
                                                            "Oasis" = "oasis",
                                                            "x100pre" = "x100pre",
                                                            "Colaboración / Remix" = "Colaboracion/remix",
                                                            "Sencillo" = "Sencillo")),
                            br(),
                            h5(strong("WordCloud:")),
                            sliderInput("max",
                                        "Número máximo de Palabras:",
                                        min = 1,  max = 100,  value = 25),
                            br(),
                            h5(strong("Palabras Largas:")),
                            sliderInput("max2",
                                        "Número máximo de Palabras:",
                                        min = 1,  max = 25,  value = 13),
                            
                            br(),
                            helpText(strong("Elaborado por:")),
                            helpText("Fernanda Jurado M."),
                            helpText(strong("Colaboración:")),
                            helpText("María José Jurado M."),
                            helpText("Roberto Esteves")
                            
                        ),
                        mainPanel(
                            fluidRow(
                              box(title = "Participación de canciones por albúm", solidHeader = T,
                                  collapsible = F, plotOutput("pastel")),
                              box(title = "Palabras más usadas", solidHeader = T,
                                collapsible = F, plotlyOutput("SumDays1"))),
                              
                            fluidRow( 
                              box(title = "Conteo de palabras por su número de caracteres", solidHeader = T,
                                  collapsible = F, plotlyOutput("SumWords")),
                              box(title = "WordCloud", solidHeader = T,
                                  collapsible = F, plotOutput("WC"))),
                            fluidRow(
                                box(title = "Diversidad de léxico", solidHeader = T,
                                    collapsible = F, plotlyOutput("DivLex")),
                                box(title = "Densidad de léxico", solidHeader = T,
                                    collapsible = F, plotlyOutput("DenLex"))),
                            fluidRow(   
                                box(title = "Palabras Largas", solidHeader = T,
                                  collapsible = F, plotOutput("LW")),
                                box(title = "Bigramas", solidHeader = T,
                                  collapsible = F, plotlyOutput("Bigram")))
                              
                          
                        )
                    )
    )
)




server <- function(input, output, session) {
  

############TAP 1
    terms1 <- reactive({
        req(input$albumsID)
        terms1 = BB_canciones2 %>% filter(album %in% input$albumsID)
    })
    
    
    terms6 <- reactive({
      req(input$albumsID)
      terms6 = BB_canciones %>% filter(album %in% input$albumsID) 
    })
    
    terms7 <- reactive({
      req(input$albumsID)
      bigramas <- Bad_bunny_canciones %>% filter(album %in% input$albumsID) %>% 
        mutate(texto = limpiar(letra)) %>%
        select(texto) %>%
        unnest_tokens(input = texto, output = "bigrama",
                      token = "ngrams",n = 2, drop = TRUE)
      
      
      # Separación de los bigramas 
      bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                                 sep = " ")
      
      # Filtrado de los bigramas que contienen alguna stopword
      bigrams_separados <- bigrams_separados  %>%
        filter(!palabra1 %in% tm::stopwords(kind="es")) %>%
        filter(!palabra2 %in% tm::stopwords(kind="es"))
      
      # Unión de las palabras para formar de nuevo los bigramas
      terms7 <- bigrams_separados %>%
        unite(bigrama, palabra1, palabra2, sep = " ")
    })
    


    # ###############################TAP 1 ##############################################
    
    output$pastel = renderPlot({
      datos = as_tibble(terms1())
      datos %>%
      group_by(album) %>%
      summarise( Frec= n()) %>%
      arrange(-Frec) %>% 
      mutate(Prop= Frec/sum(Frec)*100,
             PosLab=cumsum(Prop)-0.5*Prop) %>%       # por porcentajes
      ggplot(aes(x="", y=Prop, fill= reorder(album, Frec))) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5)) +
      geom_text(aes(y = PosLab, label = paste(round(Prop,2), "%", sep = " ")), color = "white", size=4)
    })
    
  
    #### NUBE DE PALABRAS 
    output$WC = renderPlot({
        datos = as_tibble(terms1())
        datos %>% dplyr::group_by(token) %>% count() %>% arrange(desc(n)) %>% 
          mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))) %>% 
          head(input$max) %>% top_n(input$max) %>% 
            ggplot(aes(label = token, 
                       color = token, 
                       size = n),
                      angle = angle) +
            geom_text_wordcloud_area(area_corr_power = 1)+
            scale_size_area(max_size = 20)+
            theme_minimal()
    })
    
   
    
    #### TOTAL DE PALABRAS
    output$SumDays1 = renderPlotly({
        data = as_tibble(terms1())
        SumWords_plot = data %>% group_by(album, token) %>% count(token) %>% group_by(album) %>%
            top_n(12) %>% head(10) %>% arrange(album, desc(n)) %>%
            ggplot(aes(x = reorder(token,n), y = n, fill = album)) +
            geom_col() +
            theme_bw() +
            labs(y = "", x = "") +
            theme(legend.position = "none") +
            coord_flip()
        ggplotly(SumWords_plot, tooltip = "n")

    })
    
    ### TOTAL LARGO PALABRAS VS TOTAL PALABRAS
    output$SumWords = renderPlotly({
        data = as_tibble(terms1())
    data %>% mutate(len = nchar(token)) %>%
        ggplot(aes(len),
               binwidth = 20) +
        geom_histogram(aes(fill = ..count..),
                       breaks = seq(1,20, by = 1),
                       show.legend = FALSE) +
        xlab("Número de caracteres por palabra") +
        ylab("Conteo de palabras") +
        theme(plot.title = element_text(hjust = 0.5),
              panel.grid.minor = element_blank()) +
        theme_minimal()
    })
    # 
    #### PALABRAS MÁS LARGAS 
    output$LW = renderPlot({
        datos = as_tibble(terms1())
        datos %>% group_by(token) %>% count() %>% arrange(desc(n)) %>%
            mutate(len = nchar(token)) %>% arrange(desc(len)) %>% head(input$max2) %>%
            ggplot(aes(label = token,
                       color = token,
                       size = n)) +
            geom_text_wordcloud_area(area_corr_power = 1)+
                scale_size_area(max_size = 10)+
                    theme_minimal()
     })
    
    ##### DIVERSIDAD LEXICO
    output$DivLex = renderPlotly({
        data = as.tibble(terms1())
        diversity = data %>%
        group_by(cancion, año) %>%
        summarise(lexico = n_distinct(token)) %>%
        arrange(desc(lexico)) %>%
        ggplot(aes(año, lexico)) +
        geom_point(color = "red",
                   alpha = .4,
                   size = 4,
                   position = "jitter") +
        stat_smooth(color = "black", se = FALSE, method = "lm") +
        geom_smooth(aes(x = año, y = lexico), se = FALSE,
                    color = "blue", lwd = 2) +
        theme_classic()

    })
    
    ### DENSIDAD LEXICO
    output$DenLex = renderPlotly({
    datos = as_tibble(terms6())
    density = datos %>% group_by(cancion, año) %>% 
      summarise(lexico = n_distinct(token)/n()) %>% 
      arrange(desc(lexico)) %>% 
      ggplot(aes(año, lexico)) +
      geom_point(color = "red",
                 alpha = .4, 
                 size = 4, 
                 position = "jitter") + 
      stat_smooth(color = "black", se = FALSE, method = "lm") +
      geom_smooth(aes(x = año, y = lexico), se = FALSE,
                  color = "blue", lwd = 2) +
      theme_classic()
    })

    ### BIGRAMA
    output$Bigram = renderPlotly({
    datos = as_tibble(terms7())    
    datos %>% group_by(bigrama) %>% summarise(total=n()) %>% 
      arrange(desc(total)) %>%  head(20) %>% top_n(20) %>%  
      ggplot(aes(x = reorder(bigrama,total), y = total, fill = "deeppink")) +
      geom_col() +
      theme_minimal() +
      labs(y = "", x = "") +
      theme(legend.position = "none") +
      coord_flip() +
      labs(title = "Conteo de Bigramas", x = "Bigramas",
           y = "Cantidad")
    })
 
    
}


shinyApp(ui, server)
