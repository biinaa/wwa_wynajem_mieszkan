library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(ggthemes)

shinyUI(
   
  fluidPage(theme = shinytheme("yeti"),

  titlePanel("Wynajem mieszkań w Warszawie"),
  
  sidebarLayout(
    sidebarPanel(h4("Krótko o aplikacji"),
                 h5("Aplikacja pokazuje sytuację na warszawskim rynku wynajmu 
                    w oparciu o ogłoszenia umieszczone na popularnym portalu olx.pl. 
                    Prezentowane analizy zostały wykonane z wykorzystaniem 2000 ogłoszeń."),
                 h6("Stan danych na: 2018-04-02"),
                 h2(" "),
                 h2(" "),
                 h4("Filtr"),
    checkboxGroupInput("kto",
                  h5( "Od kogo ogłoszenie:"),
                   choices = list("Osoba prywatna" = "Osoby prywatnej",
                     "Pośrednik" = "Biuro / Deweloper"),
                   selected = c("Osoby prywatnej")),
    
    # radioButtons("kto", 
    #              h5( "Od kogo ogłoszenie:"),
    #              choices = list("Osoba prywatna" = "Osoby prywatnej", 
    #                             "Pośrednik" = "Biuro / Deweloper"),
    #              selected = "Osoby prywatnej")
    
    selectizeInput("rodzaj_zabudowy",
                   h5("Wybierz rodzaj zabudowy: "),
                   choices = c("Apartamentowiec", "Blok", "Dom wolnostojący",
                               "Kamienica", "Loft", "Pozostałe", "Szeregowiec"),
                   selected = c("Apartamentowiec", "Blok", "Dom wolnostojący",
                                "Kamienica", "Loft", "Pozostałe", "Szeregowiec"),
                   multiple = TRUE)
    
    ),
    
    mainPanel(
        tabsetPanel(
            tabPanel("Mapa", leafletOutput("mapa_warszawa", height=400)),
            tabPanel("Dzielnice", plotOutput("wykres_dzielnice")),
            tabPanel("Powierzchnia mieszkania", plotOutput("wykres_powierzchnia")),
            tabPanel("Cena wynajmu", plotOutput("wykres_cena"))
                    )
            )

        )

    )
)
