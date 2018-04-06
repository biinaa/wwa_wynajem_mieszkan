library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(forcats)
library(scales)
library(knitr)
library(rmarkdown)
library(RJSONIO)

shinyServer(function(input, output) {
    
    load('data/dane_olx.RData')
    load('data/mapa_warszawa.RData')
    
    dane_filtr_shiny <-reactive({
        
        dane_olx_analiza <- left_join(dane_olx_analiza, dic_dzielnice, by = c("dzielnica" = "Dzielnica"))
        
        dane_filtr <- dane_olx_analiza[dane_olx_analiza$kto %in% input$kto &
                                       dane_olx_analiza$rodzaj_zabudowy %in% input$rodzaj_zabudowy, ]
        
        dane_filtr_sr <- dane_filtr %>%
            group_by(dzielnica, Teryt) %>%
            summarise(sr_cena = mean(cena_najmu_m2), ile_ogloszen = n())
        
        polaczone <- shape_wwa_wgs84
        
        polaczone@data <- suppressWarnings(left_join(polaczone@data, dane_filtr_sr, by = c("jpt_kod_je" = "Teryt")))
        
        polaczone
    })
    
    dane_olx_shiny <-reactive({
        
        dane_olx_filtr <- dane_olx_analiza[dane_olx_analiza$kto %in% input$kto &
                                           dane_olx_analiza$rodzaj_zabudowy %in% input$rodzaj_zabudowy, ]
        
        dane_olx_filtr
    })
   
    output$mapa_warszawa <- renderLeaflet({
        
        dane_mapa <- dane_filtr_shiny()
        
        pal <- colorNumeric(
            palette = "RdYlGn",
            domain = dane_mapa@data$sr_cena,
            reverse = T)
        
        
        popup_lista <- paste0("<strong>Dzielnica: </strong>", 
                              "<br>", dane_mapa@data$dzielnica, 
                              "<br><strong>Średnia cena najmu (za 1 m2): </strong>",
                              "<br>",round(dane_mapa@data$sr_cena, 2), " zł",
                              "<br><strong>Liczba ogłoszeń: </strong>",
                              "<br>", dane_mapa@data$ile_ogloszen)
        
        leaflet(dane_mapa) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = TRUE, weight = 0.5, smoothFactor = 0.2, 
                        fillOpacity = 0.8, color = "black",
                        fillColor = ~pal(dane_mapa@data$sr_cena),
                        popup = popup_lista) %>%
            addLegend(pal = pal, values = ~dane_mapa@data$sr_cena,
                      title = "Śr cena najmu (za 1 m2)",
                      labFormat = labelFormat(suffix = " zł"),
                      opacity = 0.8)     
        
    })
    
    
    output$wykres_dzielnice <- renderPlot({

        dane_plot <- dane_olx_shiny()
        
        dane_plot$pokoje_faktor <- factor(dane_plot$pokoje, ordered = T, levels = c("Kawalerka", "2 pokoje", "3 pokoje", "4 i więcej"))
        
        ggplot(dane_plot) +
            geom_bar(aes(dzielnica, fill = pokoje)
            ) + 
            coord_flip() + 
            scale_fill_tableau() +
            theme_minimal() + 
            labs(title = "Ogłoszenia według dzielnic", y = "Ile ogłoszeń", x = "Dzielnica", fill = "Ile pokoi")


    })
    
    output$wykres_powierzchnia <- renderPlot({
        
        dane_plot <- dane_olx_shiny()
        
        ggplot(dane_plot) +
            geom_density(aes(powierzchnia, fill = pokoje), alpha = 0.7, color = NA) +
            scale_color_tableau() +
            scale_fill_tableau() +
            scale_y_continuous(labels = NULL) + 
            theme_minimal() +
            labs(title="Rozkład ogłoszeń według powierzchni",y = "Udział ogłoszeń", x = "Powierzchnia (w m2)", fill = "Ile pokoi")
        
        
    })
    
    output$wykres_cena <- renderPlot({
        
        dane_plot <- dane_olx_shiny()
        
        ggplot(dane_plot) +
            geom_density(aes(cena_najmu, fill = pokoje), alpha = 0.7, color = NA) +
            scale_color_tableau() +
            scale_fill_tableau() +
            scale_y_continuous(labels = NULL) + 
            scale_x_continuous(labels = dollar_format(prefix = "", suffix = " zł", big.mark = " "),
                               limits = c(0,10000)) +
            theme_minimal() +
            labs(title="Rozkład ogłoszeń według ceny najmu", y = "Udział ogłoszeń", x = "Cena najmu", fill = "Ile pokoi")
        
        
    })

})
