setwd("D:/github/wynajem_mieszkan")

# pakiety
library(dplyr)
library(rvest)
library(stringr)
library(stringi)
library(ggplot2)

# podaj liczbę stron
liczba_stron <- 102

linki <- paste("http://www.olx.pl/nieruchomosci/mieszkania/wynajem/warszawa//?page=",1:liczba_stron,sep="")

lista_ofert <- function(link) {
    linki<-read_html(link) %>%
        html_nodes('.detailsLink') %>%
        html_attr('href') %>% unique()
    return(linki)
}

adresy_url <- sapply(linki, lista_ofert)
adresy_url <- unlist(adresy_url)
names(adresy_url) <- NULL

adresy_url <- adresy_url %>%
    as.data.frame()
colnames(adresy_url) <- "linki"

head(adresy_url)

# oferty z olx

adresy_olx <- adresy_url %>%
    filter(substr(adresy_url$linki, 1, 18) == 'https://www.olx.pl') %>%
    distinct()

dane_olx <- data.frame()

# pętla wyciągajaca dane z linków

for (i in 1: nrow(adresy_olx)) {

    URL <- as.character(adresy_olx[i, 1])
    
    pg <- read_html(URL)
    
    id_ogloszenia <- html_nodes(pg, ".offer-titlebox__details") %>% 
        html_text() %>%
        str_replace_all("[\r\n\t]" , "") %>% 
        stri_trim() %>%
        str_sub(-9,-1)
    id_ogloszenia <- ifelse(identical(id_ogloszenia, character(0)), "-", id_ogloszenia)
    
    dzielnica <- html_nodes(pg, ".offer-titlebox__details") %>% 
        html_text() %>%
        str_replace_all("Warszawa, Mazowieckie, " , "") %>% 
        str_replace_all("[\r\n\t]" , "") %>% 
        stri_trim() %>%
        substr(1, 30) %>%
        stri_trim()
    dzielnica <- ifelse(identical(dzielnica, character(0)), "-", dzielnica)
    
    cena <- html_nodes(pg, ".price-label") %>% 
        html_text() %>%
        str_replace_all("[\r\n\t]" , "") %>% 
        str_replace_all("Do negocjacji" , "") %>%
        stri_trim() 
    cena <- ifelse(identical(cena, character(0)), "-", cena)
    
    info <- html_nodes(pg, ".col") %>% 
        html_text() %>%
        str_replace_all("[\r\n\t]" , "") %>% 
        stri_trim() 
    
    kto <- info[substr(info, 1 ,9) == "Oferta od"] %>%
        str_replace_all("Oferta od" , "")
    kto <- ifelse(identical(kto, character(0)), "-", kto)
    
    poziom <- info[substr(info, 1 ,6) == "Poziom"] %>%
        str_replace_all("Poziom" , "")
    poziom <- ifelse(identical(poziom, character(0)), "-", poziom)
    
    umeblowane <- info[substr(info, 1 , 10) == "Umeblowane"] %>%
        str_replace_all("Umeblowane" , "")
    umeblowane <- ifelse(identical(umeblowane, character(0)), "-", umeblowane)
    
    rodzaj_zabudowy <- info[substr(info, 1 ,15) == "Rodzaj zabudowy"] %>%
        str_replace_all("Rodzaj zabudowy" , "")
    rodzaj_zabudowy <- ifelse(identical(rodzaj_zabudowy, character(0)), "-", rodzaj_zabudowy)
    
    powierzchnia <- info[substr(info, 1 ,12) == "Powierzchnia"] %>%
        str_replace_all("Powierzchnia" , "")
    powierzchnia <- ifelse(identical(powierzchnia, character(0)), "-", powierzchnia)
    
    pokoje <- info[substr(info, 1 ,12) == "Liczba pokoi"] %>%
        str_replace_all("Liczba pokoi" , "")
    
    pokoje <- ifelse(identical(pokoje, character(0)), "-", pokoje)
    
    czynsz <- info[substr(info, 1 ,6) == "Czynsz"] %>%
        str_replace_all("Czynsz" , "") %>%
        str_replace_all("dodatkowo" , "") %>%
        str_replace_all("[()]","") %>%
        stri_trim()
    czynsz <- ifelse(identical(czynsz, character(0)), "-", czynsz)
    
    # sprawdzenie czy w tytule ogłoszenia zawarte są frazy: "doby", "godziny", "pokój", "kwatery"
    # kolumna pomocnicza na podstawie której usunięte zostaną ogłoszenia dotyczące:
    # najmu na godziny, kwater pracowniczych, tylko pokoi
    czy_wyklucz <- sum(
        str_detect(
            toupper(html_nodes(pg, "h1") %>% html_text() %>% as.character()), 
            toupper(c("doby", "godziny", "pokój", "kwatery", "pokoju", 
                      "krótkoterminowy", "postojowe", "garaż", "croatia","д", "akademik" ))
            )
        )
    
    zlacz <- data.frame(id_ogloszenia, dzielnica, cena, kto, poziom, umeblowane, 
                        rodzaj_zabudowy, powierzchnia, pokoje, czynsz, czy_wyklucz)
    
    dane_olx <- rbind(dane_olx, zlacz)
}

View(dane_olx)
str(dane_olx)

# czyszczenie danych

table(dane_olx$czy_wyklucz)

dane_olx2 <- dane_olx

dane_olx2 <- dane_olx2 %>%
    filter(id_ogloszenia != "-")

head(dane_olx2)

dane_olx2$id_ogloszenia <- as.numeric(as.character(dane_olx2$id_ogloszenia))

dane_olx2$dzielnica <- as.character(dane_olx2$dzielnica)
table(dane_olx2$dzielnica)

dane_olx2$cena <- dane_olx2$cena %>% str_replace_all(" zł" , "") %>% str_replace_all(" " , "")
dane_olx2$cena <- as.numeric(as.character(dane_olx2$cena))

dane_olx2$kto <- dane_olx2$kto %>% droplevels() %>% as.character()

dane_olx2$poziom <- dane_olx2$poziom %>% droplevels() %>% as.character()
table(dane_olx2$poziom)

dane_olx2$umeblowane <- dane_olx2$umeblowane %>% droplevels() %>% as.character()
table(dane_olx2$umeblowane)

dane_olx2$rodzaj_zabudowy <- dane_olx2$rodzaj_zabudowy %>% droplevels() %>% as.character()
table(dane_olx2$rodzaj_zabudowy)

dane_olx2$powierzchnia <- gsub("\u00B2","", dane_olx2$powierzchnia %>% droplevels()) %>%
                    str_replace_all(" m" , "") %>% str_replace_all("," , ".")
dane_olx2$powierzchnia <- as.numeric(as.character(dane_olx2$powierzchnia))
table(dane_olx2$powierzchnia)

dane_olx2$pokoje <- dane_olx2$pokoje %>% droplevels() %>% as.character()
table(dane_olx2$pokoje)

dane_olx2$czynsz <- dane_olx2$czynsz %>% str_replace_all(" zł" , "") %>% str_replace_all(" " , "")
dane_olx2$czynsz <- as.numeric(as.character(dane_olx2$czynsz))
table(dane_olx2$czynsz)

# zastąpienie wartości mniejszych niż 5 zł oraz większych niż 5000 zł przez 0

dane_olx2$czynsz <- if_else(dane_olx2$czynsz <= 5 | dane_olx2$czynsz >5000, 0, dane_olx2$czynsz)
dane_olx2$czynsz <- if_else(is.na(dane_olx2$czynsz) == T, 0, dane_olx2$czynsz)

# korekta powierzchni dla dwóch przypadków

dane_olx2[dane_olx2$id_ogloszenia == 419424219, 8] <- 45  
dane_olx2[dane_olx2$id_ogloszenia == 423228813, 8] <- 55

# kolumna zawierająca całkowite koszty najmu:
# gdy cena najmu i czynszu wynoszą tyle samo -> weź cenę najmu
# gdy cena czynszu jest większa od ceny najmu -> weź cenę czynszu
# w pozostałych przypadkach -> weź sumę z ceny najmu i czynszu

dane_olx2$cena_najmu <- if_else(dane_olx2$cena == dane_olx2$czynsz, dane_olx2$cena,
                                if_else(dane_olx2$cena < dane_olx2$czynsz, dane_olx2$czynsz, 
                                        dane_olx2$czynsz + dane_olx2$cena)
                                )

table(dane_olx2$cena_najmu)

dane_olx2$cena_najmu_m2 <- dane_olx2$cena_najmu / dane_olx2$powierzchnia

# wyklucz przypadki:
# z opłatą za najem poniżej 500 zł
# z powierzchnią min. 10 m2

dane_olx3 <- dane_olx2 %>%
    filter(cena_najmu > 500) %>%
    filter(powierzchnia >= 10)

# histogram dla ceny najmu
ggplot(dane_olx3, aes(cena_najmu_m2, fill = pokoje)) +
    geom_histogram(binwidth = 2)

# odrzuć przypadki z ceną najmu (za m2) niższą niż 19.5
dane_olx3 <- dane_olx3 %>%
    filter(cena_najmu_m2 > 19.5)

# odrzuć przypadki z małą powierzchnią, ale nie będące kawalerkami
dane_olx3 <- dane_olx3 %>%
    filter((powierzchnia > 20 & pokoje != "Kawalerka") | pokoje == "Kawalerka")


dane_olx_analiza <- dane_olx3 %>%
    select(-czy_wyklucz)


save(dane_olx_analiza, file = 'dane_olx.RData')
