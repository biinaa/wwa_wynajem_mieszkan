setwd("D:/github/mapa")

# pakiety
library(dplyr)
library(rgdal)

# wczytanie mapy
shape_jedn_adm <- readOGR(dsn = "./PRG_jednostki_administracyjne_v15", layer = "jednostki_ewidencyjne", 
                          use_iconv=TRUE, encoding="UTF-8", stringsAsFactors = FALSE)

# wygranie tylko dzielnic Warszawy
shape_wwa  <- shape_jedn_adm[substr(shape_jedn_adm@data$jpt_kod_je, 1, 4) =='1465', ]
View(shape_wwa@data)
plot(shape_wwa)

# przeliczenie układu współrzędnych
uw_wgs84 <- CRS("+init=EPSG:4326")
shape_wwa_wgs84 = spTransform(shape_wwa, uw_wgs84)

# słownik - teryt i dzielnica
dic_dzielnice <- data.frame(
    Teryt = c("146502_8", "146503_8", "146504_8", "146505_8", "146506_8",
              "146507_8", "146508_8", "146509_8", "146510_8", "146511_8",
              "146512_8", "146513_8", "146514_8", "146515_8", "146516_8",
              "146517_8", "146518_8", "146519_8"),
    Dzielnica = c("Bemowo", "Białołęka", "Bielany", "Mokotów", "Ochota",
                  "Praga-Południe", "Praga-Północ", "Rembertów", "Śródmieście", "Targówek",
                  "Ursus", "Ursynów", "Wawer", "Wesoła", "Wilanów",
                  "Włochy", "Wola", "Żoliborz"), stringsAsFactors = F
)

save(shape_wwa_wgs84, dic_dzielnice, file = 'mapa_warszawa.RData')

