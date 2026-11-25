
require(tidyverse)
require(stringr)
require(fuzzyjoin)

koppeling_meetpunten_rijkswateren <- read_delim("config/koppeling_meetpunten_rijkswateren.csv", 
                                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

url_sovon = "data\\raw\\SOVON\\koppeling_gebieden.csv"
sovon_koppeling <- read_csv2(url_sovon, col_types = "cccc") %>%
  mutate(website = str_replace(website, "gebieden", "gebied")) %>%
  mutate(url = paste(website, gebiedscode, sep = "/"))

joined <- sovon_koppeling %>%
  fuzzyjoin::stringdist_right_join(
    koppeling_meetpunten_rijkswateren %>% distinct(watersysteem_id), 
    by = c(gebied = "watersysteem_id"))



mapping = c(
  `Afgedamde Maas` = "",
    `Afgesloten IJ` = "",
    Amer = "",
    `Amsterdam-Rijnkanaal` = "",
    `Bathse Spuikanaal` = "",
    `Beneden Merwede` = "",
    `Bergsche Maas` = "",
    `Boven-Rijn` = "",
    `Brabantse Biesbosch` = "",
    `Haringvliet` = "Haringvliet",
    `Havens, kreken en killen bij Gors d’n Anwas, ter hoogte van Woudrichem` = "",
    `Buiten-IJ` = "",
    `Buitenhaven van IJmuiden` = "",
    `Calandkanaal` = "",
    `Bijlandsch Kanaal` = "",
    Lek = "",
    `Dordtsche Kil` = "",
    `Eems, Dollard` = "",
    Eemskanaal = "",
    Lekkanaal = "",
    Maas = "",
    `Maas-Waalkanaal` = "",
    Nederrijn = "",
    Grevelingenmeer = "",
    `Hollandsch Diep` = "Hollands Diep",
    `Hollandsche IJssel` = "",
    IJssel = "",
    IJsselmeer = "IJsselmeer",
    Julianakanaal = "",
    `Nieuwe Maas` = "",
    `Nieuwe Waterweg` = "",
    `Kanaal van Gent naar Terneuzen` = "",
    `Kanaal van St. Andries` = "",
    `Kanaal Wessem-Nederweert` = "",
    `Lateraal kanaal` = "",
    Noord = "",
    Noordervaart = "",
    Noordzee = "",
    Noordzeekanaal = "",
    Oosterschelde = "Oosterschelde",
    `Oude Maas` = "",
    `Pannerdensch Kanaal` = "",
    `Prinses Margrietkanaal` = "",
    Randmeren = "",
    Reevediep = "",
    `Schelde-Rijnverbinding` = "",
    Markermeer = "Markermeer & IJmeer",
    Markkanaal = "",
    Maximakanaal = "",
    Meppelerdiep = "",
    Westerschelde = "Westerschelde & Saeftinghe",
    `Wilhelminakanaal - Amertak` = "",
    `Zuid-Willemsvaart (Brabantse tak)` = "",
    `Zuid-Willemsvaart (Limburgse tak)` = "",
    Spui = "",
    Twentekanalen = "",
    `Van Starkenborghkanaal` = "",
    `Veerse Meer` = "Veerse Meer",
    `Voedingskanalen van de Zuid-Willemsvaart` = "",
    `Volkerak-Zoommeer` = "",
    `Vuile Gat` = "",
    Waal = "",
    Waddenzee = "Waddenzee",
    `Zwarte Water` = "",
    `Zwolle-IJsselkanaal` = ""
)