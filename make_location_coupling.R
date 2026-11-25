require(tidyverse)

dloc <- read.csv2("config/DONARlocaties.csv") %>%
  mutate(koppellabel = trimws(tolower(Voorkeurslabel))) %>%
  select(
    koppellabel, Donarlocatiecode, Donarlocatienaam
  )

lmwloc <- read.csv2("config/LMWlocaties.csv")

wadarloc <- read.csv2("config/RWSlocaties.csv") %>%
  mutate(koppellabel = trimws(tolower(sub("^.*?,\\s*", "", Voorkeurslabel)))) %>%
  select(
    koppellabel, Fenomeenlocatiecode
  )

# werkt wel voor bij. Strand Horst, maar niet voor bijv Terschelling 20 km uit de kust

# mogelijk nog beter zou zijn om de koppelcode zo te veranderen:
# Donarlocatienaam alle spaties en punten verwijdern, daarna alles tot en met de eerste komma verwijderen
# wadar Fenomeenlocatiecode alle punten verwijderen


# check
dloc %>% filter(grepl("strand horst", koppellabel, ignore.case = T)) %>%
  select(koppellabel) %>% unlist %>% unname ==
    wadarloc %>%filter(grepl("strand horst", koppellabel, ignore.case = T)) %>%
  select(koppellabel) %>% unlist %>% unname

koppeling_locaties <- dloc %>% full_join(wadarloc, by = c(koppellabel = "koppellabel"))

koppeling_locaties %>%
  select(
    # Voorkeurslabel,
    koppellabel,
    Donarlocatiecode,
    Donarlocatienaam,
    Fenomeenlocatiecode,
  ) %>%
  filter(grepl("strand horst", koppellabel, ignore.case = T)) %>%
View()
