# bouw configuratie uit WFS

source("r/inventory.R")
library(sf)

metadata = rwsapi::rws_metadata()
parameters <- unique(metadata$content$aquometadatalijst$parameter_wat_omschrijving)
parameters

## Geparalelliseerd gegevens ophalen en 
## per parameter locaties selecteren waar recent (vanaf 2025) gemeten is

library(future.apply)
library(progressr) # nog even installeren


## oppervlaktewater opgeloste stoffen

targetparams <- list(
  OW = parameters[grep("in Oppervlaktewater", parameters)],
  ZS = parameters[grep("in Zwevende stof", parameters)],
  BS = parameters[grep("in Bodem", parameters)],
  OR = parameters[grep("in Organisme", parameters)]
)

# testtargetparams <- sample(targetparams, size = 10)

walk(names(targetparams)[1:4],
     \(x){
       testtargetparams <- targetparams[[x]]
       plan(multisession, workers = 2) # meer kan ook, maar kan tot overbelasting van de server leiden
       handlers("progress")
       with_progress({
         p <- progressor(along = testtargetparams)  # test for smaller number
         latObsActList <- future_lapply(
           testtargetparams,
           function(par) {
             res <- safe_wfs(par = par)
             p(sprintf("parameter: %s", par))
             res
           }
         )
       })
       assign(latObsActList, x)
       saveRDS(latObsActList, file = paste0("data/raw/metadata/latestobservations_", x, "_sf.rds"))
     }
)

