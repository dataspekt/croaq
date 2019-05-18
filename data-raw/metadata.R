stations <-
    read.csv("stations.csv", stringsAsFactors=FALSE)

pollutants <-
    read.csv("pollutants.csv", stringsAsFactors=FALSE)

datatypes <-
    read.csv("datatypes.csv", stringsAsFactors=FALSE)

usethis::use_data(stations, pollutants, datatypes,
                  overwrite = TRUE, internal = TRUE)

