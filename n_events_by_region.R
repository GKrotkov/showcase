# Compute average number of events by region
# Define region as state/province for USA/Canada, Country otherwise

rm(list = ls())

library(scoutR)
library(tidyverse)

YEAR <- 2025

reefscape_teams <- teams(0, year = YEAR)
for (i in 1:25){
    reefscape_teams <- rbind(reefscape_teams, teams(i, year = YEAR))
}

reefscape_teams <- reefscape_teams |>
    mutate(
        region = ifelse(!(country %in% c("USA", "Canada")), 
                        country, state_prov)
    ) |>
    mutate(
        region = case_when(
            region %in% c("Texas", "New Mexico") ~ "FiT", 
            region %in% c("Maryland", "Virginia", "District of Columbia") ~ "CHS", 
            region %in% c("Pennsylvania", "New Jersey", "Delaware") ~ "FMA", 
            region %in% c("Maine", "Massachusetts", "Rhode Island", "Vermont", 
                          "New Hampshire", "Connecticut") ~ "NE", 
            region %in% c("Oregon", "Washington") ~ "PNW", 
            .default = region
        )
    )

regions <- reefscape_teams |>
    dplyr::select(city, country, key, name, nickname, rookie_year, state_prov, 
                  team_number, region)

teams_by_region <- split(reefscape_teams$team_number, reefscape_teams$region)

count_precmp_events <- function(tm){
    evnts <- team_events(tm, year = YEAR, official = TRUE)
    evnts <- evnts |>
        filter(
            (event_type %in% c(0, 1, 5)) | 
                (event_type == 2 & !(key %in% parent_event_key))
        )
    return(nrow(evnts))
}

region_avg_precmp_events <- function(region){
    counts <- sapply(region, count_precmp_events)
    return(mean(counts, na.rm = TRUE))
}

avgs <- c()

for (i in seq_along(teams_by_region)){
    avgs <- c(avgs, region_avg_precmp_events(teams_by_region[[i]]))
}

df <- data.frame(
    avg_events = round(avgs, 2), region = names(teams_by_region), 
    n_teams = sapply(teams_by_region, length)
)

df$district <- df$region %in% c("CHS", "Michigan", "FiT", "Indiana", "Israel", 
                                "FMA", "North Carolina", "South Carolina", 
                                "NE", "Ontario", "PNW", "Georgia")

write_csv(df, "avg_events_by_region.csv")
