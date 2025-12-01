# Comparing OPR to Rank 
# Gabriel Krotkov
# 12/1/2025

# I'll pick OPR as my measure of team quality because it's tied to a particular
# event, like team rank is. 

library(tidyverse)
library(scoutR) # devtools::install_github("gkrotkov/scoutR")

event_key <- "2023txwac"

make_viz <- function(event_key){
    ranks <- event_rankings(event_key)
    ranks <- ranks |>
        dplyr::select(team = team_key, rank)
    
    coprs <- event_coprs(event_key)
    coprs <- coprs |>
        dplyr::select(team, opr = totalPoints)
    
    result <- merge(ranks, coprs, by = "team")
    
    return(result)
}

viz <- make_viz(event_key)

ggplot(viz, aes(x = rank, y = opr)) + 
    geom_point() + 
    labs(title = paste("OPR against Rank for", event_key), 
         x = "Rank", y = "OPR") + 
    theme_bw()
