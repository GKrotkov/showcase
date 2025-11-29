library(scoutR)
library(tidyverse)
blair_red <- "#a7000a" # very important to get this exactly right, obvs


# Intermediate Data Science Exercise #1 Key

# The goal with this code is to analyze the competitive strength of the 2025
# Chesapeake district qualifier events and show some fun statistics for each 
# of them. 

# user-defined function to return a vector of the playoff teams at an event
get_playoff_teams <- function(event_key){
    matches <- event_matches(event_key, match_type = "playoff")
    return(unique(unlist(matches[, c("red1", "red2", "red3", 
                                     "blue1", "blue2", "blue3")])))
}

# "2025chcmp" is the event code for the 2025 Chesapeake District Championship
dcmp_playoff_teams_25 <- get_playoff_teams("2025chcmp")

# now let's get all the playoff teams from 2023-2025
dcmp_playoff_teams_2325 <- unique(c(
    get_playoff_teams("2023chcmp"), 
    get_playoff_teams("2024chcmp"), 
    dcmp_playoff_teams_25
))

# get a vector of all the 2026 Chesapeake districts
# setdiff() is removing "2026chcmp" from the list; why is that?
fch_qualifiers_26 <- setdiff(district_events("2026fch", keys = TRUE), 
                             "2026chcmp")

# the "lapply" function can call the "event_teams" function on every input 
# event, and return the result as a list
qualifier_teams <- lapply(fch_qualifiers_26, event_teams, keys = TRUE)
names(qualifier_teams) <- fch_qualifiers_26

# For each district, how many of those teams have played in the district champs
# playoffs in the past 3 years? (2023-2025)
qualifier_dcmp_playoffs_2325 <- sapply(
    qualifier_teams, 
    function(x){return(sum(x %in% dcmp_playoff_teams_2325))}
)
# For each district, how many of those teams play in the district champs 
# playoffs last year?
qualifier_dcmp_playoffs_25 <- sapply(
    qualifier_teams, 
    function(x){return(sum(x %in% dcmp_playoff_teams_25))}
)

# statbotics' API prefers teams in numeric format, rather than "frc1234" format
# so we'll use the "id2int" function from scoutR to do the conversion, with 
# lapply to apply it to all the teams
# for an extra challenge: notice that we're using ::: instead of ::
# what is the difference between those operators?
qualifier_teams <- lapply(qualifier_teams, scoutR:::id2int)

# Ok, that's cool, but making the DCMP playoffs is hardly the only useful metric
# of team quality. Let's look at a more general one - EPA. Check out 
# statbotics.io, and especially the blogs there, to understand what EPA is. 
# But in short, it's one estimate of how many points we expect teams to score in 
# a given match. 

# function to get 2025-end EPAs
get_25_epas <- function(team_keys){
    epas <- c()
    for (i in seq_along(team_keys)){
        tmp <- team_years_sb(team = team_keys[i], year = 2025)
        if (length(tmp) == 0) next()
        epas <- c(epas, tmp[[1]]$epa$total_points$mean)
    }
    return(epas)
}

# apply our 2025-EPA-getter to all qualifiers
qualifier_epas <- sapply(qualifier_teams, get_25_epas)
# what's the average EPA at each qualifier? 
# I'm using sapply (simplify-apply) here because I'd like to return a vector 
# instead of a list
qualifier_mean_epas <- round(sapply(qualifier_epas, mean), digits = 2)
# what's the standard deviation of EPA at each qualifier?
qualifier_sd_epas <- round(sapply(qualifier_epas, sd), digits = 2)

# let's visualize that metric in a way that makes sense - it's a point estimate
# (the EPA mean) with a confidence band around it (the SD)
viz <- data.frame(
    event = fch_qualifiers_26,
    mean = qualifier_mean_epas, 
    sd = qualifier_sd_epas
)

ggplot(viz, aes(x = event, y = mean)) + 
    geom_point(color = blair_red) + 
    geom_errorbar(aes(ymin = mean - (0.5 * sd), ymax = mean + (0.5 * sd))) + 
    labs(title = "2026 Chesapeake District Strength", 
         subtitle = "Errorbars represent 0.5 SD", 
         x = "Event Key", y = "Mean 2025-End EPA") + 
    theme_bw()

# how good is the last team to make the playoffs?
bubble_epas <- sapply(qualifier_epas, function(x){sort(x, decreasing = TRUE)[24]})
