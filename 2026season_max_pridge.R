# given an event key, compute season-max pRidge values

library(scoutR)
library(tidyverse)
blair_red <- "#a7000a"
div_keys <- c("2026arc", "2026cur", "2026dal", "2026gal", 
              "2026hop", "2026joh", "2026mil", "2026new")
div_names <- c("Archimedes", "Curie", "Daly", "Galileo", 
               "Hopper", "Johnson", "Milstein", "Newton")

# ... honestly probably faster to just calculate pRidge for every team at
# every event and just query that, huh?
event_keys <- events(2026) |>
    filter(event_type %in% c(0, 1, 2), 
           map_int(division_keys, length) == 0) |>
    pull(key)

fxn_wrapper <- function(event_key){
    result <- tryCatch(
        fit_event_pridge(event_key), 
        error = function(e){
            return(NA)
        })
    return(result)
}

raw_pridge_data <- lapply(event_keys, fxn_wrapper)

event_pridges <- lapply(
    raw_pridge_data, 
    function(item){
        tibble(team = scoutR:::id2int(names(item)), pridge = item)
    }
)

for (i in seq_along(event_pridges)){
    event_pridges[[i]]$event <- event_keys[i]
}

event_pridges <- bind_rows(event_pridges)

team_max_pridges <- event_pridges |>
    group_by(team) |>
    summarize(pridge_max = max(pridge))

cmp_divs <- lapply(
    div_keys,  
    function(event_id){
        sort(scoutR:::id2int(event_teams(event_id, keys = TRUE)))
    }
)

names(cmp_divs) <- div_keys

div_max_pridges <- lapply(
    cmp_divs, 
    function(team_keys, lookup){
        return(lookup[lookup$team %in% team_keys, ])
    }, 
    team_max_pridges
)

for (i in seq_along(div_max_pridges)){
    viz <- div_max_pridges[[i]] |>
        arrange(desc(pridge_max))
    
    ggplot(viz, aes(x = reorder(as.character(team), pridge_max), 
                    y = pridge_max)) + 
        geom_bar(stat = "identity", fill = blair_red, color = "black") + 
        geom_text(aes(label = round(pridge_max, 1)), 
                  hjust = 1.1, 
                  color = "white", 
                  size = 3.0) +
        theme_bw() + 
        coord_flip() +
        labs(x = "Team", y = "Season-Max pRidge", 
             title = paste(div_names[i], "2026 Season-Max pRidge"), 
             subtitle = "Excluding teams without official events")
    
    ggsave(filename = paste0("plots/", "2026", div_names[i], ".png"), 
           height = 12, units = "in")
}
