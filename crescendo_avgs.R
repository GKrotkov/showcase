# How many notes did an alliance score on average per week of 2024?

rm(list = ls())
library(scoutR)
library(tidyverse)

# Let's assume we're interested in figuring out what would be required to be 
# in the top eighth - because there is one winning playoffs alliance 
t8th_pct <- 1 - (1 / 8)
b8th_pct <- 1 / 8
z_crit <- pnorm(t8th_pct)

########################
#### Data Wrangling ####
########################

week_keys <- function(wk, level = c("qualifier", "dcmp", "cmp")){
    level <- match.arg(level)
    level_code <- switch(
        level, 
        qualifier = c(0, 1), dcmp = c(2, 5), cmp = c(3, 4)
    )
    officials <- events(year = 2024, official = TRUE)
    keys <- officials |>
        filter(
            (week == wk - 1) | level == "cmp", # tba uses 0-indexing
            event_type %in% level_code
        ) |>
        pull(key)
    return(keys)
}

count_alliance_notes <- function(match_data){
    result <- match_data |>
        mutate(
            red_notes = red_teleopSpeakerNoteCount + red_teleopAmpNoteCount +
                red_teleopSpeakerNoteAmplifiedCount + red_autoAmpNoteCount + 
                red_autoSpeakerNoteCount + (red_endGameNoteInTrapPoints / 5),
            blue_notes = blue_teleopSpeakerNoteCount + blue_teleopAmpNoteCount +
                blue_teleopSpeakerNoteAmplifiedCount + blue_autoAmpNoteCount + 
                blue_autoSpeakerNoteCount + (blue_endGameNoteInTrapPoints / 5)
        ) |>
        select(
            key, red_notes, blue_notes
        )

    return(result)
}

week_count_notes <- function(week, level = "qualifier"){
    keys <- week_keys(week, level = level)
    match_data <- lapply(keys, event_matches, match_type = "all")
    match_data <- bind_rows(match_data)
    
    qual_matches <- match_data |>
        filter(comp_level == "qm") |>
        count_alliance_notes()

    playoff_matches <- match_data |>
        filter(comp_level != "qm") |>
        count_alliance_notes()

    # uses vector recycling to match keys to matches
    result <- rbind(
        data.frame(week = week, type = "qual", key = qual_matches$key, 
                   level = level, notes = c(qual_matches$red_notes, 
                                            qual_matches$blue_notes)), 
        data.frame(week = week, type = "playoff", key = playoff_matches$key,
                   level = level, notes = c(playoff_matches$red_notes, 
                                            playoff_matches$blue_notes))
    )
    return(result)
}

###################
#### API Calls ####
###################

# refactored from previous approach so we can retain the data (reduce the 
# amount we're bugging TBA), and do more analysis than just avg/sd

result <- list()
for (i in 1:6){
    result[[i]] <- week_count_notes(i, level = "qualifier")
}

result[[7]] <- week_count_notes(6, level = "dcmp")
result[[8]] <- week_count_notes(NA, level = "cmp")

result <- bind_rows(result)

result <- result |>
    mutate(
        stage = case_when(
            level == "qualifier" ~ week, 
            level == "dcmp" ~ 7, 
            level == "cmp" ~ 8
        )
    )

#######################
#### Vizualization ####
#######################

week_labs <- c("Week 1", "Week 2", "Week 3", "Week 4", 
               "Week 5", "Week 6", "DCMP", "CMP")

viz <- result |>
    group_by(type, stage) |>
    summarize(avg = round(mean(notes), 2), sd = round(sd(notes), 2), 
              median = median(notes), t8th = quantile(notes, t8th_pct), 
              b8th = quantile(notes, b8th_pct)) |>
    mutate(type = factor(type, levels = c("qual", "playoff")), 
           stage = factor(stage, levels = 1:8, labels = week_labs), 
           lower = avg - sd, upper = avg + sd)

ggplot(viz, aes(x = stage, y = avg, color = type, group = type)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  width = 0.5, position = "dodge", 
                  linewidth = 0.75) +
    geom_line(position = position_dodge(width = 0.5), 
              linewidth = 0.75) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 18, lty = 2, col = "black") + 
    annotate("text", x = 1.6, y = 19, label = "Melody") +
    labs(title = "How many notes did alliances score in Crescendo?",
         subtitle = "Bounds show +/- 1 SD",
         x = "Week", y = "Average Notes") +
    theme_bw()

ggplot(viz, aes(x = stage, y = median, color = type, group = type)) +
    geom_errorbar(aes(ymin = b8th, ymax = t8th), 
                  width = 0.5, position = "dodge", 
                  linewidth = 0.75) +
    geom_line(position = position_dodge(width = 0.5), 
              linewidth = 0.75) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 18, lty = 2, col = "black") + 
    annotate("text", x = 1.6, y = 19, label = "Melody") +
    labs(title = "How many notes did alliances score in Crescendo?",
         subtitle = "Bounds show [12.5%, median, 87.5%]",
         x = "Week", y = "Median Notes") +
    theme_bw()

########################
#### plot graveyard ####
########################

# ggplot(viz, aes(x = week, y = avg, color = level)) +
#     geom_point() + 
#     geom_line() +
#     geom_hline(yintercept = 18, lty = 2, col = "black") + 
#     annotate("text", x = 2, y = 18.8, label = "RP Threshold") +
#     labs(title = "Crescendo Alliance Performance",
#          x = "Week", y = "Average Notes") +
#     theme_bw()
# 
# ggplot(viz, aes(x = week, y = avg, color = level)) +
#     geom_crossbar(aes(ymin = lower, ymax = upper), 
#                   width = 0.4, position = "dodge") +
#     geom_hline(yintercept = 18, lty = 2, col = "black") + 
#     annotate("text", x = 2, y = 18.8, label = "RP Threshold") +
#     labs(title = "Crescendo Alliance Performance",
#          subtitle = "Bounds represent +/- 1 SD",
#          x = "Week", y = "Average Notes") +
#     theme_bw()
# 
# ggplot(viz, aes(x = week, y = avg, color = level)) +
#     geom_crossbar(aes(ymin = lower, ymax = upper), 
#                   width = 0.2, position = "dodge") +
#     geom_line() +
#     geom_hline(yintercept = 18, lty = 2, col = "black") + 
#     annotate("text", x = 1.5, y = 18.8, label = "RP Threshold") +
#     labs(title = "Crescendo Alliance Performance",
#          subtitle = "Bounds represent +/- 1 SD",
#          x = "Week", y = "Average Notes") +
#     theme_bw()
# 
# ggplot(viz, aes(x = week, y = avg)) + 
#     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3) + 
#     geom_line() +
#     facet_wrap(~ level) +
#     geom_hline(yintercept = 18, lty = 2, col = "red") + 
#     annotate("text", x = 2, y = 20, label = "RP Threshold") +
#     labs(title = "How many notes did alliances score in Crescendo?", 
#          subtitle = "Bounds represent +/- 1 SD", 
#          x = "Week", y = "Average Notes") +
#     theme_bw()
