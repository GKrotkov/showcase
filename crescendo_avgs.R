# How many notes did an alliance score on average per week of 2024?

rm(list = ls())
library(scoutR)
library(tidyverse)

########################
#### Data Wrangling ####
########################

week_keys <- function(wk){
    wk <- wk - 1 # tba uses 0-indexing
    officials <- events(year = 2024, official = TRUE)
    keys <- officials |>
        filter(week == wk, event_type %in% c(0, 1)) |>
        pull(key)
    return(keys)
}

matches_avg_notes <- function(match_data){
    match_data <- match_data |>
        mutate(
            red_notes = red_teleopSpeakerNoteCount + red_teleopAmpNoteCount +
                red_teleopSpeakerNoteAmplifiedCount +
                (red_endGameNoteInTrapPoints / 5),
            blue_notes = blue_teleopSpeakerNoteCount + blue_teleopAmpNoteCount +
                blue_teleopSpeakerNoteAmplifiedCount +
                (blue_endGameNoteInTrapPoints / 5)
        )

    result <- c(
        "mean" = mean(c(match_data$red_notes, match_data$blue_notes)),
        "sd" = sd(c(match_data$red_notes, match_data$blue_notes))
    )
    return(result)
}

week_alliance_avg_notes <- function(week){
    keys <- week_keys(week)

    match_data <- lapply(keys, event_matches, match_type = "all")
    match_data <- bind_rows(match_data)

    qual_matches <- match_data |>
        filter(comp_level == "qm")

    playoff_matches <- match_data |>
        filter(comp_level != "qm")

    result <- c(
        matches_avg_notes(qual_matches),
        matches_avg_notes(playoff_matches)
    )

    names(result) <- c("qual mean", "qual sd", "playoff mean", "playoff sd")

    return(round(result, digits = 2))
}

qual_avg <- rep(NA, 6)
names(qual_avg) <- paste("Week", 1:6)

qual_sd <- rep(NA, 6)
names(qual_sd) <- paste("Week", 1:6)

playoff_avg <- rep(NA, 6)
names(playoff_avg) <- paste("Week", 1:6)

playoff_sd <- rep(NA, 6)
names(playoff_sd) <- paste("Week", 1:6)

for (i in 1:6){
    tmp <- week_alliance_avg_notes(i)
    qual_avg[i] <- tmp[1]
    qual_sd[i] <- tmp[2]
    playoff_avg[i] <- tmp[3]
    playoff_sd[i] <- tmp[4]
}

#######################
#### Vizualization ####
#######################

viz <- tibble(week = 1:6, qual_avg = qual_avg, qual_sd = qual_sd, 
              playoff_avg, playoff_sd = playoff_sd)

viz <- viz |>
    pivot_longer(-week, names_to = c("level", ".value"), names_sep = "_") |>
    mutate(lower = avg - sd, upper = avg + sd, 
           level = factor(level, levels = c("qual", "playoff")))

# three options for the plot, increasing in complexity and information

ggplot(viz, aes(x = week, y = avg, color = level)) +
    geom_point() + 
    geom_line() +
    ylim(0, 20) +
    geom_hline(yintercept = 18, lty = 2, col = "black") + 
    annotate("text", x = 1.5, y = 18.8, label = "Melody RP Threshold") +
    labs(title = "Crescendo Alliance Performance",
         x = "Week", y = "Average Notes") +
    theme_bw()

ggplot(viz, aes(x = week, y = avg, color = level)) +
    geom_crossbar(aes(ymin = lower, ymax = upper), 
                  width = 0.4, position = "dodge") +
    geom_hline(yintercept = 18, lty = 2, col = "black") + 
    annotate("text", x = 1.5, y = 18.8, label = "Melody RP Threshold") +
    labs(title = "Crescendo Alliance Performance",
         subtitle = "Bounds represent +/- 1 SD",
         x = "Week", y = "Average Notes") +
    theme_bw()

ggplot(viz, aes(x = week, y = avg, color = level)) +
    geom_crossbar(aes(ymin = lower, ymax = upper), 
                  width = 0.2, position = "dodge") +
    geom_line() +
    geom_hline(yintercept = 18, lty = 2, col = "black") + 
    annotate("text", x = 1.5, y = 18.8, label = "Melody RP Threshold") +
    labs(title = "Crescendo Alliance Performance",
         subtitle = "Bounds represent +/- 1 SD",
         x = "Week", y = "Average Notes") +
    theme_bw()
