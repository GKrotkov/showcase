library(rvest)

get_match_table_scores <- function(match_tbl){
    red_scores <- match_tbl %>%
        html_elements(".redScore span") %>%
        html_text() %>%
        as.numeric()
    red_scores <- red_scores[seq(1, length(red_scores), 2)]
    
    blue_scores <- match_tbl %>%
        html_elements(".blueScore span") %>%
        html_text() %>%
        as.numeric()
    blue_scores <- blue_scores[seq(1, length(blue_scores), 2)]
    
    return(list(red = red_scores, blue = blue_scores))
}

score_summary <- function(scores){
    df <- data.frame(
        avg = mean(c(scores$red, scores$blue)), 
        winning_avg = mean(pmax(scores$red, scores$blue)), 
        margin_avg = mean(abs(scores$red - scores$blue))
    )
    # rounding to 2 digits to match TBA Insights
    return(round(df, digits = 2))
}

results_summary <- function(event_code){
    stem <- "https://www.thebluealliance.com/event/"
    html <- read_html(paste0(stem, event_code, "#results"))
    
    qual_stats <- html %>%
        html_elements("#qual-match-table") %>%
        get_match_table_scores() %>%
        score_summary()
    
    playoff_stats <- html %>%
        html_elements("#elim-match-table") %>%
        get_match_table_scores() %>%
        score_summary()
    
    stats <- t(as.matrix(rbind(qual_stats, playoff_stats)))
    colnames(stats) <- c("Qualifications", "Playoffs")
    rownames(stats) <- 
        c("Average Match Score", "Average Winning Score", "Average Win Margin")
    return(stats)
}

# For these events, the results tab qualification statistics do not match 
# the insights tab qualification statistics. However, the playoff statistics
# do match.
results_summary("2022chcmp")
results_summary("2022iscmp")
results_summary("2022micmp3")

# For these events - both the qualification and playoff statistics match!
# Note especially that we're using micmp1/2/4 here, which make a clearer 
# comparison to 2022micmp3
results_summary("2022micmp1")
results_summary("2022micmp2")
results_summary("2022micmp4")
results_summary("2022mrcmp")

# Lastly, let's check a random few other events to make sure it's not just a 
# 2022 DCMP fluke. 
results_summary("2023txwac")
results_summary("2024paca")
results_summary("2019hiho")
