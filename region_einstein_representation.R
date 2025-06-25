rm(list = ls())

library(scoutR) # devtools::install_github("gkrotkov/scoutR")

# start in 2014 since that's when 4 team alliances started, which also happens 
# to be when the event_alliances() TBA endpoint is defined for Einstein ;)

cmps <- c(paste0(2014:2016, "cmp"), "2017cmptx", "2017cmpmo", "2018cmptx", 
          "2018cmpmi", "2019cmptx", "2019cmpmi", paste0(2022:2025, "cmptx"))

alliances <- lapply(cmps, event_alliances)
yr_n_alliances <- sapply(alliances, nrow)

cap_firsts <- sapply(
    alliances, function(alliance){c(alliance$captain, alliance$`pick 1`)}
)
cap_firsts <- scoutR:::id2int(unlist(cap_firsts))
# times multiplied by 2 bc both cap and first pick
names(cap_firsts) <- rep(substr(cmps, 1, 4), times = 2 * yr_n_alliances)

second_thirds <- sapply(
    alliances, function(alliance){c(alliance$`pick 2`, alliance$`pick 3`)}
)
second_thirds <- scoutR:::id2int(unlist(second_thirds))
names(second_thirds) <- rep(substr(cmps, 1, 4), times = 2 * yr_n_alliances)

# defining region based on 2025 boundaries for simplicity and interpretability
define_region <- function(teams_df, year){
    teams_df <- teams_df |>
        dplyr::mutate(
            region = ifelse(country %in% c("USA", "Canada"), 
                            state_prov, country)
        ) |>
        dplyr::mutate(
            region = dplyr::case_when(
                region %in% c("Texas", "New Mexico", "TX", "NM") ~ "FiT", 
                region %in% c("Maryland", "MD", "Virginia", "VA", 
                              "District of Columbia", "DC") ~ "CHS", 
                region %in% c("Pennsylvania", "PA", "New Jersey", "NJ",
                              "Delaware", "DE") ~ "FMA", 
                # not including "NH" bc it only matches off-season demo teams
                region %in% c("Maine", "ME", "Massachusetts", "MA", 
                              "Rhode Island", "RI", "Vermont", "VT", 
                              "New Hampshire", "Connecticut", "CT") ~ "NE", 
                region %in% c("Oregon", "OR", "Washington", "WA") ~ "PNW", 
                region == "AZ" ~ "Arizona", region == "AR" ~ "Arkansas",
                region == "CO" ~ "Colorado", region == "CA" ~ "California", 
                region == "GA" ~ "Georgia", region == "IL" ~ "Illinois",
                region == "ON" ~ "Ontario", region == "WI" ~ "Wisconsin",
                region == "SC" ~ "South Carolina", region == "TN" ~ "Tennessee",
                region == "PR" ~ "Puerto Rico", region == "OK" ~ "Oklahoma",
                region == "NC" ~ "North Carolina", region == "NV" ~ "Nevada",
                region == "MI" ~ "Michigan", region == "KY" ~ "Kentucky", 
                region == "LA" ~ "Louisiana", region == "MS" ~ "Mississippi", 
                region == "MN" ~ "Minnesota", region == "MO" ~ "Missouri", 
                region == "OH" ~ "Ohio", region == "NY" ~ "New York",
                region == "UT" ~ "Utah", region == "QC" ~ "Québec", 
                region == "Turkey" ~ "Türkiye", region == "IN" ~ "Indiana",
                region == "HI" ~ "Hawaii", region == "ID" ~ "Idaho", 
                region == "FL" ~ "Florida", region == "AB" ~ "Alberta",
                .default = region)
        )
    
    # fix region for non-FMA PA teams
    pa_teams <- teams_df$team_number[teams_df$state_prov == "Pennsylvania"]
    fma_key <- ifelse(year > 2018, paste0(year, "fma"), paste0(year, "mar"))
    fma_teams <- district_teams(fma_key, keys = TRUE)
    fma_teams <- scoutR:::id2int(fma_teams)
    wpa_teams <- setdiff(pa_teams, fma_teams)
    
    teams_df$region[teams_df$team_number %in% wpa_teams] <- "West/Central PA"
    return(teams_df)
}

yr_regions <- function(year){
    teams_df <- teams(0, year = year)
    for (i in 1:25){
        teams_df <- rbind(teams_df, teams(i, year = year))
    }
    teams_df <- define_region(teams_df, year) |>
        dplyr::select(key = team_number, region)
    return(teams_df)
}

years <- setdiff(2014:2025, 2020:2021)
regions <- lapply(years, yr_regions)
names(regions) <- years

lookup <- Reduce(dplyr::full_join, regions)

# since cap_firsts and second_thirds have the same length, we can index both
# in one loop
cap_first_regions <- c()
second_third_regions <- c()
for (i in seq_along(cap_firsts)){
    yr <- names(cap_firsts)[i]
    cap_first_regions <- c(
        cap_first_regions, 
        lookup$region[lookup$key == cap_firsts[i]]
    )
    second_third_regions <- c(
        second_third_regions, 
        lookup$region[lookup$key == second_thirds[i]]
    )
}

########################################
#### Define Expected Einstein slots ####
########################################

# What was a particular region's share of FRC teams for each Einstein instance?
einstein_slots <- sapply(alliances, nrow) * 4
names(einstein_slots) <- cmps

# how many Einstein appearances would this region expect for its size over
# the time span 2014 - 2025, given equal skill?
# no adjustment for 2champs needed here, since both CMPs had the same # of divs
expected <- c()
for (i in seq_along(einstein_slots)){
    yr <- substr(names(einstein_slots)[i], 1, 4)
    tbl <- table(regions[[yr]]$region)
    # normalize by size of FRC
    tbl <- tbl / sum(tbl)
    # multiply the expectation by the number of einstein slots
    tbl <- tbl * einstein_slots[i]
    expected <- combine_tbls(expected, tbl)
}

expected <- round(expected, digits = 2)

####################
#### Tabulation ####
####################

actual <- table(c(cap_first_regions, second_third_regions))
actual01 <- table(cap_first_regions)
actual23 <- table(second_third_regions)

# using -expected to subtract the expected value from the actual value
diff <- combine_tbls(actual, -expected)
# expected / 2 because only half as many cap/1sts as total slots, etc
diff01 <- combine_tbls(actual01, -(expected / 2))
diff23 <- combine_tbls(actual23, -(expected / 2))

result <- tibble::tibble(
    region = names(diff),
    actual = as.numeric(actual[match(region, names(actual))]), 
    actual01 = as.numeric(actual01[match(region, names(actual01))]),
    actual23 = as.numeric(actual23[match(region, names(actual23))]), 
    expected = as.numeric(expected[match(region, names(expected))]), 
    expected01 = as.numeric(expected / 2), 
    expected23 = as.numeric(expected / 2)
)

# in this case, all NA values are appropriately 0 (no einstein appearances)
result[is.na(result)] <- 0

result <- result |>
    dplyr::mutate(
        diff = actual - expected, 
        diff01 = actual01 - expected01, 
        diff23 = actual23 - expected23,
        ratio = actual / expected, 
        ratio01 = actual01 / expected01, 
        ratio23 = actual23 / expected23, 
        # alphabetical order means these will match by default
        # size is the number of teams that existed in that region for at least
        # one year between 2014 - 2025
        size = as.numeric(table(lookup$region))
    )

result <- scoutR:::round_numerics(result, digits = 3)

readr::write_csv(result, "../../../Data/einstein_representation.csv")
