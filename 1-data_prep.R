library(dplyr)
library(tidyr)
library(stringr)

# Seedling counts

seedling_counts <- read.csv("seedling_census_counts.csv")

seedling_counts <- seedling_counts %>% 
    # Only keep the first observation of each seedling
    filter(first_year == current_year, first_census == current_census,
           species_id %in% c("ABA", "BPA", "PGL", "PTR", "TOC")) %>%
    mutate(species = recode(species_id, ABA = "Abies balsamea", BPA = "Betula papyrifera",
                            PGL = "Picea glauca", PTR = "Populus tremuloides", 
                            TOC = "Thuja occidentalis")) %>%
    rename(year = first_year) %>%
    # Sum counts from all censuses for a given subplot, species and year
    group_by(subplot_id, species, year) %>%
    summarize(count = sum(count), .groups = "drop") %>%
    # Add zeros for combinations of subplot and year where species is absent 
    complete(subplot_id, species, year, fill = list(count = 0)) %>% 
    # Extract year of last fire (stand ID) from subplot_id
    mutate(fire = as.integer(str_sub(subplot_id, 2, 5)))

# Add scarification treatment information
#  scar: scarified (subplot ending in "S")
#  scar_t: number of years since scarification (odd plots e.g. 1S, 3S, ... were only 
#          scarified at the beginning of the experiment, vs. every year for even plots)
seedling_counts <- seedling_counts %>% 
    mutate(scar = as.integer(str_sub(subplot_id, -1) == "S"),
           scar_t = ifelse(scar & as.integer(str_sub(subplot_id, -2, -2)) %% 2 == 1,
                           year - 2001, 0))


# Add climate variables with lag of 0 to 3 years

clim <- read.csv("seedling_climate.csv")

# Scale variables (mean of 0, s.d. of 1) first
clim[,c("DC", "DD", "GS.T")] <- scale(clim[,c("DC", "DD", "GS.T")])

clim <- clim %>%
    rename(DC_0 = DC, DD_0 = DD, GS.T_0 = GS.T) %>% 
    mutate(DC_1 = lag(DC_0), DC_2 = lag(DC_1), DC_3 = lag(DC_2),
           DD_1 = lag(DD_0), DD_2 = lag(DD_1), DD_3 = lag(DD_2),
           GS.T_1 = lag(GS.T_0), GS.T_2 = lag(GS.T_1), GS.T_3 = lag(GS.T_2)) %>% 
    filter(year >= 2001)

seedling_counts <- inner_join(seedling_counts, clim)


# Only keep species / stand combinations with observed adult trees in 50 x 50 m plot

ba50 <- read.csv("seedling_ba50.csv")
seedling_counts <- semi_join(seedling_counts, ba50)

# Exclude paper birch for 2017 and 2018 (missing values, not actual zeros)
seedling_counts <- filter(seedling_counts, year <= 2016 | species != "Betula papyrifera")


write.csv(seedling_counts, "seedling_mod_data.csv", row.names = FALSE)
