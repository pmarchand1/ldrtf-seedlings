library(brms)
library(tibble)
library(purrr)
library(dplyr)
library(ggplot2)
library(cowplot)

# Load data and model results

seedling <- read.csv("seedling_mod_data.csv")
ba50 <- read.csv("seedling_ba50.csv")
# Order to plot species (from least to most shade tolerant)
sp_levels <- c("Populus tremuloides", "Betula papyrifera", "Picea glauca",
               "Abies balsamea", "Thuja occidentalis")
seedling <- mutate(seedling, species = factor(species, levels = sp_levels))
ba50 <- mutate(ba50, species = factor(species, levels = sp_levels))


res <- map(sp_levels, ~ readRDS(paste0("res_", ., ".rds")))
names(res) <- sp_levels

# Figure 1: Basal area and recruitment by plot ----------------------------

seedling_plot_year <- group_by(seedling, species, fire, year) %>%
    summarize(count = sum(count)) %>% 
    inner_join(ba50)

seedling_plots <- group_by(seedling_plot_year, species, fire, ba50) %>%
    summarize(count = mean(count))

# Add 0s for species absent from plots
ba50_with0 <- complete(ba50, species, fire, fill = list(ba50 = 0))

plot_ba_bars <- 
    ggplot(ba50_with0, aes(x = as.factor(fire), y = ba50, fill = species)) +
        geom_col(position = position_dodge()) +
        labs(x = "Plot", 
             y = expression(paste("Basal area ", (m^2/ha)))) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0, 35)) +
        scale_fill_brewer(palette = "Dark2") +
        theme_bw(base_size = 14) +
        theme(legend.title = element_blank(), legend.position = c(0.75, 0.78),
              legend.text = element_text(size = 9))

plot_ba_rec <- 
    ggplot(seedling_plots, aes(x = ba50, y = count, color = species)) +
        geom_text(aes(label = fire)) +
        labs(x = expression(paste("Basal area ", (m^2/ha))),
             y = "Mean annual seedling recruits") +
        scale_x_log10() +
        scale_y_log10() +
        scale_color_brewer(palette = "Dark2") +
        theme_bw(base_size = 14) +
        theme(legend.position = "none")

plot_grid(plot_ba_bars, plot_ba_rec, nrow = 1, labels = c("a", "b"))


# Figure 2: Recruitment and climate time series ---------------------------

seedling_years <- group_by(seedling_plot_year, species, year) %>%
    summarize(count = sum(count))

ggplot(filter(seedling_years, count > 0), aes(x = year, y = count, color = species)) +
    geom_point() +
    geom_line() +
    labs(x = "Year", y = "Seedling recruits (all plots)") +
    scale_y_log10() +
    scale_color_brewer(palette = "Dark2") +
    theme_bw(base_size = 14) +
    theme(legend.title = element_blank(), axis.title = element_text(size = 13))

clim <- distinct(seedling, year, `Drought Code` = DC_0,
                 `Temperature (°C)` = GS.T_0, `Degree days (base 5°C)` = DD_0)
clim <- pivot_longer(clim, -year, names_to = "var", values_to = "value")

ggplot(clim, aes(x = year, y = value)) +
    facet_grid(var ~ ., scales = "free", switch = "y") +
    labs(x = "Year") +
    geom_point() +
    geom_line() +
    theme_bw(base_size = 14) +
    theme(strip.background = element_blank(), axis.title.y = element_blank(),
          strip.placement = "outside", axis.title = element_text(size = 13),
          strip.text = element_text(size = 13))


# Extract summaries from model output -------------------------------------

summ <- map(res, summary, robust = TRUE)

# Create a table for all the fixed effects (climate, scarif. & their interaction)
fixef <- map(summ, "fixed") %>%
    map(~ rownames_to_column(., "par")) %>%
    bind_rows(.id = "species") %>%
    mutate(par = str_replace(par, ":scar", "_scar")) %>% 
    mutate(par = str_replace(par, "scar_t", "scar.t")) %>%  
    separate(par, into = c("mod", "par", "delay", "inter"), sep = "_") %>% 
    mutate(delay = -as.integer(delay),
           par = recode(par, DD = "Degree Days", GS.T = "Temperature", 
                        DC = "Drought Code",
                        scar = "Scarification", scar.t = "Scar. Time"))
fixef <- mutate(fixef, species = factor(species, levels = sp_levels))

# Stand (fire) random effect
fire_effects <- map(res, ranef) %>% 
    map("fire") %>% 
    map(~ .[,,"interc_Intercept"]) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, var = "fire") %>% 
    bind_rows(.id = "species")
fire_effects <- mutate(fire_effects, species = factor(species, levels = sp_levels))

# Random effect of stand (fire) on effect of scarification
fire_scar_effects <- map(res, ranef) %>% 
    map("fire") %>% 
    map(~ .[,,"scar1_scar"]) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, var = "fire") %>% 
    bind_rows(.id = "species")
fire_scar_effects <- mutate(fire_scar_effects, species = factor(species, levels = sp_levels))

# Year random effect
year_effects <- map(res, ranef) %>% 
    map("year") %>% 
    map(~ .[,,"interc_Intercept"]) %>% 
    map(as.data.frame) %>% 
    map(rownames_to_column, var = "year") %>% 
    bind_rows(.id = "species")
year_effects <- mutate(year_effects, species = factor(species, levels = sp_levels))

# Extract actual draws (posterior parameter values) for each model
samp <- map(res, as_draws_df) %>% 
    bind_rows(.id = "species")
samp <- mutate(samp, species = factor(species, levels = sp_levels))


# Figure 3: Scarification effect ------------------------------------------

samp_scar <- select(samp, species, b_scar1_scar, `b_scar2_scar_t:scar`)
samp_scar <- inner_join(samp_scar, 
                        expand_grid(species = unique(samp_scar$species), t = 0:17))
samp_scar <- mutate(samp_scar, scar = 1 + b_scar1_scar * exp(`b_scar2_scar_t:scar` * t)) %>% 
    group_by(species, t) %>% 
    summarize(scar_m = median(scar), scar_lo = quantile(scar, probs = 0.025), 
              scar_hi = quantile(scar, probs = 0.975))

ggplot(samp_scar, aes(x = t, y = scar_m, ymin = scar_lo, ymax = scar_hi)) +
    labs(x = "Time since scarification (years)", y = "Scarification effect") +
    geom_ribbon(alpha = 0.3, color = NA) +
    geom_line() +
    facet_wrap(~species) +
    theme_bw(base_size = 14)


# Figure 4: Climate effects -----------------------------------------------

# Calculate climate effects for scarified plots (current year only)
samp_scar_clim <- samp %>% 
    mutate(`Degree Days` = b_lin_DD_0 + `b_lin_DD_0:scar`,
           `Drought Code` = b_lin_DC_0 + `b_lin_DC_0:scar`,
           Temperature = b_lin_GS.T_0 + `b_lin_GS.T_0:scar`) %>% 
    select(.iteration, species, `Degree Days`, `Drought Code`, Temperature) %>% 
    pivot_longer(cols = -c(.iteration, species), names_to = "par", values_to = "value") %>% 
    group_by(species, par) %>% 
    summarize(med = median(value), lo = quantile(value, probs = 0.025),
              hi = quantile(value, probs = 0.975)) %>% 
    ungroup() %>% 
    mutate(delay = 0, inter = TRUE)

# Get main climate effects
clim <- filter(fixef, !is.na(delay), is.na(inter)) %>%
    mutate(inter = FALSE) %>% 
    select(species, par, med = Estimate, lo = `l-95% CI`, hi = `u-95% CI`, delay, inter) 

clim <- bind_rows(clim, samp_scar_clim)

ggplot(clim, aes(x = delay, y = med, ymin = lo, ymax = hi, 
                 color = species, linetype = inter, shape = inter)) +
    facet_wrap(~ par) +
    labs(x = "Relative year", y = "Scaled effect", color = "Species",
         shape = "Scarified \n (current year only)", linetype = "Scarified \n (current year only)") +
    scale_color_brewer(palette = "Dark2") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_pointrange(position = position_dodge(width = 0.6), fatten = 2) +
    scale_shape_manual(values = c(16, 1)) +
    theme_bw(base_size = 14) +
    theme(legend.title = element_text(size = 12))


# Figure 5: Plot-level random effects ------------------------------------

ba50 <- mutate(ba50, fire = as.character(fire))
fire_effects <- inner_join(fire_effects, ba50)

ggplot(fire_effects, aes(x = log(ba50), y = Estimate, ymin = Q2.5, ymax = Q97.5,
                         label = fire)) +
    labs(x = "log basal area (50-m radius)", 
         y = "Plot random effect (on log seedling counts)") +
    geom_smooth(method = "lm", alpha = 0.3, se = FALSE) +
    geom_pointrange(size = 0) +
    geom_text() +
    facet_wrap(~species) +
    theme_bw(base_size = 14)


# Fig. S1: Plot-level random effects on the effect of scarification ----------

fire_scar_effects <- inner_join(fire_scar_effects, ba50)

ggplot(fire_scar_effects, aes(x = log(ba50), y = Estimate, ymin = Q2.5, ymax = Q97.5,
                         label = fire)) +
    labs(x = "log basal area (50-m radius)", 
         y = "Plot random effect on scarif. effect") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_smooth(method = "lm", alpha = 0.3, se = FALSE) +
    geom_pointrange(size = 0) +
    geom_text() +
    facet_wrap(~species) +
    theme_bw(base_size = 14)


# Fig. S2: Year random effects ----------------------------------------------

ggplot(year_effects, aes(x = as.numeric(year), y = Estimate, 
                         ymin = Q2.5, ymax = Q97.5, color = species)) +
    labs(x = "Year", 
         y = "Random effect of year \n (on log seedling counts)") +
    scale_color_brewer(palette = "Dark2") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_pointrange(position = position_dodge(width = 0.5), fatten = 2) +
    theme_bw(base_size = 14) +
    guides(color = guide_legend(override.aes = list(size=0.25))) +
    theme(legend.title = element_blank())
