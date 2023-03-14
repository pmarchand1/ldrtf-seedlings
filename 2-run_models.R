library(brms)
library(dplyr)

seedling <- read.csv("seedling_mod_data.csv")

for (sp in unique(seedling$species)) {
    seedling_sp <- filter(seedling, species == sp)
    res <- brm(bf(count ~ interc + lin + log(1 + scar1 * exp(scar2)), nl = TRUE,
                  interc ~ 1 + (1|year) + (1|fire) + (1|subplot_id),
                  lin ~ 0 + DD_0 + DD_0:scar + DD_1 + DD_2 + DD_3 +
                      DC_0 + DC_0:scar + DC_1 + DC_2 + DC_3 + 
                      GS.T_0 + GS.T_0:scar + GS.T_1 + GS.T_2 + GS.T_3,
                  scar1 ~ 0 + scar + (0 + scar|fire),
                  scar2 ~ 0 + scar_t:scar),
               prior = c(prior(student_t(3, 0, 2.5), class = b, nlpar = interc),
                         prior(normal(0, 1), class = sd, nlpar = interc),
                         prior(normal(0, 1), class = sd, nlpar = scar1),
                         prior(normal(0, 1), class = b, nlpar = lin),
                         prior(normal(0, 1), class = b, lb = -1, nlpar = scar1),
                         prior(normal(0, 1), class = b, ub = 0, nlpar = scar2)),
               family = negbinomial, data = seedling_sp, chains = 2,
               control = list(adapt_delta = 0.95))
    saveRDS(res, paste0("res_", sp, ".rds"))
}




