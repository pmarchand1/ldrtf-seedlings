Data and code for Maleki et al. "Seedling recruitment in response to stand composition, interannual climate variability, and soil disturbance in the boreal mixedwoods of Canada"

**Data**

*seedling_census_counts.csv*

This file is copied from the published Lake Duparquet Research and Teaching Forest (LDRTF) chronosequence database (https://datadryad.org/stash/dataset/doi:10.5061/dryad.tqjq2bvwz). It contains counts of observed seedlings in 7 post-fire stands with different ages, with 16 seedling monitoring subplots per stand. See the source publication for more details on the database format.

*seedling_ba.csv*

This previously unpublished data contains the total basal area (ba50, in m^2) of all stems with diameter at breast height (DBH) over 5 cm for each species, in a 50 x 50 m plot centered on the location of the seedling monitoring subplots in each post-fire stand (identified by fire, the year of the last fire in that stand).

*seedling_climate.csv*

This file contains the three climate variables: drought code (DC), growing season degree days (DD), growing season mean temperature (GS.T) extracted with the BioSIM software at the LDRTF location (48.5 N, 79.017 W, elevation of 320 m).

**Code**

*1-data_prep.R*

This script prepares the input data for the models based on the .csv data files included.

*2-run_models.R*

A short script that runs the Bayesian regression model for each of the five species.

*3-process_results.R*

This script uses the model outputs to produce the figures shown in the paper.

