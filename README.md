# SightabilityShiny
Elk abundance estimator app - South Coast Region

This repo stores the code that runs the Roosevelt Elk Abundance Estimator app (https://3ahyqy-tristen-brush.shinyapps.io/ElkAbundanceEstimator/). To run the sightability model outside of the app, please see my SightabilityModel repo.

The models and scripts are based on Feiberg (2013), whose data are available for reference in the 'Feiberg' folder.

Please be sure to copy this folder onto your C:/ drive before attempting to work with the data and scripts.

## Differences between this repo and SightabilityModel repo
  1. For the app, you need to include an "EPU_list" sheet in your excel input file. In SightabilityModel, the EPU list is created within the name_fixer function.
  2. The app runs the Bayesian model only, while the SightabilityModel repo stores scripts for both the Bayesian model and the modified Horvitz-Thompson (mHT) model.
  3. The app will throw customized warnings/errors when common issues occur. SightabilityModel scripts will either throw generic warnings/errors or problems will manifest while running the model or in the output (i.e., a fuller understanding of R is recommended when opting for the SightabilityModel scripts).
  4. The app stratifies the data by sex/age class while the SightabilityModel scripts do not. 

Please email teb310@gmail.com if you have questions about this repo.
