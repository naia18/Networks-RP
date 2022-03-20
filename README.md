# Networks-RP

## Data folder
- Household_characteristics.dta: has demographic information about a household's home (caste, roof type, number of rooms, latrine type, etc.). The household survey was conducted for every household.
- Individual_characteristics.dta: has individual demographic information (age, caste, sub-caste, religion, language, occupation, etc.) This more detailed survey was conducted among a little under half of households, and also asked for social network information.
- final_data.R: has data on all households of rural Karnataka taken from Banerjee et al. with predicted sub-castes (for the hh that have missing information).


## Code in R folder

First we need to clean thoroughly the data on individual_characteristics.dta. Most of the subcastes have misspelled duplicates with typos. An example is the case of the sub-caste Adi Karnataka (which appears written in 15 different label-types) or the case of the branches of Christianity (which I gathered in one sole label). The cleaning of the data is carried out in the file **Data_Clean.R** and it yields a cleaned .Rdata file that will be used in the following steps: creating the train-test dataset and building the RF model for classification.

In order to construct the train-test dataset, I consider the predictors that are satisfactory enough explaining the variance on the class, that is, the type of the roof, access to electricity, owned latrine, number of rooms and beds in the house and the amount of first neighbours of each sub-caste. The training set will be formed by the observations in which we know the sub-caste, and will be splitted into two: $70 \%$ will be devoted into training the model, $30 \%$ into validation. This splitting is done at random. The testing set, on the other hand, is built by using all the nodes for which we have missing data. These steps are carried out in **All_Village_Traintest_Preparation.R**.

Once I am done, I can predict the nodes in the Test set by using the trained model for all the households in every village of the dataset, which is done in **RF_Classification.R**.

Finally, the file **Shock_ALL.R** estimates a logit model that explains to a great extent the original network, we calibrate the parameters left of the model, and we propose a potential post-shock scenario constructed with the calibrated parameters and estimated coefficients for all villages.

**30th_Vil_Visualisation.R** is used to create an interactive graph with visNetwork for the 30th village, but could be any other village.

## Interactive Networks folder

Here we can download the .html files of the pre-prediction interactive representation of the 30th village in Karnataka, based on sub-castes, as well as the post-prediction representation.

## Google Earth Engine
In order to analyse the weather conditions of the Karnataka state and defend the fact that it would be pertinent to study this region, I compute a brief analysis based on the LTA of rainfall in the Karnataka state in Google Earth Engine. This is given in **earth\_engine\_code.js**.
