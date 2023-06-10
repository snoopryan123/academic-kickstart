+++
# About widget.
widget = "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless = false  # This file represents a page section.
active = true  # Activate this widget? true/false
weight = 10  # Order that this section will appear in.

# Choose the user profile to display
# This should be the username of a profile in your `content/authors/` folder.
# See https://sourcethemes.com/academic/docs/get-started/#introduce-yourself
author = "admin"
+++

# Statistics via Sports

* Recommended prerequisites: calculus, probability, R coding; matrix algebra helps
* Taught as part of the [Wharton Sports Analytics Summer Research Lab](https://wsb.wharton.upenn.edu/sports-analytics-summer-research-lab/)

## Lectures:
### Intro
* Linear algebra primer
    * [planned lecture](/pdf/lab/planned_lectures_2023/linear_algebra_primer.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/linear_algebra_primer.pdf)
* Probability primer
    * [planned lecture](/pdf/lab/planned_lectures_2023/probability_primer.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/probability_primer.pdf)
* Example of the research process
    * Rethinking WAR for starting pitchers
    * [planned lecture](/pdf/lab/planned_lectures_2023/example_of_the_research_process.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/example_of_the_research_process.pdf)
* Statistical models vs. mathematical models
    * Rethinking WAR for starting pitchers
    * [planned lecture](/pdf/lab/planned_lectures_2023/mathematical_vs_statistical_models.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/mathematical_vs_statistical_models.pdf)
    * [code](/pdf/lab/code/fitting_f_grid.R)
    * [data](/pdf/lab/code/data/df_f_grid.csv)
    * [XGBoost pre-trained hyperparameters](/pdf/lab/code/data/xgb_f_grid_params.yaml)
### Regression
* Simple linear regression
    * predict batting average across seasons, pythagorean win percentage
    * [planned lecture](/pdf/lab/planned_lectures_2023/simple_linear_regression.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/simple_linear_regression.pdf)
    * [code](/pdf/lab/code/simple_linear_regression.R)
* Multivariable linear regression
    * NCAA basketball power ratings, NFL expected points 
    * [planned lecture](/pdf/lab/planned_lectures_2023/multivariable_linear_regression.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/multivariable_linear_regression.pdf)
    * [code](/pdf/lab/code/multivariable_linear_regression.R)
    * [NCAA mbb schedule data](/pdf/lab/code/data/MRegularSeasonCompactResults.csv), [NCAA mbb team data](/pdf/lab/code/data/MTeams.csv), and [NFL expected points data](/pdf/lab/code/data/data_ep_2015_2019.csv)
* Logistic regression
    * putt success probability, Bradley-Terry power ratings
    * [planned lecture](/pdf/lab/planned_lectures_2023/logistic_regression.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/logistic_regression.pdf)
    * [code](/pdf/lab/code/logistic_regression.R)
    * [NCAA mbb schedule data](/pdf/lab/code/data/MRegularSeasonCompactResults.csv), [NCAA mbb team data](/pdf/lab/code/data/MTeams.csv)
### Shrinkage
* Regularization and the bias-variance tradeoff
    * MLB park effects
    * [planned lecture](/pdf/lab/planned_lectures_2023/regularization_and_the_bias_variance_tradeoff.pdf)
    * [live lecture]()
    * [code](/pdf/lab/code/regularization_and_the_bias_variance_tradeoff.R)
    * [MLB half-inning data](/pdf/lab/code/data/park_df.csv)
* The power of fake data (priors)
    * predict end-of-season win percentage from mid-season win percentage
    * [planned lecture](/pdf/lab/planned_lectures_2023/the_power_of_fake_data.pdf)
    * [live lecture]()
* Empirical Bayes
    * predict end-of-season batting average from mid-season batting average
    * [planned lecture](/pdf/lab/planned_lectures_2023/empirical_bayes.pdf)
    * [live lecture]()
    * [code](/pdf/lab/code/empirical_bayes.R)
    * [2019 batting average data](/pdf/lab/code/data/df_in_season_pred_BA_2019.csv)

## Tentative Lecture Plan: 
* Trees 1: decision trees and random forests
    * fixme: need an example
* Trees 2: gradient boosting
    * in-game NFL win probabilities
* Uncertainty quantification 1: nonparametric/bootstrap
    * in-game NFL win probabilities
* Uncertainty quantification 2: parametric;  bayesian? ?
    * fixme: need an example
* fixme: need a statistical theme
    * NFL draft chart 
* Clustering 1: K-means 
    * fixme: need an example
* Clustering 2: eigenvalues, diagonalization, SVD
* Clustering 3: PCA, factor analysis
    * fixme: need an example
* Primer 3: data visualization
* Selection Bias
* Regression models are sensitive (multicollinearity; model changes a lot when remove one predictor)








