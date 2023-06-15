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
    * [HW: Value of a draft position](/pdf/lab/hws_2023/hw_draft_position.pdf)
* Logistic regression
    * putt success probability, Bradley-Terry power ratings
    * [planned lecture](/pdf/lab/planned_lectures_2023/logistic_regression.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/logistic_regression.pdf)
    * [code](/pdf/lab/code/logistic_regression.R)
    * [NCAA mbb schedule data](/pdf/lab/code/data/MRegularSeasonCompactResults.csv), [NCAA mbb team data](/pdf/lab/code/data/MTeams.csv)
    * [HW: Power score comparison](/pdf/lab/hws_2023/hw_power_scores.pdf)
    
### Shrinkage & Bayesianism
* Regularization and the bias-variance tradeoff
    * MLB park effects
    * [planned lecture](/pdf/lab/planned_lectures_2023/regularization_and_the_bias_variance_tradeoff.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/regularization_and_the_bias_variance_tradeoff.pdf)
    * [code](/pdf/lab/code/regularization_and_the_bias_variance_tradeoff.R)
    * [MLB half-inning data](/pdf/lab/code/data/park_df.csv)
    * [HW: Adjusted plus-minus and RAPM](/pdf/lab/hws_2023/hw_APM_RAPM.pdf)
* The power of fake data (priors)
    * predict end-of-season win percentage from mid-season win percentage
    * [planned lecture](/pdf/lab/planned_lectures_2023/the_power_of_fake_data.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/the_power_of_fake_data.pdf)
    * [HW: Priors for in-season prediction of win percentage](/pdf/lab/hws_2023/hw_prior_WP.pdf)
* Empirical Bayes
    * predict end-of-season batting average from mid-season batting average
    * [planned lecture](/pdf/lab/planned_lectures_2023/empirical_bayes.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/empirical_bayes.pdf)
    * [code](/pdf/lab/code/empirical_bayes.R)
    * [2019 batting average data](/pdf/lab/code/data/df_in_season_pred_BA_2019.csv)
    * [HW: Empirical Bayes player quality](/pdf/lab/hws_2023/hw_emp_bayes_player_quality.pdf)
    * [paper](/pdf/statistics_in_sports_papers/Brown2008.pdf) -- In-season prediction of batting averages: a field test of empirical Bayes and Bayes methodologies
* Examples: Bayesian modeling in sports
    * [A high-level overview of Bayesian statistics](/pdf/lab/planned_lectures_2023/bayesian_high_level_overview.pdf)
    * Bayesball: A Bayesian hierarchical model for evaluating fielding in Major League Baseball
        * [Shane's slides](/pdf/statistics_in_sports_papers/shane_lab_slides_2023.pdf)
        * [paper](/pdf/statistics_in_sports_papers/bayesball_hierarchical_fielding.pdf)
    * How often does the best team win? A unified approach to understanding randomness in North American sport.
        * [planned lecture](/pdf/lab/planned_lectures_2023/randomness_across_sports.pdf)
        * [live lecture]()
        * [paper](/pdf/statistics_in_sports_papers/lopez_2018.pdf)
    * More Bayesian sports papers:
        * [A Hierarchical Bayesian model of pitch framing](/pdf/statistics_in_sports_papers/bayesball_hierarchical_pitch_framing.pdf)
        * [Evaluating plate discipline in Major League Baseball with Bayesian Additive Regression Trees ](/pdf/statistics_in_sports_papers/bayesball_plate_discipline_bart.pdf)
        * [A Bayesian analysis of the time through the order penalty in baseball](https://arxiv.org/abs/2210.06724)      
        
### Tree machine learning
* Our example: in-game NFL win probabilities
    * This unit's [HW](/pdf/lab/hws_2023/)
* Decision trees
    * [planned lecture](/pdf/lab/planned_lectures_2023/decision_trees.pdf)
    * [live lecture](/pdf/lab/live_lectures_2023/)
* Random forests
    * [planned lecture](/pdf/lab/planned_lectures_2023/)
    * [live lecture](/pdf/lab/live_lectures_2023/)
    * paper: [Arcing Classifiers](/pdf/statistics_in_sports_papers/Breiman_arcing_classifiers.pdf) (explores the bias-variance tradeoff for classifiers)
    * paper: [Making sense of random forest probabilities](/pdf/statistics_in_sports_papers/making_sense_of_random_forest_probabilities.pdf)
* Trees 2: gradient boosting
    * [planned lecture](/pdf/lab/planned_lectures_2023/)
    * [live lecture](/pdf/lab/live_lectures_2023/)
    * paper: [XGBoost](/pdf/statistics_in_sports_papers/xgboost_chen_guestrin_2016.pdf)
* Nonparametric uncertainty quantification: the bootstrap
    * [planned lecture](/pdf/lab/planned_lectures_2023/)
    * [live lecture](/pdf/lab/live_lectures_2023/)
    * stay tuned for our paper coming soon!
    
### Clustering
* K-means clustering
    * NBA player clustering
* Eigenvalues, diagonalization, SVD
* PCA, factor analysis
    * NFL player clustering

### Future lesson ideas
* Kelly betting
* NFL Draft chart
* Selection Bias
* Data visualization tutorial
* The sensitivity of regression models (models change a lot when remove one predictor)








