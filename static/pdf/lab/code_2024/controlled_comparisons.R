
### multivariable linear regression
### Author: Ryan Brill

### packages
library(tidyverse)

### ggplot settings
theme_set(theme_bw())
theme_update(text = element_text(size=25))
theme_update(plot.title = element_text(hjust = 0.5))

##########################################
### Diving Example ###
##########################################

d0 = read_csv("data/data_diving_example.csv", show_col_types = F)
d0
