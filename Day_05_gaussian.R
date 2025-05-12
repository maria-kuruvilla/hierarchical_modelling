suppressPackageStartupMessages(library(dplyr))
library(ggplot2)
library(tidyr)
# library(cmdstanr)
suppressPackageStartupMessages(library(rstan))
rstan_options("auto_write" = TRUE)
options(mc.cores = parallel::detectCores())
library(tidybayes)

# mite data
data(mite, package = "vegan")
data(mite.env, package = "vegan")

## ALSO: the spatial data
data(mite.xy, package = "vegan")



