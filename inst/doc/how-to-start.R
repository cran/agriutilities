## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(agriutilities)
library(agridat)
data(besag.met)
dat <- besag.met
results <- check_design_met(
  data = dat,
  genotype = "gen",
  trial = "county",
  traits = "yield",
  rep = "rep",
  block = "block",
  col = "col",
  row = "row"
)

## -----------------------------------------------------------------------------
print(results)

## -----------------------------------------------------------------------------
obj <- single_trial_analysis(results, progress = FALSE)
print(obj)

## -----------------------------------------------------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  met_results <- met_analysis(obj)
  print(met_results)
}

