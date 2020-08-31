library(conflicted)
library(tidyverse)
library(drake)
library(gt)
library(gtsummary)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
dotenv::load_dot_env(here::here(".env"))
set_gtsummary_theme(list(
  "tbl_summary-fn:percent_fun" = function(x) {
    scales::percent(x, accuracy = 1, suffix = NULL)
  }
))
