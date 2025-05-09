# -----------------------------
# Load libraries
library(shiny)
library(ggplot2)
library(mirt)

# -----------------------------
# Load CSV files

question_data <- read.csv("questions.csv", stringsAsFactors = FALSE)
response_data <- read.csv("responses.csv")

# -----------------------------
# Fit 2PL IRT model

irt_model <- mirt(response_data, model = 1, itemtype = "2PL", verbose = FALSE)

# -----------------------------
# Extract item parameters

item_parameters <- coef(irt_model, IRTpars = TRUE, simplify = TRUE)
a.estimate <- as.numeric(item_parameters$items[, 1])  
b.estimate <- as.numeric(item_parameters$items[, 2])

# -----------------------------
# Save combined data to .rds

saveRDS(list(
  question_data = question_data,
  response_data = response_data,
  a.estimate = a.estimate,
  b.estimate = b.estimate
), file = "data.rds")
