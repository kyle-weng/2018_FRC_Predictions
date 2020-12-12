#' ---
#' title: "IDS 126 - Project"
#' author: "Kyle Weng"
#' date: "2020-11-25"
#' output: pdf_document
#' ---

setwd("J:/Academic Archives/FA 2020/IDSEcPs 126/Project")
library(tidyverse)
library(magrittr)
library(ggplot2)
library(rstanarm)
library(loo)
library(bayesplot)

options(mc.cores=parallel::detectCores())

data <- read.csv("2018nvlv_matches.csv") %>% as_tibble(.)
team <- unique(c(unique(data$r1), unique(data$r2), unique(data$r3),
                 unique(data$b1), unique(data$b2), unique(data$b3)))
teams <- data.frame(team)

team_r <- unique(c(unique(data$r1), unique(data$r2), unique(data$r3)))
teams_r <- data.frame(team_r)

# first, let's just look at red alliance scores
for (i in teams_r$team) {
  varname <- as.name(paste(i))
  data <- mutate(data, !! varname := as.integer(
    r1 == varname | r2 == varname | r3 == varname))
    #b1 == varname | b2 == varname | b3 == varname))
}
#f <- paste(team_r, collapse = ' + ')
f <- paste(team_r[order(team_r)][1:5], collapse = ' + ')
f <- paste("r_score", f, sep = ' ~ ')
f <- as.formula(f)

r_1 <- stan_glm(formula = f, data = data, refresh = 1, cores = 10)
print(r_1)

pred <- posterior_predict(r_1)
pred <- (colMeans(test_fit))
p <- data.frame(pred)
p$residuals <- p$pred - data$r_score

ggplot(data = p) +
  geom_point(mapping = aes(x = pred, y = residuals)) +
  geom_abline(intercept = 0, slope = 0) +
  labs(
    x = paste("predicted match score"),
    y = paste("residual"),
    title = paste("Interim report - residual plot of five team fit")
  )

ggplot(data = data) +
  geom_histogram(mapping = aes(x = r_score), binwidth = 20) +
  xlim(NA, 530) +
  labs(
    x = paste("red alliance score"),
    title = paste("Interim report - distribution of red alliance scores")
  )

ggplot(data = data) +
  geom_histogram(mapping = aes(x = b_score), binwidth = 20) +
  xlim(NA, 530) +
  labs(
    x = paste("blue alliance score"),
    title = paste("Interim report - distribution of blue alliance scores")
  )

ggplot(data = data) +
  geom_histogram(mapping = aes(x = r_score - b_score), binwidth = 30) +
  labs(
    x = paste("margin of victory (red score - blue score)"),
    title = paste("Interim report - distribution of margins of victory")
  )
