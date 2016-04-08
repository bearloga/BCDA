## ----setup, include = FALSE----------------------------------------------
options(digits = 4)
library(ggplot2)
library(ggthemes)
theme_set(ggthemes::theme_tufte(base_family = "Gill Sans"))
library(magrittr)

## ----data, echo = FALSE--------------------------------------------------
aspirin <- matrix(c(18 + 171, 10845, 5 + 99, 10933), nrow = 2, byrow = TRUE)
rownames(aspirin) <- c("Placebo", "Aspirin")
colnames(aspirin) <- c("Myocardial Infraction", "No Attack")
aspirin <- aspirin[2:1, 2:1]
knitr::kable(addmargins(aspirin))

## ----probabilities, echo = FALSE-----------------------------------------
knitr::kable(addmargins(prop.table(aspirin)), digits = 3)

## ----chisq_indepen-------------------------------------------------------
stats::chisq.test(aspirin) # usually done for large sample tables

## ----fisher_exact--------------------------------------------------------
stats::fisher.test(aspirin) # usually done for small sample tables

## ----packages------------------------------------------------------------
library(BCDA)

## ----bayes_probabilities_example, eval = FALSE---------------------------
#  est_multinom(aspirin)

## ----bayes_probabilities, echo = FALSE-----------------------------------
knitr::kable(addmargins(est_multinom(aspirin)), digits = 3)

## ----prevalence, echo = FALSE--------------------------------------------
prevalence <- data.frame(Incidence = c(0.8, 2.2, 0.2, 1.0,
                                       2.0, 3.6, 1.0, 2.3,
                                       3.8, 5.7, 2.0, 3.7,
                                       6.6, 8.1, 3.6, 7.2,
                                       9.1, 12.9, 7.8, 10.2),
                         Age = c(rep('35-44', 4), rep('45-54', 4), rep('55-64', 4), rep('65-74', 4), rep('75-84', 4)),
                         Sex = c('Men', 'Women')[rep(c(1,1,2,2), 5)],
                         Race = c('White', 'Black')[rep(rep(c(1,2), 2), 5)])

## ----prevalence_visualization, echo = FALSE, fig.width = 7, fig.height = 4----
suppressWarnings(library(ggplot2))
prevalence <- transform(prevalence, Group = paste(Race, Sex))
ggplot(data = prevalence,
       aes(y = Incidence, x = Age, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Prevalence per 1,000 persons") +
  ggtitle("Incidence of myocardial infraction\nSource: American Heart Association")

## ----priors, echo = FALSE------------------------------------------------
prior <- matrix(c(1-mean(prevalence$Incidence/1000), mean(prevalence$Incidence/1000)), ncol = 2, byrow = TRUE)
colnames(prior) <- c('No Attack', 'Avg Prevalence of M.I.')
knitr::kable(prior, digits = 3)
prior <- prior[c(1, 1), ]/2
rownames(prior) <- c('Aspirin', 'Placebo')
knitr::kable(prior, digits = 3)

## ----bayes_probabilities_prior_example, eval = FALSE---------------------
#  est_multinom(aspirin, prior = prior)

## ----bayes_probabilities_prior, echo = FALSE-----------------------------
knitr::kable(addmargins(est_multinom(aspirin, prior = prior)), digits = 3)

## ----bf_indepen_BF-------------------------------------------------------
bf <- LearnBayes::ctable(aspirin, matrix(rep(1, 4), 2)) # 1793
BCDA:::interpret_bf(bf)

## ----bf_indepen----------------------------------------------------------
test_independence(aspirin)

## ----log_linear----------------------------------------------------------
library(conting) # for summary.bcct()
set.seed(0)
fit <- log_linear(aspirin, c('drug', 'attack'))
summary(fit) # from the 'conting' package

## ----beta_binom_stan_fit_example, message = FALSE------------------------
set.seed(0)
fit <- beta_binom(aspirin)

## ----beta_binom_stan_fit_example_plot, message = FALSE, fig.width = 7, fig.height = 4----
plot(fit, interval_type = "HPD") # requires coda package to be installed
# plot(fit) will use credible intervals computed using the quantile method

## ----beta_binom_stan_fit_example_summary_hpd, eval = FALSE---------------
#  summary(fit, interval_type = "HPD") # requires coda package to be installed
#  # summary(fit) will give credible intervals using the quantile method

## ----beta_binom_stan_fit, echo = FALSE-----------------------------------
knitr::kable(summary(fit, interval_type = "HPD"))

## ----bayesian_initial, fig.width = 7, fig.height = 4---------------------
set.seed(0)
group_1 <- sample.int(2, 20, prob = c(0.55, 0.45), replace = TRUE)-1
group_2 <- sample.int(2, 20, prob = c(0.65, 0.35), replace = TRUE)-1
fit <- beta_binom(c(sum(group_1), sum(group_2)), c(length(group_1), length(group_2)))
temp <- summary(fit); temp$term <- rownames(temp); rownames(temp) <- NULL; temp$day <- 1
posterior_summaries <- temp

## ----bayesian_updating, fig.width = 7, fig.height = 4--------------------
for (day in 2:14) {
  group_1 <- sample.int(2, 10, prob = c(0.55, 0.45), replace = TRUE)-1
  group_2 <- sample.int(2, 10, prob = c(0.65, 0.35), replace = TRUE)-1
  fit <- update(fit, c(sum(group_1), sum(group_2)), c(length(group_1), length(group_2)))
  temp <- summary(fit); temp$term <- rownames(temp); rownames(temp) <- NULL; temp$day <- day
  posterior_summaries <- rbind(posterior_summaries, temp)
}
posterior_summaries$term <- factor(posterior_summaries$term,
                                   levels = c("p1", "p2", "prop_diff",
                                              "relative_risk", "odds_ratio"),
                                   labels = c("Prop 1", "Prop 2", "Prop 1 - Prop 2",
                                              "Relative Risk", "Odds Ratio"))

## ----visualize_updates, fig.width = 7, fig.height = 14, echo = FALSE-----
posterior_summaries %>%
  { .[(.$day %% 2) == 0 | .$day == 1, ] } %>%
  ggplot(data = ., aes(x = estimate, y = term)) +
  facet_wrap(~day, nrow = 5,
             labeller = function(days) { days$day <- paste("Day", days$day); days }) +
  geom_segment(aes(x = conf.low, xend = conf.high,
                   y = term, yend = term),
               color = "#e41a1c", size = 0.75) +
  geom_point(size = 2) +
  scale_y_discrete(limits = rev(levels(posterior_summaries$term))) +
  labs(title = "Updating posterior with 10 observations/day for 2 weeks") +
  geom_segment(aes(x = 0.45, xend = 0.45, y = 4.75, yend = 5.25),
               color = "#377eb8", size = 1.1) +
  geom_segment(aes(x = 0.35, xend = 0.35, y = 3.75, yend = 4.25),
               color = "#377eb8", size = 1.1) +
  geom_segment(aes(x = 0.10, xend = 0.10, y = 2.75, yend = 3.25),
               color = "#377eb8", size = 1.1) +
  geom_segment(aes(x = 0.45/0.35, xend = 0.45/0.35, y = 1.75, yend = 2.25),
               color = "#377eb8", size = 1.1) +
  geom_segment(aes(x = (0.45/0.55)/(0.35/0.65), xend = (0.45/0.55)/(0.35/0.65),
                   y = 0.75, yend = 1.25), color = "#377eb8", size = 1.1) +
  theme(panel.grid = element_line(color = "black")) +
  scale_x_continuous(limits = c(0, 3), oob = scales::squish)

## ----visualize_updates_animated, fig.show = 'animate', eval = FALSE, echo = FALSE----
#  gg <- posterior_summaries %>%
#    ggplot(data = ., aes(x = estimate, y = term, frame = day)) +
#    geom_segment(aes(x = conf.low, xend = conf.high,
#                     y = term, yend = term),
#                 color = "#e41a1c", size = 0.75) +
#    geom_point(size = 2) +
#    scale_y_discrete(limits = rev(levels(posterior_summaries$term))) +
#    labs(title = "Estimates and HPD Intervals after day", y = NULL, x = NULL) +
#    geom_segment(aes(x = 0.45, xend = 0.45, y = 4.75, yend = 5.25),
#                 lty = "dotted", color = "#377eb8") +
#    geom_segment(aes(x = 0.35, xend = 0.35, y = 3.75, yend = 4.25),
#                 lty = "dotted", color = "#377eb8") +
#    geom_segment(aes(x = 0.10, xend = 0.10, y = 2.75, yend = 3.25),
#                 lty = "dotted", color = "#377eb8") +
#    geom_segment(aes(x = 0.45/0.35, xend = 0.45/0.35, y = 1.75, yend = 2.25),
#                 lty = "dotted", color = "#377eb8") +
#    geom_segment(aes(x = (0.45/0.55)/(0.35/0.65), xend = (0.45/0.55)/(0.35/0.65),
#                     y = 0.75, yend = 1.25), lty = "dotted", color = "#377eb8") +
#    theme(panel.grid = element_line(color = "black"))
#  gganimate::gg_animate(gg, "~/Desktop/updating.gif", interval = 0.5, ani.width = 600, ani.height = 400)

