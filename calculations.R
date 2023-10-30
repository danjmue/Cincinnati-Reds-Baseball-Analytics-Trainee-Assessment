library(tidyverse)

# Reading in the dataset
read.csv('data.csv') -> data

# Pre-processing the data
data %>%
  filter(!EVENT_RESULT_KEY %in% c('stolen_base_3b', 'stolen_base_2b', 'catcher_interf', 'caught_stealing_2b', 
                                 'pickoff_caught_stealing_2b', 'pickoff_caught_stealing_3b')) -> data
data %>%
  mutate(HORIZONTAL_BREAK = abs(HORIZONTAL_BREAK),
         RELEASE_SIDE = abs(RELEASE_SIDE)) -> data
data %>%
  mutate(STRIKE = ifelse(PITCH_RESULT_KEY %in% c('InPlay', 'FoulBall', 'StrikeCalled', 'StrikeSwinging'), 1, 0),
         BALL = 1 - STRIKE) -> data

# Standardizing the data
data %>%
  group_by(PITCHER_KEY, PITCH_TYPE_TRACKED_KEY) %>%
  mutate_at(c('INDUCED_VERTICAL_BREAK', 'HORIZONTAL_BREAK', 'SPIN_RATE_ABSOLUTE', 
              'RELEASE_SPEED', 'RELEASE_SIDE', 'RELEASE_HEIGHT', 'RELEASE_EXTENSION'), function(x) {
                (x - mean(x)) / sd(x)
              }) %>%
  ungroup() -> data_standardized

# Calculating p-values
data_standardized %>%
  mutate_at(c('INDUCED_VERTICAL_BREAK', 'HORIZONTAL_BREAK', 'SPIN_RATE_ABSOLUTE', 
              'RELEASE_SPEED', 'RELEASE_SIDE', 'RELEASE_HEIGHT', 'RELEASE_EXTENSION'), function(x) {
                2 * pnorm(-abs(x))
              }) -> data_p_values

# Creating p-value bins
data_p_values %>%
  mutate_at(c('INDUCED_VERTICAL_BREAK', 'HORIZONTAL_BREAK', 'SPIN_RATE_ABSOLUTE', 
              'RELEASE_SPEED', 'RELEASE_SIDE', 'RELEASE_HEIGHT', 'RELEASE_EXTENSION'), function(x) {
                cut(x, c(-Inf, 0.05, Inf))
              }) -> data_p_values_binned

# Creating the table of ball rates of both bins for each metric
rbind(
  
  data_p_values_binned %>%
    group_by(INDUCED_VERTICAL_BREAK) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(INDUCED_VERTICAL_BREAK), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Induced Vertical Break'),
  
  data_p_values_binned %>%
    group_by(HORIZONTAL_BREAK) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(HORIZONTAL_BREAK), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Horizontal Break'),
  
  data_p_values_binned %>%
    group_by(SPIN_RATE_ABSOLUTE) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(SPIN_RATE_ABSOLUTE), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Spin Rate'),
  
  data_p_values_binned %>%
    group_by(RELEASE_SPEED) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(RELEASE_SPEED), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Release Speed'),
  
  data_p_values_binned %>%
    group_by(RELEASE_SIDE) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(RELEASE_SIDE), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Release Side'),
  
  data_p_values_binned %>%
    group_by(RELEASE_HEIGHT) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(RELEASE_HEIGHT), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Release Height'),
  
  data_p_values_binned %>%
    group_by(RELEASE_EXTENSION) %>%
    summarise(
      BALL_PCT = mean(BALL)
    ) %>%
    na.omit() %>%
    pivot_wider(names_from = c(RELEASE_EXTENSION), values_from = c(BALL_PCT)) %>%
    mutate(Metric = 'Release Extension')
  
) %>%
  rename(c('Ball Rate for P < 0.05 Bin' = 1, 'Ball Rate for P > 0.05 Bin' = 2)) %>%
  select(Metric, everything()) -> ball_rate_comparisons

# Creating plots for the four metrics with the greatest bin differences
data_standardized %>%
  mutate(
    INDUCED_VERTICAL_BREAK = cut(INDUCED_VERTICAL_BREAK, c(-Inf, seq(from = -2, to = 2, by = 1), Inf))
  ) %>%
  drop_na(INDUCED_VERTICAL_BREAK) %>%
  group_by(INDUCED_VERTICAL_BREAK) %>%
  summarise(
    N_PITCHES = n(),
    BALL_RATE = mean(BALL)
  ) %>%
  ggplot(aes(INDUCED_VERTICAL_BREAK, BALL_RATE)) +
  geom_bar(stat = 'identity', fill = 'red') +
  theme(legend.position = "none") +
  labs(title = 'Standardized Induced Vertical Break vs. Ball Rate',
       x = 'Standardized Induced Vertical Break Bins',
       y = 'Ball Rate') -> vertical_break_plot

data_standardized %>%
  mutate(
    SPIN_RATE_ABSOLUTE = cut(SPIN_RATE_ABSOLUTE, c(-Inf, seq(from = -2, to = 2, by = 1), Inf))
  ) %>%
  drop_na(SPIN_RATE_ABSOLUTE) %>%
  group_by(SPIN_RATE_ABSOLUTE) %>%
  summarise(
    N_PITCHES = n(),
    BALL_RATE = mean(BALL)
  ) %>%
  ggplot(aes(SPIN_RATE_ABSOLUTE, BALL_RATE)) +
  geom_bar(stat = 'identity', fill = 'blue') +
  theme(legend.position = "none") +
  labs(title = 'Standardized Spin Rate vs. Ball Rate',
       x = 'Standardized Spin Rate Bins',
       y = 'Ball Rate') -> spin_rate_plot

data_standardized %>%
  mutate(
    RELEASE_SIDE = cut(RELEASE_SIDE, c(-Inf, seq(from = -2, to = 2, by = 1), Inf))
  ) %>%
  drop_na(RELEASE_SIDE) %>%
  group_by(RELEASE_SIDE) %>%
  summarise(
    N_PITCHES = n(),
    BALL_RATE = mean(BALL)
  ) %>%
  ggplot(aes(RELEASE_SIDE, BALL_RATE)) +
  geom_bar(stat = 'identity', fill = 'purple') +
  theme(legend.position = "none") +
  labs(title = 'Standardized Release Side vs. Ball Rate',
       x = 'Standardized Release Side Bins',
       y = 'Ball Rate') -> release_side_plot

data_standardized %>%
  mutate(
    RELEASE_EXTENSION = cut(RELEASE_EXTENSION, c(-Inf, seq(from = -2, to = 2, by = 1), Inf))
  ) %>%
  drop_na(RELEASE_EXTENSION) %>%
  group_by(RELEASE_EXTENSION) %>%
  summarise(
    N_PITCHES = n(),
    BALL_RATE = mean(BALL)
  ) %>%
  ggplot(aes(RELEASE_EXTENSION, BALL_RATE)) +
  geom_bar(stat = 'identity', fill = 'darkgreen') +
  theme(legend.position = "none") +
  labs(title = 'Standardized Release Extension vs. Ball Rate',
       x = 'Standardized Release Extension Bins',
       y = 'Ball Rate') -> release_extension_plot

# Creating the binary "AFFECTED" variable
data_p_values %>%
  ungroup() %>%
  mutate(
    AFFECTED = case_when(
      INDUCED_VERTICAL_BREAK <= 0.05 ~ 1,
      SPIN_RATE_ABSOLUTE <= 0.05 ~ 1,
      RELEASE_SIDE <= 0.05 ~ 1,
      RELEASE_EXTENSION <= 0.05 ~ 1,
      .default = 0
    )
  ) %>%
  select(PID, AFFECTED) -> data_affected_pct
data_standardized %>%
  inner_join(data_affected_pct) -> data_affected_pct

# Test/train split
set.seed(1)
inds <- sample(1:nrow(data_affected_pct), 0.7 * nrow(data_affected_pct))
data_affected_pct[inds,] -> training
data_affected_pct[-inds,] -> testing

# Logistic regression model
glm(AFFECTED ~ INDUCED_VERTICAL_BREAK + SPIN_RATE_ABSOLUTE + RELEASE_SIDE + RELEASE_EXTENSION, training, family = 'binomial') -> glm_fit
summary(glm_fit)

# Making predictions and checking accuracy
testing$DEWPOINT_AFFECT <- predict(glm_fit, testing, type = 'response')
testing %>%
  mutate(PREDICTION = ifelse(DEWPOINT_AFFECT > 0.2, 1, 0)) -> testing
table(testing$PREDICTION, testing$AFFECTED)

# Making predictions on entire dataset and writing it to submission csv file
data_standardized$DEWPOINT_AFFECTED <- predict(glm_fit, data_standardized, type = 'response')
data_standardized %>%
  select(PID, DEWPOINT_AFFECTED) %>%
  write.csv('submission.csv', row.names = F, quote = F)
