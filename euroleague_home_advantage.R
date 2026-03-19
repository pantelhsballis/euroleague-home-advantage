# ==============================================================================
# Home Advantage Analysis in Euroleague Basketball (2010-2024)
# Author: Pantelis Ballis
# ==============================================================================

# --- Libraries ----------------------------------------------------------------
library(euroleagueR)
library(tidyverse)
library(broom)
library(lubridate)
library(car)
library(cluster)
library(rstatix)
library(kableExtra)

# ==============================================================================
# 1. Data Collection & Preparation
# ==============================================================================

# Fetch results and team box scores from euroleagueR package
results  <- euroleague_results()
team_box <- euroleague_team_box(season = c(2010:2024))

# Add home/away flag based on row order within each game
team_box2 <- team_box %>%
  group_by(season_code, code) %>%
  mutate(location = case_when(
    row_number() == 1 ~ "home",
    row_number() == 2 ~ "away",
    TRUE              ~ NA_character_
  )) %>%
  ungroup()

# Pivot to wide format: one row per game with _home / _away suffixes
wide_stats <- team_box2 %>%
  pivot_wider(
    id_cols    = c(season_code, code),
    names_from = location,
    names_sep  = "_",
    values_from = -c(season_code, code, location)
  )

# Join with results
full_wide <- results %>%
  left_join(wide_stats, by = c("season_code", "code")) %>%
  filter(season_year >= 2010)

# ==============================================================================
# 2. Feature Engineering
# ==============================================================================

df <- full_wide %>%
  filter(!season_year %in% c("2019", "2020")) %>%  # Exclude COVID seasons
  mutate(
    # Convert to numeric
    venue_capacity = as.numeric(venue_capacity),
    audience       = as.numeric(audience),
    score_home     = as.numeric(score_home),
    score_away     = as.numeric(score_away),

    # Home win indicator
    home_win = score_home > score_away,

    # Field Goal %
    fg_made_home = field_goals_made2_home + field_goals_made3_home,
    fg_made_away = field_goals_made2_away + field_goals_made3_away,
    fg_att_home  = field_goals_attempted2_home + field_goals_attempted3_home,
    fg_att_away  = field_goals_attempted2_away + field_goals_attempted3_away,
    fg_pct_home  = fg_made_home / fg_att_home,
    fg_pct_away  = fg_made_away / fg_att_away,

    # 3-Point %
    tp_pct_home = field_goals_made3_home / field_goals_attempted3_home,
    tp_pct_away = field_goals_made3_away / field_goals_attempted3_away,

    # Free Throw %
    ft_pct_home = free_throws_made_home / free_throws_attempted_home,
    ft_pct_away = free_throws_made_away / free_throws_attempted_away,

    # Rebounds & Assists
    rebounds_home = total_rebounds_home,
    rebounds_away = total_rebounds_away,
    assists_home  = assistances_home,
    assists_away  = assistances_away,

    # Differences
    score_diff   = score_home - score_away,
    fg_diff      = fg_pct_home - fg_pct_away,
    tp_diff      = tp_pct_home - tp_pct_away,
    ft_diff      = ft_pct_home - ft_pct_away,
    reb_diff     = rebounds_home - rebounds_away,
    assists_diff = assists_home - assists_away,
    turn_diff    = turnovers_home - turnovers_away,
    val_diff     = valuation_home - valuation_away
  )

# ==============================================================================
# 3. Chi-squared Test: Home Win Advantage
# ==============================================================================

# Overall home win rate
win_table <- table(df$home_win)
win_test  <- chisq.test(win_table)
cat("\n==== Chi-squared Test: Home Wins ====\n")
print(win_table)
print(win_test)

# Home win vs game phase (group stage vs knockout)
tbl_phase <- table(df$home_win, df$phase_type_is_group_phase)
phase_test <- chisq.test(tbl_phase)
cat("\n==== Chi-squared Test: Home Win x Game Phase ====\n")
print(phase_test)

# ==============================================================================
# 4. Paired Tests: Home vs Away Performance per Metric
# ==============================================================================

metrics <- list(
  "Points"    = c("score_home", "score_away"),
  "FG%"       = c("fg_pct_home", "fg_pct_away"),
  "3P%"       = c("tp_pct_home", "tp_pct_away"),
  "FT%"       = c("ft_pct_home", "ft_pct_away"),
  "Rebounds"  = c("rebounds_home", "rebounds_away"),
  "Assists"   = c("assists_home", "assists_away"),
  "Turnovers" = c("turnovers_home", "turnovers_away"),
  "PIR"       = c("valuation_home", "valuation_away")
)

results_table <- data.frame()

for (label in names(metrics)) {
  home <- df[[metrics[[label]][1]]]
  away <- df[[metrics[[label]][2]]]
  diff <- home - away
  diff <- diff[!is.na(diff)]

  # Normality check
  normality_p <- if (length(diff) >= 3 && length(diff) <= 5000) {
    shapiro.test(diff)$p.value
  } else { NA }

  # Select test based on normality
  if (!is.na(normality_p) && normality_p >= 0.05) {
    test <- t.test(home, away, paired = TRUE)
    test_type <- "paired t-test"
  } else {
    test <- wilcox.test(home, away, paired = TRUE, exact = FALSE)
    test_type <- "Wilcoxon"
  }

  results_table <- rbind(results_table, data.frame(
    Variable    = label,
    Test        = test_type,
    Normality_p = round(normality_p, 4),
    Stat        = round(test$statistic, 3),
    P_value     = round(test$p.value, 4),
    Mean_home   = round(mean(home, na.rm = TRUE), 2),
    Mean_away   = round(mean(away, na.rm = TRUE), 2),
    Median_home = round(median(home, na.rm = TRUE), 2),
    Median_away = round(median(away, na.rm = TRUE), 2),
    Diff_mean   = round(mean(diff, na.rm = TRUE), 2),
    Diff_median = round(median(diff, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  ))
}

# Display results
results_table %>%
  kbl(digits = 3, caption = "Home vs Away Statistical Comparison") %>%
  kable_styling(full_width = FALSE, position = "center") %>%
  row_spec(which(results_table$P_value < 0.05), bold = TRUE)

# ==============================================================================
# 5. Season-by-Season Summary
# ==============================================================================

seasons <- sort(unique(df$season_year))
season_summary <- data.frame()

for (season in seasons) {
  subset_df <- df %>%
    filter(season_year == season, !is.na(score_home), !is.na(score_away))

  season_summary <- rbind(season_summary, data.frame(
    Season          = season,
    Games           = nrow(subset_df),
    Home_Win_Pct    = round(mean(subset_df$home_win) * 100, 2),
    Mean_Score_Diff = round(mean(subset_df$score_home - subset_df$score_away), 2)
  ))
}

print(season_summary)

# Plot: Home win % per season
ggplot(season_summary, aes(x = Season, y = Home_Win_Pct)) +
  geom_line(group = 1, color = "darkblue", linewidth = 1.2) +
  geom_point(size = 2, color = "blue") +
  ylim(0, 100) +
  theme_minimal(base_size = 14) +
  labs(title = "Home Win % per Season", x = "Season", y = "Win %")

# ==============================================================================
# 6. Team Clustering (K-Means on PIR Differences)
# ==============================================================================

keep_vars <- c("score_diff", "tp_diff", "ft_diff", "reb_diff", "turn_diff")

labelize <- function(cl, score) {
  m <- tapply(score, cl, mean, na.rm = TRUE)
  labs <- setNames(c("Strong", "Medium", "Weak"), order(m, decreasing = TRUE))
  labs[as.character(cl)]
}

res_list <- list()
sil_tbl  <- data.frame()

for (s in seasons) {
  ts <- df %>%
    filter(season_year == s) %>%
    group_by(name_home) %>%
    summarise(
      games      = n(),
      score_diff = mean(score_home - score_away, na.rm = TRUE),
      tp_diff    = mean(tp_pct_home - tp_pct_away, na.rm = TRUE),
      ft_diff    = mean(ft_pct_home - ft_pct_away, na.rm = TRUE),
      reb_diff   = mean(total_rebounds_home - total_rebounds_away, na.rm = TRUE),
      turn_diff  = mean(turnovers_home - turnovers_away, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    filter(games >= 6)

  if (nrow(ts) < 4) next

  scaled <- as.data.frame(scale(ts[, keep_vars]))

  # PCA for dimensionality reduction (keep >= 80% variance)
  pca <- prcomp(scaled, center = TRUE, scale. = FALSE)
  ev  <- cumsum(pca$sdev^2) / sum(pca$sdev^2)
  k_pc <- max(2, which(ev >= 0.80)[1])
  pcs  <- pca$x[, 1:k_pc, drop = FALSE]

  # K-means clustering (k=3)
  set.seed(123)
  km <- kmeans(pcs, centers = 3, nstart = 50)
  ts$cluster       <- km$cluster
  ts$cluster_label <- labelize(km$cluster, ts$score_diff)

  # Silhouette score
  sil_val <- mean(silhouette(km$cluster, dist(pcs))[, 3])
  sil_tbl <- rbind(sil_tbl, data.frame(
    season_year = s, silhouette = round(sil_val, 3)
  ))

  ts$season_year <- s
  res_list[[as.character(s)]] <- ts
}

all_clusters <- bind_rows(res_list)

# Silhouette summary
print(sil_tbl)

# ==============================================================================
# 7. Match-up Analysis: Win % and PIR by Cluster Pairing
# ==============================================================================

games <- df %>%
  transmute(
    season_year = as.character(season_year),
    team_home   = name_home,
    team_away   = name_away,
    score_home  = as.numeric(score_home),
    score_away  = as.numeric(score_away)
  )

clusters <- all_clusters %>% select(season_year, name_home, cluster_label)

games2 <- games %>%
  left_join(clusters, by = c("season_year", "team_home" = "name_home")) %>%
  rename(cluster_home = cluster_label) %>%
  left_join(clusters, by = c("season_year", "team_away" = "name_home")) %>%
  rename(cluster_away = cluster_label) %>%
  mutate(
    home_win    = score_home > score_away,
    home_margin = score_home - score_away,
    matchup     = paste(cluster_home, "vs", cluster_away)
  ) %>%
  filter(!is.na(cluster_home) & !is.na(cluster_away))

# Descriptive table by matchup
desc_tbl <- games2 %>%
  group_by(matchup) %>%
  summarise(
    Games      = n(),
    HomeWins   = sum(home_win),
    HomeWinPct = round(mean(home_win) * 100, 1),
    MeanMargin = round(mean(home_margin), 2),
    .groups    = "drop"
  )
print(desc_tbl)

# Statistical tests
chi_total  <- chisq.test(table(games2$matchup, games2$home_win))
kw_total   <- kruskal_test(home_margin ~ matchup, data = games2)
dunn_total <- games2 %>%
  dunn_test(home_margin ~ matchup, p.adjust.method = "bonferroni")

cat("\n==== Chi-squared: HomeWin ~ Matchup ====\n")
print(chi_total)

cat("\n==== Kruskal-Wallis: Margin ~ Matchup ====\n")
print(kw_total)

# ==============================================================================
# 8. Visualizations
# ==============================================================================

# Home Win % by matchup
ggplot(desc_tbl, aes(x = reorder(matchup, -HomeWinPct), y = HomeWinPct, fill = matchup)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(HomeWinPct, "%")), vjust = -0.4, size = 4) +
  ylim(0, 100) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(title = "Home-Win % by Match-up (PIR clusters)", x = NULL, y = "Win %")

# Mean PIR difference by matchup
pir_matchup <- games2 %>%
  mutate(pir_diff = score_home - score_away) %>%
  group_by(matchup) %>%
  summarise(MeanPIR = round(mean(pir_diff), 2), .groups = "drop")

ggplot(pir_matchup, aes(x = reorder(matchup, -MeanPIR), y = MeanPIR, fill = matchup)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = MeanPIR), vjust = -0.4, size = 4) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
  labs(title = "Mean PIR-diff by Match-up (PIR clusters)", x = NULL, y = "Mean PIR Difference")
