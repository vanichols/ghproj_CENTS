# ------------------------------------------------------------------
# Multi-criteria assessment of 30 cropping systems across ALL weight
# combinations of six metrics.
#
# Idea: sweep every weight vector w (w_i >= 0, sum(w) = 1, step = 1/n),
# compute each system's weighted-sum score, rank the systems under each
# weighting, then ask: how robust is each system's performance to the
# choice of weights?
# ------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(scales)

# ---- 1. Settings --------------------------------------------------
infile <- read_csv("data/tidy_multicriteria-values.csv")

# Weight resolution: weights move in steps of 1/n_steps.
#   n_steps = 10 ->   3,003 weight combinations
#   n_steps = 20 ->  53,130 weight combinations (still fast)
n_steps <- 10

# Values look pre-scaled to 0-1. Set FALSE for any metric where a LOWER
# value is better (it will be flipped to 1 - value). ASSUMPTION: all TRUE.
higher_is_better <- c(
  fallvegvalue = TRUE, fallbio = TRUE, yield = TRUE,
  minitox = TRUE, perennweeds = TRUE, pctcov = TRUE
)

# ---- 2. Data -> matrix (30 systems x 6 metrics) ------------------
dat <- infile

wide <- dat |>
  pivot_wider(names_from = name, values_from = value) |>
  arrange(till_id, cctrt_id, straw_id)

metrics <- names(higher_is_better)
V <- as.matrix(wide[, metrics])
for (m in metrics) if (!higher_is_better[[m]]) V[, m] <- 1 - V[, m]

systems <- wide |> select(cctrt_id, till_id, straw_id)

# ---- 3. All weight combinations (simplex grid) -------------------
# Stars-and-bars: every way to split n_steps units across k metrics
make_weights <- function(k, n) {
  bars <- utils::combn(n + k - 1, k - 1)
  parts <- apply(bars, 2, function(b) diff(c(0, b, n + k)) - 1)
  t(parts) / n                       # rows = weight vectors
}
W <- make_weights(length(metrics), n_steps)
colnames(W) <- metrics
stopifnot(all(abs(rowSums(W) - 1) < 1e-9))
message(nrow(W), " weight combinations")

# ---- 4. Score and rank every system under every weighting --------
S <- V %*% t(W)                                        # 30 x n_weights
R <- apply(-S, 2, rank, ties.method = "min")           # 1 = best

# ---- 5. Summaries per system -------------------------------------
systems <- systems |>
  mutate(
    win_share = rowMeans(R == 1),                       # % of weightings it ranks #1
    top5_share = rowMeans(R <= 5),
    rank_med  = apply(R, 1, median),
    rank_lo   = apply(R, 1, quantile, probs = 0.05),
    rank_hi   = apply(R, 1, quantile, probs = 0.95),
    label     = paste(cctrt_id, till_id, straw_id, sep = " | ")
  )

# Average weight profile among the weightings under which each system wins
win_profile <- map_dfr(seq_len(nrow(systems)), function(i) {
  idx <- which(R[i, ] == 1)
  if (length(idx) == 0) return(NULL)
  tibble(label = systems$label[i],
         metric = metrics,
         mean_weight = colMeans(W[idx, , drop = FALSE]))
}) |>
  left_join(systems, by = "label") |>
  mutate(metric = factor(metric, levels = metrics))

win_profile |> 
  write_csv("data/tidy_system-winning-weight-profile.csv")

# Order cropping-system rows by overall median rank
cc_order <- systems |>
  group_by(cctrt_id) |>
  summarise(m = median(rank_med)) |>
  arrange(desc(m)) |>
  pull(cctrt_id)
systems <- systems |> mutate(cctrt_id = factor(cctrt_id, levels = cc_order))
win_profile <- win_profile |> mutate(cctrt_id = factor(cctrt_id, levels = cc_order))

# ---- 6. Visual ---------------------------------------------------
straw_cols <- c(removed = "#C8553D", retained = "#2A6F97")

# Panel A: how robust is each system's rank to the choice of weights?
#   dot  = median rank across all weightings
#   bar  = 5th-95th percentile of rank
#   text = % of weightings under which the system ranks #1 overall
pA <- ggplot(systems, aes(y = cctrt_id, colour = straw_id)) +
  geom_vline(xintercept = c(1, 10, 20, 30), colour = "grey90") +
  geom_linerange(aes(xmin = rank_lo, xmax = rank_hi),
                 linewidth = 3, alpha = 0.35,
                 position = position_dodge(width = 0.6)) +
  geom_point(aes(x = rank_med, size = win_share),
             position = position_dodge(width = 0.6)) +
  geom_text(aes(x = rank_hi,
                label = ifelse(win_share > 0, percent(win_share, 1), "")),
            hjust = -0.3, size = 3, show.legend = FALSE,
            position = position_dodge(width = 0.6)) +
  facet_wrap(~ till_id, ncol = 1, strip.position = "left") +
  scale_x_continuous(breaks = c(1, 10, 20, 30),
                     expand = expansion(mult = c(0.02, 0.12))) +
  scale_colour_manual(values = straw_cols, name = "Straw") +
  scale_size_area(max_size = 6, labels = percent, name = "Share of weightings\nwhere system ranks #1") +
  labs(title = "How robust is each system to the choice of weights?",
       subtitle = "Dot = median rank; bar = 5th\u201395th percentile of rank across all weight combinations (1 = best of 30)",
       x = "Rank among the 30 systems", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot")

# Panel B: *why* do winners win? Mean weight profile when they rank #1
metric_cols <- c("#8DA0CB", "#66C2A5", "#E5B93C", "#D95F5F", "#9B7EBD", "#7FB069")
pB <- ggplot(win_profile,
             aes(x = mean_weight, y = interaction(cctrt_id, straw_id, sep = " / "), fill = metric)) +
  geom_col(width = 0.75, colour = "white", linewidth = 0.3) +
  facet_wrap(~ till_id, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = percent, expand = c(0, 0)) +
  scale_fill_manual(values = setNames(metric_cols, metrics), name = "Metric") +
  labs(title = "Weight profile under which each winning system ranks #1",
       subtitle = "Only systems that win under at least one weighting are shown",
       x = "Average weight on each metric", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        plot.title.position = "plot",
        strip.text = element_text(face = "bold"))

p <- pA + pB + plot_layout(widths = c(1.3, 1))
print(p)

pA
pB

ggsave("multicriteria_weight_sweep.png", p, width = 15, height = 9, dpi = 300, bg = "white")

# ---- 7. Table of results -----------------------------------------
results <- systems |>
  select(cctrt_id, till_id, straw_id, win_share, top5_share, rank_med, rank_lo, rank_hi) |>
  arrange(desc(win_share), rank_med)
print(results, n = 30)
write_csv(results, "multicriteria_weight_sweep_results.csv")