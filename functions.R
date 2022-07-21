source("setting.R")

save_plot <- function(plot, fname, width = 10, height = 5) {
  ggsave(
    plot = plot,
    filename = glue("figure/{fname}.png"),
    width = width,
    height = height,
    dpi = 272
  )
}

# 散布図+回帰直線
plot_scatter_regression <- function(df, title, ylims = c(NA, NA)) {
  g <- df %>%
    ggplot(aes(x, y)) +
    geom_point(color = "white", fill = cols[7], size = 3, shape = 21) +
    geom_smooth(
      method = "lm",
      se = F,
      color = cols[2],
      size = 1
    ) +
    scale_x_continuous(
      breaks = breaks_width(1),
      limits = c(-2, 2)
    ) +
    scale_y_continuous(
      breaks = breaks_width(2),
      limits = ylims
    ) +
    labs(x = "X", y = "Y") +
    theme_scatter()

  save_plot(g, title)

  return(g)
}

# 回帰直線をたくさん描く
plot_regression_lines <- function(df, title, n_lines = 50) {
  g <- df %>%
    filter(m <= n_lines) %>%
    ggplot(aes(x, y, group = m)) +
    geom_smooth(
      method = "lm",
      se = F,
      size = 0.5,
      color = cols[2],
      fullrange = T
    ) +
    scale_x_continuous(breaks = breaks_width(1)) +
    scale_y_continuous(breaks = breaks_width(2)) +
    scale_color_brewer(palette = "Set2") +
    labs(x = "X", y = "Y") +
    theme_scatter()

  save_plot(g, title)

  return(g)
}

plot_regression_lines_with_facet <- function(df, title, n_lines = 50) {
  g <- df %>%
    filter(m <= n_lines) %>%
    ggplot(aes(x, y, group = m)) +
    geom_smooth(
      method = "lm",
      se = F,
      size = 0.5,
      color = cols[2],
      fullrange = T
    ) +
    facet_rep_wrap(~type, repeat.tick.labels = "all") + 
    scale_x_continuous(breaks = breaks_width(1)) +
    scale_y_continuous(breaks = breaks_width(2)) +
    scale_color_brewer(palette = "Set2") +
    labs(x = "X", y = "Y") +
    theme_scatter()
  
  save_plot(g, title)
  
  return(g)
}

# 回帰係数の分布
plot_coef_histogram <- function(df, title) {
  g <- df %>%
    ggplot(aes(estimate)) +
    geom_histogram(
      fill = cols[1],
      alpha = 0.7
    ) +
    scale_x_continuous() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      x = "回帰係数（傾き）",
      y = "度数"
    ) +
    theme_line()

  save_plot(g, title)

  return(g)
}

# 回帰係数標準誤差の分布
plot_se_histogram <- function(df, title) {
  sd_estimate <- sd(df$estimate)

  g <- df %>%
    ggplot(aes(std.error)) +
    geom_histogram(
      fill = cols[1],
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = sd_estimate,
      color = cols[8],
      linetype = "dotted"
    ) +
    # annotate(
    #   geom = "text",
    #   x = sd_estimate,
    #   y = 0,
    #   label = glue("実際の回帰係数のばらつき{number(sd_estimate, 0.001)}"),
    #   color = cols[9],
    #   family = base_family,
    #   size = 12 / .pt,
    #   hjust = 1.1,
    #   vjust = -1
    # ) +
    scale_x_continuous() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(
      x = "回帰係数の標準誤差",
      y = "度数"
    ) +
    theme_line()

  save_plot(g, title)

  return(g)
}

# 回帰係数標準誤差の分布を比較
plot_se_histograms <- function(df, title) {
  sd_estimate <- df %>%
    filter(se_type == "classical") %>%
    summarise(sd(estimate)) %>%
    pull()

  g <- df %>%
    ggplot(aes(std.error, fill = se_type)) +
    geom_histogram(
      position = position_identity(),
      alpha = 0.5,
      bins = 50
    ) +
    geom_vline(
      xintercept = sd_estimate,
      color = cols[8],
      linetype = "dotted"
    ) +
    # annotate(
    #   geom = "text",
    #   x = sd_estimate,
    #   y = 0,
    #   label = glue("実際の回帰係数のばらつき{number(sd_estimate, 0.001)}"),
    #   color = cols[9],
    #   family = base_family,
    #   size = 12 / .pt,
    #   hjust = -0.05,
    #   vjust = -1
    # ) +
    scale_x_continuous() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_fill_brewer(name = "", palette = "Set2") +
    labs(
      x = "回帰係数の標準誤差",
      y = "度数"
    ) +
    theme_line() +
    theme(legend.position = c(0.8, 0.7))

  save_plot(g, title)

  return(g)
}


plot_sina <- function(df, title) {
  g <- df %>%
    ggplot(aes(x, y)) +
    geom_sina(aes(x = factor(x)), color = cols[7], alpha = 0.3) +
    geom_point(
      data = df %>% group_by(x) %>% summarise(y = mean(y)),
      color =  cols[6],
      size = 3
    ) +
    facet_rep_wrap(~type, repeat.tick.labels = "y") +
    labs(x = "X", y = "Y") +
    theme_scatter()

  save_plot(g, title)

  return(g)
}
