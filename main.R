source("setting.R")
source("functions.R")

library(estimatr)
library(ggforce)

set.seed(42)

if (dir_exists("figure/")) {dir_delete("figure")}

dir_create("figure")

# 均一分散 --------------------------------------------------------------------

N <- 50
df_homogeneous <- tibble(
  x = runif(N, -2, 2),
  u = rnorm(N, 0, 1),
  y = x + u
)

df_homogeneous %>%
  lm(y ~ x, data = .) %>%
  tidy()


df_homogeneous %>%
  plot_scatter_regression("シミュレーションデータと線形回帰の結果")

M <- 5000
df_simulation_homogenious <- tibble(
  m = rep(seq_len(M), each = N),
  x = runif(N * M, -2, 2),
  u = rnorm(N * M, 0, 1),
  y = x + u
)



df_simulation_homogenious %>%
  plot_regression_lines("何度もデータを抽出した場合の回帰直線のばらつき")


df_result_homogenious <- df_simulation_homogenious %>%
  nest(data = c(x, u, y)) %>%
  mutate(result = map(data, ~ tidy(lm(y ~ x, data = .)))) %>%
  select(m, result) %>%
  unnest(c(m, result)) %>%
  filter(term == "x")


df_result_homogenious %>%
  plot_coef_histogram("回帰係数のばらつき")



df_result_homogenious %>%
  plot_se_histogram("実際の回帰係数のばらつきと推定された標準誤差")



# 不均一分散 -------------------------------------------------------------------

df_heterogeneous <- tibble(
  x = runif(N, -2, 2),
  u = rnorm(N, 0, abs(x)),
  y = x + u
)

# df_homogeneous %>%
#   plot_scatter_regression("均一分散の場合")

df_heterogeneous %>%
  plot_scatter_regression("不均一分散の場合")


df_simulation_heterogeneous <- tibble(
  m = rep(seq_len(M), each = N),
  x = runif(N * M, -2, 2),
  u = rnorm(N * M, 0, abs(x)),
  y = x + u
)

df_simulation_heterogeneous %>%
  plot_regression_lines("回帰直線のばらつき（不均一分散の場合）")


df_result_heterogeneous <- df_simulation_heterogeneous %>%
  nest(data = c(x, u, y)) %>%
  mutate(result = map(data, ~ tidy(lm(y ~ x, data = .)))) %>%
  select(m, result) %>%
  unnest(c(m, result)) %>%
  filter(term == "x")


df_result_heterogeneous %>%
  plot_coef_histogram("回帰係数のばらつき（不均一分散の場合）")

df_result_heterogeneous %>%
  plot_se_histogram("実際不均一分散の場合）")


# なぜ不均一分散があると標準誤差をうまく推定できないのか ---------------------------------------------

generate_simulation_data <- function(N, M) {
  make_sd <- function(x, sigmas) {
    case_when(
      x == 1 ~ sigmas[1],
      x == 2 ~ sigmas[2],
      x == 3 ~ sigmas[3],
    )
  }

  generate <- function(N, M, sigmas) {
    tibble(
      m = rep(seq_len(M), each = N),
      x = sample(c(1, 2, 3), N * M, replace = TRUE),
      u = rnorm(N * M, 0, make_sd(x, sigmas)),
      y = x + u
    )
  }

  bind_rows(
    generate(N, M, sqrt(c(16, 16, 16))) %>% mutate(type = 1),
    generate(N, M, sqrt(c(1, 31, 16))) %>% mutate(type = 2),
    generate(N, M, sqrt(c(31, 1, 16))) %>% mutate(type = 3)
  ) %>%
    mutate(type = factor(type, labels = c("均一分散", "不均一分散(1)", "不均一分散(2)")))
}


df_compare <- generate_simulation_data(500, 1)

df_compare %>%
  plot_sina("分散タイプ別のシミュレーションデータ")

df_compare_regression_lines <- generate_simulation_data(50, 50)

df_compare_regression_lines %>%
  plot_regression_lines_with_facet("回帰直線のばらつきの比較") 


df <- generate_simulation_data(N, M)

df_compare_result <- df %>%
  nest(data = c(x, u, y)) %>%
  mutate(result = map(data, ~ tidy(lm(y ~ x, data = .)))) %>%
  select(type, m, result) %>%
  unnest(c(type, m, result)) %>%
  filter(term == "x")

df_compare_result %>%
  group_by(type) %>%
  summarise(
    実際の回帰係数のばらつき = sd(estimate),
    推定された標準誤差の平均値 = mean(std.error)
  ) %>% 
  openxlsx::write.xlsx("df_compare_result.xlsx")

# df_compare_result %>% 
#   ggplot(aes(estimate, fill = type)) +
#   # geom_histogram(
#   #   position = position_identity(),
#   #   alpha = 0.5,
#   #   bins = 50
#   # ) +
#   geom_density( position = position_identity(), alpha = 0.5) + 
#   scale_x_continuous() +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
#   scale_fill_brewer(name = "", palette = "Set2") +
#   labs(
#     x = "推定された回帰係数のばらつき（標準誤差）",
#     y = "度数",
#     title = "title"
#   ) +
#   theme_line() +
#   theme(legend.position = c(0.8, 0.7))

# 頑健な標準誤差 -----------------------------------------------------------------

df_heterogeneous %>%
  estimatr::lm_robust(y ~ x, data = ., se_type = "HC0") %>%
  tidy()


df_result_robust <- df_simulation_heterogeneous %>%
  nest(data = c(x, u, y)) %>%
  mutate(
    classical = map(data, ~ tidy(lm_robust(y ~ x, data = ., se_type = "classical"))),
    HC0 = map(data, ~ tidy(lm_robust(y ~ x, data = ., se_type = "HC0"))),
    HC1 = map(data, ~ tidy(lm_robust(y ~ x, data = ., se_type = "HC1"))),
    HC2 = map(data, ~ tidy(lm_robust(y ~ x, data = ., se_type = "HC2"))),
    HC3 = map(data, ~ tidy(lm_robust(y ~ x, data = ., se_type = "HC3")))
  ) %>%
  pivot_longer(
    cols = c(classical, HC0, HC1, HC2, HC3),
    names_to = "se_type"
  ) %>%
  select(m, se_type, value) %>%
  unnest(cols = c(value)) %>%
  filter(term == "x")


df_result_robust %>%
  filter(se_type %in% c("classical", "HC0")) %>%
  plot_se_histograms("推定方法による回帰係数の標準誤差の比較")


df_result_robust %>%
  plot_se_histograms("推定方法による回帰係数の標準誤差の比較（補足）")
