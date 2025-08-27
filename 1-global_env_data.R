# --- 0. 环境设置 ---
rm(list = ls())
library(fredr)
library(tidyverse)
library(lubridate)

fredr_set_key("d66e8c49c188ecf6f774d5e55bb275b9")


# --- 1. 数据获取与处理 ---
# 1.1 处理需要聚合的 FRED 数据 (包括用于计算通胀的CPI)
series_to_aggregate <- c(
  t_bill_3m = "DTB3",
  t_note_10y = "DGS10",
  brent_oil = "DCOILBRENTEU",
  dollar_index = "DTWEXBGS",
  cpi_inflation_rate = "CPIAUCSL" # 直接命名为最终想要的变量名
)

fred_aggregated_quarterly <- map_dfr(
  names(series_to_aggregate),
  ~ fredr(
    series_id = series_to_aggregate[.],
    # 为CPI年同比计算提前一年获取数据
    observation_start = as.Date("2007-10-01"),
    observation_end = as.Date("2024-12-31")
  ) |> mutate(friendly_name = .) # 将友好名称添加为一列
) |>
  # 对CPI数据进行特殊处理，计算年同比通胀率
  group_by(friendly_name) |>
  mutate(
    value = if_else(
      friendly_name == "cpi_inflation_rate",
      ((value / lag(value, 12)) - 1) * 100,
      value
    )
  ) |>
  ungroup() |>
  filter(!is.na(value)) |> # 移除因lag产生的NA值
  # 对所有序列进行季度聚合
  group_by(
    friendly_name,
    quarter_start_date = floor_date(date, "quarter")
  ) |>
  summarise(avg_value = mean(value), .groups = "drop") |>
  # 转换为宽格式
  pivot_wider(names_from = friendly_name, values_from = avg_value)


# 1.2 处理本身就是季度的 FRED 数据
wb_commodities_quarterly <- fredr(
  series_id = "PALLFNFINDEXQ",
  observation_start = as.Date("2008-10-01"),
  observation_end = as.Date("2024-12-31")
) |>
  select(quarter_start_date = date, wb_commodities = value)


# 1.3 处理 BDI 和 EPU 数据 (来自 BDI.csv)
bdi_epu_quarterly <- read_csv(
  "BDI.csv",
  col_types = cols(Date = col_date(format = "%m/%d/%Y"), BDI = col_character(), EPU = col_double())
) |>
  mutate(BDI = parse_number(BDI)) |>
  group_by(quarter_start_date = floor_date(Date, "quarter")) |>
  summarise(bdi_avg = mean(BDI, na.rm = TRUE), epu_avg = mean(EPU, na.rm = TRUE), .groups = "drop")


# 1.4 处理 VIX 数据 (来自 VIX_History.csv)
vix_quarterly <- read_csv(
  "VIX_History.csv",
  col_types = cols(DATE = col_date(format = "%m/%d/%Y"), VIX = col_double())
) |>
  group_by(quarter_start_date = floor_date(DATE, "quarter")) |>
  summarise(vix_avg = mean(VIX, na.rm = TRUE), .groups = "drop")


# --- 2. 合并所有数据源 ---

# 将所有处理好的数据框放入一个列表
list_of_dfs <- list(
  fred_aggregated_quarterly,
  wb_commodities_quarterly,
  bdi_epu_quarterly,
  vix_quarterly
)

# 使用 purrr::reduce 进行链式左连接，将所有数据框合并
merged_data <- reduce(list_of_dfs, left_join, by = "quarter_start_date")


# --- 3. 最终计算与整理 ---

final_global_env_vars <- merged_data |>
  # 计算期限利差和实际利率
  mutate(
    term_spread = t_note_10y - t_bill_3m,
    real_interest_rate = t_bill_3m - cpi_inflation_rate
  ) |>
  # 移除用于计算的中间变量 cpi_inflation_rate
  select(-cpi_inflation_rate) |>
  # 确保所有列的顺序合乎逻辑，并按日期排序
  select(
    quarter_start_date, t_bill_3m, t_note_10y, term_spread, real_interest_rate,
    dollar_index, brent_oil, wb_commodities, bdi_avg, epu_avg, vix_avg
  ) |>
  arrange(quarter_start_date) |> 
  # 确保只保留2008-Q4及之后的数据
  filter(quarter_start_date >= as.Date("2008-10-01"))

write_csv(final_global_env_vars, file = "global_env_data.csv")
