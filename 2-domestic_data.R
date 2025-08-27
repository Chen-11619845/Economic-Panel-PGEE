# 清空环境
rm(list = ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(BCDating)

# ---------- 通用工具函数 ----------
# 读取任意一个 IMF 宽表，保留国家+所有季度列，转成长表，并统一国家名与季度变量
load_imf_wide_to_long <- function(filepath, indicator_name,
                                  country_map_tbl,
                                  keep_countries_std) {
  df_raw <- read_csv(filepath, show_col_types = FALSE)
  
  # 识别季度列（形如 2008-Q4）
  quarter_cols <- names(df_raw) %>% str_subset("^\\d{4}-Q[1-4]$")
  
  df_long <- df_raw %>%
    # 保留国家+季度列
    select(COUNTRY, all_of(quarter_cols)) %>%
    # 长表
    pivot_longer(cols = all_of(quarter_cols),
                 names_to = "QUARTER",
                 values_to = "value") %>%
    # 按 IMF 名称映射为标准国家名
    inner_join(country_map_tbl, by = c("COUNTRY" = "imf_name")) %>%
    mutate(COUNTRY = std_name) %>%
    select(-std_name) %>%
    # 仅保留目标23国
    filter(COUNTRY %in% keep_countries_std) %>%
    # 添加指标名
    mutate(indicator = indicator_name)
  
  return(df_long)
}

# ---------- 国家映射与目标集合 ----------
# IMF 文件里的确切国家名（含你指出的差异命名）
country_map_tbl <- tribble(
  ~imf_name,                         ~std_name,
  # 亚洲（去掉 Taiwan）
  "China, People's Republic of",     "China",
  "India",                           "India",
  "Indonesia",                       "Indonesia",
  "Korea, Republic of",              "Korea",
  "Malaysia",                        "Malaysia",
  "Pakistan",                        "Pakistan",
  "Philippines",                     "Philippines",
  "Thailand",                        "Thailand",
  
  # 拉美
  "Brazil",                          "Brazil",
  "Chile",                           "Chile",
  "Colombia",                        "Colombia",
  "Mexico",                          "Mexico",
  "Peru",                            "Peru",
  
  # 欧洲、中东与非洲（EEMEA）
  "Czech Republic",                  "Czech Republic",
  "Egypt, Arab Republic of",         "Egypt",
  "Greece",                          "Greece",
  "Hungary",                         "Hungary",
  "Poland, Republic of",             "Poland",
  "Qatar",                           "Qatar",
  "Saudi Arabia",                    "Saudi Arabia",
  "South Africa",                    "South Africa",
  "Türkiye, Republic of",            "Turkey"
)

# 目标 22 国（标准名；注意已排除 Taiwan）
keep_countries_std <- c(
  # Asia (9)
  "China","India","Indonesia","Korea","Malaysia","Pakistan","Philippines","Thailand",
  # LatAm (5)
  "Brazil","Chile","Colombia","Mexico","Peru",
  # EEMEA (9)
  "Czech Republic","Egypt","Greece","Hungary",
  "Poland","Qatar","Saudi Arabia","South Africa","Turkey","United Arab Emirates"
) %>% unique()

# 为了安全（某些人会手滑把 UAE 写成 U.A.E.），这里只保留我们在映射表能找到的 23 个
keep_countries_std <- keep_countries_std[keep_countries_std %in% country_map_tbl$std_name]

# ---------- 读取并规整五个指标 ----------
cpi_long <- load_imf_wide_to_long("CPI.csv",              "CPI",          country_map_tbl, keep_countries_std)
ppi_long <- load_imf_wide_to_long("PPI.csv",              "PPI",          country_map_tbl, keep_countries_std)
fx_long  <- load_imf_wide_to_long("foreign reserve.csv",  "FX_Reserves",  country_map_tbl, keep_countries_std)
fdi_long <- load_imf_wide_to_long("FDI.csv",              "FDI_Inflows",  country_map_tbl, keep_countries_std)
bm_long  <- load_imf_wide_to_long("broad money.csv",      "Broad_Money",  country_map_tbl, keep_countries_std)
GDP_long <- load_imf_wide_to_long("GDP.csv",              "GDP",          country_map_tbl, keep_countries_std)
Consumption_long <- load_imf_wide_to_long("Consumption.csv",              "Consumption",          country_map_tbl, keep_countries_std)
Investment_long <- load_imf_wide_to_long("Investment.csv",              "Investment",          country_map_tbl, keep_countries_std)
CA_long <- load_imf_wide_to_long("external balance.csv",              "External_Balance",          country_map_tbl, keep_countries_std)

# 合并成长表（国家 × 季度 × 指标 × 数值）
all_long <- bind_rows(cpi_long, ppi_long, fx_long, fdi_long, bm_long, GDP_long, Consumption_long, Investment_long, CA_long)

# ---------- 转成宽表（典型 panel data：一行=国家×季度；列=各指标） ----------
panel_imf <- all_long %>%
  mutate(
    # 规范季度排序键（YYYY-Qn）
    year = as.integer(str_sub(QUARTER, 1, 4)),
    qtr  = as.integer(str_sub(QUARTER, 7, 7)),
    # 构造一个可排序的键
    sort_key = year * 10 + qtr
  ) %>%
  arrange(COUNTRY, sort_key, indicator) %>%
  select(COUNTRY, QUARTER, indicator, value) %>%
  # 宽表
  pivot_wider(names_from = indicator, values_from = value) %>%
  # 可选：限制样本期为 2008-Q4 到 2024-Q4（以免原始文件带入额外列）
  filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4") %>%
  arrange(COUNTRY, QUARTER)




# ----------- 国家映射表（与前面一致） -----------
bis_to_std <- tribble(
  ~bis_name,                   ~std_name,
  "CN:China",                  "China",
  "IN:India",                  "India",
  "ID:Indonesia",              "Indonesia",
  "KR:Korea",                  "Korea",
  "MY:Malaysia",               "Malaysia",
  "PK:Pakistan",               "Pakistan",
  "PH:Philippines",            "Philippines",
  "TH:Thailand",               "Thailand",
  "BR:Brazil",                 "Brazil",
  "CL:Chile",                  "Chile",
  "CO:Colombia",               "Colombia",
  "MX:Mexico",                 "Mexico",
  "PE:Peru",                   "Peru",
  "CZ:Czechia",                "Czech Republic",
  "EG:Egypt",                  "Egypt",
  "GR:Greece",                 "Greece",
  "HU:Hungary",                "Hungary",
  "PL:Poland",                 "Poland",
  "QA:Qatar",                  "Qatar",
  "SA:Saudi Arabia",           "Saudi Arabia",
  "ZA:South Africa",           "South Africa",
  "TR:Türkiye",                "Turkey"
)

keep_countries_23 <- bis_to_std$std_name

# ----------- 通用处理函数 -----------
process_bis_file <- function(filepath, indicator_name){
  raw <- read_excel(filepath, sheet = 1) # 默认第一张表
  
  raw %>%
    inner_join(bis_to_std, by = c("Country" = "bis_name")) %>%
    rename(COUNTRY = std_name) %>%
    filter(COUNTRY %in% keep_countries_23) %>%
    mutate(
      Period = as.Date(Period),
      QUARTER = paste0(year(Period), "-Q", quarter(Period))
    ) %>%
    filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4") %>%
    transmute(COUNTRY, QUARTER, !!indicator_name := as.numeric(Value)) %>%
    arrange(COUNTRY, QUARTER)
}

# ----------- 分别读取四个文件 -----------
credit_gap   <- process_bis_file("credit to gdp gap.xlsx",                "Credit_to_GDP_Gap")
property     <- process_bis_file("property prices.xlsx",                  "Property_Price")
credit_total <- process_bis_file("Credit to the non-financial sector.xlsx","Credit_Total")
capital_in   <- process_bis_file("capital inflow.xlsx",                   "Capital_Inflow")
exchange_rate<- process_bis_file("exchaneg rate.xlsx",                    "Exchange_Rate")
# ----------- 合并成一个 panel 宽表 -----------
panel_bis <- credit_gap %>%
  full_join(property,     by = c("COUNTRY","QUARTER")) %>%
  full_join(credit_total, by = c("COUNTRY","QUARTER")) %>%
  full_join(capital_in,   by = c("COUNTRY","QUARTER")) %>%
  full_join(exchange_rate,   by = c("COUNTRY","QUARTER")) %>%
  arrange(COUNTRY, QUARTER)


# 检查一下
panel_bis %>% group_by(COUNTRY) %>% summarise(n_quarters = n())


policy_q <- read_excel("policy rate.xlsx", sheet = 1) %>%
  inner_join(bis_to_std, by = c("Country" = "bis_name")) %>%
  rename(COUNTRY = std_name) %>%
  filter(COUNTRY %in% keep_countries_23) %>%
  mutate(
    Period  = as.Date(Period),
    QUARTER = paste0(year(Period), "-Q", quarter(Period))
  ) %>%
  filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4") %>%
  group_by(COUNTRY, QUARTER) %>%
  summarise(Policy_Rate = mean(as.numeric(Value), na.rm = TRUE), .groups = "drop") %>%
  arrange(COUNTRY, QUARTER)

panel_bis <- panel_bis %>%
  full_join(policy_q, by = c("COUNTRY", "QUARTER")) %>%
  arrange(COUNTRY, QUARTER)

panel_final <- panel_imf %>%
  full_join(panel_bis, by = c("COUNTRY", "QUARTER")) %>%
  arrange(COUNTRY, QUARTER)


# ---------- 目标国家（23个，不含台湾） ----------
keep_countries_23 <- c(
  # Asia (8)
  "China","India","Indonesia","Korea","Malaysia","Pakistan","Philippines","Thailand",
  # LatAm (5)
  "Brazil","Chile","Colombia","Mexico","Peru",
  # EEMEA (10)
  "Czech Republic","Egypt","Greece","Hungary","Poland",
  "Qatar","Saudi Arabia","South Africa","Turkey"
)

# ---------- 读取 ILO 数据 ----------
unemp_raw <- read_excel("unemployment.xlsx", sheet = 1)

# ---------- 统一国家命名 ----------
# 注意 ILO 用 "Czechia" 而不是 "Czech Republic"
# 也可能用 "Republic of Korea" 而不是 "Korea"
# 所以需要做个映射
ilo_to_std <- tribble(
  ~ilo_name,                      ~std_name,
  "China",                        "China",
  "India",                        "India",
  "Indonesia",                    "Indonesia",
  "Republic of Korea",            "Korea",
  "Malaysia",                     "Malaysia",
  "Pakistan",                     "Pakistan",
  "Philippines",                  "Philippines",
  "Thailand",                     "Thailand",
  "Brazil",                       "Brazil",
  "Chile",                        "Chile",
  "Colombia",                     "Colombia",
  "Mexico",                       "Mexico",
  "Peru",                         "Peru",
  "Czechia",                      "Czech Republic",
  "Egypt",                        "Egypt",
  "Greece",                       "Greece",
  "Hungary",                      "Hungary",
  "Poland",                       "Poland",
  "Qatar",                        "Qatar",
  "Saudi Arabia",                 "Saudi Arabia",
  "South Africa",                 "South Africa",
  "Turkey",                       "Turkey",
  "United Arab Emirates",         "United Arab Emirates"
)

# ---------- 数据处理 ----------
unemp_q <- unemp_raw %>%
  inner_join(ilo_to_std, by = c("Country" = "ilo_name")) %>%
  rename(COUNTRY = std_name) %>%
  filter(COUNTRY %in% keep_countries_23) %>%
  group_by(COUNTRY, time) %>%
  summarise(Unemployment = mean(obs_value, na.rm = TRUE), .groups = "drop") %>%
  filter(time >= "2008Q4", time <= "2024Q4") %>%
  rename(QUARTER = time) %>%
  arrange(COUNTRY, QUARTER)



norm_quarter <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "\\s+", "")        # 去空格
  x <- str_replace_all(x, "[–—]", "-")       # 破折号统一成 "-"
  x <- str_replace_all(x, "^(\\d{4})[- ]?Q0?([1-4])$", "\\1-Q\\2") # 2008Q01/2008-Q01/2008 Q1 -> 2008-Q1
  x <- str_replace_all(x, "^(\\d{4})Q([1-4])$", "\\1-Q\\2")        # 2008Q1 -> 2008-Q1
  x
}
unemp_q <- unemp_q %>%
  mutate(QUARTER = norm_quarter(QUARTER))


panel_final <- panel_final %>%
  full_join(unemp_q, by = c("COUNTRY", "QUARTER")) %>%
  arrange(COUNTRY, QUARTER)



finopen_raw <- read_excel("financial openness.xlsx", sheet = 1)
names(finopen_raw) <- names(finopen_raw) %>% tolower() %>% str_replace_all("\\s+", "_")

# 尝试鲁棒识别列名（必要时可手动改成你的确切列名）
country_col <- names(finopen_raw)[str_detect(names(finopen_raw), "country|econom|nation")]
year_col    <- names(finopen_raw)[str_detect(names(finopen_raw), "^year$|fiscal_year|time|date")]
# 取数值列作为指数列（排除 year 列）
value_col   <- setdiff(names(finopen_raw), c(country_col, year_col))
value_col   <- value_col[map_lgl(finopen_raw[value_col], is.numeric)][1]

finopen_annual <- finopen_raw %>%
  transmute(
    Country_raw = .data[[country_col]],
    Year        = as.integer(.data[[year_col]]),
    Fin_Openness = as.numeric(.data[[value_col]])
  ) %>%
  # 统一到你的标准国家名；优先复用你上文已定义的映射（若两个都在环境中，优先 ilo_to_std）
  {
    if (exists("ilo_to_std")) {
      left_join(., ilo_to_std, by = c("Country_raw" = "ilo_name")) %>%
        mutate(COUNTRY = coalesce(std_name, Country_raw)) %>%
        select(-std_name)
    } else if (exists("country_map_tbl")) {
      left_join(., country_map_tbl, by = c("Country_raw" = "imf_name")) %>%
        mutate(COUNTRY = coalesce(std_name, Country_raw)) %>%
        select(-std_name)
    } else {
      mutate(., COUNTRY = Country_raw)
    }
  } %>%
  {
    if (exists("keep_countries_23")) filter(., COUNTRY %in% keep_countries_23)
    else if (exists("keep_countries_std")) filter(., COUNTRY %in% keep_countries_std)
    else .
  } %>%
  select(COUNTRY, Year, Fin_Openness) %>%
  # 样本内只有 2009–2022：先保留并去重
  filter(Year >= 2009, Year <= 2022) %>%
  distinct()

# ---------- 按要求补全年份 ----------
# 2008 用 2009 的值；2023/2024 用 2022 的值
finopen_augmented <- finopen_annual %>%
  group_by(COUNTRY) %>%
  # 对每个国家生成完整 2008–2024 年
  tidyr::complete(Year = 2008:2024) %>%
  arrange(COUNTRY, Year) %>%
  # 填补 2008 / 2023 / 2024
  group_by(COUNTRY) %>%
  mutate(
    Fin_Openness = case_when(
      Year == 2008 ~ Fin_Openness[Year == 2009][1],
      Year %in% c(2023, 2024) ~ Fin_Openness[Year == 2022][1],
      TRUE ~ Fin_Openness
    )
  ) %>%
  ungroup()

# ---------- 年 -> 季：把年度值复制到当年四个季度 ----------
finopen_quarterly <- finopen_augmented %>%
  # 仅保留 2008–2024
  filter(Year >= 2008, Year <= 2024) %>%
  tidyr::expand_grid(Quarter = paste0("Q", 1:4)) %>%
  mutate(QUARTER = paste0(Year, "-", Quarter)) %>%
  select(COUNTRY, QUARTER, Financial_Openness = Fin_Openness) %>%
  filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4") %>%
  arrange(COUNTRY, QUARTER)

panel_final <- panel_final %>%
  full_join(finopen_quarterly, by = c("COUNTRY", "QUARTER")) %>%
  arrange(COUNTRY, QUARTER)



ge_raw <- read_csv("global_env_data.csv", show_col_types = FALSE)

# 自动识别季度列：匹配 quarter/time/period/date 等关键词
nm <- names(ge_raw)
qcol <- nm[str_detect(tolower(nm), "quarter|time|period|date")][1]
if (is.na(qcol)) stop("global_env_data.csv 未识别到季度/日期列")

# 将形如 "2008/10/1" 的日期解析为 Date，再转成 YYYY-Qn
ge <- ge_raw %>%
  mutate(
    .Qdate = as.Date(as.character(.data[[qcol]]), format = "%Y/%m/%d"),
    .Qdate = if_else(is.na(.Qdate),
                     suppressWarnings(as.Date(as.character(.data[[qcol]]), format = "%Y-%m-%d")),
                     .Qdate),
    .Qdate = if_else(is.na(.Qdate),
                     suppressWarnings(lubridate::ymd(as.character(.data[[qcol]]))),
                     .Qdate),
    QUARTER = paste0(year(.Qdate), "-Q", quarter(.Qdate))
  ) %>%
  select(-all_of(qcol), -.Qdate, everything()) %>%
  mutate(QUARTER = norm_quarter(QUARTER)) %>%
  # 只保留数值型全球变量 + QUARTER；同一季度多行则聚合为均值
  group_by(QUARTER) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(QUARTER) %>%
  filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4")

# —— 合并（按 QUARTER 左连接；全球变量将复制到所有国家当季） ——
panel_final <- panel_final %>%
  left_join(ge, by = "QUARTER") %>%
  arrange(COUNTRY, QUARTER)

# ======================= 新增：基于“GDP 原始水平”用 BBQ 识别衰退 =======================
# 思路：对每个国家，把 GDP（水平）先做轻度插补以消除缺口 → 取 log → 转为 ts(., freq = 4)
# 然后调用 BCDating::BBQ(mincycle=5, minphase=2)。结果对象的 @states == -1 为衰退。
# 注意：这里假定 GDP 为“实际、季调后”水平（最符合 BBQ 的经典定义）。
# 若部分国家 GDP 出现 <= 0（罕见），会平移为正再取对数（不改变转折点次序）。
bbq_flags <- panel_final %>%
  dplyr::select(COUNTRY, QUARTER, GDP) %>%
  group_by(COUNTRY) %>%
  arrange(QUARTER, .by_group = TRUE) %>%
  mutate(
    GDP_for_bbq = {
      x <- as.numeric(GDP)
      x <- zoo::na.approx(x, na.rm = FALSE)
      x <- zoo::na.locf(x, na.rm = FALSE)
      x <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)
      x
    }
  ) %>%
  group_modify(~{
    d   <- .x
    key <- .y$COUNTRY[[1]]  # 仅用于给 BBQ 命名，别放进返回列
    
    rec <- rep(NA_integer_, nrow(d))
    y   <- d$GDP_for_bbq
    
    if (sum(is.finite(y)) >= 8) {
      miny <- suppressWarnings(min(y, na.rm = TRUE))
      if (!is.finite(miny)) miny <- 0
      if (miny <= 0) y <- y - miny + 1e-6
      ylog <- log(y)
      
      qstr <- d$QUARTER[1]
      yr   <- as.integer(substr(qstr, 1, 4))
      qq   <- as.integer(sub(".*Q", "", qstr))
      
      y_ts <- ts(ylog, start = c(yr, qq), frequency = 4)
      
      bb <- try(BCDating::BBQ(y_ts, mincycle = 5, minphase = 2, name = key),
                silent = TRUE)
      
      if (!inherits(bb, "try-error")) {
        s <- bb@states               # -1 = 衰退, +1 = 扩张
        if (is.ts(s) && length(s) == nrow(d)) {
          rec <- ifelse(as.numeric(s) == -1, 1L, 0L)
        }
      }
    }
    
    # 关键：这里不要返回 COUNTRY，让 dplyr 自动加回去
    tibble(QUARTER = d$QUARTER, Recession_BBQ = rec)
  }) %>%
  ungroup()



# 把衰退 0/1 连接回总面板
panel_final <- panel_final %>%
  left_join(bbq_flags, by = c("COUNTRY","QUARTER")) %>%
  arrange(COUNTRY, QUARTER)



# --------- 1) 将以下指标替换为“环比变化率” ---------
# 变化率定义： (X_t / X_{t-1} - 1)，按 COUNTRY 分组逐期计算
vars_to_rate <- c(
  "Broad_Money","FDI_Inflows","FX_Reserves",
  "Credit_to_GDP_Gap","Property_Price","Credit_Total",
  "Capital_Inflow","GDP","Investment","Consumption","External_Balance"
)

vars_present <- intersect(vars_to_rate, names(panel_final))

panel_final <- panel_final %>%
  group_by(COUNTRY) %>%
  mutate(across(all_of(vars_present), ~ (.x / dplyr::lag(.x) - 1), .names = "{.col}")) %>%
  ungroup()

# --------- 2) Growth_Momentum：用“滞后1期GDP增长率” ---------
# 先得到 GDP 的增长率（已在上一步把 GDP 替换为环比增长率了）
panel_final <- panel_final %>%
  group_by(COUNTRY) %>%
  mutate(
    Growth_Momentum = dplyr::lag(GDP, 1)  # GDP 此时已是环比增长率
  ) %>%
  ungroup()

# --------- 3) Historical_Momentum：滞后2至4期GDP增长率的平均 ---------
panel_final <- panel_final %>%
  group_by(COUNTRY) %>%
  mutate(
    Historical_Momentum = rowMeans(cbind(lag(GDP, 2), lag(GDP, 3), lag(GDP, 4)), na.rm = TRUE)
  ) %>%
  ungroup()

# --------- 4) Real Interest Rate：Policy_Rate - CPI ---------
# CPI 已是通胀率（前面使用的是 period-to-period change），直接相减
stopifnot(all(c("Policy_Rate","CPI") %in% names(panel_final)))
panel_final <- panel_final %>%
  mutate(Real_Interest_Rate = Policy_Rate - CPI)

# --------- 5) Inflation Volatility：过去4个季度 CPI 的标准差 ---------
panel_final <- panel_final %>%
  group_by(COUNTRY) %>%
  arrange(QUARTER) %>%
  mutate(
    Inflation_Volatility = zoo::rollapply(
      CPI, 
      width = 4, 
      FUN = function(x) sd(x, na.rm = TRUE), 
      align = "right", 
      fill = NA
    )
  ) %>%
  ungroup()

# --------- 6) 仅保留 2009Q1–2024Q4；忽视因缺失导致的 NA ---------
panel_final_model <- panel_final %>%
  filter(QUARTER >= "2009-Q1", QUARTER <= "2024-Q4") %>%
  arrange(COUNTRY, QUARTER) %>%
  # 统一把数值列里的 NaN / Inf / -Inf 转成 NA
  mutate(across(where(is.numeric), \(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })) %>%
  # 如果文件里有文本形式的 "NaN"（少见），也一并转 NA（可留可删）
  mutate(across(where(is.character), ~ na_if(., "NaN")))

write_csv(panel_final_model, file = "Economic Growth.csv")
