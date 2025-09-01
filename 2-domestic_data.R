# Clear environment
rm(list = ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)
library(BCDating)

# ---------- General Utility Functions ----------
# Load any IMF wide table, keep country + all quarter columns, convert to long format, and standardize country names and quarter variables
load_imf_wide_to_long <- function(filepath, indicator_name,
                                  country_map_tbl,
                                  keep_countries_std) {
  df_raw <- read_csv(filepath, show_col_types = FALSE)
  
  # Identify quarter columns (e.g., 2008-Q4)
  quarter_cols <- names(df_raw) %>% str_subset("^\\d{4}-Q[1-4]$")
  
  df_long <- df_raw %>%
    # Keep country + quarter columns
    select(COUNTRY, all_of(quarter_cols)) %>%
    # Long format
    pivot_longer(cols = all_of(quarter_cols),
                 names_to = "QUARTER",
                 values_to = "value") %>%
    # Map IMF names to standard country names
    inner_join(country_map_tbl, by = c("COUNTRY" = "imf_name")) %>%
    mutate(COUNTRY = std_name) %>%
    select(-std_name) %>%
    # Keep only the target countries
    filter(COUNTRY %in% keep_countries_std) %>%
    # Add indicator name
    mutate(indicator = indicator_name)
  
  return(df_long)
}

# ---------- Country Mapping and Target Set ----------
# Exact country names in IMF files (including specified variations)
country_map_tbl <- tribble(
  ~imf_name,                         ~std_name,
  # Asia (excluding Taiwan)
  "China, People's Republic of",     "China",
  "India",                           "India",
  "Indonesia",                       "Indonesia",
  "Korea, Republic of",              "Korea",
  "Malaysia",                        "Malaysia",
  "Pakistan",                        "Pakistan",
  "Philippines",                     "Philippines",
  "Thailand",                        "Thailand",
  
  # Latin America
  "Brazil",                          "Brazil",
  "Chile",                           "Chile",
  "Colombia",                        "Colombia",
  "Mexico",                          "Mexico",
  "Peru",                            "Peru",
  
  # Europe, Middle East & Africa (EEMEA)
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

# Target countries (standard names; note that Taiwan is excluded)
keep_countries_std <- c(
  # Asia (9)
  "China","India","Indonesia","Korea","Malaysia","Pakistan","Philippines","Thailand",
  # LatAm (5)
  "Brazil","Chile","Colombia","Mexico","Peru",
  # EEMEA (9)
  "Czech Republic","Egypt","Greece","Hungary",
  "Poland","Qatar","Saudi Arabia","South Africa","Turkey","United Arab Emirates"
) %>% unique()

# For safety, only keep the countries found in our mapping table
keep_countries_std <- keep_countries_std[keep_countries_std %in% country_map_tbl$std_name]

# ---------- Read and Standardize Indicators ----------
cpi_long <- load_imf_wide_to_long("CPI.csv",               "CPI",               country_map_tbl, keep_countries_std)
ppi_long <- load_imf_wide_to_long("PPI.csv",               "PPI",               country_map_tbl, keep_countries_std)
fx_long  <- load_imf_wide_to_long("foreign reserve.csv",   "FX_Reserves",       country_map_tbl, keep_countries_std)
fdi_long <- load_imf_wide_to_long("FDI.csv",               "FDI_Inflows",       country_map_tbl, keep_countries_std)
bm_long  <- load_imf_wide_to_long("broad money.csv",       "Broad_Money",       country_map_tbl, keep_countries_std)
GDP_long <- load_imf_wide_to_long("GDP.csv",               "GDP",               country_map_tbl, keep_countries_std)
Consumption_long <- load_imf_wide_to_long("Consumption.csv", "Consumption",     country_map_tbl, keep_countries_std)
Investment_long <- load_imf_wide_to_long("Investment.csv",   "Investment",       country_map_tbl, keep_countries_std)
CA_long <- load_imf_wide_to_long("external balance.csv",    "External_Balance",  country_map_tbl, keep_countries_std)

# Merge into a long table (Country × Quarter × Indicator × Value)
all_long <- bind_rows(cpi_long, ppi_long, fx_long, fdi_long, bm_long, GDP_long, Consumption_long, Investment_long, CA_long)

# ---------- Pivot to a wide table (typical panel data: one row = Country × Quarter; columns = indicators) ----------
panel_imf <- all_long %>%
  mutate(
    # Standardize quarter sorting key (YYYY-Qn)
    year = as.integer(str_sub(QUARTER, 1, 4)),
    qtr  = as.integer(str_sub(QUARTER, 7, 7)),
    # Construct a sortable key
    sort_key = year * 10 + qtr
  ) %>%
  arrange(COUNTRY, sort_key, indicator) %>%
  select(COUNTRY, QUARTER, indicator, value) %>%
  # Wide format
  pivot_wider(names_from = indicator, values_from = value) %>%
  # Optional: Limit sample period to 2008-Q4 to 2024-Q4 (to avoid extra columns from raw files)
  filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4") %>%
  arrange(COUNTRY, QUARTER)




# ----------- Country Mapping Table (consistent with the above) -----------
bis_to_std <- tribble(
  ~bis_name,               ~std_name,
  "CN:China",              "China",
  "IN:India",              "India",
  "ID:Indonesia",          "Indonesia",
  "KR:Korea",              "Korea",
  "MY:Malaysia",           "Malaysia",
  "PK:Pakistan",           "Pakistan",
  "PH:Philippines",        "Philippines",
  "TH:Thailand",           "Thailand",
  "BR:Brazil",             "Brazil",
  "CL:Chile",              "Chile",
  "CO:Colombia",           "Colombia",
  "MX:Mexico",             "Mexico",
  "PE:Peru",               "Peru",
  "CZ:Czechia",            "Czech Republic",
  "EG:Egypt",              "Egypt",
  "GR:Greece",             "Greece",
  "HU:Hungary",            "Hungary",
  "PL:Poland",             "Poland",
  "QA:Qatar",              "Qatar",
  "SA:Saudi Arabia",       "Saudi Arabia",
  "ZA:South Africa",       "South Africa",
  "TR:Türkiye",            "Turkey"
)

keep_countries_23 <- bis_to_std$std_name

# ----------- General Processing Function -----------
process_bis_file <- function(filepath, indicator_name){
  raw <- read_excel(filepath, sheet = 1) # Default to the first sheet
  
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

# ----------- Read Files Separately -----------
credit_gap   <- process_bis_file("credit to gdp gap.xlsx",                 "Credit_to_GDP_Gap")
property     <- process_bis_file("property prices.xlsx",                   "Property_Price")
credit_total <- process_bis_file("Credit to the non-financial sector.xlsx","Credit_Total")
capital_in   <- process_bis_file("capital inflow.xlsx",                    "Capital_Inflow")
exchange_rate<- process_bis_file("exchaneg rate.xlsx",                     "Exchange_Rate")
# ----------- Merge into one panel wide table -----------
panel_bis <- credit_gap %>%
  full_join(property,     by = c("COUNTRY","QUARTER")) %>%
  full_join(credit_total, by = c("COUNTRY","QUARTER")) %>%
  full_join(capital_in,   by = c("COUNTRY","QUARTER")) %>%
  full_join(exchange_rate,  by = c("COUNTRY","QUARTER")) %>%
  arrange(COUNTRY, QUARTER)


# Quick check
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


# ---------- Target Countries (23, excluding Taiwan) ----------
keep_countries_23 <- c(
  # Asia (8)
  "China","India","Indonesia","Korea","Malaysia","Pakistan","Philippines","Thailand",
  # LatAm (5)
  "Brazil","Chile","Colombia","Mexico","Peru",
  # EEMEA (10)
  "Czech Republic","Egypt","Greece","Hungary","Poland",
  "Qatar","Saudi Arabia","South Africa","Turkey"
)

# ---------- Read ILO Data ----------
unemp_raw <- read_excel("unemployment.xlsx", sheet = 1)

# ---------- Standardize Country Names ----------
# Note: ILO uses "Czechia" instead of "Czech Republic"
# It might also use "Republic of Korea" instead of "Korea"
# So a mapping is needed
ilo_to_std <- tribble(
  ~ilo_name,                 ~std_name,
  "China",                   "China",
  "India",                   "India",
  "Indonesia",               "Indonesia",
  "Republic of Korea",       "Korea",
  "Malaysia",                "Malaysia",
  "Pakistan",                "Pakistan",
  "Philippines",             "Philippines",
  "Thailand",                "Thailand",
  "Brazil",                  "Brazil",
  "Chile",                   "Chile",
  "Colombia",                "Colombia",
  "Mexico",                  "Mexico",
  "Peru",                    "Peru",
  "Czechia",                 "Czech Republic",
  "Egypt",                   "Egypt",
  "Greece",                  "Greece",
  "Hungary",                 "Hungary",
  "Poland",                  "Poland",
  "Qatar",                   "Qatar",
  "Saudi Arabia",            "Saudi Arabia",
  "South Africa",            "South Africa",
  "Turkey",                  "Turkey",
  "United Arab Emirates",    "United Arab Emirates"
)

# ---------- Data Processing ----------
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
  x <- str_replace_all(x, "\\s+", "")      # remove spaces
  x <- str_replace_all(x, "[–—]", "-")     # unify dashes to "-"
  x <- str_replace_all(x, "^(\\d{4})[- ]?Q0?([1-4])$", "\\1-Q\\2") # 2008Q01/2008-Q01/2008 Q1 -> 2008-Q1
  x <- str_replace_all(x, "^(\\d{4})Q([1-4])$", "\\1-Q\\2")       # 2008Q1 -> 2008-Q1
  x
}
unemp_q <- unemp_q %>%
  mutate(QUARTER = norm_quarter(QUARTER))


panel_final <- panel_final %>%
  full_join(unemp_q, by = c("COUNTRY", "QUARTER")) %>%
  arrange(COUNTRY, QUARTER)



finopen_raw <- read_excel("financial openness.xlsx", sheet = 1)
names(finopen_raw) <- names(finopen_raw) %>% tolower() %>% str_replace_all("\\s+", "_")

# Robustly identify column names (can be manually changed to your exact column names if necessary)
country_col <- names(finopen_raw)[str_detect(names(finopen_raw), "country|econom|nation")]
year_col    <- names(finopen_raw)[str_detect(names(finopen_raw), "^year$|fiscal_year|time|date")]
# Take the numeric column as the index column (excluding the year column)
value_col   <- setdiff(names(finopen_raw), c(country_col, year_col))
value_col   <- value_col[map_lgl(finopen_raw[value_col], is.numeric)][1]

finopen_annual <- finopen_raw %>%
  transmute(
    Country_raw = .data[[country_col]],
    Year        = as.integer(.data[[year_col]]),
    Fin_Openness = as.numeric(.data[[value_col]])
  ) %>%
  # Standardize to your country names; reuse mappings defined above (prefer ilo_to_std if both exist)
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
  # Sample only contains 2009–2022: keep and remove duplicates first
  filter(Year >= 2009, Year <= 2022) %>%
  distinct()

# ---------- Augment Years as Required ----------
# Use 2009's value for 2008; use 2022's value for 2023/2024
finopen_augmented <- finopen_annual %>%
  group_by(COUNTRY) %>%
  # For each country, generate a complete 2008–2024 year range
  tidyr::complete(Year = 2008:2024) %>%
  arrange(COUNTRY, Year) %>%
  # Fill in 2008 / 2023 / 2024
  group_by(COUNTRY) %>%
  mutate(
    Fin_Openness = case_when(
      Year == 2008 ~ Fin_Openness[Year == 2009][1],
      Year %in% c(2023, 2024) ~ Fin_Openness[Year == 2022][1],
      TRUE ~ Fin_Openness
    )
  ) %>%
  ungroup()

# ---------- Year -> Quarter: Copy annual value to all four quarters of the year ----------
finopen_quarterly <- finopen_augmented %>%
  # Keep only 2008–2024
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

# Automatically identify the quarter column: match keywords like quarter/time/period/date
nm <- names(ge_raw)
qcol <- nm[str_detect(tolower(nm), "quarter|time|period|date")][1]
if (is.na(qcol)) stop("Quarter/date column not found in global_env_data.csv")

# Parse date like "2008/10/1" into Date format, then convert to YYYY-Qn
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
  # Keep only numeric global variables + QUARTER; aggregate multiple rows in the same quarter by mean
  group_by(QUARTER) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(QUARTER) %>%
  filter(QUARTER >= "2008-Q4", QUARTER <= "2024-Q4")

# -- Merge (left join by QUARTER; global variables will be copied to all countries for that quarter) --
panel_final <- panel_final %>%
  left_join(ge, by = "QUARTER") %>%
  arrange(COUNTRY, QUARTER)

# ======================= NEW: Identify Recessions using BBQ on "Original GDP Level" =======================
# Approach: For each country, lightly interpolate GDP (level) to fill gaps -> take log -> convert to ts(., freq = 4)
# Then call BCDating::BBQ(mincycle=5, minphase=2). @states == -1 in the result object indicates a recession.
# Note: Assumes GDP is "real, seasonally adjusted" level (which best fits the classic definition for BBQ).
# If any country's GDP is <= 0 (rare), it will be shifted to be positive before taking the log (this does not change the turning point order).
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
    key <- .y$COUNTRY[[1]]  # Only used for naming in BBQ, do not include in the returned columns
    
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
        s <- bb@states             # -1 = recession, +1 = expansion
        if (is.ts(s) && length(s) == nrow(d)) {
          rec <- ifelse(as.numeric(s) == -1, 1L, 0L)
        }
      }
    }
    
    # Key: Do not return COUNTRY here, let dplyr add it back automatically
    tibble(QUARTER = d$QUARTER, Recession_BBQ = rec)
  }) %>%
  ungroup()



# Join the recession 0/1 flags back to the main panel
panel_final <- panel_final %>%
  left_join(bbq_flags, by = c("COUNTRY","QUARTER")) %>%
  arrange(COUNTRY, QUARTER)



# --------- 1) Replace the following indicators with "Quarter-over-Quarter Change Rate" ---------
# Rate of change definition: (X_t / X_{t-1} - 1), calculated period-by-period grouped by COUNTRY
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

# --------- 2) Growth_Momentum: Use "lagged 1-quarter GDP growth rate" ---------
# First get the GDP growth rate (GDP has already been replaced by QoQ growth rate in the previous step)
panel_final <- panel_final %>%
  group_by(COUNTRY) %>%
  mutate(
    Growth_Momentum = dplyr::lag(GDP, 1)  # GDP is already the quarter-over-quarter growth rate at this point
  ) %>%
  ungroup()

# --------- 3) Historical_Momentum: Average of lagged 2 to 4-quarter GDP growth rates ---------
panel_final <- panel_final %>%
  group_by(COUNTRY) %>%
  mutate(
    Historical_Momentum = rowMeans(cbind(lag(GDP, 2), lag(GDP, 3), lag(GDP, 4)), na.rm = TRUE)
  ) %>%
  ungroup()

# --------- 4) Real Interest Rate: Policy_Rate - CPI ---------
# CPI is already an inflation rate (period-to-period change was used earlier), so subtract directly
stopifnot(all(c("Policy_Rate","CPI") %in% names(panel_final)))
panel_final <- panel_final %>%
  mutate(Real_Interest_Rate = Policy_Rate - CPI)

# --------- 5) Inflation Volatility: Standard deviation of CPI over the past 4 quarters ---------
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

# --------- 6) Keep only 2009Q1–2024Q4; ignore NAs due to missing values ---------
panel_final_model <- panel_final %>%
  filter(QUARTER >= "2009-Q1", QUARTER <= "2024-Q4") %>%
  arrange(COUNTRY, QUARTER) %>%
  # Standardize NaN / Inf / -Inf in numeric columns to NA
  mutate(across(where(is.numeric), \(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })) %>%
  # If there are text "NaN"s in the file (rare), convert them to NA as well (optional)
  mutate(across(where(is.character), ~ na_if(., "NaN")))

write_csv(panel_final_model, file = "Economic Growth.csv")

