rm(list = ls())
library(tidyverse)
library(geepack)
library(PGEE)
library(kableExtra)
library(pROC)
library(zoo)
library(R.utils)
library(dplyr)
library(tidyr)
library(purrr)
library(kableExtra)
library(officer)
library(flextable)
set.seed(123) 

df <- readr::read_csv("Economic Growth.csv") %>%
  mutate(
    year = as.integer(stringr::str_extract(QUARTER, "^\\d{4}")),
    q    = as.integer(stringr::str_extract(QUARTER, "(?<=Q)\\d")),
    tnum = year*4 + q
  ) 
num_vars <- df %>% dplyr::select(-COUNTRY, -QUARTER) %>% names()

# 描述性统计
desc_tbl <- df %>%
  mutate(across(all_of(num_vars), as.numeric)) %>%
  summarise(across(
    all_of(num_vars),
    list(
      max  = ~ max(., na.rm = TRUE),
      min  = ~ min(., na.rm = TRUE),
      mean = ~ mean(., na.rm = TRUE),
      sd   = ~ sd(.,   na.rm = TRUE)
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(),
               names_to = c("Variable","stat"),
               names_sep = "__",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  relocate(Variable, max, min, mean, sd) %>%
  arrange(Variable) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

desc_tbl %>%
  kbl(booktabs = TRUE, format = "latex",
      caption = "Descriptive statistics for all numeric variables",
      col.names = c("Variable","Max","Min","Mean","SD"),
      align = c("l","r","r","r","r")) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  writeLines(con = "descriptive_stats_table.tex")

# 相关性矩阵
cor_mat <- df %>%
  dplyr::select(all_of(num_vars)) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(use = "pairwise.complete.obs")

cor_long <- as.data.frame(cor_mat) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr")

p <- ggplot(cor_long, aes(x = var2, y = var1, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), name = "Correlation") +
  coord_fixed() +
  labs(x = NULL, y = NULL,
       title = "Correlation Heatmap") +
  theme_minimal(base_size = 10) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave("correlation_heatmap.png", p, width = 16, height = 16, dpi = 200)



# —— 因变量：GDP（已是q/q增长率；若是水平请改为 diff(log(GDP)) —— #
g <- df$GDP %>% as.numeric() %>% discard(is.na)

# 分布统计
n  <- sum(!is.na(g))
mu <- mean(g, na.rm = TRUE)
sdv<- sd(g, na.rm = TRUE)
sk <- moments::skewness(g, na.rm = TRUE)
ek <- moments::kurtosis(g, na.rm = TRUE) - 3
JB <- n/6 * (sk^2 + (ek^2)/4)

tibble(n=n, mean=mu, sd=sdv, skew=sk, excess_kurtosis=ek, JB=JB) %>% print()

# 直方图（白底）+ 正态对比
p1 <- ggplot(tibble(g=g), aes(x=g)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="grey90") +
  stat_function(fun = dnorm, args = list(mean = mu, sd = sdv), linewidth = 0.8) +
  labs(title = "GDP growth (q/q) 2010–2019, EMEs (pooled)",
       x = "Growth", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill="white", colour=NA),
        plot.background  = element_rect(fill="white", colour=NA))
ggsave("gdp_growth_hist.png", p1, width = 7, height = 5, dpi = 200)

# 国别内标准化后再看一次
df2 <- df %>%
  group_by(COUNTRY) %>%
  mutate(GDP_z = (GDP - mean(GDP, na.rm=TRUE)) / sd(GDP, na.rm=TRUE)) %>%
  ungroup()
gz <- df2$GDP_z %>% as.numeric() %>% discard(is.na)
mu_z <- mean(gz); sd_z <- sd(gz)
sk_z <- moments::skewness(gz); ek_z <- moments::kurtosis(gz) - 3
JB_z <- length(gz)/6 * (sk_z^2 + (ek_z^2)/4)

tibble(n=length(gz), mean=mu_z, sd=sd_z, skew=sk_z, excess_kurtosis=ek_z, JB=JB_z) %>% print()

p2 <- ggplot(tibble(gz=gz), aes(x=gz)) +
  geom_histogram(aes(y=..density..), bins=40, color="black", fill="grey90") +
  stat_function(fun = dnorm, args = list(mean = mu_z, sd = sd_z), linewidth = 0.8) +
  labs(title = "Within-country standardized GDP growth (q/q)",
       x = "Standardized growth (z)", y = "Density") +
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill="white", colour=NA),
        plot.background  = element_rect(fill="white", colour=NA))
ggsave("gdp_growth_within_hist.png", p2, width = 7, height = 5, dpi = 200)




# 1) 读入 & 基础字段
y_var <- "GDP"
x_vars   <- setdiff(num_vars, c(y_var, "year","q","tnum"))

# 2) 只对 X 做国家内时序插补（不插补 Y）
impute_series <- function(x) {
  y <- na.approx(x, na.rm = FALSE)
  y <- na.locf(y, na.rm = FALSE)
  y <- na.locf(y, fromLast = TRUE, na.rm = FALSE)
  if (anyNA(y)) {
    med <- suppressWarnings(median(x, na.rm = TRUE)); if (!is.finite(med)) med <- 0
    y[is.na(y)] <- med
  }
  y
}

# 安全标准化：sd==0 时仅中心化
scale_safe <- function(x){
  mu <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(x - mu)
  as.numeric((x - mu)/s)
}

dat <- df %>%
  arrange(COUNTRY, tnum) %>%
  group_by(COUNTRY) %>%
  mutate(across(all_of(x_vars), impute_series)) %>%           # 插补 X
  mutate(across(all_of(x_vars), scale_safe)) %>%              # 安全标准化 X
  ungroup()

# 3) 诊断 & 清理导致秩亏的列
X0 <- dat %>% dplyr::select(all_of(x_vars)) %>% mutate(across(everything(), as.numeric))

# 3.1 零/近零方差列
sd_all <- sapply(X0, function(z) sd(z, na.rm = TRUE))
drop_nzv <- names(sd_all)[!is.finite(sd_all) | sd_all < 1e-10]

# 3.2 完全重复列（1:1 相同）
dup_map <- function(M){
  cn <- colnames(M); keep <- rep(TRUE, ncol(M))
  seen <- list()
  for (j in seq_along(cn)) {
    if (!keep[j]) next
    key <- paste0(round(M[, j], 12), collapse = "|")  # 近似键
    if (key %in% names(seen)) { keep[j] <- FALSE } else { seen[[key]] <- TRUE }
  }
  cn[!keep]
}
drop_dup <- character(0)
# 为避免内存，分块比对；简单起见直接一次性做（样本量允许）
drop_dup <- dup_map(as.matrix(X0))

# 3.3 超高相关（|corr|>0.999）：保留列名排序靠前者
keep_corr <- function(M, thr = 0.999){
  C <- suppressWarnings(cor(M, use = "pairwise.complete.obs"))
  p <- ncol(M); keep <- rep(TRUE, p)
  for (i in 1:(p-1)) if (keep[i]) {
    hits <- which(abs(C[i, (i+1):p]) > thr) + i
    keep[hits] <- FALSE
  }
  colnames(M)[keep]
}
X1 <- X0 %>% dplyr::select(-any_of(unique(c(drop_nzv, drop_dup))))
x_corr_keep <- keep_corr(as.matrix(X1), thr = 0.999)
drop_corr <- setdiff(colnames(X1), x_corr_keep)

# 3.4 用 QR 取满秩子集（最终保险）
X2 <- X1 %>% dplyr::select(all_of(x_corr_keep))
mm <- model.matrix(~ 0 + ., data = X2)  # 不含截距，纯自变量矩阵
qrX <- qr(mm)
idx <- qrX$pivot[seq_len(qrX$rank)]
x_fullrank <- colnames(mm)[idx]

# 汇总被删除的列（供检查/记录）
dropped <- tibble(
  reason = c(rep("nearZeroVar", length(drop_nzv)),
             rep("duplicate", length(drop_dup)),
             rep("highCorr",  length(drop_corr))),
  var = c(drop_nzv, drop_dup, drop_corr)
) %>% distinct()

print(dropped)

# 4) 以满秩子集拟合 GEE
x_use <- make.names(gsub("^`|`$", "", gsub("`", "", x_fullrank)))
form <- as.formula(paste(y_var, "~", paste(x_use, collapse = " + ")))

dat_fit <- dat %>%
  dplyr::select(COUNTRY, QUARTER, GDP, all_of(x_fullrank)) %>%
  filter(!is.na(GDP)) %>%
  mutate(COUNTRY = factor(COUNTRY))  # 将COUNTRY转换为因子

fit_ar1  <- geeglm(form, id = COUNTRY, data = dat_fit,
                   family = gaussian("identity"), corstr = "ar1",
                   na.action = na.exclude)

fit_ind  <- geeglm(form, id = COUNTRY, data = dat_fit,
                   family = gaussian("identity"), corstr = "independence",
                   na.action = na.exclude)

fit_exch <- geeglm(form, id = COUNTRY, data = dat_fit,
                   family = gaussian("identity"), corstr = "exchangeable",
                   na.action = na.exclude)


# ========= 优化版：连续变量PGEE矩阵接口CV =========
cat("=== 开始优化版PGEE交叉验证 ===\n")
total_start <- Sys.time()

# ==================== 数据预处理：一次性构建所有矩阵 ====================
cat("预计算设计矩阵和索引...\n")
prep_start <- Sys.time()

# 0) 构造数据框（与原版保持一致）
dat_cv <- dat_fit %>%
  transmute(
    id = COUNTRY,
    y  = GDP,
    !!!dplyr::select(., all_of(x_fullrank))
  ) %>%
  as.data.frame()

form_cv <- y ~ . - id

# 1. 一次性构建完整设计矩阵（包含截距）
X_full <- model.matrix(form_cv, data = dat_cv)
y_full <- dat_cv$y
id_full <- dat_cv$id

# 2. 确定惩罚索引（截距不惩罚）
pindex <- as.integer(colnames(X_full) != "(Intercept)")

# 3. 优化lambda网格（减少搜索点数）
lambda_grid <- seq(0.01, 0.3, by = 0.01) 
cat("Lambda网格大小:", length(lambda_grid), "个点\n")

# 4. 预计算交叉验证折分（按行索引）
K <- 5
ids <- unique(id_full)
fold_lab <- sample(rep(1:K, length.out = length(ids)))
id2fold <- setNames(fold_lab, ids)

# 预计算每折的行索引
fold_row_indices <- lapply(1:K, function(k) {
  ids_valid <- names(id2fold)[id2fold == k]
  valid_rows <- which(id_full %in% ids_valid)
  train_rows <- which(!id_full %in% ids_valid)
  list(train_rows = train_rows, valid_rows = valid_rows)
})

cat("预处理用时:", round(difftime(Sys.time(), prep_start, units = "secs"), 2), "秒\n")

# ==================== 优化的PGEE拟合函数 ====================
fit_pgee_matrix <- function(X, y, id, lambda, corstr) {
  # 将矩阵转换为数据框，保持原始变量名
  X_df <- as.data.frame(X[, -1, drop = FALSE])  # 去掉截距列
  temp_df <- data.frame(y = y, id = id, X_df)
  
  # 获取原始变量名（去掉截距）
  var_names <- colnames(X)[-1]
  colnames(temp_df)[-(1:2)] <- var_names
  
  # 构建公式
  if (length(var_names) > 0) {
    temp_formula <- as.formula(paste("y ~", paste(var_names, collapse = " + ")))
  } else {
    temp_formula <- as.formula("y ~ 1")
  }
  
  # 调整pindex（对应去掉截距后的变量）
  pindex_adj <- pindex[-1]
  
  PGEE::PGEE(
    formula = temp_formula,
    id = id,
    data = temp_df,
    family = gaussian("identity"),
    corstr = corstr,
    lambda = lambda,
    pindex = pindex_adj,
    scale.fix = FALSE,
    eps = 1e-4,
    maxiter = 30,
    tol = 1e-2,
    silent = TRUE
  )
}

# 快速预测函数：直接矩阵乘法
pred_fast <- function(X, coef_vec) {
  X_aligned <- X[, names(coef_vec), drop = FALSE]
  as.numeric(X_aligned %*% coef_vec)
}

# 评估指标函数
mse_fast <- function(y, yhat) mean((y - yhat)^2, na.rm = TRUE)

# ==================== 矩阵接口的交叉验证 ====================
cv_pgee_matrix <- function(corstr) {
  cat(paste("开始", corstr, "相关结构的交叉验证...\n"))
  
  results <- vector("list", length(lambda_grid))
  
  for (lam_idx in seq_along(lambda_grid)) {
    lam <- lambda_grid[lam_idx]
    if (lam_idx %% 5 == 1) cat(paste("  进度:", lam_idx, "/", length(lambda_grid), "lambda =", lam, "\n"))
    
    mse_folds <- numeric(K)
    success_count <- 0
    
    for (k in 1:K) {
      # 直接使用预计算的行索引进行矩阵切分
      train_rows <- fold_row_indices[[k]]$train_rows
      valid_rows <- fold_row_indices[[k]]$valid_rows
      
      X_train <- X_full[train_rows, , drop = FALSE]
      y_train <- y_full[train_rows]
      id_train <- id_full[train_rows]
      
      X_valid <- X_full[valid_rows, , drop = FALSE]
      y_valid <- y_full[valid_rows]
      
      # 检查数据质量
      if (length(unique(id_train)) < 3 || length(y_train) < 10) {
        mse_folds[k] <- NA_real_
        next
      }
      
      # 拟合模型
      fit_k <- try({
        fit_pgee_matrix(X_train, y_train, id_train, lam, corstr)
      }, silent = TRUE)
      
      if (!inherits(fit_k, "try-error")) {
        coef_k <- try(coef(fit_k), silent = TRUE)
        
        if (!inherits(coef_k, "try-error") && all(is.finite(coef_k))) {
          # 快速预测：直接矩阵乘法
          pred_k <- try(pred_fast(X_valid, coef_k), silent = TRUE)
          
          if (!inherits(pred_k, "try-error") && all(is.finite(pred_k))) {
            mse_folds[k] <- mse_fast(y_valid, pred_k)
            success_count <- success_count + 1
          } else {
            mse_folds[k] <- NA_real_
          }
        } else {
          mse_folds[k] <- NA_real_
        }
      } else {
        mse_folds[k] <- NA_real_
      }
    }
    
    if (success_count >= 3) {  # 至少3折成功
      results[[lam_idx]] <- list(
        lambda = lam,
        mse = mean(mse_folds, na.rm = TRUE),
        n_success = success_count
      )
    } else {
      results[[lam_idx]] <- list(
        lambda = lam,
        mse = NA_real_,
        n_success = success_count
      )
    }
  }
  
  # 转换为tibble并筛选有效结果
  res_df <- map_dfr(results, ~ as_tibble(.x)) %>%
    filter(is.finite(mse) & n_success >= 3)
  
  if (nrow(res_df) == 0) {
    cat("矩阵方法失败，回退到原始方法...\n")
    return(fallback_cv_continuous(corstr))
  }
  
  lam_opt <- res_df$lambda[which.min(res_df$mse)]
  cat(paste("  最优lambda:", lam_opt, "MSE:", round(min(res_df$mse), 6), "\n"))
  
  list(grid = res_df, lam_opt = lam_opt)
}

# 回退方法：使用原始approach
fallback_cv_continuous <- function(corstr) {
  cat("使用回退方法进行交叉验证...\n")
  
  results <- map_dfr(lambda_grid, function(lam) {
    mse_k <- numeric(K)
    
    for (k in 1:K) {
      ids_valid <- names(id2fold)[id2fold == k]
      valid_idx <- dat_cv$id %in% ids_valid
      train_df <- dat_cv[!valid_idx, , drop = FALSE]
      valid_df <- dat_cv[valid_idx, , drop = FALSE]
      
      if (nrow(train_df) < 10 || nrow(valid_df) < 3) {
        mse_k[k] <- NA_real_
        next
      }
      
      fit_k <- try(PGEE::PGEE(
        formula = form_cv, id = id, data = train_df,
        family = gaussian("identity"), corstr = corstr,
        lambda = lam, pindex = 1,
        scale.fix = FALSE, eps = 1e-4, maxiter = 30, tol = 1e-2, silent = TRUE
      ), silent = TRUE)
      
      if (!inherits(fit_k, "try-error")) {
        beta <- try(coef(fit_k), silent = TRUE)
        if (!inherits(beta, "try-error") && all(is.finite(beta))) {
          MM <- model.matrix(form_cv, data = valid_df)
          MM <- MM[, names(beta), drop = FALSE]
          yhat <- try(as.numeric(MM %*% beta), silent = TRUE)
          
          if (!inherits(yhat, "try-error") && all(is.finite(yhat))) {
            mse_k[k] <- mean((valid_df$y - yhat)^2)
          } else {
            mse_k[k] <- NA_real_
          }
        } else {
          mse_k[k] <- NA_real_
        }
      } else {
        mse_k[k] <- NA_real_
      }
    }
    
    tibble(lambda = lam, mse = mean(mse_k, na.rm = TRUE))
  }) %>% filter(is.finite(mse))
  
  if (nrow(results) == 0) {
    stop("所有方法都失败，请检查数据和参数设置")
  }
  
  lam_opt <- results$lambda[which.min(results$mse)]
  list(grid = results, lam_opt = lam_opt)
}

# ==================== 执行交叉验证 ====================
cat("开始交叉验证参数选择...\n")
cv_start <- Sys.time()

# 分别对三种相关结构做CV
cv_ind  <- cv_pgee_matrix("independence")
cv_exch <- cv_pgee_matrix("exchangeable")
cv_ar1  <- cv_pgee_matrix("AR-1")

cat("交叉验证总用时:", round(difftime(Sys.time(), cv_start, units = "secs"), 2), "秒\n")

# 输出最优参数
message(sprintf("[independence]  最优 lambda = %.3f (CV-MSE=%.6f)",
                cv_ind$lam_opt, min(cv_ind$grid$mse)))
message(sprintf("[exchangeable]  最优 lambda = %.3f (CV-MSE=%.6f)",
                cv_exch$lam_opt, min(cv_exch$grid$mse)))
message(sprintf("[AR-1]          最优 lambda = %.3f (CV-MSE=%.6f)",
                cv_ar1$lam_opt, min(cv_ar1$grid$mse)))

# ==================== 拟合最终模型 ====================
cat("拟合最终模型...\n")
fit_start <- Sys.time()

# 用各自的最优lambda在全样本上拟合三种最终PGEE模型
fit_pgee_ind  <- fit_pgee_matrix(X_full, y_full, id_full, cv_ind$lam_opt, "independence")
fit_pgee_exch <- fit_pgee_matrix(X_full, y_full, id_full, cv_exch$lam_opt, "exchangeable")
fit_pgee_ar1  <- fit_pgee_matrix(X_full, y_full, id_full, cv_ar1$lam_opt, "AR-1")

cat("最终模型拟合用时:", round(difftime(Sys.time(), fit_start, units = "secs"), 2), "秒\n")

# ==================== 模型摘要 ====================
cat("\n=== 模型摘要 ===\n")
print(summary(fit_pgee_ind))
print(summary(fit_pgee_exch))
print(summary(fit_pgee_ar1))

# 非零系数
nz_names <- function(fit) {
  cf <- coef(fit)
  names(cf)[abs(cf) > 1e-8]  # 考虑数值精度
}

cat("\n[PGEE-independence] 非零系数：\n");  print(nz_names(fit_pgee_ind))
cat("\n[PGEE-exchangeable] 非零系数：\n");   print(nz_names(fit_pgee_exch))
cat("\n[PGEE-AR(1)] 非零系数：\n");        print(nz_names(fit_pgee_ar1))

# ==================== 系数比较表 ====================
cat("\n=== 系数比较 ===\n")

# 助手函数：提取系数并包装为 tibble
coef_tbl <- function(fit, model_label) {
  cf <- coef(fit)
  tibble(term = names(cf), !!model_label := as.numeric(cf))
}

# 各模型系数（假设原始GEE模型已存在）
if (exists("fit_ar1") && exists("fit_ind") && exists("fit_exch")) {
  tb_gee_ar1  <- coef_tbl(fit_ar1,        "GEE-AR(1)")
  tb_gee_ind  <- coef_tbl(fit_ind,        "GEE-独立")
  tb_gee_exch <- coef_tbl(fit_exch,       "GEE-可交换")
  
  tb_p_ar1    <- coef_tbl(fit_pgee_ar1,   "PGEE-AR(1)")
  tb_p_ind    <- coef_tbl(fit_pgee_ind,   "PGEE-独立")
  tb_p_exch   <- coef_tbl(fit_pgee_exch,  "PGEE-可交换")
  
  # 合并系数表
  coef_all <- list(tb_gee_ar1, tb_gee_ind, tb_gee_exch,
                   tb_p_ar1,   tb_p_ind,   tb_p_exch) %>%
    reduce(full_join, by = "term")
  
  # 友好的数值格式化
  fmt_num <- function(x, digits = 4, sci_cut = 1e-3) {
    ifelse(is.finite(x) & abs(x) < sci_cut,
           formatC(x, format = "e", digits = 1),
           formatC(x, format = "f", digits = digits))
  }
  
  # 变量排序
  x_order <- colnames(dat_cv) %>% setdiff(c("id","y"))
  
  pretty_tbl <- coef_all %>%
    mutate(term = if_else(term == "(Intercept)", "截距", term)) %>%
    mutate(term = factor(term, levels = c("截距", x_order))) %>%
    arrange(term) %>%
    mutate(across(-term, ~ fmt_num(.x, digits = 4, sci_cut = 1e-3))) %>%
    rename(变量 = term)
  
  print(pretty_tbl, n = nrow(pretty_tbl))
}

# ==================== 预测准确性评估 ====================
cat("\n=== 预测准确性评估 ===\n")
eval_start <- Sys.time()

# 评估函数
eps_mape <- 1e-6
metrics <- function(y, yhat, eps = eps_mape) {
  tibble(
    Criterion = c("MSE", "MAE", "MAPE"),
    Value = c(
      mean((y - yhat)^2, na.rm = TRUE),
      mean(abs(y - yhat), na.rm = TRUE),
      100 * mean(abs((y - yhat) / pmax(abs(y), eps)), na.rm = TRUE)
    )
  )
}

# GEE预测（假设原始模型存在）
if (exists("fit_ar1") && exists("fit_ind") && exists("fit_exch")) {
  y_gee <- dat_fit$GDP
  pred_gee <- list(
    "GEE - AR(1)"    = as.numeric(predict(fit_ar1,  newdata = dat_fit, type = "response")),
    "GEE - Indep."   = as.numeric(predict(fit_ind,  newdata = dat_fit, type = "response")),
    "GEE - Exch."    = as.numeric(predict(fit_exch, newdata = dat_fit, type = "response"))
  )
} else {
  pred_gee <- list()
  y_gee <- numeric(0)
}

# PGEE预测：快速矩阵乘法
y_pgee <- dat_cv$y
pred_p <- list(
  "PGEE - AR(1)"   = pred_fast(X_full, coef(fit_pgee_ar1)),
  "PGEE - Indep."  = pred_fast(X_full, coef(fit_pgee_ind)),
  "PGEE - Exch."   = pred_fast(X_full, coef(fit_pgee_exch))
)

# 计算评估指标
if (length(pred_gee) > 0) {
  res_gee <- imap(pred_gee, ~ metrics(y_gee,  .x) %>% mutate(Method = .y))
} else {
  res_gee <- list()
}

res_p <- imap(pred_p, ~ metrics(y_pgee, .x) %>% mutate(Method = .y))

res_long <- bind_rows(c(res_gee, res_p)) %>%
  dplyr::select(Criterion, Method, Value)

# 生成结果表
if (nrow(res_long) > 0) {
  tbl <- res_long %>%
    mutate(Method = factor(Method,
                           levels = c("GEE - AR(1)", "GEE - Indep.", "GEE - Exch.",
                                      "PGEE - AR(1)", "PGEE - Indep.", "PGEE - Exch."))) %>%
    pivot_wider(names_from = Method, values_from = Value) %>%
    arrange(match(Criterion, c("MSE","MAE","MAPE")))
  
  # 格式化数值
  method_cols <- setdiff(names(tbl), "Criterion")
  for (col in method_cols) {
    if (col %in% names(tbl)) {
      tbl[[col]] <- ifelse(tbl$Criterion == "MAPE", 
                           sprintf("%.2f%%", tbl[[col]]), 
                           round(tbl[[col]], 3))
    }
  }
  
  print(tbl, n = 3)
}

cat("评估用时:", round(difftime(Sys.time(), eval_start, units = "secs"), 2), "秒\n")
cat("总运行时间:", round(difftime(Sys.time(), total_start, units = "secs"), 2), "秒\n")

# ==================== 保存结果 ====================
saveRDS(list(
  cv_ind  = cv_ind,  cv_exch = cv_exch,  cv_ar1 = cv_ar1,
  pgee_ind = fit_pgee_ind, pgee_exch = fit_pgee_exch, pgee_ar1 = fit_pgee_ar1,
  optimization_summary = list(
    lambda_grid_size = length(lambda_grid),
    total_matrix_computations = 4,  # 预计算1次 + 测试1次 + 最终拟合时临时2次
    total_runtime_seconds = as.numeric(difftime(Sys.time(), total_start, units = "secs"))
  )
), file = "pgee_cv_and_fits_optimized.rds")

# ---------- 1) 系数对比表（变量原名；缺项或被惩罚掉的项以 0 展示） ----------
method_levels <- c("GEE - AR(1)","GEE - Indep.","GEE - Exch.",
                   "PGEE - AR(1)","PGEE - Indep.","PGEE - Exch.")

coef_list <- list(
  "GEE - AR(1)"   = coef(fit_ar1),
  "GEE - Indep."  = coef(fit_ind),
  "GEE - Exch."   = coef(fit_exch),
  "PGEE - AR(1)"  = coef(fit_pgee_ar1),
  "PGEE - Indep." = coef(fit_pgee_ind),
  "PGEE - Exch."  = coef(fit_pgee_exch)
)

# 以训练设计矩阵的列顺序优先（含 "(Intercept)"），若某模型多出项就追加在末尾
ord_from_X <- colnames(X_full)
all_terms  <- Reduce(union, lapply(coef_list, names))
term_order <- unique(c(ord_from_X, setdiff(all_terms, ord_from_X)))

coef_df <- tibble(term = term_order)
for (m in method_levels) {
  b <- coef_list[[m]]
  coef_df[[m]] <- b[match(term_order, names(b))]
}
coef_df <- coef_df %>% mutate(across(all_of(method_levels), ~ replace_na(., 0)))

coef_tbl <- coef_df %>%
  mutate(Variable = if_else(term == "(Intercept)", "Intercept", term)) %>%
  dplyr::select(Variable, all_of(method_levels)) %>%
  mutate(across(all_of(method_levels), ~ round(.x, 3)))

# ---------- 2) 模型效力表（MSE/MAE/MAPE） ----------
eps_mape <- 1e-6
metrics <- function(y, yhat){
  tibble(
    Criterion = c("MSE","MAE","MAPE"),
    Value = c(mean((y-yhat)^2, na.rm = TRUE),
              mean(abs(y-yhat),  na.rm = TRUE),
              100*mean(abs((y-yhat)/pmax(abs(y), eps_mape)), na.rm = TRUE))
  )
}
# GEE 预测在 dat_fit 上
y_gee <- dat_fit$GDP
pred_gee <- list(
  "GEE - AR(1)"  = as.numeric(predict(fit_ar1,  newdata = dat_fit, type = "response")),
  "GEE - Indep." = as.numeric(predict(fit_ind,  newdata = dat_fit, type = "response")),
  "GEE - Exch."  = as.numeric(predict(fit_exch, newdata = dat_fit, type = "response"))
)
# PGEE 预测（与训练域一致，用 X_full 矩阵乘法）
pred_mat <- function(fit, MM) { b <- coef(fit); drop(MM[, names(b), drop = FALSE] %*% b) }
y_pgee <- dat_cv$y
pred_p <- list(
  "PGEE - AR(1)"  = pred_mat(fit_pgee_ar1,  X_full),
  "PGEE - Indep." = pred_mat(fit_pgee_ind,  X_full),
  "PGEE - Exch."  = pred_mat(fit_pgee_exch, X_full)
)

res_gee <- imap(pred_gee, ~ metrics(y_gee,  .x) %>% mutate(Method = .y))
res_p   <- imap(pred_p,   ~ metrics(y_pgee, .x) %>% mutate(Method = .y))

tbl_perf <- bind_rows(c(res_gee, res_p)) %>%
  dplyr::select(Criterion, Method, Value) %>%
  mutate(Method = factor(Method, levels = method_levels)) %>%
  pivot_wider(names_from = Method, values_from = Value) %>%
  arrange(match(Criterion, c("MSE","MAE","MAPE"))) %>%
  mutate(across(-Criterion, ~ ifelse(Criterion=="MAPE", sprintf("%.2f%%", .x), round(.x, 4))))

# ---------- 3) LaTeX 导出（两张表各一份 .tex） ----------
# 参数估计表
align_coef <- c("l", rep("c", ncol(coef_tbl)-1))
latex_coef <- kbl(
  coef_tbl, format = "latex", booktabs = TRUE, escape = TRUE,
  caption = "Comparison of parameter estimates from six methods: GEE and PGEE",
  align = align_coef, linesep = ""
) %>%
  add_header_above(c(" " = 1, "Method" = length(method_levels))) %>%
  kable_styling(latex_options = c("hold_position","striped","scale_down"))
save_kable(latex_coef, "cont_table_param_estimates.tex")

# 效力表
metrics_tbl <- tbl_perf %>% mutate(Criterion = factor(Criterion, levels = c("MSE","MAE","MAPE")))
align_met <- c("l", rep("c", ncol(metrics_tbl)-1))
latex_met <- kbl(
  metrics_tbl, format = "latex", booktabs = TRUE, escape = TRUE,
  caption = "Model performance on the full sample (MSE / MAE / MAPE)",
  align = align_met, linesep = ""
) %>% kable_styling(latex_options = c("hold_position","striped","scale_down"))
save_kable(latex_met, "cont_table_performance.tex")

# ---------- 4) Word 导出（两张表放同一个 .docx） ----------
ft_coef <- flextable(coef_tbl) |>
  add_header_row(values = c("Variable","Method"), colwidths = c(1, length(method_levels))) |>
  theme_vanilla() |>
  align(align = "center", part = "all") |>
  autofit()

ft_perf <- flextable(metrics_tbl) |>
  theme_vanilla() |>
  align(align = "center", part = "all") |>
  autofit()

doc <- read_docx()
doc <- body_add_par(doc, "Table 1. Model performance (MSE / MAE / MAPE)", style = "heading 2")
doc <- body_add_flextable(doc, ft_perf)
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_par(doc, "Table 2. Comparison of parameter estimates from six methods: GEE and PGEE", style = "heading 2")
doc <- body_add_flextable(doc, ft_coef)
print(doc, target = "continuous_GEE_PGEE_results.docx")

cat("LaTeX 导出：cont_table_param_estimates.tex, cont_table_performance.tex\n")
cat("Word 导出：continuous_GEE_PGEE_results.docx\n")

# ---------- 5) 画 CV(λ) 曲线（使用 independence 的 CV 结果） ----------
cv_grid <- if (exists("cv_ind") && is.list(cv_ind)) cv_ind$grid else if (exists("cv_result")) cv_result$grid else stop("没有找到 CV 结果对象")
cv_grid <- cv_grid %>% arrange(lambda)

i_min <- which.min(cv_grid$mse)
lam_star <- cv_grid$lambda[i_min]; loss_min <- cv_grid$mse[i_min]

xr <- range(cv_grid$lambda, na.rm = TRUE)
yr <- range(cv_grid$mse,    na.rm = TRUE)
xpad <- diff(xr) * 0.08
ypad <- diff(yr) * 0.12

p_cv <- ggplot(cv_grid, aes(x = lambda, y = mse)) +
  geom_line(color = "black") +
  geom_point(size = 1.8, color = "black") +
  annotate("point", x = lam_star, y = loss_min, shape = 21, size = 3.4,
           stroke = 1.1, colour = "red", fill = NA) +
  annotate("text", x = lam_star, y = loss_min,
           label = sprintf("(%.2f,%.4f)", lam_star, loss_min),
           vjust = -0.6, hjust = 0, size = 3.6) +
  scale_x_continuous(limits = c(xr[1] - xpad, xr[2] + xpad)) +
  scale_y_continuous(limits = c(yr[1] - ypad, yr[2] + ypad)) +
  labs(x = expression(lambda), y = expression(CV[mse](lambda)),
       title = "CV curve (independence)") +
  coord_cartesian(clip = "off") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.margin = margin(14, 28, 10, 12))

ggsave("cv_lambda_plot_independence.png", plot = p_cv, width = 6.5, height = 4.8, dpi = 300)
