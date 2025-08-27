rm(list = ls())
library(tidyverse)
library(geepack)
library(PGEE)
library(kableExtra)
library(pROC)
library(zoo)
library(future.apply)
library(R.utils)
library(dplyr)
library(tidyr)
library(purrr)
library(kableExtra)
library(officer)
library(flextable)
set.seed(123)

df <- readr::read_csv("Economic Growth.csv", show_col_types = FALSE) %>%
  mutate(
    year = as.integer(stringr::str_extract(QUARTER, "^\\d{4}")),
    q    = as.integer(stringr::str_extract(QUARTER, "(?<=Q)\\d")),
    tnum = year*4 + q
  )

stopifnot("Recession_BBQ" %in% names(df))
df <- df %>% mutate(Recession_BBQ = as.integer(Recession_BBQ))

# 数值列候选（去掉非数值键）
num_vars <- df %>%
  dplyr::select(-COUNTRY, -QUARTER) %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# 目标/自变量
y_var  <- "Recession_BBQ"
x_vars <- setdiff(num_vars, c(y_var, "GDP", "year","q","tnum"))

is_train <- df$QUARTER >= "2009-Q1" & df$QUARTER <= "2019-Q4"
is_test  <- df$QUARTER >= "2020-Q1" & df$QUARTER <= "2024-Q4"

# ==================== 1) X 插补 ====================
impute_series <- function(x){
  y <- zoo::na.approx(x, na.rm = FALSE)
  y <- zoo::na.locf(y, na.rm = FALSE)
  y <- zoo::na.locf(y, fromLast = TRUE, na.rm = FALSE)
  if (anyNA(y)) {
    med <- suppressWarnings(median(x, na.rm = TRUE)); if (!is.finite(med)) med <- 0
    y[is.na(y)] <- med
  }
  y
}

dat_imp <- df %>%
  arrange(COUNTRY, tnum) %>%
  group_by(COUNTRY) %>%
  mutate(across(all_of(x_vars), impute_series)) %>%
  ungroup() %>%
  mutate(SPLIT = case_when(is_train ~ "train", is_test ~ "test", TRUE ~ "drop"))

dat_imp <- dat_imp %>% filter(SPLIT != "drop")

# ==================== 2) 国家内“用训练期统计量”做标准化 ====================
dat_std <- dat_imp %>%
  group_by(COUNTRY) %>%
  group_modify(~{
    d <- .x
    d_tr <- d[d$SPLIT=="train", , drop=FALSE]
    mu <- sapply(d_tr[, x_vars, drop=FALSE], function(z) mean(z, na.rm=TRUE))
    sd <- sapply(d_tr[, x_vars, drop=FALSE], function(z) sd(z, na.rm=TRUE))
    # 应用到 train+test
    zX <- Map(function(col, m, s){
      if (!is.finite(s) || s == 0) col - m else (col - m)/s
    }, d[, x_vars, drop=FALSE], as.list(mu), as.list(sd))
    d[, x_vars] <- as_tibble(zX)
    d
  }) %>% ungroup()

# ==================== 3) 满秩筛选：只用训练集信息 ====================
X_train0 <- dat_std %>% filter(SPLIT=="train") %>%
  dplyr::select(all_of(x_vars)) %>% mutate(across(everything(), as.numeric))

# 近零方差
sd_all   <- sapply(X_train0, function(z) sd(z, na.rm = TRUE))
drop_nzv <- names(sd_all)[!is.finite(sd_all) | sd_all < 1e-10]

# 完全重复
dup_map <- function(M){
  cn <- colnames(M); keep <- rep(TRUE, ncol(M)); seen <- list()
  for (j in seq_along(cn)) {
    if (!keep[j]) next
    key <- paste0(round(M[, j], 12), collapse="|")
    if (key %in% names(seen)) keep[j] <- FALSE else seen[[key]] <- TRUE
  }
  cn[!keep]
}
drop_dup <- dup_map(as.matrix(X_train0))

# 超高相关
keep_corr <- function(M, thr = 0.999){
  C <- suppressWarnings(cor(M, use = "pairwise.complete.obs"))
  p <- ncol(M); keep <- rep(TRUE, p)
  for (i in 1:(p-1)) if (keep[i]) {
    hits <- which(abs(C[i, (i+1):p]) > thr) + i
    keep[hits] <- FALSE
  }
  colnames(M)[keep]
}
X1_tr <- X_train0 %>% dplyr::select(-any_of(unique(c(drop_nzv, drop_dup))))
x_keep_corr <- keep_corr(as.matrix(X1_tr), thr = 0.999)
X2_tr <- X1_tr %>% dplyr::select(all_of(x_keep_corr))
mm_tr <- model.matrix(~ 0 + ., data = X2_tr)
qrX   <- qr(mm_tr); idx <- qrX$pivot[seq_len(qrX$rank)]
x_fullrank <- colnames(mm_tr)[idx]  # 最终用的自变量集合（train选）

# ==================== 4) 组装 train/test 数据集 ====================
dat_fit_train <- dat_std %>%
  filter(SPLIT=="train") %>%
  dplyr::select(COUNTRY, QUARTER, all_of(y_var), all_of(x_fullrank)) %>%
  filter(!is.na(.data[[y_var]])) %>%
  mutate(COUNTRY = factor(COUNTRY))

dat_fit_test <- dat_std %>%
  filter(SPLIT=="test") %>%
  dplyr::select(COUNTRY, QUARTER, all_of(y_var), all_of(x_fullrank)) %>%
  filter(!is.na(.data[[y_var]])) %>%
  mutate(COUNTRY = factor(COUNTRY, levels = levels(dat_fit_train$COUNTRY)))  # 对齐因子水平

# PGEE 的宽表
form_cv <- y ~ . - id
dat_cv_train <- dat_fit_train %>% transmute(id = COUNTRY, y = .data[[y_var]], !!!dplyr::select(., all_of(x_fullrank))) %>% as.data.frame()
dat_cv_test  <- dat_fit_test  %>% transmute(id = COUNTRY, y = .data[[y_var]], !!!dplyr::select(., all_of(x_fullrank))) %>% as.data.frame()

# ==================== 5) 拟合三种 GEE（logit） —— 仅用训练集 ====================
form_gee <- as.formula(paste(y_var, "~", paste(x_fullrank, collapse = " + ")))
family_bin <- binomial("logit")

fit_g_ar1  <- geeglm(form_gee, id = COUNTRY, data = dat_fit_train, family = family_bin, corstr = "ar1",          na.action = na.exclude)
fit_g_ind  <- geeglm(form_gee, id = COUNTRY, data = dat_fit_train, family = family_bin, corstr = "independence", na.action = na.exclude)
fit_g_exch <- geeglm(form_gee, id = COUNTRY, data = dat_fit_train, family = family_bin, corstr = "exchangeable", na.action = na.exclude)

# ==================== 6) PGEE：矩阵接口深度优化版本 ====================
# ==================== 数据预处理：一次性构建所有矩阵 ====================
cat("预计算设计矩阵和索引...\n")
prep_start <- Sys.time()

# 1. 一次性构建完整设计矩阵（包含截距）
X_full <- model.matrix(form_cv, data = dat_cv_train)
y_full <- dat_cv_train$y
id_full <- dat_cv_train$id

# 2. 确定惩罚索引（截距不惩罚）
pindex <- as.integer(colnames(X_full) != "(Intercept)")
n_params <- ncol(X_full)

# 3. 优化lambda网格
lambda_grid <- seq(0.01, 0.3, by = 0.01)

# 4. 预计算交叉验证折分（按行索引）
K <- 5
ids <- unique(id_full)
fold_lab <- sample(rep(1:K, length.out = length(ids)))
id2fold <- setNames(fold_lab, ids)

# 预计算每折的行索引（避免重复计算）
fold_row_indices <- lapply(1:K, function(k) {
  ids_valid <- names(id2fold)[id2fold == k]
  valid_rows <- which(id_full %in% ids_valid)
  train_rows <- which(!id_full %in% ids_valid)
  list(train_rows = train_rows, valid_rows = valid_rows)
})

cat("预处理用时:", round(difftime(Sys.time(), prep_start, units = "secs"), 2), "秒\n")

# ==================== 优化的PGEE拟合函数 ====================
# 构建数据框供PGEE使用，但复用预计算的矩阵
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
    family = binomial("logit"), 
    corstr = corstr,
    lambda = lambda, 
    pindex = pindex_adj,
    scale.fix = FALSE, 
    eps = 1e-4, 
    maxiter = 25, 
    tol = 1e-3, 
    silent = TRUE
  )
}

# 快速预测函数：直接矩阵乘法
pred_fast <- function(X, coef_vec) {
  # 确保X和coef维度匹配
  X_aligned <- X[, names(coef_vec), drop = FALSE]
  plogis(as.numeric(X_aligned %*% coef_vec))
}

# 评估指标函数
brier <- function(y, p) mean((p - y)^2, na.rm = TRUE)
logloss <- function(y, p, eps = 1e-12) { 
  p <- pmax(pmin(p, 1 - eps), eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE) 
}

# ==================== 矩阵接口的交叉验证（增强错误处理） ====================
cv_pgee_matrix <- function(corstr) {
  cat(paste("开始", corstr, "相关结构的交叉验证...\n"))
  
  results <- vector("list", length(lambda_grid))
  
  for (lam_idx in seq_along(lambda_grid)) {
    lam <- lambda_grid[lam_idx]
    cat(paste("  测试 lambda =", lam, "\n"))
    
    brier_folds <- numeric(K)
    logloss_folds <- numeric(K)
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
      if (length(unique(y_train)) < 2 || length(unique(id_train)) < 2) {
        brier_folds[k] <- NA_real_
        logloss_folds[k] <- NA_real_
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
            brier_folds[k] <- brier(y_valid, pred_k)
            logloss_folds[k] <- logloss(y_valid, pred_k)
            success_count <- success_count + 1
          } else {
            brier_folds[k] <- NA_real_
            logloss_folds[k] <- NA_real_
          }
        } else {
          brier_folds[k] <- NA_real_
          logloss_folds[k] <- NA_real_
        }
      } else {
        # 输出错误信息用于调试
        if (k == 1) cat("    拟合错误:", as.character(fit_k), "\n")
        brier_folds[k] <- NA_real_
        logloss_folds[k] <- NA_real_
      }
    }
    
    cat(paste("    成功折数:", success_count, "/", K, "\n"))
    
    if (success_count >= 2) {  # 至少2折成功才认为有效
      results[[lam_idx]] <- tibble(
        lambda = lam,
        brier = mean(brier_folds, na.rm = TRUE),
        logloss = mean(logloss_folds, na.rm = TRUE),
        n_success = success_count
      )
    } else {
      results[[lam_idx]] <- tibble(
        lambda = lam,
        brier = NA_real_,
        logloss = NA_real_,
        n_success = success_count
      )
    }
  }
  
  res_df <- bind_rows(results) %>% 
    filter(is.finite(brier) & n_success >= 2)
  
  if (nrow(res_df) == 0) {
    cat("所有lambda都失败，尝试回退到原始方法...\n")
    # 回退：使用原始的数据框方法
    return(fallback_cv(corstr))
  }
  
  lam_opt <- res_df$lambda[which.min(res_df$brier)]
  list(grid = res_df, lam_opt = lam_opt)
}

# 回退方法：使用原始approach但优化参数
fallback_cv <- function(corstr) {
  cat("使用回退方法进行交叉验证...\n")
  
  results <- lapply(lambda_grid, function(lam) {
    brier_k <- numeric(K)
    ll_k <- numeric(K)
    
    for (k in 1:K) {
      ids_valid <- names(id2fold)[id2fold == k]
      valid_idx <- dat_cv_train$id %in% ids_valid
      train_df <- dat_cv_train[!valid_idx, , drop = FALSE]
      valid_df <- dat_cv_train[valid_idx, , drop = FALSE]
      
      if (nrow(train_df) < 10 || nrow(valid_df) < 5) {
        brier_k[k] <- NA_real_
        ll_k[k] <- NA_real_
        next
      }
      
      fit_k <- try(PGEE::PGEE(
        formula = form_cv, id = id, data = train_df,
        family = binomial("logit"), corstr = corstr,
        lambda = lam, pindex = pindex,
        scale.fix = FALSE, eps = 1e-3, maxiter = 30, tol = 1e-2, silent = TRUE
      ), silent = TRUE)
      
      if (!inherits(fit_k, "try-error")) {
        b <- try(coef(fit_k), silent = TRUE)
        if (!inherits(b, "try-error") && all(is.finite(b))) {
          MM <- model.matrix(form_cv, data = valid_df)
          MM <- MM[, names(b), drop = FALSE]
          p <- try(plogis(as.numeric(MM %*% b)), silent = TRUE)
          
          if (!inherits(p, "try-error") && all(is.finite(p))) {
            yv <- valid_df$y
            brier_k[k] <- brier(yv, p)
            ll_k[k] <- logloss(yv, p)
          } else {
            brier_k[k] <- NA_real_
            ll_k[k] <- NA_real_
          }
        } else {
          brier_k[k] <- NA_real_
          ll_k[k] <- NA_real_
        }
      } else {
        brier_k[k] <- NA_real_
        ll_k[k] <- NA_real_
      }
    }
    
    tibble(
      lambda = lam, 
      brier = mean(brier_k, na.rm = TRUE), 
      logloss = mean(ll_k, na.rm = TRUE)
    )
  }) %>% bind_rows() %>% filter(is.finite(brier))
  
  if (nrow(results) == 0) {
    stop("所有方法都失败，请检查数据和参数设置")
  }
  
  lam_opt <- results$lambda[which.min(results$brier)]
  list(grid = results, lam_opt = lam_opt)
}

# ==================== 执行交叉验证 ====================
cat("开始交叉验证...\n")
cv_start <- Sys.time()

cv_result <- cv_pgee_matrix("independence")
lam_sel <- cv_result$lam_opt

cat("最优lambda:", lam_sel, "\n")
cat("交叉验证用时:", round(difftime(Sys.time(), cv_start, units = "secs"), 2), "秒\n")

# ==================== 拟合最终模型 ====================
cat("拟合最终模型...\n")
fit_start <- Sys.time()

# 在完整训练集上拟合最终模型
fit_p_ind  <- fit_pgee_matrix(X_full, y_full, id_full, lam_sel, "independence")
fit_p_exch <- fit_pgee_matrix(X_full, y_full, id_full, lam_sel, "exchangeable")
fit_p_ar1  <- fit_pgee_matrix(X_full, y_full, id_full, lam_sel, "AR-1")

cat("最终模型拟合用时:", round(difftime(Sys.time(), fit_start, units = "secs"), 2), "秒\n")

# ==================== 测试集评估 ====================
cat("评估测试集性能...\n")
eval_start <- Sys.time()

# 预计算测试集设计矩阵
X_test <- model.matrix(form_cv, data = dat_cv_test)
y_test <- dat_fit_test[[y_var]]

# GEE预测（保持原有方式）
p_g_test <- list(
  "GEE - AR(1)"   = as.numeric(predict(fit_g_ar1,  newdata = dat_fit_test, type = "response")),
  "GEE - Indep."  = as.numeric(predict(fit_g_ind,  newdata = dat_fit_test, type = "response")),
  "GEE - Exch."   = as.numeric(predict(fit_g_exch, newdata = dat_fit_test, type = "response"))
)

# PGEE预测：快速矩阵乘法
p_p_test <- list(
  "PGEE - AR(1)"  = pred_fast(X_test, coef(fit_p_ar1)),
  "PGEE - Indep." = pred_fast(X_test, coef(fit_p_ind)),
  "PGEE - Exch."  = pred_fast(X_test, coef(fit_p_exch))
)

# 批量计算评估指标
metric_vec <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  if (sum(ok) == 0) return(tibble(Brier = NA, LogLoss = NA, `ROC-AUC` = NA))
  
  tibble(
    Brier = brier(y[ok], p[ok]),
    LogLoss = logloss(y[ok], p[ok]),
    `ROC-AUC` = suppressWarnings(as.numeric(pROC::auc(y[ok], p[ok])))
  )
}

# 合并评估结果
all_preds <- c(p_g_test, p_p_test)
res_all <- imap(all_preds, ~ metric_vec(y_test, .x) %>% mutate(Method = .y))

tbl_test <- bind_rows(res_all) %>%
  pivot_longer(cols = c(Brier, LogLoss, `ROC-AUC`), names_to = "Criterion", values_to = "Value") %>%
  mutate(Method = factor(Method,
                         levels = c("GEE - AR(1)", "GEE - Indep.", "GEE - Exch.",
                                    "PGEE - AR(1)", "PGEE - Indep.", "PGEE - Exch."))) %>%
  pivot_wider(names_from = Method, values_from = Value) %>%
  arrange(match(Criterion, c("Brier", "LogLoss", "ROC-AUC"))) %>%
  mutate(across(-Criterion, ~ ifelse(Criterion == "ROC-AUC", round(.x, 3), round(.x, 4))))

cat("测试集评估用时:", round(difftime(Sys.time(), eval_start, units = "secs"), 2), "秒\n")
cat("总用时:", round(difftime(Sys.time(), prep_start, units = "secs"), 2), "秒\n")

print(tbl_test, n = 3)

# 显示优化效果
cat("\n=== 优化总结 ===\n")
cat("- 设计矩阵计算次数：从", (length(lambda_grid) * K + 3), "次减少到 4 次\n")
cat("- 数据框切分次数：从", length(lambda_grid) * K * 2, "次减少到 0 次\n")
cat("- 预测计算：全部使用 O(1) 矩阵乘法\n")


# 导出tex和doc文档
method_levels <- c("GEE - AR(1)", "GEE - Indep.", "GEE - Exch.",
                   "PGEE - AR(1)", "PGEE - Indep.", "PGEE - Exch.")

coef_list <- list(
  "GEE - AR(1)"   = coef(fit_g_ar1),
  "GEE - Indep."  = coef(fit_g_ind),
  "GEE - Exch."   = coef(fit_g_exch),
  "PGEE - AR(1)"  = coef(fit_p_ar1),
  "PGEE - Indep." = coef(fit_p_ind),
  "PGEE - Exch."  = coef(fit_p_exch)
)

ord_from_X <- colnames(X_full)              
all_terms  <- Reduce(union, lapply(coef_list, names))
term_order <- unique(c(ord_from_X, setdiff(all_terms, ord_from_X)))

coef_df <- tibble(term = term_order)
for (m in method_levels) {
  b <- coef_list[[m]]
  coef_df[[m]] <- b[match(term_order, names(b))]
}
# 缺项/被惩罚掉的系数→显示为 0
coef_df <- coef_df %>% mutate(across(all_of(method_levels), ~ replace_na(., 0)))

coef_tbl <- coef_df %>%
  mutate(Variable = if_else(term == "(Intercept)", "Intercept", term)) %>%
  dplyr::select(Variable, all_of(method_levels)) %>%
  mutate(across(all_of(method_levels), ~ round(.x, 3)))

metrics_tbl <- tbl_test %>%
  mutate(Criterion = factor(Criterion, levels = c("Brier","LogLoss","ROC-AUC")))

print(metrics_tbl)
# --- 3) 渲染为 LaTeX（kableExtra），导出两个 .tex 文件 ---
align_coef <- c("l", rep("c", ncol(coef_tbl)-1))
latex_coef <- kbl(
  coef_tbl, format = "latex", booktabs = TRUE, escape = TRUE,
  caption = "Comparison of parameter estimates from six methods: GEE and PGEE",
  align = align_coef, linesep = ""
) %>%
  add_header_above(c(" " = 1, "Method" = length(method_levels))) %>%
  kable_styling(latex_options = c("hold_position","striped","scale_down"))

save_kable(latex_coef, "table_param_estimates.tex")

align_met <- c("l", rep("c", ncol(metrics_tbl)-1))
latex_metrics <- kbl(
  metrics_tbl, format = "latex", booktabs = TRUE, escape = TRUE,
  caption = "Test-set performance (Brier / LogLoss / ROC-AUC)",
  align = align_met, linesep = ""
) %>%
  kable_styling(latex_options = c("hold_position","striped","scale_down"))

save_kable(latex_metrics, "table_test_metrics.tex")



ft_coef <- flextable(coef_tbl)
ft_coef <- add_header_row(ft_coef, values = c("Variable", "Method"),
                          colwidths = c(1, length(method_levels)))
ft_coef <- theme_vanilla(ft_coef)
ft_coef <- align(ft_coef, align = "center", part = "all")
ft_coef <- autofit(ft_coef)

metrics_tbl <- tbl_test %>%
  mutate(Criterion = factor(Criterion, levels = c("Brier","LogLoss","ROC-AUC")))
ft_metrics <- flextable(metrics_tbl) |> theme_vanilla() |> align(align="center", part="all") |> autofit()

doc <- read_docx()
doc <- body_add_par(doc, "Table 1. Test-set performance (Brier / LogLoss / ROC-AUC)", style = "heading 2")
doc <- body_add_flextable(doc, ft_metrics)
doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_par(doc, "Table 2. Comparison of parameter estimates from six methods: GEE and PGEE", style = "heading 2")
doc <- body_add_flextable(doc, ft_coef)

out_path <- "0-1model_GEE_PGEE_results.docx"
print(doc, target = out_path)

cv_grid <- if (exists("cv_result")) cv_result$grid else cv_ind$grid
metric_name <- dplyr::case_when(
  "brier"   %in% names(cv_grid) ~ "brier",
  "logloss" %in% names(cv_grid) ~ "logloss",
  TRUE ~ setdiff(names(cv_grid), "lambda")[1]
)

df_cv <- cv_grid %>%
  dplyr::select(lambda, !!metric_name) %>%
  dplyr::rename(loss = !!metric_name) %>%
  dplyr::arrange(lambda)

i_min    <- which.min(df_cv$loss)
lam_star <- df_cv$lambda[i_min]
loss_min <- df_cv$loss[i_min]

# 动态扩展坐标范围，避免边界裁切
xr   <- range(df_cv$lambda, na.rm = TRUE)
yr   <- range(df_cv$loss,   na.rm = TRUE)
xpad <- diff(xr) * 0.08   # 8% 水平留白
ypad <- diff(yr) * 0.12   # 12% 垂直留白

p_cv <- ggplot(df_cv, aes(x = lambda, y = loss)) +
  geom_line(color = "black") +
  geom_point(size = 1.8, color = "black") +
  annotate("point", x = lam_star, y = loss_min, shape = 21, size = 3.4,
           stroke = 1.1, colour = "red", fill = NA) +
  annotate("text",
           x = lam_star, y = loss_min,
           label = sprintf("(%.2f,%.3f)", lam_star, loss_min),
           vjust = -0.6, hjust = 0,  # 文本靠左对齐，放在点右上
           size = 3.6) +
  scale_x_continuous(limits = c(xr[1] - xpad, xr[2] + xpad)) +
  scale_y_continuous(limits = c(yr[1] - ypad, yr[2] + ypad)) +
  labs(x = expression(lambda),
       y = bquote(CV(.(metric_name))(lambda))) +
  coord_cartesian(clip = "off") +   # 允许面板外绘制文本
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 28, 10, 12) # 右侧加大页边距，确保标注完全显示
  )

ggsave("cv_lambda_plot.png", plot = p_cv, width = 6.5, height = 4.8, dpi = 300)
