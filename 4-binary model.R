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

# Candidate numeric columns (excluding non-numeric keys)
num_vars <- df %>%
  dplyr::select(-COUNTRY, -QUARTER) %>%
  dplyr::select(where(is.numeric)) %>%
  names()

# Target/Independent variables
y_var  <- "Recession_BBQ"
x_vars <- setdiff(num_vars, c(y_var, "GDP", "year","q","tnum"))

is_train <- df$QUARTER >= "2009-Q1" & df$QUARTER <= "2019-Q4"
is_test  <- df$QUARTER >= "2020-Q1" & df$QUARTER <= "2024-Q4"

# ==================== 1) X Imputation ====================
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

# ==================== 2) Within-country standardization using training set statistics ====================
dat_std <- dat_imp %>%
  group_by(COUNTRY) %>%
  group_modify(~{
    d <- .x
    d_tr <- d[d$SPLIT=="train", , drop=FALSE]
    mu <- sapply(d_tr[, x_vars, drop=FALSE], function(z) mean(z, na.rm=TRUE))
    sd <- sapply(d_tr[, x_vars, drop=FALSE], function(z) sd(z, na.rm=TRUE))
    # Apply to train+test
    zX <- Map(function(col, m, s){
      if (!is.finite(s) || s == 0) col - m else (col - m)/s
    }, d[, x_vars, drop=FALSE], as.list(mu), as.list(sd))
    d[, x_vars] <- as_tibble(zX)
    d
  }) %>% ungroup()

# ==================== 3) Full-rank selection: using only training set information ====================
X_train0 <- dat_std %>% filter(SPLIT=="train") %>%
  dplyr::select(all_of(x_vars)) %>% mutate(across(everything(), as.numeric))

# Near-zero variance
sd_all   <- sapply(X_train0, function(z) sd(z, na.rm = TRUE))
drop_nzv <- names(sd_all)[!is.finite(sd_all) | sd_all < 1e-10]

# Exactly duplicate
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

# Highly correlated
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
x_fullrank <- colnames(mm_tr)[idx]  # Final set of independent variables (selected from train)

# ==================== 4) Assemble train/test datasets ====================
dat_fit_train <- dat_std %>%
  filter(SPLIT=="train") %>%
  dplyr::select(COUNTRY, QUARTER, all_of(y_var), all_of(x_fullrank)) %>%
  filter(!is.na(.data[[y_var]])) %>%
  mutate(COUNTRY = factor(COUNTRY))

dat_fit_test <- dat_std %>%
  filter(SPLIT=="test") %>%
  dplyr::select(COUNTRY, QUARTER, all_of(y_var), all_of(x_fullrank)) %>%
  filter(!is.na(.data[[y_var]])) %>%
  mutate(COUNTRY = factor(COUNTRY, levels = levels(dat_fit_train$COUNTRY)))  # Align factor levels

# Wide format for PGEE
form_cv <- y ~ . - id
dat_cv_train <- dat_fit_train %>% transmute(id = COUNTRY, y = .data[[y_var]], !!!dplyr::select(., all_of(x_fullrank))) %>% as.data.frame()
dat_cv_test  <- dat_fit_test  %>% transmute(id = COUNTRY, y = .data[[y_var]], !!!dplyr::select(., all_of(x_fullrank))) %>% as.data.frame()

# ==================== 5) Fit three GEE (logit) models — using only the training set ====================
form_gee <- as.formula(paste(y_var, "~", paste(x_fullrank, collapse = " + ")))
family_bin <- binomial("logit")

fit_g_ar1  <- geeglm(form_gee, id = COUNTRY, data = dat_fit_train, family = family_bin, corstr = "ar1",          na.action = na.exclude)
fit_g_ind  <- geeglm(form_gee, id = COUNTRY, data = dat_fit_train, family = family_bin, corstr = "independence", na.action = na.exclude)
fit_g_exch <- geeglm(form_gee, id = COUNTRY, data = dat_fit_train, family = family_bin, corstr = "exchangeable", na.action = na.exclude)

# ==================== 6) PGEE: Deeply optimized matrix interface version ====================
# ==================== Data Preprocessing: Build all matrices at once ====================
cat("Pre-calculating design matrix and indices...\n")
prep_start <- Sys.time()

# 1. Build the full design matrix at once (including intercept)
X_full <- model.matrix(form_cv, data = dat_cv_train)
y_full <- dat_cv_train$y
id_full <- dat_cv_train$id

# 2. Determine penalty index (intercept is not penalized)
pindex <- as.integer(colnames(X_full) != "(Intercept)")
n_params <- ncol(X_full)

# 3. Optimize lambda grid
lambda_grid <- seq(0.01, 0.3, by = 0.01)

# 4. Pre-calculate cross-validation folds (by row index)
K <- 5
ids <- unique(id_full)
fold_lab <- sample(rep(1:K, length.out = length(ids)))
id2fold <- setNames(fold_lab, ids)

# Pre-calculate row indices for each fold (to avoid recalculation)
fold_row_indices <- lapply(1:K, function(k) {
  ids_valid <- names(id2fold)[id2fold == k]
  valid_rows <- which(id_full %in% ids_valid)
  train_rows <- which(!id_full %in% ids_valid)
  list(train_rows = train_rows, valid_rows = valid_rows)
})

cat("Preprocessing time:", round(difftime(Sys.time(), prep_start, units = "secs"), 2), "seconds\n")

# ==================== Optimized PGEE fitting function ====================
# Build data frame for PGEE, but reuse pre-calculated matrices
fit_pgee_matrix <- function(X, y, id, lambda, corstr) {
  # Convert matrix to data frame, keeping original variable names
  X_df <- as.data.frame(X[, -1, drop = FALSE])  # Remove intercept column
  temp_df <- data.frame(y = y, id = id, X_df)
  
  # Get original variable names (without intercept)
  var_names <- colnames(X)[-1]
  colnames(temp_df)[-(1:2)] <- var_names
  
  # Build formula
  if (length(var_names) > 0) {
    temp_formula <- as.formula(paste("y ~", paste(var_names, collapse = " + ")))
  } else {
    temp_formula <- as.formula("y ~ 1")
  }
  
  # Adjust pindex (corresponding to variables after removing intercept)
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

# Fast prediction function: direct matrix multiplication
pred_fast <- function(X, coef_vec) {
  # Ensure dimensions of X and coef match
  X_aligned <- X[, names(coef_vec), drop = FALSE]
  plogis(as.numeric(X_aligned %*% coef_vec))
}

# Evaluation metric function
brier <- function(y, p) mean((p - y)^2, na.rm = TRUE)
logloss <- function(y, p, eps = 1e-12) { 
  p <- pmax(pmin(p, 1 - eps), eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE) 
}

# ==================== Cross-validation with matrix interface (with enhanced error handling) ====================
cv_pgee_matrix <- function(corstr) {
  cat(paste("Starting cross-validation for", corstr, "correlation structure...\n"))
  
  results <- vector("list", length(lambda_grid))
  
  for (lam_idx in seq_along(lambda_grid)) {
    lam <- lambda_grid[lam_idx]
    cat(paste("  Testing lambda =", lam, "\n"))
    
    brier_folds <- numeric(K)
    logloss_folds <- numeric(K)
    success_count <- 0
    
    for (k in 1:K) {
      # Directly use pre-calculated row indices for matrix splitting
      train_rows <- fold_row_indices[[k]]$train_rows
      valid_rows <- fold_row_indices[[k]]$valid_rows
      
      X_train <- X_full[train_rows, , drop = FALSE]
      y_train <- y_full[train_rows]
      id_train <- id_full[train_rows]
      
      X_valid <- X_full[valid_rows, , drop = FALSE]
      y_valid <- y_full[valid_rows]
      
      # Check data quality
      if (length(unique(y_train)) < 2 || length(unique(id_train)) < 2) {
        brier_folds[k] <- NA_real_
        logloss_folds[k] <- NA_real_
        next
      }
      
      # Fit model
      fit_k <- try({
        fit_pgee_matrix(X_train, y_train, id_train, lam, corstr)
      }, silent = TRUE)
      
      if (!inherits(fit_k, "try-error")) {
        coef_k <- try(coef(fit_k), silent = TRUE)
        
        if (!inherits(coef_k, "try-error") && all(is.finite(coef_k))) {
          # Fast prediction: direct matrix multiplication
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
        # Output error message for debugging
        if (k == 1) cat("    Fit error:", as.character(fit_k), "\n")
        brier_folds[k] <- NA_real_
        logloss_folds[k] <- NA_real_
      }
    }
    
    cat(paste("    Successful folds:", success_count, "/", K, "\n"))
    
    if (success_count >= 2) {  # Considered valid only if at least 2 folds succeed
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
    cat("All lambdas failed, attempting to fall back to the original method...\n")
    # Fallback: use the original data frame method
    return(fallback_cv(corstr))
  }
  
  lam_opt <- res_df$lambda[which.min(res_df$brier)]
  list(grid = res_df, lam_opt = lam_opt)
}

# Fallback method: use original approach but with optimized parameters
fallback_cv <- function(corstr) {
  cat("Using fallback method for cross-validation...\n")
  
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
    stop("All methods failed, please check data and parameter settings")
  }
  
  lam_opt <- results$lambda[which.min(results$brier)]
  list(grid = results, lam_opt = lam_opt)
}

# ==================== Execute Cross-Validation ====================
cat("Starting cross-validation...\n")
cv_start <- Sys.time()

cv_result <- cv_pgee_matrix("independence")
lam_sel <- cv_result$lam_opt

cat("Optimal lambda:", lam_sel, "\n")
cat("Cross-validation time:", round(difftime(Sys.time(), cv_start, units = "secs"), 2), "seconds\n")

# ==================== Fit Final Models ====================
cat("Fitting final models...\n")
fit_start <- Sys.time()

# Fit the final model on the complete training set
fit_p_ind  <- fit_pgee_matrix(X_full, y_full, id_full, lam_sel, "independence")
fit_p_exch <- fit_pgee_matrix(X_full, y_full, id_full, lam_sel, "exchangeable")
fit_p_ar1  <- fit_pgee_matrix(X_full, y_full, id_full, lam_sel, "AR-1")

cat("Final model fitting time:", round(difftime(Sys.time(), fit_start, units = "secs"), 2), "seconds\n")

# ==================== Test Set Evaluation ====================
cat("Evaluating test set performance...\n")
eval_start <- Sys.time()

# Pre-calculate test set design matrix
X_test <- model.matrix(form_cv, data = dat_cv_test)
y_test <- dat_fit_test[[y_var]]

# GEE prediction (keeping original method)
p_g_test <- list(
  "GEE - AR(1)"   = as.numeric(predict(fit_g_ar1,  newdata = dat_fit_test, type = "response")),
  "GEE - Indep."  = as.numeric(predict(fit_g_ind,  newdata = dat_fit_test, type = "response")),
  "GEE - Exch."   = as.numeric(predict(fit_g_exch, newdata = dat_fit_test, type = "response"))
)

# PGEE prediction: fast matrix multiplication
p_p_test <- list(
  "PGEE - AR(1)"  = pred_fast(X_test, coef(fit_p_ar1)),
  "PGEE - Indep." = pred_fast(X_test, coef(fit_p_ind)),
  "PGEE - Exch."  = pred_fast(X_test, coef(fit_p_exch))
)

# Batch calculate evaluation metrics
metric_vec <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  if (sum(ok) == 0) return(tibble(Brier = NA, LogLoss = NA, `ROC-AUC` = NA))
  
  tibble(
    Brier = brier(y[ok], p[ok]),
    LogLoss = logloss(y[ok], p[ok]),
    `ROC-AUC` = suppressWarnings(as.numeric(pROC::auc(y[ok], p[ok])))
  )
}

# Combine evaluation results
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

cat("Test set evaluation time:", round(difftime(Sys.time(), eval_start, units = "secs"), 2), "seconds\n")
cat("Total time:", round(difftime(Sys.time(), prep_start, units = "secs"), 2), "seconds\n")

print(tbl_test, n = 3)

# Show optimization effect
cat("\n=== Optimization Summary ===\n")
cat("- Number of design matrix calculations: reduced from", (length(lambda_grid) * K + 3), "to 4\n")
cat("- Number of data frame splits: reduced from", length(lambda_grid) * K * 2, "to 0\n")
cat("- Prediction calculation: all using O(1) matrix multiplication\n")


# Export tex and doc documents
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
# Missing/penalized coefficients -> display as 0
coef_df <- coef_df %>% mutate(across(all_of(method_levels), ~ replace_na(., 0)))

coef_tbl <- coef_df %>%
  mutate(Variable = if_else(term == "(Intercept)", "Intercept", term)) %>%
  dplyr::select(Variable, all_of(method_levels)) %>%
  mutate(across(all_of(method_levels), ~ round(.x, 3)))

metrics_tbl <- tbl_test %>%
  mutate(Criterion = factor(Criterion, levels = c("Brier","LogLoss","ROC-AUC")))

print(metrics_tbl)
# --- 3) Render to LaTeX (kableExtra), export two .tex files ---
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

# Dynamically expand coordinate range to avoid boundary clipping
xr   <- range(df_cv$lambda, na.rm = TRUE)
yr   <- range(df_cv$loss,   na.rm = TRUE)
xpad <- diff(xr) * 0.08    # 8% horizontal padding
ypad <- diff(yr) * 0.12    # 12% vertical padding

p_cv <- ggplot(df_cv, aes(x = lambda, y = loss)) +
  geom_line(color = "black") +
  geom_point(size = 1.8, color = "black") +
  annotate("point", x = lam_star, y = loss_min, shape = 21, size = 3.4,
           stroke = 1.1, colour = "red", fill = NA) +
  annotate("text",
           x = lam_star, y = loss_min,
           label = sprintf("(%.2f,%.3f)", lam_star, loss_min),
           vjust = -0.6, hjust = 0,  # Align text to the left, place it above and to the right of the point
           size = 3.6) +
  scale_x_continuous(limits = c(xr[1] - xpad, xr[2] + xpad)) +
  scale_y_continuous(limits = c(yr[1] - ypad, yr[2] + ypad)) +
  labs(x = expression(lambda),
       y = bquote(CV(.(metric_name))(lambda))) +
  coord_cartesian(clip = "off") +   # Allow text to be drawn outside the panel
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 28, 10, 12) # Increase right margin to ensure labels are fully displayed
  )

ggsave("cv_lambda_plot.png", plot = p_cv, width = 6.5, height = 4.8, dpi = 300)

