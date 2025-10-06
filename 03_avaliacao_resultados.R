# 03_avaliacao_resultados.R — avaliação COM truth set (id_A, id_B)
# ----------------------------------------------------------------
# Usa:
#   - 3_results/pairs_ranked.csv  (todos os candidatos + Weight)
#   - 3_results/matches_links.csv (opcional: links do epiClassify)
#   - 3_results/truth_pairs.csv   (pares verdadeiros: id_A, id_B)
# Gera:
#   - metrics_top1.csv, metrics_epi.csv (se houver), pr_curve.csv
#   - false_positives.csv, false_negatives.csv
#   - coverage.txt (cobertura da blocagem), hist_pesos.png, pr_curve.png
#   - roc_curve_allpairs.csv/.png e roc_curve_top1.csv/.png  (NOVO)

library(dplyr)
library(ggplot2)
library(readr)
library(pROC)  # para ROC/AUC

# leitura dos datasets
pairs_ranked <- read.csv("3_results/linkage/pairs_ranked.csv", stringsAsFactors = FALSE, check.names = FALSE)
truth        <- read.csv("1_raw_data/truth_pairs.csv",         stringsAsFactors = FALSE, check.names = FALSE)
links_raw    <- read.csv("3_results/linkage/matches_links.csv", stringsAsFactors = FALSE, check.names = FALSE)

# --- helpers ---------------------------------------------------------------
guess_col <- function(df, candidates) {
  opts <- candidates[candidates %in% names(df)]
  if (length(opts) == 0) NA_character_ else opts[1]
}
guess_weight_col <- function(df) {
  guess_col(df, c("Weight","weight","score","Score","peso","PESO"))
}
metrics_from_pred <- function(pred_df, truth_df) {
  pred_keys  <- paste(pred_df$id_A,  pred_df$id_B,  sep = "_")
  truth_keys <- paste(truth_df$id_A, truth_df$id_B, sep = "_")
  TP <- sum(pred_keys %in% truth_keys)
  FP <- nrow(pred_df) - TP
  FN <- sum(!(truth_keys %in% pred_keys))
  precision <- ifelse((TP+FP)==0, NA, TP/(TP+FP))
  recall    <- ifelse((TP+FN)==0, NA, TP/(TP+FN))
  f1        <- ifelse(is.na(precision)|is.na(recall)|(precision+recall)==0, NA, 2*precision*recall/(precision+recall))
  tibble(TP, FP, FN, precision, recall, f1, n_pred = nrow(pred_df))
}

# --- padronizar colunas ----------------------------------------------------
idA_col_p <- guess_col(pairs_ranked, c("id_A","id_A.1","id1","id_A_1","id_A.x"))
idB_col_p <- guess_col(pairs_ranked, c("id_B","id_B.2","id2","id_B_2","id_B.y"))
w_col_p   <- guess_weight_col(pairs_ranked)
if (is.na(idA_col_p) || is.na(idB_col_p)) stop("pairs_ranked não tem id_A/id_B (verifique saída do getPairs).")
if (is.na(w_col_p)) stop("pairs_ranked não tem Weight/score.")
pairs_ranked <- pairs_ranked |> rename(id_A = !!idA_col_p, id_B = !!idB_col_p, Weight = !!w_col_p)

idA_col_t <- guess_col(truth, c("id_A","id1","id_A_true","idA"))
idB_col_t <- guess_col(truth, c("id_B","id2","id_B_true","idB"))
if (is.na(idA_col_t) || is.na(idB_col_t)) stop("truth_pairs.csv precisa ter colunas id_A e id_B (ou nomes equivalentes).")
truth <- truth |> rename(id_A = !!idA_col_t, id_B = !!idB_col_t) |> distinct(id_A, id_B)

# --- cobertura da blocagem (upper bound do recall) -------------------------
cand_keys  <- paste(pairs_ranked$id_A, pairs_ranked$id_B, sep = "_")
truth_keys <- paste(truth$id_A,       truth$id_B,       sep = "_")
coverage   <- mean(truth_keys %in% cand_keys)  # fração de verdade que apareceu como candidato
writeLines(sprintf("Cobertura da blocagem (verdade presente nos candidatos): %.3f", coverage),
           con = "3_results/coverage.txt")

# --- histograma de pesos (diagnóstico) -------------------------------------
p_hist <- ggplot(pairs_ranked, aes(x = Weight)) +
  geom_histogram(bins = 40) +
  ggtitle("Distribuição dos Pesos de Similaridade (pairs_ranked)")
ggsave("3_results/hist_pesos.png", p_hist, width = 7, height = 4, dpi = 120)

# --- DECISÃO A: top-1 por id_B (com e sem limiar) --------------------------
decision_threshold <- 0.85  # ajuste conforme sua calibração

pairs_top1 <- pairs_ranked |>
  group_by(id_B) |>
  slice_max(order_by = Weight, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(id_A, id_B, Weight)

pred_top1_all   <- pairs_top1
pred_top1_thr   <- pairs_top1 |> filter(Weight >= decision_threshold)

m_top1_all <- metrics_from_pred(pred_top1_all, truth) |> mutate(set = "top1_sem_limiar")
m_top1_thr <- metrics_from_pred(pred_top1_thr, truth) |> mutate(set = paste0("top1_weight>=", decision_threshold))
bind_rows(m_top1_all, m_top1_thr) |>
  write.csv("3_results/metrics_top1.csv", row.names = FALSE)

# --- DECISÃO B: links do epiClassify (se houver) ---------------------------
if (!is.null(links_raw)) {
  idA_col_l <- guess_col(links_raw, c("id_A","id_A.1","id1","id_A_1","id_A.x"))
  idB_col_l <- guess_col(links_raw, c("id_B","id_B.2","id2","id_B_2","id_B.y"))
  w_col_l   <- guess_weight_col(links_raw); if (is.na(w_col_l)) w_col_l <- "Weight"
  if (!w_col_l %in% names(links_raw)) links_raw$Weight <- NA_real_
  links <- links_raw |> rename(id_A = !!idA_col_l, id_B = !!idB_col_l, Weight = !!w_col_l) |>
    select(id_A, id_B, Weight)
  
  m_links <- metrics_from_pred(links, truth) |> mutate(set = "epiClassify_links")
  write.csv(m_links, "3_results/metrics_epi.csv", row.names = FALSE)  # (corrigi só para gravar o objeto)
}

# --- PR curve (varrendo limiar em top-1) -----------------------------------
grid <- unique(round(seq(quantile(pairs_top1$Weight, 0.05),
                         quantile(pairs_top1$Weight, 0.99), length.out = 40), 3))
pr_df <- lapply(grid, function(t) {
  pred <- pairs_top1 |> filter(Weight >= t)
  cbind(threshold = t, metrics_from_pred(pred, truth))
}) |> bind_rows()
write.csv(pr_df,"3_results/pr_curve.csv", row.names = FALSE)

p_pr <- ggplot(pr_df, aes(x = recall, y = precision)) +
  geom_path() + geom_point(size = 1) +
  ggtitle("Precision-Recall (varrendo limiar no top-1 por id_B)")
ggsave("3_results/pr_curve.png", p_pr, width = 6, height = 5, dpi = 120)

# --- erros para revisão: FP e FN -------------------------------------------
# Base para checagens (usar top-1 com limiar definido)
pred_keys <- paste(pred_top1_thr$id_A, pred_top1_thr$id_B, sep = "_")
FP_keys   <- setdiff(pred_keys, truth_keys)
FN_keys   <- setdiff(truth_keys, pred_keys)

# FP: pegue do pairs_ranked (tem Weight)
fp_tbl <- pairs_ranked |>
  mutate(key = paste(id_A, id_B, sep = "_")) |>
  filter(key %in% FP_keys) |>
  arrange(desc(Weight)) |>
  select(id_A, id_B, Weight)

fn_tbl <- truth |>
  mutate(key = paste(id_A, id_B, sep = "_")) |>
  filter(key %in% FN_keys) |>
  select(id_A, id_B)

write.csv(fp_tbl, "3_results/false_positives.csv", row.names = FALSE)
write.csv(fn_tbl, "3_results/false_negatives.csv", row.names = FALSE)

# --- curva ROC --------------------------------------------------------------
# Objetivo: avaliar Sensibilidade (TPR) vs Taxa de Falso-Positivo (FPR)
# a) ROC com TODOS os pares candidatos (mais pontos, visão global)
pairs_ranked$.__key <- paste(pairs_ranked$id_A, pairs_ranked$id_B, sep = "_")
y_all <- as.integer(pairs_ranked$.__key %in% truth_keys)   # 1 = par verdadeiro, 0 = não
roc_all <- roc(response = y_all, predictor = pairs_ranked$Weight, quiet = TRUE, direction = ">")

roc_all_df <- data.frame(
  threshold = roc_all$thresholds,
  TPR = roc_all$sensitivities,                 # sensibilidade = recall
  FPR = 1 - roc_all$specificities              # 1 - especificidade
)
write.csv(roc_all_df, "3_results/roc_curve_allpairs.csv", row.names = FALSE)

p_roc_all <- ggplot(roc_all_df, aes(x = FPR, y = TPR)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  ggtitle(paste0("ROC (todos os pares) — AUC = ", round(as.numeric(auc(roc_all)), 4))) +
  xlab("Taxa de falso-positivo (FPR)") + ylab("Sensibilidade (TPR)")
ggsave("3_results/roc_curve_allpairs.png", p_roc_all, width = 6, height = 5, dpi = 120)

# b) ROC no TOP-1 por id_B (reflete seu uso final: decisão única por registro B)
y_top1 <- as.integer(paste(pairs_top1$id_A, pairs_top1$id_B, sep = "_") %in% truth_keys)
roc_top1 <- roc(response = y_top1, predictor = pairs_top1$Weight, quiet = TRUE, direction = ">")

roc_top1_df <- data.frame(
  threshold = roc_top1$thresholds,
  TPR = roc_top1$sensitivities,
  FPR = 1 - roc_top1$specificities
)
write.csv(roc_top1_df, "3_results/roc_curve_top1.csv", row.names = FALSE)

p_roc_top1 <- ggplot(roc_top1_df, aes(x = FPR, y = TPR)) +
  geom_path() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  ggtitle(paste0("ROC (top-1 por id_B) — AUC = ", round(as.numeric(auc(roc_top1)), 4))) +
  xlab("Taxa de falso-positivo (FPR)") + ylab("Sensibilidade (TPR)")
ggsave("3_results/roc_curve_top1.png", p_roc_top1, width = 6, height = 5, dpi = 120)

# --- resumo no console ------------------------------------------------------
cat("\nCobertura (truth presente entre candidatos): ", sprintf("%.3f", coverage), "\n", sep = "")
cat("Métricas (top1):\n"); print(read.csv("3_results/metrics_top1.csv"))
if (!is.null(links_raw)) { cat("Métricas (epiClassify):\n"); print(read.csv("3_results/metrics_epi.csv")) }

# AUCs para consulta rápida no console
cat(sprintf("AUC-ROC (todos os pares): %.4f\n", as.numeric(auc(roc_all))))
cat(sprintf("AUC-ROC (top-1 por id_B): %.4f\n", as.numeric(auc(roc_top1))))


