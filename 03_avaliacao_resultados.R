# 03_avaliacao_resultados.R 

library(dplyr)
library(readr)
library(ggplot2)
library(pROC)   # para ROC/AUC

PAIRS_F   <- "3_results/linkage/pairs_ranked.csv"  # candidatos + Weight
TRUTH_F   <- "1_raw_data/truth_pairs.csv"          # id_A,id_B (opcional)
THRESHOLD <- 0.85                                  # ajuste seu corte
OUT_DIR   <- "3_results/eval_simple"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)


pairs <- read_csv(PAIRS_F, show_col_types = FALSE)
pairs <- pairs %>% dplyr::rename(id_A = id1, id_B = id2)

# top-1 por id_B (decisão única por registro B)
top1 <- pairs %>%
  group_by(id_B) %>% slice_max(Weight, n = 1, with_ties = FALSE) %>% ungroup()

pred_links <- top1 %>% filter(Weight >= THRESHOLD)

# COM PARES VERDADEIROS -------------------------------------------
#-----------------
truth <- read_csv(TRUTH_F, show_col_types = FALSE)
true_keys <- paste(truth$id_A, truth$id_B, sep = "_")

pred_keys <- paste(pred_links$id_A, pred_links$id_B, sep = "_")
top1_keys <- paste(top1$id_A, top1$id_B, sep = "_")

TP <- sum(pred_keys %in% true_keys)
FP <- nrow(pred_links) - TP
FN <- sum(!(true_keys %in% pred_keys))
TN <- sum(!(top1_keys %in% true_keys) & !(top1_keys %in% pred_keys))  # negativos corretos no universo top-1

precision <- ifelse(TP+FP==0, NA, TP/(TP+FP))
recall    <- ifelse(TP+FN==0, NA, TP/(TP+FN))
f1        <- ifelse(is.na(precision)|is.na(recall)|(precision+recall)==0, NA, 2*precision*recall/(precision+recall))

tibble::tibble(THRESHOLD, TP, FP, FN, TN, precision, recall, f1) %>%
  readr::write_csv(file.path(OUT_DIR, "metrics_with_truth.csv"))

#------------------------------------Histogramas: score e gap (top1 – top2)
# --- Histograma de scores (top-1 por id_B) ---------------------------------
p_scores <- ggplot(top1, aes(x = Weight)) +
  geom_histogram(bins = 40, na.rm = TRUE) +
  labs(title = "Distribuição de scores (top-1 por id_B)",
       x = "Weight", y = "Contagem")

ggsave(file.path(OUT_DIR, "hist_scores_top1.png"),
       plot = p_scores, width = 6.5, height = 4, dpi = 120)

# --- Gap top1 - top2 -------------------------------------------------------
top2 <- pairs %>%
  dplyr::group_by(id_B) %>%
  dplyr::slice_max(Weight, n = 2, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(id_B) %>%
  dplyr::summarise(
    top1 = dplyr::first(sort(Weight, decreasing = TRUE)),
    top2 = ifelse(dplyr::n() >= 2, sort(Weight, decreasing = TRUE)[2], NA_real_),
    gap  = top1 - top2,
    .groups = "drop"
  )

p_gap <- ggplot(top2, aes(x = gap)) +
  geom_histogram(bins = 40, na.rm = TRUE) +
  labs(title = "Separação top1 - top2 (maiores gaps = decisão mais fácil)",
       x = "gap (top1 - top2)", y = "Contagem")

ggsave(file.path(OUT_DIR, "hist_gap_top1_top2.png"),
       plot = p_gap, width = 6.5, height = 4, dpi = 120)

#---------------CURVA ROC
y_true <- (top1_keys %in% true_keys) * 1L
roc_top1 <- roc(response = y_true, predictor = top1$Weight, direction = ">")

roc_df <- tibble::tibble(
  TPR = roc_top1$sensitivities,
  FPR = 1 - roc_top1$specificities
)

ggplot(roc_df, aes(FPR, TPR)) +
  geom_path() + geom_abline(slope = 1, intercept = 0, linetype = 2) +
  labs(title = paste0("ROC (top-1) — AUC = ", round(as.numeric(auc(roc_top1)), 4)),
       x = "FPR (1 - Especificidade)", y = "TPR (Recall)") +
  ggsave(file.path(OUT_DIR, "roc_top1.png"), width = 6, height = 5, dpi = 120)

#------------------GERANDO BASE FINAL


