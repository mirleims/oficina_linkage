# 03_avaliacao_resultados.R 

library(dplyr)
library(readr)
library(ggplot2)

# ==== Parâmetros básicos ====
THRESHOLD <- 0.85                                   # corte para aceitar link
LOWER     <- 0.60                                   # zona cinza para revisão
OUT_DIR   <- "3_results/eval_simple"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ==== 1) Ler pares ranqueados ====
pairs <- read_csv("3_results/linkage/pairs_ranked.csv", show_col_types = FALSE)

# Se vierem colunas id1/id2 (índices), adapta para id_A/id_B:
if (all(c("id1","id2") %in% names(pairs)) && !all(c("id_A","id_B") %in% names(pairs))) {
  pairs <- pairs %>% rename(id_A = id1, id_B = id2)
}

stopifnot(all(c("id_A","id_B","Weight") %in% names(pairs)))

# ==== 2) Top-1 por id_B e decisão por corte ====
top1 <- pairs %>%
  group_by(id_B) %>%
  slice_max(Weight, n = 1, with_ties = FALSE) %>%
  ungroup()

pred_links <- top1 %>% filter(Weight >= THRESHOLD)
gray_review <- top1 %>% filter(Weight >= LOWER, Weight < THRESHOLD)

# ==== 3) Métricas ====
truth <- read_csv("1_raw_data/truth_pairs.csv", show_col_types = FALSE) %>%
    mutate(id_A = as.character(id_A), id_B = as.character(id_B))
  
pred_keys <- paste(pred_links$id_A, pred_links$id_B)
top1_keys <- paste(top1$id_A, top1$id_B)
true_keys <- paste(truth$id_A, truth$id_B)
  
TP <- sum(pred_keys %in% true_keys)
FP <- nrow(pred_links) - TP
FN <- sum(!(true_keys %in% pred_keys))
TN <- sum(!(top1_keys %in% true_keys) & !(top1_keys %in% pred_keys))
  
precision <- ifelse(TP+FP == 0, NA, TP/(TP+FP))
recall    <- ifelse(TP+FN == 0, NA, TP/(TP+FN))
f1        <- ifelse(is.na(precision)|is.na(recall)|(precision+recall)==0,
                      NA, 2*precision*recall/(precision+recall))
  
tibble(THRESHOLD, TP, FP, FN, TN, precision, recall, f1) %>%
write_csv(file.path(OUT_DIR, "metrics_with_truth.csv"))


# ==== 4) Gráficos====
# Histograma dos scores (top-1)
p1 <- ggplot(top1, aes(Weight)) +
  geom_histogram(bins = 40, na.rm = TRUE) +
  labs(title = "Distribuição de scores (top-1 por id_B)",
       x = "Weight", y = "Contagem")
ggsave(file.path(OUT_DIR, "hist_scores_top1.png"), p1, width = 6.5, height = 4, dpi = 120)


# ==== 5) Salvar saídas principais ====
write_csv(pred_links, file.path(OUT_DIR, "links_final.csv"))     # aceitos (≥ THRESHOLD)
write_csv(gray_review, file.path(OUT_DIR, "review_gray.csv"))    # revisar [LOWER, THRESHOLD)


#------------------GERANDO BASE FINAL
df_A <- read.csv("1_raw_data/dataset_A.csv", stringsAsFactors = FALSE) 
df_B <- read.csv("1_raw_data/dataset_B.csv", stringsAsFactors = FALSE)

links_final <- pred_links %>% 
  dplyr::select(id_A, id_B, Weight) %>% 
  mutate(decision = "link")

links_final_df <- links_final %>%
  dplyr::left_join(df_A, by = "id_A") %>%
  dplyr::left_join(df_B, by = "id_B", suffix = c(".A", ".B"))

write.csv(links_final_df,"3_results/links_final.csv", row.names = FALSE)


gray_review <- top1 %>%
  dplyr::filter(Weight >= LOWER, Weight < THRESHOLD) %>%
  dplyr::mutate(decision = "review_gray") %>%
  dplyr::select(id_A, id_B, Weight, decision)

gray_review_df <- gray_review %>%
  dplyr::left_join(A, by = "id_A") %>%
  dplyr::left_join(B, by = "id_B", suffix = c(".A", ".B"))

write.csv(gray_review_df,"3_results/gray_links_final.csv", row.names = FALSE)

