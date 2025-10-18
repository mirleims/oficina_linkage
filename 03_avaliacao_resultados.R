# 03_avaliacao_resultados.R
# --------------------------------------------------------
# Objetivos:
# 1) Leitura dos resultados (pares e links)
# 2) Diagnósticos básicos (contagens, histograma de Weight)
# 3) Amostra da zona ambígua para revisão (clerical review)
# 4) Métricas com gabarito (TP/FP/FN e TN opcional)
# 5) Saídas finais juntando com as bases originais

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)


THRESHOLD <- 0.85  
LOWER     <- 0.77 


truth <- read_csv("1_raw_data/truth_pairs.csv",show_col_types = FALSE)  

# ===================== 1) Leitura =====================
pairs <- read_csv("3_results/linkage/pairs_ranked.csv" ,show_col_types = FALSE)
links <- read_csv("3_results/linkage/matches_links.csv", show_col_types = FALSE)
cat("\n===== DIAGNÓSTICO GERAL =====\n")
cat("Total de pares candidatos: ", nrow(pairs), "\n")
cat("Total de links decididos : ", nrow(links), "\n\n")

# ===================== 2) Diagnósticos =====================
cat("Resumo de 'Weight' (pares candidatos):\n")
print(summary(pairs$Weight))

p_hist <- ggplot(pairs, aes(Weight)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribuição de Weight (pares candidatos)",
       x = "Weight", y = "Frequência")
ggsave(file.path("3_results/hist_pesos_pairs.png"),
       p_hist, width = 7, height = 5, dpi = 120)

# ===================== 3) Amostra p/ revisão =====================
ambigua <- pairs %>%
  filter(Weight > LOWER, Weight < THRESHOLD)

clerical_n <- min(50, nrow(ambigua))
amostra <- ambigua %>% slice_sample(n = clerical_n)
write_csv(amostra,"3_results/linkage/revisao_manual.csv")
 

# ===================== 4) Métricas (com gabarito) =====================
pred_keys <- paste(links$id1, links$id2)
true_keys <- paste(truth$id_A, truth$id_B)

TP <- sum(pred_keys %in% true_keys)
FP <- length(pred_keys) - TP
FN <- sum(!true_keys %in% pred_keys)

if (file.exists("3_results/linkage/pairs_ranked.csv")) {
  cand <- read.csv("3_results/linkage/pairs_ranked.csv", stringsAsFactors = FALSE)
  cand_keys <- paste(cand$id1, cand$id2)
  TN <- sum(!(cand_keys %in% true_keys) & !(cand_keys %in% pred_keys))
} else TN <- NA_integer_

precision <- ifelse(TP+FP==0, NA, TP/(TP+FP))
recall    <- ifelse(TP+FN==0, NA, TP/(TP+FN))
f1        <- ifelse(is.na(precision)|is.na(recall)|(precision+recall)==0, NA, 2*precision*recall/(precision+recall))

metrics <- data.frame(THRESHOLD=0.85, TP, FP, FN, TN, precision, recall, f1)
write.csv(metrics, "3_results/metrics_with_truth.csv", row.names = FALSE)
print(metrics)

# ===================== 5) Saídas finais (join com bases) =====================
# Aqui vamos montar duas tabelas:
# - links_final.csv       -> todos os links aceitos (>= THRESHOLD), com dados de A e B
# - gray_links_final.csv  -> pares na zona ambígua, com dados de A e B (para revisão)
#
# Base A/B usadas: as mesmas do passo 02 (pré-processadas)
A <- read.csv("2_refined_data/dataset_A_pre.csv", stringsAsFactors = FALSE)
B <- read.csv("2_refined_data/dataset_B_pre.csv", stringsAsFactors = FALSE)
A$A_idx <- seq_len(nrow(A))  
B$B_idx <- seq_len(nrow(B))  

# ---- Links aceitos (matches_links.csv) ----
if (nrow(links) > 0) {
  links_final <- links %>%
    transmute(id1 = as.integer(id1),
              id2 = as.integer(id2),
              Weight = if ("Weight" %in% names(links)) Weight else NA_real_,
              decision = "link") %>%
    left_join(A, by = c("id1" = "A_idx")) %>%
    left_join(B, by = c("id2" = "B_idx"),
              suffix = c(".A", ".B"))
  
  write.csv(links_final,"3_results/links_final.csv", row.names = FALSE)
}
# ---- Zona cinza (ambígua) ----
if (nrow(ambigua) > 0) {
  gray_links_final <- ambigua %>%
    transmute(id1 = as.integer(id1),
              id2 = as.integer(id2),
              Weight,
              decision = "review_gray") %>%
    left_join(A, by = c("id1" = "A_idx")) %>%
    left_join(B, by = c("id2" = "B_idx"),
              suffix = c(".A", ".B"))
  
  write.csv(gray_links_final, "gray_links_final.csv", row.names = FALSE)
} 

