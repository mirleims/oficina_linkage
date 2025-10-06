# 02_linkage_probabilistico.R — comparação e pesos (Fellegi–Sunter)
# -----------------------------------------------------------------
# Objetivo:
# - rodar compare.linkage() com blocagem e comparação aproximada
# - calcular pesos, classificar por limiares
# - exportar pares ranqueados e links decididos

library(RecordLinkage)
library(dplyr)

# 1) Leitura -----------------------------------------------------------------
df_A <- read.csv("2_refined_data/dataset_A_pre.csv", stringsAsFactors = FALSE)
df_B <- read.csv("2_refined_data/dataset_B_pre.csv", stringsAsFactors = FALSE)


# 2) Definir campos para blocagem e comparação -------------------------------
cols_block  <- c("municipio_blk", "ano_nasc")                  # blocagem
cols_strcmp <- c("nome", "nome_mae", "data_nascimento")        # comparação aproximada

# Checagem de colunas
miss_A <- setdiff(c(cols_block, cols_strcmp), names(df_A))
miss_B <- setdiff(c(cols_block, cols_strcmp), names(df_B))
if (length(miss_A) > 0) stop("Base A faltando colunas: ", paste(miss_A, collapse = ", "))
if (length(miss_B) > 0) stop("Base B faltando colunas: ", paste(miss_B, collapse = ", "))

# 3) Harmonizar tipos (evita “Data sets have different format”) ---------------
to_char <- function(v) {
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  as.character(v)
}
for (cl in c(cols_block, cols_strcmp)) {
  df_A[[cl]] <- to_char(df_A[[cl]])
  df_B[[cl]] <- to_char(df_B[[cl]])
}

# Subconjunto nas MESMAS colunas e mesma ordem
common_cols <- c(cols_block, cols_strcmp)
A_use <- df_A[, common_cols]
B_use <- df_B[, common_cols]

# (Diagnóstico opcional: classes devem bater)
# print(rbind(A = sapply(A_use, class), B = sapply(B_use, class)))

# 4) Comparação com blocagem + strcmp ---------------------------------------
rp <- compare.linkage(
  A_use, B_use,
  blockfld = cols_block,
  strcmp   = cols_strcmp
)

cat("\nResumo da comparação (antes dos pesos):\n")
print(summary(rp))

# 5) Pesos, pares e classificação -------------------------------------------
rp <- epiWeights(rp)

# Pares (todos candidatos), ordena por peso
pairs_tbl <- getPairs(rp, single.rows = TRUE)
if (!is.null(pairs_tbl) && nrow(pairs_tbl) > 0) {
  pairs_tbl <- pairs_tbl[order(-pairs_tbl$Weight), ]
  write.csv(pairs_tbl, "3_results/linkage/pairs_ranked.csv", row.names = FALSE)
} else {
  warning("getPairs(rp) retornou 0 linhas (nenhum candidato).")
  write.csv(data.frame(), "3_results/linkage/pairs_ranked.csv", row.names = FALSE)
}

# Classificação por limiares
threshold_upper <- 0.85
threshold_lower <- 0.60
rp <- epiClassify(rp, threshold.upper = threshold_upper, threshold.lower = threshold_lower)

cat("\nResumo após classificação por limiares:\n")
print(summary(rp))

# 6) LINKS — compatível com várias versões ----------------------------------
# Primeiro tenta com o argumento 'show = "links"'.
links_tbl <- tryCatch(
  getPairs(rp, single.rows = TRUE, show = "links"),
  error = function(e) NULL
)

# Fallback: filtra manualmente via rp$prediction
if (is.null(links_tbl)) {
  pairs_all <- getPairs(rp, single.rows = TRUE)
  
  pred <- rp$prediction
  # normaliza 'prediction' (pode ser lógico, numérico, fator ou char)
  if (is.factor(pred))    pred <- as.character(pred)
  
  if (is.logical(pred)) {
    idx <- pred
  } else if (is.numeric(pred)) {
    idx <- (pred == 1)
  } else {
    idx <- tolower(pred) %in% c("l","link","links","match","m","1","true","matched")
  }
  
  links_tbl <- pairs_all[idx, , drop = FALSE]
}

# Exporta links (vazio se não houver)
if (!is.null(links_tbl) && nrow(links_tbl) > 0) {
  write.csv(links_tbl, "3_results/linkage/matches_links.csv", row.names = FALSE)
} else {
  warning("Nenhum link classificado acima do limiar. Arquivo salvo vazio.")
  write.csv(data.frame(),"3_results/linkage/matches_links.csv", row.names = FALSE)
}

# Salva o objeto rp para reuso
saveRDS(rp, file = file.path("3_results/linkage/rp_object.rds"))

message("✔ Comparação concluída.")
