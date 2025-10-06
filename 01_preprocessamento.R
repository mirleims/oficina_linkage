# 01_preprocessamento.R — limpeza, padronização e blocagem
# ---------------------------------------------------------
# Objetivo: preparar as bases A e B para o linkage probabilístico.
# - harmonização dos dados (remove acentos, caixa alta, espaços)
# - padroniza datas
# - cria variáveis de blocagem (municipio_blk, ano_nasc)
# - salva versões "pre" em 2_refined_data/

# ---------------------------------------------------------
# Carregando os pacotes 
library(janitor)  # limpeza e checagem de dados
library(stringr)  # manipulação de string
library(abjutils) # normalizar texto (ex.: remover acentos)

# ----------------------------
# 1) Carregar dados
# ----------------------------
df_A <- read.csv("1_raw_data/dataset_A.csv", stringsAsFactors = FALSE) 
df_B <- read.csv("1_raw_data/dataset_B.csv", stringsAsFactors = FALSE)

# Visualização das colunas de cada dataset
cat("Colunas dataset A: ", names(df_A))
cat("Colunas dataset B: ", names(df_B))

message("Prévia A:"); print(utils::head(df_A[, c("nome","nome_mae","data_nascimento","municipio")], 5))
message("Prévia B:"); print(utils::head(df_B[, c("nome","nome_mae","data_nascimento","municipio")], 5))

# Campos mínimos esperados:
req_cols <- c("nome", "nome_mae", "data_nascimento", "municipio")
miss_A <- setdiff(req_cols, names(df_A))
miss_B <- setdiff(req_cols, names(df_B))
if (length(miss_A) > 0) stop("Base A faltando colunas: ", paste(miss_A, collapse = ", "))
if (length(miss_B) > 0) stop("Base B faltando colunas: ", paste(miss_B, collapse = ", "))

# ----------------------------------------------------
# 2) Funções auxiliares (tutorial: entenda e reutilize)
# ----------------------------------------------------

# 2.1) Harmonização dos dados
harmonizacao <- function(x, rm_punct = FALSE, rm_stopwords = FALSE, empty_as_na = TRUE) {
  x <- as.character(x)
  
  # remover acentos: abjutils -> stringi -> iconv
  if (requireNamespace("abjutils", quietly = TRUE)) {
    x <- abjutils::rm_accent(x)
  } else if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  } else {
    x <- iconv(x, to = "ASCII//TRANSLIT")
  }
  
  # caixa alta + trims/squish
  x <- toupper(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_squish(x)
  
  # remover pontuação (mantém A–Z, 0–9 e espaço)
  if (isTRUE(rm_punct)) {
    x <- stringr::str_replace_all(x, "[^A-Z0-9 ]", " ")
    x <- stringr::str_squish(x)
  }
  
  # remover preposições comuns (útil para blocking por município/nome)
  if (isTRUE(rm_stopwords)) {
    x <- stringr::str_replace_all(x, "\\b(DO|DA|DOS|DAS|DE|DI|DU)\\b", " ")
    x <- stringr::str_squish(x)
  }
  
  if (isTRUE(empty_as_na)) x[nchar(x) == 0] <- NA_character_
  x
}

# 2.2) Datas: conversão segura tentando formatos comuns
parse_date_safe <- function(x) {
  x <- as.character(x)
  # tenta ISO (YYYY-MM-DD) / default
  d <- suppressWarnings(as.Date(x))
  # tenta BR (DD/MM/YYYY)
  idx <- is.na(d) & grepl("^\\d{2}/\\d{2}/\\d{4}$", x)
  if (any(idx)) d[idx] <- as.Date(x[idx], format = "%d/%m/%Y")
  # tenta US (MM/DD/YYYY)
  idx <- is.na(d) & grepl("^\\d{2}/\\d{2}/\\d{4}$", x)
  if (any(idx)) d[idx] <- as.Date(x[idx], format = "%m/%d/%Y")
  d
}

# ----------------------------------------------------
# 3) Limpeza e padronização (NOMES + DATAS)
# ----------------------------------------------------

# Faça backup dos originais (didático, para comparação futura):
df_A$nome_original      <- df_A$nome
df_A$nome_mae_original  <- df_A$nome_mae
df_B$nome_original      <- df_B$nome
df_B$nome_mae_original  <- df_B$nome_mae

# Normalização textual (sem remover pontuação por padrão)
df_A$nome     <- harmonizacao(df_A$nome)
df_A$nome_mae <- harmonizacao(df_A$nome_mae)
df_B$nome     <- harmonizacao(df_B$nome)
df_B$nome_mae <- harmonizacao(df_B$nome_mae)

# Datas como Date (com parse seguro)
df_A$data_nascimento <- parse_date_safe(df_A$data_nascimento)
df_B$data_nascimento <- parse_date_safe(df_B$data_nascimento)

# (Boa prática) avise se houve muitas NAs após parsing de datas:
naA <- sum(is.na(df_A$data_nascimento))
naB <- sum(is.na(df_B$data_nascimento))
if (naA > 0) message("Atenção: Base A tem ", naA, " datas não parseadas.")
if (naB > 0) message("Atenção: Base B tem ", naB, " datas não parseadas.")

# ----------------------------------------------------
# 4) Variáveis de blocagem (municipio_blk, ano_nasc)
# ----------------------------------------------------

# município normalizado (aqui aproveitamos para remover preposições)
df_A$municipio_blk <- harmonizacao(df_A$municipio, rm_stopwords = TRUE)
df_B$municipio_blk <- harmonizacao(df_B$municipio, rm_stopwords = TRUE)

# ano de nascimento (como texto, útil para blocking)
df_A$ano_nasc <- format(df_A$data_nascimento, "%Y")
df_B$ano_nasc <- format(df_B$data_nascimento, "%Y")

# ----------------------------------------------------
# 5) Diagnóstico rápido
# ----------------------------------------------------
message("Prévia A:"); print(utils::head(df_A[, c("nome","nome_mae","data_nascimento","municipio_blk","ano_nasc")], 5))
message("Prévia B:"); print(utils::head(df_B[, c("nome","nome_mae","data_nascimento","municipio_blk","ano_nasc")], 5))

# ----------------------------------------------------
# 6) Salvar versões preprocessadas
# ----------------------------------------------------
write.csv(df_A, "2_refined_data/dataset_A_pre.csv", row.names = FALSE)
write.csv(df_B, "2_refined_data/dataset_B_pre.csv", row.names = FALSE)

message("Pré-processamento concluído.")

