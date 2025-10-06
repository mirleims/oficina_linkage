# scripts/generate_data.R — gera datasets simulados com 'truth set'

set.seed(42)

# -----------------------------
# 1) Parâmetros de simulação
# -----------------------------
n <- 500  # nº de registros na base A 
prop_B_from_A <- 0.8   # fração de A que "vira" B com ruído
prop_B_new    <- 0.10  # fração adicional de B com registros novos (sem match)

# -----------------------------
# 2) Dicionários de nomes
# -----------------------------
names_base <- c(
  "Ana Maria","Maria Aparecida","João Silva","Carlos Eduardo","Marcos Paulo",
  "Lúcia Helena","Paulo Henrique","Fernanda Souza","Juliana Santos","Tiago Lima",
  "Rafael Almeida","Patrícia Moraes","André Luiz","Rodrigo Costa","Beatriz Oliveira",
  "Bruna Carvalho","Camila Pereira","Luiz Fernando","Gustavo Rocha","Roberta Nunes",
  "Felipe Araújo","Sérgio Ribeiro","Aline Gomes","Natália Pires","Fábio Martins",
  "Renata Lima","Eduardo Santos","Gabriela Almeida","Diego Moreira","Helena Castro",
  "Marcela Freitas","Priscila Figueiredo","Igor Barros","Leandro Duarte","Tatiane Mendes",
  "Carolina Brito","Letícia Andrade","Danilo Teixeira","Viviane Ramos","Maurício Souza",
  "Mário Augusto","César Augusto","Elaine Cristina","Simone Alves","Cláudio Henrique"
)

mothers_base <- c(
  "Maria de Lourdes","Ana Lúcia","Josefa Santos","Luzia Helena","Marta Cristina",
  "Elisangela Pereira","Roseli Souza","Ivone Marques","Raimunda Carvalho","Carla Moreira",
  "Sonia Regina","Rita de Cássia","Aparecida Silva","Tereza Cristina","Neide Oliveira",
  "Conceição Santos","Vera Lúcia","Ângela Maria","Elza Rocha","Francisca Nunes",
  "Helena Moura","Jurema Gonçalves","Iolanda Ribeiro","Zilda Gomes","Noêmia Pires"
)

# Capitais + grandes cidades (com acentos)
municipios <- c(
  # Capitais
  "Rio Branco","Maceió","Macapá","Manaus","Salvador","Fortaleza","Brasília","Vitória",
  "Goiânia","São Luís","Cuiabá","Campo Grande","Belo Horizonte","Belém","João Pessoa",
  "Curitiba","Recife","Teresina","Rio de Janeiro","Natal","Porto Alegre","Porto Velho",
  "Boa Vista","Florianópolis","São Paulo","Aracaju","Palmas",
  # Grandes cidades
  "Campinas","Ribeirão Preto","Guarulhos","Osasco","Sorocaba","Santos","São José dos Campos",
  "Niterói","Duque de Caxias","Nova Iguaçu","São Gonçalo","Petrópolis","Campos dos Goytacazes",
  "Feira de Santana","Vitória da Conquista","Lauro de Freitas","Camaçari","Itabuna","Ilhéus",
  "Juazeiro","Petrolina","Caruaru","Olinda","Jaboatão dos Guararapes","Paulista","Arapiraca",
  "Mossoró","Campina Grande","Contagem","Betim","Uberlândia","Uberaba","Juiz de Fora",
  "Blumenau","Joinville","Itajaí","Criciúma","Balneário Camboriú",
  "Maringá","Londrina","Foz do Iguaçu","Cascavel","Ponta Grossa",
  "Canoas","Caxias do Sul","Pelotas","Santa Maria","Passo Fundo","Chapecó",
  "Dourados","Rondonópolis","Sinop","Santarém","Marabá","Ananindeua",
  "Aparecida de Goiânia","Anápolis"
)

sexos <- c("F","M")

rand_date <- function(){
  as.Date("1970-01-01") + sample.int(as.integer(as.Date("2005-12-31")-as.Date("1970-01-01")), 1)
}

# -----------------------------
# 3) Funções de ruído
# -----------------------------
# mistura maiúsculo/minúsculo de forma realista
random_case <- function(s){
  mode <- runif(1)
  if (mode < 0.40) {
    tools::toTitleCase(tolower(s))         # Título: "Joao Silva"
  } else if (mode < 0.70) {
    toupper(s)                              # CAIXA ALTA
  } else if (mode < 0.90) {
    tolower(s)                              # caixa baixa
  } else {
    # mistura aleatória por caractere
    chars <- strsplit(tolower(s), "")[[1]]
    up <- sample(c(TRUE, FALSE), length(chars), replace = TRUE, prob = c(0.5,0.5))
    paste0(ifelse(up, toupper(chars), chars), collapse = "")
  }
}

drop_random_char <- function(s){
  if (nchar(s) <= 3) return(s)
  i <- sample.int(nchar(s),1)
  paste0(substr(s,1,i-1), substr(s,i+1,nchar(s)))
}
swap_order <- function(s){
  parts <- strsplit(s, " ")[[1]]
  if (length(parts) >= 2) paste(rev(parts), collapse = " ") else s
}
remove_accents <- function(s){
  if (requireNamespace("stringi", quietly = TRUE)) {
    stringi::stri_trans_general(s, "Latin-ASCII")
  } else {
    iconv(s, to = "ASCII//TRANSLIT")
  }
}
noise_name <- function(s){
  r <- runif(1)
  if (r < 0.20) {
    remove_accents(s)                    # perde acentos
  } else if (r < 0.45) {
    drop_random_char(s)                  # falta uma letra
  } else if (r < 0.65) {
    swap_order(s)                        # troca de ordem
  } else if (r < 0.80) {
    sub(" +", " ", gsub("[^A-Za-z ]", " ", s))  # tira pontuação
  } else {
    s
  }
}

jitter_date <- function(iso){
  d <- suppressWarnings(as.Date(as.character(iso), format = "%Y-%m-%d"))
  if (is.na(d)) return(as.character(iso))   # se não parsear, não quebra
  d <- d + sample(c(-2,-1,0,1,2), 1)
  format(d, "%Y-%m-%d")
}

rand_date <- function(n = 1L, start = "1970-01-01", end = "2005-12-31", as_char = TRUE) {
  s <- as.Date(start)
  e <- as.Date(end)
  d <- s + sample.int(as.integer(e - s) + 1L, n, replace = TRUE)
  if (isTRUE(as_char)) format(d, "%Y-%m-%d") else d
}
mun_noise <- function(m){
  r <- runif(1)
  if (r < 0.20) {
    remove_accents(m)                    # Salvador -> Salvador (sem acento; algumas não mudam)
  } else if (r < 0.30 && nchar(m) > 5) {
    substr(m, 1, nchar(m)-1)             # truncar 1 char
  } else {
    m
  }
}

# -----------------------------
# 4) Construir base A
# -----------------------------
A <- data.frame(
  id_A = seq_len(n),
  nome = sample(names_base, n, replace = TRUE),
  nome_mae = sample(mothers_base, n, replace = TRUE),
  data_nascimento = rand_date(n),
  municipio = sample(municipios, n, replace = TRUE),
  sexo = sample(sexos, n, replace = TRUE),
  ano_atendimento = sample(2010:2023, n, replace = TRUE),
  stringsAsFactors = FALSE
)

safe_as_date <- function(x){
  x <- as.character(x)
  
  # 1) ISO: YYYY-MM-DD
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  
  # 2) BR: DD/MM/YYYY
  idx <- is.na(d) & grepl("^\\d{2}/\\d{2}/\\d{4}$", x)
  if (any(idx)) d[idx] <- as.Date(x[idx], format = "%d/%m/%Y")
  
  # 3) ISO com barras: YYYY/MM/DD
  idx <- is.na(d) & grepl("^\\d{4}/\\d{2}/\\d{2}$", x)
  if (any(idx)) d[idx] <- as.Date(x[idx], format = "%Y/%m/%d")
  
  # 4) fallback: tenta conversão genérica (timestamps etc.)
  idx <- is.na(d)
  if (any(idx)) d[idx] <- suppressWarnings(as.Date(x[idx]))
  
  d
}

# aplicar casos (maiúsc/minúsc) nos NOME e MUNICÍPIO da base A
A$nome       <- vapply(A$nome, random_case, character(1))
A$nome_mae   <- vapply(A$nome_mae, random_case, character(1))
A$municipio  <- vapply(A$municipio, random_case, character(1))

A$data_nascimento[A$data_nascimento == ""] <- NA
A$data_nascimento <- ifelse(
  grepl("^\\d{4}-\\d{2}-\\d{2}$", A$data_nascimento),
  A$data_nascimento,
  format(safe_as_date(A$data_nascimento), "%Y-%m-%d")
)


# -----------------------------
# 5) Construir base B a partir de A (+ registros novos)
# -----------------------------
idx <- sample(seq_len(n), size = floor(n * prop_B_from_A))
Bcore <- A[idx, , drop = FALSE]
Bcore$id_B <- seq_len(nrow(Bcore))

# ruídos em B (nomes, data, município) + recase
Bcore$nome         <- vapply(Bcore$nome,       function(s) random_case(noise_name(s)), character(1))
Bcore$nome_mae     <- vapply(Bcore$nome_mae,   function(s) random_case(noise_name(s)), character(1))
Bcore$data_nascimento <- vapply(Bcore$data_nascimento, jitter_date, character(1))
Bcore$municipio    <- vapply(Bcore$municipio,  function(m) random_case(mun_noise(m)), character(1))
Bcore$ano_evento   <- sample(2012:2024, nrow(Bcore), replace = TRUE)

# registros novos (sem correspondência em A)
n_new <- floor(n * prop_B_new)
Bnew <- data.frame(
  id_B = (nrow(Bcore)+1):(nrow(Bcore)+n_new),
  nome = sample(names_base, n_new, replace = TRUE),
  nome_mae = sample(mothers_base, n_new, replace = TRUE),
  data_nascimento = rand_date(n_new),
  municipio = sample(municipios, n_new, replace = TRUE),
  sexo = sample(sexos, n_new, replace = TRUE),
  ano_evento = sample(2012:2024, n_new, replace = TRUE),
  stringsAsFactors = FALSE
)
Bnew$nome       <- vapply(Bnew$nome, random_case, character(1))
Bnew$nome_mae   <- vapply(Bnew$nome_mae, random_case, character(1))
Bnew$municipio  <- vapply(Bnew$municipio, random_case, character(1))

# base B final
B <- rbind(
  Bcore[, c("id_B","nome","nome_mae","data_nascimento","municipio","sexo","ano_evento")],
  Bnew
)

# -----------------------------
# 6) Truth set (id_A, id_B)
# -----------------------------
truth <- data.frame(
  id_A = A$id_A[idx],
  id_B = Bcore$id_B,
  stringsAsFactors = FALSE
)

# -----------------------------
# 7) Salvar
# -----------------------------
dir.create("1_raw_data", showWarnings = FALSE, recursive = TRUE)
write.csv(A,     "1_raw_data/dataset_A.csv",     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(B,     "1_raw_data/dataset_B.csv",     row.names = FALSE, fileEncoding = "UTF-8")
write.csv(truth, "1_raw_data/truth_pairs.csv",   row.names = FALSE, fileEncoding = "UTF-8")

message("Bases simuladas geradas em '1_raw_data/'.")
message(sprintf("A: %d linhas | B: %d linhas (%.1f%% de A + %d novos)", nrow(A), nrow(B), prop_B_from_A*100, n_new))
