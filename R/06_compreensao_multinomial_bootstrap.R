# ============================================================
# 06 - COMPREENSAO (AHL03): LOGIT MULTINOMIAL COM RIDGE - robustez
# Desfecho nominal com 5 categorias (ref. = "Respondeu corretamente").
# nnet::multinom com decay = 0,5 e pesos reescalonados (media 1).
# Inferencia: bootstrap por escola (1.000 reamostragens), IC percentilico
# e valor-p bootstrap. Semente fixa (set.seed(2024)) para reprodutibilidade.
# ATENCAO: leva alguns minutos.
# ============================================================
if (!exists("dados_cov")) source("R/01_preparacao.R")
library(nnet)

# ============================================================
# PARTE 3 - BASE ANALITICA E DESFECHO (5 categorias)
# ============================================================
# Ordem dos niveis = ordem das colunas da Tabela 7; o 1o e a referencia
dm_c5 <- dados_cov %>%
  filter(AHL03_num %in% 1:5) %>%
  mutate(compreensao5 = factor(AHL03_num, levels = c(1,4,5,2,3),
                               labels = c("Respondeu corretamente",
                                          "Leu mas nao entendeu",
                                          "Entendeu mas esqueceu",
                                          "Nao respondeu corretamente",
                                          "Nao conseguiu ler no tempo")))

rhs <- paste("onda + sexo + raca + serie + carros + banheiros +",
             "esc_mae + esc_pai + regiao + categoria + localizacao +",
             "capital + le_casa")
form <- as.formula(paste("compreensao5 ~", rhs))

vars_modelo <- c("compreensao5", "onda", "sexo", "raca", "serie", "carros", "banheiros",
                 "esc_mae", "esc_pai", "regiao", "categoria", "localizacao",
                 "capital", "le_casa")

# Exclui alunos com NA em alguma variavel do modelo ANTES do bootstrap,
# para que todas as reamostras usem a mesma base
n_antes <- nrow(dm_c5)
dados <- dm_c5[complete.cases(dm_c5[, vars_modelo]), ] %>%
  mutate(peso_esc = peso_amostral / mean(peso_amostral)) %>%
  droplevels()

escolas <- unique(dados$id_escola)
n_esc   <- length(escolas)

cat("\nAlunos com AHL03 valida:", n_antes,
    "| excluidos por NA:", n_antes - nrow(dados),
    "| N do modelo:", nrow(dados), "| Escolas:", n_esc, "\n")

# Conferencia das referencias
cat("\n== Categorias de referencia ==\n")
for (v in vars_modelo) cat(sprintf("%-13s -> %s\n", v, levels(dados[[v]])[1]))

# ============================================================
# PARTE 4 - DIAGNOSTICOS (celulas pequenas)
# ============================================================
cat("\n== Distribuicao do desfecho ==\n")
print(dados %>% count(compreensao5) %>% mutate(pct = round(100 * n / sum(n), 1)))

for (v in c("serie","raca","categoria","localizacao","sexo","regiao","le_casa")) {
  cat("\n---", v, "x compreensao5 ---\n")
  print(table(dados[[v]], dados$compreensao5))
}

# ============================================================
# PARTE 5 - MODELO NO DADO COMPLETO (estimativas pontuais)
# ============================================================
DECAY <- 0.5
ajusta <- function(df, decay = DECAY) {
  multinom(form, data = df, weights = peso_esc,
           decay = decay, maxit = 1000, trace = FALSE)
}

vetoriza <- function(fit) {
  cf <- coef(fit)
  v  <- as.vector(t(cf))
  names(v) <- paste(rep(rownames(cf), each = ncol(cf)),
                    rep(colnames(cf), times = nrow(cf)), sep = ":")
  v
}

fit0  <- ajusta(dados)
coef0 <- vetoriza(fit0)
cat("\nConvergiu (0 = sim):", fit0$convergence, "\n")

# ============================================================
# PARTE 6 - BOOTSTRAP POR ESCOLA
# ============================================================
set.seed(2024)
B <- 1000
boot_mat <- matrix(NA_real_, nrow = B, ncol = length(coef0),
                   dimnames = list(NULL, names(coef0)))
descartadas <- 0
t0 <- Sys.time()
lista_esc <- split(dados, as.character(dados$id_escola))   # alunos de cada escola

for (b in seq_len(B)) {
  esc_b <- sample(names(lista_esc), size = n_esc, replace = TRUE)
  df_b  <- bind_rows(lista_esc[esc_b])                     # todos os alunos das escolas sorteadas

  # descarta reamostra que perdeu alguma categoria do desfecho
  if (length(unique(df_b$compreensao5)) < nlevels(dados$compreensao5)) {
    descartadas <- descartadas + 1; next
  }
  fit_b <- tryCatch(ajusta(df_b), error = function(e) NULL)
  if (is.null(fit_b)) { descartadas <- descartadas + 1; next }

  cb <- vetoriza(fit_b)
  comum <- intersect(names(coef0), names(cb))
  boot_mat[b, comum] <- cb[comum]

  if (b %% 50 == 0) cat("  reamostra", b, "de", B, "\n")
}
cat("Tempo:", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min |",
    "reamostras descartadas:", descartadas, "de", B, "\n")

# ============================================================
# PARTE 7 - IC PERCENTILICO E VALOR-P BOOTSTRAP
# ============================================================
prop_neg <- colMeans(boot_mat < 0, na.rm = TRUE)
prop_pos <- colMeans(boot_mat > 0, na.rm = TRUE)
p_boot   <- pmin(1, 2 * pmin(prop_neg, prop_pos))

resumo <- tibble(
  termo  = names(coef0),
  beta   = coef0,
  ic_inf = apply(boot_mat, 2, quantile, 0.025, na.rm = TRUE),
  ic_sup = apply(boot_mat, 2, quantile, 0.975, na.rm = TRUE),
  p      = p_boot[names(coef0)]
) %>%
  mutate(categoria_desf = sub(":.*$", "", termo),
         variavel       = sub("^[^:]*:", "", termo),
         OR = exp(beta), OR_inf = exp(ic_inf), OR_sup = exp(ic_sup)) %>%
  filter(variavel != "(Intercept)")

# ============================================================
# PARTE 8 - TABELA 7 (formato da dissertacao)
# ============================================================
meta_mn <- tribble(
  ~variavel,                  ~rotulo,                                        ~bloco,
  "ondaOnda 9",               "Onda 9 (ref. Onda 6)",                         "1. Onda e caracteristicas individuais",
  "sexoFeminino",             "Sexo feminino (ref. masculino)",               "1. Onda e caracteristicas individuais",
  "racaPreta",                "Raca/cor preta (ref. branca)",                 "1. Onda e caracteristicas individuais",
  "racaParda",                "Raca/cor parda (ref. branca)",                 "1. Onda e caracteristicas individuais",
  "racaAmarela",              "Raca/cor amarela (ref. branca)",               "1. Onda e caracteristicas individuais",
  "racaIndigena",             "Raca/cor indigena (ref. branca)",              "1. Onda e caracteristicas individuais",
  "serie1o ano",              "1o ano (ref. 2o ano)",                         "2. Serie escolar",
  "serie3o ano",              "3o ano (ref. 2o ano)",                         "2. Serie escolar",
  "serie4o ano",              "4o ano (ref. 2o ano)",                         "2. Serie escolar",
  "serie5o ano",              "5o ano (ref. 2o ano)",                         "2. Serie escolar",
  "carros1",                  "1 carro (ref. nenhum)",                        "3. Condicoes domiciliares",
  "carros2",                  "2 carros (ref. nenhum)",                       "3. Condicoes domiciliares",
  "carros3+",                 "3+ carros (ref. nenhum)",                      "3. Condicoes domiciliares",
  "banheirosNenhum",          "Nenhum banheiro (ref. 1 banheiro)",            "3. Condicoes domiciliares",
  "banheiros2",               "2 banheiros (ref. 1 banheiro)",                "3. Condicoes domiciliares",
  "banheiros3+",              "3+ banheiros (ref. 1 banheiro)",               "3. Condicoes domiciliares",
  "esc_maeAte EF",            "Mae: ate EF (ref. nao sabe/nao tem)",          "4. Escolaridade dos responsaveis",
  "esc_maeMedio",             "Mae: Medio (ref. nao sabe/nao tem)",           "4. Escolaridade dos responsaveis",
  "esc_maeSuperior/Tecnico",  "Mae: Superior/Tecnico (ref. nao sabe/nao tem)","4. Escolaridade dos responsaveis",
  "esc_paiAte EF",            "Pai: ate EF (ref. nao sabe/nao tem)",          "4. Escolaridade dos responsaveis",
  "esc_paiMedio",             "Pai: Medio (ref. nao sabe/nao tem)",           "4. Escolaridade dos responsaveis",
  "esc_paiSuperior/Tecnico",  "Pai: Superior/Tecnico (ref. nao sabe/nao tem)","4. Escolaridade dos responsaveis",
  "regiaoNorte",              "Norte (ref. Nordeste)",                        "5. Caracteristicas territoriais e escolares",
  "regiaoSudeste",            "Sudeste (ref. Nordeste)",                      "5. Caracteristicas territoriais e escolares",
  "regiaoSul",                "Sul (ref. Nordeste)",                          "5. Caracteristicas territoriais e escolares",
  "regiaoCentro-Oeste",       "Centro-Oeste (ref. Nordeste)",                 "5. Caracteristicas territoriais e escolares",
  "categoriaPrivada",         "Escola privada (ref. publica)",                "5. Caracteristicas territoriais e escolares",
  "localizacaoRural",         "Localizacao rural (ref. urbana)",              "5. Caracteristicas territoriais e escolares",
  "capitalNao capital",       "Nao capital (ref. capital)",                   "5. Caracteristicas territoriais e escolares",
  "le_casaTodos os dias",     "Le todos os dias (ref. nao le)",               "6. Praticas de leitura em casa",
  "le_casaAlgumas vezes",     "Le algumas vezes (ref. nao le)",               "6. Praticas de leitura em casa",
  "le_casaRaramente",         "Le raramente (ref. nao le)",                   "6. Praticas de leitura em casa"
)

fora <- setdiff(unique(resumo$variavel), meta_mn$variavel)
if (length(fora) > 0) { cat("\n!! ATENCAO: variaveis fora da tabela:\n"); print(fora) }


celulas <- resumo %>%
  mutate(celula = paste0(virg(OR, 2), estrelas(p), " [", virg(OR_inf, 2), "; ", virg(OR_sup, 2), "]"))

tabela7 <- meta_mn %>%
  left_join(celulas %>% select(variavel, categoria_desf, celula), by = "variavel") %>%
  tidyr::pivot_wider(names_from = categoria_desf, values_from = celula) %>%
  select(bloco, Variavel = rotulo,
         `Leu mas nao entendeu`, `Entendeu mas esqueceu`,
         `Nao respondeu corretamente`, `Nao conseguiu ler no tempo`)

cat("\n== TABELA 7: OR [IC 95% bootstrap] ==\n")
for (bl in unique(tabela7$bloco)) {
  cat("\n", bl, "\n", sep = "")
  print(as.data.frame(tabela7 %>% filter(bloco == bl) %>% select(-bloco)), row.names = FALSE)
}

# Valores-p bootstrap (para conferencia e para o texto)
cat("\n== Valores-p bootstrap ==\n")
print(as.data.frame(meta_mn %>%
  left_join(resumo %>% select(variavel, categoria_desf, p), by = "variavel") %>%
  mutate(p = round(p, 3)) %>%
  tidyr::pivot_wider(names_from = categoria_desf, values_from = p) %>%
  select(-bloco)), row.names = FALSE)

writexl::write_xlsx(list(tabela_OR = tabela7, coeficientes = resumo),
                    file.path(PASTA_RESULTADOS, "06_multinomial_bootstrap.xlsx"))

# ============================================================
# PARTE 9 - ROBUSTEZ: SEM PENALIZACAO (decay = 0)
# (so estimativas pontuais, sem bootstrap)
# ============================================================
fit_sem <- ajusta(dados, decay = 0)
cat("\nModelo sem ridge convergiu (0 = sim):", fit_sem$convergence, "\n")
comp <- tibble(termo = names(coef0),
               OR_ridge = exp(coef0),
               OR_sem_ridge = exp(vetoriza(fit_sem)[names(coef0)])) %>%
  filter(!grepl("Intercept", termo)) %>%
  mutate(across(c(OR_ridge, OR_sem_ridge), ~ round(.x, 2)))
cat("\n== Robustez: com ridge vs. sem ridge ==\n")
print(as.data.frame(comp), row.names = FALSE)
