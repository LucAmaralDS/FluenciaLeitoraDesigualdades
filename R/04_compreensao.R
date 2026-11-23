# ============================================================
# 04 - COMPREENSAO LEITORA (AHL03) - Tabela 4, Tabela A1 e Tabela A3 do artigo
# Cinco MPL "uma categoria contra todas", na amostra completa:
#   y_k = 1 se o estudante esta na categoria k; 0 caso contrario.
# Para cada covariavel, os efeitos das 5 categorias somam zero
# (conferido na Parte 6).
# Robustez: (a) logit binario com o mesmo desfecho (EMM);
#           (b) multinomial com ridge (decay = 0,5) - efeitos pontuais.
#           A inferencia do multinomial (bootstrap) esta no script 06.
# N esperado: 2.219 estudantes, 140 escolas
# ============================================================
if (!exists("dados_cov")) source("R/01_preparacao.R")

# ============================================================
# PARTE 4 - BASE ANALITICA DA COMPREENSAO
# Mesma amostra dos modelos de compreensao: alunos que fizeram a
# leitura da tirinha (AHL03a e AHL03 validas), sem sexo "Outro".
# ============================================================
rhs <- paste("onda + sexo + raca + serie + carros + banheiros +",
             "esc_mae + esc_pai + regiao + categoria + localizacao + capital + le_casa")
vars_cov <- c("onda","sexo","raca","serie","carros","banheiros","esc_mae","esc_pai",
              "regiao","categoria","localizacao","capital","le_casa")

dm_c <- dados_cov %>% filter(AHL03_num %in% 1:5, AHL03a_num %in% 1:4)
dm_c <- dm_c[complete.cases(dm_c[, c("compreensao", vars_cov)]), ]
cat("\n== COMPREENSAO: N =", nrow(dm_c), "(esperado 2.219) | escolas =",
    length(unique(dm_c$id_escola)), "==\n")
print(as.data.frame(dm_c %>% group_by(compreensao) %>%
  summarise(n = n(), peso = sum(peso_amostral)) %>%
  mutate(pct = round(100 * n / sum(n), 1), pct_ponderado = round(100 * peso / sum(peso), 1)) %>%
  select(-peso)), row.names = FALSE)

ref <- "Respondeu corretamente"

cats5 <- c(correta        = "Respondeu corretamente",
           nao_respondeu  = "Nao respondeu corretamente",
           tempo          = "Nao conseguiu ler no tempo",
           nao_entendeu   = "Leu, mas nao entendeu",
           esqueceu       = "Entendeu, mas esqueceu depois")
for (nm in names(cats5)) dm_c[[paste0("y_", nm)]] <- as.integer(dm_c$compreensao == cats5[[nm]])

# Um unico desenho para os cinco modelos (mesma amostra)
des <- faz_desenho(dm_c)

# ============================================================
# PARTE 5 - OS CINCO MODELOS "UMA CONTRA TODAS"
# ============================================================
res <- list()
for (nm in names(cats5)) {
  k <- cats5[[nm]]
  cat("\n\n#################################################################\n")
  cat("MODELO:", k, "(y = 1) x todas as demais categorias (y = 0)\n")
  cat("% com y = 1 (ponderado):", virg(100 * weighted.mean(dm_c[[paste0("y_", nm)]], dm_c$peso_amostral)), "\n")
  cat("#################################################################\n")

  form <- as.formula(paste0("y_", nm, " ~ ", rhs))
  m_lpm   <- svyglm(form, design = des, family = gaussian())
  m_logit <- svyglm(form, design = des, family = quasibinomial())

  tab <- tabela_lpm(m_lpm, paste0("MPL - ", k, " x demais (p.p.)"))
  cat("\n-- Testes de Wald conjuntos (p) --\n")
  print(round(sapply(vars_cov, function(t) regTermTest(m_lpm, as.formula(paste("~", t)))$p), 3))
  pct  <- fora_01(m_lpm, dm_c, por = "serie")
  comp <- compara(m_logit, m_lpm, paste(k, "x demais"))

  res[[k]] <- list(m = m_lpm, tab = tab, comp = comp, pct = pct)
}

# ============================================================
# PARTE 6 - CONFERENCIA: OS EFEITOS SOMAM ZERO?
# ============================================================
B <- sapply(res, function(r) coef(r$m))
cat("\n\n== Conferencia: soma dos coeficientes nas 5 categorias ==\n")
cat("Interceptos somam:", round(sum(B["(Intercept)", ]), 6), "(deve ser 1)\n")
cat("Maior |soma| entre os demais coeficientes:",
    signif(max(abs(rowSums(B[rownames(B) != "(Intercept)", ]))), 3), "(deve ser ~0)\n")

# ============================================================
# PARTE 7 - RESUMO E PAINEL (para o artigo)
# ============================================================
cat("\n\n================ RESUMO: MPL x LOGIT, UMA CONTRA TODAS ================\n")
resumo <- bind_rows(lapply(names(res), function(k) {
  cp <- res[[k]]$comp
  tibble(modelo = k,
         correlacao   = round(cor(cp$est_logit, cp$est_lpm), 3),
         dif_media    = round(mean(abs(cp$diferenca)), 2),
         dif_maxima   = round(max(abs(cp$diferenca)), 2),
         mesmo_sinal  = paste(sum(cp$mesmo_sinal), "/", nrow(cp)),
         mesma_signif = paste(sum(cp$mesma_signif), "/", nrow(cp)),
         pct_abaixo_0 = round(res[[k]]$pct["abaixo_0"], 1),
         pct_acima_1  = round(res[[k]]$pct["acima_1"], 1))
}))
print(as.data.frame(resumo), row.names = FALSE)

cat("\n-- Todos os termos que MUDAM de significancia (5%) --\n")
mud <- bind_rows(lapply(names(res), function(k) res[[k]]$comp %>% filter(!mesma_signif) %>%
  transmute(modelo = k, term, contrast, EMM_logit = round(est_logit, 1), p_logit = round(p_logit, 3),
            MPL = round(est_lpm, 1), p_lpm = round(p_lpm, 3))))
if (nrow(mud) == 0) cat("Nenhum.\n") else print(as.data.frame(mud), row.names = FALSE)

# Painel: uma coluna por categoria; cada linha soma zero
painel <- Reduce(function(a, b) full_join(a, b, by = c("bloco", "Variavel")),
  lapply(names(res), function(k) res[[k]]$tab %>% select(bloco, Variavel, !!k := `Efeito (p.p.)`)))
painel <- meta %>% select(bloco, Variavel = rotulo) %>% inner_join(painel, by = c("bloco", "Variavel"))
cat("\n\n================ PAINEL MPL: efeito (p.p.) na probabilidade de cada categoria ================\n")
cat("(cada linha soma zero: redistribuicao entre as categorias)\n")
print(as.data.frame(painel %>% select(-bloco)), row.names = FALSE)

# ============================================================
# PARTE 8 - COMPARACAO COM O MULTINOMIAL (robustez atual)
# Efeitos marginais medios PONTUAIS do multinomial com ridge
# (decay = 0,5), calculados por previsao contrafactual: para cada
# covariavel, todos os alunos recebem a categoria c e depois a
# referencia; a diferenca nas probabilidades previstas e ponderada
# pelo peso. Sem intervalo de confianca (o bootstrap fica no script
# do multinomial).
# ============================================================
dm_c$peso_esc <- dm_c$peso_amostral / mean(dm_c$peso_amostral)
dmn <- droplevels(dm_c)
m_mn <- multinom(as.formula(paste("compreensao ~", rhs)), data = dmn,
                 weights = peso_esc, decay = 0.5, maxit = 1000, trace = FALSE)

ame_mn <- bind_rows(lapply(vars_cov, function(v) {
  lv <- levels(dmn[[v]]); w <- dmn$peso_esc
  prev <- function(nivel) {
    nd <- dmn; nd[[v]] <- factor(nivel, levels = lv)
    colSums(predict(m_mn, newdata = nd, type = "probs") * w) / sum(w)
  }
  p_ref <- prev(lv[1])
  bind_rows(lapply(lv[-1], function(c) {
    d <- 100 * (prev(c) - p_ref)
    tibble(term = v, contrast = paste(c, "-", lv[1]), categoria = names(d), est_mn = as.numeric(d))
  }))
}))

mpl_long <- bind_rows(lapply(names(res), function(k) res[[k]]$comp %>%
  transmute(term, contrast, categoria = k, est_lpm)))
cmn <- inner_join(ame_mn, mpl_long, by = c("term", "contrast", "categoria")) %>%
  mutate(diferenca = est_lpm - est_mn)

cat("\n\n================ MPL (uma contra todas) x MULTINOMIAL COM RIDGE (p.p.) ================\n")
cat("Pares comparados:                ", nrow(cmn), "\n")
cat("Correlacao:                      ", virg(cor(cmn$est_mn, cmn$est_lpm), 3), "\n")
cat("Diferenca media absoluta (p.p.): ", virg(mean(abs(cmn$diferenca)), 2), "\n")
cat("Mesmo sinal:                     ", sum(sign(cmn$est_mn) == sign(cmn$est_lpm)), "de", nrow(cmn), "\n")
cat("Razao media |MPL| / |multinomial| (> 1 indica encolhimento pelo ridge):",
    virg(mean(abs(cmn$est_lpm)) / mean(abs(cmn$est_mn)), 2), "\n")
cat("\n-- 10 maiores diferencas --\n")
print(as.data.frame(cmn %>% arrange(desc(abs(diferenca))) %>% head(10) %>%
  mutate(across(c(est_mn, est_lpm, diferenca), ~ round(.x, 1)))), row.names = FALSE)

writexl::write_xlsx(c(list(resumo = resumo, painel = painel, vs_multinomial = cmn),
                      setNames(lapply(res, `[[`, "comp"), paste0("comp_", names(cats5)))),
                    file.path(PASTA_RESULTADOS, "04_compreensao.xlsx"))
