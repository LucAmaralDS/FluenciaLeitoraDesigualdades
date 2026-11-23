# ============================================================
# 07 - COMPREENSAO (AHL03): ESPECIFICACAO ALTERNATIVA "DOIS A DOIS"
# Quatro MPL (e logits), cada um comparando uma categoria de dificuldade
# com "respondeu corretamente", so com os estudantes dessas duas categorias.
# Citada no Apendice A como alternativa nao adotada.
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

cats <- c("Leu, mas nao entendeu", "Entendeu, mas esqueceu depois",
          "Nao respondeu corretamente", "Nao conseguiu ler no tempo")
esperado <- c(1215, 1333, 1508, 1361)
termos_wald <- vars_cov

# ============================================================
# PARTE 5 - OS QUATRO MODELOS DOIS A DOIS
# ============================================================
res <- list()
for (k in seq_along(cats)) {
  j <- cats[k]
  base <- dm_c %>% filter(compreensao %in% c(ref, j)) %>%
    mutate(y = as.integer(compreensao == j))
  cat("\n\n#################################################################\n")
  cat("MODELO", k, ":", j, "(y = 1) x respondeu corretamente (y = 0)\n")
  cat("N =", nrow(base), "(esperado", esperado[k], ") | escolas =",
      length(unique(base$id_escola)), "| % com y = 1 (ponderado):",
      virg(100 * weighted.mean(base$y, base$peso_amostral)), "\n")
  cat("#################################################################\n")

  des  <- faz_desenho(base)
  form <- as.formula(paste("y ~", rhs))
  m_lpm   <- svyglm(form, design = des, family = gaussian())
  m_logit <- svyglm(form, design = des, family = quasibinomial())

  tab <- tabela_lpm(m_lpm, paste0("MPL - ", j, " x respondeu corretamente (p.p.)"))
  cat("\n-- Testes de Wald conjuntos (p) --\n")
  print(round(sapply(termos_wald, function(t) regTermTest(m_lpm, as.formula(paste("~", t)))$p), 3))
  pct  <- fora_01(m_lpm, base, por = "serie")
  comp <- compara(m_logit, m_lpm, j)

  res[[j]] <- list(tab = tab, comp = comp, n = nrow(base), pct = pct)
}

# ============================================================
# PARTE 6 - RESUMO DOS QUATRO MODELOS
# ============================================================
cat("\n\n================ RESUMO: MPL x LOGIT, BINARIOS DOIS A DOIS ================\n")
resumo <- bind_rows(lapply(names(res), function(j) {
  cp <- res[[j]]$comp
  tibble(modelo = j, N = res[[j]]$n,
         correlacao   = round(cor(cp$est_logit, cp$est_lpm), 3),
         dif_media    = round(mean(abs(cp$diferenca)), 2),
         dif_maxima   = round(max(abs(cp$diferenca)), 2),
         mesmo_sinal  = paste(sum(cp$mesmo_sinal), "/", nrow(cp)),
         mesma_signif = paste(sum(cp$mesma_signif), "/", nrow(cp)),
         pct_abaixo_0 = round(res[[j]]$pct["abaixo_0"], 1),
         pct_acima_1  = round(res[[j]]$pct["acima_1"], 1))
}))
print(as.data.frame(resumo), row.names = FALSE)

cat("\n-- Todos os termos que MUDAM de significancia (5%) --\n")
mud <- bind_rows(lapply(names(res), function(j) res[[j]]$comp %>% filter(!mesma_signif) %>%
  transmute(modelo = j, term, contrast, EMM_logit = round(est_logit, 1), p_logit = round(p_logit, 3),
            MPL = round(est_lpm, 1), p_lpm = round(p_lpm, 3))))
if (nrow(mud) == 0) cat("Nenhum.\n") else print(as.data.frame(mud), row.names = FALSE)

# ============================================================
# PARTE 7 - PAINEL DO MPL (uma coluna por modelo) - para o artigo
# ============================================================
painel <- Reduce(function(a, b) full_join(a, b, by = c("bloco", "Variavel")),
  lapply(names(res), function(j) res[[j]]$tab %>% select(bloco, Variavel, !!j := `Efeito (p.p.)`)))
painel <- meta %>% select(bloco, Variavel = rotulo) %>% inner_join(painel, by = c("bloco", "Variavel"))
cat("\n\n================ PAINEL MPL: efeito (p.p.) em cada binario dois a dois ================\n")
print(as.data.frame(painel %>% select(-bloco)), row.names = FALSE)

writexl::write_xlsx(c(list(resumo = resumo, painel = painel),
                      setNames(lapply(res, `[[`, "comp"), paste0("comp_", seq_along(res)))),
                    file.path(PASTA_RESULTADOS, "07_compreensao_dois_a_dois.xlsx"))
