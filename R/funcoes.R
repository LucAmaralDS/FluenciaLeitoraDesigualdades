# ============================================================
# FUNCOES AUXILIARES (usadas por todos os scripts de analise)
# ============================================================

# ---------- formatacao ----------
as_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
virg   <- function(x, d = 1) sub(".", ",", sprintf(paste0("%.", d, "f"), x), fixed = TRUE)
estrelas <- function(p) case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "")
fmt_p  <- function(p) ifelse(p < 0.001, "<0,001", virg(p, 3))

# ============================================================
# TABELAS E COMPARACOES
# ============================================================
meta <- tribble(
  ~termo,                     ~rotulo,                                        ~bloco,
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
  "le_casaRaramente",         "Le raramente (ref. nao le)",                   "6. Praticas de leitura em casa",
  "fluencia_nomPoucas palavras", "Fluencia: leu poucas palavras (ref. nao leu nada)", "7. Desempenho observado na tirinha",
  "fluencia_nomQuase todas",     "Fluencia: leu quase todas (ref. nao leu nada)",     "7. Desempenho observado na tirinha",
  "fluencia_nomPerfeitamente",   "Fluencia: leu perfeitamente (ref. nao leu nada)",   "7. Desempenho observado na tirinha",
  "compreensaoNao respondeu corretamente",    "Nao respondeu corretamente (ref. respondeu corretamente)", "7. Desempenho observado na tirinha",
  "compreensaoNao conseguiu ler no tempo",    "Nao conseguiu ler no tempo (ref. respondeu corretamente)", "7. Desempenho observado na tirinha",
  "compreensaoLeu, mas nao entendeu",         "Leu, mas nao entendeu (ref. respondeu corretamente)",      "7. Desempenho observado na tirinha",
  "compreensaoEntendeu, mas esqueceu depois", "Entendeu, mas esqueceu (ref. respondeu corretamente)",     "7. Desempenho observado na tirinha"
)

# Tabela do MPL em blocos (efeitos em p.p.)
tabela_lpm <- function(m, titulo) {
  cf <- summary(m)$coefficients; ic <- confint(m, df = df.residual(m))
  tab <- tibble(termo = rownames(cf), est = 100 * cf[, "Estimate"],
                lo = 100 * ic[, 1], hi = 100 * ic[, 2], p = cf[, "Pr(>|t|)"]) %>%
    filter(termo != "(Intercept)")
  fora <- setdiff(tab$termo, meta$termo)
  if (length(fora)) { cat("\n!! Coeficientes sem rotulo:\n"); print(fora) }
  res <- meta %>% inner_join(tab, by = "termo") %>%
    mutate(`Efeito (p.p.)` = paste0(virg(est), estrelas(p)),
           `IC 95%`        = paste0("[", virg(lo), "; ", virg(hi), "]"),
           `Valor-p`       = fmt_p(p)) %>%
    select(bloco, Variavel = rotulo, `Efeito (p.p.)`, `IC 95%`, `Valor-p`)
  cat("\n==========", titulo, "==========\n")
  for (b in unique(res$bloco)) {
    cat("\n", b, "\n", sep = "")
    print(as.data.frame(res %>% filter(bloco == b) %>% select(-bloco)), row.names = FALSE)
  }
  invisible(res)
}

# Previsoes fora de [0, 1]
fora_01 <- function(m, base, por = c("serie")) {
  stopifnot(nrow(base) == nobs(m))
  base$p_lpm <- as.numeric(fitted(m)); w <- base$peso_amostral
  cat("\n-- Previsoes fora de [0, 1] --\n")
  cat("Minimo:", round(min(base$p_lpm), 3), "| Maximo:", round(max(base$p_lpm), 3), "\n")
  cat("Abaixo de 0:", sum(base$p_lpm < 0), "alunos (", virg(100 * weighted.mean(base$p_lpm < 0, w)), "% ponderado )\n")
  cat("Acima de 1: ", sum(base$p_lpm > 1), "alunos (", virg(100 * weighted.mean(base$p_lpm > 1, w)), "% ponderado )\n")
  pct <- c(abaixo_0 = 100 * weighted.mean(base$p_lpm < 0, w), acima_1 = 100 * weighted.mean(base$p_lpm > 1, w))
  for (v in por) {
    cat("\nPor", v, ":\n")
    print(as.data.frame(base %>% group_by(.data[[v]]) %>%
      summarise(n = n(), abaixo_0 = sum(p_lpm < 0), acima_1 = sum(p_lpm > 1),
                prev_min = round(min(p_lpm), 3), prev_max = round(max(p_lpm), 3))), row.names = FALSE)
  }
  invisible(pct)
}

# Comparacao termo a termo: EMM do logit x MPL
pega <- function(a, nome) as.data.frame(a) %>%
  transmute(term = sub("_nom$", "", term), contrast,
            !!paste0("est_", nome) := 100 * estimate,
            !!paste0("lo_",  nome) := 100 * conf.low,
            !!paste0("hi_",  nome) := 100 * conf.high,
            !!paste0("p_",   nome) := p.value)

compara <- function(m_logit, m_lpm, titulo) {
  a_logit <- avg_slopes(m_logit, wts = TRUE, df = df.residual(m_logit))
  a_lpm   <- avg_slopes(m_lpm,   wts = TRUE, df = df.residual(m_lpm))
  comp <- pega(a_logit, "logit") %>%
    inner_join(pega(a_lpm, "lpm"), by = c("term", "contrast")) %>%
    mutate(diferenca    = est_lpm - est_logit,
           mesmo_sinal  = sign(est_lpm) == sign(est_logit),
           mesma_signif = (p_logit < 0.05) == (p_lpm < 0.05),
           sobrepos_IC  = pmax(0, pmin(hi_logit, hi_lpm) - pmax(lo_logit, lo_lpm)) / (hi_logit - lo_logit))
  if (nrow(comp) != nrow(as.data.frame(a_logit)))
    cat("\n!! ATENCAO: nem todos os termos foram pareados (", nrow(comp), "de", nrow(as.data.frame(a_logit)), ")\n")

  cat("\n\n========== COMPARACAO LOGIT (EMM) x MPL (p.p.):", titulo, "==========\n")
  print(as.data.frame(comp %>%
    transmute(variavel = term, categoria = contrast,
              EMM_logit = paste0(virg(est_logit), estrelas(p_logit)),
              IC_logit  = paste0("[", virg(lo_logit), "; ", virg(hi_logit), "]"),
              MPL       = paste0(virg(est_lpm), estrelas(p_lpm)),
              IC_MPL    = paste0("[", virg(lo_lpm), "; ", virg(hi_lpm), "]"),
              dif       = virg(diferenca), mesmo_sinal, mesma_signif)), row.names = FALSE)

  cat("\n-- Resumo da comparacao --\n")
  cat("Termos comparados:               ", nrow(comp), "\n")
  cat("Diferenca media absoluta (p.p.): ", virg(mean(abs(comp$diferenca)), 2), "\n")
  k <- which.max(abs(comp$diferenca))
  cat("Maior diferenca absoluta (p.p.): ", virg(abs(comp$diferenca[k]), 2), "(", comp$term[k], "-", comp$contrast[k], ")\n")
  cat("Termos com |dif| > 2 p.p.:       ", sum(abs(comp$diferenca) > 2), "\n")
  cat("Correlacao entre as estimativas: ", virg(cor(comp$est_logit, comp$est_lpm), 3), "\n")
  cat("Mesmo sinal:                     ", sum(comp$mesmo_sinal), "de", nrow(comp), "\n")
  cat("Mesma significancia (5%):        ", sum(comp$mesma_signif), "de", nrow(comp), "\n")

  cat("\n-- Termos que MUDAM de significancia (5%) --\n")
  muda <- comp %>% filter(!mesma_signif)
  if (nrow(muda) == 0) cat("Nenhum.\n") else
    print(as.data.frame(muda %>% transmute(term, contrast,
          EMM_logit = round(est_logit, 1), p_logit = round(p_logit, 3),
          MPL = round(est_lpm, 1), p_lpm = round(p_lpm, 3))), row.names = FALSE)

  cat("\n-- 5 maiores diferencas absolutas --\n")
  print(as.data.frame(comp %>% arrange(desc(abs(diferenca))) %>% head(5) %>%
    transmute(term, contrast, EMM_logit = round(est_logit, 1), MPL = round(est_lpm, 1),
              dif = round(diferenca, 1), sobrepos_IC = round(sobrepos_IC, 2))), row.names = FALSE)

  cat("\nProbabilidade media prevista - logit:",
      virg(100 * as.data.frame(avg_predictions(m_logit, wts = TRUE))$estimate),
      "% | MPL:", virg(100 * as.data.frame(avg_predictions(m_lpm, wts = TRUE))$estimate), "%\n")
  invisible(comp)
}
