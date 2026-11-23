# ============================================================
# 08 - ROBUSTEZ DOS MPL AOS PESOS AMOSTRAIS
# Para cada MPL do artigo, compara o modelo com pesos (principal) com:
#   (a) o modelo SEM pesos (mantendo o conglomerado por escola);
#   (b) o modelo com pesos APARADOS no percentil 95.
# Um coeficiente significativo (5%) no modelo principal e marcado como
# FRAGIL se perder a significancia ou trocar de sinal em (a) ou (b).
# ============================================================
if (!exists("dados_cov")) source("R/01_preparacao.R")

rhs_base <- paste("onda + sexo + raca + serie + carros + banheiros +",
                  "esc_mae + esc_pai + regiao + categoria + localizacao + capital")
rhs      <- paste(rhs_base, "+ le_casa")
vars_base <- c("onda","sexo","raca","serie","carros","banheiros","esc_mae","esc_pai",
               "regiao","categoria","localizacao","capital")
vars_cov  <- c(vars_base, "le_casa")

desenho_sem_peso <- function(base)
  svydesign(ids = ~id_escola, weights = ~1, data = droplevels(base))
apara <- function(base, q = 0.95) {
  lim <- quantile(base$peso_amostral, q)
  base$peso_amostral <- pmin(base$peso_amostral, lim); base
}
coefs <- function(m) {
  cf <- summary(m)$coefficients
  tibble(termo = rownames(cf), est = 100 * cf[, "Estimate"], p = cf[, "Pr(>|t|)"]) %>%
    filter(termo != "(Intercept)")
}
testa <- function(nome, base, form) {
  m0   <- svyglm(form, design = faz_desenho(base),        family = gaussian())
  m_sp <- svyglm(form, design = desenho_sem_peso(base),   family = gaussian())
  m_ap <- svyglm(form, design = faz_desenho(apara(base)), family = gaussian())
  coefs(m0) %>%
    left_join(coefs(m_sp) %>% rename(est_sem_peso = est, p_sem_peso = p), by = "termo") %>%
    left_join(coefs(m_ap) %>% rename(est_aparado  = est, p_aparado  = p), by = "termo") %>%
    mutate(modelo = nome,
           fragil = p < 0.05 & (p_sem_peso >= 0.05 | p_aparado >= 0.05 |
                                sign(est) != sign(est_sem_peso) | sign(est) != sign(est_aparado))) %>%
    select(modelo, termo, est, p, est_sem_peso, p_sem_peso, est_aparado, p_aparado, fragil)
}

# ---------- bases (as mesmas dos scripts 03 a 05) ----------
dm_f <- dados_cov %>% filter(AHL03a_num %in% 1:4, AHL03_num %in% 1:5) %>%
  mutate(leu_bem = as.integer(AHL03a_num >= 3))
dm_f <- dm_f[complete.cases(dm_f[, c("leu_bem", "compreensao", vars_cov)]), ]

cats5 <- c(correta = "Respondeu corretamente", nao_respondeu = "Nao respondeu corretamente",
           tempo = "Nao conseguiu ler no tempo", nao_entendeu = "Leu, mas nao entendeu",
           esqueceu = "Entendeu, mas esqueceu depois")
for (nm in names(cats5)) dm_f[[paste0("y_", nm)]] <- as.integer(dm_f$compreensao == cats5[[nm]])

dm_p1 <- dados_cov %>% filter(!is.na(AHL10_num)) %>%
  mutate(nao_le_ainda = as.integer(AHL10_num == 9))
dm_p1 <- dm_p1[complete.cases(dm_p1[, c("nao_le_ainda", vars_base)]), ]

dm_p2 <- dados_cov %>% filter(!is.na(dificuldade), !is.na(fluencia), !is.na(compreensao)) %>%
  mutate(dif_bin = as.integer(as.integer(dificuldade) >= 2),
         fluencia_nom = factor(as.character(fluencia),
                               levels = c("Nao leu nada","Poucas palavras","Quase todas","Perfeitamente")))
dm_p2 <- dm_p2[complete.cases(dm_p2[, c("dif_bin", vars_cov, "fluencia", "compreensao")]), ]

cat("N: fluencia/compreensao =", nrow(dm_f), "| percepcao etapa 1 =", nrow(dm_p1),
    "| etapa 2 =", nrow(dm_p2), "\n")

# ---------- estimacao ----------
res <- bind_rows(
  testa("Fluencia", dm_f, as.formula(paste("leu_bem ~", rhs))),
  bind_rows(lapply(names(cats5), function(nm)
    testa(paste("Compreensao:", cats5[[nm]]), dm_f, as.formula(paste0("y_", nm, " ~ ", rhs))))),
  testa("Percepcao etapa 1", dm_p1, as.formula(paste("nao_le_ainda ~", rhs_base))),
  testa("Percepcao etapa 2", dm_p2, as.formula(paste("dif_bin ~", rhs, "+ fluencia_nom + compreensao")))
)

cat("\n================ COEFICIENTES SIGNIFICATIVOS (5%) NO MODELO PRINCIPAL ================\n")
print(as.data.frame(res %>% filter(p < 0.05) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

cat("\n================ RESULTADOS FRAGEIS (perdem significancia ou trocam de sinal) ================\n")
frageis <- res %>% filter(fragil)
if (nrow(frageis) == 0) cat("Nenhum.\n") else
  print(as.data.frame(frageis %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

writexl::write_xlsx(list(todos = res, frageis = frageis),
                    file.path(PASTA_RESULTADOS, "08_robustez_sem_pesos.xlsx"))
