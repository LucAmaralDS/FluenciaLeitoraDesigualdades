# ============================================================
# 03 - FLUENCIA LEITORA (AHL03a) - Tabela 3 e Tabela A2 do artigo
# Desfecho: leitura satisfatoria (1 = leu quase todas as palavras ou
#           leu perfeitamente; 0 = nao leu nada ou leu poucas palavras)
# Modelo principal: modelo de probabilidade linear (MPL)
#   svyglm(family = gaussian()), peso reescalonado (media 1),
#   conglomerado = escola. Coeficiente x 100 = efeito em p.p.
# Robustez: logit binario (efeitos marginais medios) - comparacao termo a termo
# N esperado: 2.219 estudantes, 140 escolas
# ============================================================
if (!exists("dados_cov")) source("R/01_preparacao.R")

rhs <- paste("onda + sexo + raca + serie + carros + banheiros +",
             "esc_mae + esc_pai + regiao + categoria + localizacao + capital + le_casa")
vars_cov <- c("onda","sexo","raca","serie","carros","banheiros","esc_mae","esc_pai",
              "regiao","categoria","localizacao","capital","le_casa")

# Amostra: estudantes que fizeram a leitura da tirinha, sem NA nas covariaveis
# (exclui os 5 estudantes com sexo "Outro", que ficam com sexo = NA)
dm_f <- dados_cov %>%
  filter(AHL03a_num %in% 1:4, AHL03_num %in% 1:5) %>%
  mutate(leu_bem = as.integer(AHL03a_num >= 3))
dm_f <- dm_f[complete.cases(dm_f[, c("leu_bem", vars_cov)]), ]
cat("\n== FLUENCIA: N =", nrow(dm_f), "(esperado 2.219) | escolas =",
    length(unique(dm_f$id_escola)), "==\n")

des_f  <- faz_desenho(dm_f)
form_f <- as.formula(paste("leu_bem ~", rhs))

m_f_mpl   <- svyglm(form_f, design = des_f, family = gaussian())
m_f_logit <- svyglm(form_f, design = des_f, family = quasibinomial())

tab_f <- tabela_lpm(m_f_mpl, "FLUENCIA (MPL) - leitura satisfatoria (p.p.)")
cat("\n-- Testes de Wald conjuntos (p) --\n")
print(round(sapply(vars_cov, function(t) regTermTest(m_f_mpl, as.formula(paste("~", t)))$p), 3))
fora_01(m_f_mpl, dm_f, por = "serie")
comp_f <- compara(m_f_logit, m_f_mpl, "FLUENCIA")

writexl::write_xlsx(list(MPL = tab_f, MPL_vs_logit = comp_f),
                    file.path(PASTA_RESULTADOS, "03_fluencia.xlsx"))
