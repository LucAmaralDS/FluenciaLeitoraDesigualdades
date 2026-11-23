# ============================================================
# 05 - PERCEPCAO DE DIFICULDADE EM LEITURA (AHL10) - Tabela 5 e Tabela A2
# Duas etapas, cada uma estimada por MPL (e por logit, como robustez):
#   Etapa 1: N = 1 se "nao sei ler ainda"; 0 nas demais respostas (N = 2.620)
#   Etapa 2: D = 1 se "as vezes acho dificil" ou "acho dificil";
#            0 se "acho facil", so entre quem le (N = 2.219), com
#            fluencia e compreensao como covariaveis.
# No MPL a fluencia entra como indicadoras (ref. "Nao leu nada");
# no logit, como no modelo original, com contrastes polinomiais.
# ============================================================
if (!exists("dados_cov")) source("R/01_preparacao.R")

rhs_base <- paste("onda + sexo + raca + serie + carros + banheiros +",
                  "esc_mae + esc_pai + regiao + categoria + localizacao + capital")
vars_base <- c("onda","sexo","raca","serie","carros","banheiros","esc_mae","esc_pai",
               "regiao","categoria","localizacao","capital")

# ============================================================
# PARTE 1 - ETAPA 1: "NAO SEI LER AINDA"  (mesma amostra do logit)
# ============================================================
dm_p1 <- dados_cov %>%
  filter(!is.na(AHL10_num)) %>%
  mutate(nao_le_ainda = as.integer(AHL10_num == 9))
dm_p1 <- dm_p1[complete.cases(dm_p1[, c("nao_le_ainda", vars_base)]), ]
cat("\n== ETAPA 1: N =", nrow(dm_p1), "| escolas =", length(unique(dm_p1$id_escola)), "==\n")
print(dm_p1 %>% count(nao_le_ainda) %>% mutate(pct = round(100 * n / sum(n), 1)))

des1  <- faz_desenho(dm_p1)
form1 <- as.formula(paste("nao_le_ainda ~", rhs_base))

m1_lpm   <- svyglm(form1, design = des1, family = gaussian())
m1_logit <- svyglm(form1, design = des1, family = quasibinomial())

tab1 <- tabela_lpm(m1_lpm, "ETAPA 1 (MPL) - declara que ainda nao sabe ler (p.p.)")
cat("\nN (observacoes no modelo):", nobs(m1_lpm), "\n")
cat("\n-- Etapa 1 (MPL): testes de Wald conjuntos (p) --\n")
print(round(sapply(vars_base, function(t) regTermTest(m1_lpm, as.formula(paste("~", t)))$p), 3))
fora_01(m1_lpm, dm_p1, por = "serie")
comp1 <- compara(m1_logit, m1_lpm, "ETAPA 1")

# ============================================================
# PARTE 2 - ETAPA 2: PERCEBE ALGUMA DIFICULDADE (entre quem le)
# ============================================================
dm_p2 <- dados_cov %>%
  filter(!is.na(dificuldade), !is.na(fluencia), !is.na(compreensao)) %>%
  mutate(dif_bin = as.integer(as.integer(dificuldade) >= 2),
         # fluencia como fator NAO ordenado (indicadoras), ref. "Nao leu nada"
         fluencia_nom = factor(as.character(fluencia),
                               levels = c("Nao leu nada","Poucas palavras","Quase todas","Perfeitamente")))
vars_p2 <- c("dif_bin", vars_base, "le_casa", "fluencia", "compreensao")
dm_p2 <- dm_p2[complete.cases(dm_p2[, vars_p2]), ]
cat("\n\n== ETAPA 2: N =", nrow(dm_p2), "| escolas =", length(unique(dm_p2$id_escola)), "==\n")
print(dm_p2 %>% count(dif_bin) %>% mutate(pct = round(100 * n / sum(n), 1)))

des2 <- faz_desenho(dm_p2)
form2_lpm   <- as.formula(paste("dif_bin ~", rhs_base, "+ le_casa + fluencia_nom + compreensao"))
form2_logit <- as.formula(paste("dif_bin ~", rhs_base, "+ le_casa + fluencia + compreensao"))  # igual ao modelo final

m2_lpm   <- svyglm(form2_lpm,   design = des2, family = gaussian())
m2_logit <- svyglm(form2_logit, design = des2, family = quasibinomial())

tab2 <- tabela_lpm(m2_lpm, "ETAPA 2 (MPL) - percebe alguma dificuldade, entre quem le (p.p.)")
cat("\nN (observacoes no modelo):", nobs(m2_lpm), "\n")
cat("\n-- Etapa 2 (MPL): testes de Wald conjuntos (p) --\n")
print(round(sapply(c(vars_base, "le_casa", "fluencia_nom", "compreensao"),
                   function(t) regTermTest(m2_lpm, as.formula(paste("~", t)))$p), 3))
fora_01(m2_lpm, dm_p2, por = c("serie", "fluencia_nom"))
comp2 <- compara(m2_logit, m2_lpm, "ETAPA 2")

writexl::write_xlsx(list(etapa1_MPL = tab1, etapa1_comp = comp1,
                         etapa2_MPL = tab2, etapa2_comp = comp2),
                    file.path(PASTA_RESULTADOS, "05_percepcao.xlsx"))
