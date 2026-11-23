# ============================================================
# 02 - ESTATISTICAS DESCRITIVAS DOS DESFECHOS
#   A  - distribuicao geral dos tres desfechos (N e % ponderado)
#   B1 - fluencia (AHL03a, 4 niveis) por covariavel
#   B2 - compreensao (AHL03, 5 categorias) por covariavel
#   B3 - percepcao (AHL10, 4 respostas) por covariavel
# Amostras: 2.224 estudantes que fizeram a leitura (fluencia e
# compreensao, inclui os 5 com sexo "Outro") e 2.625 com AHL10 valida.
# ============================================================
if (!exists("dados_cov")) source("R/01_preparacao.R")


# ------------------------------------------------------------
# Variaveis para a descricao (com "Outro" no sexo, idade em faixas
# e banheiros agrupados como alternativa mais compacta)
# ------------------------------------------------------------
desc <- dados_cov %>%
  mutate(
    sexo_d = factor(case_when(API03_num == 1 ~ "Masculino",
                              API03_num == 2 ~ "Feminino",
                              !is.na(API03_num) ~ "Outro"),
                    levels = c("Masculino", "Feminino", "Outro")),
    idade_d = factor(case_when(idade <= 6 ~ "Até 6 anos", idade == 7 ~ "7 anos",
                               idade == 8 ~ "8 anos", idade == 9 ~ "9 anos",
                               idade == 10 ~ "10 anos", idade >= 11 ~ "11 anos ou mais"),
                     levels = c("Até 6 anos", "7 anos", "8 anos", "9 anos",
                                "10 anos", "11 anos ou mais")),
    banheiros_ag = factor(case_when(banheiros == "Nenhum" ~ "Nenhum",
                                    banheiros == "1" ~ "1",
                                    banheiros %in% c("2", "3+") ~ "2 ou mais"),
                          levels = c("Nenhum", "1", "2 ou mais")),
    serie_d = factor(serie, levels = c("1o ano", "2o ano", "3o ano", "4o ano", "5o ano")),
    banheiros_d = factor(banheiros, levels = c("Nenhum", "1", "2", "3+")),
    regiao_d = factor(regiao, levels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
  )

vars <- c("onda", "sexo_d", "raca", "idade_d", "serie_d", "carros", "banheiros_d", "banheiros_ag",
          "esc_mae", "esc_pai", "regiao_d", "categoria", "localizacao", "capital", "le_casa")
rotulos <- c(onda = "Onda", sexo_d = "Sexo", raca = "Raça/cor", idade_d = "Idade",
             serie_d = "Série", carros = "Carros no domicílio",
             banheiros_d = "Banheiros no domicílio",
             banheiros_ag = "Banheiros no domicílio (agrupado)",
             esc_mae = "Escolaridade da mãe", esc_pai = "Escolaridade do pai",
             regiao_d = "Região", categoria = "Rede", localizacao = "Localização da escola",
             capital = "Escola na capital", le_casa = "Lê livros em casa")

# ------------------------------------------------------------
# Amostras
# ------------------------------------------------------------
base_fc <- desc %>% filter(AHL03a_num %in% 1:4, AHL03_num %in% 1:5) %>%
  mutate(fluencia4 = factor(AHL03a_num, levels = 1:4,
                            labels = c("Não leu nada", "Leu poucas palavras",
                                       "Leu quase todas as palavras", "Leu perfeitamente")),
         fluencia_bin = factor(if_else(AHL03a_num >= 3, "Satisfatória", "Insatisfatória"),
                               levels = c("Insatisfatória", "Satisfatória")),
         compreensao5 = factor(AHL03_num, levels = c(1, 4, 5, 2, 3),
                               labels = c("Respondeu corretamente", "Leu, mas não entendeu",
                                          "Entendeu, mas esqueceu", "Não respondeu corretamente",
                                          "Não conseguiu ler no tempo")))
base_p <- desc %>% filter(AHL10_num %in% c(0, 1, 2, 9)) %>%
  mutate(percepcao = factor(AHL10_num, levels = c(0, 1, 2, 9),
                            labels = c("Acho fácil", "Às vezes acho difícil",
                                       "Acho difícil", "Não sei ler ainda")))
cat("N fluência/compreensão:", nrow(base_fc), "(esperado 2.224) | N percepção:",
    nrow(base_p), "(esperado 2.625)\n")

des_fc <- faz_desenho(base_fc)
des_p  <- faz_desenho(base_p)

# ------------------------------------------------------------
# Tabela A - distribuicao geral dos desfechos
# ------------------------------------------------------------
dist_y <- function(des, base, y, nome) {
  pct <- 100 * prop.table(svytable(as.formula(paste("~", y)), des))
  n   <- table(base[[y]])
  tibble(Desfecho = nome, Categoria = names(pct),
         N = as.integer(n[names(pct)]), `% ponderado` = virg(as.numeric(pct)))
}
tabA <- bind_rows(
  dist_y(des_fc, base_fc, "fluencia4",    "Fluência leitora (AHL03a)"),
  dist_y(des_fc, base_fc, "fluencia_bin", "Fluência – desfecho binário do modelo"),
  dist_y(des_fc, base_fc, "compreensao5", "Compreensão leitora (AHL03)"),
  dist_y(des_p,  base_p,  "percepcao",    "Percepção de dificuldade (AHL10)"))
cat("\n== TABELA A: distribuição dos desfechos ==\n"); print(as.data.frame(tabA), row.names = FALSE)

# ------------------------------------------------------------
# Tabelas B - desfecho por covariavel
# ------------------------------------------------------------
por_y <- function(des, base, y, vs) {
  tot <- 100 * prop.table(svytable(as.formula(paste("~", y)), des))
  linha1 <- tibble(Variável = "Total", Categoria = "", N = format(nrow(base), big.mark = ".")) %>%
    bind_cols(as_tibble(as.list(setNames(virg(as.numeric(tot)), names(tot)))))
  resto <- bind_rows(lapply(vs, function(v) {
    tt  <- svytable(as.formula(paste("~", v, "+", y)), des)
    lin <- 100 * prop.table(tt, 1)                     # % do desfecho em cada categoria
    nn  <- table(base[[v]])
    out <- as.data.frame.matrix(lin) %>% mutate(across(everything(), virg))
    tibble(Variável = c(rotulos[v], rep("", nrow(out) - 1)),
           Categoria = rownames(out), N = as.character(nn[rownames(out)])) %>%
      bind_cols(out)
  }))
  bind_rows(linha1, resto)
}
tabB1 <- por_y(des_fc, base_fc, "fluencia4",    vars)
tabB2 <- por_y(des_fc, base_fc, "compreensao5", vars)
tabB3 <- por_y(des_p,  base_p,  "percepcao",    vars)

cat("\n== TABELA B1: fluência por covariável ==\n");    print(as.data.frame(tabB1), row.names = FALSE)
cat("\n== TABELA B2: compreensão por covariável ==\n"); print(as.data.frame(tabB2), row.names = FALSE)
cat("\n== TABELA B3: percepção por covariável ==\n");   print(as.data.frame(tabB3), row.names = FALSE)

write_xlsx(list(A_desfechos = tabA, B1_fluencia = tabB1,
                B2_compreensao = tabB2, B3_percepcao = tabB3),
           file.path(PASTA_RESULTADOS, "02_descritivas.xlsx"))
cat("\nArquivo salvo em:", file.path(getwd(), file.path(PASTA_RESULTADOS, "02_descritivas.xlsx")), "\n")
