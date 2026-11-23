# ============================================================
# 01 - PREPARACAO DOS DADOS
# Leitura da base, recorte da amostra e construcao das covariaveis.
# Cria: dados_recorte, dados_cov, faz_desenho()
# Base: Equidade.info (alunos.csv) | ondas 6 e 9 | EF anos iniciais | 1o ao 5o ano
# Este script e chamado pelos demais; nao precisa ser rodado sozinho.
# ============================================================

source("R/00_config.R")
source("R/funcoes.R")

# ============================================================
# PARTE 1 - LEITURA E RECORTE
# ============================================================
caminho_base <- CAMINHO_BASE
if (!file.exists(caminho_base)) stop("Base nao encontrada em: ", caminho_base,
  "\nVeja dados/README.md para obter o arquivo alunos.csv.")

primeira_linha <- readLines(caminho_base, n = 1, encoding = "UTF-8")
delim_base <- ifelse(str_count(primeira_linha, ";") > str_count(primeira_linha, ","), ";", ",")
alunos <- read_delim(caminho_base, delim = delim_base,
                     col_types = cols(.default = col_character()),
                     show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
cat("Linhas lidas:", nrow(alunos), "| Colunas:", ncol(alunos), "\n")

var_peso   <- c("PESO","pesos","peso","Peso")[c("PESO","pesos","peso","Peso") %in% names(alunos)][1]
var_escola <- c("ID_ESCOLA","id_escola","CO_ESCOLA")[c("ID_ESCOLA","id_escola","CO_ESCOLA") %in% names(alunos)][1]
if (is.na(var_peso))   stop("Variavel de peso (PESO) nao encontrada.")
if (is.na(var_escola)) stop("ID_ESCOLA nao encontrada (cluster).")

alunos <- alunos %>%
  mutate(peso_amostral = as_num(.data[[var_peso]]),
         id_escola      = .data[[var_escola]])

dados_recorte <- alunos %>%
  filter(as_num(ID_ONDA) %in% c(6, 9), as_num(API01) == 1,
         as_num(API09) %in% 1:5,
         !is.na(peso_amostral), peso_amostral > 0, !is.na(id_escola)) %>%
  mutate(
    ID_ONDA_num = as_num(ID_ONDA),
    API03_num = as_num(API03),  API05_num = as_num(API05),
    API09_num = as_num(API09),  API21_num = as_num(API21),
    API22_num = as_num(API22),  API24_num = as_num(API24),
    API27_num = as_num(API27),  API28_num = as_num(API28),
    API29_num = as_num(API29),  API30_num = as_num(API30),
    AHL03_num = as_num(AHL03),  AHL03a_num = as_num(AHL03a),
    AHL08_num = as_num(AHL08),  AHL09_num = as_num(AHL09),
    AHL10_num = as_num(AHL10),
    CO_REGIAO_num        = as_num(CO_REGIAO),
    TIPO_CATEGORIA_num   = as_num(TIPO_CATEGORIA),
    TIPO_LOCALIZACAO_num = as_num(TIPO_LOCALIZACAO),
    IN_CAPITAL_num       = as_num(IN_CAPITAL)
  )

# ============================================================
# PARTE 2 - COVARIAVEIS COMUNS
# ============================================================
dados_cov <- dados_recorte %>%
  mutate(
    onda = factor(ID_ONDA_num, levels = c(6,9), labels = c("Onda 6","Onda 9")),
    fluencia = factor(AHL03a_num, levels = 1:4,
                      labels = c("Nao leu nada","Poucas palavras","Quase todas","Perfeitamente"),
                      ordered = TRUE),
    compreensao = factor(AHL03_num, levels = 1:5,
                         labels = c("Respondeu corretamente","Nao respondeu corretamente",
                                    "Nao conseguiu ler no tempo","Leu, mas nao entendeu",
                                    "Entendeu, mas esqueceu depois")),
    dificuldade = factor(AHL10_num, levels = c(0,1,2),
                         labels = c("Acho facil","As vezes dificil","Acho dificil"),
                         ordered = TRUE),
    sexo = factor(API03_num, levels = c(1,2), labels = c("Masculino","Feminino")),
    raca = fct_relevel(factor(API05_num, levels = 1:5,
                              labels = c("Branca","Preta","Parda","Amarela","Indigena")), "Branca"),
    serie = fct_relevel(factor(API09_num, levels = 1:5,
                               labels = c("1o ano","2o ano","3o ano","4o ano","5o ano")), "2o ano"),
    serie_num = API09_num,
    idade = API21_num,
    carros    = factor(API22_num, levels = 0:3, labels = c("Nenhum","1","2","3+")),
    banheiros = fct_relevel(factor(API24_num, levels = 0:3,
                                   labels = c("Nenhum","1","2","3+")), "1"),
    esc_mae = case_when(
      API28_num %in% c(1,2,3,4) ~ "Ate EF",
      API28_num %in% c(5,6)     ~ "Medio",
      API28_num %in% c(7,8,10)  ~ "Superior/Tecnico",
      API28_num %in% c(0,9)     ~ "Nao sabe/nao tem",
      is.na(API28_num) & API27_num %in% c(0,2) ~ "Nao sabe/nao tem",
      TRUE ~ "Nao informado"),
    esc_mae = fct_relevel(factor(esc_mae), "Nao sabe/nao tem"),
    esc_pai = case_when(
      API30_num %in% c(1,2,3,4) ~ "Ate EF",
      API30_num %in% c(5,6)     ~ "Medio",
      API30_num %in% c(7,8,10)  ~ "Superior/Tecnico",
      API30_num %in% c(0,9)     ~ "Nao sabe/nao tem",
      is.na(API30_num) & API29_num %in% c(0,2) ~ "Nao sabe/nao tem",
      TRUE ~ "Nao informado"),
    esc_pai = fct_relevel(factor(esc_pai), "Nao sabe/nao tem"),
    regiao = fct_relevel(factor(CO_REGIAO_num, levels = 1:5,
                                labels = c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")), "Nordeste"),
    categoria   = factor(TIPO_CATEGORIA_num, levels = c(1,2), labels = c("Publica","Privada")),
    localizacao = factor(TIPO_LOCALIZACAO_num, levels = c(1,2), labels = c("Urbana","Rural")),
    capital     = factor(IN_CAPITAL_num, levels = c(1,0), labels = c("Capital","Nao capital")),
    le_casa = factor(AHL08_num, levels = c(0,1,2,3),
                     labels = c("Nao le","Todos os dias","Algumas vezes","Raramente"))
  )

faz_desenho <- function(base) {
  base <- droplevels(base)
  base$peso_esc <- base$peso_amostral / mean(base$peso_amostral)
  svydesign(ids = ~id_escola, weights = ~peso_esc, data = base, nest = TRUE)
}

cat("Preparacao concluida:", nrow(dados_cov), "estudantes no recorte\n")
