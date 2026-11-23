# ============================================================
# 00 - CONFIGURACAO
# Caminho da base, pasta de resultados e pacotes.
# ============================================================

# Caminho do arquivo alunos.csv do Equidade.info.
# Padrao: dados/alunos.csv (dentro do repositorio, ignorado pelo git).
# Para usar outro local sem editar o codigo, defina a variavel de ambiente
# EQUIDADE_ALUNOS, por exemplo no arquivo .Renviron:
#   EQUIDADE_ALUNOS=C:/caminho/para/alunos.csv
CAMINHO_BASE <- Sys.getenv("EQUIDADE_ALUNOS", unset = file.path("dados", "alunos.csv"))

PASTA_RESULTADOS <- "resultados"
dir.create(PASTA_RESULTADOS, showWarnings = FALSE)

pacotes <- c("readr", "dplyr", "tidyr", "stringr", "forcats", "tibble",
             "survey", "marginaleffects", "nnet", "writexl")
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando) > 0) {
  stop("Instale antes os pacotes: ", paste(faltando, collapse = ", "),
       "\n  install.packages(c(", paste0('"', faltando, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages(
  invisible(lapply(pacotes, library, character.only = TRUE))
)

options(survey.lonely.psu = "adjust")
