# ============================================================
# Roda todas as analises, na ordem, a partir da raiz do repositorio.
# Os resultados (planilhas .xlsx) sao salvos na pasta resultados/.
# O script 06 (bootstrap do multinomial) leva alguns minutos.
# ============================================================
scripts <- c("R/02_descritivas.R",
             "R/03_fluencia.R",
             "R/04_compreensao.R",
             "R/05_percepcao.R",
             "R/06_compreensao_multinomial_bootstrap.R",
             "R/07_robustez_compreensao_dois_a_dois.R",
             "R/08_robustez_sem_pesos.R")

source("R/01_preparacao.R")
for (s in scripts) {
  cat("\n\n############################", s, "############################\n")
  source(s, echo = FALSE, print.eval = TRUE)
}

# Registro das versoes do R e dos pacotes usados
writeLines(capture.output(sessionInfo()), file.path(PASTA_RESULTADOS, "sessionInfo.txt"))
cat("\nConcluido. Resultados em:", normalizePath(PASTA_RESULTADOS), "\n")
