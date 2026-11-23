# Dados

Os microdados **não estão incluídos neste repositório**. Eles pertencem ao
Instituto Equidade.info e devem ser obtidos diretamente com a instituição
(<https://equidade.info>), conforme seus termos de uso.

Para reproduzir as análises:

1. Obtenha o arquivo de estudantes `alunos.csv` (base geral com as ondas de coleta).
2. Coloque-o nesta pasta: `dados/alunos.csv`.
   - Alternativamente, defina a variável de ambiente `EQUIDADE_ALUNOS` com o
     caminho completo do arquivo (ver `R/00_config.R`).

O arquivo usado no artigo tem 30.252 registros e 538 colunas. Variáveis
utilizadas: `ID_ONDA`, `ID_ESCOLA`, `PESO`, `API01`, `API03`, `API05`, `API09`,
`API21`, `API22`, `API24`, `API27` a `API30`, `AHL03`, `AHL03a`, `AHL08`,
`AHL09`, `AHL10`, `CO_REGIAO`, `TIPO_CATEGORIA`, `TIPO_LOCALIZACAO` e `IN_CAPITAL`.
