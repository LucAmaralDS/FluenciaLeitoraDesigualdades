# Fluência leitora e desigualdades educacionais nos anos iniciais do Ensino Fundamental no Brasil

Código de replicação do artigo **"Fluência leitora e desigualdades educacionais nos anos iniciais do Ensino Fundamental no Brasil"**.

> **Autores:** [preencher]

O artigo analisa os fatores associados a três medidas de leitura de estudantes do 1º ao 5º ano do Ensino Fundamental, com microdados do Instituto Equidade.info (ondas 6 e 9, 2024):

| Desfecho | Variável | Modelo principal | Robustez |
|---|---|---|---|
| Fluência leitora | AHL03a | Modelo de probabilidade linear (MPL) | Logit (efeitos marginais médios) |
| Compreensão leitora | AHL03 | MPL "uma categoria contra todas" (5 modelos) | Logit binário; logit multinomial com *ridge* |
| Percepção de dificuldade | AHL10 | MPL em duas etapas | Logit em duas etapas |

Todas as estimações usam o desenho amostral da pesquisa (`survey::svyglm`)

## Estrutura do repositório

```
.
├── README.md
├── LICENSE
├── run_all.R                     # roda todas as análises em ordem
├── leitura-anos-iniciais.Rproj   # projeto do RStudio
├── dados/
│   └── README.md                 # como obter a base (não incluída)
├── resultados/                   # planilhas geradas pelos scripts
└── R/
    ├── 00_config.R               # caminho da base, pacotes e opções
    ├── funcoes.R                 # funções auxiliares (tabelas e comparações)
    ├── 01_preparacao.R           # leitura, recorte e construção das variáveis
    ├── 02_descritivas.R          # distribuição dos desfechos por covariável
    ├── 03_fluencia.R             # MPL e logit – fluência
    ├── 04_compreensao.R          # MPL uma contra todas, logit e multinomial – compreensão
    ├── 05_percepcao.R            # MPL e logit em duas etapas – percepção
    ├── 06_compreensao_multinomial_bootstrap.R   # multinomial com bootstrap por escola
    ├── 07_robustez_compreensao_dois_a_dois.R    # especificação alternativa (dois a dois)
    └── 08_robustez_sem_pesos.R   # MPL sem pesos e com pesos aparados
```

## Correspondência com o artigo

| Script | Tabelas do artigo | Arquivo gerado em `resultados/` |
|---|---|---|
| `02_descritivas.R` | Descritivas dos desfechos | `02_descritivas.xlsx` |
| `03_fluencia.R` | Tabela 3; Tabelas A1 e A2 | `03_fluencia.xlsx` |
| `04_compreensao.R` | Tabela 4; Tabelas A1 e A3 | `04_compreensao.xlsx` |
| `05_percepcao.R` | Tabela 5; Tabelas A1 e A2 | `05_percepcao.xlsx` |
| `06_compreensao_multinomial_bootstrap.R` | Apêndice A (multinomial) | `06_multinomial_bootstrap.xlsx` |
| `07_robustez_compreensao_dois_a_dois.R` | Apêndice A (alternativa não adotada) | `07_compreensao_dois_a_dois.xlsx` |
| `08_robustez_sem_pesos.R` | Resultados classificados como frágeis | `08_robustez_sem_pesos.xlsx` |

## Como reproduzir

1. **Instale o R** (versão 4.3 ou superior) e, de preferência, o RStudio.
2. **Instale os pacotes:**
   ```r
   install.packages(c("readr", "dplyr", "tidyr", "stringr", "forcats", "tibble",
                      "survey", "marginaleffects", "nnet", "writexl"))
   ```
3. **Obtenha os dados** e coloque o arquivo em `dados/alunos.csv` (ver [`dados/README.md`](dados/README.md)).

O bootstrap do multinomial (script 06) usa 1.000 reamostragens e leva alguns minutos. A semente é fixa (`set.seed(2024)`), de modo que os resultados são reproduzíveis. As versões do R e dos pacotes usadas na execução ficam registradas em `resultados/sessionInfo.txt`.

## Dados

Os microdados pertencem ao Instituto Equidade.info e **não são redistribuídos** neste repositório. A pasta `dados/` está no `.gitignore`; nenhum arquivo de dados deve ser enviado ao GitHub.


O código está disponível sob a licença MIT (ver [`LICENSE`](LICENSE)). A licença não se aplica aos dados do Equidade.info.

