# Fluência leitora e desigualdades educacionais nos anos iniciais do Ensino Fundamental no Brasil

Código de replicação do artigo **"Fluência leitora e desigualdades educacionais nos anos iniciais do Ensino Fundamental no Brasil"**.

> **Autores:** Ana Caroline de Freitas Peixoto e Lucas Pereira do Amaral

O artigo analisa os fatores associados a três medidas de leitura de estudantes do 1º ao 5º ano do Ensino Fundamental, com microdados do Instituto Equidade.info (ondas 6 e 9, 2024).

## Sobre este repositório

Este repositório tem dois objetivos:

1. **Reprodutibilidade:** permitir que qualquer pessoa com acesso aos microdados reproduza todos os resultados apresentados no artigo.
2. **Aprofundamento técnico:** reunir, com mais detalhe do que o espaço do artigo permite, as estimações dos modelos logísticos binários e multinomiais e as comparações entre eles e os modelos lineares.

### Por que o artigo usa o modelo de probabilidade linear

Os três desfechos são categóricos: a fluência e a percepção de dificuldade foram analisadas como variáveis binárias, e a compreensão tem cinco categorias sem ordenação. Para essa natureza de problema, os modelos **mais adequados** do ponto de vista estatístico são o **logit binário** e o **logit multinomial**, que mantêm as probabilidades previstas entre 0 e 1 e descrevem melhor a relação entre as covariáveis e o desfecho quando as probabilidades estão próximas desses limites.

Os coeficientes desses modelos, porém, estão na escala do logaritmo da chance e precisam ser transformados em razões de chances ou em efeitos marginais para serem interpretados, e o multinomial exige ainda a leitura simultânea de quatro conjuntos de coeficientes. Como o artigo se dirige também a leitores da área de educação, **optou-se por apresentar os resultados com o modelo de probabilidade linear (MPL)**, cujos coeficientes correspondem diretamente a diferenças de probabilidade em pontos percentuais e podem ser resumidos em tabelas mais simples.

Essa escolha foi feita apenas depois de verificar que ela não altera as conclusões. Os coeficientes do MPL são muito próximos dos efeitos marginais médios dos logits (correlações acima de 0,97 e diferenças médias entre 0,6 e 1,3 ponto percentual), e as poucas diferenças de significância ocorrem em coeficientes que estão no limite dos 5% nas duas especificações. Na compreensão, os MPL também concordam com o multinomial. A principal limitação do MPL, as probabilidades previstas fora do intervalo entre 0 e 1, está documentada nos resultados dos scripts.

Assim, no artigo, o MPL é a especificação principal e os modelos logísticos aparecem como análise de robustez. **Neste repositório, os dois são estimados lado a lado**, e o leitor interessado nos modelos mais adequados à natureza dos dados encontra aqui todas as suas estimativas:

| Desfecho | Variável | No artigo (MPL) | Modelos logísticos (neste repositório) |
|---|---|---|---|
| Fluência leitora | AHL03a | MPL binário: leitura satisfatória | Logit binário (razões de chances e efeitos marginais médios) |
| Compreensão leitora | AHL03 | MPL "uma categoria contra todas" (5 modelos) | Logit binário para cada categoria; logit multinomial com regularização *ridge* e *bootstrap* por escola |
| Percepção de dificuldade | AHL10 | MPL em duas etapas | Logit binário em duas etapas |

Todas as estimações usam o desenho amostral da pesquisa (`survey::svyglm`), com pesos amostrais e erros-padrão que consideram o agrupamento dos estudantes por escola.

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

Os scripts 03 a 05 estimam, para cada desfecho, o MPL e o logit correspondente e comparam os dois termo a termo (efeito, intervalo de confiança, sinal e significância). O script 06 traz a estimação completa do multinomial, com intervalos de confiança por *bootstrap*.

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
