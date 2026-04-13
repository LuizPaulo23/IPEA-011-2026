# PROCESSO SELETIVO: IPEA-011-2026

# 📊 Desigualdades na Conclusão do Ensino Médio no Brasil (2013–2019)

Este repositório contém a análise da evolução da taxa de conclusão do ensino médio entre jovens de 15 a 17 anos no Brasil, no período de 2013 a 2019, com base nos microdados da PNAD Contínua do IBGE. O foco central é a avaliação das desigualdades educacionais segundo raça/cor e região geográfica.

---

## 📂 Estrutura do repositório

.
├── db/
│   ├── pnad_data_raw.rds
│   └── rate_survey.rds
│
├── documentation/
│   └── (notas metodológicas e documentação auxiliar)
│
├── functions/
│   └── run_011_IPEA.R
│
├── plots/
│   └── (figuras geradas pela análise)
│
└── README.md


---

---

## 📊 Produtos gerados

O pipeline produz:

- taxas de conclusão do ensino médio por trimestre, raça/cor e região  
- evolução temporal das desigualdades educacionais  
- gaps raciais (Branca vs Preta/Parda)  
- gráficos de tendência e comparação entre grupos  

---

## 🚀 Como executar

A execução completa da análise é realizada pelo script principal (CLONAR E DEFINIR A PASTA db COMO DIRETÓRIO):

```r
source("functions/run_011_IPEA.R")
