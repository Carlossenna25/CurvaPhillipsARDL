# CurvaPhillipsARDL

Modelagem da Curva de Phillips Ampliada com abordagem ARDL/ECM para o IPCA brasileiro.

## 📌 Objetivo

Investigar os determinantes de médio/longo prazo da inflação brasileira considerando:
- Inércia inflacionária (lags da inflação)
- Fatores fiscais (dívida bruta)
- Fatores monetários (taxa de juros)
- Pressões de custo (câmbio, desemprego)
- Regimes políticos e sazonalidade

## 📦 Variáveis

| Variável       | Descrição                              |
|----------------|------------------------------------------|
| `ipca`         | Inflação anualizada (IPCA)              |
| `ldiv_brut`    | Log da dívida bruta                     |
| `dlcambio`     | Variação do log do câmbio               |
| `i`            | Taxa de juros nominal (Selic)           |
| `u`            | Taxa de desemprego                      |
| `reg2_dilma`   | Dummy para governo Dilma                |
| `m`            | Dummies mensais (sazonalidade)          |


## 📁 Estrutura do Projeto

- `data/`: dados utilizados (`dados.xlsx`)
- `modelo_phillips_ardl.R`: script principal com leitura, tratamento, ARDL, UECM e gráficos
- `output/`: gráficos gerados e efeitos estimados
- `README.md`: Documentação

## 📈 Gráficos

- **01**: Séries normalizadas: IPCA, dívida e base monetária
- **02**: Efeitos de longo prazo (UECM)
- **03**: Efeitos de curto prazo (diferenças)
- **04**: IPCA observado vs previsto

## ⚙️ Como Rodar

1. Abra o `modelo_phillips_ardl.R` no RStudio
2. Garanta que os pacotes estão instalados
3. Execute o script completo
4. Verifique os gráficos em `output/graficos/`

## 📦 Pacotes usados

```r
tidyverse, lubridate, zoo, readxl, readr,
dynlm, urca, tseries, sandwich, lmtest, ARDL

## 📊 Resultados

- Cointegração entre inflação e fundamentos
- Especificação válida (RESET)
- Erro padrão robusto (Newey-West)
- Leve autocorrelação residual no BG(12)

---

© Carlos Sena — Economista | XP Investimentos
