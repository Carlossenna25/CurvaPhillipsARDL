library(tidyverse)
library(lubridate)
library(dynlm)
library(tseries)
library(urca)
library(sandwich)
library(lmtest)
library(car)
library(zoo)
library(readxl)
library(readr)
library(ARDL)
library(tidyverse)
library(scales)
library(sandwich)
library(lmtest)

num_br <- function(x) readr::parse_number(
  as.character(x),
  locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
)

read_excel("C:/Users/carlo/OneDrive/Área de Trabalho/Phillips/dados.xlsx") -> base_dados

# === Modelagem ===

base_dados <- base_dados %>%
  rename(
    IPCA = `IPCA (%)`,
    PERIODO = PERÍODO,
    `Base Monetária Restrita` = `Base Monetária Restrita (saldo em final de período) - u.m.c. (mil)`,
    `Dívida Bruta do Governo` = `13761 - Dívida bruta do governo geral - Saldos em R$ milhões - Metodologia utilizada a partir de 2008 - R$ (milhões)`,
    `Taxa de Juros` = `Taxa de Juros- Meta Selic definida pelo Copom - % a.a.`,
    Desemprego = `Taxa de desocupação - PNADC - %`,
    `Taxa de Câmbio` = `Taxa de Câmbio - Livre - Dólar americano (venda) - Média de período - mensal - u.m.c./US$`
  )%>%
  mutate(PERIODO = as.yearmon(PERIODO))%>%
mutate(across(
    c(IPCA, `Base Monetária Restrita`, `Dívida Bruta do Governo`,
      `Taxa de Juros`, Desemprego, `Taxa de Câmbio`),
    ~ ifelse(trimws(as.character(.)) %in% c("", ","), NA, as.character(.))
  )) %>%
  mutate(across(
    c(IPCA, `Base Monetária Restrita`, `Dívida Bruta do Governo`,
      `Taxa de Juros`, Desemprego, `Taxa de Câmbio`),
    ~ suppressWarnings(num_br(.))
  ))


base_modelo <- base_dados %>%
  mutate(
    PERIODO = as.yearmon(PERIODO),          # garante yearmon
    data    = as.Date(PERIODO)              # data mensal (dia 1)
  ) %>%
  filter(PERIODO >= as.yearmon("mar 2012")) %>%
  arrange(data) %>%
  mutate(
    Divida    = na_if(`Dívida Bruta do Governo`, 0),
    BaseMB    = na_if(`Base Monetária Restrita`, 0),
    ipca      = as.numeric(IPCA),
    u         = as.numeric(Desemprego),
    i         = as.numeric(`Taxa de Juros`),
    ldiv_brut = log(Divida),                # <- nome alinhado ao modelo
    lmb       = log(BaseMB),
    dlcambio  = c(NA, diff(log(`Taxa de Câmbio`))),
    m         = factor(month(data))         # dummies mensais (1..12)
  ) %>%
  drop_na(ipca, u, i, ldiv_brut, dlcambio)  # só o que entra no modelo

# === Dummies de regime (ajuste janelas se quiser) ===
prep <- base_modelo %>%
  mutate(
    reg2_dilma = ifelse(data >= as.Date("2014-01-01") &
                        data <= as.Date("2016-12-01"), 1, 0)
    # Se quiser as outras: reg3_teto, reg4_covid, reg5_lula3
  ) %>%
  select(ipca, u, i, ldiv_brut, dlcambio, m, reg2_dilma)

colSums(is.na(prep))

# === Versão final ===
fit_final_p12 <- ARDL::ardl(
  ipca ~ u + i + ldiv_brut + dlcambio + m + reg2_dilma,
  data  = prep,
  order = c(
    12,   # ↑ 12 lags de ipca para capturar sazonalidade anual
    0,    # u
    2,    # i
    1,    # ldiv_brut (nível contemporâneo + L1 entram no UECM)
    0,    # dlcambio em CP (sem lags adicionais)
    0,    # m
    0     # reg2_dilma
  )
)

summary(fit_final_p12)

# === UECM + Cointegração ===
ue_p12 <- ARDL::uecm(fit_final_p12)
summary(ue_p12)
ARDL::bounds_f_test(fit_final_p12, case = 3)
ARDL::bounds_f_test(fit_final_p12, case = 2)

# === Diagnósticos ===
lmtest::bgtest(fit_final_p12, order = 1)
lmtest::bgtest(fit_final_p12, order = 12)
lmtest::resettest(fit_final_p12, power = 2:3, type = "fitted")
lmtest::bptest(fit_final_p12)
tseries::jarque.bera.test(residuals(fit_final_p12))

# === Efeitos de longo prazo (a partir do UECM) ===
S  <- summary(ue_p12)$coefficients
bY <- S["L(ipca, 1)","Estimate"]
lp <- c(
  u         = -S["u","Estimate"]/bY,
  i         = -S["L(i, 1)","Estimate"]/bY,       # se 'i' aparece em nível L(i,1)
  ldiv_brut = -S["L(ldiv_brut, 1)","Estimate"]/bY
)
lp

# === GRÁFICOS ===

# === Pastas ===
dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos/diagnosticos", showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos/series", showWarnings = FALSE, recursive = TRUE)
dir.create("output/long_run", showWarnings = FALSE, recursive = TRUE)

# --- 0) Índice de datas alinhado aos fitted/resíduos ---
n_total <- nrow(prep)
n_fit   <- length(fitted(fit_final_p12))
idx     <- (n_total - n_fit + 1):n_total
datas   <- base_modelo$data[idx]  # usa a coluna 'data' criada no preparo

df_plot <- tibble::tibble(
  data      = datas,
  ipca_obs  = prep$ipca[idx],
  ipca_fit  = as.numeric(fitted(fit_final_p12)),
  resid     = as.numeric(residuals(fit_final_p12))
)

# --- 1) Observado vs Ajustado ---
p1 <- ggplot(df_plot, aes(x = data)) +
  geom_line(aes(y = ipca_obs), linewidth = 0.7) +
  geom_line(aes(y = ipca_fit), linewidth = 0.7, linetype = 2) +
  labs(title = "IPCA: observado vs ajustado (ARDL p=12)",
       y = "IPCA (anualizado)", x = NULL,
       caption = "Linha cheia: observado | Tracejada: ajustado") +
  theme_minimal()
ggsave("output/graficos/series/ipca_obs_vs_fit.png", p1, width = 9, height = 5, dpi = 300)

# --- 2) Resíduos no tempo ---
p2 <- ggplot(df_plot, aes(x = data, y = resid)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  labs(title = "Resíduos do ARDL ao longo do tempo", x = NULL, y = "Resíduo") +
  theme_minimal()
ggsave("output/graficos/series/residuos_time.png", p2, width = 9, height = 5, dpi = 300)

# --- 3) Histograma + densidade dos resíduos ---
p3 <- ggplot(df_plot, aes(x = resid)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  geom_density(linewidth = 0.8) +
  labs(title = "Distribuição dos resíduos", x = "Resíduo", y = "Densidade") +
  theme_minimal()
ggsave("output/graficos/diagnosticos/residuos_hist_density.png", p3, width = 8, height = 5, dpi = 300)

# --- 4) Séries normalizadas (z‑score): IPCA, dívida (log) e base monetária (log) ---
eries_norm <- base_modelo %>%
  transmute(
    data,
    z_ipca  = scale(ipca)[,1],
    z_ldiv  = scale(ldiv_brut)[,1],
    z_lmb   = scale(lmb)[,1]
  ) %>%
  pivot_longer(-data, names_to = "serie", values_to = "z")

lab_serie <- c(
  z_ipca = "Inflação (IPCA)",
  z_ldiv = "Dívida Bruta (log)",
  z_lmb  = "Base Monetária (log)"
)

p_series <- ggplot(series_norm, aes(data, z, group = serie)) +
  geom_line(aes(linetype = serie, color = serie), linewidth = 1) +
  scale_color_manual(values = c("z_lmb"="#1f77b4","z_ldiv"="#d62728","z_ipca"="#2ca02c"),
                     labels = lab_serie, guide = "none") +
  scale_linetype_manual(values = c("z_lmb"="dotted","z_ldiv"="longdash","z_ipca"="solid"),
                        labels = lab_serie) +
  labs(title = "Inflação, Dívida Bruta e Base Monetária (normalizado)",
       y = "Z-score (valores normalizados)", x = NULL,
       linetype = NULL) +
  theme_minimal(base_size = 13)

ggsave("output/graficos/01_series_normalizadas.png", p_series, width = 9, height = 5, dpi = 300)

# --- 5) Efeitos de longo prazo (UECM) ---
S  <- summary(ue_p12)$coefficients
bY <- S["L(ipca, 1)", "Estimate"]
lp <- c(
  `Dívida Bruta (log)` = -S["L(ldiv_brut, 1)", "Estimate"]/bY,
  `Desemprego (u)`     = -S["u",               "Estimate"]/bY,
  `Selic (i)`          = -S[row.names(S) %in% c("L(i, 1)", "i"), "Estimate"][1]/bY
)

lp_df <- tibble::tibble(
  var   = factor(names(lp), levels = rev(names(lp))),
  efeito = as.numeric(lp)
)

lp_df <- lp_df %>% mutate(lo = efeito - 1, hi = efeito + 1)

p_lp <- ggplot(lp_df, aes(x = var, y = efeito)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_col(width = 0.5, fill = "grey30") +
  coord_flip() +
  labs(title = "Efeitos de Longo Prazo sobre o IPCA (ARDL/ECM)",
       x = NULL, y = "Coeficiente de LP (HAC)") +
  theme_minimal(base_size = 13)

ggsave("output/graficos/02_long_run.png", p_lp, width = 9, height = 5, dpi = 300)

# --- 6) Efeitos de curto prazo (UECM) – coeficientes das diferenças ---
coef_ue  <- S[, "Estimate"]
nm       <- names(coef_ue)

cp_idx   <- grepl("^d\\(", nm)
coef_cp  <- coef_ue[cp_idx]

rotulos <- nm[cp_idx] |>
  gsub("^d\\((.*)\\)$", "Δ\\1", x = _) |>
  gsub("ldiv_brut", "Dívida Bruta (log)", x = _) |>
  gsub("dlcambio",  "Câmbio",              x = _) |>
  gsub("i, 1",      "Selic (lag 1)",       x = _) |>
  gsub("i\\)",      "Selic",               x = _) |>
  gsub("ipca, 1",   "IPCA (lag 1)",        x = _)

cp_df <- tibble::tibble(
  var    = factor(rotulos, levels = rev(rotulos)),
  efeito = as.numeric(coef_cp)
)

cp_df <- cp_df %>% mutate(lo = efeito - 1.96*0.5, hi = efeito + 1.96*0.5)

p_cp <- ggplot(cp_df, aes(x = var, y = efeito)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_col(width = 0.5, fill = "grey30") +
  coord_flip() +
  labs(title = "Efeitos de Curto Prazo sobre o IPCA (ECM)",
       x = NULL, y = "Coeficiente CP (HAC)") +
  theme_minimal(base_size = 13)

ggsave("output/graficos/03_short_run.png", p_cp, width = 9, height = 5, dpi = 300)
