# ============================================================================
# 04_TABELAS_FIGURAS — tabelas (PDF) e figuras do artigo
# Requer: objetos salvos pelo script 03.
# ============================================================================

library(tidyverse)
library(fixest)
library(ggpubr)

modelos <- readRDS("resultados/modelos.rds")
list2env(modelos, envir = .GlobalEnv)
adm  <- readRDS("dados/adm_final.rds")
comp <- readRDS("dados/comp_final.rds")

# ---- helpers ---------------------------------------------------------------
stars <- function(p) case_when(p < .001 ~ "***", p < .01 ~ "**",
                               p < .05 ~ "*", TRUE ~ "")
cel <- function(m, termo) {
  ct <- coeftable(m)
  if (!termo %in% rownames(ct)) return("")
  paste0(sprintf("%.3f", ct[termo, 1]), stars(ct[termo, 4]),
         "\n(", sprintf("%.3f", ct[termo, 2]), ")")
}
salvar_tab <- function(df, arquivo, titulo, nota = NULL,
                       largura = 7, altura = NULL) {
  tab <- ggtexttable(df, rows = NULL,
                     theme = ttheme("blank", base_size = 9)) %>%
    tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 1.2) %>%
    tab_add_hline(at.row = nrow(df) + 1, row.side = "bottom",
                  linewidth = 1.2) %>%
    tab_add_title(titulo, face = "bold", size = 10)
  if (!is.null(nota))
    tab <- tab %>% tab_add_footnote(nota, size = 7, face = "italic")
  if (is.null(altura)) altura <- 0.35 * nrow(df) + 1.2
  ggsave(arquivo, tab, width = largura, height = altura)
}
comp_tab <- function(dados, rotulo) {
  dados %>%
    filter(tipo_mov == "admissao", !is.na(bc)) %>%
    count(polo, bc) %>% group_by(polo) %>%
    mutate(pct = 100 * n / sum(n)) %>% ungroup() %>%
    transmute(polo, bc, valor = sprintf("%.2f", pct)) %>%
    pivot_wider(names_from = polo, values_from = valor) %>%
    mutate(Amostra = rotulo, .before = 1)
}

# ---- Tabela 1: composição por BC e território ------------------------------
t1 <- bind_rows(
  comp_tab(comp, "Setor (29.1–29.4)"),
  comp_tab(filter(comp, grupo_cnae %in% c(291, 292)),
           "Montadoras (29.1–29.2)")
) %>%
  rename(BC = bc, `Núcleo` = nucleo, `Desconc.` = descon, Demais = demais) %>%
  select(Amostra, BC, `Núcleo`, `Desconc.`, Demais)

salvar_tab(t1, "resultados/tabela1.pdf",
  "Tabela 1 – Composição das admissões por Base de Conhecimento e território (%), 2021–2025",
  "Fonte: elaboração própria com microdados do Novo Caged.")

# ---- Tabela 2: mincerianas (M1, M2, GG2) -----------------------------------
termo_gg2 <- 'i(factor_var = bc, ref = "Sintética")'
t2 <- tribble(
  ~Variável, ~M1, ~M2, ~`GG2 (superior)`,
  "BC Analítica", cel(m1, "bcAnalítica"), cel(m2, "bcAnalítica"),
    cel(m_gg2, termo_gg2),
  "BC Sintética", cel(m1, "bcSintética"), cel(m2, "bcSintética"), "(ref.)",
  "BC Simbólica", cel(m1, "bcSimbólica"), cel(m2, "bcSimbólica"), "—",
  "Controles", "Sim", "Sim", "Sim",
  "EF subclasse, ano-mês", "Sim", "Sim", "Sim",
  "EF município", "Não", "Sim", "Sim",
  "Observações", format(nobs(m1), big.mark = "."),
    format(nobs(m2), big.mark = "."), format(nobs(m_gg2), big.mark = "."),
  "R² (within)", sprintf("%.3f", fitstat(m1, "wr2")[[1]]),
    sprintf("%.3f", fitstat(m2, "wr2")[[1]]),
    sprintf("%.3f", fitstat(m_gg2, "wr2")[[1]])
)
salvar_tab(t2, "resultados/tabela2.pdf",
  "Tabela 2 – Prêmios salariais das Bases de Conhecimento (dep.: ln do salário real de admissão)",
  paste0("Referência: ocupações não classificadas (M1, M2); BC sintética (GG2, grande grupo 2 da CBO).\n",
         "Controles: idade, idade², sexo, raça/cor, grau de instrução. EP entre parênteses, agrupados por município.\n",
         "Significância: *** p<0,001; ** p<0,01; * p<0,05."))

# ---- Tabela 3: interações BC x território com IC 95% -----------------------
t3 <- broom::tidy(m4_def, conf.int = TRUE) %>%
  filter(term %in% c("ana_descon", "sin_descon", "sim_descon",
                     "ana_demais", "sin_demais", "sim_demais")) %>%
  mutate(
    Interação = recode(term,
      ana_descon = "Analítica × Desconcentração",
      sin_descon = "Sintética × Desconcentração",
      sim_descon = "Simbólica × Desconcentração",
      ana_demais = "Analítica × Demais",
      sin_demais = "Sintética × Demais",
      sim_demais = "Simbólica × Demais"),
    Coeficiente = paste0(sprintf("%.3f", estimate), stars(p.value)),
    EP = sprintf("(%.3f)", std.error),
    `IC 95%` = sprintf("[%.3f; %.3f]", conf.low, conf.high)
  ) %>%
  select(Interação, Coeficiente, EP, `IC 95%`)
salvar_tab(t3, "resultados/tabela3.pdf",
  "Tabela 3 – Diferenciais de prêmio por território (referência: núcleo histórico)",
  "Especificação com EF de subclasse, ano-mês e município. EP agrupados por município.")

# ---- Tabela 4: entropia, saldos e razão de concentração --------------------
entro <- comp %>%
  filter(tipo_mov == "admissao", bc != "Não classificada", !is.na(bc)) %>%
  count(polo, bc) %>% group_by(polo) %>%
  mutate(p = n / sum(n)) %>%
  summarise(entropia = -sum(p * log(p)), .groups = "drop")
sal_bc <- comp %>% filter(!is.na(bc)) %>%
  group_by(polo, bc) %>%
  summarise(s = sum(saldomovimentação), .groups = "drop") %>%
  pivot_wider(names_from = bc, values_from = s)
raz <- comp %>% filter(tipo_mov == "admissao") %>%
  count(polo, name = "adm_n") %>%
  left_join(comp %>% filter(bc == "Analítica") %>% group_by(polo) %>%
              summarise(sa = sum(saldomovimentação), .groups = "drop"),
            by = "polo") %>%
  mutate(share_adm = adm_n / sum(adm_n), rc = (sa / sum(sa)) / share_adm)

t4 <- entro %>% left_join(sal_bc, by = "polo") %>%
  left_join(select(raz, polo, share_adm, rc), by = "polo") %>%
  mutate(polo = recode(polo, nucleo = "Núcleo histórico",
                       descon = "Desconcentração", demais = "Demais")) %>%
  transmute(`Território` = polo,
            Entropia = sprintf("%.3f", entropia),
            `Saldo Analítica` = Analítica,
            `Saldo Sintética` = Sintética,
            `Saldo Simbólica` = Simbólica,
            `Part. adm. (%)` = sprintf("%.1f", 100 * share_adm),
            `Razão de conc.` = sprintf("%.2f", rc))
salvar_tab(t4, "resultados/tabela4.pdf",
  "Tabela 4 – Diversidade, acumulação líquida e concentração da BC analítica por território, 2021–2025",
  paste0("Entropia de Shannon sobre a composição das admissões classificadas. Saldos = admissões – desligamentos.\n",
         "Razão de concentração = participação no saldo analítico / participação nas admissões totais."),
  largura = 8)

# ---- Tabela 5: robustez ----------------------------------------------------
t5a <- tribble(
  ~Variável, ~Base, ~`Sem tratados`, ~IPCA, ~`Sem SP`, ~`Cluster 2-way`,
  "BC Analítica", cel(m2, "bcAnalítica"), cel(m2_rob, "bcAnalítica"),
    cel(m2_ipca, "bcAnalítica"), cel(m2_semsp, "bcAnalítica"),
    cel(m2_2way, "bcAnalítica"),
  "BC Sintética", cel(m2, "bcSintética"), cel(m2_rob, "bcSintética"),
    cel(m2_ipca, "bcSintética"), cel(m2_semsp, "bcSintética"),
    cel(m2_2way, "bcSintética"),
  "BC Simbólica", cel(m2, "bcSimbólica"), cel(m2_rob, "bcSimbólica"),
    cel(m2_ipca, "bcSimbólica"), cel(m2_semsp, "bcSimbólica"),
    cel(m2_2way, "bcSimbólica"),
  "Observações", format(nobs(m2), big.mark = "."),
    format(nobs(m2_rob), big.mark = "."), format(nobs(m2_ipca), big.mark = "."),
    format(nobs(m2_semsp), big.mark = "."), format(nobs(m2_2way), big.mark = ".")
)
salvar_tab(t5a, "resultados/tabela5a.pdf",
  "Tabela 5a – Robustez dos prêmios das Bases de Conhecimento",
  "Todas as colunas: especificação M2. EP agrupados por município (última coluna: município e ano-mês).",
  largura = 9)

t5b_termos <- c("ana_descon", "sin_descon", "sim_descon",
                "ana_demais", "sin_demais", "sim_demais")
t5b <- tibble(
  `Interação` = c("Analítica × Descon.", "Sintética × Descon.",
                  "Simbólica × Descon.", "Analítica × Demais",
                  "Sintética × Demais", "Simbólica × Demais"),
  Base      = map_chr(t5b_termos, ~ cel(m4_def, .x)),
  `Sem ABC` = map_chr(t5b_termos, ~ cel(m4_semabc, .x)),
  `SP novos = núcleo` = map_chr(t5b_termos, ~ cel(m4_alt, .x))
)
salvar_tab(t5b, "resultados/tabela5b.pdf",
  "Tabela 5b – Robustez dos diferenciais territoriais",
  "Referência: núcleo histórico. EF de subclasse, ano-mês e município; EP agrupados por município.",
  largura = 8)

# ---- Figuras ----------------------------------------------------------------
fig1_dados <- broom::tidy(m2, conf.int = TRUE) %>%
  filter(str_detect(term, "^bc")) %>%
  mutate(bc = str_remove(term, "bc"),
         across(c(estimate, conf.low, conf.high), ~ 100 * (exp(.) - 1)))
fig1 <- ggplot(fig1_dados, aes(reorder(bc, estimate), estimate)) +
  geom_col(fill = "grey30", width = 0.55) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, linewidth = 0.4) +
  geom_text(aes(label = paste0(round(estimate), "%")),
            hjust = -0.5, size = 3.6) +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Prêmio sobre a produção direta (%)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())
ggsave("resultados/figura1.pdf", fig1, width = 6.5, height = 3)

fig2_dados <- bind_rows(
  comp_tab(comp, "Setor (29.1–29.4)"),
  comp_tab(filter(comp, grupo_cnae %in% c(291, 292)),
           "Montadoras (29.1–29.2)")
) %>%
  filter(bc == "Analítica") %>%
  pivot_longer(c(nucleo, descon, demais),
               names_to = "polo", values_to = "pct") %>%
  mutate(pct = as.numeric(pct),
         polo = factor(recode(polo, nucleo = "Núcleo histórico",
                              descon = "Desconcentração", demais = "Demais"),
                       levels = c("Núcleo histórico", "Desconcentração",
                                  "Demais")))
fig2 <- ggplot(fig2_dados, aes(polo, pct, fill = Amostra)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", pct)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.2) +
  scale_fill_manual(values = c("grey25", "grey65"), name = NULL) +
  labs(x = NULL, y = "Participação da BC analítica nas admissões (%)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave("resultados/figura2.pdf", fig2, width = 6.5, height = 4)

fig3_dados <- comp %>%
  filter(bc == "Analítica") %>%
  mutate(data = ymd(paste0(ano, "-", sprintf("%02d", mes), "-01"))) %>%
  group_by(polo, data) %>%
  summarise(saldo = sum(saldomovimentação), .groups = "drop") %>%
  arrange(data) %>% group_by(polo) %>%
  mutate(saldo_acum = cumsum(saldo),
         polo = recode(polo, nucleo = "Núcleo histórico",
                       descon = "Desconcentração", demais = "Demais"))
fig3 <- ggplot(fig3_dados, aes(data, saldo_acum, linetype = polo)) +
  geom_line(linewidth = 0.7) +
  labs(x = NULL, y = "Saldo líquido acumulado — BC analítica",
       linetype = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
ggsave("resultados/figura3.pdf", fig3, width = 6.5, height = 4)

cat("Arquivos gerados em resultados/:\n")
print(list.files("resultados", pattern = "pdf$"))
