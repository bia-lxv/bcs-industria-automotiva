# ============================================================================
# 03_ESTIMACAO — deflacionamento, tipologia territorial, mincerianas
#                (H1, H2, H3a) e medidas de composição (H3b)
# Requer internet (deflateBR baixa os índices da API do IPEA).
# ============================================================================

library(tidyverse)
library(fixest)
library(deflateBR)

caged <- readRDS("dados/caged_automotiva_limpo.rds")

# ----------------------------------------------------------------------------
# 1. AMOSTRA SALARIAL: admissões 2021-2025, deflacionadas (INPC, dez/2025)
# ----------------------------------------------------------------------------
adm <- caged %>%
  filter(tipo_mov == "admissao", ano %in% 2021:2025, salário > 0) %>%
  mutate(data_ref = ymd(paste0(ano, "-", sprintf("%02d", mes), "-01")))

adm$sal_real <- deflate(adm$salário, adm$data_ref, "12/2025", "inpc")
adm$sal_ipca <- deflate(adm$salário, adm$data_ref, "12/2025", "ipca")  # robustez

# ----------------------------------------------------------------------------
# 2. TIPOLOGIA TERRITORIAL (Quadro 1 do artigo)
#    Critério: idade da implantação produtiva (ANFAVEA, 2025; ILAESE, 2025)
#    Códigos IBGE de 6 dígitos (como no Caged)
# ----------------------------------------------------------------------------
polos <- tribble(
  ~município, ~polo,    ~nome_mun,
  # Núcleo histórico (implantação <= 1990)
  354870, "nucleo", "São Bernardo do Campo-SP",
  354880, "nucleo", "São Caetano do Sul-SP",
  354780, "nucleo", "Santo André-SP",
  351380, "nucleo", "Diadema-SP",
  352940, "nucleo", "Mauá-SP",
  355030, "nucleo", "São Paulo-SP",
  354990, "nucleo", "São José dos Campos-SP",
  355410, "nucleo", "Taubaté-SP",
  310670, "nucleo", "Betim-MG",
  410690, "nucleo", "Curitiba-PR",
  430510, "nucleo", "Caxias do Sul-RS",
  # Polos da desconcentração (Novo Regime Automotivo / guerra fiscal, 1996+)
  355240, "descon", "Sumaré-SP",
  352050, "descon", "Indaiatuba-SP",
  355220, "descon", "Sorocaba-SP",
  353870, "descon", "Piracicaba-SP",
  352250, "descon", "Itirapina-SP",
  352090, "descon", "Iracemápolis-SP",
  354890, "descon", "São Carlos-SP",
  412550, "descon", "São José dos Pinhais-PR",
  430920, "descon", "Gravataí-RS",
  330420, "descon", "Resende-RJ",
  330411, "descon", "Porto Real-RJ",
  330225, "descon", "Itatiaia-RJ",
  313670, "descon", "Juiz de Fora-MG",
  316720, "descon", "Sete Lagoas-MG",
  290570, "descon", "Camaçari-BA",
  260620, "descon", "Goiana-PE",
  520110, "descon", "Anápolis-GO",
  520510, "descon", "Catalão-GO",
  420130, "descon", "Araquari-SC"
)

adm <- adm %>%
  left_join(polos, by = "município") %>%
  mutate(polo = factor(replace_na(polo, "demais"),
                       levels = c("nucleo", "descon", "demais")))

# ----------------------------------------------------------------------------
# 3. VARIÁVEIS DA REGRESSÃO
# ----------------------------------------------------------------------------
adm <- adm %>%
  mutate(
    lw      = log(sal_real),
    idade2  = idade^2,
    sexo_f  = factor(sexo, levels = c(1, 3), labels = c("Homem", "Mulher")),
    raca_f  = fct_collapse(factor(raçacor),
                Branca = "1", Preta = "2", Parda = "3",
                Amarela = "4", Indigena = "5",
                other_level = "NaoInformada"),
    instr_f = factor(graudeinstrução),
    bc      = fct_relevel(bc, "Não classificada"),
    ano_mes = paste0(ano, "_", sprintf("%02d", mes)),
    montadora = grupo_cnae %in% c(291, 292),
    # dummies de interação BC x território (referência: núcleo)
    ana_descon = as.numeric(bc == "Analítica" & polo == "descon"),
    sin_descon = as.numeric(bc == "Sintética" & polo == "descon"),
    sim_descon = as.numeric(bc == "Simbólica" & polo == "descon"),
    ana_demais = as.numeric(bc == "Analítica" & polo == "demais"),
    sin_demais = as.numeric(bc == "Sintética" & polo == "demais"),
    sim_demais = as.numeric(bc == "Simbólica" & polo == "demais")
  )

# ----------------------------------------------------------------------------
# 4. MODELOS PRINCIPAIS
# ----------------------------------------------------------------------------
# M1: EF de subclasse e ano-mês
m1 <- feols(lw ~ bc + idade + idade2 + sexo_f + raca_f + instr_f
            | subclasse + ano_mes,
            data = adm, cluster = ~município)

# M2: + EF de município (especificação principal, H1/H2)
m2 <- feols(lw ~ bc + idade + idade2 + sexo_f + raca_f + instr_f
            | subclasse + ano_mes + município,
            data = adm, cluster = ~município)

# M4: interações BC x território com EF de município (H3a)
m4_def <- feols(lw ~ bc + ana_descon + sin_descon + sim_descon +
                     ana_demais + sin_demais + sim_demais +
                     idade + idade2 + sexo_f + raca_f + instr_f
                | subclasse + ano_mes + município,
                data = adm, cluster = ~município)

# GG2: analítica vs sintética entre profissionais de nível superior
adm_gg2 <- adm %>%
  filter(str_sub(cbo2002ocupação, 1, 1) == "2",
         bc %in% c("Analítica", "Sintética"))
m_gg2 <- feols(lw ~ i(bc, ref = "Sintética") + idade + idade2 + sexo_f +
                 raca_f + instr_f | subclasse + ano_mes + município,
               data = adm_gg2, cluster = ~município)

etable(m1, m2, m_gg2, keep = "%bc", digits = 3, fitstat = ~ n + r2 + wr2)
broom::tidy(m4_def, conf.int = TRUE) %>%
  filter(str_detect(term, "^bc|descon|demais"))

# ----------------------------------------------------------------------------
# 5. COMPOSIÇÃO, ENTROPIA, SALDOS E RAZÃO DE CONCENTRAÇÃO (H3b)
# ----------------------------------------------------------------------------
comp <- caged %>%
  filter(ano %in% 2021:2025) %>%
  left_join(polos, by = "município") %>%
  mutate(polo = replace_na(polo, "demais"))

# Participação de cada BC nas admissões, por território (e só montadoras)
comp %>% filter(tipo_mov == "admissao") %>%
  count(polo, bc) %>% group_by(polo) %>%
  mutate(pct = round(100 * n / sum(n), 2))

comp %>% filter(tipo_mov == "admissao", grupo_cnae %in% c(291, 292)) %>%
  count(polo, bc) %>% group_by(polo) %>%
  mutate(pct = round(100 * n / sum(n), 2))

# Entropia de Shannon da composição classificada
comp %>%
  filter(tipo_mov == "admissao", bc != "Não classificada", !is.na(bc)) %>%
  count(polo, bc) %>% group_by(polo) %>%
  mutate(p = n / sum(n)) %>%
  summarise(entropia = -sum(p * log(p)), .groups = "drop")

# Saldo líquido (admissões - desligamentos) por BC e território
comp %>% filter(!is.na(bc)) %>%
  group_by(polo, bc) %>%
  summarise(saldo = sum(saldomovimentação), .groups = "drop") %>%
  pivot_wider(names_from = bc, values_from = saldo)

# Razão de concentração da acumulação analítica
comp %>% filter(tipo_mov == "admissao") %>% count(polo, name = "adm_n") %>%
  left_join(comp %>% filter(bc == "Analítica") %>% group_by(polo) %>%
              summarise(saldo_ana = sum(saldomovimentação), .groups = "drop"),
            by = "polo") %>%
  mutate(share_adm   = adm_n / sum(adm_n),
         share_saldo = saldo_ana / sum(saldo_ana),
         razao_conc  = round(share_saldo / share_adm, 2))

# ----------------------------------------------------------------------------
# 6. ROBUSTEZ (Tabela 5 do artigo)
# ----------------------------------------------------------------------------
# (a) sem registros tratados na limpeza
m2_rob <- update(m2, data = adm %>%
  filter(flag_salario == "ok", flag_parcial != "proporcional_x44h",
         !flag_horista))
# (b) deflator IPCA
m2_ipca <- update(m2, data = adm %>% mutate(lw = log(sal_ipca)))
# (c) sem São Paulo capital
m2_semsp <- update(m2, data = adm %>% filter(município != 355030))
# (d) cluster duplo município x ano-mês
m2_2way <- update(m2, cluster = ~ município + ano_mes)
# (e) sem o ABC (teste da compressão sindical)
abc <- c(354870, 354880, 354780, 351380)
m4_semabc <- update(m4_def, data = adm %>% filter(!município %in% abc))
# (f) polos paulistas pós-1996 reclassificados como núcleo
sp_novos <- c(355240, 352050, 355220, 353870, 352250, 352090, 354890)
m4_alt <- update(m4_def, data = adm %>%
  mutate(across(c(ana_descon, sin_descon, sim_descon),
                ~ if_else(município %in% sp_novos, 0, .))))

etable(m2, m2_rob, m2_ipca, m2_semsp, m2_2way,
       keep = "%bc", digits = 3, fitstat = ~ n + r2)
etable(m4_def, m4_semabc, m4_alt,
       keep = c("%bc", "%ana_", "%sin_", "%sim_"),
       digits = 3, fitstat = ~ n + r2)

saveRDS(list(m1 = m1, m2 = m2, m4_def = m4_def, m_gg2 = m_gg2,
             m2_rob = m2_rob, m2_ipca = m2_ipca, m2_semsp = m2_semsp,
             m2_2way = m2_2way, m4_semabc = m4_semabc, m4_alt = m4_alt),
        "resultados/modelos.rds")
saveRDS(adm,  "dados/adm_final.rds")
saveRDS(comp, "dados/comp_final.rds")
