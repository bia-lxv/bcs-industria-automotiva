# ============================================================================
# 02_LIMPEZA — limpeza baseada em regras (seção 3.1 do artigo)
#
# Regras:
#  H1. 0 < horas < 1        -> deslocamento de vírgula: x100 (0,44 -> 44)
#  H2. horas 0/NA           -> 44 (jornada-padrão)
#  H3. horas > 44           -> 44 (teto legal)
#  S1. salário < 60         -> registro horista: mensalizar (x horas x 5)
#  P1. parcial (h < 44), não-chefia: teste proporcional vs erro de jornada
#        salário ~ mediana x h/44  -> parcial real: reescala p/ base 44h
#        salário ~ mediana integral -> erro de jornada: h = 44, salário fica
#  P2. chefia (diretor|gerente|supervisor|presidente): preservada integralmente
#  S2. razão salário/mediana do cargo fora de [0,1; 10] -> corrige pela
#      potência de 10 que melhor aproxima a mediana ocupacional
#  S3. zero/NA/abaixo de 25% do SM (e adultos abaixo de 50% do SM, em base
#      44h) -> imputa a mediana do cargo
#
# Todas as intervenções são registradas em flags (flag_horas, flag_horista,
# flag_parcial, flag_salario); valores originais preservados em *_orig.
# ============================================================================

library(tidyverse)

caged <- readRDS("dados/caged_automotiva_bc.rds") %>%
  filter(cbo2002ocupação != "999999")   # ocupação não identificada

# Salário mínimo vigente por ano
sal_minimo <- tibble(ano = 2021:2025,
                     sm  = c(1100, 1212, 1320, 1412, 1518))
caged <- caged %>%
  left_join(sal_minimo, by = "ano") %>%
  mutate(salario_orig = salário,
         horas_orig   = horascontratuais,
         instr_orig   = graudeinstrução)

# ----------------------------------------------------------------------------
# 1. GRAU DE INSTRUÇÃO: 99 descartado; 80 (sem definição no layout) inferido
#    pelo título da ocupação; casos ambíguos descartados.
#    Layout Novo Caged: 7 = médio completo | 9 = superior completo
# ----------------------------------------------------------------------------
padrao_superior <- regex(
  paste0("analista|engenheir|econom|advogad|administrador|contador|",
         "arquitet|médico|medico|psicólog|psicolog|farmac|enfermeiro|",
         "gerente|supervisor|diretor|presidente|pesquisador|professor|",
         "estatístic|matemátic|físic|desenvolvedor"), ignore_case = TRUE)
padrao_medio <- regex(
  paste0("técnic|tecnic|operador|montador|soldador|ferramenteiro|",
         "mecânic|mecanic|eletricista|inspetor|prensista|torneiro|",
         "auxiliar|assistente|almoxarife|alimentador|pintor|caldeireiro"),
  ignore_case = TRUE)

caged <- caged %>%
  filter(graudeinstrução != 99) %>%
  mutate(
    instr_inferida = graudeinstrução == 80,
    graudeinstrução = case_when(
      graudeinstrução == 80 & str_detect(titulo, padrao_superior) ~ 9,
      graudeinstrução == 80 & str_detect(titulo, padrao_medio)    ~ 7,
      TRUE ~ graudeinstrução
    )
  ) %>%
  filter(graudeinstrução != 80)

# ----------------------------------------------------------------------------
# 2. HORAS (H1-H3)
# ----------------------------------------------------------------------------
caged <- caged %>%
  mutate(
    flag_horas = case_when(
      is.na(horascontratuais) | horascontratuais == 0 ~ "imputada_44",
      horascontratuais > 0 & horascontratuais < 1     ~ "virgula_x100",
      horascontratuais > 44                           ~ "teto_44",
      TRUE                                            ~ "ok"
    ),
    horascontratuais = case_when(
      flag_horas == "imputada_44"  ~ 44,
      flag_horas == "virgula_x100" ~ pmin(horascontratuais * 100, 44),
      flag_horas == "teto_44"      ~ 44,
      TRUE ~ horascontratuais
    ),
    eh_chefia = str_detect(
      titulo, regex("diretor|gerente|supervisor|presidente",
                    ignore_case = TRUE))
  )

# ----------------------------------------------------------------------------
# 3. REFERÊNCIA SALARIAL: mediana do cargo no miolo plausível (44h, entre
#    0,5 SM e 60 mil); cargos com < 5 observações usam a mediana da família.
# ----------------------------------------------------------------------------
base_sana <- caged %>%
  filter(horascontratuais == 44, salário >= 0.5 * sm, salário <= 60000)

mediana_cargo <- base_sana %>%
  group_by(cbo2002ocupação) %>%
  summarise(med_cargo = median(salário), n_sanas = n(), .groups = "drop")

mediana_familia <- base_sana %>%
  group_by(cod_familia) %>%
  summarise(med_familia = median(salário), .groups = "drop")

caged <- caged %>%
  left_join(mediana_cargo,   by = "cbo2002ocupação") %>%
  left_join(mediana_familia, by = "cod_familia") %>%
  mutate(referencia = if_else(!is.na(med_cargo) & n_sanas >= 5,
                              med_cargo, med_familia))

# ----------------------------------------------------------------------------
# 4. HORISTAS (S1): salário registrado em base horária -> mensal
#    (ex.: 6,42/h x 220h = 1.412 = salário mínimo de 2024)
# ----------------------------------------------------------------------------
caged <- caged %>%
  mutate(
    flag_horista = salário > 0 & salário < 60,
    salário = if_else(flag_horista, salário * horascontratuais * 5, salário)
  )

# ----------------------------------------------------------------------------
# 5. PARCIAIS (P1/P2): proporcional (reescala p/ 44h) vs erro de jornada
# ----------------------------------------------------------------------------
caged <- caged %>%
  mutate(
    parcial = horascontratuais < 44 & !eh_chefia &
              salário > 0 & !is.na(referencia),
    d_prop = if_else(parcial,
      abs(log(salário / (referencia * horascontratuais / 44))), NA_real_),
    d_full = if_else(parcial, abs(log(salário / referencia)), NA_real_),
    flag_parcial = case_when(
      parcial & d_prop <= d_full ~ "proporcional_x44h",
      parcial & d_prop >  d_full ~ "erro_horas_h44",
      TRUE                       ~ "nao_parcial"
    ),
    salário = if_else(flag_parcial == "proporcional_x44h",
                      salário * 44 / horascontratuais, salário),
    horascontratuais = if_else(flag_parcial != "nao_parcial",
                               44, horascontratuais)
  ) %>%
  select(-parcial, -d_prop, -d_full)

# ----------------------------------------------------------------------------
# 6. ORDEM DE GRANDEZA (S2)
# ----------------------------------------------------------------------------
melhor_potencia <- function(sal, ref) {
  cand <- sal * 10^(-3:3)
  cand[which.min(abs(log(cand) - log(ref)))]
}

caged <- caged %>%
  mutate(
    razao = salário / referencia,
    flag_salario = case_when(
      is.na(salário) | salário == 0 ~ "imputar",
      is.na(referencia)             ~ "sem_referencia",
      razao > 10 | razao < 0.1      ~ "ordem_grandeza",
      TRUE                          ~ "ok"
    )
  )

corrigidos <- caged %>%
  filter(flag_salario == "ordem_grandeza") %>%
  rowwise() %>%
  mutate(salário = melhor_potencia(salário, referencia)) %>%
  ungroup() %>%
  mutate(flag_salario = if_else(
    salário / referencia > 10 | salário / referencia < 0.1,
    "imputar", "corrigido_pot10"))

caged <- caged %>%
  filter(flag_salario != "ordem_grandeza") %>%
  bind_rows(corrigidos)

# ----------------------------------------------------------------------------
# 7. PISO E IMPUTAÇÃO (S3)
# ----------------------------------------------------------------------------
caged <- caged %>%
  mutate(
    flag_salario = case_when(
      flag_salario == "ok" & salário < 0.25 * sm              ~ "imputar",
      flag_salario == "ok" & idade >= 18 & salário < 0.5 * sm ~ "imputar",
      TRUE ~ flag_salario
    ),
    salário = if_else(flag_salario == "imputar" & !is.na(referencia),
                      referencia, salário),
    flag_salario = if_else(flag_salario == "imputar" & !is.na(referencia),
                           "imputado_mediana", flag_salario)
  ) %>%
  filter(!(flag_salario == "imputar" & is.na(referencia))) %>%
  select(-razao, -med_cargo, -med_familia, -n_sanas)

# ----------------------------------------------------------------------------
# 8. DIAGNÓSTICO (percentuais reportados na seção 3.1 do artigo)
# ----------------------------------------------------------------------------
caged %>% count(flag_horas)   %>% mutate(pct = round(100 * n / sum(n), 2))
caged %>% count(flag_horista) %>% mutate(pct = round(100 * n / sum(n), 2))
caged %>% count(flag_parcial) %>% mutate(pct = round(100 * n / sum(n), 2))
caged %>% count(flag_salario) %>% mutate(pct = round(100 * n / sum(n), 2))

saveRDS(caged, "dados/caged_automotiva_limpo.rds")
