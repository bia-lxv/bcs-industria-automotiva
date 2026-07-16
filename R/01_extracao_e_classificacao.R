# ============================================================================
# 01_EXTRACAO_E_CLASSIFICACAO
# (i)  Constrói o crosswalk CBO (6 dígitos) -> Bases de Conhecimento,
#      herdando a classificação da família ocupacional (4 dígitos);
# (ii) Extrai as movimentações do Novo Caged (CNAE 29.1-29.4, 2021-2025)
#      e as classifica nas BCs.
#
# Fonte dos microdados (baixar antes; ver README):
# https://www.gov.br/trabalho-e-emprego/pt-br/acesso-a-informacao/
#   acoes-e-programas/programas-projetos-acoes-obras-e-atividades/
#   estatisticas-trabalho/microdados-rais-e-caged
# ============================================================================

library(tidyverse)
library(readxl)

caminho_caged <- "dados/caged/"   # arquivos CAGEDMOV{AAAAMM}.txt

# ----------------------------------------------------------------------------
# 1. CROSSWALK CBO -> BC
#    tipo_conhecimento: 0 = sem classificação; 1 = analítica;
#                       2 = sintética; 3 = simbólica
# ----------------------------------------------------------------------------
cbo_ocupacoes <- read_delim(
  "dados/CBO2002_Ocupacao.csv", delim = ";",
  locale = locale(encoding = "latin1"),
  col_types = cols(CODIGO = col_character(), TITULO = col_character())
) %>%
  rename(cbo2002 = CODIGO, titulo = TITULO) %>%
  mutate(cod_familia = str_sub(cbo2002, 1, 4))

classificacao_bc <- read_excel("dados/sint_simb_anal.xlsx",
                               sheet = "Planilha3") %>%
  mutate(cod_familia = str_pad(as.character(Cod_familia), 4, pad = "0")) %>%
  select(cod_familia, tipo_conhecimento, nome_familia = nome_cbo)

# Complementos (famílias ausentes na planilha original):
#  3951 Técnicos de apoio em P&D -> analítica (subgrupo 395)
#  3148 Inspetores de equipamentos -> sintética (subgrupo 314, metalmecânica)
#  2342 Prof. física/química ens. superior -> analítica (coerente c/ 2341/2343)
crosswalk_cbo_bc <- cbo_ocupacoes %>%
  left_join(classificacao_bc, by = "cod_familia") %>%
  mutate(
    tipo_conhecimento = case_when(
      cod_familia == "3951" ~ 1,
      cod_familia == "3148" ~ 2,
      cod_familia == "2342" ~ 1,
      TRUE ~ replace_na(tipo_conhecimento, 0)
    ),
    bc = factor(tipo_conhecimento, levels = c(0, 1, 2, 3),
                labels = c("Não classificada", "Analítica",
                           "Sintética", "Simbólica"))
  )

write_csv(crosswalk_cbo_bc, "dados/cbo_bc_crosswalk.csv")

# ----------------------------------------------------------------------------
# 2. EXTRAÇÃO DO NOVO CAGED — grupos CNAE 29.1-29.4, 2021-2025
#    (29.5, recondicionamento de motores, excluído: manutenção, não produção)
# ----------------------------------------------------------------------------
grupos_interesse <- c(291, 292, 293, 294)

ler_caged_automotiva <- function(ano, mes, caminho_base = caminho_caged) {

  arquivo <- paste0(caminho_base, "CAGEDMOV", ano,
                    sprintf("%02d", mes), ".txt")
  if (!file.exists(arquivo)) {
    warning(paste("Arquivo não encontrado:", arquivo)); return(NULL)
  }
  cat("Processando", arquivo, "\n")

  read_csv2(
    arquivo, locale = locale(encoding = "UTF-8"), show_col_types = FALSE,
    col_types = cols(
      competênciamov    = col_double(),
      município         = col_double(),
      subclasse         = col_double(),
      saldomovimentação = col_double(),
      cbo2002ocupação   = col_character(),  # texto: preserva zeros à esquerda
      graudeinstrução   = col_double(),
      idade             = col_double(),
      horascontratuais  = col_double(),
      salário           = col_double(),
      raçacor           = col_double(),
      sexo              = col_double(),
      .default = col_skip()
    )
  ) %>%
    filter(saldomovimentação %in% c(1, -1)) %>%
    filter(floor(subclasse / 10000) %in% grupos_interesse) %>%
    mutate(
      ano = ano, mes = mes,
      grupo_cnae = floor(subclasse / 10000),
      tipo_mov = if_else(saldomovimentação == 1, "admissao", "desligamento"),
      cbo2002ocupação = str_pad(cbo2002ocupação, 6, pad = "0")
    )
}

grade <- expand_grid(ano = 2021:2025, mes = 1:12)
caged_auto <- map2_dfr(grade$ano, grade$mes, ler_caged_automotiva)

# ----------------------------------------------------------------------------
# 3. CLASSIFICAÇÃO NAS BCs
#    Códigos do Caged sem correspondência na CBO (residuais, ~200 registros)
#    permanecem com bc = NA e são excluídos das análises por BC.
# ----------------------------------------------------------------------------
caged_auto_bc <- caged_auto %>%
  left_join(
    crosswalk_cbo_bc %>%
      select(cbo2002, titulo, cod_familia, tipo_conhecimento, bc),
    by = c("cbo2002ocupação" = "cbo2002")
  )

saveRDS(caged_auto_bc, "dados/caged_automotiva_bc.rds")
