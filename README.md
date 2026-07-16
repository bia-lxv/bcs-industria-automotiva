# Hierarquia espacial do conhecimento na indústria automotiva brasileira

Código de replicação do artigo **"A hierarquia espacial do conhecimento: uma
precificação das Bases de Conhecimento na indústria automotiva brasileira"**
(Encontro Nacional de Economia — ANPEC, Área 9: Economia Industrial e da
Tecnologia).

**Autora:** XXXXXXX

## O que o código faz

1. Extrai as movimentações do **Novo Caged** (jan/2021–dez/2025) dos grupos
   CNAE 29.1–29.4 (indústria automotiva, exceto recondicionamento de motores);
2. Classifica cada ocupação (CBO 2002) em uma **Base de Conhecimento** —
   analítica, sintética ou simbólica — seguindo Martin (2012) e a adaptação de
   Santos e Marcellino (2016);
3. Aplica a limpeza baseada em regras descrita na seção 3 do artigo
   (todas as intervenções são registradas em *flags* para replicação com e
   sem os registros tratados);
4. Estima as equações mincerianas aumentadas (prêmios das BCs e interações
   com a tipologia territorial) e calcula as medidas de composição, entropia
   e razão de concentração;
5. Gera as tabelas e figuras do artigo (pasta `resultados/`).

## Dados

Os **microdados do Novo Caged não são redistribuídos neste repositório**
(arquivos de grande porte, de acesso público). Baixe os arquivos
`CAGEDMOV{AAAAMM}.txt` (jan/2021 a dez/2025) diretamente do
Ministério do Trabalho e Emprego / PDET:

- Página oficial dos microdados RAIS e Caged:
  <https://www.gov.br/trabalho-e-emprego/pt-br/acesso-a-informacao/acoes-e-programas/programas-projetos-acoes-obras-e-atividades/estatisticas-trabalho/microdados-rais-e-caged>
- Acesso direto via FTP do PDET (pasta `pdet/microdados/NOVO CAGED`):
  ftp://ftp.mtps.gov.br/pdet/microdados/
- Guia não oficial de acesso aos microdados: <http://cemin.wikidot.com/raisr>

Descompacte os arquivos `.7z` e salve os `.txt` em `dados/caged/`
(ou ajuste `caminho_caged` no script 01).

Arquivos auxiliares necessários em `dados/` (incluídos no repositório):

| Arquivo | Conteúdo | Fonte |
|---|---|---|
| `CBO2002_Ocupacao.csv` | Códigos e títulos das ocupações (6 dígitos) | MTE/CBO |
| `sint_simb_anal.xlsx` | Classificação das famílias ocupacionais nas BCs | Adaptado de Santos e Marcellino (2016) |

## Como rodar

Execute os scripts da pasta `R/` **em ordem**:

```r
source("R/01_extracao_e_classificacao.R")  # ~10 min (leitura dos 60 arquivos)
source("R/02_limpeza.R")
source("R/03_estimacao.R")                 # requer internet (API IPEA p/ deflatores)
source("R/04_tabelas_figuras.R")
```

**Requisitos:** R (>= 4.3) e os pacotes `tidyverse`, `readxl`, `fixest`,
`deflateBR`, `broom`, `ggpubr`.

```r
install.packages(c("tidyverse", "readxl", "fixest", "deflateBR",
                   "broom", "ggpubr"))
```

## Estrutura

```
├── R/
│   ├── 01_extracao_e_classificacao.R   # crosswalk CBO->BC + extração Caged
│   ├── 02_limpeza.R                    # limpeza baseada em regras (flags)
│   ├── 03_estimacao.R                  # deflação, polos, mincerianas, composição
│   └── 04_tabelas_figuras.R            # tabelas e figuras do artigo (PDF)
├── dados/                              # insumos auxiliares (Caged: baixar, ver acima)
├── resultados/                         # saídas geradas pelos scripts
└── README.md
```

## Citação

> XXXXXXXX A hierarquia espacial do conhecimento: uma
> precificação das Bases de Conhecimento na indústria automotiva brasileira.
> 2026.

## Licença

Código sob licença MIT. Os microdados do Novo Caged são de titularidade do
Ministério do Trabalho e Emprego e seguem seus termos de uso.
