library(readr)
library(readxl)
library(dplyr)
library(stringr)

cod_mun_atl <- read_excel("Atlantico/cod_mun.xlsx")
names(cod_mun_atl) <- c("COD_MUNIC", "MUNICIPIO")

## Natalidad -------------------------------------------------------------------
nac2022 <- read_csv("EV22/nac2022.csv")
nac_atl <- nac2022[nac2022$COD_DPTO == "08", "COD_MUNIC"]

# total de nacimientos
num_nac_atl <- as.data.frame(table(nac_atl)); num_nac_atl
num_nac_atl <- left_join(num_nac_atl, cod_mun_atl, by = c("COD_MUNIC"))


## Mortalidad infantil  --------------------------------------------------------
nofetal2022 <- read_csv("EV22/nofetal2022.csv")
def_atl <- nofetal2022[(nofetal2022$COD_DPTO == "08" & nofetal2022$GRU_ED2 == "01"),
                      "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_atl <- as.data.frame(table(def_atl)); num_def_atl
num_def_atl <- left_join(num_def_atl, cod_mun_atl, by = c("COD_MUNIC"))

tasa_def_atl <- left_join(num_def_atl, num_nac_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.x, nac = Freq.y) %>%
  mutate(tasa = (def/nac)*1000)


## Violencia Intrafamiliar ----------------------------------------------------- 
vio2022 <- read_excel("violencia_intrafamiliar_12.xls", skip = 9)
vio_atl <- vio2022[vio2022$DEPARTAMENTO == "ATLÁNTICO", c("MUNICIPIO", "CANTIDAD")]
num_vio_atl <- vio_atl %>%
  group_by(MUNICIPIO) %>%
  summarise(vio = sum(CANTIDAD)) %>%
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "Barranquilla (CT)", "Barranquilla", MUNICIPIO))

num_vio_atl <- na.omit(num_vio_atl)
num_vio_atl <- within(num_vio_atl,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

num_vio_atl <- left_join(num_vio_atl, cod_mun_atl, by = c("MUNICIPIO"))




