library(readr)
library(readxl)
library(dplyr)
library(stringr)

#Medicion de pobreza
HOGARES <- read_csv("MEP22/HOGARES.csv")

#Nacimientos
nac2022 <- read_csv("EV22/nac2022.csv")

#Defuciones 
nofetal2022 <- read_csv("EV22/nofetal2022.csv")

#Violencia Intrafamiliar
vio2022 <- read_excel("violencia_intrafamiliar_12.xls", skip = 9)

#Poblacion 
poblacion <- read_excel("DCD-area-proypoblacion-Mun-2020-2035-ActPostCOVID-19.xlsx", 
                        skip = 5)

#------------------------------------------------------------------------------#

# Antioquia ----
H05 <- unique(HOGARES[HOGARES$dpto == "05", c("mes","clase","dominio","lp")])

cod_mun_ant <- read_excel("Antioquia/cod_mun_ATL.xlsx", 
                          col_types = c("skip", "skip", "text", "text"))
names(cod_mun_ant) <- c("COD_MUNIC", "MUNICIPIO")

pob_ant <- poblacion[poblacion$DP == "05" & poblacion$AÑO == 2022 & poblacion$`ÁREA GEOGRÁFICA` == "Total", -c(2,5,6)]
names(pob_ant) <- c("COD_DPTO", "COD_MUNIC", "MUNICIPIO","Población")
pob_ant$COD_MUNIC <- substr(pob_ant$COD_MUNIC, 3, nchar(pob_ant$COD_MUNIC))
pob_ant <- within(pob_ant,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

## Natalidad -------------------------------------------------------------------
nac_ant <- nac2022[nac2022$COD_DPTO == "05", "COD_MUNIC"]

# total de nacimientos
num_nac_ant <- as.data.frame(table(nac_ant)); num_nac_ant
num_nac_ant <- left_join(num_nac_ant, cod_mun_ant, by = c("COD_MUNIC"))

tasa_nac_ant <- left_join(num_nac_ant, pob_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_nat = (Freq/pop)*1000)

## Mortalidad infantil  --------------------------------------------------------
def_ant <- nofetal2022[(nofetal2022$COD_DPTO == "05" & nofetal2022$GRU_ED2 == "01"),
                       "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_ant <- as.data.frame(table(def_ant)); num_def_ant
num_def_ant <- left_join(num_def_ant, cod_mun_ant, by = c("COD_MUNIC"))

tasa_def_ant <- left_join(num_def_ant, num_nac_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.x, nac = Freq.y) %>%
  mutate(tasa_def = (def/nac)*1000)


## Violencia Intrafamiliar ----------------------------------------------------- 
vio_ant <- vio2022[vio2022$DEPARTAMENTO == "ANTIOQUIA", c("MUNICIPIO", "CANTIDAD")]
num_vio_ant <- vio_ant %>%
  group_by(MUNICIPIO) %>%
  summarise(Freq = sum(CANTIDAD)) %>%
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "Medellín (CT)", "MEDELLIN", MUNICIPIO))

num_vio_ant <- na.omit(num_vio_ant)
num_vio_ant <- within(num_vio_ant,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

num_vio_ant <- left_join(num_vio_ant, cod_mun_ant, by = c("MUNICIPIO"))

tasa_vio_ant <- left_join(num_vio_ant, pob_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_vio = (Freq/pop)*100000)

### consolidación ----

ANT <- cod_mun_ant %>% 
  left_join(tasa_nac_ant, by = c("COD_MUNIC", "MUNICIPIO")) %>%
  left_join(tasa_def_ant, by = c("COD_MUNIC", "MUNICIPIO")) %>%
  left_join(tasa_vio_ant, by = c("COD_MUNIC", "MUNICIPIO")) %>%
  select(COD_MUNIC, MUNICIPIO, tasa_nat, tasa_def, tasa_vio, tot = pop.x)

#------------------------------------------------------------------------------#

# Atlántico ----
H08 <- unique(HOGARES[HOGARES$dpto == "08", c("mes","clase","dominio","lp")])

cod_mun_atl <- read_excel("Atlantico/cod_mun.xlsx")
names(cod_mun_atl) <- c("COD_MUNIC", "MUNICIPIO")

pob_atl <- poblacion[poblacion$DP == "08" & poblacion$AÑO == 2022 & poblacion$`ÁREA GEOGRÁFICA` == "Total", -c(2,5,6)]
names(pob_atl) <- c("COD_DPTO", "COD_MUNIC", "MUNICIPIO","Población")
pob_atl$COD_MUNIC <- substr(pob_atl$COD_MUNIC, 3, nchar(pob_atl$COD_MUNIC))
pob_atl <- within(pob_atl,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})
## Natalidad -------------------------------------------------------------------
nac_atl <- nac2022[nac2022$COD_DPTO == "08", "COD_MUNIC"]

# total de nacimientos
num_nac_atl <- as.data.frame(table(nac_atl)); num_nac_atl
num_nac_atl <- left_join(num_nac_atl, cod_mun_atl, by = c("COD_MUNIC"))

tasa_nac_atl <- left_join(num_nac_atl, pob_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_nat = (Freq/pop)*1000)

## Mortalidad infantil  --------------------------------------------------------
def_atl <- nofetal2022[(nofetal2022$COD_DPTO == "08" & nofetal2022$GRU_ED2 == "01"),
                       "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_atl <- as.data.frame(table(def_atl)); num_def_atl
num_def_atl <- left_join(num_def_atl, cod_mun_atl, by = c("COD_MUNIC"))

tasa_def_atl <- left_join(num_def_atl, num_nac_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.x, nac = Freq.y) %>%
  mutate(tasa_def = (def/nac)*1000)


## Violencia Intrafamiliar ----------------------------------------------------- 
vio_atl <- vio2022[vio2022$DEPARTAMENTO == "ATLÁNTICO", c("MUNICIPIO", "CANTIDAD")]
num_vio_atl <- vio_atl %>%
  group_by(MUNICIPIO) %>%
  summarise(Freq = sum(CANTIDAD)) %>%
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "Barranquilla (CT)", "Barranquilla", MUNICIPIO))

num_vio_atl <- na.omit(num_vio_atl)
num_vio_atl <- within(num_vio_atl,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

num_vio_atl <- left_join(num_vio_atl, cod_mun_atl, by = c("MUNICIPIO"))

tasa_vio_atl <- left_join(num_vio_atl, pob_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_vio = (Freq/pop)*100000)

### consolidación ----
ATL <- cod_mun_atl %>% 
  left_join(tasa_nac_atl, by = c("COD_MUNIC", "MUNICIPIO")) %>%
  left_join(tasa_def_atl, by = c("COD_MUNIC", "MUNICIPIO")) %>%
  left_join(tasa_vio_atl, by = c("COD_MUNIC", "MUNICIPIO")) %>%
  select(COD_MUNIC, MUNICIPIO, tasa_nat, tasa_def, tasa_vio, tot = pop.x)

# TOTAL ----
datos <- rbind(ANT, ATL)
datos[is.na(datos)] <- 0; View(datos)

