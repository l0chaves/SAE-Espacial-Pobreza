library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

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

#División Política
divipola <- read_excel("divipola.xls", sheet = "Listado Vigentes",
                       col_types = c("text", "text", "skip", "text", "text", "skip", "skip"))

#------------------------------------------------------------------------------#

# Antioquia ----
H05 <- unique(HOGARES[HOGARES$dpto == "05", c("mes","clase","dominio","lp")])

cod_mun_ant <- unique(divipola[divipola$`Código Departamento` == "05", c("Código Municipio", "Nombre Municipio")])
names(cod_mun_ant) <- c("COD_MUNIC", "MUNICIPIO")
cod_mun_ant$COD_MUNIC <- substr(cod_mun_ant$COD_MUNIC, 3, nchar(cod_mun_ant$COD_MUNIC))
cod_mun_ant <- na.omit(cod_mun_ant)
cod_mun_ant <- within(cod_mun_ant,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

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

tasa_nac_ant <- left_join(pob_ant, num_nac_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_nat = (Freq/pop)*1000) %>%
  mutate(tasa_nat = replace_na(tasa_nat, 0))

## Mortalidad infantil  --------------------------------------------------------
def_ant <- nofetal2022[(nofetal2022$COD_DPTO == "05" & nofetal2022$GRU_ED2 == "01"),
                       "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_ant <- as.data.frame(table(def_ant)); num_def_ant
num_def_ant <- left_join(num_def_ant, cod_mun_ant, by = c("COD_MUNIC"))

tasa_def_ant <- left_join(num_nac_ant, num_def_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.y, nac = Freq.x) %>%
  mutate(tasa_def = (def/nac)*1000) %>%
  mutate(tasa_def = replace_na(tasa_def, 0))

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
  MUNICIPIO <- str_replace_all(MUNICIPIO, "\n", "")
})

tasa_vio_ant <- left_join(pob_ant, num_vio_ant, by = c("MUNICIPIO")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO, Freq, pop = Población) %>%
  mutate(tasa_vio = (Freq/pop)*100000) %>%
  mutate(tasa_vio = replace_na(tasa_vio, 0))


## Bajo Peso -------------------------------------------------------------------

# Total de bajo peso al nacer
bpn_ant <- nac2022[nac2022$COD_DPTO == "05"  & nac2022$PESO_NAC <= 4, "COD_MUNIC"]

num_bpn_ant <- as.data.frame(table(bpn_ant)); num_bpn_ant
num_bpn_ant <- left_join(num_bpn_ant, cod_mun_ant, by = c("COD_MUNIC"))

# Total de peso registrado al nacer
peso_ant <- nac2022[nac2022$COD_DPTO == "05"  & nac2022$PESO_NAC != 9, "COD_MUNIC"]
peso_ant <- as.data.frame(table(peso_ant)); peso_ant
peso_ant <- left_join(peso_ant, cod_mun_ant, by = c("COD_MUNIC"))

tasa_bpn_ant <- left_join(peso_ant, num_bpn_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Bajo = Freq.y, Tot = Freq.x) %>%
  mutate(tasa_bps = (Bajo/Tot)*1000)  %>%
  mutate(tasa_bps = replace_na(tasa_bps, 0))


### consolidación ----
ANT <- pob_ant %>% 
  left_join(tasa_nac_ant, by = c("COD_MUNIC")) %>%
  left_join(tasa_def_ant, by = c("COD_MUNIC")) %>%
  left_join(tasa_vio_ant, by = c("COD_MUNIC")) %>%
  left_join(tasa_bpn_ant, by = c("COD_MUNIC")) %>%
  select(COD_DPTO, COD_MUNIC, MUNICIPIO = MUNICIPIO.y, tasa_nat, tasa_def, tasa_vio, tasa_bps, tot = pop.x)

#------------------------------------------------------------------------------#

# Atlántico ----
H08 <- unique(HOGARES[HOGARES$dpto == "08", c("mes","clase","dominio","lp")])

cod_mun_atl <- unique(divipola[divipola$`Código Departamento` == "08", c("Código Municipio", "Nombre Municipio")])
names(cod_mun_atl) <- c("COD_MUNIC", "MUNICIPIO")

cod_mun_atl$COD_MUNIC <- substr(cod_mun_atl$COD_MUNIC, 3, nchar(cod_mun_atl$COD_MUNIC))

cod_mun_atl <- na.omit(cod_mun_atl)
cod_mun_atl <- within(cod_mun_atl,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

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

tasa_nac_atl <- left_join(pob_atl, num_nac_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_nat = (Freq/pop)*1000) %>%
  mutate(tasa_nat = replace_na(tasa_nat, 0))

## Mortalidad infantil  --------------------------------------------------------
def_atl <- nofetal2022[(nofetal2022$COD_DPTO == "08" & nofetal2022$GRU_ED2 == "01"),
                       "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_atl <- as.data.frame(table(def_atl)); num_def_atl
num_def_atl <- left_join(num_def_atl, cod_mun_atl, by = c("COD_MUNIC"))

tasa_def_atl <- left_join(num_nac_atl, num_def_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.y, nac = Freq.x) %>%
  mutate(tasa_def = (def/nac)*1000) %>%
  mutate(tasa_def = replace_na(tasa_def, 0))

## Violencia Intrafamiliar ----------------------------------------------------- 
vio_atl <- vio2022[vio2022$DEPARTAMENTO == "ATLÁNTICO", c("MUNICIPIO", "CANTIDAD")]
num_vio_atl <- vio_atl %>%
  group_by(MUNICIPIO) %>%
  summarise(Freq = sum(CANTIDAD)) %>%
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "Barranquilla (CT)", "BARRANQUILLA", MUNICIPIO))

num_vio_atl <- na.omit(num_vio_atl)
num_vio_atl <- within(num_vio_atl,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
  MUNICIPIO <- str_replace_all(MUNICIPIO, "\n", "")
})

tasa_vio_atl <- left_join(pob_atl, num_vio_atl, by = c("MUNICIPIO")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO, Freq, pop = Población) %>%
  mutate(tasa_vio = (Freq/pop)*100000) %>%
  mutate(tasa_vio = replace_na(tasa_vio, 0))


## Bajo Peso -------------------------------------------------------------------

# Total de bajo peso al nacer
bpn_atl <- nac2022[nac2022$COD_DPTO == "08"  & nac2022$PESO_NAC <= 4, "COD_MUNIC"]

num_bpn_atl <- as.data.frame(table(bpn_atl)); num_bpn_atl
num_bpn_atl <- left_join(num_bpn_atl, cod_mun_atl, by = c("COD_MUNIC"))

# Total de peso registrado al nacer
peso_atl <- nac2022[nac2022$COD_DPTO == "08"  & nac2022$PESO_NAC != 9, "COD_MUNIC"]
peso_atl <- as.data.frame(table(peso_atl)); peso_atl
peso_atl <- left_join(peso_atl, cod_mun_atl, by = c("COD_MUNIC"))

tasa_bpn_atl <- left_join(peso_atl, num_bpn_atl, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Bajo = Freq.y, Tot = Freq.x) %>%
  mutate(tasa_bps = (Bajo/Tot)*1000)  %>%
  mutate(tasa_bps = replace_na(tasa_bps, 0))


### consolidación ----
ATL <- pob_atl %>% 
  left_join(tasa_nac_atl, by = c("COD_MUNIC")) %>%
  left_join(tasa_def_atl, by = c("COD_MUNIC")) %>%
  left_join(tasa_vio_atl, by = c("COD_MUNIC")) %>%
  left_join(tasa_bpn_atl, by = c("COD_MUNIC")) %>%
  select(COD_DPTO, COD_MUNIC, MUNICIPIO, tasa_nat, tasa_def, tasa_vio, tasa_bps, tot = pop.x)

# Choco ----
H27 <- unique(HOGARES[HOGARES$dpto == "27", c("mes","clase","dominio","lp")])

cod_mun_chc <- unique(divipola[divipola$`Código Departamento` == "27", c("Código Municipio", "Nombre Municipio")])
names(cod_mun_chc) <- c("COD_MUNIC", "MUNICIPIO")
cod_mun_chc$COD_MUNIC <- substr(cod_mun_chc$COD_MUNIC, 3, nchar(cod_mun_chc$COD_MUNIC))
cod_mun_chc <- na.omit(cod_mun_chc)
cod_mun_chc <- within(cod_mun_chc,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

pob_chc <- poblacion[poblacion$DP == "27" & poblacion$AÑO == 2022 & poblacion$`ÁREA GEOGRÁFICA` == "Total", -c(2,5,6)]
names(pob_chc) <- c("COD_DPTO", "COD_MUNIC", "MUNICIPIO","Población")
pob_chc$COD_MUNIC <- substr(pob_chc$COD_MUNIC, 3, nchar(pob_chc$COD_MUNIC))
pob_chc <- within(pob_chc,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

## Natalidad -------------------------------------------------------------------
nac_chc <- nac2022[nac2022$COD_DPTO == "27", "COD_MUNIC"]

# total de nacimientos
num_nac_chc <- as.data.frame(table(nac_chc)); num_nac_chc
num_nac_chc <- left_join(num_nac_chc, cod_mun_chc, by = c("COD_MUNIC"))

tasa_nac_chc <- left_join(pob_chc, num_nac_chc, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Freq, pop = Población) %>%
  mutate(tasa_nat = (Freq/pop)*1000) %>%
  mutate(tasa_nat = replace_na(tasa_nat, 0))

## Mortalidad infantil  --------------------------------------------------------
def_chc <- nofetal2022[(nofetal2022$COD_DPTO == "27" & nofetal2022$GRU_ED2 == "01"),
                       "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_chc <- as.data.frame(table(def_chc)); num_def_chc
num_def_chc <- left_join(num_def_chc, cod_mun_chc, by = c("COD_MUNIC"))

tasa_def_chc <- left_join(num_nac_chc, num_def_chc, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.y, nac = Freq.x) %>%
  mutate(tasa_def = (def/nac)*1000) %>%
  mutate(tasa_def = replace_na(tasa_def, 0))

## Violencia Intrafamiliar ----------------------------------------------------- 
vio_chc <- vio2022[vio2022$DEPARTAMENTO == "CHOCÓ", c("MUNICIPIO", "CANTIDAD")]
num_vio_chc <- vio_chc %>%
  group_by(MUNICIPIO) %>%
  summarise(Freq = sum(CANTIDAD)) %>%
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "Quibdó (CT)", "QUIBDO", MUNICIPIO))

num_vio_chc <- na.omit(num_vio_chc)
num_vio_chc <- within(num_vio_chc,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
  MUNICIPIO <- str_replace_all(MUNICIPIO, "\n", "")
})

tasa_vio_chc <- left_join(pob_chc, num_vio_chc, by = c("MUNICIPIO")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO, Freq, pop = Población) %>%
  mutate(tasa_vio = (Freq/pop)*100000) %>%
  mutate(tasa_vio = replace_na(tasa_vio, 0))


## Bajo Peso -------------------------------------------------------------------

# Total de bajo peso al nacer
bpn_chc <- nac2022[nac2022$COD_DPTO == "27"  & nac2022$PESO_NAC <= 4, "COD_MUNIC"]

num_bpn_chc <- as.data.frame(table(bpn_chc)); num_bpn_chc
num_bpn_chc <- left_join(num_bpn_chc, cod_mun_chc, by = c("COD_MUNIC"))

# Total de peso registrado al nacer
peso_chc <- nac2022[nac2022$COD_DPTO == "27"  & nac2022$PESO_NAC != 9, "COD_MUNIC"]
peso_chc <- as.data.frame(table(peso_chc)); peso_chc
peso_chc <- left_join(peso_chc, cod_mun_chc, by = c("COD_MUNIC"))

tasa_bpn_chc <- left_join(peso_chc, num_bpn_chc, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, Bajo = Freq.y, Tot = Freq.x) %>%
  mutate(tasa_bps = (Bajo/Tot)*1000)  %>%
  mutate(tasa_bps = replace_na(tasa_bps, 0))


### consolidación ----
CHC <- pob_chc %>% 
  left_join(tasa_nac_chc, by = c("COD_MUNIC")) %>%
  left_join(tasa_def_chc, by = c("COD_MUNIC")) %>%
  left_join(tasa_vio_chc, by = c("COD_MUNIC")) %>%
  left_join(tasa_bpn_chc, by = c("COD_MUNIC")) %>%
  select(COD_DPTO, COD_MUNIC, MUNICIPIO, tasa_nat, tasa_def, tasa_vio, tasa_bps, tot = pop.x)


# Guardado ----
save(ATL, ANT, CHC, file = "Base_de_Datos.RData")

write.csv(ANT, file = "ANT.csv")
write.csv(ATL, file = "ATL.csv")
write.csv(CHC, file = "CHC.csv")


rm(list = ls()); load("Base_de_Datos.RData")
