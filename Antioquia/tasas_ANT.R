

cod_mun_ant <- read_excel("Antioquia/cod_mun_ATL.xlsx", 
                      col_types = c("skip", "skip", "text", "text"))
names(cod_mun_ant) <- c("COD_MUNIC", "MUNICIPIO")


## Natalidad -------------------------------------------------------------------
nac2022 <- read_csv("EV22/nac2022.csv")
nac_ant <- nac2022[nac2022$COD_DPTO == "05", "COD_MUNIC"]

# total de nacimientos
num_nac_ant <- as.data.frame(table(nac_ant)); num_nac_ant
num_nac_ant <- left_join(num_nac_ant, cod_mun_ant, by = c("COD_MUNIC"))


## Mortalidad infantil  --------------------------------------------------------
nofetal2022 <- read_csv("EV22/nofetal2022.csv")
def_ant <- nofetal2022[(nofetal2022$COD_DPTO == "05" & nofetal2022$GRU_ED2 == "01"),
                       "COD_MUNIC"]

# Total del muertes menor a 1 año
num_def_ant <- as.data.frame(table(def_ant)); num_def_ant
num_def_ant <- left_join(num_def_ant, cod_mun_ant, by = c("COD_MUNIC"))

tasa_def_ant <- left_join(num_def_ant, num_nac_ant, by = c("COD_MUNIC")) %>%
  select(COD_MUNIC, MUNICIPIO = MUNICIPIO.x, def = Freq.x, nac = Freq.y) %>%
  mutate(tasa = (def/nac)*1000)


## Violencia Intrafamiliar ----------------------------------------------------- 
vio2022 <- read_excel("violencia_intrafamiliar_12.xls", skip = 9)
vio_ant <- vio2022[vio2022$DEPARTAMENTO == "ANTIOQUIA", c("MUNICIPIO", "CANTIDAD")]
num_vio_ant <- vio_ant %>%
  group_by(MUNICIPIO) %>%
  summarise(vio = sum(CANTIDAD)) %>%
  mutate(MUNICIPIO = ifelse(MUNICIPIO == "Medellín (CT)", "MEDELLIN", MUNICIPIO))

num_vio_ant <- na.omit(num_vio_ant)
num_vio_ant <- within(num_vio_ant,{
  MUNICIPIO <- str_to_upper(MUNICIPIO)
  MUNICIPIO <- chartr("ÁÉÍÓÚÜ", "AEIOUU", MUNICIPIO)
})

num_vio_ant <- left_join(num_vio_ant, cod_mun_ant, by = c("MUNICIPIO"))
