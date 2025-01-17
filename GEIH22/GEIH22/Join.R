# carga de archivos ----
library(readr)

Enero <- read_delim("GEIH22/GEIH22/GEIH_Enero_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ",", escape_double = FALSE, trim_ws = TRUE)

Febrero <- read_delim("GEIH22/GEIH22/GEIH_Febrero_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Marzo <- read_delim("GEIH22/GEIH22/GEIH_Marzo_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Abril <- read_delim("GEIH22/GEIH22/GEIH_Abril_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Mayo  <- read_delim("GEIH22/GEIH22/GEIH_Mayo_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Junio <- read_delim("GEIH22/GEIH22/GEIH_Junio_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Julio <- read_delim("GEIH22/GEIH22/GEIH_Julio_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Agosto <- read_delim("GEIH22/GEIH22/GEIH_Agosto_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Septiembre <- read_delim("GEIH22/GEIH22/GEIH_Septiembre_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Octubre <- read_delim("GEIH22/GEIH22/GEIH_Octubre_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Noviembre <- read_delim("GEIH22/GEIH22/GEIH_Noviembre_2022_Marco_2018/CSV/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)

Diciembre <- read_delim("GEIH22/GEIH22/GEIH_Diciembre_2022_Marco_2018/CVS/Características generales, seguridad social en salud y educación.CSV", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE)


## Join ----
library(dplyr)

GEIH <- rbind((Enero %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)), 
              (Febrero %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)), 
              (Marzo %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)), 
              (Abril %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Mayo %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Junio %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Julio %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Agosto %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Septiembre %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Octubre %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Noviembre %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)),
              (Diciembre %>% select(DIRECTORIO, SECUENCIA_P,ORDEN, DPTO, HOGAR)))

GEIH_atlantico <- GEIH %>% filter(DPTO == "08")

ID_atlantinco <- unique(GEIH$DIRECTORIO)

