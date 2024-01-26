# Repositorio ENEMDU proyecto empirica

# Preliminares --------------------------------------------------------------------------------------------

# Cargar librerias

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(survey)) install.packages("survey", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("survey", repos = "http://cran.us.r-project.org")

# Cargando datos ------------------------------------------------------------------------------------------

ENEMDU_2016 <- read_delim("Data/ENEMDU_PERSONAS_2016_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(ENEMDU_PERSONAS_2016_12_hom)

ENEMDU_2015 <- read_delim("Data/ENEMDU_PERSONAS_2015_12_hom.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(ENEMDU_PERSONAS_2015_12_hom)

# Crear una lista de las bases de datos

bases <- list(ENEMDU_2015, ENEMDU_2016)

# Definir una función para seleccionar las variables deseadas de las ENEMDU

vars <- function(data) {
  return(data[, c("p02", "p01", "p03", "p06", "p24", "p45", "ingrl", "nnivins", 
                  "periodo", "rama1", "p60a", "p60b",
                  "p60c","p60d","p60e","p60f","p60g","p60h","p60i","p60j","p60k")])
}

# Aplicar la función a cada base de datos usando lapply

bases_selec <- lapply(bases, vars)

# Unir los dataframes en la lista con bind_rows

ENEMDU_FIN <- bind_rows(bases_selec)

# Base para el analisis ------------------------------------------------------------------------------------------------

df_enemdu <- ENEMDU_FIN %>%
  select(   'sexo' = 'p02',
            'persona' = 'p01',
            'edad' = 'p03',
            'estado_civil' = 'p06',
            'horas_trabajadas' = 'p24',
            'experiencia_laboral' = 'p45',
            'ingreso_laboral'='ingrl',
            'nivel_instruccion'='nnivins',
            'ano' = 'periodo',
            'ciiu' = 'rama1',
            "Descontento_ingresos_bajos"= 'p60a',
            'Descontento_muchash_trabajo' = 'p60b',
            'Descontento_horarios' = 'p60c',
            'descontento_sobrecarga' = 'p60d',
            'Descontento_inestabilidad' = 'p60e',
            'Descontento_ambiente' = 'p60f',
            'Descontento_trabajo_calle' = 'p60g',
            'Descontento_accidente' = 'p60h',
            'Descontento_actividades' = 'p60i',
            'Descontento_progreso' = 'p60j',
            'Descontento_malas_rel_lab' = 'p60k') %>%
  filter(edad >= 18, edad <= 65, 
         horas_trabajadas > 0, horas_trabajadas < 999,
         experiencia_laboral > 0, experiencia_laboral < 99,
         ingreso_laboral > 0, ingreso_laboral < 999999,
         estado_civil %in% c(1,6)) %>%
  mutate(Sexo = factor(sexo, levels = c(1:2), labels = c("Hombre", "Mujer")),
         estado_civil = factor(estado_civil, levels = c(1,6), labels = c("Casado", "Soltero")),
         nivel_instruccion = factor(nivel_instruccion, levels = c(1:5), labels = c("Ninguno",
                                                                                   "Centro de alfabetizacion",
                                                                                   "Básica ",
                                                                                   "Media",
                                                                                   "Superior")),
         ciiu4 = as.factor(ciiu),
         fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d"))







