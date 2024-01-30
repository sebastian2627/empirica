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
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")

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
                  "periodo", "rama1", "p60a", "p60b", "fexp", "ciudad", "rn", "p14","p44f",
                  "p15","p60c","p60d","p60e","p60f","p60g","p60h","p60i","p60j","p60k")])
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
            'idioma'= 'p14',
            'etnia' = 'p15',
            'region' = 'rn',
            'seguro_social' = "p44f",
            "Descontento_ingresos_bajos"= 'p60a',
            'Descontento_muchash_trabajo' = 'p60b',
            'Descontento_horarios' = 'p60c',
            'Descontento_sobrecarga' = 'p60d',
            'Descontento_inestabilidad' = 'p60e',
            'Descontento_ambiente' = 'p60f',
            'Descontento_trabajo_calle' = 'p60g',
            'Descontento_accidente' = 'p60h',
            'Descontento_actividades' = 'p60i',
            'Descontento_progreso' = 'p60j',
            'Descontento_malas_rel_lab' = 'p60k',
            "fexp","ciudad") %>%
  filter(edad >= 18, edad <= 65, 
         horas_trabajadas > 0, horas_trabajadas < 999,
         experiencia_laboral > 0, experiencia_laboral < 99,
         ingreso_laboral > 0, ingreso_laboral < 999999,
         idioma %in% c(1:4),
         etnia %in% c(1:7),
         region %in% c(1:4),
         estado_civil %in% c(1,6)) %>%
  mutate(Sexo = factor(sexo, levels = c(1:2), labels = c("Hombre", "Mujer")),
         estado_civil = factor(estado_civil, levels = c(1,6), labels = c("Casado", "Soltero")),
         idioma = factor(idioma, levels = c(1:4), labels = c("indigena",
                                                             "indigena_espanol",
                                                             "espanol",
                                                             "espanol_extranjero")),
         etnia = factor(etnia, levels = c(1:7), labels = c("indigena",
                                                           "afroecuatoriano",
                                                           "negro",
                                                           "mulato",
                                                           "montubio",
                                                           "mestizo",
                                                           "blanco")),
         region = factor(region, levels = c(1:4), labels = c("sierra",
                                                             "costa",
                                                             "amazonia",
                                                             "insular")),
         seguro_social = factor(seguro_social, levels = c(1:2), labels = c(1,0)),
         nivel_instruccion = factor(nivel_instruccion, levels = c(1:5), labels = c("Ninguno",
                                                                                   "Centro de alfabetizacion",
                                                                                   "Básica ",
                                                                                   "Media",
                                                                                   "Superior")),
         Descontento_ingresos_bajos = factor(Descontento_ingresos_bajos, levels = c(1:2), labels = c(1,0)),
         Descontento_muchash_trabajo = factor(Descontento_muchash_trabajo, levels = c(1:2), labels = c(1,0)),
         Descontento_horarios = factor(Descontento_horarios, levels = c(1:2), labels = c(1,0)),
         Descontento_sobrecarga = factor(Descontento_sobrecarga, levels = c(1:2), labels = c(1,0)),
         Descontento_inestabilidad = factor(Descontento_inestabilidad, levels = c(1:2), labels = c(1,0)),
         Descontento_ambiente = factor(Descontento_ambiente, levels = c(1:2), labels = c(1,0)),
         Descontento_trabajo_calle = factor(Descontento_trabajo_calle, levels = c(1:2), labels = c(1,0)),
         Descontento_accidente = factor(Descontento_accidente, levels = c(1:2), labels = c(1,0)),
         Descontento_actividades = factor(Descontento_actividades, levels = c(1:2), labels = c(1,0)),
         Descontento_progreso = factor(Descontento_progreso, levels = c(1:2), labels = c(1,0)),
         Descontento_malas_rel_lab = factor(Descontento_malas_rel_lab, levels = c(1:2), labels = c(1,0)),
         ciiu4 = as.factor(ciiu),
         fecha_1= as.Date(paste0(ano, "01"), format = "%Y%m%d"),
         ciiu4_fct = fct_collapse(ciiu4,
                                  "Agricultura, ganadería,  silvicultura y pesca" = "1",
                                  "Explotación de minas y canteras" = "2",
                                  "Industrias manufactureras" = "3",
                                  "Suministro de electricidad, gas, vapor y aire acondicionado" = "4",
                                  "Distribución de agua; alcantarillado, gestión de desechos y actividades de saneamiento." = "5",
                                  "Construcción" = "6",
                                  "Comercio al por mayor y al por menor; reparación de vehículos automotores y motocicletas" = "7",
                                  "Transporte y almacenamiento" = "8",
                                  "Actividades de alojamiento y de servicio de comidas" = "9",
                                  "Información y comunicación" = "10",
                                  "Actividades financieras y de seguros" = "11",
                                  "Actividades inmobiliarias" = "12",
                                  "Actividades profesionales, científicas y técnicas" = "13",
                                  "Actividades de servicios administrativos y de apoyo" = "14",
                                  "Administración pública y defensa; planes de seguridad social de afiliación obligatoria" = "15",
                                  "Enseñanza" = "16",
                                  "Actividades de atención de la salud humana y de asistencia social" = "17",
                                  "Artes, entretenimiento y recreación" = "18",
                                  "Otras actividades de servicios" = "19",
                                  "Actividades de los hogares como empleadores; actividades no diferenciadas de los hogares como productores de bienes y servicios para uso propio" = "20",
                                  "Organizaciones internacionales" = "21"))

# Exportar df_enemdu a Excel

write.xlsx(df_enemdu, "df_enemdu.xlsx")







