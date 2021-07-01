#---------------------------------------------------------------------------------------------------------#
# Title: EJEMPLO DE APLICACION DE METODOLOGIA CRISP-DM EN PROYECTO DE ANALISIS DE SENTIMIENTO             #
# Datos: Recopilacion de publicaciones de Twitter relacionadas as COVID/ QUARANTINE & HOBBIES             #
#        PASO 02 (COMPRENSION DE LOS DATOS) Y PASO 03 (PREPARACION DE LOS DATOS)                          #
#                                                                                                         #
# Autor: Paula Munoz                                                                                      #
#                                                                                                         #
# Fecha: 10/17/2020                                                                                       #
#---------------------------------------------------------------------------------------------------------#


### IMPORTANTE: Con la cuenta de developer standard (Gratis) tenemos algunas limitaciones como:
#   1. Solo nos va arrojar resulatados de los ultimos 7 o 8 dias, asi que si queremos un recolectar mas datos
#      tendremos que repetir este proceso varias veces o una vez por semana
#
#   2. Solo se pueden descargar hasta maximo 18000 publicaciones en 15 minutos, asi que es importante
#      usar el parametro de retryonratelimit = TRUE  en la funcion de search_tweets o search_tweets2


### PASO 2 - CRISP-DM: COMPRENSION DE LOS DATOS ----

# * Cuales son las fuentes de datos disponibles?
# * Recoleccion inicial de datos
# * Exploracion inicial de los datos
# * Analisis descriptivo (Tablas de frecuencia, graficos de distribucion)
# * Descripcion de los datos: Tipos de datos (Nominales/ Categoricos)
# * Verificar Calidad de los datos



## 2.1 INSTALAR Y CARGAR LIBRERIAS ----

#install.packages("rtweet")   #Quitar comentario "#" si necesita instalar libreria


library(rtweet)
library(tidyverse)
library(DataExplorer)
library(tidytext) 



## 2.2 INTERACTUAR CON EL API DE TWITTER ----

#      Asegurarse de estar conectado en su cuenta de desarrollador de Twitter  
#      Referirse a las credenciales proporcionadas por Twitter (Consumer API Keys)


# * 2.2.1  Crear Token por primera vez ----
#        (Ir  a "2.2.2 Obtener token" si ya ha creado el token en su computador previamente)
#        Al crear el token debe abrirse una ventana en su web browser para comprobar la autenticacion con Twitter


# Guardar API keys (Borrar despues de Cargar)

#api_key <- "reemplazar con key"
#api_secret_key <- "reemplazar con secret key"


# Crear Token

#twitter_token <- create_token(
 #   app = "Nombre de su app",   ## << Nombre de su APP info en su cuenta de Developer
  #  consumer_key = api_key,
   # consumer_secret = api_secret_key)



# * 2.2.2 Obtener Token ----
#       Se abrirá una ventana en su web browser para comprobar la autenticación con Twitter

get_token()



## 2.3 BUSCAR PUBLICACIONES ----

#      Funciones **search_tweets** (Una sola busqueda) o **search_tweets2** (varias busquedas)
#      PARA VARIAS BUSQUEDAS A LA VEZ SALTAR A 2.3.2


# * 2.3.1 Ejemplo busqueda individual ----

#     Vamos a hacer una busqueda de tweets con los terminos "quarantine hobby"

quarantine_hobby_01 <- search_tweets(
    q = "quarantine hobby", 
    n = 10000,                
    #lang = "en",             
    type = "mixed",
    include_rts = FALSE,       
    retryonratelimit = TRUE    
)


# 2.4 EXPLORACION INICIAL DE LOS DATOS ( Busqueda sencilla) ----

# Dimensiones (Numero de observaciones x numero de columnas)
quarantine_hobby_01 %>%
    dim()


## Primeras 5 lineas - Preview

quarantine_hobby_01 %>%
    head() %>%
    View()


## glimpse - Vista transpuesta 

quarantine_hobby_01 %>%
    glimpse()


## Sin datos? (Missing values)
plot_missing(quarantine_hobby_01)


## Histogram - Distribucion de variables continuas
plot_histogram(quarantine_hobby_01)



quarantine_hobby_01$text


quarantine_hobby_01$created_at %>%
    summary()


# * 2.3.2 Busqueda de multiples combinaciones ----


covid_hobbies_multiple_01 <-  search_tweets2(
    c("quarantine hobbies","#CovidHobbies", "quarantine hobbies", "covid hobbies", 
      "covid hobby", "quarantine hobby", "#StayHome Hobby", 
      "#StayHome Hobbies", "#quedatenencasa hobbies", "cuarentena hobbies",  
      "cuarentena hobby", "#cuarentena hobby", "#cuarentena hobbies", 
      "cuarentena #hobbies", "cuarentena #hobby", "#yomequedoencasa hobby", 
      "#pandemiahobbies","pandemia hobbies",  "pandemia hobby", 
      "#Pandemia hobby", "#Pandemia hobbies", "pandemia #hobbies", 
      "pandemia #hobby", "#pandemicHobbies",
      "pandemic hobbies", "pandemic hobby"),
    n = 18000,                  
    #lang = "en",              
    type = "mixed",
    include_rts = FALSE,       
    retryonratelimit = TRUE
) 


# 2.4 EXPLORACION INICIAL DE LOS DATOS ( Busqueda Multiple) ----

# Dimensiones (Numero de observaciones x numero de columnas)
covid_hobbies_multiple_01 %>%
    dim()

## Primeras 5 lineas - Preview

covid_hobbies_multiple_01 %>%
    head() %>%
    View()


## glimpse - Vista transpuesta 

covid_hobbies_multiple_01 %>%
    glimpse()


# Fechas
covid_hobbies_multiple_01$created_at %>%
    summary()


# Que busqueda/ terminos esta arrojando mas resultados?
covid_hobbies_multiple_01 %>%
    group_by(query) %>%
    summarise(Total = n()) %>%
    arrange(desc(Total)) %>%
    View()


# En que lenguajes aparecen los tweets?
covid_hobbies_multiple_01 %>%
    group_by(lang) %>%
    summarise(Total = n()) %>%
    arrange(desc(Total))



# Vistazo a tweets en Español
covid_hobbies_multiple_01 %>%
    filter(lang == "es") %>%
    select(text)



## Sin datos? (Missing values)
plot_missing(covid_hobbies_multiple_01)



## Histogram - Distribucion de variables continuas
plot_histogram(covid_hobbies_multiple_01)




# **************************************************** ----


# PASO 3 - CRISP-DM: PREPARACION DE LOs DATOS ----

# * Seleccionar Variables de Interes
# * Limpiar los datos
# * Aplicar formato deseado
# * Integrar/ Fusionar / Generar nuevas variables
# * Si los datos son muy grandes se pueden aplicar tecnias de muestreo para trabajar con dataset mas manejable


# 3.1 SELECCIONAR VARIABLES DE INTERES  ----
#     Para simplificar el ejemplo de hoy vamos a trabajar solo 
#     con tweets en Ingles para poder hacer analisis de Sentimento


covid_hobbies_multiple_02 <- covid_hobbies_multiple_01 %>%
    select(user_id, created_at, text, source, hashtags, lang, country, location, query) %>%
    filter(lang == "en") %>%
    # Eliminar observaciones que se repiten
    unique() 


covid_hobbies_multiple_02%>%
  dim()


covid_hobbies_multiple_02


covid_hobbies_multiple_02 %>%
    plot_missing()


covid_hobbies_multiple_02 %>%
    plot_bar()



# * En este punto tenemos un dataset con todas las variables de interes (sin limpiar) ----

covid_hobbies_multiple_all_03 <- covid_hobbies_multiple_02





# 3.2 ESTRUCTURACION ----

# 3.2.1 Crear un dataset enfocado al analisis del texto de la publicacion
#Duplicar variable "text" y llamarla "text_clean" con el fin usar esta variable para limpiar texto de los tweets----


covid_hobbies_multiple_text_04 <- covid_hobbies_multiple_all_03%>%
  mutate(text_clean = text) 

covid_hobbies_multiple_text_04

# 3.2.2 seleccionar solo Variables de interes relacionadas al texto: "user_id", "created_at", "text" y "text_clean"

covid_hobbies_multiple_text_04 <- covid_hobbies_multiple_text_04 %>%
    select(user_id,created_at, text, text_clean) %>%
    unique()

covid_hobbies_multiple_text_04

covid_hobbies_multiple_text_04 %>%
  dim()



# 3.3 LIMPIAR DATOS ----
#     Limpiar Texto de la variable text_clean, y para esto usamos Regular Expressions

covid_hobbies_multiple_text_04$text_clean <- covid_hobbies_multiple_text_04$text_clean %>%   
    gsub("http://t.co/[a-z,A-Z,0-9]*{8}","", .) %>%  # Eliminar http links
    gsub("https://t.co/[a-z,A-Z,0-9]*{8}","", .) %>% # Eliminar https links
    iconv(from = "latin1", to = "ASCII", sub="") %>% # Eliminar Emojis y caracteres speciales 
    gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>%   # Eliminar re-tweet entities 
    gsub("@\\w+", "", .) %>%                         # Eliminar @persona
    gsub("[[:punct:]]", " ", .) %>%                  # Eliminar puntuaciones
    gsub("[[:digit:]]", "", .) %>%                   # Eliminar digitos
    gsub("\n", " ", .) %>%                           # Eliminar \n 
    gsub("[ \t]{2,}", " ", .) %>%                    # Eliminar espacios innecesarios
    gsub("^\\s+|\\s+$", "", .) %>%                   # Eliminar espacios innecesarios
    tolower()                                        # convertir a minuscula



covid_hobbies_multiple_text_04 %>% 
    select(text, text_clean) 

covid_hobbies_multiple_text_04 



# 3.4 EXPORTAR PRIMERAS VERSIONES DE NUESTROS DATASETS PARA EXPLORARLOS EN OTRO PROGRAMA (Tableau Prep) ----

# * Dataset 1: Enfocado al analisis de texto
#   **** IMPORTANTE: AJUSTAR LA FECHA EN EL NOMBRE DEL ARCHIVO EN CADA ITERATION Y GUARDARLO SIEMPRE EN EL MISMO FOLDER PARA QUE TABLEAU PRE LOS PUEDA LEER AUTOMATICAMENTE
covid_hobbies_multiple_text_04 %>%
    save_as_csv("step02_and_step03_data_understanding_and_prep/Data_collection_clean_01/covid_hobbies_clean_text_01_20201018pm.csv", prepend_ids = TRUE, na = "",
                fileEncoding = "UTF-8")


# * Dataset 2: Dataset con todas las variables (Antes de limpiar)
covid_hobbies_multiple_all_03 %>%
  save_as_csv("step02_and_step03_data_understanding_and_prep/Data_collection_not_clean_01/covid_hobbies_not_clean_all_01_20201018pm.csv", prepend_ids = TRUE, na = "",
              fileEncoding = "UTF-8")


#   **** Vamos a Continuar la exploracion y limpieza de nuestros datasets en Tableau Prep Builder **** ----
