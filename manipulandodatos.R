
# Manipulando, analizando y exportando datos con tydiverse
## Lección 3 del episodio Análisis y visualización de datos en R para ecológxs 
### Datacarpentry



###
install.packages("tidyverse")
library(tidyverse)

# Crear carpetas para datos y scripts

dir.create("scripts")

# Descargamos la base de datos con la que trabajaremos

download.file(url = "https://ndownloader.figshare.com/files/2292169",
              destfile = "data_raw/portal_data_joined.csv")

# leer y nombrar la base de datos
base_datos <- read_csv("data_raw/portal_data_joined.csv")

# inspeccionar los datos
str(base_datos)

# visualizar los datos
view(base_datos)

#seleccionar columnas
select(base_datos, plot_id, species_id, weight)

#seleccionar columnas excepto alguna (-)
select(base_datos, plot_id, species_id, -weight)

#Elegir columnas basadas en un criterio específico

filter(base_datos, year == 1995)

#Seleccionar y filtrar al mismo tiempo


surveys_sml <- select(filter(base_datos, weight < 5), species_id, sex, weight)

#Crear una nueva columna de peso en kg
base_datos %>%
  mutate(weight_kg = weight / 1000,
         weight_lb = weight_kg * 2.2)

#Las primeras filas de la salida están llenas de NA, por lo que si quisiéramos eliminarlas podríamos insertar un filtro en la cadena
#is.na() es una función que determina si algo es NA. El ! El símbolo niega el resultado, por lo que solicitamos cada fila donde el peso no sea NA

base_datos %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

#############RETO###########################

#Muchas tareas de análisis de datos se pueden abordar utilizando 
#el paradigma dividir-aplicar-combinar: 
#dividir los datos en grupos, aplicar algún análisis a cada grupo y 
#luego combinar los resultados.

#Para calcular el peso medio por sexo

base_datos %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

#También podemos agrupar por múltiples columnas

base_datos %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE)) %>%
  tail()

#En el comando anterior, 
#la columna mean_weight resultante no contiene NA sino NaN (que se refiere a "No es un número") 
#porque se llamó a mean() en un vector de valores de NA y al mismo tiempo se establecía na.rm = TRUE.

base_datos %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

#Agregar una columna que indique el peso mínimo de cada especie para cada sexo

base_datos %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight))
#Reorganizar el resultado para poner primero las especies más ligeras

base_datos %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(min_weight)

#Por orden decreciente de peso medio

base_datos %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight),
            min_weight = min(weight)) %>%
  arrange(desc(mean_weight))

#Contar el número de filas de datos de cada sexo

base_datos %>%
  count(sex)

#Contar una combinación de factores, como sexo y especie

base_datos %>%
  count(sex, species)

#Organizar la tabla anterior en 
#(i) orden alfabético de los niveles de las especies y 
#(ii) en orden descendente del recuento

base_datos %>%
  count(sex, species) %>%
  arrange(species, desc(n))

###### REORGANIZANDO LOS DATOS ######
# Comparar los diferentes pesos medios de cada género entre parcelas
#FORMATO LARGO AL ANCHO

base_gw <- base_datos %>%
  filter(!is.na(weight)) %>%
  group_by(plot_id, genus) %>%
  summarize(mean_weight = mean(weight))

base_wide <- base_gw %>%
  pivot_wider(names_from = genus, values_from = mean_weight)

# Completamos los valores perdidos
base_gw %>%
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0) %>%
  head()

#FORMATO ANCHO A LARGO

base_long <- base_wide %>%
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id)

base_long2 <- base_wide %>%
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = Baiomys:Spermophilus)

#####RETO#####
?n_distinct

base_wide_gen <- base_datos %>%
  group_by(plot_id, year) %>%
  summarize(n_genera = n_distinct(genus)) %>%
  pivot_wider(names_from = year, values_from = n_genera) 

head(base_wide_gen)

base_wide_gen %>%
  pivot_longer(names_to = "year", values_to = "n_genera", cols = -plot_id)

####guardar 
#creamos la carpeta data para datos manipulados y guardamos

dir.create("data")
write_csv(base_long, file = "data/base_long.csv")
