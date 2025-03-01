Prácticas de aula 3 (PA03). Agrupar provincias según su riqueza relativa
de especies por géneros de Bromeliaceae<small><br>Biogeografía
(GEO-131)<br>Universidad Autónoma de Santo Domingo (UASD)<br>Semestre
2025-01</small>
================
El Tali
2025-03-01

Versión HTML (quizá más legible),
[aquí](https://biogeografia-master.github.io/agrupamiento-por-riqueza/README.html)

# Fecha/hora de entrega

**[VER PORTAL DE LA
ASIGNATURA](https://github.com/biogeografia-202501)**

# Introducción

La biogeografía examina patrones de diversidad biológica (en sentido
amplio) en el espacio geográfico y a través del tiempo (Lomolino et al.
2006). En este contexto, el análisis de la riqueza de especies por
géneros de Bromeliaceae, puede ofrecer información valiosa sobre la
diversidad y la riqueza relativa de especies en diferentes regiones
geográficas. La familia Bromeliaceae, que incluye al género
*Tillandsia*, presenta una alta diversidad en la República Dominicana y
es un buen ejemplo para estudiar patrones biogeográficos. No obstante,
los géneros monoespecíficos (representados por una especie), también son
relevantes. Por esta razón, caracterizar cómo se agrupan las provincias
según riqueza de especies de cada género, es un camino prometedor.

Para analizar cómo se agrupan las provincias dominicanas según la
riqueza de especies por género, utilizaremos técnicas de agrupamiento
jerárquico, como el método de agrupamiento jerárquico aglomerativo
promedio no ponderado (UPGMA) (Borcard, Gillet, and Legendre 2018). Este
método agrupa elementos, en este caso provincias, en función de sus
características similares, permitiendo identificar patrones y relaciones
entre diferentes unidades territoriales basadas en la riqueza relativa
de especies.

En esta práctica, utilizaremos datos de riqueza relativa de especies por
género de Bromeliaceae en diversas provincias. Usando datos de GBIF
(GBIF.org 2024), me ocupé de calcular primero la riqueza relativa de
especies por género en cada provincia, así que la tabla base ya está
disponible. A título informativo, este mapa te muestra los registros de
especies de la familia Bromeliaceae según GBIF:

![](registros-provincias.jpg)

Te pido dos cosas en esta práctica: 1) Evaluar tu rendimiento calculando
distancia euclidiana usando los datos de riqueza relativa de especies de
bromelias según género; 2) Aplicar el método de UPGMA para agrupar las
provincias en función de estas características. Este enfoque te
permitirá identificar agrupamientos naturales de provincias con riqueza
de especies similares, facilitando la comprensión de la distribución
biogeográfica de la familia Bromeliaceae.

# Ejercicio. Clasificar 6 provincias dominicanas según su riqueza relativa de especies por géneros de Bromeliaceae por el método de agrupamiento jerárquico aglomerativo promedio no ponderado (UPGMA)

## Objetivo

Agrupar provincias de la República Dominicana según su riqueza relativa
de especies por géneros de Bromeliaceae utilizando el método de
agrupamiento jerárquico aglomerativo promedio no ponderado (UPGMA), con
el fin de identificar patrones biogeográficos comunes y diferenciar
unidades territoriales en función de su diversidad de géneros.

## Planteamiento del Problema

Se dispone del archivo `data/riqueza_relativa_provincias_sf.gpkg`, que
contiene la riqueza relativa de especies por género de Bromeliaceae en
diferentes provincias de la República Dominicana, obtenido a partir de
Martínez-Batlle (2022). Usando estos datos realizarás un agrupamiento de
las provincias según su riqueza relativa de especies por género de
bromelias, aplicando el método de agrupamiento jerárquico UPGMA. Este
agrupamiento permitirá identificar similitudes y diferencias en la
riqueza relativa de especies por géneros entre las provincias,
facilitando el análisis y la comprensión de los patrones biogeográficos
a nivel regional.

## Obtención de los datos

> En lo adelante, se incluyen varios bloques de código. En la versión
> HTML, estos aparecen ocultos por defecto. Si deseas verlos, presiona
> el botón `Show`.

1.  Cargar datos fuente. Los datos fueron obtenidos luego de procesar la
    fuente Martínez-Batlle (2022).

Primero, es necesario cargar paquetes.

``` r
library(tidyverse)
library(sf)
library(tmap)
library(cowplot)
```

Luego leemos los datos.

``` r
prov <- st_read('data/riqueza_relativa_provincias_sf.gpkg', quiet = T) %>% 
  rename(nombre = TOPONIMIA) %>% select(-PROV, -REG, -ENLACE)
# Comprobar 100%
# prov %>% st_drop_geometry() %>% select(-nombre) %>% rowSums(na.rm = T)
```

Una representación cartográfica te ayudará a ver las distribuciones
porcentuales a lo largo del país.

``` r
# Transformar los datos
prov_long <- prov %>%
  pivot_longer(names_to = 'variable', values_to = 'value', -c(geom, nombre))

# Lista de variables únicas
variables <- unique(prov_long$variable)

# Crear mapas en gráficos separados, forzando la misma escala
maps <- map(variables, function(var) {
  prov_long %>%
    filter(variable == var) %>%
    ggplot(aes(fill = value, geometry = geom)) +
    geom_sf(color = 'grey15', size = 0.3) +
    scale_fill_distiller(palette = "BrBG", direction = -1, name = "Riqueza Relativa") +
    ggtitle(var) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"), # Tamaño de los cuadrados de la leyenda
      legend.text = element_text(size = 6), # Tamaño del texto de la leyenda
      legend.title = element_text(size = 7), # Tamaño del título de la leyenda
      legend.spacing.x = unit(0.2, 'cm'), # Espaciado horizontal de la leyenda
      legend.spacing.y = unit(0.2, 'cm')  # Espaciado vertical de la leyenda

    )
})

# Combinar los mapas en una sola figura con leyenda común
combined_map <- plot_grid(plotlist = maps, labels = LETTERS[1:length(maps)], label_size = 10, nrow = 3)

# Guardar el mapa combinado
ggsave("salidas/riqueza_relativa_provincias.png", combined_map, width = 10, height = 10, dpi = 300)
```

<img src="salidas/riqueza_relativa_provincias.png" width="100%" />

2.  **Creación de los 30 conjuntos** (reserva el conjunto 1 al Tali). Se
    han creado 30 conjuntos, cada uno de seis provincias elegidas al
    azar utilizando sus nombres.

``` r
prov <- prov %>% filter(nombre != 'HERMANAS MIRABAL')
set.seed(123)
replicas_1 <- replicate(1, sample(prov$nombre, 6))
set.seed(321)
replicas_n_1 <- replicate(n_conjuntos - 1, sample(prov$nombre, 6))
replicas <- cbind(replicas_1, replicas_n_1)
df <- data.frame(Conjunto = 1:n_conjuntos, t(replicas))
df_conjuntos <- df %>%
  unite("Provincias asignadas", X1:X6, sep = ", ")
df_conjuntos %>% 
  knitr::kable()
```

| Conjunto | Provincias asignadas                                                                           |
|---------:|:-----------------------------------------------------------------------------------------------|
|        1 | SANTO DOMINGO, MONTE CRISTI, SAMANÁ, MARÍA TRINIDAD SÁNCHEZ, BAORUCO, INDEPENDENCIA            |
|        2 | SAN PEDRO DE MACORÍS, PUERTO PLATA, HATO MAYOR, LA VEGA, SANTIAGO RODRÍGUEZ, SANTIAGO          |
|        3 | PEDERNALES, VALVERDE, PERAVIA, BARAHONA, MONTE CRISTI, LA ALTAGRACIA                           |
|        4 | SANTIAGO RODRÍGUEZ, MONTE PLATA, PUERTO PLATA, SAN PEDRO DE MACORÍS, MONTE CRISTI, ESPAILLAT   |
|        5 | AZUA, LA ALTAGRACIA, PUERTO PLATA, BARAHONA, MARÍA TRINIDAD SÁNCHEZ, SANCHEZ RAMÍREZ           |
|        6 | AZUA, BARAHONA, SAN CRISTÓBAL, SAN JOSÉ DE OCOA, PEDERNALES, SAMANÁ                            |
|        7 | PEDERNALES, SAN JOSÉ DE OCOA, LA VEGA, MONTE PLATA, HATO MAYOR, SANTO DOMINGO                  |
|        8 | SANCHEZ RAMÍREZ, DISTRITO NACIONAL, ESPAILLAT, MONSEÑOR NOUEL, MARÍA TRINIDAD SÁNCHEZ, LA VEGA |
|        9 | ELÍAS PIÑA, SAN JOSÉ DE OCOA, PERAVIA, VALVERDE, EL SEIBO, ESPAILLAT                           |
|       10 | MONTE CRISTI, DUARTE, HATO MAYOR, BAORUCO, ESPAILLAT, BARAHONA                                 |
|       11 | PEDERNALES, MARÍA TRINIDAD SÁNCHEZ, DISTRITO NACIONAL, LA VEGA, MONTE CRISTI, ELÍAS PIÑA       |
|       12 | BAORUCO, LA VEGA, SANTO DOMINGO, SAN CRISTÓBAL, PUERTO PLATA, SAN PEDRO DE MACORÍS             |
|       13 | LA VEGA, SANTO DOMINGO, BAORUCO, ESPAILLAT, LA ROMANA, DISTRITO NACIONAL                       |
|       14 | VALVERDE, BAORUCO, MONTE CRISTI, SANTIAGO, HATO MAYOR, SAN JUAN                                |
|       15 | SANTO DOMINGO, VALVERDE, SAN CRISTÓBAL, HATO MAYOR, AZUA, SAN JOSÉ DE OCOA                     |
|       16 | SANCHEZ RAMÍREZ, DUARTE, BAORUCO, LA VEGA, SANTIAGO RODRÍGUEZ, SAN PEDRO DE MACORÍS            |
|       17 | SANCHEZ RAMÍREZ, PUERTO PLATA, MARÍA TRINIDAD SÁNCHEZ, LA ROMANA, HATO MAYOR, SANTO DOMINGO    |
|       18 | LA VEGA, EL SEIBO, PERAVIA, MONTE CRISTI, SAN JUAN, PUERTO PLATA                               |
|       19 | MARÍA TRINIDAD SÁNCHEZ, MONTE CRISTI, SANTIAGO RODRÍGUEZ, SAN JOSÉ DE OCOA, SAMANÁ, BARAHONA   |
|       20 | AZUA, ESPAILLAT, ELÍAS PIÑA, LA VEGA, SAMANÁ, MONTE PLATA                                      |
|       21 | MONTE PLATA, SAN JOSÉ DE OCOA, AZUA, MARÍA TRINIDAD SÁNCHEZ, BAORUCO, SANCHEZ RAMÍREZ          |
|       22 | DISTRITO NACIONAL, LA VEGA, SAN JUAN, DUARTE, BAORUCO, HATO MAYOR                              |
|       23 | PEDERNALES, DAJABÓN, BAORUCO, EL SEIBO, MONSEÑOR NOUEL, SANCHEZ RAMÍREZ                        |
|       24 | PUERTO PLATA, LA ALTAGRACIA, MONTE CRISTI, ELÍAS PIÑA, PERAVIA, SANTO DOMINGO                  |
|       25 | PEDERNALES, SAN PEDRO DE MACORÍS, BARAHONA, LA VEGA, MONTE PLATA, BAORUCO                      |
|       26 | DAJABÓN, LA ALTAGRACIA, BARAHONA, VALVERDE, PEDERNALES, LA VEGA                                |
|       27 | SAMANÁ, LA ROMANA, PERAVIA, MARÍA TRINIDAD SÁNCHEZ, SAN PEDRO DE MACORÍS, AZUA                 |
|       28 | SAMANÁ, SAN JUAN, SANTO DOMINGO, MARÍA TRINIDAD SÁNCHEZ, BARAHONA, BAORUCO                     |
|       29 | SANTIAGO, SAN JOSÉ DE OCOA, LA ALTAGRACIA, MONSEÑOR NOUEL, MONTE PLATA, LA VEGA                |
|       30 | DUARTE, SANCHEZ RAMÍREZ, LA VEGA, SAN JUAN, EL SEIBO, DISTRITO NACIONAL                        |

**Nota: Todos los conjuntos de provincias son únicos.**

3.  **Presentación de los datos crudos de cada uno de los 30
    conjuntos**. Esta matriz contiene la riqueza relativa (en tanto por
    ciento) de especies de bromelias según género de la base de datos
    GBIF (GBIF.org 2024). Con esta matriz podrás hacer un cálculo de
    distancia (parte 1 del mandato, que verás en la siguiente sección).

``` r
conjuntos_l <- sapply(1:ncol(replicas),
       function(x)
         prov %>%
         filter(nombre %in% replicas[, x]) %>% st_drop_geometry(),
       simplify = F) %>%
  setNames(paste0('Conjunto ', 1:n_conjuntos))
conjuntos_l_k <- lapply(
  conjuntos_l,
  function(x) {
    colnames(x) <- gsub('\\n', '', colnames(x))
    knitr::kable(x, digits = 2, align = 'c')
    })
```

``` r
# Imprimir tablas
for (tabla in 1:length(conjuntos_l_k)) {
  cat('Conjunto', tabla, "\n\n")
  print(conjuntos_l_k[[tabla]])
  cat("\n\n")
}
```

Conjunto 1

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|        BAORUCO         |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |  0.00  |     0      |   0.00   |  0.00   |     0      |
|     INDEPENDENCIA      |   0.00   |  10.53   |   68.42    |   0.00   |   10.53    |   0.00   |    0    |  0.00  |     0      |   5.26   |  5.26   |     0      |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |  0.00  |     0      |   0.00   |  0.00   |     0      |
|      MONTE CRISTI      |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |  0.00  |     0      |   0.00   |  0.00   |     0      |
|         SAMANÁ         |   0.00   |  10.00   |   40.00    |  10.00   |   20.00    |   0.00   |    0    | 10.00  |     0      |   0.00   |  10.00  |     0      |
|     SANTO DOMINGO      |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.00   |    0    |  8.33  |     0      |   0.00   |  8.33   |     0      |

Conjunto 2

|        nombre        | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:--------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       LA VEGA        |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |   0    |    2.78    |   5.56   |  2.78   |    0.00    |
|     PUERTO PLATA     |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    7.14    |
| SAN PEDRO DE MACORÍS |  25.00   |   0.00   |   75.00    |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    0.00    |
|       SANTIAGO       |   6.67   |  13.33   |   60.00    |  20.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    0.00    |
|  SANTIAGO RODRÍGUEZ  |   0.00   |   9.09   |   90.91    |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    0.00    |
|      HATO MAYOR      |   0.00   |  10.00   |   65.00    |   0.00   |   10.00    |   0.00   |    0    |   5    |    0.00    |   5.00   |  5.00   |    0.00    |

Conjunto 3

|    nombre     | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   BARAHONA    |   0.00   |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |    0    |     0      |
| LA ALTAGRACIA |   7.69   |   0.00   |   76.92    |  15.38   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
| MONTE CRISTI  |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|  PEDERNALES   |   0.00   |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|    PERAVIA    |   0.00   |  11.11   |   77.78    |  11.11   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|   VALVERDE    |   0.00   |  14.29   |   71.43    |   0.00   |   14.29    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |

Conjunto 4

|        nombre        | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:--------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|      ESPAILLAT       |   0.00   |   8.33   |   66.67    |   8.33   |    0.00    |   8.33   |  8.33   |   0    |     0      |    0     |    0    |    0.00    |
|     MONTE CRISTI     |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |   0    |     0      |    0     |    0    |    0.00    |
|     PUERTO PLATA     |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |   0.00   |  0.00   |   0    |     0      |    0     |    0    |    7.14    |
| SAN PEDRO DE MACORÍS |  25.00   |   0.00   |   75.00    |   0.00   |    0.00    |   0.00   |  0.00   |   0    |     0      |    0     |    0    |    0.00    |
|  SANTIAGO RODRÍGUEZ  |   0.00   |   9.09   |   90.91    |   0.00   |    0.00    |   0.00   |  0.00   |   0    |     0      |    0     |    0    |    0.00    |
|     MONTE PLATA      |  12.50   |  12.50   |   75.00    |   0.00   |    0.00    |   0.00   |  0.00   |   0    |     0      |    0     |    0    |    0.00    |

Conjunto 5

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|          AZUA          |   5.88   |   5.88   |   88.24    |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |    0.00    |
|        BARAHONA        |   0.00   |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |    0    |    0.00    |
|     LA ALTAGRACIA      |   7.69   |   0.00   |   76.92    |  15.38   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |    0.00    |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |    0.00    |
|      PUERTO PLATA      |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |    7.14    |
|    SANCHEZ RAMÍREZ     |   0.00   |   0.00   |   85.71    |  14.29   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |    0.00    |

Conjunto 6

|      nombre      | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       AZUA       |   5.88   |   5.88   |   88.24    |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|     BARAHONA     |   0.00   |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |  0.00   |     0      |
|    PEDERNALES    |   0.00   |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      SAMANÁ      |   0.00   |  10.00   |   40.00    |  10.00   |   20.00    |   0.00   |  0.00   | 10.00  |    0.00    |   0.00   |  10.00  |     0      |
|  SAN CRISTÓBAL   |   6.25   |   0.00   |   68.75    |   6.25   |    6.25    |   0.00   |  0.00   |  6.25  |    0.00    |   0.00   |  6.25   |     0      |
| SAN JOSÉ DE OCOA |   0.00   |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |  0.00   |  0.00  |    2.94    |   2.94   |  0.00   |     0      |

Conjunto 7

|      nombre      | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|     LA VEGA      |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
|    PEDERNALES    |   0.00   |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|   MONTE PLATA    |  12.50   |  12.50   |   75.00    |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|    HATO MAYOR    |   0.00   |  10.00   |   65.00    |   0.00   |   10.00    |   0.00   |    0    |  5.00  |    0.00    |   5.00   |  5.00   |     0      |
| SAN JOSÉ DE OCOA |   0.00   |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |    0    |  0.00  |    2.94    |   2.94   |  0.00   |     0      |
|  SANTO DOMINGO   |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.00   |    0    |  8.33  |    0.00    |   0.00   |  8.33   |     0      |

Conjunto 8

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   DISTRITO NACIONAL    |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |  0.00   |     0      |
|       ESPAILLAT        |   0.00   |   8.33   |   66.67    |   8.33   |    0.00    |   8.33   |  8.33   |   0    |    0.00    |   0.00   |  0.00   |     0      |
|        LA VEGA         |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |  0.00   |   0    |    2.78    |   5.56   |  2.78   |     0      |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |  0.00   |     0      |
|    SANCHEZ RAMÍREZ     |   0.00   |   0.00   |   85.71    |  14.29   |    0.00    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |  0.00   |     0      |
|     MONSEÑOR NOUEL     |   4.35   |  13.04   |   56.52    |   4.35   |   13.04    |   0.00   |  0.00   |   0    |    0.00    |   4.35   |  4.35   |     0      |

Conjunto 9

|      nombre      | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|    ELÍAS PIÑA    |    0     |   6.25   |   68.75    |   0.00   |    6.25    |  12.50   |  0.00   |   0    |    0.00    |   6.25   |    0    |     0      |
|     EL SEIBO     |    0     |  20.00   |   40.00    |  20.00   |    0.00    |   0.00   |  20.00  |   0    |    0.00    |   0.00   |    0    |     0      |
|    ESPAILLAT     |    0     |   8.33   |   66.67    |   8.33   |    0.00    |   8.33   |  8.33   |   0    |    0.00    |   0.00   |    0    |     0      |
|     PERAVIA      |    0     |  11.11   |   77.78    |  11.11   |    0.00    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |    0    |     0      |
|     VALVERDE     |    0     |  14.29   |   71.43    |   0.00   |   14.29    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |    0    |     0      |
| SAN JOSÉ DE OCOA |    0     |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |  0.00   |   0    |    2.94    |   2.94   |    0    |     0      |

Conjunto 10

|    nombre    | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   BAORUCO    |    0     |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|   BARAHONA   |    0     |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |    0    |     0      |
|    DUARTE    |    0     |   0.00   |   50.00    |  33.33   |    0.00    |   0.00   |  0.00   | 16.67  |    0.00    |   0.00   |    0    |     0      |
|  ESPAILLAT   |    0     |   8.33   |   66.67    |   8.33   |    0.00    |   8.33   |  8.33   |  0.00  |    0.00    |   0.00   |    0    |     0      |
| MONTE CRISTI |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|  HATO MAYOR  |    0     |  10.00   |   65.00    |   0.00   |   10.00    |   0.00   |  0.00   |  5.00  |    0.00    |   5.00   |    5    |     0      |

Conjunto 11

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   DISTRITO NACIONAL    |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |
|       ELÍAS PIÑA       |    0     |   6.25   |   68.75    |   0.00   |    6.25    |  12.50   |    0    |   0    |    0.00    |   6.25   |  0.00   |     0      |
|        LA VEGA         |    0     |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |   0    |    2.78    |   5.56   |  2.78   |     0      |
| MARÍA TRINIDAD SÁNCHEZ |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |
|      MONTE CRISTI      |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |
|       PEDERNALES       |    0     |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |

Conjunto 12

|        nombre        | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:--------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       BAORUCO        |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |    0.00    |
|       LA VEGA        |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |  0.00  |    2.78    |   5.56   |  2.78   |    0.00    |
|     PUERTO PLATA     |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |    7.14    |
|    SAN CRISTÓBAL     |   6.25   |   0.00   |   68.75    |   6.25   |    6.25    |   0.00   |    0    |  6.25  |    0.00    |   0.00   |  6.25   |    0.00    |
| SAN PEDRO DE MACORÍS |  25.00   |   0.00   |   75.00    |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |    0.00    |
|    SANTO DOMINGO     |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.00   |    0    |  8.33  |    0.00    |   0.00   |  8.33   |    0.00    |

Conjunto 13

|      nombre       | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
| DISTRITO NACIONAL |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      BAORUCO      |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|     ESPAILLAT     |   0.00   |   8.33   |   66.67    |   8.33   |    0.00    |   8.33   |  8.33   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|     LA ROMANA     |  12.50   |   0.00   |   75.00    |   0.00   |    0.00    |   0.00   |  12.50  |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      LA VEGA      |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |  0.00   |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
|   SANTO DOMINGO   |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.00   |  0.00   |  8.33  |    0.00    |   0.00   |  8.33   |     0      |

Conjunto 14

|    nombre    | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   BAORUCO    |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |   0    |     0      |   0.00   |    0    |     0      |
| MONTE CRISTI |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |     0      |   0.00   |    0    |     0      |
|   SAN JUAN   |   0.00   |  11.11   |   66.67    |   0.00   |   16.67    |   0.00   |    0    |   0    |     0      |   5.56   |    0    |     0      |
|   SANTIAGO   |   6.67   |  13.33   |   60.00    |  20.00   |    0.00    |   0.00   |    0    |   0    |     0      |   0.00   |    0    |     0      |
|   VALVERDE   |   0.00   |  14.29   |   71.43    |   0.00   |   14.29    |   0.00   |    0    |   0    |     0      |   0.00   |    0    |     0      |
|  HATO MAYOR  |   0.00   |  10.00   |   65.00    |   0.00   |   10.00    |   0.00   |    0    |   5    |     0      |   5.00   |    5    |     0      |

Conjunto 15

|      nombre      | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       AZUA       |   5.88   |   5.88   |   88.24    |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|  SAN CRISTÓBAL   |   6.25   |   0.00   |   68.75    |   6.25   |    6.25    |   0.00   |    0    |  6.25  |    0.00    |   0.00   |  6.25   |     0      |
|     VALVERDE     |   0.00   |  14.29   |   71.43    |   0.00   |   14.29    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|    HATO MAYOR    |   0.00   |  10.00   |   65.00    |   0.00   |   10.00    |   0.00   |    0    |  5.00  |    0.00    |   5.00   |  5.00   |     0      |
| SAN JOSÉ DE OCOA |   0.00   |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |    0    |  0.00  |    2.94    |   2.94   |  0.00   |     0      |
|  SANTO DOMINGO   |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.00   |    0    |  8.33  |    0.00    |   0.00   |  8.33   |     0      |

Conjunto 16

|        nombre        | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:--------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       BAORUCO        |    0     |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|        DUARTE        |    0     |   0.00   |   50.00    |  33.33   |    0.00    |   0.00   |    0    | 16.67  |    0.00    |   0.00   |  0.00   |     0      |
|       LA VEGA        |    0     |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
| SAN PEDRO DE MACORÍS |    25    |   0.00   |   75.00    |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|   SANCHEZ RAMÍREZ    |    0     |   0.00   |   85.71    |  14.29   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|  SANTIAGO RODRÍGUEZ  |    0     |   9.09   |   90.91    |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |

Conjunto 17

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       LA ROMANA        |  12.50   |   0.00   |   75.00    |   0.00   |    0.00    |    0     |  12.5   |  0.00  |     0      |    0     |  0.00   |    0.00    |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |    0     |   0.0   |  0.00  |     0      |    0     |  0.00   |    0.00    |
|      PUERTO PLATA      |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |    0     |   0.0   |  0.00  |     0      |    0     |  0.00   |    7.14    |
|    SANCHEZ RAMÍREZ     |   0.00   |   0.00   |   85.71    |  14.29   |    0.00    |    0     |   0.0   |  0.00  |     0      |    0     |  0.00   |    0.00    |
|       HATO MAYOR       |   0.00   |  10.00   |   65.00    |   0.00   |   10.00    |    0     |   0.0   |  5.00  |     0      |    5     |  5.00   |    0.00    |
|     SANTO DOMINGO      |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |    0     |   0.0   |  8.33  |     0      |    0     |  8.33   |    0.00    |

Conjunto 18

|    nombre    | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   EL SEIBO   |   0.00   |  20.00   |   40.00    |  20.00   |    0.00    |   0.00   |   20    |   0    |    0.00    |   0.00   |  0.00   |    0.00    |
|   LA VEGA    |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |   0    |    2.78    |   5.56   |  2.78   |    0.00    |
| MONTE CRISTI |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    0.00    |
|   PERAVIA    |   0.00   |  11.11   |   77.78    |  11.11   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    0.00    |
| PUERTO PLATA |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |    7.14    |
|   SAN JUAN   |   0.00   |  11.11   |   66.67    |   0.00   |   16.67    |   0.00   |    0    |   0    |    0.00    |   5.56   |  0.00   |    0.00    |

Conjunto 19

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|        BARAHONA        |    0     |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |    0    |     0      |
| MARÍA TRINIDAD SÁNCHEZ |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|      MONTE CRISTI      |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|         SAMANÁ         |    0     |  10.00   |   40.00    |  10.00   |   20.00    |   0.00   |  0.00   | 10.00  |    0.00    |   0.00   |   10    |     0      |
|   SANTIAGO RODRÍGUEZ   |    0     |   9.09   |   90.91    |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |    0    |     0      |
|    SAN JOSÉ DE OCOA    |    0     |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |  0.00   |  0.00  |    2.94    |   2.94   |    0    |     0      |

Conjunto 20

|   nombre    | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-----------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|    AZUA     |   5.88   |   5.88   |   88.24    |   0.00   |    0.00    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |  0.00   |     0      |
| ELÍAS PIÑA  |   0.00   |   6.25   |   68.75    |   0.00   |    6.25    |  12.50   |  0.00   |   0    |    0.00    |   6.25   |  0.00   |     0      |
|  ESPAILLAT  |   0.00   |   8.33   |   66.67    |   8.33   |    0.00    |   8.33   |  8.33   |   0    |    0.00    |   0.00   |  0.00   |     0      |
|   LA VEGA   |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |  0.00   |   0    |    2.78    |   5.56   |  2.78   |     0      |
|   SAMANÁ    |   0.00   |  10.00   |   40.00    |  10.00   |   20.00    |   0.00   |  0.00   |   10   |    0.00    |   0.00   |  10.00  |     0      |
| MONTE PLATA |  12.50   |  12.50   |   75.00    |   0.00   |    0.00    |   0.00   |  0.00   |   0    |    0.00    |   0.00   |  0.00   |     0      |

Conjunto 21

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|          AZUA          |   5.88   |   5.88   |   88.24    |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |    0    |     0      |
|        BAORUCO         |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |   0    |    0.00    |   0.00   |    0    |     0      |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |    0    |     0      |
|    SANCHEZ RAMÍREZ     |   0.00   |   0.00   |   85.71    |  14.29   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |    0    |     0      |
|      MONTE PLATA       |  12.50   |  12.50   |   75.00    |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |    0    |     0      |
|    SAN JOSÉ DE OCOA    |   0.00   |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |    0    |   0    |    2.94    |   2.94   |    0    |     0      |

Conjunto 22

|      nombre       | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
| DISTRITO NACIONAL |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      BAORUCO      |    0     |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      DUARTE       |    0     |   0.00   |   50.00    |  33.33   |    0.00    |   0.00   |    0    | 16.67  |    0.00    |   0.00   |  0.00   |     0      |
|      LA VEGA      |    0     |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
|     SAN JUAN      |    0     |  11.11   |   66.67    |   0.00   |   16.67    |   0.00   |    0    |  0.00  |    0.00    |   5.56   |  0.00   |     0      |
|    HATO MAYOR     |    0     |  10.00   |   65.00    |   0.00   |   10.00    |   0.00   |    0    |  5.00  |    0.00    |   5.00   |  5.00   |     0      |

Conjunto 23

|     nombre      | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:---------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|     BAORUCO     |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |    0    |   0    |     0      |   0.00   |  0.00   |     0      |
|     DAJABÓN     |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |   0    |     0      |   0.00   |  0.00   |     0      |
|    EL SEIBO     |   0.00   |  20.00   |   40.00    |  20.00   |    0.00    |   0.00   |   20    |   0    |     0      |   0.00   |  0.00   |     0      |
|   PEDERNALES    |   0.00   |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |    0    |   0    |     0      |   0.00   |  0.00   |     0      |
| SANCHEZ RAMÍREZ |   0.00   |   0.00   |   85.71    |  14.29   |    0.00    |   0.00   |    0    |   0    |     0      |   0.00   |  0.00   |     0      |
| MONSEÑOR NOUEL  |   4.35   |  13.04   |   56.52    |   4.35   |   13.04    |   0.00   |    0    |   0    |     0      |   4.35   |  4.35   |     0      |

Conjunto 24

|    nombre     | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|  ELÍAS PIÑA   |   0.00   |   6.25   |   68.75    |   0.00   |    6.25    |   12.5   |    0    |  0.00  |     0      |   6.25   |  0.00   |    0.00    |
| LA ALTAGRACIA |   7.69   |   0.00   |   76.92    |  15.38   |    0.00    |   0.0    |    0    |  0.00  |     0      |   0.00   |  0.00   |    0.00    |
| MONTE CRISTI  |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.0    |    0    |  0.00  |     0      |   0.00   |  0.00   |    0.00    |
|    PERAVIA    |   0.00   |  11.11   |   77.78    |  11.11   |    0.00    |   0.0    |    0    |  0.00  |     0      |   0.00   |  0.00   |    0.00    |
| PUERTO PLATA  |   7.14   |  14.29   |   57.14    |   7.14   |    7.14    |   0.0    |    0    |  0.00  |     0      |   0.00   |  0.00   |    7.14    |
| SANTO DOMINGO |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.0    |    0    |  8.33  |     0      |   0.00   |  8.33   |    0.00    |

Conjunto 25

|        nombre        | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:--------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|       BAORUCO        |   0.0    |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|       BARAHONA       |   0.0    |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |  0.00   |     0      |
|       LA VEGA        |   0.0    |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |  0.00   |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
|      PEDERNALES      |   0.0    |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
| SAN PEDRO DE MACORÍS |   25.0   |   0.00   |   75.00    |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|     MONTE PLATA      |   12.5   |  12.50   |   75.00    |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |

Conjunto 26

|    nombre     | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|   BARAHONA    |   0.00   |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |  0.00   |     0      |
|    DAJABÓN    |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
| LA ALTAGRACIA |   7.69   |   0.00   |   76.92    |  15.38   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|    LA VEGA    |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |  0.00   |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
|  PEDERNALES   |   0.00   |   6.67   |   80.00    |   0.00   |   13.33    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|   VALVERDE    |   0.00   |  14.29   |   71.43    |   0.00   |   14.29    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |

Conjunto 27

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|          AZUA          |   5.88   |   5.88   |   88.24    |   0.00   |     0      |    0     |   0.0   |   0    |     0      |    0     |    0    |     0      |
|       LA ROMANA        |  12.50   |   0.00   |   75.00    |   0.00   |     0      |    0     |  12.5   |   0    |     0      |    0     |    0    |     0      |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |     0      |    0     |   0.0   |   0    |     0      |    0     |    0    |     0      |
|        PERAVIA         |   0.00   |  11.11   |   77.78    |  11.11   |     0      |    0     |   0.0   |   0    |     0      |    0     |    0    |     0      |
|         SAMANÁ         |   0.00   |  10.00   |   40.00    |  10.00   |     20     |    0     |   0.0   |   10   |     0      |    0     |   10    |     0      |
|  SAN PEDRO DE MACORÍS  |  25.00   |   0.00   |   75.00    |   0.00   |     0      |    0     |   0.0   |   0    |     0      |    0     |    0    |     0      |

Conjunto 28

|         nombre         | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|        BAORUCO         |   0.00   |   0.00   |   76.92    |   7.69   |    7.69    |   7.69   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|        BARAHONA        |   0.00   |  10.34   |   51.72    |  10.34   |   10.34    |   3.45   |  3.45   |  3.45  |    3.45    |   3.45   |  0.00   |     0      |
| MARÍA TRINIDAD SÁNCHEZ |   0.00   |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |  0.00   |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|         SAMANÁ         |   0.00   |  10.00   |   40.00    |  10.00   |   20.00    |   0.00   |  0.00   | 10.00  |    0.00    |   0.00   |  10.00  |     0      |
|        SAN JUAN        |   0.00   |  11.11   |   66.67    |   0.00   |   16.67    |   0.00   |  0.00   |  0.00  |    0.00    |   5.56   |  0.00   |     0      |
|     SANTO DOMINGO      |   8.33   |   8.33   |   50.00    |   8.33   |    8.33    |   0.00   |  0.00   |  8.33  |    0.00    |   0.00   |  8.33   |     0      |

Conjunto 29

|      nombre      | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
|  LA ALTAGRACIA   |   7.69   |   0.00   |   76.92    |  15.38   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |
|     LA VEGA      |   0.00   |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |   0    |    2.78    |   5.56   |  2.78   |     0      |
|     SANTIAGO     |   6.67   |  13.33   |   60.00    |  20.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |
|  MONSEÑOR NOUEL  |   4.35   |  13.04   |   56.52    |   4.35   |   13.04    |   0.00   |    0    |   0    |    0.00    |   4.35   |  4.35   |     0      |
|   MONTE PLATA    |  12.50   |  12.50   |   75.00    |   0.00   |    0.00    |   0.00   |    0    |   0    |    0.00    |   0.00   |  0.00   |     0      |
| SAN JOSÉ DE OCOA |   0.00   |   2.94   |   67.65    |   8.82   |    8.82    |   5.88   |    0    |   0    |    2.94    |   2.94   |  0.00   |     0      |

Conjunto 30

|      nombre       | Bromelia | Catopsis | Tillandsia | Guzmania | Pitcairnia | Racinaea | Aechmea | Ananas | Cipuropsis | Werauhia | Zizkaea | Billbergia |
|:-----------------:|:--------:|:--------:|:----------:|:--------:|:----------:|:--------:|:-------:|:------:|:----------:|:--------:|:-------:|:----------:|
| DISTRITO NACIONAL |    0     |   0.00   |   100.00   |   0.00   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      DUARTE       |    0     |   0.00   |   50.00    |  33.33   |    0.00    |   0.00   |    0    | 16.67  |    0.00    |   0.00   |  0.00   |     0      |
|     EL SEIBO      |    0     |  20.00   |   40.00    |  20.00   |    0.00    |   0.00   |   20    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |
|      LA VEGA      |    0     |   8.33   |   61.11    |   2.78   |    8.33    |   8.33   |    0    |  0.00  |    2.78    |   5.56   |  2.78   |     0      |
|     SAN JUAN      |    0     |  11.11   |   66.67    |   0.00   |   16.67    |   0.00   |    0    |  0.00  |    0.00    |   5.56   |  0.00   |     0      |
|  SANCHEZ RAMÍREZ  |    0     |   0.00   |   85.71    |  14.29   |    0.00    |   0.00   |    0    |  0.00  |    0.00    |   0.00   |  0.00   |     0      |

4.  **Generación de la matriz de distancias de cada uno de los 30
    conjuntos**. Esta es la matriz de distancias con la que podrás
    realizar el agrupamiento jerárquico UPGMA (parte 2 del mandato, que
    verás en la siguiente sección).

``` r
# Mostrar las matrices de distancia
print(sapply(
  conjuntos_l,
  function(x) {
    x %>%
      as.data.frame() %>%
      column_to_rownames('nombre') %>%
      dist(diag = T, upper = T) %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      knitr::kable(digits = 2, align = 'c')
  },
  simplify = F))
```

\$`Conjunto 1`

|                        | BAORUCO | INDEPENDENCIA | MARÍA TRINIDAD SÁNCHEZ | MONTE CRISTI | SAMANÁ | SANTO DOMINGO |
|:-----------------------|:-------:|:-------------:|:----------------------:|:------------:|:------:|:-------------:|
| BAORUCO                |  0.00   |     19.10     |         26.65          |    26.65     | 43.35  |     32.60     |
| INDEPENDENCIA          |  19.10  |     0.00      |         35.70          |    35.70     | 33.88  |     24.38     |
| MARÍA TRINIDAD SÁNCHEZ |  26.65  |     35.70     |          0.00          |     0.00     | 66.33  |     54.01     |
| MONTE CRISTI           |  26.65  |     35.70     |          0.00          |     0.00     | 66.33  |     54.01     |
| SAMANÁ                 |  43.35  |     33.88     |         66.33          |    66.33     |  0.00  |     17.80     |
| SANTO DOMINGO          |  32.60  |     24.38     |         54.01          |    54.01     | 17.80  |     0.00      |

\$`Conjunto 2`

|                      | LA VEGA | PUERTO PLATA | SAN PEDRO DE MACORÍS | SANTIAGO | SANTIAGO RODRÍGUEZ | HATO MAYOR |
|:---------------------|:-------:|:------------:|:--------------------:|:--------:|:------------------:|:----------:|
| LA VEGA              |  0.00   |    17.01     |        32.87         |  23.50   |       32.88        |   11.65    |
| PUERTO PLATA         |  17.01  |     0.00     |        31.54         |  16.63   |       37.03        |   17.79    |
| SAN PEDRO DE MACORÍS |  32.87  |    31.54     |         0.00         |  33.75   |       31.00        |   31.62    |
| SANTIAGO             |  23.50  |    16.63     |        33.75         |   0.00   |       37.65        |   25.60    |
| SANTIAGO RODRÍGUEZ   |  32.88  |    37.03     |        31.00         |  37.65   |        0.00        |   29.11    |
| HATO MAYOR           |  11.65  |    17.79     |        31.62         |  25.60   |       29.11        |    0.00    |

\$`Conjunto 3`

|               | BARAHONA | LA ALTAGRACIA | MONTE CRISTI | PEDERNALES | PERAVIA | VALVERDE |
|:--------------|:--------:|:-------------:|:------------:|:----------:|:-------:|:--------:|
| BARAHONA      |   0.00   |     31.51     |    52.07     |   31.44    |  29.09  |  24.20   |
| LA ALTAGRACIA |  31.51   |     0.00      |    28.78     |   22.97    |  14.20  |  27.10   |
| MONTE CRISTI  |  52.07   |     28.78     |     0.00     |   24.94    |  27.22  |  34.99   |
| PEDERNALES    |  31.44   |     22.97     |    24.94     |    0.00    |  18.05  |  11.51   |
| PERAVIA       |  29.09   |     14.20     |    27.22     |   18.05    |  0.00   |  19.44   |
| VALVERDE      |  24.20   |     27.10     |    34.99     |   11.51    |  19.44  |   0.00   |

\$`Conjunto 4`

|                      | ESPAILLAT | MONTE CRISTI | PUERTO PLATA | SAN PEDRO DE MACORÍS | SANTIAGO RODRÍGUEZ | MONTE PLATA |
|:---------------------|:---------:|:------------:|:------------:|:--------------------:|:------------------:|:-----------:|
| ESPAILLAT            |   0.00    |    37.27     |    20.48     |        31.18         |       28.22        |    21.25    |
| MONTE CRISTI         |   37.27   |     0.00     |    47.38     |        35.36         |       12.86        |    30.62    |
| PUERTO PLATA         |   20.48   |    47.38     |     0.00     |        31.54         |       37.03        |    22.45    |
| SAN PEDRO DE MACORÍS |   31.18   |    35.36     |    31.54     |         0.00         |       31.00        |    17.68    |
| SANTIAGO RODRÍGUEZ   |   28.22   |    12.86     |    37.03     |        31.00         |        0.00        |    20.52    |
| MONTE PLATA          |   21.25   |    30.62     |    22.45     |        17.68         |       20.52        |    0.00     |

\$`Conjunto 5`

|                        | AZUA  | BARAHONA | LA ALTAGRACIA | MARÍA TRINIDAD SÁNCHEZ | PUERTO PLATA | SANCHEZ RAMÍREZ |
|:-----------------------|:-----:|:--------:|:-------------:|:----------------------:|:------------:|:---------------:|
| AZUA                   | 0.00  |  40.76   |     20.06     |         14.41          |    34.53     |      16.72      |
| BARAHONA               | 40.76 |   0.00   |     31.51     |         52.07          |    15.06     |      38.00      |
| LA ALTAGRACIA          | 20.06 |  31.51   |     0.00      |         28.78          |    27.67     |      11.73      |
| MARÍA TRINIDAD SÁNCHEZ | 14.41 |  52.07   |     28.78     |          0.00          |    47.38     |      20.20      |
| PUERTO PLATA           | 34.53 |  15.06   |     27.67     |         47.38          |     0.00     |      34.99      |
| SANCHEZ RAMÍREZ        | 16.72 |  38.00   |     11.73     |         20.20          |    34.99     |      0.00       |

\$`Conjunto 6`

|                  | AZUA  | BARAHONA | PEDERNALES | SAMANÁ | SAN CRISTÓBAL | SAN JOSÉ DE OCOA |
|:-----------------|:-----:|:--------:|:----------:|:------:|:-------------:|:----------------:|
| AZUA             | 0.00  |  40.76   |   16.76    | 55.48  |     23.89     |      25.98       |
| BARAHONA         | 40.76 |   0.00   |   31.44    | 20.53  |     23.75     |      18.53       |
| PEDERNALES       | 16.76 |  31.44   |    0.00    | 44.22  |     19.43     |      17.79       |
| SAMANÁ           | 55.48 |  20.53   |   44.22    |  0.00  |     34.60     |      34.53       |
| SAN CRISTÓBAL    | 23.89 |  23.75   |   19.43    | 34.60  |     0.00      |      13.86       |
| SAN JOSÉ DE OCOA | 25.98 |  18.53   |   17.79    | 34.53  |     13.86     |       0.00       |

\$`Conjunto 7`

|                  | LA VEGA | PEDERNALES | MONTE PLATA | HATO MAYOR | SAN JOSÉ DE OCOA | SANTO DOMINGO |
|:-----------------|:-------:|:----------:|:-----------:|:----------:|:----------------:|:-------------:|
| LA VEGA          |  0.00   |   22.54    |    23.65    |   11.65    |      11.37       |     20.79     |
| PEDERNALES       |  22.54  |    0.00    |    19.83    |   17.95    |      17.79       |     34.72     |
| MONTE PLATA      |  23.65  |   19.83    |    0.00     |   20.92    |      22.57       |     30.62     |
| HATO MAYOR       |  11.65  |   17.95    |    20.92    |    0.00    |      15.28       |     20.41     |
| SAN JOSÉ DE OCOA |  11.37  |   17.79    |    22.57    |   15.28    |       0.00       |     24.52     |
| SANTO DOMINGO    |  20.79  |   34.72    |    30.62    |   20.41    |      24.52       |     0.00      |

\$`Conjunto 8`

|                        | DISTRITO NACIONAL | ESPAILLAT | LA VEGA | MARÍA TRINIDAD SÁNCHEZ | SANCHEZ RAMÍREZ | MONSEÑOR NOUEL |
|:-----------------------|:-----------------:|:---------:|:-------:|:----------------------:|:---------------:|:--------------:|
| DISTRITO NACIONAL      |       0.00        |   37.27   |  42.13  |          0.00          |      20.20      |     48.02      |
| ESPAILLAT              |       37.27       |   0.00    |  15.71  |         37.27          |      24.63      |     22.51      |
| LA VEGA                |       42.13       |   15.71   |  0.00   |         42.13          |      31.50      |     12.96      |
| MARÍA TRINIDAD SÁNCHEZ |       0.00        |   37.27   |  42.13  |          0.00          |      20.20      |     48.02      |
| SANCHEZ RAMÍREZ        |       20.20       |   24.63   |  31.50  |         20.20          |      0.00       |     36.71      |
| MONSEÑOR NOUEL         |       48.02       |   22.51   |  12.96  |         48.02          |      36.71      |      0.00      |

\$`Conjunto 9`

|                  | ELÍAS PIÑA | EL SEIBO | ESPAILLAT | PERAVIA | VALVERDE | SAN JOSÉ DE OCOA |
|:-----------------|:----------:|:--------:|:---------:|:-------:|:--------:|:----------------:|
| ELÍAS PIÑA       |    0.00    |  45.28   |   15.59   |  21.52  |  18.21   |      12.65       |
| EL SEIBO         |   45.28    |   0.00   |   34.48   |  44.56  |  44.99   |      41.35       |
| ESPAILLAT        |   15.59    |  34.48   |   0.00    |  16.67  |  21.69   |      14.17       |
| PERAVIA          |   21.52    |  44.56   |   16.67   |  0.00   |  19.44   |      17.45       |
| VALVERDE         |   18.21    |  44.99   |   21.69   |  19.44  |   0.00   |      17.40       |
| SAN JOSÉ DE OCOA |   12.65    |  41.35   |   14.17   |  17.45  |  17.40   |       0.00       |

\$`Conjunto 10`

|              | BAORUCO | BARAHONA | DUARTE | ESPAILLAT | MONTE CRISTI | HATO MAYOR |
|:-------------|:-------:|:--------:|:------:|:---------:|:------------:|:----------:|
| BAORUCO      |  0.00   |  28.66   | 42.17  |   17.44   |    26.65     |   21.00    |
| BARAHONA     |  28.66  |   0.00   | 31.11  |   20.54   |    52.07     |   18.68    |
| DUARTE       |  42.17  |  31.11   |  0.00  |   37.27   |    62.36     |   41.50    |
| ESPAILLAT    |  17.44  |  20.54   | 37.27  |   0.00    |    37.27     |   19.72    |
| MONTE CRISTI |  26.65  |  52.07   | 62.36  |   37.27   |     0.00     |   38.73    |
| HATO MAYOR   |  21.00  |  18.68   | 41.50  |   19.72   |    38.73     |    0.00    |

\$`Conjunto 11`

|                        | DISTRITO NACIONAL | ELÍAS PIÑA | LA VEGA | MARÍA TRINIDAD SÁNCHEZ | MONTE CRISTI | PEDERNALES |
|:-----------------------|:-----------------:|:----------:|:-------:|:----------------------:|:------------:|:----------:|
| DISTRITO NACIONAL      |       0.00        |   35.36    |  42.13  |          0.00          |     0.00     |   24.94    |
| ELÍAS PIÑA             |       35.36       |    0.00    |  10.39  |         35.36          |    35.36     |   19.29    |
| LA VEGA                |       42.13       |   10.39    |  0.00   |         42.13          |    42.13     |   22.54    |
| MARÍA TRINIDAD SÁNCHEZ |       0.00        |   35.36    |  42.13  |          0.00          |     0.00     |   24.94    |
| MONTE CRISTI           |       0.00        |   35.36    |  42.13  |          0.00          |     0.00     |   24.94    |
| PEDERNALES             |       24.94       |   19.29    |  22.54  |         24.94          |    24.94     |    0.00    |

\$`Conjunto 12`

|                      | BAORUCO | LA VEGA | PUERTO PLATA | SAN CRISTÓBAL | SAN PEDRO DE MACORÍS | SANTO DOMINGO |
|:---------------------|:-------:|:-------:|:------------:|:-------------:|:--------------------:|:-------------:|
| BAORUCO              |  0.00   |  19.77  |    27.52     |     15.73     |        28.39         |     32.60     |
| LA VEGA              |  19.77  |  0.00   |    17.01     |     18.50     |        32.87         |     20.79     |
| PUERTO PLATA         |  27.52  |  17.01  |     0.00     |     21.69     |        31.54         |     16.75     |
| SAN CRISTÓBAL        |  15.73  |  18.50  |    21.69     |     0.00      |        23.39         |     21.04     |
| SAN PEDRO DE MACORÍS |  28.39  |  32.87  |    31.54     |     23.39     |         0.00         |     35.36     |
| SANTO DOMINGO        |  32.60  |  20.79  |    16.75     |     21.04     |        35.36         |     0.00      |

\$`Conjunto 13`

|                   | DISTRITO NACIONAL | BAORUCO | ESPAILLAT | LA ROMANA | LA VEGA | SANTO DOMINGO |
|:------------------|:-----------------:|:-------:|:---------:|:---------:|:-------:|:-------------:|
| DISTRITO NACIONAL |       0.00        |  26.65  |   37.27   |   30.62   |  42.13  |     54.01     |
| BAORUCO           |       26.65       |  0.00   |   17.44   |   22.22   |  19.77  |     32.60     |
| ESPAILLAT         |       37.27       |  17.44  |   0.00    |   21.25   |  15.71  |     26.35     |
| LA ROMANA         |       30.62       |  22.22  |   21.25   |   0.00    |  27.71  |     33.85     |
| LA VEGA           |       42.13       |  19.77  |   15.71   |   27.71   |  0.00   |     20.79     |
| SANTO DOMINGO     |       54.01       |  32.60  |   26.35   |   33.85   |  20.79  |     0.00      |

\$`Conjunto 14`

|              | BAORUCO | MONTE CRISTI | SAN JUAN | SANTIAGO | VALVERDE | HATO MAYOR |
|:-------------|:-------:|:------------:|:--------:|:--------:|:--------:|:----------:|
| BAORUCO      |  0.00   |    26.65     |  21.41   |  27.90   |  19.90   |   21.00    |
| MONTE CRISTI |  26.65  |     0.00     |  39.28   |  47.14   |  34.99   |   38.73    |
| SAN JUAN     |  21.41  |    39.28     |   0.00   |  28.33   |   8.32   |    9.94    |
| SANTIAGO     |  27.90  |    47.14     |  28.33   |   0.00   |  27.93   |   25.60    |
| VALVERDE     |  19.90  |    34.99     |   8.32   |  27.93   |   0.00   |   12.37    |
| HATO MAYOR   |  21.00  |    38.73     |   9.94   |  25.60   |  12.37   |    0.00    |

\$`Conjunto 15`

|                  | AZUA  | SAN CRISTÓBAL | VALVERDE | HATO MAYOR | SAN JOSÉ DE OCOA | SANTO DOMINGO |
|:-----------------|:-----:|:-------------:|:--------:|:----------:|:----------------:|:-------------:|
| AZUA             | 0.00  |     23.89     |  24.33   |   27.68    |      25.98       |     41.85     |
| SAN CRISTÓBAL    | 23.89 |     0.00      |  20.79   |   15.31    |      13.86       |     21.04     |
| VALVERDE         | 24.33 |     20.79     |   0.00   |   12.37    |      17.40       |     28.42     |
| HATO MAYOR       | 27.68 |     15.31     |  12.37   |    0.00    |      15.28       |     20.41     |
| SAN JOSÉ DE OCOA | 25.98 |     13.86     |  17.40   |   15.28    |       0.00       |     24.52     |
| SANTO DOMINGO    | 41.85 |     21.04     |  28.42   |   20.41    |      24.52       |     0.00      |

\$`Conjunto 16`

|                      | BAORUCO | DUARTE | LA VEGA | SAN PEDRO DE MACORÍS | SANCHEZ RAMÍREZ | SANTIAGO RODRÍGUEZ |
|:---------------------|:-------:|:------:|:-------:|:--------------------:|:---------------:|:------------------:|
| BAORUCO              |  0.00   | 42.17  |  19.77  |        28.39         |      15.46      |       21.35        |
| DUARTE               |  42.17  |  0.00  |  39.87  |        51.37         |      43.77      |       56.08        |
| LA VEGA              |  19.77  | 39.87  |  0.00   |        32.87         |      31.50      |       32.88        |
| SAN PEDRO DE MACORÍS |  28.39  | 51.37  |  32.87  |         0.00         |      30.72      |       31.00        |
| SANCHEZ RAMÍREZ      |  15.46  | 43.77  |  31.50  |        30.72         |      0.00       |       17.71        |
| SANTIAGO RODRÍGUEZ   |  21.35  | 56.08  |  32.88  |        31.00         |      17.71      |        0.00        |

\$`Conjunto 17`

|                        | LA ROMANA | MARÍA TRINIDAD SÁNCHEZ | PUERTO PLATA | SANCHEZ RAMÍREZ | HATO MAYOR | SANTO DOMINGO |
|:-----------------------|:---------:|:----------------------:|:------------:|:---------------:|:----------:|:-------------:|
| LA ROMANA              |   0.00    |         30.62          |    29.34     |      25.13      |   26.22    |     33.85     |
| MARÍA TRINIDAD SÁNCHEZ |   30.62   |          0.00          |    47.38     |      20.20      |   38.73    |     54.01     |
| PUERTO PLATA           |   29.34   |         47.38          |     0.00     |      34.99      |   17.79    |     16.75     |
| SANCHEZ RAMÍREZ        |   25.13   |         20.20          |    34.99     |      0.00       |   30.14    |     40.72     |
| HATO MAYOR             |   26.22   |         38.73          |    17.79     |      30.14      |    0.00    |     20.41     |
| SANTO DOMINGO          |   33.85   |         54.01          |    16.75     |      40.72      |   20.41    |     0.00      |

\$`Conjunto 18`

|              | EL SEIBO | LA VEGA | MONTE CRISTI | PERAVIA | PUERTO PLATA | SAN JUAN |
|:-------------|:--------:|:-------:|:------------:|:-------:|:------------:|:--------:|
| EL SEIBO     |   0.00   |  38.26  |    69.28     |  44.56  |    32.32     |  43.57   |
| LA VEGA      |  38.26   |  0.00   |    42.13     |  23.24  |    17.01     |  14.16   |
| MONTE CRISTI |  69.28   |  42.13  |     0.00     |  27.22  |    47.38     |  39.28   |
| PERAVIA      |  44.56   |  23.24  |    27.22     |  0.00   |    24.59     |  23.57   |
| PUERTO PLATA |  32.32   |  17.01  |    47.38     |  24.59  |     0.00     |  19.38   |
| SAN JUAN     |  43.57   |  14.16  |    39.28     |  23.57  |    19.38     |   0.00   |

\$`Conjunto 19`

|                        | BARAHONA | MARÍA TRINIDAD SÁNCHEZ | MONTE CRISTI | SAMANÁ | SANTIAGO RODRÍGUEZ | SAN JOSÉ DE OCOA |
|:-----------------------|:--------:|:----------------------:|:------------:|:------:|:------------------:|:----------------:|
| BARAHONA               |   0.00   |         52.07          |    52.07     | 20.53  |       42.55        |      18.53       |
| MARÍA TRINIDAD SÁNCHEZ |  52.07   |          0.00          |     0.00     | 66.33  |       12.86        |      35.54       |
| MONTE CRISTI           |  52.07   |          0.00          |     0.00     | 66.33  |       12.86        |      35.54       |
| SAMANÁ                 |  20.53   |         66.33          |    66.33     |  0.00  |       57.38        |      34.53       |
| SANTIAGO RODRÍGUEZ     |  42.55   |         12.86          |    12.86     | 57.38  |        0.00        |      28.05       |
| SAN JOSÉ DE OCOA       |  18.53   |         35.54          |    35.54     | 34.53  |       28.05        |       0.00       |

\$`Conjunto 20`

|             | AZUA  | ELÍAS PIÑA | ESPAILLAT | LA VEGA | SAMANÁ | MONTE PLATA |
|:------------|:-----:|:----------:|:---------:|:-------:|:------:|:-----------:|
| AZUA        | 0.00  |   25.47    |   26.72   |  31.13  | 55.48  |    16.21    |
| ELÍAS PIÑA  | 25.47 |    0.00    |   15.59   |  10.39  | 39.05  |    21.65    |
| ESPAILLAT   | 26.72 |   15.59    |   0.00    |  15.71  | 38.15  |    21.25    |
| LA VEGA     | 31.13 |   10.39    |   15.71   |  0.00   | 29.95  |    23.65    |
| SAMANÁ      | 55.48 |   39.05    |   38.15   |  29.95  |  0.00  |    45.69    |
| MONTE PLATA | 16.21 |   21.65    |   21.25   |  23.65  | 45.69  |    0.00     |

\$`Conjunto 21`

|                        | AZUA  | BAORUCO | MARÍA TRINIDAD SÁNCHEZ | SANCHEZ RAMÍREZ | MONTE PLATA | SAN JOSÉ DE OCOA |
|:-----------------------|:-----:|:-------:|:----------------------:|:---------------:|:-----------:|:----------------:|
| AZUA                   | 0.00  |  19.36  |         14.41          |      16.72      |    16.21    |      25.98       |
| BAORUCO                | 19.36 |  0.00   |         26.65          |      15.46      |    22.22    |      10.86       |
| MARÍA TRINIDAD SÁNCHEZ | 14.41 |  26.65  |          0.00          |      20.20      |    30.62    |      35.54       |
| SANCHEZ RAMÍREZ        | 16.72 |  15.46  |         20.20          |      0.00       |    25.13    |      22.24       |
| MONTE PLATA            | 16.21 |  22.22  |         30.62          |      25.13      |    0.00     |      22.57       |
| SAN JOSÉ DE OCOA       | 25.98 |  10.86  |         35.54          |      22.24      |    22.57    |       0.00       |

\$`Conjunto 22`

|                   | DISTRITO NACIONAL | BAORUCO | DUARTE | LA VEGA | SAN JUAN | HATO MAYOR |
|:------------------|:-----------------:|:-------:|:------:|:-------:|:--------:|:----------:|
| DISTRITO NACIONAL |       0.00        |  26.65  | 62.36  |  42.13  |  39.28   |   38.73    |
| BAORUCO           |       26.65       |  0.00   | 42.17  |  19.77  |  21.41   |   21.00    |
| DUARTE            |       62.36       |  42.17  |  0.00  |  39.87  |  45.81   |   41.50    |
| LA VEGA           |       42.13       |  19.77  | 39.87  |  0.00   |  14.16   |   11.65    |
| SAN JUAN          |       39.28       |  21.41  | 45.81  |  14.16  |   0.00   |    9.94    |
| HATO MAYOR        |       38.73       |  21.00  | 41.50  |  11.65  |   9.94   |    0.00    |

\$`Conjunto 23`

|                 | BAORUCO | DAJABÓN | EL SEIBO | PEDERNALES | SANCHEZ RAMÍREZ | MONSEÑOR NOUEL |
|:----------------|:-------:|:-------:|:--------:|:----------:|:---------------:|:--------------:|
| BAORUCO         |  0.00   |  26.65  |  49.33   |   14.29    |      15.46      |     27.24      |
| DAJABÓN         |  26.65  |  0.00   |  69.28   |   24.94    |      20.20      |     48.02      |
| EL SEIBO        |  49.33  |  69.28  |   0.00   |   52.49    |      54.06      |     34.54      |
| PEDERNALES      |  14.29  |  24.94  |  52.49   |    0.00    |      21.42      |     25.84      |
| SANCHEZ RAMÍREZ |  15.46  |  20.20  |  54.06   |   21.42    |      0.00       |     36.71      |
| MONSEÑOR NOUEL  |  27.24  |  48.02  |  34.54   |   25.84    |      36.71      |      0.00      |

\$`Conjunto 24`

|               | ELÍAS PIÑA | LA ALTAGRACIA | MONTE CRISTI | PERAVIA | PUERTO PLATA | SANTO DOMINGO |
|:--------------|:----------:|:-------------:|:------------:|:-------:|:------------:|:-------------:|
| ELÍAS PIÑA    |    0.00    |     25.22     |    35.36     |  21.52  |    23.42     |     28.87     |
| LA ALTAGRACIA |   25.22    |     0.00      |    28.78     |  14.20  |    27.67     |     32.45     |
| MONTE CRISTI  |   35.36    |     28.78     |     0.00     |  27.22  |    47.38     |     54.01     |
| PERAVIA       |   21.52    |     14.20     |    27.22     |  0.00   |    24.59     |     32.63     |
| PUERTO PLATA  |   23.42    |     27.67     |    47.38     |  24.59  |     0.00     |     16.75     |
| SANTO DOMINGO |   28.87    |     32.45     |    54.01     |  32.63  |    16.75     |     0.00      |

\$`Conjunto 25`

|                      | BAORUCO | BARAHONA | LA VEGA | PEDERNALES | SAN PEDRO DE MACORÍS | MONTE PLATA |
|:---------------------|:-------:|:--------:|:-------:|:----------:|:--------------------:|:-----------:|
| BAORUCO              |  0.00   |  28.66   |  19.77  |   14.29    |        28.39         |    22.22    |
| BARAHONA             |  28.66  |   0.00   |  14.62  |   31.44    |        39.34         |    31.24    |
| LA VEGA              |  19.77  |  14.62   |  0.00   |   22.54    |        32.87         |    23.65    |
| PEDERNALES           |  14.29  |  31.44   |  22.54  |    0.00    |        29.53         |    19.83    |
| SAN PEDRO DE MACORÍS |  28.39  |  39.34   |  32.87  |   29.53    |         0.00         |    17.68    |
| MONTE PLATA          |  22.22  |  31.24   |  23.65  |   19.83    |        17.68         |    0.00     |

\$`Conjunto 26`

|               | BARAHONA | DAJABÓN | LA ALTAGRACIA | LA VEGA | PEDERNALES | VALVERDE |
|:--------------|:--------:|:-------:|:-------------:|:-------:|:----------:|:--------:|
| BARAHONA      |   0.00   |  52.07  |     31.51     |  14.62  |   31.44    |  24.20   |
| DAJABÓN       |  52.07   |  0.00   |     28.78     |  42.13  |   24.94    |  34.99   |
| LA ALTAGRACIA |  31.51   |  28.78  |     0.00      |  26.88  |   22.97    |  27.10   |
| LA VEGA       |  14.62   |  42.13  |     26.88     |  0.00   |   22.54    |  17.34   |
| PEDERNALES    |  31.44   |  24.94  |     22.97     |  22.54  |    0.00    |  11.51   |
| VALVERDE      |  24.20   |  34.99  |     27.10     |  17.34  |   11.51    |   0.00   |

\$`Conjunto 27`

|                        | AZUA  | LA ROMANA | MARÍA TRINIDAD SÁNCHEZ | PERAVIA | SAMANÁ | SAN PEDRO DE MACORÍS |
|:-----------------------|:-----:|:---------:|:----------------------:|:-------:|:------:|:--------------------:|
| AZUA                   | 0.00  |   20.24   |         14.41          |  17.17  | 55.48  |        23.98         |
| LA ROMANA              | 20.24 |   0.00    |         30.62          |  23.81  | 48.35  |        17.68         |
| MARÍA TRINIDAD SÁNCHEZ | 14.41 |   30.62   |          0.00          |  27.22  | 66.33  |        35.36         |
| PERAVIA                | 17.17 |   23.81   |         27.22          |  0.00   | 45.05  |        29.66         |
| SAMANÁ                 | 55.48 |   48.35   |         66.33          |  45.05  |  0.00  |        51.48         |
| SAN PEDRO DE MACORÍS   | 23.98 |   17.68   |         35.36          |  29.66  | 51.48  |         0.00         |

\$`Conjunto 28`

|                        | BAORUCO | BARAHONA | MARÍA TRINIDAD SÁNCHEZ | SAMANÁ | SAN JUAN | SANTO DOMINGO |
|:-----------------------|:-------:|:--------:|:----------------------:|:------:|:--------:|:-------------:|
| BAORUCO                |  0.00   |  28.66   |         26.65          | 43.35  |  21.41   |     32.60     |
| BARAHONA               |  28.66  |   0.00   |         52.07          | 20.53  |  20.56   |     15.01     |
| MARÍA TRINIDAD SÁNCHEZ |  26.65  |  52.07   |          0.00          | 66.33  |  39.28   |     54.01     |
| SAMANÁ                 |  43.35  |  20.53   |         66.33          |  0.00  |  32.47   |     17.80     |
| SAN JUAN               |  21.41  |  20.56   |         39.28          | 32.47  |   0.00   |     25.76     |
| SANTO DOMINGO          |  32.60  |  15.01   |         54.01          | 17.80  |  25.76   |     0.00      |

\$`Conjunto 29`

|                  | LA ALTAGRACIA | LA VEGA | SANTIAGO | MONSEÑOR NOUEL | MONTE PLATA | SAN JOSÉ DE OCOA |
|:-----------------|:-------------:|:-------:|:--------:|:--------------:|:-----------:|:----------------:|
| LA ALTAGRACIA    |     0.00      |  26.88  |  22.06   |     30.45      |    20.49    |      18.07       |
| LA VEGA          |     26.88     |  0.00   |  23.50   |     12.96      |    23.65    |      11.37       |
| SANTIAGO         |     22.06     |  23.50  |   0.00   |     21.69      |    25.69    |      21.58       |
| MONSEÑOR NOUEL   |     30.45     |  12.96  |  21.69   |      0.00      |    25.20    |      18.62       |
| MONTE PLATA      |     20.49     |  23.65  |  25.69   |     25.20      |    0.00     |      22.57       |
| SAN JOSÉ DE OCOA |     18.07     |  11.37  |  21.58   |     18.62      |    22.57    |       0.00       |

\$`Conjunto 30`

|                   | DISTRITO NACIONAL | DUARTE | EL SEIBO | LA VEGA | SAN JUAN | SANCHEZ RAMÍREZ |
|:------------------|:-----------------:|:------:|:--------:|:-------:|:--------:|:---------------:|
| DISTRITO NACIONAL |       0.00        | 62.36  |  69.28   |  42.13  |  39.28   |      20.20      |
| DUARTE            |       62.36       |  0.00  |  36.82   |  39.87  |  45.81   |      43.77      |
| EL SEIBO          |       69.28       | 36.82  |   0.00   |  38.26  |  43.57   |      54.06      |
| LA VEGA           |       42.13       | 39.87  |  38.26   |  0.00   |  14.16   |      31.50      |
| SAN JUAN          |       39.28       | 45.81  |  43.57   |  14.16  |   0.00   |      31.61      |
| SANCHEZ RAMÍREZ   |       20.20       | 43.77  |  54.06   |  31.50  |  31.61   |      0.00       |

## **Mandato**.

> Ten presente un aspecto importante. El Tali te provee la matriz de
> distancias para que realices el agrupamiento (parte 2 de este
> mandato). Sin embargo, el Tali también te pide, en la parte 1 de este
> mandato que, a modo de prueba, calcules la distancia euclidiana para
> dos pares de provincias elegidos al azar de entre todos los pares
> posibles. Esto tiene por objetivo que puedas medir tu rendimiento en
> el cálculo de distancias en un espacio n-dimensional.

> Los cálculos se pueden realizar a mano o con una calculadora del
> teléfono, utilizando las fórmulas proporcionadas.

> **IMPORTANTE**. Anuncia tu conjunto elegido en el foro, indicando que
> te refieres a la PD03.

1.  Parte 1 del mandato. Usando los datos generados por Martínez-Batlle
    (2022), para el conjunto que te tocó (recuerda reservar el conjunto
    1 al Tali), obtén la distancia euclidiana entre un par de provincias
    elegidas por ti al azar (aunque tendrás que analizar 6 provincias en
    la parte 2, en este mandato elige un par al azar). Haz este cálculo
    usando todas las dimensiones (atributos) disponibles (un máximo de
    doce en total, que coincide con el número de géneros de Bromeliaceae
    disponibles). Ten presente que, para incluir todas las dimensiones,
    deberás usar la fórmula generalizada de la distancia que verás en el
    ejemplo práctico, la cual considera un espacio n-dimensional (es
    decir, sumando los cuadrados de las diferencias entre sus
    coordenadas correspondientes en cada una de las dimensiones, y
    tomando la raíz cuadrada del resultado). **Esta parte del ejercicio
    es obligatoria, pero no es imprescindible para realizar la segunda
    parte**, y lo único que quiero es verificar tu rendimiento para
    calcular distancias en un espacio n-dimensional, donde n\>3. Es
    decir, no te estoy pidiendo que hagas esta parte 1 para poder hacer
    la 2. Simplemente, quiero que calcules distancias. Además, ten
    presente que el cómputo final de la distancia lo podrás comprobar
    con el que se encuentra en la matriz de distancias ya calculada, y
    que te incluí arriba.

2.  Parte 2 del mandato. Aplica el método de agrupamiento jerárquico
    aglomerativo promedio no ponderado (UPGMA) para agrupar las
    provincias según su riqueza relativa de especies por género de
    bromelias, usando la matriz de distancias provista arriba. Este
    método de agrupamiento jerárquico te permitirá identificar patrones
    biogeográficos comunes y diferenciar unidades territoriales en
    función de su diversidad de géneros de Bromeliaceae. Para realizar
    este agrupamiento, puedes seguir el ejemplo práctico que te
    proporciono más abajo. Comprueba que el resultado obtenido coincide
    con el que te proporcioné al final de este cuaderno, según tu
    conjunto elegido.

3.  Parte 3 del mandado. Consolidando. Redacta, en un máximo de cuatro
    párrafos, lo siguiente:

- Introducción, en el que podrías incluir importancia del ejercicio,
  objetivo, justificación.
- Materiales y métodos, donde resumas que materiales usaste (esto
  incluye hasta el teléfono móvil, papel, lápiz, etc.), y las técnicas
  específicas empleadas, que en tu caso son la distancia y el método de
  agrupamiento enseñado.
- Resultado, lo cual supone describir, fríamente, lo que obtuviste.
- Discusión, donde indiques si alcanzaste el objetivo, y donde
  posteriormente interpretas el resultado, indicas las limitaciones y
  los posibles trabajos futuros.

# Ejemplo práctico

## Demostración de la parte 1 del mandato. Calcular distancia entre dos provincias.

> Recuerda: esta parte es obligatoria, pero no es un imprescindible para
> realizar la parte 2. Sólo me interesa que verifiques tu rendimiento
> calculando distancias en un espacio n-dimensional, donde en este caso
> n=12.

Vamos a calcular la distancia euclidiana entre las provincias
**BAORUCO** e **INDEPENDENCIA** utilizando las doce dimensiones
proporcionadas en los datos del Conjunto 1, que representan las
proporciones de especies de Bromeliaceae por género en cada provincia.

### Paso a Paso para Calcular la Distancia Euclidiana

La fórmula para la distancia euclidiana es:

$$
d(A, B) = \sqrt{\sum_{i=1}^{n} (x_{i} - y_{i})^2}
$$

donde: - $d(A, B)$ es la distancia euclidiana entre las provincias
**BAORUCO** e **INDEPENDENCIA**. - $x_{i}$ y $y_{i}$ son las
proporciones de especies para las provincias **BAORUCO** e
**INDEPENDENCIA**, respectivamente. - $n = 12$ es el número de géneros
considerados.

### Cálculo Paso a Paso

| Género     | BAORUCO ($x_i$) | INDEPENDENCIA ($y_i$) |                     $(x_i - y_i)^2$ |
|:-----------|----------------:|----------------------:|------------------------------------:|
| Bromelia   |        0.000000 |              0.000000 |       $(0.000000 - 0.000000)^2 = 0$ |
| Catopsis   |        0.000000 |             10.526316 | $(0.000000 - 10.526316)^2 = 110.81$ |
| Tillandsia |        76.92308 |              68.42105 |   $(76.92308 - 68.42105)^2 = 72.42$ |
| Guzmania   |        7.692308 |              0.000000 |   $(7.692308 - 0.000000)^2 = 59.20$ |
| Pitcairnia |        7.692308 |             10.526316 |   $(7.692308 - 10.526316)^2 = 8.03$ |
| Racinaea   |        7.692308 |              0.000000 |   $(7.692308 - 0.000000)^2 = 59.20$ |
| Aechmea    |        0.000000 |              0.000000 |       $(0.000000 - 0.000000)^2 = 0$ |
| Ananas     |        0.000000 |              0.000000 |       $(0.000000 - 0.000000)^2 = 0$ |
| Cipuropsis |        0.000000 |              0.000000 |       $(0.000000 - 0.000000)^2 = 0$ |
| Werauhia   |        0.000000 |              5.263158 |   $(0.000000 - 5.263158)^2 = 27.69$ |
| Zizkaea    |        0.000000 |              5.263158 |   $(0.000000 - 5.263158)^2 = 27.69$ |
| Billbergia |        0.000000 |              0.000000 |       $(0.000000 - 0.000000)^2 = 0$ |

### Suma de las Diferencias al Cuadrado

$$
\sum_{i=1}^{12} (x_{i} - y_{i})^2 = 0 + 110.81 + 72.42 + 59.20 + 8.03 + 59.20 + 0 + 0 + 0 + 27.69 + 27.69 + 0 = 365.04
$$

### Cálculo de la Distancia Euclidiana

$$
d(\text{BAORUCO, INDEPENDENCIA}) = \sqrt{365.04} \approx 19.11
$$

Si se compara con el valor del par correspondiente en la matriz de
distancias incluida arriba, se puede comprobar que el cálculo es
correcto, con apenas una centésima de diferencia.

### Resultado

La distancia euclidiana entre las provincias **BAORUCO** e
**INDEPENDENCIA** es aproximadamente **19.11** unidades porcentuales.
Esta distancia refleja la magnitud de la diferencia en la distribución
de géneros de Bromeliaceae entre las dos provincias consideradas.

## Demostración de la parte 2 del mandato. Aplicar el método UPGMA

Para realizar un agrupamiento jerárquico utilizando el método de
agrupamiento jerárquico aglomerativo promedio no ponderado (UPGMA),
seguimos un enfoque iterativo que consiste en agrupar los pares de
elementos o clusters más cercanos hasta que todos los elementos estén en
un único cluster. UPGMA utiliza las distancias promedio entre todos los
miembros de los clusters para calcular la distancia entre clusters.
[Este vídeo](https://www.youtube.com/watch?v=RdT7bhm1M3E) también podría
resultarte útil, aunque en el vídeo, la instructora se basa en el
vínculo simple, no en el promedio; es decir, ella, en lugar de obtener
promedios a la hora de recalcular la matriz de distancias, lo que
obtiene son valores mínimos, pero en nuestro casos serían promedios.

### Matriz de Distancias Inicial

Dado el conjunto de datos del Conjunto 1, su matriz de distancia es:

|                        | BAORUCO | INDEPENDENCIA | MARÍA TRINIDAD SÁNCHEZ | MONTE CRISTI | SAMANÁ | SANTO DOMINGO |
|:-----------------------|:-------:|:-------------:|:----------------------:|:------------:|:------:|:-------------:|
| BAORUCO                |  0.00   |     19.10     |         26.65          |    26.65     | 43.35  |     32.60     |
| INDEPENDENCIA          |  19.10  |     0.00      |         35.70          |    35.70     | 33.88  |     24.38     |
| MARÍA TRINIDAD SÁNCHEZ |  26.65  |     35.70     |          0.00          |     0.00     | 66.33  |     54.01     |
| MONTE CRISTI           |  26.65  |     35.70     |          0.00          |     0.00     | 66.33  |     54.01     |
| SAMANÁ                 |  43.35  |     33.88     |         66.33          |    66.33     |  0.00  |     17.80     |
| SANTO DOMINGO          |  32.60  |     24.38     |         54.01          |    54.01     | 17.80  |     0.00      |

### Paso 1: Encuentra el par con la distancia más pequeña

- El par con la distancia más pequeña es **MARÍA TRINIDAD SÁNCHEZ** y
  **MONTE CRISTI** con una distancia de 0.0. Este par forma el primer
  grupo (MTS-MC).

<img src="arbol-paso-1.jpg" style="width:40.0%" />

- Calculamos la distancia entre este nuevo cluster (MTS-MC) y los otros
  elementos utilizando el promedio de las distancias:

$$
\text{Distancia}(MTS-MC, \text{BAORUCO}) = \frac{26.65 + 26.65}{2} = 26.65
$$

$$
\text{Distancia}(MTS-MC, \text{INDEPENDENCIA}) = \frac{35.70 + 35.70}{2} = 35.70
$$

$$
\text{Distancia}(MTS-MC, \text{SAMANÁ}) = \frac{66.33 + 66.33}{2} = 66.33
$$

$$
\text{Distancia}(MTS-MC, \text{SANTO DOMINGO}) = \frac{54.01 + 54.01}{2} = 54.01
$$

- Nueva matriz de distancias:

|                   | MTS-MC | BAORUCO | INDEPENDENCIA | SAMANÁ | SANTO DOMINGO |
|-------------------|--------|---------|---------------|--------|---------------|
| **MTS-MC**        |        | 26.65   | 35.70         | 66.33  | 54.01         |
| **BAORUCO**       | 26.65  |         | 19.10         | 43.35  | 32.60         |
| **INDEPENDENCIA** | 35.70  | 19.10   |               | 33.88  | 24.38         |
| **SAMANÁ**        | 66.33  | 43.35   | 33.88         |        | 17.80         |
| **SANTO DOMINGO** | 54.01  | 32.60   | 24.38         | 17.80  |               |

### Paso 2: Encuentra el Siguiente Par con la Distancia más Pequeña

- El siguiente par con la distancia más pequeña es **SAMANÁ** y **SANTO
  DOMINGO** con una distancia de 17.80.

### Paso 3: Agrupa el Nuevo Cluster

- Agrupamos **SAMANÁ** y **SANTO DOMINGO** en un nuevo cluster (SAM-SD).

<img src="arbol-paso-2.jpg" style="width:40.0%" />

- Calculamos la distancia entre el nuevo cluster (SAM-SD) y los otros
  elementos:

$$
\text{Distancia}(SAM-SD, \text{MTS-MC}) = \frac{66.33 + 54.01}{2} = 60.17
$$

$$
\text{Distancia}(SAM-SD, \text{BAORUCO}) = \frac{43.35 + 32.60}{2} = 37.98
$$

$$
\text{Distancia}(SAM-SD, \text{INDEPENDENCIA}) = \frac{33.88 + 24.38}{2} = 29.13
$$

- Nueva matriz de distancias:

|                   | MTS-MC | BAORUCO | INDEPENDENCIA | SAM-SD |
|-------------------|--------|---------|---------------|--------|
| **MTS-MC**        |        | 26.65   | 35.70         | 60.17  |
| **BAORUCO**       | 26.65  |         | 19.10         | 37.98  |
| **INDEPENDENCIA** | 35.70  | 19.10   |               | 29.13  |
| **SAM-SD**        | 60.17  | 37.98   | 29.13         |        |

### Paso 4: Encuentra el Siguiente Par con la Distancia más Pequeña

- El siguiente par con la distancia más pequeña es **BAORUCO** e
  **INDEPENDENCIA** con una distancia de 19.10.

### Paso 5: Agrupa el Nuevo Cluster

- Agrupamos **BAORUCO** e **INDEPENDENCIA** en un nuevo cluster
  (BA-IND).

<img src="arbol-paso-3.jpg" style="width:40.0%" />

- Calculamos la distancia entre el nuevo cluster (BA-IND) y los otros
  elementos:

$$
\text{Distancia}(BA-IND, \text{MTS-MC}) = \frac{26.65 + 35.70}{2} = 31.18
$$

$$
\text{Distancia}(BA-IND, \text{SAM-SD}) = \frac{37.98 + 29.13}{2} = 33.56
$$

- Nueva matriz de distancias:

|            | MTS-MC | BA-IND | SAM-SD |
|------------|--------|--------|--------|
| **MTS-MC** |        | 31.18  | 60.17  |
| **BA-IND** | 31.18  |        | 33.56  |
| **SAM-SD** | 60.17  | 33.56  |        |

### Paso 6: Encuentra el Siguiente Par con la Distancia más Pequeña

- El siguiente par con la distancia más pequeña es **MTS-MC** y
  **BA-IND** con una distancia de 31.18.

### Paso 7: Agrupa el Nuevo Cluster

- Agrupamos **MTS-MC** y **BA-IND** en un nuevo cluster (MTS-MC-BA-IND).

<img src="arbol-paso-4.jpg" style="width:40.0%" />

- Calculamos la distancia entre el nuevo cluster (MTS-MC-BA-IND) y el
  otro elemento:

$$
\text{Distancia}(MTS-MC-BA-IND, \text{SAM-SD}) = \frac{60.17 + 33.56}{2} = 46.87
$$

- Nueva matriz de distancias:

|                   | MTS-MC-BA-IND | SAM-SD |
|-------------------|---------------|--------|
| **MTS-MC-BA-IND** |               | 46.87  |
| **SAM-SD**        | 46.87         |        |

### Paso 8: Agrupa los Últimos Clusters

- Finalmente, agrupamos **MTS-MC-BA-IND** y **SAM-SD** con una distancia
  de 46.87.

<img src="arbol-paso-5.jpg" style="width:40.0%" />

### Dendrograma

El resultado final del proceso es un dendrograma que muestra la
estructura jerárquica completa de los agrupamientos hasta que todos los
elementos quedan en un solo cluster.

### Conclusión

En resumen, el método UPGMA aplicado a las provincias dominicanas revela
agrupamientos basados en la similitud de la riqueza relativa de especies
de Bromeliaceae. Este enfoque jerárquico facilita la identificación de
patrones biogeográficos y destaca las relaciones entre provincias con
características comunes, lo que es útil para estudios de biodiversidad y
planificación de conservación.

## Demostración de la parte 3 del mandato. Consolidando.

Redacta, en un máximo de cuatro párrafos, una introducción, materiales y
métodos, resultados y discusión de la práctica realizada. No te incluiré
una redacción demostrativa aquí, para evitar limitar tu creatividad.
Sólo sigue las instrucciones señaladas arriba sobre lo que debes incluir
en cada sección.

## ¿Cómo se haría el ejemplo práctico en R?

``` r
conjuntos_l_1 <- conjuntos_l[[1]]
dist_conj_1 <- conjuntos_l_1 %>%
  as.data.frame() %>%
  column_to_rownames('nombre') %>%
  dist(diag = T, upper = T)
```

- Para la parte 1 del mandato, imprimir la matriz de distancias y
  localizar el par de interés.

``` r
dist_conj_1
```

- Para la parte 2 del mandato, aplicar el método UPGMA y graficar.

``` r
conj_1_upgma <- hclust(dist_conj_1, method = 'average')
plot(conj_1_upgma)
```

# Solución de la parte 2 del mandato, para todos los conjuntos

``` r
invisible(sapply(names(conjuntos_l),
       function(x) {
         d <- conjuntos_l[[x]] %>% 
           as.data.frame() %>%
           column_to_rownames('nombre') %>%
           dist(diag = T, upper = T)
         u <- hclust(d, method = 'average')
         plot(u, main = x, cex = 0.5)
       }))
```

<img src="README_files/figure-gfm/unnamed-chunk-18-1.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-2.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-3.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-4.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-5.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-6.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-7.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-8.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-9.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-10.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-11.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-12.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-13.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-14.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-15.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-16.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-17.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-18.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-19.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-20.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-21.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-22.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-23.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-24.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-25.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-26.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-27.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-28.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-29.png" width="100%" /><img src="README_files/figure-gfm/unnamed-chunk-18-30.png" width="100%" />

## Referencias

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-borcard_numerical_2018" class="csl-entry">

Borcard, Daniel, François Gillet, and Pierre Legendre. 2018. *Numerical
Ecology with R*. Use R! Cham: Springer International Publishing.
<https://doi.org/10.1007/978-3-319-71404-2>.

</div>

<div id="ref-gbiforg2024what" class="csl-entry">

GBIF.org. 2024. “What Is GBIF?” <https://www.gbif.org/what-is-gbif>.

</div>

<div id="ref-lomolino2006biogeography" class="csl-entry">

Lomolino, Mark V, Brett R Riddle, James H Brown, and James H Brown.
2006. *Biogeography*. QH84 L65 2006. Sinauer Associates Sunderland, MA.

</div>

<div id="ref-martinez2022material" class="csl-entry">

Martínez-Batlle, José-Ramón. 2022.
“Biogeografia-202202/Material-de-Apoyo.” biogeografia-202202.
<https://github.com/biogeografia-202202/material-de-apoyo>.

</div>

</div>
