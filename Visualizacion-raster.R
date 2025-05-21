#===============================================================================
#                         CENTRO DE INVESTIGACIÓN EN MATEMÁTICAS
#                                 UNIDAD MONTERREY
#
#                          ANÁLISIS ESPACIAL DE DATOS RASTER
#             Visualización sobre mapas tipo Google Maps con bloques 3x3
#
#                         Autor: Ing. Nelson Ariza Morales
#                         Fecha: 20-05-25
#
#           Este script permite analizar datos raster multivariados, aplicar
#           técnicas de reducción espacial con bloques representativos y 
#           visualizar los resultados georreferenciados usando mapas base
#           provistos por la API de Google Maps a través de ggmap en R.
#===============================================================================



#===============================================================================
# CARGA Y ANÁLISIS ESPACIAL DE RASTERS CON MAPAS DE GOOGLE
#===============================================================================
# Requiere: API Key de Google Maps habilitada para `ggmap`
#===============================================================================

#------------------- 1. CONFIGURACIÓN INICIAL -------------------
setwd("")  # Directorio de trabajo
list.files()

#------------------- 2. PAQUETES NECESARIOS -------------------
library(raster)       # Manejo de datos raster
library(ggmap)        # Mapas base de Google
library(dplyr)        # Manipulación de datos
library(viridis)      # Paleta de colores
library(ggplot2)      # Gráficas
library(tibble)       # tibbles
library(fields)       # Funciones espaciales
library(matrixStats)  # Estadística de matrices
library(imager)       # Imágenes rasterizadas

#------------------- 3. LECTURA DEL RASTER -------------------
year <- "2018"
raster_files <- list.files(pattern = "\\.tif$", full.names = TRUE)
rstr <- stack(raster_files[1])  # Se asume raster multibanda (día 1)

cat("Resolución:\n"); print(res(rstr))
cat("Extensión:\n"); print(extent(rstr))
cat("Dimensiones:\n"); print(dim(rstr))  # filas, columnas, capas

#------------------- 4. RECORTE DEL RASTER -------------------
# Elimina márgenes para centrarse en la CDMX
e <- extent(rstr)
x_min <- e@xmin + 25 * res(rstr)[1]
x_max <- e@xmax - 25 * res(rstr)[1]
y_min <- e@ymin + 20 * res(rstr)[2]
y_max <- e@ymax - 35 * res(rstr)[2]

new_extent <- extent(x_min, x_max, y_min, y_max)
cr_rstr <- crop(rstr, new_extent)

cat("Post-recorte:\n"); print(dim(cr_rstr))

#===============================================================================
# VISUALIZACIÓN EN MAPA TIPO GOOGLE MAPS (VARIABLE CONTINUA)
#===============================================================================

#------------------- 5. CONFIGURACIÓN GOOGLE MAPS -------------------
register_google(key = "TU_API_KEY_AQUÍ")  # Reemplazar por tu propia API
cdmx_coords <- c(lon = -99.143209, lat = 19.432608)

#------------------- 6. TRANSFORMAR RASTER A DATA FRAME -------------------
r_df <- as.data.frame(rasterToPoints(cr_rstr))
colnames(r_df) <- c("LON", "LAT", names(cr_rstr))

# Variable a graficar (debe coincidir con una capa del raster)
variable <- "temperature_2m"

#------------------- 7. OBTENER MAPA BASE -------------------
cdmx_map <- get_map(
  location = cdmx_coords,
  zoom = 10,
  maptype = "terrain",
  color = "bw",
  scale = 2
)

#------------------- 8. MAPA TIPO HEATMAP -------------------
ggmap(cdmx_map, extent = "device") +
  geom_tile(data = r_df, aes_string(x = "LON", y = "LAT", fill = variable), alpha = 0.8) +
  scale_fill_viridis_c(name = "Valor") +
  labs(title = paste("Mapa de", variable)) +
  coord_cartesian(xlim = range(r_df$LON), ylim = range(r_df$LAT)) +
  theme_minimal()

#===============================================================================
# VISUALIZACIÓN DE BLOQUES REPRESENTATIVOS (AOD IMPUTADO)
#===============================================================================

#------------------- 9. COORDENADAS CENTRALES POR BLOQUE 3x3 -------------------
n_row <- dim(cr_rstr)[1]
n_col <- dim(cr_rstr)[2]

lon_coords <- lat_coords <- x_idx <- y_idx <- c()
for (i in 0:27) {
  for (j in 0:25) {
    i_c <- min(2 + i * 3, n_row)
    j_c <- min(2 + j * 3, n_col)
    
    xy <- xyFromCell(cr_rstr, cellFromRowCol(cr_rstr, i_c, j_c))
    lon_coords <- c(lon_coords, xy[1])
    lat_coords <- c(lat_coords, xy[2])
    x_idx <- c(x_idx, j_c)
    y_idx <- c(y_idx, i_c)
  }
}

df_coords <- data.frame(
  X = x_idx, Y = y_idx,
  LON = lon_coords, LAT = lat_coords
)

#------------------- 10. CARGA DE VALORES IMPUTADOS -------------------
# Se asume objeto: AOD_33[día, 728] ya cargado (e.g. readRDS)
dia <- 1
df_coords$AOD <- as.numeric(AOD_33[dia, ])

#------------------- 11. MAPA AOD IMPUTADO -------------------
block_width <- 3 * res(cr_rstr)[1]
block_height <- 3 * res(cr_rstr)[2]

cdmx_map <- get_map(
  location = c(lon = mean(df_coords$LON), lat = mean(df_coords$LAT)),
  zoom = 10, maptype = "terrain", color = "bw", scale = 2
)

ggmap(cdmx_map, extent = "device") +
  geom_tile(data = df_coords, aes(x = LON, y = LAT, fill = AOD),
            width = block_width, height = block_height, alpha = 0.9) +
  scale_fill_viridis_c(name = "AOD imputado") +
  coord_cartesian(xlim = range(df_coords$LON), ylim = range(df_coords$LAT)) +
  labs(
    title = paste("AOD Imputado - Día", dia),
    caption = "Fuente: Datos reconstruidos con bloques 3x3"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

