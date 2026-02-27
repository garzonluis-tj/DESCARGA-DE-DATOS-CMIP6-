# DESCARGA-DE-DATOS-CMIP6-
Descarga de datos cmip6 en formato .nc para un area especifica
# DESCARGA DE DATOS NEX-GDDP-CMIP6 VÍA THREDDS
# Modelo: MPI-ESM1-2-HR | Escenarios: historical, SSP245, SSP585
# Período: 1980-2100 | Versión corregida con sufijo _v2.0
# =============================================================================

# 1. CONFIGURACIÓN INICIAL
# -----------------------------------------------------------------------------
ruta_base <- "D:/MGC"
setwd(ruta_base)

# Crear carpetas necesarias
dirs_crear <- c("datos_nasa/historical", 
                "datos_nasa/ssp245", 
                "datos_nasa/ssp585",
                "resultados/csv_poligonos", 
                "resultados/downscaling",
                "resultados/nasa_raw")

for(d in dirs_crear) {
  if(!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# 2. CARGAR LIBRERÍAS
# -----------------------------------------------------------------------------
library(httr)      # Para hacer solicitudes HTTP
library(ncdf4)     # Para leer archivos NetCDF
library(raster)    # Para manejo de datos espaciales
library(sf)        # Para manejo de shapefiles
library(terra)     # Alternativa moderna para raster
library(dplyr)     # Para manipulación de datos
library(tidyverse) # Conjunto de paquetes para ciencia de datos

cat("✅ Librerías cargadas correctamente\n")

# 3. CONFIGURAR CREDENCIALES DE NASA EARTHDATA
# -----------------------------------------------------------------------------
usuario_nasa <- "-------------------"           # <-- TU USUARIO
password_nasa <- "-------------------"  # <-- TU CONTRASEÑA

credenciales_nasa <- list(
  usuario = usuario_nasa,
  password = password_nasa
)

# Configurar autenticación para sesión
config_autenticacion <- function(usuario, password) {
  netrc_path <- file.path(path.expand("~"), ".netrc")
  writeLines(c(
    "machine urs.earthdata.nasa.gov",
    paste("login", usuario),
    paste("password", password)
  ), netrc_path)
  
  Sys.setenv(USER = usuario)
  Sys.setenv(PASSWORD = password)
  
  cat("✅ Autenticación configurada\n")
}

config_autenticacion(usuario_nasa, password_nasa)

# 4. CARGAR SHAPEFILE Y OBTENER BOUNDING BOX
# -----------------------------------------------------------------------------
cat("\n📦 Cargando shapefile...\n")

if(!file.exists("consata.shp")) {
  stop("❌ ERROR: No se encuentra el archivo consata.shp")
}

poligonos <- st_read("consata.shp", quiet = TRUE)
valores_UH7 <- poligonos$UH_7
n_poligonos <- length(valores_UH7)
poligonos_vect <- vect(poligonos)

# Obtener bounding box y agregar margen
bbox <- st_bbox(poligonos)
lon_min <- bbox$xmin - 0.1
lon_max <- bbox$xmax + 0.1
lat_min <- bbox$ymin - 0.1
lat_max <- bbox$ymax + 0.1

cat("✓ Shapefile cargado:", n_poligonos, "polígonos\n")
cat("📊 Bounding box para extracción:\n")
cat("   Latitud:", round(lat_min, 4), "a", round(lat_max, 4), "\n")
cat("   Longitud:", round(lon_min, 4), "a", round(lon_max, 4), "\n")

# 5. FUNCIÓN PARA CONSTRUIR URL DE DESCARGA THREDDS
# -----------------------------------------------------------------------------
construir_url_thredds <- function(modelo, escenario, variable, año, version = "v2.0") {
  
  # Validar parámetros (solo para MPI-ESM1-2-HR)
  stopifnot(modelo == "MPI-ESM1-2-HR")
  stopifnot(escenario %in% c("historical", "ssp245", "ssp585"))
  stopifnot(variable %in% c("pr", "tas"))
  stopifnot(is.numeric(año) && año >= 1980 && año <= 2100)
  
  # Construir URL base
  url_base <- paste0("https://ds.nccs.nasa.gov/thredds/ncss/grid/AMES/NEX/GDDP-CMIP6/",
                     modelo, "/", escenario, "/r1i1p1f1/", variable, "/")
  
  # Nombre del archivo con sufijo de versión
  archivo <- paste0(variable, "_day_", modelo, "_", escenario, 
                    "_r1i1p1f1_gn_", año, "_", version, ".nc")
  
  # URL completa al archivo (sin parámetros aún)
  url_completa <- paste0(url_base, archivo)
  
  # Parámetros para el subconjunto espacial y temporal
  params <- list(
    var = variable,
    north = lat_max,
    west = lon_min,
    east = lon_max,
    south = lat_min,
    horizStride = 1,
    time_start = paste0(año, "-01-01T12:00:00Z"),
    time_end = paste0(año, "-12-31T12:00:00Z"),
    accept = "netcdf3",
    addLatLon = "true"
  )
  
  # Construir URL final con parámetros
  url_params <- paste0(names(params), "=", unlist(params), collapse = "&")
  url_final <- paste0(url_completa, "?", url_params)
  
  return(list(
    base = url_completa,
    full = url_final,
    archivo = archivo,
    modelo = modelo,
    escenario = escenario,
    variable = variable,
    año = año,
    version = version
  ))
}

# 6. FUNCIÓN PARA DESCARGAR Y PROCESAR UN AÑO
# -----------------------------------------------------------------------------
descargar_ano_thredds <- function(modelo, escenario, variable, año,
                                  lat_min, lat_max, lon_min, lon_max,
                                  credenciales = NULL, version = "v2.0") {
  
  cat("\n   📥 Procesando:", modelo, escenario, variable, año, "\n")
  
  # Construir URL
  urls <- construir_url_thredds(modelo, escenario, variable, año, version)
  
  cat("      📂 Archivo solicitado:", urls$archivo, "\n")
  cat("      🔗 URL completa (primeros 100 caracteres):\n")
  cat("         ", substr(urls$full, 1, 100), "...\n")
  
  # Archivo temporal para descarga
  archivo_temporal <- tempfile(fileext = ".nc")
  
  tryCatch({
    # Intentar con autenticación
    if(!is.null(credenciales)) {
      cat("      🔐 Intentando con autenticación...\n")
      response <- GET(urls$full, 
                      authenticate(credenciales$usuario, credenciales$password),
                      write_disk(archivo_temporal, overwrite = TRUE),
                      timeout(60))
    } else {
      cat("      🌐 Intentando sin autenticación...\n")
      response <- GET(urls$full, 
                      write_disk(archivo_temporal, overwrite = TRUE),
                      timeout(60))
    }
    
    codigo <- status_code(response)
    
    if(codigo == 200) {
      tamaño <- file.size(archivo_temporal)
      cat("      ✅ Descarga exitosa! (", tamaño, "bytes)\n")
      
      # Guardar archivo permanentemente
      dir_destino <- file.path("datos_nasa", escenario)
      if(!dir.exists(dir_destino)) dir.create(dir_destino, recursive = TRUE)
      
      archivo_destino <- file.path(dir_destino, 
                                   paste0(variable, "_", modelo, "_", año, ".nc"))
      
      file.copy(archivo_temporal, archivo_destino, overwrite = TRUE)
      cat("      💾 Archivo guardado en:", archivo_destino, "\n")
      
      unlink(archivo_temporal)
      return(archivo_destino)
      
    } else {
      cat("      ⚠️ Error en descarga. Código:", codigo, "\n")
      unlink(archivo_temporal)
      return(NULL)
    }
    
  }, error = function(e) {
    cat("      ❌ Error en la descarga:", e$message, "\n")
    unlink(archivo_temporal)
    return(NULL)
  })
}

# 7. VERIFICACIÓN RÁPIDA (OPCIONAL - DESCOMENTAR PARA PROBAR)
# -----------------------------------------------------------------------------
# cat("\n", strrep("=", 70), "\n", sep="")
# cat("🔬 VERIFICANDO DESCARGA DE PRUEBA\n")
# cat(strrep("=", 70), "\n", sep="")
# 
# resultado_prueba <- descargar_ano_thredds(
#   modelo = "MPI-ESM1-2-HR",
#   escenario = "historical",
#   variable = "pr",
#   año = 1980,
#   lat_min = lat_min, lat_max = lat_max,
#   lon_min = lon_min, lon_max = lon_max,
#   credenciales = credenciales_nasa,
#   version = "v2.0"
# )
# 
# if(!is.null(resultado_prueba)) {
#   cat("\n✅ VERIFICACIÓN EXITOSA!\n")
# }

# 8. DESCARGA COMPLETA PARA MPI-ESM1-2-HR
# -----------------------------------------------------------------------------
cat("\n", strrep("=", 70), "\n", sep="")
cat("🚀 INICIANDO DESCARGA COMPLETA - MODELO MPI-ESM1-2-HR\n")
cat(strrep("=", 70), "\n", sep="")

modelo <- "MPI-ESM1-2-HR"
escenarios <- c("historical", "ssp245", "ssp585")
variables <- c("pr", "tas")

# Definir años para cada escenario
años_historical <- 1980:2014
años_ssp245 <- 2015:2100
años_ssp585 <- 2015:2100

# Calcular total de archivos a descargar
total_archivos <- length(escenarios) * length(variables) * (
  length(años_historical) + length(años_ssp245) + length(años_ssp585)
)

cat("📊 Resumen de descarga:\n")
cat("   Modelo:", modelo, "\n")
cat("   Escenarios: historical, SSP245, SSP585\n")
cat("   Variables: precipitación (pr) y temperatura (tas)\n")
cat("   Período historical: 1980-2014 (", length(años_historical), "años)\n")
cat("   Período SSP245: 2015-2100 (", length(años_ssp245), "años)\n")
cat("   Período SSP585: 2015-2100 (", length(años_ssp585), "años)\n")
cat("   Total archivos a descargar:", total_archivos, "\n\n")

# Contador para seguimiento
archivo_actual <- 0
archivos_exitosos <- 0
archivos_fallidos <- 0

# Registrar inicio
tiempo_inicio <- Sys.time()

# Bucle principal de descarga
for(escenario in escenarios) {
  
  # Seleccionar años según escenario
  if(escenario == "historical") {
    años_procesar <- años_historical
  } else if(escenario == "ssp245") {
    años_procesar <- años_ssp245
  } else if(escenario == "ssp585") {
    años_procesar <- años_ssp585
  }
  
  for(variable in variables) {
    for(año in años_procesar) {
      
      archivo_actual <- archivo_actual + 1
      
      # Mostrar progreso
      cat(sprintf("\n[%d/%d] ", archivo_actual, total_archivos))
      
      # Descargar archivo
      resultado <- descargar_ano_thredds(
        modelo = modelo,
        escenario = escenario,
        variable = variable,
        año = año,
        lat_min = lat_min, lat_max = lat_max,
        lon_min = lon_min, lon_max = lon_max,
        credenciales = credenciales_nasa,
        version = "v2.0"
      )
      
      if(!is.null(resultado)) {
        archivos_exitosos <- archivos_exitosos + 1
      } else {
        archivos_fallidos <- archivos_fallidos + 1
      }
      
      # Pausa para no saturar el servidor
      Sys.sleep(1)
    }
  }
}

# 9. RESUMEN DE LA DESCARGA
# -----------------------------------------------------------------------------
tiempo_fin <- Sys.time()
duracion <- difftime(tiempo_fin, tiempo_inicio, units = "mins")

cat("\n", strrep("=", 70), "\n", sep="")
cat("📊 RESUMEN DE DESCARGA\n")
cat(strrep("=", 70), "\n", sep="")
cat("\n")
cat("✅ Archivos descargados exitosamente:", archivos_exitosos, "\n")
cat("❌ Archivos fallidos:", archivos_fallidos, "\n")
cat("⏱️  Tiempo total:", round(duracion, 2), "minutos\n")
cat("📁 Los archivos se guardaron en: datos_nasa/[escenario]/\n")

if(archivos_fallidos > 0) {
  cat("\n⚠️ Nota: Algunos archivos fallaron. Posibles causas:\n")
  cat("   - El archivo no existe en el servidor\n")
  cat("   - Problemas de conexión temporal\n")
  cat("   - Timeout en la descarga\n")
}

# 10. FUNCIÓN PARA EXTRAER VALORES POR POLÍGONO (OPCIONAL - PARA USAR DESPUÉS)
# -----------------------------------------------------------------------------
extraer_valores_poligono <- function(archivo_nc, poligonos_vect, valores_UH7) {
  
  # Abrir archivo NetCDF
  nc <- nc_open(archivo_nc)
  
  # Obtener nombre de la variable
  variable <- names(nc$var)[1]
  
  # Crear raster brick
  raster_brick <- brick(archivo_nc, varname = variable)
  
  # Extraer valores para cada polígono
  df_resultado <- data.frame(UH_7 = valores_UH7)
  
  for(i in 1:nlyr(raster_brick)) {
    valores <- numeric(length(valores_UH7))
    for(j in 1:length(valores_UH7)) {
      val <- terra::extract(raster_brick[[i]], poligonos_vect[j], fun = mean, na.rm = TRUE)
      valores[j] <- if(!is.null(val) && nrow(val) > 0) val[1,1] else NA
    }
    df_resultado[[paste0("dia_", i)]] <- round(valores, 2)
  }
  
  nc_close(nc)
  return(df_resultado)
}

cat("\n✅ Script completado. Los datos están listos para su procesamiento.\n")
