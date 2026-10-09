# Análisis del repositorio y propuestas de mejora

Revisión de `clustering.sismos.R`, `sismos.csv` y `clustering.results.table.csv`. Las cifras de este documento salen de los datos del propio repositorio y se pueden reproducir con `scripts/prepare_web_data.py` y la web en `docs/`.

## 1. Hallazgos en los datos

**Dos formatos de fecha en la misma columna.** `LocalHour` tiene 1037 filas en `dd-mm-aaaa H:M` (mayo de 2016 a mayo de 2018) y 391 filas en `m-dd-aaaa H:M:S` (abril de 2016). Un parser único interpreta mal una de las dos partes. El script en R no usa el tiempo, por eso el problema no aparece, pero cualquier análisis temporal lo va a tropezar.

**El catálogo empieza casi cinco horas después del sismo principal.** El primer evento es del 16 de abril de 2016 a las 23:47 hora local. El Mw 7.8 fue a las 18:58. Las primeras horas, que concentran la mayor cantidad de réplicas, no están. Hay que decirlo en el artículo porque afecta cualquier estimación de Omori o de completitud.

**Abril de 2016 viene sin ciudad cercana.** 385 de los 391 eventos de abril tienen `CloserCity = "nas"`. El script filtra `!(CloserCity %in% "nas")` antes del análisis de centralidad, así que esa parte del estudio ignora el periodo más activo de la secuencia. En los resultados guardados, 377 de los 853 eventos agrupados tienen `nas`.

**Profundidad fija de 10 km.** 266 eventos (160 dentro del área de estudio) tienen exactamente 10 km, que es el valor que la red asigna cuando no logra estimar la profundidad. Si se usa la profundidad como variable, esos eventos deben marcarse o excluirse.

**`clustering.results.table.csv` no corresponde al script actual.** El script filtra a Manabí y Esmeraldas y quita `nas` antes de `write.table`, pero el CSV guardado tiene 671 filas con región `Near Coast of Ecuador` y 377 con `nas`. El archivo se generó con otra versión del código. Para que el artículo sea reproducible, el CSV publicado debe salir del script publicado.

**Encabezado desalineado.** `write.table(tabla, sep=",")` escribe los nombres de fila como primera columna pero solo 9 nombres de columna para 10 columnas. Pandas y `read.csv` lo leen, pero otras herramientas desplazan todo una columna. Se corrige con `col.names = NA` o `row.names = FALSE`.

**Codificación.** `sismos.csv` está en ISO-8859-1 (el símbolo de grado aparece como `�` en UTF-8). Conviene declararlo con `fileEncoding = "latin1"` o convertir el archivo a UTF-8.

## 2. Problemas en `clustering.sismos.R`

1. `setwd("JCC2018")` hace que el script falle en cualquier otra máquina. Usar rutas relativas al proyecto (`here::here()`) o un proyecto de RStudio.
2. `sub('[^A-Za-z0-9.]', '', Lat)` quita solo el primer carácter no alfanumérico, y su resultado depende de cómo se leyó la codificación. Es más seguro extraer el número con una expresión regular explícita: `as.numeric(sub("^([0-9.]+).*", "\\1", Lat))`.
3. La región `"Peru-Ecuador Border Region"` se renombra a `"Peru-Ecuador Border"` y luego se intenta borrar con el nombre viejo. Esa línea no hace nada.
4. `cut(Mag, c(1.0, 3.0, 3.9, ...))` deja `Mag = 1.0` como `NA` (falta `include.lowest = TRUE`), las etiquetas `"1.0 - 3.0"` y `"3.0 - 3.9"` se solapan, y el intervalo `(6.9, 8]` se llama `"Epicenter"`, así que cualquier réplica M7 quedaría etiquetada como epicentro.
5. `subset(Depth > 0)` descarta eventos con profundidad 0, que en catálogos regionales suelen ser eventos someros con profundidad fijada. Revisar si es intencional.
6. `get_map(location = 'Riobamba', ...)` depende de la API de Google Maps, que exige clave desde 2018. El mapa ya no se puede regenerar. Alternativa sin claves: `sf` + `rnaturalearth`, o `leaflet` para versión web.
7. En el análisis de centralidad los pesos son `log(distancia en metros)` y los valores menores o iguales a cero se reemplazan por 0. `igraph::closeness` interpreta los pesos como distancias, así que un peso 0 vuelve idénticos a dos eventos distintos e infla la cercanía. El logaritmo comprime las diferencias y no tiene una justificación física. `cat(which.max(medida))` imprime la posición dentro del vector y no el identificador del evento: usar `names(which.max(medida))`.
8. `clustering.results.distances.csv` se escribe pero no está en el repositorio. Puede regenerarse desde las coordenadas, así que basta con documentarlo o eliminar la escritura.
9. `library()` dentro de la función y `dplyr` cargado dos veces. Mover todas las cargas al inicio y fijar versiones con `renv`.

## 3. Mejoras metodológicas

**Validar los grupos.** La silueta global con distancia de Haversine es 0,34. Por grupo: G1 0,56, G2 0,32, G3 0,06, G4 0,75, G5 −0,19. G3 y G5 se solapan con sus vecinos. Reportar silueta o Davies-Bouldin por grupo le da al lector una medida de calidad que hoy no tiene.

**Comparar con otros algoritmos.** DBSCAN con ε = 12 km y 8 vecinos mínimos da un ARI de 0,26 frente a MST-kNN y deja la zona Muisne a Jama en un solo grupo grande. HDBSCAN, k-medoides con Haversine o ST-DBSCAN (espacio y tiempo) son referencias razonables. La web incluye DBSCAN en vivo para explorar esto.

**Usar la tercera dimensión.** Agrupar por distancia hipocentral (latitud, longitud y profundidad) en lugar de solo epicentral, excluyendo o ponderando los eventos de 10 km fijos.

**Incorporar el tiempo.** El exponente de Omori estimado desde el día 1 es p ≈ 0,97, típico de una secuencia de subducción. Separar réplicas de sismicidad de fondo (declustering de Gardner-Knopoff o Reasenberg) antes de agrupar evita mezclar procesos distintos.

**Estadística de magnitudes.** Magnitud de completitud Mc ≈ 3,8 por máxima curvatura y valor b ≈ 0,75 ± 0,04 (Aki) en el área de estudio. Por grupo: G1 0,70, G2 0,77, G3 0,73. Comparar b entre grupos es una forma física de decir si los grupos representan zonas con comportamiento distinto.

**Estabilidad.** Repetir el agrupamiento con submuestras (bootstrap) y medir el ARI entre corridas. Si los grupos cambian mucho con pequeñas perturbaciones, no conviene interpretarlos como zonas sísmicas.

**Catálogo más completo.** El catálogo del IG-EPN o de USGS ComCat permite recuperar las primeras horas y tener magnitudes homogéneas.

## 4. Mejoras al repositorio

- Ampliar el README: resumen del artículo, referencia o DOI, requisitos (versión de R y paquetes) y cómo ejecutar el análisis.
- Organizar en carpetas: `data/`, `R/`, `scripts/`, `docs/`.
- Agregar licencia (código y datos por separado) y un `CITATION.cff`.
- Fijar dependencias con `renv::snapshot()`.
- Archivar una versión en Zenodo para obtener DOI.

## 5. La web de visualización (`docs/`)

`docs/index.html` es una página estática sin servidor. Muestra:

- Mapa con las réplicas coloreadas por grupo MST-kNN, por DBSCAN, por profundidad o por tiempo, con el tamaño según la magnitud y el epicentro marcado.
- Filtros por magnitud mínima, profundidad máxima y grupo, y una línea de tiempo con reproducción animada de la secuencia.
- Tabla de grupos con silueta, centro, distancia al epicentro y primer evento.
- Gráficos de Omori, Gutenberg-Richter, perfil latitud y profundidad, y magnitud en el tiempo.
- DBSCAN calculado en el navegador con ARI frente a MST-kNN.

Para regenerar los datos:

```bash
python3 scripts/prepare_web_data.py            # docs/data.js desde los CSV
python3 scripts/build_basemap.py <geoBoundaries-ECU-ADM1_simplified.geojson> <ne_10m_admin_0_countries.geojson>
```

Para verla localmente: `python3 -m http.server -d docs` y abrir `http://localhost:8000`. Para publicarla en GitHub Pages: Settings → Pages → Branch `main`, carpeta `/docs`.
