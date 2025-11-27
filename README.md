Análisis del PIB de Argentina con Datos de INDEC

Descargué, limpié, analicé y grafiqué los datos de Valor agregado bruto a precios básicos para distintos rubros de la economía argentina. Voy a proceder a realizar análisis de datos y sacaré conclusiones en base a esos datos, sin hacer análisis económico, lo cual no podría hacer pues no soy economista.



Primero, miremos el VAB total desde 2004. 
![total](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/Total.png)

Vemos un gran crecimiento 2004-2011, luego estancamiento 2012-2018. Caída leve 2019, caída profunda por pandemia en 2020, recuperación 2021 y 2022 (el VAB más alto de la historia), caída 2023, otra caída 2024 y aparente recuperación en 2025.



Novedades y dónde encontrar los gráficos
-------------------------------------

Se movieron los PNG generados a la carpeta `plot/`. Si ejecutas `anali.R` se generarán los mismos gráficos dentro de `plot/`.

Además, se incluyó el análisis del EMAE (Estimador Mensual de Actividad Económica) junto al VAB trimestral. Las series EMAE se re-basan a promedio 2023 = 100 y hay una opción para desestacionalizar las series (usando multiplicative decomposition). Para la estética de los gráficos, las escalas del eje y ahora usan `scales::pretty_breaks(n = 6)` para generar saltos relativos en cada panel.

Ejemplos de gráficos EMAE están disponibles en el mismo `plot/`:

* `general_emae.png` — Panorama general entre rubros (EMAE, mensual)
* `suben_emae.png` — Rubros que crecieron (EMAE)
* `bajan_emae.png` — Rubros que perdieron actividad (EMAE)

Todos los PNG manejados por este repo ahora están en la carpeta `plot/`.


Ahora, miramos los rubros más significativos de la economía argentina, indicando el trimestre de cada año comparable con el último dato disponible con un círculo en cada rubro. El promedio anual se indica como una línea azul para cada año.

![grandes](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/grandes.png)

Aquí se puede ver que hay rubros donde la actividad económica es mayor a la de 2023 como el campo y la actividad inmobiliaria y de alquiler, pero otros dos rubros de enorme peso en la economía argentina como el comercio y la industria manufacturera crecieron en 2025 respecto a 2024 pero sin recuperar los niveles de actividad de 2023.

Esto puede observarse en un gráfico más simplificado donde sólo se comparan los valores de estas mismas industrias para el 2ndo trimestre:

![grandes_Q2](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/grandes_Q2.png)

Analizando más en detalle los rubros donde la actividad en 2025 supera los niveles de 2023 y 2024, podemos encontrar a las minas y canteras, agricultura, intermediación financiera y transporte.
![suben](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/suben.png)


Por otro lado, hay grandes rubros donde se observa alguna recuperación en comparación con 2024, pero la actividad sigue siendo muy inferior a la de 2023. Esto ocurre con el comercio, la construcción, la industria manufacturera en general, y en particular la industria química.
![bajan2](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/bajan1.png)

Otros rubros que siguen sin recuperar los niveles de 2023 son Fabricación de maquinaria, de metales comunes, fabricación de vehículos y servicios culturales y deportivos.
![bajan1](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/bajan2.png)

En resumen, no se observa una recuperación homogénea de la economía, sino que hay sectores donde la actividad continúa a niveles muy por debajo de los de 2023, como el comercio, la construcción y la industria, los cuales son los sectores que mayor empleo generan en la economía argentina. Mientras tanto, los sectores donde sí aumentó la actividad por encima de los niveles de 2023 son Intermediación financiera, impuestos netos de subsidios, hoteles y restaurantes, y minería.

![general_emae](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/2015-2025_general_emae.png)

Cómo reproducir
----------------

1. Instale las dependencias de R si no tiene ya instaladas: `tidyverse`, `zoo`, `readxl`, `scales`.
2. Ejecute el script:

```bash
Rscript anali.R
```

3. Los gráficos se guardarán en `plot/`.
