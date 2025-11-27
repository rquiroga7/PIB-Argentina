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


Ahora, miramos los rubros más significativos de la economía argentina, indicando los terceros trimestres con un círculo en cada rubro. El promedio anual se indica como una línea azul para cada año.

![grandes](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/grandes.png)

Aquí se puede ver que hay rubros donde la actividad económica es mayor a la de 2023 como el campo y la elaboración de alimentos y bebidas, pero otros dos rubros de enorme peso en la economía argentina como el comercio y el resto de la industria manufacturera crecieron en 2025 respecto a 2024 pero aún no recuperan los niveles de actividad de 2023.

Esto puede observarse en un gráfico más simplificado donde sólo se comparan los valores de estas mismas industrias para el 2ndo trimestre:

![grandes_Q2](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/grandes_Q2.png)

Analizando más en detalle los rubros donde la actividad supera los niveles de 2023 y 2024, podemos encontrar a la minas y canteras, restaurantes y bares, .
![suben](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/suben.png)

Algunos rubros donde la actividad rebotó respecto al 2do trimestre de 2024, y están en niveles similares a los de 2023 serían Actividades Inmobiliarias, Electricidad, gas y agua, Elaboración de alimentos y Transporte y Comunicaciones.
![iguales](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/iguales.png)

Por otro lado, hay grandes rubros donde se observa alguna recuperación en comparación con el 2do trimestre de 2024, pero donde la actividad en el 3er trimestre de 2024 sigue siendo bajísima, en algunos casos sólo superando el nivel de actividad de la pandemia (2020).
Esto ocurre con la construcción, y en menor medida, con el comercio, la fabricación de cauchos y plásticos, y la industria química.
![bajan2](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/bajan2.png)

Por otro lado, el grueso de la industria y manufactura no-alimentos se recupera un poco en el 3er trimestre respecto al 2do de 2024, pero sigue en niveles bajísimos, también cercanos a los de la pandemia.
Aquí podemos observar algunos rubros manufactureros en esa situación, Fabricación de maquinaria, de metales comunes, de productos minerales no metálicos y fabricación de vehículos.
![bajan1](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/bajan1.png)

En resumen, no se observa una recuperación homogénea de la economía, sino que hay sectores donde la actividad continúa a niveles similares a los de 2020-2021, como la construcción y la industria no-alimenticia. Mientras tanto, el comercio mayorista y minorista se recuperó levemente entre el secundo y el tercer trimestre de 2024, pero aún está lejos de recuperar los niveles de 2023.

Cómo reproducir
----------------

1. Instale las dependencias de R si no tiene ya instaladas: `tidyverse`, `zoo`, `readxl`, `scales`.
2. Ejecute el script:

```bash
Rscript anali.R
```

3. Los gráficos se guardarán en `plot/`.
