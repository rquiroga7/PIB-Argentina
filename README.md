Análisis del PIB de Argentina con Datos de INDEC

Descargué, limpié, analicé y grafiqué los datos de Valor agregado bruto a precios básicos para distintos rubros de la economía argentina. Voy a proceder a realizar análisis de datos y sacaré conclusiones en base a esos datos, sin hacer análisis económico, lo cual no podría hacer pues no soy economista.



Primero, miremos el VAB total desde 2004. 
![total](https://raw.githubusercontent.com/rquiroga7/PIB-Argentina/main/plot/Total.png)

Vemos un gran crecimiento 2004-2011, luego estancamiento 2012-2018. Caída leve 2019, caída profunda por pandemia en 2020, recuperación 2021 y 2022 (el VAB más alto de la historia), caída 2023, otra caída 2024 y aparente recuperación en 2025.



Novedades y dónde encontrar los gráficos
-------------------------------------

Se movieron los PNG generados a la carpeta `plot/`. Si ejecutas `anali.R` se generarán los mismos gráficos dentro de `plot/`.

Además, se incluyó el análisis del EMAE (Estimador Mensual de Actividad Económica) junto al VAB trimestral. Las series EMAE se re-basan a promedio 2023 = 100 y hay una opción para desestacionalizar las series con **X-13ARIMA-SEATS** (a través del paquete R `seasonal`), incluyendo un regresor de días hábiles construido a partir de la serie diaria de tipo de cambio del BCRA (`com3500.xls`). Para la estética de los gráficos, las escalas del eje y ahora usan `scales::pretty_breaks(n = 6)` para generar saltos relativos en cada panel.

Ejemplos de gráficos EMAE están disponibles en el mismo `plot/`:

* `general_emae.png` — Panorama general entre rubros (EMAE, mensual)
* `general_emae_desest.png` — Idem, desestacionalizado con X-13ARIMA-SEATS
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

Desestacionalización con X-13ARIMA-SEATS
----------------------------------------

La desestacionalización del EMAE se realiza con X-13ARIMA-SEATS (envoltorio R `seasonal`, que requiere el binario `x13binary`). El ajuste se hace en dos etapas dentro de un único modelo Reg-ARIMA + SEATS:

1. **Regresores de calendario** (estimados en la Reg-ARIMA):
   * **6 dummies de día de la semana**: `lun..sab` (conteos totales por mes; el domingo es la categoría de referencia). Se incluyen los sábados aunque el BCRA no publique en sábados: para varios sectores (comercio, restaurantes, servicios) la actividad del sábado es positiva, y la regresión estima su propio coeficiente.
   * **`feriados`**: cantidad de días hábiles (lun–vie) ausentes de `com3500.xls` — la serie diaria de tipo de cambio de referencia del BCRA, que sólo publica en días hábiles. Sirve como proxy de feriados nacionales y puentes.

   Los conteos se construyen sobre un calendario completo dentro del rango cubierto por `com3500.xls` (2002-03-04 a 2026-06-02). El archivo `com3500.xls` es entonces la única fuente externa que entra en el ajuste estacional: define implícitamente qué días son feriados (los días hábiles ausentes) y qué días son laborables (los presentes).

2. **Componente estacional** (extraído por SEATS de los residuos de la Reg-ARIMA).

La elección del modelo ARIMA es automática (PICKMDL / `automdl`), con `transform.function = "auto"` (log si la serie es positiva) y `seats.noadmiss = "yes"` para aceptar aproximaciones si la descomposición canónica de SEATS no es admisible. Se desactiva el test AIC sobre los regresores `td` y `easter` incorporados por X-13 (`regression.aictest = NULL`) porque ya proveemos nuestros propios regresores de calendario; sin esto, el ajuste falla por conflicto entre los dos conjuntos.

Coeficientes típicos (Industria Manufacturera, ARIMA (3,1,1)(0,1,1)):

| regresor | coeficiente | interpretación |
|---|---|---|
| lun | 2.48 | efecto promedio de un lunes |
| mar | 2.98 | día hábil con mayor efecto |
| mié | 1.50 | día hábil con menor efecto |
| jue | 2.68 | |
| vie | 2.49 | |
| **sáb** | **1.21** | actividad positiva el sábado |
| **feriados** | **−1.00** | cada hábil perdido reduce actividad ~1 unidad (escala log) |

La diferencia clave respecto al enfoque anterior (dividir cada valor por la cantidad de días hábiles del mes y luego desestacionalizar) es que el efecto calendario se estima *conjuntamente* con la descomposición estacional, en lugar de corregir antes y volver a desestacionalizar después — lo cual genera doble corrección y distorsión. La especificación con 6 dummies totales + variable de feriados es la forma estándar "td + holi" de X-13.

Cómo reproducir
----------------

1. Instale las dependencias de R si no tiene ya instaladas: `tidyverse`, `zoo`, `readxl`, `scales`, `seasonal` (que requiere `x13binary`).
2. Ejecute el script:

```bash
Rscript anali.R
```

3. Los gráficos se guardarán en `plot/`.
