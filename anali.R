# Cargar las librerías necesarias
library(tidyverse)
library(scales)
library(ggplot2)
library(zoo)
#Cargar la libreria de read_excel
library(readxl)
library(ggborderline)

# Ensure a directory exists for plot output
if (!dir.exists("plot")) {
  dir.create("plot")
}

# Leer el archivo CSV
data <- read_csv("sh_VBP_VAB_12_24.csv",col_names = FALSE, skip=2)

#Leer la pestaña 4, "Cuadro 3" del xls
data2 <- read_excel("sh_VBP_VAB_09_25.xls", sheet = 4, skip = 5, col_names = FALSE)

# Identificar las columnas a mantener: columna 1 y aquellas que no son múltiplos de 6 o 6 + 1
columns_to_keep <- c(1, which(!(seq_along(data) %% 6 %in% c(0, 1))))
# Subconjunto de datos para mantener solo las columnas identificadas
data <- data[, columns_to_keep]

# Identificar las columnas a mantener: columna 1 y aquellas que no son múltiplos de 6 o 6 + 1
columns_to_keep <- c(1, which(!(seq_along(data2) %% 6 %in% c(0, 1))))
# Subconjunto de datos para mantener solo las columnas identificadas
data2 <- data2[, columns_to_keep]


# Eliminar las filas 1 y 3
data <- data[-c(1,3),]
# Eliminar la fila 2
data2 <- data2[-c(2),]

# Reemplazar valores NA con ""
data[is.na(data)] <- ""
data2[is.na(data2)] <- ""


# Nuevo dataframe combinando datos con años y trimestres
# Reemplazar la fila 2 con la fila de años, comenzando en 2004, repitiendo 4 veces cada uno, excepto para 2024
#years <- c("",rep(2004:2023, each = 4),2024,2024,2024)
years <- c("",rep(2004:2024, each = 4),rep(2025, each=2))
#quarters<-c("",rep(c("Q1","Q2","Q3","Q4"),times=20),c("Q1","Q2","Q3"))
quarters<-c("",rep(c("Q1","Q2","Q3","Q4"),times=21),"Q1","Q2")
data2 <- data.frame(rbind(years,quarters,data2))


# Extraer información de años y trimestres
years2 <- as.vector(data2[1, -1])
quarters2 <- as.vector(data2[2, -1])

# Extraer información de nombres
names <- as.vector(data2[-c(1, 2), 1])

# Pivotar a formato largo
data_long <- data2[-c(1, 2), ] %>%
  pivot_longer(cols = -1, names_to = "column", values_to = "value") %>%
  mutate(year = as.numeric(rep(years2, times = length(names))),
         quarter = rep(quarters2, times = length(names)),
         name = rep(names, each = length(years2))) %>%
  select(year, quarter, name, value)
data_long <- data_long %>%
  mutate(year = as.numeric(year),
         value = as.numeric(value))

# Create new rows for "Industria manufacturera (no alimentos)" and "Valor agregado bruto a precios básicos (sin campo)"
data_long <- data_long %>%
  group_by(year, quarter) %>%
  mutate(value_no_alimentos = if_else(name == "Industria manufacturera", 
                                      value - sum(value[name == "Elaboración de productos alimenticios y bebidas"], na.rm = TRUE), 
                                      NA_real_),
         value_sin_campo = if_else(name == "Valor agregado bruto a precios básicos", 
                                   value - sum(value[name == "Agricultura, ganadería, caza y silvicultura"], na.rm = TRUE), 
                                   NA_real_)) %>%
  ungroup()

# Filter out NA values and create new rows
new_rows <- data_long %>%
  filter(!is.na(value_no_alimentos) | !is.na(value_sin_campo)) %>%
  mutate(name = case_when(
    !is.na(value_no_alimentos) ~ "Industria manufacturera (no alimentos)",
    !is.na(value_sin_campo) ~ "Valor agregado bruto a precios básicos (sin campo)"
  ),
  value = coalesce(value_no_alimentos, value_sin_campo)) %>%
  select(-value_no_alimentos, -value_sin_campo)

# Bind the new rows to the original data
data_long <- bind_rows(data_long, new_rows) %>%
  select(-value_no_alimentos, -value_sin_campo)


# Acortar algunos nombres usando case_when
data_long <- data_long %>%
  mutate(name = case_when(
    name == "Fabricación de vehículos automotores, remolques y semirremolques" ~ "Fabricación de vehículos",
    name == "Extracción de minerales metalíferos. Explotación de minas y canteras n.c.p." ~ "Minerales metalíferos",
    name == "Extracción de carbón y lignito; extracción de turba. Extracción de petróleo crudo y gas natural; actividades de servicios relacionadas con la extracción de petróleo y gas, excepto las actividades de prospección" ~ "Petróleo, gas, carbón",
    TRUE ~ name))

# Definir la función de trazado
plot_evolution <- function(data, names, filename,quarters) {
  if (length(names) == 1) {
    stripsize = 12
  } else {
    stripsize = 8
  }
  data %>%
    filter(name %in% names) %>%
    filter(quarter %in% quarters) %>%
    ggplot(aes(x = year, y = value, color = name)) +
    geom_borderline(size=1.2) +
    ylab("Valor Agregado Bruto a Precios Básicos") +
    xlab("Año") +
    ggtitle(paste0("VAB a Precios Básicos por sector económico")) +
    facet_wrap(~name, labeller=ggplot2::label_wrap_gen(width=30), scales = "free_y", ncol = 2) +
    theme_light() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = comma, minor_breaks = NULL) +
    scale_x_continuous(expand = c(0, 0.5), minor_breaks = NULL,breaks=seq(2004,2025,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = stripsize, face = "bold"))+
    labs(subtitle= paste0("Para el trimestre ",quarters," de cada año"), caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
  ggsave(file.path("plot", paste0(filename,"_",quarters,".png")), dpi = 300)
}

# Crear los gráficos
for (quarter in c("Q2")) {
  plot_evolution(data_long, c("Valor agregado bruto a precios básicos"), "Total", quarter)
  plot_evolution(data_long, c("Agricultura, ganadería, caza y silvicultura", "Elaboración de productos alimenticios y bebidas", "Industria manufacturera (no alimentos)", "Comercio mayorista, minorista y reparaciones"), "grandes", quarter)
  plot_evolution(data_long, c("Cultivos agrícolas", "Pesca", "Restaurantes, bares y cantinas", "Explotación de minas y canteras"), "suben", quarter)
  plot_evolution(data_long, c("Electricidad, gas y agua", "Transporte, almacenamiento y comunicaciones", "Elaboración de productos alimenticios y bebidas", "Actividades inmobiliarias, empresariales y de alquiler"), "iguales", quarter)
  plot_evolution(data_long, c("Fabricación de productos minerales no metálicos", "Fabricación de metales comunes", "Fabricación de maquinaria y equipo n.c.p.", "Fabricación de vehículos"), "bajan1", quarter)
  plot_evolution(data_long, c("Fabricación de sustancias y productos químicos", "Construcción", "Comercio mayorista, minorista y reparaciones", "Fabricación de productos de caucho y plástico"), "bajan2", quarter)
}


data_long2 <- data_long %>%
    mutate(fecha = as.yearqtr(paste0(year, quarter)))


plot_evolutionall <- function(data, names, filename, yearly = FALSE, lastq = NULL) {
  if (length(names) == 1) {
    stripsize = 12
  } else {
    stripsize = 8
  }
  if (length(names) == 2) {
    scalev = "fixed"
  } else {
    scalev = "free_y"
  }
  # Compute the last quarter to highlight in the plot if not provided
  if (is.null(lastq)) {
    lastq = data %>% dplyr::select(fecha) %>%  dplyr::slice_tail(n = 1) %>%  dplyr::pull() %>%    as.character() %>% substr(nchar(.) - 1, nchar(.))
  }

  p <- data %>%
    filter(name %in% names) %>%
    ggplot(aes(x = fecha, y = value, color = name)) +
    geom_line(size = 1.2) +
    ylab("Valor Agregado Bruto a Precios Básicos") +
    xlab("Año") +
    ggtitle(paste0(paste0("VAB a Precios Básicos por sector económico para cada sector económico para cada trimestre 2004-2024"))) +
    facet_wrap(~name, labeller=ggplot2::label_wrap_gen(width=30), scales = scalev, ncol = 2) +
    theme_light() +
    # Etiquetar cada tercer trimestre con un punto
  geom_point(data = data %>%  filter(name %in% names) %>% filter(quarter == lastq), aes(x = fecha, y = value,fill=name),color="black",pch=21, size = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = comma) +
    #zoo::scale_x_yearqtr(format = '%Y-T%q',expand = c(0, 0.22), minor_breaks = NULL, breaks = seq(2004.50, 2024.50, 1)) +
    zoo::scale_x_yearqtr(format = '%Y-T%q',expand = c(0, 0.5), minor_breaks = NULL, breaks = seq(2004.75, 2025.75, 1)) +
    #zoo::scale_x_yearqtr(format = '%Y-T%q',expand = c(0, 0.22), minor_breaks = NULL, breaks = seq(2004.50, 2024.50, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none", 
          strip.text = element_text(size = stripsize, face = "bold"))+
    labs(caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
  if (yearly) {
    # Calculate yearly average
    yearly_avg <- data %>%
      filter(name %in% names) %>%
      group_by(year, name) %>%
      summarize(value = mean(value, na.rm = TRUE), n = sum(!is.na(value)), .groups = "drop") %>%
          # Add yearly average segments
      mutate(start_date = year,
             end_date = paste0(year, ".75")) %>%
      ungroup()

    # If the latest year has fewer than 4 quarters of data, replace its yearly average
    # with a 4-quarter rolling mean (trailing) so it is comparable to full years.
    latest_year <- max(yearly_avg$year, na.rm = TRUE)
    # compute rolling 4-quarter mean per series
    rolling_4 <- data %>%
      filter(name %in% names) %>%
      arrange(name, year, quarter) %>%
      group_by(name) %>%
      mutate(roll4 = zoo::rollapply(value, width = 4, FUN = function(x) mean(x, na.rm = TRUE), align = "right", fill = NA, partial = FALSE)) %>%
      ungroup()

    rolling_last <- rolling_4 %>%
      group_by(name) %>%
      summarize(roll4_last = dplyr::last(roll4[!is.na(roll4)], default = NA_real_), .groups = "drop")

    yearly_avg <- yearly_avg %>%
      left_join(rolling_last, by = "name") %>%
      mutate(value = if_else(year == latest_year & n < 4 & !is.na(roll4_last), roll4_last, value)) %>%
      select(-n, -roll4_last)

    # Add yearly average line
     p <- p + geom_segment(data = yearly_avg, aes(x = start_date, xend = end_date, y = value, yend = value), color= "blue", size = 1)
     p <- p + labs(caption = "Datos INDEC. Datos del trimeste correspondiente a cada año señalados con un círculo.\nPromedios anuales en azul. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
    #p <- p + geom_segment(data = yearly_avg, aes(x = fecha, y = value), color="blue", size = 1)
  }
  else {
     p <- p + labs(caption = "Datos INDEC. Datos del trimeste correspondiente a cada año señalados con un círculo.\nAnálisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
  }
  p
  ggsave(file.path("plot", paste0(filename, ".png")), plot = p, dpi = 300)
}

# Let's write code to identify which sectors are above 2023 levels, which are similar, and which are below
# for Q2 of the reference year (2023) vs the latest year available (dynamic). Pivot duplicates
# are summarised with mean to avoid list-cols.

ref_year <- 2023L
other_year <- max(data_long2$year, na.rm = TRUE)
# tolerance for considering values 'similar' (relative difference)
tolerance <- 0.05 # 5% relative tolerance

# Diagnostics: check for duplicates
dup_check <- data_long2 %>%
  filter(quarter == "Q2", year %in% c(ref_year, other_year)) %>%
  dplyr::count(year, quarter, name) %>%
  dplyr::filter(n > 1)
if (nrow(dup_check) > 0) {
  message("Warning: duplicates found for Q2 (year,name) — pivot will aggregate with mean. Example rows:")
  print(head(dup_check))
}

data_comparison <- data_long2 %>%
  filter(quarter == "Q2", year %in% c(ref_year, other_year)) %>%
  select(year, quarter, name, value) %>%
  pivot_wider(names_from = year,
              values_from = value,
              names_prefix = "year_",
              values_fn = mean,
              values_fill = list(value = NA_real_)) %>%
  # safe ratio: NA if denominator missing or zero
  mutate(
    !!paste0("ratio_", other_year, "_", ref_year) := ifelse(is.na(.data[[paste0("year_", ref_year)]]) | .data[[paste0("year_", ref_year)]] == 0,
                                                                NA_real_,
                                                                .data[[paste0("year_", other_year)]] / .data[[paste0("year_", ref_year)]]),
    comparison = case_when(
      is.na(.data[[paste0("year_", other_year)]]) | is.na(.data[[paste0("year_", ref_year)]]) ~ "missing",
      .data[[paste0("year_", ref_year)]] == 0 ~ "missing",
      abs(.data[[paste0("year_", other_year)]] - .data[[paste0("year_", ref_year)]]) / .data[[paste0("year_", ref_year)]] <= tolerance ~ "similar",
      .data[[paste0("year_", other_year)]] > .data[[paste0("year_", ref_year)]] ~ "above",
      TRUE ~ "below"
    )
  )

#lets build list of sectores that "crecen", "iguales" y "bajan"
sectores_suben <- data_comparison %>%
  filter(comparison == "above") %>%
  filter(!grepl("Valor agregado bruto|\\(no alimentos\\)|Petróleo|Resto", name)) %>%
  #get top 4 by ratio and year_2025 > 20000
  filter(!!sym(paste0("ratio_", other_year, "_", ref_year)) > 1 & .data[[paste0("year_", other_year)]] > 20000) %>%
  top_n(4, !!sym(paste0("ratio_", other_year, "_", ref_year))) %>%
  pull(name)

sectores_grandes <- data_comparison %>%
# Filter out anything containing "Valor agregado bruto" or "(no alimentos)"
  filter(!grepl("Valor agregado bruto|\\(no alimentos\\)|Petróleo|Resto", name)) %>%
  # Get top 4 by year_2025
  top_n(4, !!sym(paste0("year_", other_year))) %>%
  pull(name)

ratio_col <- paste0("ratio_", other_year, "_", ref_year)
sectores_iguales <- data_comparison %>%
  filter(!grepl("Valor agregado bruto|\\(no alimentos\\)|Petróleo|Resto", name)) %>%
  filter(.data[[ratio_col]] >= 1 - tolerance & .data[[ratio_col]] <= 1 + tolerance) %>%
  # Get top 4 by year_2025
  top_n(4, !!sym(paste0("year_", other_year))) %>%
  pull(name)

sectores_bajan1 <- data_comparison %>%
  filter(comparison == "below") %>%
  filter(!grepl("Valor agregado bruto|\\(no alimentos\\)|Petróleo|Resto", name)) %>%
  # Get bottom 4 by ratio; ratio < 1 for 'below' and optionally filter by magnitude
  filter(.data[[paste0("ratio_", other_year, "_", ref_year)]] < 1 & .data[[paste0("year_", other_year)]] > 15000) %>%
  top_n(-4, .data[[paste0("ratio_", other_year, "_", ref_year)]]) %>%
  pull(name)

sectores_bajan2 <- data_comparison %>%
  filter(comparison == "below") %>%
  filter(!grepl("Valor agregado bruto|\\(no alimentos\\)|Petróleo|Resto", name)) %>%
  # Get bottom 4 by ratio; ratio < 1 for 'below' and optionally filter by magnitude
  filter(.data[[paste0("ratio_", other_year, "_", ref_year)]] < 1 & .data[[paste0("year_", other_year)]] <= 15000 & .data[[paste0("year_", other_year)]] > 5000) %>%
  top_n(-4, .data[[paste0("ratio_", other_year, "_", ref_year)]]) %>%
  pull(name)

plot_evolutionall(data_long2, names = c("Valor agregado bruto a precios básicos"), "Total",yearly=TRUE)
plot_evolutionall(data_long2, names = sectores_grandes, "grandes",yearly=TRUE)
plot_evolutionall(data_long2, names = sectores_suben, "suben",yearly=TRUE)
plot_evolutionall(data_long2, names = sectores_iguales, "iguales",yearly=TRUE)
plot_evolutionall(data_long2, names = sectores_bajan1, "bajan1",yearly=TRUE)
plot_evolutionall(data_long2, names = sectores_bajan2, "bajan2",yearly=TRUE)


#Ahora analizamos EMAE
data3 <- read_excel("sh_emae_actividad_base2004.xls", sheet = 1, skip = 2, col_names = FALSE)
data3 <- data3[-c(2),]

#Make first row the header
colnames(data3) <- data3[1,]
data3 <- data3[-1,]
colnames(data3)[2] <- "Mes"

#For column "Período", replace NA with the last non-NA value
data3$Período <- na.locf(data3$Período, na.rm = FALSE)

#pivot to long format
data3_long <- data3 %>%
  pivot_longer(cols = -c(Período, Mes), names_to = "column", values_to = "value") %>%
  mutate(year = as.numeric(substr(Período, 1, 4)),
         mes = as.factor(Mes),
         name = column) %>%
  select(year, mes, name, value)

#Remove rows with NA for value
data3_long <- data3_long %>%
  filter(!is.na(value))

#Convert month to a numeric value
# Function to convert month names in Spanish to numbers
month_2_num <- function(mes) {
  case_when(
    mes == "Enero" ~ 1,
    mes == "Febrero" ~ 2,
    mes == "Marzo" ~ 3,
    mes == "Abril" ~ 4,
    mes == "Mayo" ~ 5,
    mes == "Junio" ~ 6,
    mes == "Julio" ~ 7,
    mes == "Agosto" ~ 8,
    mes == "Septiembre" ~ 9,
    mes == "Octubre" ~ 10,
    mes == "Noviembre" ~ 11,
    mes == "Diciembre" ~ 12,
    TRUE ~ NA_real_
  )
}

# Function to convert numbers to Spanish month names
num_2_month <- function(number) {
  case_when(
    number == 1 ~ "Enero",
    number == 2 ~ "Febrero",
    number == 3 ~ "Marzo",
    number == 4 ~ "Abril",
    number == 5 ~ "Mayo",
    number == 6 ~ "Junio",
    number == 7 ~ "Julio",
    number == 8 ~ "Agosto",
    number == 9 ~ "Septiembre",
    number == 10 ~ "Octubre",
    number == 11 ~ "Noviembre",
    number == 12 ~ "Diciembre",
    TRUE ~ NA_character_
  )
}

# Then replace the existing case_when with:
data3_long <- data3_long %>%
  mutate(month = month_2_num(mes))

data3_long <- data3_long %>%
    mutate(fecha = as.Date(paste0(year, "-", month, "-01")),
           value = as.numeric(value))

deseasonalize_ts <- function(data, frequency=12) {
  # Convert to time series object
  ts_data <- ts(data$value, frequency=frequency)
  # Decompose
  decomp <- decompose(ts_data, type="multiplicative")
  # Return seasonally adjusted data
  return(decomp$x / decomp$seasonal)
}

plot_evolutionallemae <- function(data, names, filename, desde=as.Date("2004-01-01"), 
                                hasta=as.Date("2025-03-31"), deseasonalize=FALSE) {
  max_mes = data3_long %>% 
    filter(year == max(year)) %>% 
    filter(month == max(month)) %>% 
    select(month) %>% 
    unique() %>% 
    pull(month)
    
  if(deseasonalize) {
    # Deseasonalize each series
    adjusted_data <- data %>%
      filter(name %in% names) %>%
      group_by(name) %>%
      group_modify(~{
        adj_values <- deseasonalize_ts(.)
        mutate(., value = adj_values)
      }) %>%
      ungroup()
  } else {
    adjusted_data <- data %>%
      filter(name %in% names)
  }
  
  # Rest of plotting code using adjusted_data
  plott<-adjusted_data %>%
    ggplot(aes(x = fecha, y = value, color = name)) +
    geom_line(size=0.8) +
    ylab("EMAE base 100 = 2023") +
    xlab("Año") +
    ggtitle(paste0("Estimador Mensual de Actividad Económica")) +
    facet_wrap(~name, labeller=ggplot2::label_wrap_gen(width=30), scales = "free_y", ncol = 2) +
    theme_light() +
    # Etiquetar cada tercer trimestre con un punto
    geom_point(data = adjusted_data %>%  filter(name %in% names) %>% filter(month == max_mes), aes(x = fecha, y = value,fill=name),color="black",pch=21, size = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6), labels = comma, minor_breaks = NULL) +
    #coord_cartesian(ylim = c(0, NA),xlim = c(desde, hasta)) +
    scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%Y", 
                 limits = as.Date(c(desde, hasta))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = 8, face = "bold"))+
    labs(subtitle= paste0("Base promedio 2023=100 por actividad económica",
                         if(deseasonalize) "\n(Series desestacionalizadas)" else ""))
  plott <- plott + labs(caption = paste0("Datos INDEC. Cada mes de ", 
                                        num_2_month(max_mes), 
                                        " indicado como un punto.\nAnálisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina"))  
  ggsave(file.path("plot", paste0(filename, if(deseasonalize) "_desest" else "", ".png")), dpi = 300)
}

last_m <- data3_long %>%
  filter(year == max(year)) %>%
  filter(month == max(month)) %>%
  select(fecha) %>%
  unique() %>%
  pull(fecha)

#Rebase EMAE values to average 2023 = 100
data3_long <- data3_long %>%
  group_by(name) %>%
  mutate(avg_2023 = mean(value[year == 2023], na.rm = TRUE),
         value = (value / avg_2023) * 100) %>%
  ungroup() %>%
  select(-avg_2023)

#PANORAMA GENERAL
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones", "J - Intermediación financiera"), "general_emae",desde = as.Date("2015-01-01"),hasta=last_m)
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones","J - Intermediación financiera"), "2015-2025_general_emae",desde = as.Date("2015-01-01"),hasta=last_m)
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones","J - Intermediación financiera"), "2015-2025_general_emae",desde = as.Date("2015-01-01"),deseasonalize = TRUE,hasta=last_m)
#GANAN "H - Hoteles y restaurantes", "C - Explotación de minas y canteras", "Impuestos netos de subsidios", "J - Intermediación financiera"
plot_evolutionallemae(data3_long, c("H - Hoteles y restaurantes", "C - Explotación de minas y canteras", "Impuestos netos de subsidios", "J - Intermediación financiera"), "suben_emae",hasta=last_m)
plot_evolutionallemae(data3_long, c("H - Hoteles y restaurantes", "C - Explotación de minas y canteras", "Impuestos netos de subsidios", "J - Intermediación financiera"), "2015-2025_suben_emae",desde = as.Date("2015-01-01"),hasta=last_m)
plot_evolutionallemae(data3_long, c("H - Hoteles y restaurantes", "C - Explotación de minas y canteras", "Impuestos netos de subsidios", "J - Intermediación financiera"), "2015-2025_suben_emae",desde = as.Date("2015-01-01"),deseasonalize = TRUE,hasta=last_m)
#PIERDEN "D - Industria manufacturera", "F - Construcción", "H - Hoteles y restaurantes", "O - Otras actividades de servicios comunitarios, sociales y personales"
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones", "O - Otras actividades de servicios comunitarios, sociales y personales"), "bajan_emae",hasta=last_m)
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones", "O - Otras actividades de servicios comunitarios, sociales y personales"), "2015-2025_bajan_emae",desde = as.Date("2015-01-01"),hasta=last_m)
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones", "O - Otras actividades de servicios comunitarios, sociales y personales"), "2015-2025_bajan_emae",desde = as.Date("2015-01-01"),deseasonalize = TRUE,hasta=last_m)
