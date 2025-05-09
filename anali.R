# Cargar las librerías necesarias
library(tidyverse)
library(scales)
library(ggplot2)
library(zoo)
#Cargar la libreria de read_excel
library(readxl)

# Leer el archivo CSV
data <- read_csv("sh_VBP_VAB_12_24.csv",col_names = FALSE, skip=2)

#Leer la pestaña 4, "Cuadro 3" del xls
data2 <- read_excel("sh_VBP_VAB_03_25.xls", sheet = 4, skip = 5, col_names = FALSE)

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
years <- c("",rep(2004:2024, each = 4))
#quarters<-c("",rep(c("Q1","Q2","Q3","Q4"),times=20),c("Q1","Q2","Q3"))
quarters<-c("",rep(c("Q1","Q2","Q3","Q4"),times=21))
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
    geom_line(size=1.2) +
    ylab("Valor Agregado Bruto a Precios Básicos") +
    xlab("Año") +
    ggtitle(paste0("VAB a Precios Básicos por sector económico")) +
    facet_wrap(~name, labeller=ggplot2::label_wrap_gen(width=30), scales = "free_y", ncol = 2) +
    theme_light() +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(expand = c(0, 0.5), minor_breaks = NULL,breaks=seq(2004,2024,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = stripsize, face = "bold"))+
    labs(subtitle= paste0("Para el trimestre ",quarters," de cada año"), caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
    ggsave(paste0(filename,"_",quarters,".png"), dpi = 300)
}

# Crear los gráficos
for (quarter in c("Q4")) {
  plot_evolution(data_long, c("Valor agregado bruto a precios básicos"), "Total", quarter)
  plot_evolution(data_long, c("Agricultura, ganadería, caza y silvicultura", "Elaboración de productos alimenticios y bebidas", "Industria manufacturera (no alimentos)", "Comercio mayorista, minorista y reparaciones"), "grandes", quarter)
  plot_evolution(data_long, c("Cultivos agrícolas", "Pesca", "Restaurantes, bares y cantinas", "Explotación de minas y canteras"), "suben", quarter)
  plot_evolution(data_long, c("Electricidad, gas y agua", "Transporte, almacenamiento y comunicaciones", "Elaboración de productos alimenticios y bebidas", "Actividades inmobiliarias, empresariales y de alquiler"), "iguales", quarter)
  plot_evolution(data_long, c("Fabricación de productos minerales no metálicos", "Fabricación de metales comunes", "Fabricación de maquinaria y equipo n.c.p.", "Fabricación de vehículos"), "bajan1", quarter)
  plot_evolution(data_long, c("Fabricación de sustancias y productos químicos", "Construcción", "Comercio mayorista, minorista y reparaciones", "Fabricación de productos de caucho y plástico"), "bajan2", quarter)
}


data_long2 <- data_long %>%
    mutate(fecha = as.yearqtr(paste0(year, quarter)))

plot_evolutionall <- function(data, names, filename, yearly = FALSE) {
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
    geom_point(data = data %>%  filter(name %in% names) %>% filter(quarter == "Q4"), aes(x = fecha, y = value,fill=name),color="black",pch=21, size = 2) +
    scale_y_continuous(labels = comma) +
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
      summarize(value = mean(value, na.rm = TRUE)) %>%
          # Add yearly average segments
      mutate(start_date = year,
             end_date = paste0(year,".75")) %>%
      ungroup()

    # Add yearly average line
     p <- p + geom_segment(data = yearly_avg, aes(x = start_date, xend = end_date, y = value, yend = value), color= "blue", size = 1)
     p <- p + labs(caption = "Datos INDEC. Datos del trimeste correspondiente a cada año señalados con un círculo.\nPromedios anuales en azul. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
    #p <- p + geom_segment(data = yearly_avg, aes(x = fecha, y = value), color="blue", size = 1)
  }
  else {
     p <- p + labs(caption = "Datos INDEC. Datos del trimeste correspondiente a cada año señalados con un círculo.\nAnálisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
  }
  p
  ggsave(paste0(filename, ".png"), plot = p, dpi = 300)
}

#Find 

plot_evolutionall(data_long2, c("Valor agregado bruto a precios básicos"), "Total",yearly=TRUE)
plot_evolutionall(data_long2, c("Agricultura, ganadería, caza y silvicultura", "Elaboración de productos alimenticios y bebidas", "Industria manufacturera (no alimentos)", "Comercio mayorista, minorista y reparaciones"), "grandes",yearly=TRUE)
plot_evolutionall(data_long2, c("Cultivos agrícolas", "Elaboración de productos alimenticios y bebidas", "Restaurantes, bares y cantinas", "Cría de animales"), "suben",yearly=TRUE)
plot_evolutionall(data_long2, c("Electricidad, gas y agua", "Transporte, almacenamiento y comunicaciones", "Intermediación financiera", "Actividades inmobiliarias, empresariales y de alquiler"), "iguales",yearly=TRUE)
plot_evolutionall(data_long2, c("Fabricación de productos minerales no metálicos", "Fabricación de metales comunes", "Fabricación de maquinaria y equipo n.c.p.", "Fabricación de vehículos"), "bajan1",yearly=TRUE)
plot_evolutionall(data_long2, c("Fabricación de sustancias y productos químicos", "Construcción", "Comercio mayorista, minorista y reparaciones", "Minerales metalíferos"), "bajan2",yearly=TRUE)


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
  adjusted_data %>%
    ggplot(aes(x = fecha, y = value, color = name)) +
    geom_line(size=0.8) +
    ylab("Valor Agregado Bruto a Precios Básicos") +
    xlab("Año") +
    ggtitle(paste0("Estimador Mensual de Actividad Económica")) +
    facet_wrap(~name, labeller=ggplot2::label_wrap_gen(width=30), scales = "free_y", ncol = 2) +
    theme_light() +
    # Etiquetar cada tercer trimestre con un punto
    geom_point(data = adjusted_data %>%  filter(name %in% names) %>% filter(month == max_mes), aes(x = fecha, y = value,fill=name),color="black",pch=21, size = 2) +
    scale_y_continuous(labels = comma) +
    #coord_cartesian(ylim = c(0, NA),xlim = c(desde, hasta)) +
    scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%Y", 
                 limits = as.Date(c(desde, hasta))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = 8, face = "bold"))+
    labs(subtitle= paste0("Base promedio 2004=100 por sector económico para cada mes 2004-2025",
                         if(deseasonalize) "\n(Series desestacionalizadas)" else ""))
    
  ggsave(paste0(filename, if(deseasonalize) "_desest" else "", ".png"), dpi = 300)
}

#GRANDES
plot_evolutionallemae(data3_long, c("A -  Agricultura, ganadería, caza y silvicultura", "D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones"), "grandes_emae")
plot_evolutionallemae(data3_long, c("A -  Agricultura, ganadería, caza y silvicultura", "D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones"), "2015-2025_grandes_emae",desde = as.Date("2015-01-01"))
plot_evolutionallemae(data3_long, c("A -  Agricultura, ganadería, caza y silvicultura", "D - Industria manufacturera", "F - Construcción", "G - Comercio mayorista, minorista y reparaciones"), "2015-2025_grandes_emae",desde = as.Date("2015-01-01"),deseasonalize = TRUE)
#CRECEN B - Pesca , C - Explotación de minas y canteras, E - Electricidad, gas y agua, J - Intermediación financiera
plot_evolutionallemae(data3_long, c("B - Pesca", "C - Explotación de minas y canteras", "E - Electricidad, gas y agua", "J - Intermediación financiera"), "suben_emae")
plot_evolutionallemae(data3_long, c("B - Pesca", "C - Explotación de minas y canteras", "E - Electricidad, gas y agua", "J - Intermediación financiera"), "2015-2025_suben_emae",desde = as.Date("2015-01-01"))
plot_evolutionallemae(data3_long, c("B - Pesca", "C - Explotación de minas y canteras", "E - Electricidad, gas y agua", "J - Intermediación financiera"), "2015-2025_suben_emae",desde = as.Date("2015-01-01"),deseasonalize = TRUE)
#PIERDEN  D - Industria manufacturera, F - Construcción, H - Hoteles y restaurantes, O - Otras actividades de servicios comunitarios, sociales y personales
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "H - Hoteles y restaurantes", "O - Otras actividades de servicios comunitarios, sociales y personales"), "bajan_emae")
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "H - Hoteles y restaurantes", "O - Otras actividades de servicios comunitarios, sociales y personales"), "2015-2025_bajan_emae",desde = as.Date("2015-01-01"))
plot_evolutionallemae(data3_long, c("D - Industria manufacturera", "F - Construcción", "H - Hoteles y restaurantes", "O - Otras actividades de servicios comunitarios, sociales y personales"), "2015-2025_bajan_emae",desde = as.Date("2015-01-01"),deseasonalize = TRUE)



, 
# Definir la función de trazado
plot_evolutionemae <- function(data, names, filename) {
  max_mes = data3_long %>% 
    filter(year == max(year)) %>% 
    filter(month == max(month)) %>% 
    select(month) %>% 
    unique() %>% 
    pull(month)
  
  filtered_data <- data %>%
    filter(name %in% names) %>%
    filter(month == max_mes)
  
  filtered_data %>%
    ggplot(aes(x = fecha, y = value, color = name)) +
    geom_line(size = 0.8) +
    geom_point(aes(fill = name), pch = 21, color = "black", size = 2) +
    ylab("Valor Agregado Bruto a Precios Básicos") +
    xlab("Año") +
    ggtitle(paste0("Estimador Mensual de Actividad Económica")) +
    facet_wrap(~name, labeller = ggplot2::label_wrap_gen(width = 30), 
               scales = "free_y", ncol = 2) +
    theme_light() +
    scale_y_continuous(labels = comma) +
    scale_x_date(minor_breaks = NULL,date_breaks = "1 year", date_labels = "%Y",
                 limits = as.Date(c("2004-01-01", "2025-03-31"))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = 8, face = "bold")) +
    labs(subtitle = paste0("Base 2004=100 por sector económico para cada mes de ", 
                          num_2_month(max_mes)),
         caption = paste0("Datos INDEC. Datos de cada mes de ", 
                         num_2_month(max_mes), 
                         " indicado como un punto.\nAnálisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina"))
  ggsave(paste0(filename, ".png"), dpi = 300)
}

  plot_evolutionemae(data3_long, c("C - Explotación de minas y canteras", "D - Industria manufacturera", "F - Construcción", "J - Intermediación financiera"), "Feb_grandes_emae")