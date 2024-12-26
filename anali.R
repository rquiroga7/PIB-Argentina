# Cargar las librerías necesarias
library(tidyverse)
library(scales)
library(ggplot2)
library(zoo)

# Leer el archivo CSV
data <- read_csv("sh_VBP_VAB_12_24.csv",col_names = FALSE, skip=2)

# Identificar las columnas a mantener: columna 1 y aquellas que no son múltiplos de 6 o 6 + 1
columns_to_keep <- c(1, which(!(seq_along(data) %% 6 %in% c(0, 1))))

# Subconjunto de datos para mantener solo las columnas identificadas
data <- data[, columns_to_keep]

# Eliminar las filas 1 y 3
data <- data[-c(1,3),]
# Reemplazar valores NA con ""
data[is.na(data)] <- ""

# Nuevo dataframe combinando datos con años y trimestres
# Reemplazar la fila 2 con la fila de años, comenzando en 2004, repitiendo 4 veces cada uno, excepto para 2024
years <- c("",rep(2004:2023, each = 4),2024,2024,2024)
quarters<-c("",rep(c("Q1","Q2","Q3","Q4"),times=20),c("Q1","Q2","Q3"))
data2 <- data.frame(rbind(years,quarters,data))

# Extraer información de años y trimestres
years2 <- as.vector(data2[1, -1])
quarters2 <- as.vector(data2[2, -1])

# Extraer información de nombres
names <- as.vector(data2[-c(1, 2), 1])

# Pivotar a formato largo
data_long <- data2[-c(1, 2), ] %>%
  pivot_longer(cols = -X1, names_to = "column", values_to = "value") %>%
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
    ggtitle(paste0("VAB a Precios Básicos por sector económico para el trimestre ",quarters," de cada año")) +
    facet_wrap(~name, scales = "free_y", ncol = 2) +
    theme_light() +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(expand = c(0, 0.5), minor_breaks = NULL,breaks=seq(2004,2024,1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = stripsize, face = "bold"))+
    labs(caption = "Datos INDEC. Análisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
    ggsave(paste0(filename,"_",quarters,".png"), dpi = 300)
}

# Crear los gráficos
for (quarter in c("Q3")) {
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
    ggtitle("VAB a Precios Básicos por trimestre 2004-2024") +
    facet_wrap(~name, scales = scalev, ncol = 2) +
    theme_light() +
    # Etiquetar cada tercer trimestre con un punto
    geom_point(data = data %>% filter(name %in% names) %>% filter(quarter == "Q3"), aes(x = fecha, y = value, color = name), size = 2) +
    scale_y_continuous(labels = comma) +
    zoo::scale_x_yearqtr(format = '%Y-T%q', expand = c(0, 0.22), minor_breaks = NULL, breaks = seq(2004.50, 2024.50, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none", 
          strip.text = element_text(size = stripsize, face = "bold"))
  if (yearly) {
    # Calculate yearly average
    yearly_avg <- data %>%
      filter(name %in% names) %>%
      group_by(year, name) %>%
      summarize(value = mean(value, na.rm = TRUE)) %>%
          # Add yearly average segments
      mutate(start_date = year,
             end_date = paste0(year,".99")) %>%
      ungroup()

    # Add yearly average line
     p <- p + geom_segment(data = yearly_avg, aes(x = start_date, xend = end_date, y = value, yend = value), color= "blue", size = 1)
     p <- p + labs(caption = "Datos INDEC. Datos del tercer trimeste de cada año señalados con un círculo. Promedios anuales en azul.\nAnálisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
    #p <- p + geom_segment(data = yearly_avg, aes(x = fecha, y = value), color="blue", size = 1)
  }
  else {
     p <- p + labs(caption = "Datos INDEC. Datos del tercer trimeste de cada año señalados con un círculo.\nAnálisis y visualización por Rodrigo Quiroga. Ver github.com/rquiroga7/PIB-Argentina")
  }
  p
  ggsave(paste0(filename, ".png"), plot = p, dpi = 300)
}

#Find 

plot_evolutionall(data_long2, c("Valor agregado bruto a precios básicos","Valor agregado bruto a precios básicos (sin campo)"), "sin_campo",yearly = TRUE)
plot_evolutionall(data_long2, c("Valor agregado bruto a precios básicos"), "Total",yearly = TRUE)
plot_evolutionall(data_long2, c("Agricultura, ganadería, caza y silvicultura", "Elaboración de productos alimenticios y bebidas", "Industria manufacturera (no alimentos)", "Comercio mayorista, minorista y reparaciones"), "grandes", yearly = TRUE)
plot_evolutionall(data_long2, c("Cultivos agrícolas", "Pesca", "Restaurantes, bares y cantinas", "Explotación de minas y canteras"), "suben", yearly = TRUE)
plot_evolutionall(data_long2, c("Electricidad, gas y agua", "Transporte, almacenamiento y comunicaciones", "Elaboración de productos alimenticios y bebidas", "Actividades inmobiliarias, empresariales y de alquiler"), "iguales", yearly = TRUE)
plot_evolutionall(data_long2, c("Fabricación de productos minerales no metálicos", "Fabricación de metales comunes", "Fabricación de maquinaria y equipo n.c.p.", "Fabricación de vehículos"), "bajan1", yearly = TRUE)
plot_evolutionall(data_long2, c("Fabricación de sustancias y productos químicos", "Construcción", "Comercio mayorista, minorista y reparaciones", "Fabricación de productos de caucho y plástico"), "bajan2", yearly = TRUE)