# Load necessary libraries
library(tidyverse)
library(scales)
library(ggplot2)
# Read the CSV file
data <- read_csv("sh_VBP_VAB_12_24.csv",col_names = FALSE, skip=2)

# Identify columns to keep: column 1 and those not multiples of 6 or 6 + 1
columns_to_keep <- c(1, which(!(seq_along(data) %% 6 %in% c(0, 1))))

# Subset the data to keep only the identified columns
data <- data[, columns_to_keep]

#Remove rows 1 and 3
data <- data[-c(1,3),]
#Replace NA values with ""
data[is.na(data)] <- ""

#New dataframe combining data with years and quarters
# Replace row 2 with year row, starting at 2004, repeating 4 times each, except for 2024
years <- c("",rep(2004:2023, each = 4),2024,2024,2024)
quarters<-c("",rep(c("Q1","Q2","Q3","Q4"),times=20),c("Q1","Q2","Q3"))
data2 <- data.frame(rbind(years,quarters,data))

# Extract year and quarter information
years2 <- as.vector(data2[1, -1])
quarters2 <- as.vector(data2[2, -1])

# Extract name information
names <- as.vector(data2[-c(1, 2), 1])

# Pivot to long format
data_long <- data2[-c(1, 2), ] %>%
  pivot_longer(cols = -X1, names_to = "column", values_to = "value") %>%
  mutate(year = as.numeric(rep(years2, times = length(names))),
         quarter = rep(quarters2, times = length(names)),
         name = rep(names, each = length(years2))) %>%
  select(year, quarter, name, value)
data_long <- data_long %>%
  mutate(year = as.numeric(year),
         value = as.numeric(value))

#Create new name variable "Industria manufacturera sin alimentos", by substracting "Elaboración de productos alimenticios y bebidas" from "Industria manufacturera"
data_long <- data_long %>%
  group_by(year, quarter) %>%
  mutate(value = if_else(name == "Industria manufacturera", 
                         value - sum(value[name == "Elaboración de productos alimenticios y bebidas"], na.rm = TRUE), 
                         value),
         name = if_else(name == "Industria manufacturera", "Industria manufacturera sin alimentos", name)) %>%
  ungroup()

#Shorten some names using case_when
data_long <- data_long %>%
  mutate(name = case_when(
    name == "Fabricación de vehículos automotores, remolques y semirremolques" ~ "Fabricación de vehículos",
    name == "Extracción de minerales metalíferos. Explotación de minas y canteras n.c.p." ~ "Minerales metalíferos",
    TRUE ~ name))

# Define the plotting function
plot_evolution <- function(data, names, filename,quarters) {
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
          strip.text = element_text(size = 12, face = "bold"))
    ggsave(paste0(filename,"_",quarters,".png"), dpi = 300)
}

# Create the plots
for (quarter in c("Q2", "Q3")) {
  plot_evolution(data_long, c("Agricultura, ganadería, caza y silvicultura", "Elaboración de productos alimenticios y bebidas", "Industria manufacturera sin alimentos", "Comercio mayorista, minorista y reparaciones"), "grandes", quarter)
  
  # suben
  plot_evolution(data_long, c("Cultivos agrícolas", "Elaboración de productos alimenticios y bebidas", "Restaurantes, bares y cantinas", "Cría de animales"), "suben", quarter)
  
  # iguales
  plot_evolution(data_long, c("Electricidad, gas y agua", "Transporte, almacenamiento y comunicaciones", "Intermediación financiera", "Actividades inmobiliarias, empresariales y de alquiler"), "iguales", quarter)
  
  # bajan1
  plot_evolution(data_long, c("Fabricación de productos minerales no metálicos", "Fabricación de metales comunes", "Fabricación de maquinaria y equipo n.c.p.", "Fabricación de vehículos"), "bajan1", quarter)
  
  # bajan2
  plot_evolution(data_long, c("Fabricación de sustancias y productos químicos", "Construcción", "Comercio mayorista, minorista y reparaciones", "Minerales metalíferos"), "bajan2", quarter)
}

data_long2 <- data_long %>%
    mutate(fecha = as.yearqtr(paste0(year, quarter)))


plot_evolutionall <- function(data, names, filename) {
  data %>%
    filter(name %in% names) %>%
    ggplot(aes(x = fecha, y = value, color = name)) +
    geom_line(size=1.2) +
    ylab("Valor Agregado Bruto a Precios Básicos") +
    xlab("Año") +
    ggtitle(paste0("VAB a Precios Básicos por sector económico para cada trimestre 2004-2024")) +
    facet_wrap(~name, scales = "free_y", ncol = 2) +
    theme_light() +
    #Label each third quarter with a dot
    geom_point(data = data %>%  filter(name %in% names) %>% filter(quarter == "Q3"), aes(x = fecha, y = value,color=name), size = 2) +
    scale_y_continuous(labels = comma) +
    zoo::scale_x_yearqtr(format = '%Y-T%q',expand = c(0, 0.22), minor_breaks = NULL, breaks = seq(2004.50, 2024.50, 1)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "none",
          strip.text = element_text(size = 12, face = "bold"))
    ggsave(paste0(filename,".png"), dpi = 300)
}

plot_evolutionall(data_long2, c("Agricultura, ganadería, caza y silvicultura", "Elaboración de productos alimenticios y bebidas", "Industria manufacturera sin alimentos", "Comercio mayorista, minorista y reparaciones"), "grandes")
plot_evolutionall(data_long2, c("Cultivos agrícolas", "Elaboración de productos alimenticios y bebidas", "Restaurantes, bares y cantinas", "Cría de animales"), "suben")
plot_evolutionall(data_long2, c("Electricidad, gas y agua", "Transporte, almacenamiento y comunicaciones", "Intermediación financiera", "Actividades inmobiliarias, empresariales y de alquiler"), "iguales")
plot_evolutionall(data_long2, c("Fabricación de productos minerales no metálicos", "Fabricación de metales comunes", "Fabricación de maquinaria y equipo n.c.p.", "Fabricación de vehículos"), "bajan1")
plot_evolutionall(data_long2, c("Fabricación de sustancias y productos químicos", "Construcción", "Comercio mayorista, minorista y reparaciones", "Minerales metalíferos"), "bajan2")