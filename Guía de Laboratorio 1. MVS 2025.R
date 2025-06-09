####################################################################################
# TÍTULO: Análisis de Diversidad Alfa y Estructura de Comunidades de Aves
# AUTOR: Nhering Ortiz Lobo
# FECHA: 3 de junio de 2025
# DESCRIPCIÓN: Este script realiza el cálculo de índices de diversidad alfa (Shannon, Simpson, riqueza), genera curvas de acumulación de especies, y ejecuta análisis multivariado (NMDS, PCA) usando datos de abundancia de aves por punto de muestreo (GrillaID), con visualización gráfica.
####################################################################################

# ================================
# 1. CARGA DE PAQUETES NECESARIOS
# ================================
install.packages(c("tidyverse", "vegan", "readxl")) # Ejecutar solo una vez si no están instalados
library(tidyverse)   # Manipulación y gráficos
library(vegan)       # Ecología numérica
library(readxl)      # Lectura de archivos Excel

# ================================
# 2. IMPORTACIÓN DE DATOS
# ================================
data <- read.delim("Data2.txt", sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")

# Matriz de abundancia: GrillaID (filas) x Especies (columnas)
abund_matrix <- data %>%
  group_by(GrillaID, Especie) %>%
  summarise(Abundancia = sum(Abundancia), .groups = "drop") %>%
  pivot_wider(names_from = Especie, values_from = Abundancia, values_fill = 0) %>%
  column_to_rownames(var = "GrillaID")

# ================================
# 3. CÁLCULO DE ÍNDICES DE DIVERSIDAD
# ================================
shannon_index <- diversity(abund_matrix, index = "shannon")
simpson_index <- diversity(abund_matrix, index = "simpson")
richness <- specnumber(abund_matrix)

diversidad <- data.frame(
  GrillaID = rownames(abund_matrix),
  Shannon_H = round(shannon_index, 2),
  Simpson_D = round(simpson_index, 2),
  Riqueza = richness
)

print("\nÍNDICES DE DIVERSIDAD POR GRILLA")
print(diversidad)

# ================================
# 4. GRÁFICO DE DIVERSIDAD (mejorado)
# ================================
diversidad_long <- pivot_longer(diversidad, cols = -GrillaID, names_to = "Indice", values_to = "Valor")

ggplot(diversidad_long, aes(x = GrillaID, y = Valor, fill = Indice)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = Valor), vjust = -0.2, position = position_dodge(0.8), size = 3) +
  scale_fill_manual(values = c("Riqueza" = "#458B74", "Shannon_H" = "#66CDAA", "Simpson_D" = "#76EEC6")) +
  labs(title = "Índices de Diversidad Alfa por Grillas", y = "Valor del índice", x = "GrillaID") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ===============================================
# 5. CURVA DE ACUMULACIÓN DE ESPECIES
# ===============================================

# ----- PENDIENTE -------------------------------

# ========================================================
# 6. ANÁLISIS MULTIVARIADO DE LA COMUNIDAD
# ========================================================
# Librerías necesarias
library(tidyverse)   # Manipulación y gráficos
library(vegan)       # Ecología numérica
library(readxl)      # Lectura de archivos Excel

data <- read.delim("Data2.txt", sep = "\t", stringsAsFactors = FALSE, encoding = "UTF-8")

# Matriz de abundancia: GrillaID (filas) x Especies (columnas)
abund_matrix <- data %>%
  group_by(GrillaID, Especie) %>%
  summarise(Abundancia = sum(Abundancia), .groups = "drop") %>%
  pivot_wider(names_from = Especie, values_from = Abundancia, values_fill = 0) %>%
  column_to_rownames(var = "GrillaID")

# Crear tabla de relación GrillaID - Habitat
grupo_por_grilla <- data %>%
  select(GrillaID, Habitat) %>%
  distinct() %>%
  column_to_rownames(var = "GrillaID")

grupos <- grupo_por_grilla[rownames(abund_matrix), "Habitat"]


# A) Matrices de disimilitud
bray_curtis_dist <- vegdist(abund_matrix, method = "bray")

# B) NMDS
nmds <- metaMDS(abund_matrix, distance = "bray", k = 2, trymax = 100)

# C) Gráfico NMDS
plot(nmds, type = "n", main = "NMDS (Bray-Curtis)", xlab = "NMDS1", ylab = "NMDS2")

# D) Puntos por sitio
points(nmds, display = "sites", col = as.factor(grupos), pch = 19, cex = 1.2)

# E) Etiquetas de sitios
text(nmds, display = "sites", labels = rownames(abund_matrix), pos = 3, cex = 0.8)

# F) Elipses de agrupación
ordiellipse(nmds, groups = grupos, display = "sites",
            kind = "sd", draw = "polygon", alpha = 80,
            col = 1:length(unique(grupos)), label = TRUE)

# Después de plot(), points(), text() y ordiellipse()
mtext("Nhering Ortiz Lobo", side = 1, line = 4, adj = 1, cex = 0.9, col = "darkblue", font = 3)



##########SegundoProceso########################################################
################################################################################


# C) PCA
pca <- rda(abund_matrix)
biplot(pca, scaling = 2, main = "PCA de la Comunidad de Aves")

library(ggplot2)
library(ggrepel)
library(vegan)

# PCA (ya calculado)
pca <- rda(abund_matrix)
sitios_pca <- scores(pca, display = "sites", scaling = 2)
especies_pca <- scores(pca, display = "species", scaling = 2)

df_sitios <- as.data.frame(sitios_pca)
df_sitios$GrillaID <- rownames(df_sitios)
df_sitios$Grupo <- grupo_por_grilla[rownames(df_sitios), "Habitat"]

df_especies <- as.data.frame(especies_pca)
df_especies$Especie <- rownames(df_especies)

# Paleta de colores suave
colores_personalizados <- RColorBrewer::brewer.pal(n = length(unique(df_sitios$Grupo)), "Set2")

# Gráfico PCA mejorado
ggplot(data = df_sitios, aes(x = PC1, y = PC2, color = Grupo)) +
  geom_point(size = 4, alpha = 0.9) +
  stat_ellipse(type = "t", linetype = "solid", linewidth = 1, alpha = 0.2) +
  geom_text_repel(aes(label = GrillaID), size = 3.8, fontface = "bold", max.overlaps = 100) +
  geom_segment(data = df_especies, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "darkred", linewidth = 0.7) +
  geom_text_repel(data = df_especies, aes(x = PC1, y = PC2, label = Especie),
                  color = "darkred", size = 3, max.overlaps = 100) +
  scale_color_manual(values = colores_personalizados) +
  labs(title = "PCA de la Comunidad de Aves (Agrupado por Hábitat)",
       x = "PC1", y = "PC2", color = "Grupo") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.grid = element_line(color = "grey90"))


# Gráfico PCA mejorado
ggplot(data = df_sitios, aes(x = PC1, y = PC2, color = Grupo)) +
  geom_point(size = 4, alpha = 0.9) +
  stat_ellipse(type = "t", linetype = "solid", linewidth = 1, alpha = 0.2) +
  geom_text_repel(aes(label = GrillaID), size = 3.5, fontface = "plain", max.overlaps = 300, force = 1) +
  geom_segment(data = df_especies, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "darkred", linewidth = 0.7) +
  geom_text_repel(data = df_especies, aes(x = PC1, y = PC2, label = Especie),
                  color = "darkred", size = 3, max.overlaps = 100) +
  annotate("text", x = Inf, y = -Inf, label = "Nhering Ortiz Lobo",
           hjust = 1.1, vjust = -1, size = 4, color = "darkblue", fontface = "italic") +
  scale_color_manual(values = colores_personalizados) +
  labs(title = "PCA de la Comunidad de Aves (Agrupado por Hábitat)",
       x = "PC1", y = "PC2", color = "Grupo") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        panel.grid = element_line(color = "grey90"))


# ================================
# 7. EXPORTACIÓN OPCIONAL
# ================================
write.csv(diversidad, "indices_diversidad_aves.csv", row.names = FALSE)
