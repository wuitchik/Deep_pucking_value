# Load libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(plotly)
library(purrr)

# Load the data
df <- read_csv("../Data/skaters.csv") %>%
  filter(situation == "5on5")

df_fantasy <- read_csv("../Data/Fantrax-Players-Do That Hockey (2).csv") %>%
  mutate(name = Player)

df_merged <- df %>%
  inner_join(df_fantasy, by = "name")

# Keep identifying info for later
meta <- df_merged %>% select(name, position, FPts)

# Optional: View available columns
glimpse(df)

# Select only numeric columns (you can fine-tune this as needed)
df_numeric <- df_merged %>%
  select(where(is.numeric)) %>%
  drop_na() 

# Standardize and run PCA
pca_res <- PCA(df_numeric, scale.unit = TRUE, graph = FALSE)
pca_coords <- as.data.frame(pca_res$ind$coord) %>%
  bind_cols(meta)

# Get percent variance explained
eig_vals <- pca_res$eig
pc1_var <- round(eig_vals[1, 2], 1)  # % of variance explained by PC1
pc2_var <- round(eig_vals[2, 2], 1)  # % of variance explained by PC2

# Extract variable coordinates (loadings)
var_coords <- as.data.frame(pca_res$var$coord)

# Get top 10 loadings
var_coords <- as.data.frame(pca_res$var$coord)
var_coords$impact <- sqrt(var_coords$Dim.1^2 + var_coords$Dim.2^2)
top_vars <- var_coords %>%
  arrange(desc(impact)) %>%
  slice(1:10) %>%
  rownames_to_column("variable")

# Compute vector magnitude (impact across PC1 and PC2)
var_coords$impact <- sqrt(var_coords$Dim.1^2 + var_coords$Dim.2^2)

# Only keep the needed columns for plotting arrows
arrow_df <- top_vars %>%
  select(variable, Dim.1, Dim.2, impact)


# Scale factor for arrow length (tweak if arrows are too long/short)
arrow_scale <- 3

# Create arrow shapes using pmap
loading_shapes <- pmap(arrow_df, function(variable, Dim.1, Dim.2, impact) {
  list(
    type = "line",
    x0 = 0,
    y0 = 0,
    x1 = Dim.1 * arrow_scale,
    y1 = Dim.2 * arrow_scale,
    line = list(color = "black", width = 1)
  )
})


# Build shapes (lines as arrows)
loading_shapes <- pmap(top_vars, function(variable, Dim.1, Dim.2, impact) {
  list(
    type = "line",
    x0 = 0,
    y0 = 0,
    x1 = Dim.1 * arrow_scale,
    y1 = Dim.2 * arrow_scale,
    line = list(color = "black", width = 1)
  )
})

# Extract PCA coordinates and join metadata
pca_coords <- as.data.frame(pca_res$ind$coord) %>%
  bind_cols(meta)


# Create line segments for each variable
loading_lines <- map2(top_vars$Dim.1, top_vars$Dim.2, ~{
  list(
    type = "line",
    x0 = 0, y0 = 0,
    x1 = .x * arrow_scale, y1 = .y * arrow_scale,
    line = list(color = "black", width = 1)
  )
})

# Add variable labels
loading_labels <- data.frame(
  x = top_vars$Dim.1 * arrow_scale * 1.1,
  y = top_vars$Dim.2 * arrow_scale * 1.1,
  text = top_vars$variable
)


# Make interactive plot
fig <- plot_ly(data = pca_coords,
               x = ~Dim.1,
               y = ~Dim.2,
               type = 'scatter',
               mode = 'markers',
               text = ~paste0(name, "<br>FPts: ", FPts),
               hoverinfo = 'text',
               marker = list(size = 8,
                             color = ~FPts,
                             colorscale = 'RdYlBu',
                             showscale = TRUE,
                             colorbar = list(title = "Fantasy Points")))

# Add layout with variance
fig <- layout(fig,
              title = "Interactive PCA of NHL Skaters (5v5)",
              xaxis = list(title = paste0("PC1 (", pc1_var, "%)")),
              yaxis = list(title = paste0("PC2 (", pc2_var, "%)")))


# Add labels and arrows to fig
fig <- fig %>%
  add_trace(data = loading_labels,
            x = ~x, y = ~y,
            text = ~text,
            type = "scatter",
            mode = "text",
            textposition = "top right",
            showlegend = FALSE) %>%
  layout(shapes = loading_lines)

fig
