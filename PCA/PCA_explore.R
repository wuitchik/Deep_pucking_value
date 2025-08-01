# Load libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(plotly)

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

# Visualize individuals
fviz_pca_ind(pca_res,
             geom.ind = "point",
             col.ind = pca_coords$FPts,      # Numeric vector for gradient
             gradient.cols = c("blue", "white", "red"),  # Continuous gradient
             repel = TRUE) +
  labs(title = "PCA of NHL Skaters (5v5)", color = "Fantasy Points")




# Visualize variables (loadings)
fviz_pca_var(pca_res,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


# Extract PCA coordinates and join metadata
pca_coords <- as.data.frame(pca_res$ind$coord) %>%
  bind_cols(meta)

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
