# ---------------------------------------------------------------
# Author: Purna Saud
# Date: November 2024
# Title: Analysis of Median Home Value in Wyoming
# Description: This code performs spatial analysis techniques 
# on ACS 2022 data for Wyoming. It includes Moran's I, Local Moran's I, 
# and regression models to explore spatial relationships in 
# median home value and related demographic/housing characteristics.
# ---------------------------------------------------------------

# ---------------------------------------------------------------
# I. Load Required Libraries
# ---------------------------------------------------------------
library(tidycensus)
library(mapview)
library(sf)
library(tidyverse)
library(tigris)
library(tmap)
library(mapboxapi)
library(leaflet)
library(spdep)
library(RColorBrewer)
library(spatialreg)
library(Boruta)
library(flextable)
require(stargazer)
require(DT)
require(plotly)
require(ggplot2)
require(leaflet)

# ---------------------------------------------------------------
# II. Set Up Census API Key
# ---------------------------------------------------------------
census_api_key('312248c846fdb30816100611e375688ee60be39f', install = TRUE, overwrite = TRUE)

# ---------------------------------------------------------------
# III. Define Census Variables
# ---------------------------------------------------------------
variables <- c(
  # Population & Demographics
  t_pop = "B01003_001",
  m_age = "B01002_001",
  p_coll_de = "B15003_022",  # College degree (25+)
  p_white = "B03002_003",
  
  # Housing Characteristics
  mhv = "B25077_001",
  m_rent = "B25064_001",
  p_owner = "B25008_003",
  h_age = "B25035_001",
  m_y_built = "B25037_001",
  m_rooms = "B25018_001",
  
  # Income & Employment
  m_h_income = "B19013_001",
  uemp_rate = "B23025_005",
  po_rate = "B17001_002",
  
  # Additional Variables
  m_h_year = "B25034_001",
  p_t_rate = "B25091_001",
  a_h_size = "B25010_001"
)
options(tigris_use_cache = TRUE)

# ---------------------------------------------------------------
# IV. Retrieve ACS Data and Preprocess
# ---------------------------------------------------------------
wyoming_data <- get_acs(
  geography = "tract",
  variables = variables,
  state = "WY",
  year = 2022,
  survey = "acs5",
  output = 'wide',
  geometry = TRUE
) %>%
  select(!ends_with("M")) %>%
  rename_with(.fn = ~str_remove(.x, "E$")) %>%
  st_transform(wyoming_data, crs = 3738)

# Replace N/A values with neighbors' mean (5 neighbors before & after)
for (col in 2:(ncol(wyoming_data)-1)) {  # Skip first and last columns
  for (i in 1:nrow(wyoming_data)) {
    
    # Check if the value at row i, column col is NA
    if (is.na(wyoming_data[i, col][[1]])) {  
      
      # Define the range for neighbors (before and after the current row)
      start_index <- max(1, i - 5)  
      end_index <- min(nrow(wyoming_data), i + 5)  
      
      # Extract neighbor values, including valid non-NA values
      neighbor_values <- wyoming_data[start_index:end_index, col][[1]]
      
      # Filter out NA values
      valid_neighbors <- neighbor_values[!is.na(neighbor_values)]
      
      # Replace NA with the mean of valid neighbors if available
      if (length(valid_neighbors) > 0) {
        wyoming_data[i, col] <- mean(valid_neighbors)
      }
    }
  }
}

# Create log-transformed variable for median home value
l_wyoming_mhv <- wyoming_data %>% 
  mutate(l_mhv = log10(mhv))

# Display data in an interactive table
datatable (l_wyoming_mhv, list(pageLength = 5, autowidth = TRUE), rownames = FALSE )

# ---------------------------------------------------------------
# V. Visualization: Median Home Value Distribution
# ---------------------------------------------------------------
plot <- ggplot(l_wyoming_mhv, aes(x = l_mhv)) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous() +
  labs(x = "Median Home Value")

# Convert to an interactive plot
ggplotly(plot)

# ---------------------------------------------------------------
# VI. Data Normalization- Min-Max
# ---------------------------------------------------------------
## Z-Score Normalization
normalized_zscore <- l_wyoming_mhv %>%
  st_drop_geometry() %>%
  mutate(across(where(is.numeric) & !last_col(), ~ ( . - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE))) %>%
  rename_with(~ paste0(., "Z"), where(is.numeric)) %>%
  bind_cols(wyoming_data %>% select(geometry))  # Keep geometry column intact

# Visualization of Z-Score Normalization
zplot = ggplot(normalized_zscore, aes(x = mhvZ)) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous() +
  labs(x = "Median home value")
ggplotly(zplot) 

## Min-Max Normalization
normalized_minmax <- l_wyoming_mhv %>%
  st_drop_geometry() %>%
  mutate(across(where(is.numeric) & !last_col(), ~ (.-min(.))/(max(.)-min(.)))) %>%
  rename_with(~ paste0(., "Mm"), where(is.numeric)) %>% 
  bind_cols(wyoming_data %>% select(geometry)) %>% 
  st_as_sf()

# Visualization of Min-Max Normalization
mplot = ggplot(normalized_minmax, aes(x = mhvMm)) +
  geom_histogram(alpha = 0.5, fill = "navy", color = "navy", bins = 100) +
  theme_minimal() +
  scale_x_continuous() +
  labs(x = "Median home value")
ggplotly(mplot) 


# ---------------------------------------------------------------
# VII. Make a Choropleth Map for the Median Home Value Distribution
# ---------------------------------------------------------------
tmap_mode('view')
tm_shape(normalized_minmax) +
  tm_polygons(
    col = "mhvMm",               # Column for Median Home Value
    palette = "YlGnBu",         # Sequential color palette
    title = "Median Home Value",
    alpha = 0.6                 # Transparency
  ) +
  tm_layout(
    main.title = "Median Home Value in Wyoming by Census Tract",
    main.title.size = 1.3,
    main.title.fontface = "bold",
    main.title.position = c(0.1, 0.2), # Adjust title position
    bg.color = "white",
    panel.label.bg.color = "white",
    panel.label.size = 0.8,             
    inner.margins = c(0.05, 0.02, 0.08, 0.02),
    outer.margins = c(0.05, 0.05, 0.1, 0.1)
  ) +
  tm_borders(
    col = "gray",
    lwd = 0.08                
  ) +
  tm_scale_bar(
    position = c(0.55, 0.01), 
    text.size = 0.5,          
    lwd = 0.5                 
  ) 


# ---------------------------------------------------------------
# VIII. Construct Spatial Weight Matrix
# ---------------------------------------------------------------
nqueen <- poly2nb(normalized_minmax, queen = TRUE)
nqueenw <- nb2listw(nqueen, style = 'W', zero.policy = TRUE)

# ---------------------------------------------------------------
# IX. Static Plot of Neighboring Structure
# ---------------------------------------------------------------
centroids <- st_centroid(normalized_minmax)
centroids_transformed <- st_transform(centroids, crs = 4326)

plot(st_geometry(normalized_minmax), border = 'grey', 
     main = "Neighboring Structure at Census Tract Level: Wyoming (Queen's)")
plot(nqueen, st_geometry(centroids), add = TRUE, col = 'magenta', lwd = 1)


# ---------------------------------------------------------------
# X. Dynamic Plot Using Tmap
# ---------------------------------------------------------------
coords <- st_coordinates(st_geometry(normalized_minmax))
neighbor_lines <- list()

for (i in seq_along(nqueen)) {
  for (neighbor_id in nqueen[[i]]) {
    if (i < neighbor_id) {
      centroid_i <- st_coordinates(st_centroid(normalized_minmax[i, ]))
      centroid_neighbor <- st_coordinates(st_centroid(normalized_minmax[neighbor_id, ]))
      line <- st_sfc(st_linestring(rbind(centroid_i, centroid_neighbor)), crs = st_crs(wyoming_data))
      neighbor_lines <- append(neighbor_lines, list(line))
    }
  }
}

neighbor_lines_sf <- do.call(c, neighbor_lines)
centroids_sf <- st_as_sf(centroids)

# Create the map using tmap
tmap_mode("view")  # Set to interactive mode

# Plot the map with Leaflet basemap and advanced styling
tm_shape(normalized_minmax) + 
  tm_borders(col = "grey", lwd = 2) +  # Polygon borders with slight width
  tm_fill(col = "lightgray", 
          alpha = 0.5,
          popup.vars = "NAM") +
  tm_dots(col = "blue", size = 0.07, legend.show = TRUE, title = "Centroids") +  # Centroids in magenta
  tm_shape(neighbor_lines_sf) + 
  tm_lines(col = "red", lwd = 1) +  # Neighbor lines in green
  tm_layout(
    main.title = "Neighboring Structure at Census Tract Level: Wyoming (Queen's)",
    main.title.size = 1.5,  # Increase title size
    main.title.position = c("center", "top"),
    legend.title.size = 1.1,  # Legend title size
    legend.text.size = 0.8,   # Legend text size
    frame = FALSE,  # No frame around the map
    fontfamily = "Arial"
  ) + 
  tm_basemap("OpenStreetMap")  # Adding OpenStreetMap as the Leaflet basemap

# ---------------------------------------------------------------
# XI. Spatial Autocorrelation Analysis (Moran's I) 
# ---------------------------------------------------------------
gmoran_result <- moran.test(normalized_minmax $ mhvMm, nqueenw, zero.policy = TRUE, na.action = na.exclude)

# Global Moran's static Plot
moran.plot(normalized_minmax $ mhvMm, nqueenw)


# Global Moran's Interactive visualization
# Prepare data for Moran's plot
lagged_var <- lag.listw(nqueenw, normalized_minmax$ mhvMm, zero.policy = TRUE)
moran_data <- data.frame(
  log_mhv = normalized_minmax$ mhvMm,
  lagged_mhv = lagged_var
)

# Linear regression for the scatterplot line
fit <- lm(lagged_mhv ~ log_mhv, data = moran_data)
line_data <- data.frame(
  log_mhv = seq(min(moran_data$log_mhv), max(moran_data$log_mhv), length.out = 150),
  lagged_mhv = predict(fit, newdata = data.frame(log_mhv = seq(min(moran_data$log_mhv), max(moran_data$log_mhv), length.out = 150)))
)

# Create interactive scatterplot with regression line
plot_ly() %>%
  add_trace(
    data = moran_data,
    x = ~log_mhv,
    y = ~lagged_mhv,
    type = 'scatter',
    mode = 'markers',
    marker = list(size = 7, color = 'blue'),
    hoverinfo = 'text',
    text = ~paste("log_mhv: ", round(log_mhv, 2), "<br>Lagged: ", round(lagged_mhv, 2))
  ) %>%
  add_lines(
    data = line_data,
    x = ~log_mhv,
    y = ~lagged_mhv,
    line = list(color = 'red', dash = 'solid'),
    name = "Trend Line"
  ) %>%
  layout(
    title = "Interactive Moran's I Scatterplot",
    xaxis = list(title = "Median Home Value (log10)"),
    yaxis = list(title = "Spatial Lag of Median Home Value")
  )



# ---------------------------------------------------------------
# XII. Local Moran's I (LISA) Analysis and Cluster Mapping
# ---------------------------------------------------------------

lmoran <- localmoran(normalized_minmax$mhvMm, nqueenw)
# Bind Local Moran's I result to data and classify clusters
normalized_minmax <- cbind(normalized_minmax, lmoran)

# Quadrant Classification
m_log_mhv <- normalized_minmax$mhvMm - mean(normalized_minmax$mhvMm)
m_lmoran <- lmoran[, 1] - mean(lmoran[, 1])
signif <- 0.1  # Significance threshold

quadrant <- vector(mode = "numeric", length = nrow(lmoran))
quadrant[m_log_mhv > 0 & m_lmoran > 0] <- 4
quadrant[m_log_mhv < 0 & m_lmoran < 0] <- 1
quadrant[m_log_mhv < 0 & m_lmoran > 0] <- 2
quadrant[m_log_mhv > 0 & m_lmoran < 0] <- 3
quadrant[lmoran[, 5] > signif] <- 0

normalized_minmax$quadrant <- factor(quadrant, levels = c(0, 1, 2, 3, 4),
                                     labels = c("Insignificant", "Low-Low", "Low-High", "High-Low", "High-High"))

# Cluster Map Visualization
colors <- c("white", "blue", rgb(0, 1, 0, alpha = 0.6), rgb(1, 1, 0, alpha = 0.6), "red")

# moran_clusters <- 
tmap_mode('view')
tm_shape(normalized_minmax) +
  tm_polygons(
    col = "quadrant",
    palette = colors,  # Color palette for clusters
    title = "Clusters", 
    alpha = 0.6,
    popup.vars = c("NAM", "quadrant")
  ) +
  tm_layout(
    main.title = "Local Moran's I Clusters",
    main.title.size = 1.5,
    main.title.fontface = "bold",
    main.title.position = c(0.25, 0.2),                                      # Title size
    bg.color = "white", 
    panel.label.bg.color = "white",                             # Panel label background
    panel.label.size = 0.8,                                     # Smaller panel label size
    inner.margins = c(0.05, 0.02, 0.08, 0.02),                  # Adjust margins
    outer.margins = c(0.05, 0.05, 0.1, 0.1)                   # Space for outside elements
  ) +
  tm_borders(
    col = "gray",
    lwd = 0.08
  ) +
  tm_basemap("OpenStreetMap")


# ---------------------------------------------------------------
# XIII Test for Residual Social Outcome Dependence or Autocorrelation
# ---------------------------------------------------------------

# Ordinary Least Squares (OLS) Model
model1 <- lm(formula = mhvMm ~ 1, data = normalized_minmax)
res <- residuals(model1)

# Test for Residual Autocorrelation
moran.test(res, nqueenw, zero.policy = TRUE)

lm.morantest(model1, nqueenw, zero.policy = TRUE)

# Lagrange Multiplier Test
lm.LMtests(model1, nqueenw, zero.policy = TRUE)


# ---------------------------------------------------------------
# IX. Simultaneous Autoregressive (SAR) Modelling
# ---------------------------------------------------------------

# SAR Modelling
norm_SAR <- spatialreg::spautolm(
  formula = mhvMm ~ t_popMm + m_ageMm + p_coll_deMm + p_whiteMm +
    m_rentMm + p_ownerMm + h_ageMm + m_y_builtMm + m_roomsMm + 
    m_h_incomeMm + uemp_rateMm + po_rateMm + m_h_yearMm + p_t_rateMm + 
    a_h_sizeMm,
  data = normalized_minmax, listw = nqueenw, zero.policy = TRUE
)

summary(norm_SAR)



# ---------------------------------------------------------------
# X. Boruta Feature Selection
# ---------------------------------------------------------------
# Prepare data for Boruta
selected_columns <- c( "p_ownerMm", "uemp_rateMm", "a_h_sizeMm", "m_ageMm",
                       "t_popMm", "p_coll_deMm", "p_whiteMm", "m_rentMm", 
                       "h_ageMm", "m_y_builtMm", "m_roomsMm", "m_h_incomeMm", 
                       "po_rateMm", "m_h_yearMm", "p_t_rateMm")

boruta_data <- normalized_minmax[, selected_columns] %>%
  st_drop_geometry()  # Drop geometry for Boruta
boruta_data <- as.data.frame(boruta_data)  # Ensure it's a data frame

# Run Boruta
set.seed(123)
boruta_result <- Boruta(boruta_data, normalized_minmax$mhvMm, doTrace = 2)

# Display results and retrieve important features
print(boruta_result)

plot(boruta_result, main = "Boruta Feature Importance", las = 2)
final_features <- getSelectedAttributes(boruta_result, withTentative = TRUE)

# Re-run SAR model with selected features
SAR_boruta <- spatialreg::spautolm(formula = mhvMm ~ t_popMm + p_coll_deMm + 
                                     p_whiteMm + m_rentMm + h_ageMm + +
                                     m_y_builtMm + m_roomsMm + m_h_incomeMm 
                                   + p_t_rateMm,
                                   data = normalized_minmax, nqueenw, zero.policy = TRUE)
summary(SAR_boruta)




