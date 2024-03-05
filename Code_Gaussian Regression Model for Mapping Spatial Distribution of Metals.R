# Load necessary libraries
library(ggplot2)
library(sf)
library(raster)
library(gstat)
library(rasterVis)

# Read in shapefile of Ayanfuri mining areas
ayanfuri_shape <- st_read("path/to/ayanfuri_shapefile.shp")

# Read in data frame of heavy metal concentrations in surface and ground water
heavy_metals <- read.csv("path/to/heavy_metals_data.csv")

# Convert data frame to spatial data frame
coordinates(heavy_metals) <- ~ x + y
proj4string(heavy_metals) <- proj4string(ayanfuri_shape)

# Perform Gaussian regression model to Map the spatial distribution of metals
metal_model <- gstat(formula = heavy_metals$metal_concentration ~ 1, locations = ~x+y, data = heavy_metals)

# Predict metal concentrations for Ayanfuri mining areas
predicted_metals <- predict(metal_model, ayanfuri_shape)

# Plot the spatial distribution of metals in Ayanfuri's mining areas
ggplot() +
  geom_sf(data = ayanfuri_shape, fill = "gray") +
  geom_raster(data = predicted_metals, aes(fill = metal_concentration)) +
  scale_fill_gradientn(colours = c("green", "yellow", "red"),
                       values = c(0, 0.5, 1),
                       guide = "colorbar", name = "Metal Concentration") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()

# Create a buffer around the mining areas to indicate surrounding regions potentially at risk
buffer <- st_buffer(ayanfuri_shape, dist = 5000) # adjust distance as needed

# Predict metal concentrations for the buffer area
predicted_metals_buffer <- predict(metal_model, buffer)

# Calculate the risk based on the predicted metal concentrations
risk <- ifelse(predicted_metals_buffer$metal_concentration > 0.5, "High Risk", "Low Risk")

# Create a raster layer for the risk
raster_risk <- rasterFromXYZ(cbind(coordinates(buffer), as.numeric(risk)))

# Plot the risk layer
ggplot() +
  geom_raster(data = raster_risk, aes(fill = value)) +
  scale_fill_manual(values = c("Low Risk" = "green", "High Risk" = "red")) +
  geom_sf(data = buffer, fill = NA, color = "black") +
  labs(x = "Longitude", y = "Latitude", fill = "Risk") +
  theme_bw()