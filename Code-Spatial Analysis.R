# Load necessary libraries
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(ggplot2)

# Read in shapefile of Ayanfuri mining areas
shapefile <- readOGR("path/to/shapefile.shp")

# Read in data frame of heavy metal concentrations, including longitude and latitude columns
heavy_metals <- read.csv("path/to/heavy_metals.csv")

# Create spatial points data frame from heavy metal concentrations data
coords <- SpatialPoints(coords = cbind(heavy_metals$longitude, heavy_metals$latitude), proj4string = shapefile@proj4string)
heavy_metals_spdf <- SpatialPointsDataFrame(coords, data = heavy_metals)

# Create semivariogram object for surface water heavy metals
semivariogram_surface_heavy_metals <- variogram(surface_heavy_metals ~ 1, heavy_metals_spdf)

# Fit semivariogram model for surface water heavy metals
model_surface_heavy_metals <- gstat(formula = surface_heavy_metals ~ 1, data = heavy_metals_spdf, trendmodel = "cte", fit.k = FALSE, model = vgm(psill = 1, model = "Sph", range = 1000, nugget = 0.01))

# Predict surface water heavy metal concentrations for Ayanfuri mining areas
predicted_surface_heavy_metals <- predict(model_surface_heavy_metals, newdata = shapefile, na.action = na.omit)

# Create semivariogram object for ground water heavy metals
semivariogram_ground_heavy_metals <- variogram(ground_heavy_metals ~ 1, heavy_metals_spdf)

# Fit semivariogram model for ground water heavy metals
model_ground_heavy_metals <- gstat(formula = ground_heavy_metals ~ 1, data = heavy_metals_spdf, trendmodel = "cte", fit.k = FALSE, model = vgm(psill = 1, model = "Sph", range = 1000, nugget = 0.01))

# Predict ground water heavy metal concentrations for Ayanfuri mining areas
predicted_ground_heavy_metals <- predict(model_ground_heavy_metals, newdata = shapefile, na.action = na.omit)

# Create raster layers for surface water and ground water heavy metal concentrations
raster_surface_heavy_metals <- rasterFromXYZ(predicted_surface_heavy_metals, crs = shapefile@proj4string)
raster_ground_heavy_metals <- rasterFromXYZ(predicted_ground_heavy_metals, crs = shapefile@proj4string)

# Create spatial data frames for surface water and ground water heavy metal concentrations
spdf_surface_heavy_metals <- SpatialPointsDataFrame(coordinates(raster_surface_heavy_metals), data = raster_surface_heavy_metals)
spdf_ground_heavy_metals <- SpatialPointsDataFrame(coordinates(raster_ground_heavy_metals), data = raster_ground_heavy_metals)

# Merge surface water and ground water heavy metal concentrations spatial data frames
spdf_heavy_metals <- merge(spdf_surface_heavy_metals, spdf_ground_heavy_metals)

# Plot heavy metal concentrations in Ayanfuri mining areas
ggplot() +
  geom_raster(data = raster_surface_heavy_metals, aes(fill = surface_heavy_metals)) +
  geom_raster(data = raster_ground_heavy_metals, aes(fill = ground_heavy_metals))