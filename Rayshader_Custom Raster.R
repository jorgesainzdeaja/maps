### Code references:
#### https://cheartography.blogspot.com/2011/06/vintage-colour-palette-for-maps.html
#### https://medium.com/@niloy.swe/how-to-create-a-3d-population-density-map-in-r-33dfaf7a71d7
#### https://www.rayshader.com/
#### https://gist.github.com/norwegianblueparrot/b9d5d48f2d591d78a14320bf17459cc5

## required libraries

library(geojsonsf)
library(jsonify)
library(rayshader)
library(raster)
library(sf)
library(sp)
library(magick)

# get SW catchment data or Boundary
## You can avoid this section by providing any sf POLYGON in EPSG::27700 e.g. below
### SW_C <- sf::st_transform(sf::st_read("your POLYGON PATH HERE"), 27700)
SW <- sf::st_transform(sf::st_read("https://environment.data.gov.uk/catchment-planning/WaterBody/GB106039017700.geojson"), 27700)
SW_C <- sf::st_as_sf(SW[sf::st_geometry_type(SW) == "POLYGON", ])
SW_C_4326 <- sf::st_transform(SW_C, 4326)

#### defining the data extend based on the provided boundary -- SW_C
bb <- sf::st_bbox(SW_C)

pop_r <- raster::raster("https://data.worldpop.org/GIS/Population/Global_2000_2020/2019/GBR/gbr_ppp_2019.tif")
#raster::crs(pop_r) <- "EPSG:4326" # just in case, sometimes raster comes with NA values in the CRS and that affects the visualization
pop <- raster::crop(pop_r, raster::extent(SW_C_4326)) # the raster is in epsg:4326 therefore we need to use our boundary transformed to epsg:4326

## this raster give a lot of problems to visualize when reprojection, my guess is a problem with the metadata...
### to solve the issue lets convert it to points, reproject the points and recompose the raster, which does the trick

df <- data.frame(coordinates(pop), value = getValues(pop))  # get raster coordinates
df <- sf::st_as_sf(df, coords = c("x", "y"), crs = 4326, agr = "constant") # convert coordinates to sf points
df <- sf::st_transform(df, 27700) # transform the points to new crs
df <- as.data.frame(sf::st_coordinates(df)) # extract the re-projected coordinates

res <- 100  # assign the raster resolution, this raster cells are 100 x 100 meters 
df$X <- round(df$X / res) * res # rounding the cooridnates based on resolution
df$Y <- round(df$Y / res) * res
df$vals <- raster::getValues(pop) # re-assigning the raster values to the data frame

spg <- df
sp::coordinates(spg) <- ~ X + Y
# coerce to SpatialPixelsDataFrame
sp::gridded(spg) <- TRUE # grid == matrix
# coerce to raster
pop <- raster(spg) # convert the grid to raster
pop <- raster::disaggregate(pop, fact = 5, method = "bilinear") # adding a some extra detail as the raster cells are 100x100m
pop <- raster::mask(pop, SW_C) # masking raster values outside our boundary

## converting raster into matrix
pop_mat <- rayshader::raster_to_matrix(pop)

### taking out all NAs and Inf values from the cropping process
pop_mat[is.na(pop_mat)] <- 0       # Remove NAs
pop_mat[!is.finite(pop_mat)] <- 0  # Remove Inf/-Inf
storage.mode(pop_mat) <- "double"

Bound_overlay <- sf::st_cast(SW_C, "MULTILINESTRING") %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(pop),
    heightmap = pop_mat,
    color = "red",
    linewidth = 2
  )

############## 3D POP MAP #############

## play around with the "zscale" value to get optimal results for visualization
### you can assign different palettes, e.g. viridis. But I leave here a simplified way so you can select 3 colors (minimum) to generate a ramp palette

pop_mat %>%
  rayshader::height_shade(
    texture = (grDevices::colorRampPalette(c("white", "#FCFFA4FF", "#F98C0AFF", "#BB3754FF", "#56106EFF", "#000004FF")))(256),
    range = NULL,
    keep_user_par = TRUE
  ) %>%
  add_shadow(ray_shade(pop_mat, zscale = 0.5), 0.5) %>%
  add_shadow(ambient_shade(pop_mat, zscale = 0.5), 0) %>%
  rayshader::add_overlay(overlay = Bound_overlay, alphalayer = 1) %>%
  plot_3d(pop_mat, zscale = 0.5, fov = 60, theta = 1, zoom = 0.7, phi = 30, windowsize = c(2000, 1500))

render_camera(fov = 60, theta = 1, zoom = 0.7, phi = 30)
render_snapshot("pop_camera.png") # <- execute this for a preview of you render (low quality)

### rendering in high quality
outfile <- glue::glue("Pop_Density.png") # define the render file name

render_highquality(
  filename = outfile,
  interactive = F,
  lightdirection = 55, #Degree
  lightaltitude = c(30, 80),
  lightcolor = c("white", "white"),  # Set both lights to white
  lightintensity = c(600, 100),
  width = 2000,
  height = 1580,
  samples = 500
) # once executed this, the render will be saved automatically

### annotating your render
POP_raster <- magick::image_read("Population_Density.png") # loading the rendered image from previous step
POP_raster %>%
  # adding a tile
  magick::image_annotate("River Wey Cathment Area",
                         gravity = "northeast",
                         location = "+50+50",
                         color = "#aa9966",
                         size = 60,
                         font = "Arial",
                         weight = 700,
                         # degrees = 0,
  ) %>%
  # adding a subtitle
  magick::image_annotate("Population Density",
                         gravity = "northeast",
                         location = "+20+50",
                         color = "#aa9966",
                         size = 60,
                         font = "Arial",
                         weight = 700,
                         # degrees = 0,
  ) %>%
  # saving the annotated image
  magick::image_write("Annotated_Population_Density.png", format = "png", quality = 100)

###########################################################################################################
############################# EXAMPLE 02 ##################################################################
############################# USING LANDSAT 8 RASTERS #####################################################


# getting landsat 8 raster based on defined bb
## API call for Band 4
B4_url <- paste0("https://landsat2.arcgis.com/arcgis/rest/services/Landsat8_Views/ImageServer/exportImage?bbox=", bb[1], "%2C+", bb[2], "%2C+", bb[3], "%2C+", bb[4], "&bboxSR=", 27700, "&size=", 5000, "&imageSR=", 27700, "&time=&format=tiff&pixelType=S16&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=", 4, "&sliceId=&mosaicRule=&renderingRule=&adjustAspectRatio=true&validateExtent=false&lercVersion=2&compressionTolerance=&f=pjson")
### reading API respond
B4 <- jsonify::from_json(B4_url)
#### converting to raster
B4 <- raster::raster(B4$href)
## API call for Band 5
B5_url <- paste0("https://landsat2.arcgis.com/arcgis/rest/services/Landsat8_Views/ImageServer/exportImage?bbox=", bb[1], "%2C+", bb[2], "%2C+", bb[3], "%2C+", bb[4], "&bboxSR=", 27700, "&size=", 5000, "&imageSR=", 27700, "&time=&format=tiff&pixelType=S16&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=", 5, "&sliceId=&mosaicRule=&renderingRule=&adjustAspectRatio=true&validateExtent=false&lercVersion=2&compressionTolerance=&f=pjson")
B5 <- jsonify::from_json(B5_url)
B5 <- raster::raster(B5$href)

## cropping raster with BB extent
if (!is.null(B4)){red <- raster::crop(B4, sf::as_Spatial(sf::st_as_sfc(bb)))}
if (!is.null(B5)){near.infrared <- raster::crop(B5, sf::as_Spatial(sf::st_as_sfc(bb)))}
### calculating NDVI
LANDSAT_NDVI <- (near.infrared - red) / (near.infrared + red)

########### alternative option (TIR) ############

#B10_url <- paste0("https://landsat2.arcgis.com/arcgis/rest/services/Landsat8_Views/ImageServer/exportImage?bbox=", bb[1], "%2C+", bb[2], "%2C+", bb[3], "%2C+", bb[4], "&bboxSR=", 4326, "&size=", 5000, "&imageSR=", 4326, "&time=&format=tiff&pixelType=S16&noData=&noDataInterpretation=esriNoDataMatchAny&interpolation=+RSP_BilinearInterpolation&compression=&compressionQuality=&bandIds=", 10, "&sliceId=&mosaicRule=&renderingRule=&adjustAspectRatio=true&validateExtent=false&lercVersion=2&compressionTolerance=&f=pjson")
#B10 <- jsonify::from_json(B10_url)
#B10 <- raster::raster(B10$href)

#TIR <- B10
#LANDSAT_TIR <- raster::crop(TIR, raster::extent(SW_C))

##########################################

# Masking raster with boundary
NDVI <- raster::mask(LANDSAT_NDVI, SW_C, updatevalue = min(LANDSAT_NDVI@data@values))
## convert raster to matrix
NDVI_mat <- rayshader::raster_to_matrix(NDVI)
## assigning the minimum values in the raster to NA values, nor mally we would assign a 0 but NDVI values could be negative
NDVI_mat[is.na(NDVI_mat)] <- min(LANDSAT_NDVI@data@values)       # Remove NAs
NDVI_mat[!is.finite(NDVI_mat)] <- min(LANDSAT_NDVI@data@values)  # Remove Inf/-Inf
storage.mode(NDVI_mat) <- "double"

## this step is optional - it normalize the NDVI values, to avoid negative results and re-scale the results to ensure the maximmum heights are more visible on the visualization
#### if you go ahead with this, bear in mind that this will affect NDVI values and it is only for visualization purposes
NDVI_mat <- (NDVI_mat - min(NDVI_mat, na.rm = TRUE)) / (max(NDVI_mat, na.rm = TRUE) - min(NDVI_mat, na.rm = TRUE))
NDVI_mat <- NDVI_mat^3

## from here the comments are exactly the same as the previous example

Bound_overlay <- sf::st_cast(SW_C, "MULTILINESTRING") %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(NDVI),
    heightmap = NDVI_mat,
    color = "red",
    linewidth = 2
  )

############## 3D NDVI MAP #############
NDVI_mat %>%
  rayshader::height_shade(
    texture = (grDevices::colorRampPalette(c("white", "#FCFFA4FF", "#F98C0AFF", "#BB3754FF", "#56106EFF", "#000004FF")))(256),
    range = NULL,
    keep_user_par = TRUE
  ) %>%
  add_shadow(ray_shade(NDVI_mat, zscale = 0.05), 0.5) %>%
  add_shadow(ambient_shade(NDVI_mat, zscale = 0.05), 0) %>%
  rayshader::add_overlay(overlay = Bound_overlay, alphalayer = 1) %>%
  plot_3d(NDVI_mat, zscale = 0.05, fov = 60, theta = 1, zoom = 0.7, phi = 30, windowsize = c(2000, 1500))

render_camera(fov = 60, theta = 1, zoom = 0.7, phi = 30)
render_snapshot("NDVI_camera.png")

outfile <- glue::glue("Pop_Density.png")

render_highquality(
  filename = outfile,
  interactive = F,
  lightdirection = 55, #Degree
  lightaltitude = c(30, 80),
  lightcolor = c("white", "white"),  # Set both lights to white
  lightintensity = c(600, 100),
  width = 2000,
  height = 1580,
  samples = 500
  #samples = 2
)

NDVI_raster <- magick::image_read("NDVI.png")
NDVI_raster %>%
  magick::image_annotate("River Wey Cathment Area",
                         gravity = "northeast",
                         location = "+50+50",
                         color = "#aa9966",
                         size = 60,
                         font = "Arial",
                         weight = 700,
                         # degrees = 0,
  ) %>%
  magick::image_annotate("NDVI",
                         gravity = "northeast",
                         location = "+20+50",
                         color = "#aa9966",
                         size = 60,
                         font = "Arial",
                         weight = 700,
                         # degrees = 0,
  ) %>%
  magick::image_write("Annotated_NDVI.png", format = "png", quality = 100)
