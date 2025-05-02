
# required libraries
library(sf)
library(raster)
library(rayshader)
library(ows4R)
library(osdatahub)
library(geojsonsf)
library(grDevices)
library(magick)

# get SW catchment data or Boundary
## You can avoid this section by providing any sf POLYGON in EPSG::27700, e.g. below
### SW_C <- sf::st_transform(sf::st_read("your POLYGON PATH HERE"), 27700)
SW <- sf::st_transform(sf::st_read("https://environment.data.gov.uk/catchment-planning/WaterBody/GB106039017700.geojson"), 27700)
SW_C <- sf::st_as_sf(SW[sf::st_geometry_type(SW) == "POLYGON", ])
SW_R <- sf::st_as_sf(SW[sf::st_geometry_type(SW) == "MULTILINESTRING"])

# Function to retrieve necessary layers from Ordnance Survey
Zoomstack <- function(theme, bbox) {
  
  data <- list()
  
  for (i in seq_along(1:10000)) {
    
    c <- ((i - 1) * 100) + 1
    
    base_url <- "https://api.os.uk/features/v1/wfs?service=wfs&version=2.0.0&request=GetFeature"
    dataset <- paste0("&typeNames=", theme)
    count <- paste0("&count=", 100)
    index <- paste0("&startIndex=", c)
    key <- paste0("&key=", osdatahub::get_os_key())
    area <- paste0("&bbox=", bbox[1],",", bbox[2],",", bbox[3],",", bbox[4])
    srs <- "&srsName=EPSG:27700"
    format <- "&outputFormat=GeoJSON"           
    
    request <- paste0(base_url, dataset, index, count, key, area, srs, format)
    dat <- geojsonsf::geojson_sf(request)
    dat <- sf::`st_crs<-`(dat, 27700)
    data[[length(data) + 1]] <- dat 
    
    if (length(dat$geometry) < 100) {break}
    
  }
  
  return(all_data <- do.call(rbind, data))
  
}

## you need to have a KEY from Ordnance Survey (it is free!)
### you can always manually download the layers from Ordnance Survey website, but you will loose part of the automation process
osdatahub::set_os_key("YOUR KEY") # <- ADD YOUR KEY HERE  

### stack layers required to generate the map (there are a few more, please check Ordnance Survey API documentation for full list -- https://www.ordnancesurvey.co.uk/products/os-maps-api)
zoomstack_list <- c("Zoomstack_Greenspace", "Zoomstack_NationalParks","Zoomstack_Rail",
                    "Zoomstack_RoadsLocal","Zoomstack_RoadsNational","Zoomstack_RoadsRegional","Zoomstack_Sites",
                    "Zoomstack_Surfacewater","Zoomstack_Waterlines","Zoomstack_Woodland", "Zoomstack_LocalBuildings")

#### defining the data extend based on the provided boundary -- SW_C
bb <- sf::st_bbox(SW_C)

# Apply the Ordnance Survey data function to the list of layers desired
for (i in seq_along(zoomstack_list)) {
  assign(zoomstack_list[i], Zoomstack(zoomstack_list[i], bb))
}

## Cropping all Ordnance Survey layers with the provided boundary
for (i in seq_along(zoomstack_list)) {
  assign(zoomstack_list[i], sf::st_intersection(get(zoomstack_list[i]), SW_C))
}

# get DSM raster from DEFRA
dsm_wcs <- ows4R::WCSClient$new("https://environment.data.gov.uk/geoservices/datasets/9ba4d5ac-d596-445a-9056-dae3ddec0178/wcs?", "2.0.1", logger = "INFO")
dsm_cov <- dsm_wcs$capabilities$findCoverageSummaryById("9ba4d5ac-d596-445a-9056-dae3ddec0178__Lidar_Composite_Elevation_LZ_DSM_1m", exact = F)
dsm <- dsm_cov$getCoverage(bbox = ows4R::OWSUtils$toBBOX(bb[1],bb[3],bb[2],bb[4]))
DEM_DSM <- raster::raster(dsm)

# reducing the size of the raster, for areas too large, this could crash your computer, so be mindful, the larger the "fact" variable, the lower the detail on the raster 
DEM_DSM <- raster::aggregate(DEM_DSM, fact = 5, fun = mean, na.rm = TRUE)

## Cropping the raster with the defined boundary
DEM_DSM <- raster::mask(DEM_DSM, SW_C)

## converting raster into matrix
DEM_mat <- rayshader::raster_to_matrix(DEM_DSM)

### taking out all NAs and Inf values from the cropping process
DEM_mat[is.na(DEM_mat)] <- 0       # Remove NAs
DEM_mat[!is.finite(DEM_mat)] <- 0  # Remove Inf/-Inf
storage.mode(DEM_mat) <- "double"

## adding overlay layers, each Zoomstack layer from the Ordnance Survey data is assigned a colour and a type of overlay
grass_overlay <- Zoomstack_Greenspace %>%
  rayshader::generate_line_overlay(extent = raster::extent(DEM_DSM),
                                   heightmap = DEM_mat,
                                   color = "#aabb88")

tree_area <- sf::st_as_sf(rbind(Zoomstack_Woodland, Zoomstack_NationalParks))

tree_overlay <- tree_area %>%
  rayshader::generate_line_overlay(extent = raster::extent(DEM_DSM),
                                   heightmap = DEM_mat,
                                   color = "#738550")

Building_overlay <- Zoomstack_LocalBuildings %>%
  rayshader::generate_polygon_overlay(extent = raster::extent(DEM_DSM),
                                      heightmap = DEM_mat,
                                      palette = "grey")

Sites_overlay <- Zoomstack_Sites %>%
  rayshader::generate_polygon_overlay(extent = raster::extent(DEM_DSM),
                                      heightmap = DEM_mat,
                                      palette = "pink")

Water_overlay <- Zoomstack_Waterlines %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(DEM_DSM),
    heightmap = DEM_mat,
    color = "#226b87",
    linewidth = 2)

SurfaceWater_overlay <- Zoomstack_Surfacewater %>%
  rayshader::generate_line_overlay(extent = raster::extent(DEM_DSM),
                                   heightmap = DEM_mat,
                                   color = "#6fa4ad")

LocalRoads_overlay <- Zoomstack_RoadsLocal %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(DEM_DSM),
    heightmap = DEM_mat,
    color = "gainsboro",
    linewidth = 2
  )

RegRoads_overlay <- Zoomstack_RoadsRegional %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(DEM_DSM),
    heightmap = DEM_mat,
    color = "gainsboro",
    linewidth = 2.25
  )

NatRoads_overlay <- Zoomstack_RoadsNational %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(DEM_DSM),
    heightmap = DEM_mat,
    color = "gainsboro",
    linewidth = 2.5
  )

Rail_overlay <- Zoomstack_Rail %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(DEM_DSM),
    heightmap = DEM_mat,
    color = "#4a4a4a",
    linewidth = 2.5
  )

Bound_overlay <- sf::st_cast(SW_C, "MULTILINESTRING") %>%
  rayshader::generate_line_overlay(
    extent = raster::extent(DEM_DSM),
    heightmap = DEM_mat,
    color = "red",
    linewidth = 2
  )

######### GENRATING A 2D MAP ###########

DEM_mat %>%
  #sphere_shade(texture = "imhof1") %>%
  rayshader::height_shade(
    texture = (grDevices::colorRampPalette(c("white", "#aabb88", "#eedd99", "#ccbb66", "#aa9966")))(256),
    range = NULL,
    keep_user_par = TRUE
  ) %>%
  add_shadow(ray_shade(DEM_mat), 0.5) %>%
  add_shadow(ambient_shade(DEM_mat), 0) %>%
  rayshader::add_overlay(overlay = grass_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = tree_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = Sites_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = SurfaceWater_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = LocalRoads_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = RegRoads_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = NatRoads_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = Rail_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = Water_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = Building_overlay, alphalayer = 0.8) %>%
  rayshader::add_overlay(overlay = Bound_overlay, alphalayer = 1) %>%
  plot_3d(DEM_mat, zscale = 10, fov = 40, theta = 0.1, zoom = 0.55, phi = 89, windowsize = c(2000, 1600))

render_camera(fov = 60, theta = 0, zoom = 0.55, phi = 89)
render_snapshot("after_map.png")

outfile <- glue::glue("River_Wey_map.png")

render_highquality(
  filename = outfile,
  interactive = F,
  lightdirection = 55, #Degree
  lightaltitude = c(30, 80),
  lightcolor = c("white", "white"),  # Set both lights to white
  lightintensity = c(600, 100),
  width = 2000,
  height = 1080,
  samples = 500
  #samples = 2
)

WR_map <- magick::image_read("River_Wey_map.png")

WR_map %>%
  magick::image_annotate("River Wey Cathment Area",
                         gravity = "north",
                         location = "0+50",
                         color = "#aa9966",
                         size = 60,
                         font = "Arial",
                         weight = 700,
                         # degrees = 0,
  ) %>%
  magick::image_annotate("Visualization by: Jorge Sainz de Aja Curbelo with Rayshader\nData: DEFRA + Ordnance Survey",
                         gravity = "south",
                         location = "0+20",
                         color = "black",
                         font = "Arial",  # Corrected font name
                         size = 20,
                         # degrees = 0,
  ) %>%
  magick::image_write("Annotated_River_Wey_map.png", format = "png", quality = 100)

######### GENRATING A 3D MAP ###########
DEM_mat %>%
  rayshader::height_shade(
    texture = (grDevices::colorRampPalette(c("white", "#aabb88", "#eedd99", "#ccbb66", "#aa9966")))(256),
    range = NULL,
    keep_user_par = TRUE
  ) %>%
  add_shadow(ray_shade(DEM_mat), 0.5) %>%
  add_shadow(ambient_shade(DEM_mat), 0) %>%
  rayshader::add_overlay(overlay = grass_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = tree_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = Sites_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = SurfaceWater_overlay, alphalayer = 0.6) %>%
  rayshader::add_overlay(overlay = LocalRoads_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = RegRoads_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = NatRoads_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = Rail_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = Water_overlay, alphalayer = 0.9) %>%
  rayshader::add_overlay(overlay = Building_overlay, alphalayer = 0.8) %>%
  rayshader::add_overlay(overlay = Bound_overlay, alphalayer = 1) %>%
  plot_3d(DEM_mat, zscale = 10, fov = 40, theta = 1, zoom = 0.4, phi = 30, windowsize = c(2000, 1000))

render_camera(fov = 60, theta = 1, zoom = 0.5, phi = 30)

# adding clouds, if you prefer not to add lovely fluffy clouds, skip this step.
render_clouds(DEM_mat,
              zscale = 4,
              start_altitude = 200,
              end_altitude = 400,
              attenuation_coef = 2,
              clear_clouds = T,
              cloud_cover = 0.4,
              scale_y = 2,
              seed = 3)

# save image
render_snapshot("after_camera.png")

## generating a high-quality render
outfile <- glue::glue("River_Wey_clouds.png") # render file name

render_highquality(
  filename = outfile,
  interactive = F,
  lightdirection = 55, #Degree
  lightaltitude = c(30, 80),
  lightcolor = c("white", "white"),  # Set both lights to white
  lightintensity = c(600, 100),
  width = 2000,
  height = 1080,
  samples = 500
  #samples = 2
) # once executed, the render will be saved automatically

### annotating your render
WR_raster <- magick::image_read("River_Wey_clouds.png")# loading the rendered image from previous step
WR_raster %>%
  # adding a tile
  magick::image_annotate("River Wey Cathment Area",
                 gravity = "northeast",
                 location = "+50+50",
                 color = "#aa9966",
                 size = 60,
                 font = "Arial",
                 weight = 700,
  ) %>%
  # adding a subtitle
  magick::image_annotate("Visualization by: Jorge Sainz de Aja Curbelo with Rayshader\nData: DEFRA + Ordnance Survey",
                 gravity = "southwest",
                 location = "+20+20",
                 color = "black",
                 font = "Arial",
                 size = 25
  ) %>%
  # saving the annotated image
  magick::image_write("Annotated_River_Wey_clouds.png", format = "png", quality = 100)
