# =========================================================
# Sentinel-2 NDVI Downloader for Rio de Janeiro (RJ)
# =========================================================

# --- 0️⃣ Libraries ---
library(sf)       # for geometry manipulation
library(httr)     # for HTTP requests
library(jsonlite) # for JSON parsing
library(terra)    # for raster and NDVI calculations

# =========================================================
# 1️⃣ Define Area of Interest (AOI) - Rio de Janeiro city
# =========================================================

aoi <- list(
  xmin = -43.795,  # bbox
  ymin = -23.045,  
  xmax = -43.145,  
  ymax = -22.725   
)

# =========================================================
# 2️⃣ Function to search Sentinel-2 L2A images via AWS STAC
# =========================================================
search_sentinel2 <- function(aoi, start_date = "2023-01-01", end_date = "2023-12-31", limit = 200) {
  url <- "https://earth-search.aws.element84.com/v0/search"
  
  body <- list(
    collections = list("sentinel-s2-l2a-cogs"),
    datetime = paste0(start_date, "/", end_date),
    bbox = c(aoi$xmin, aoi$ymin, aoi$xmax, aoi$ymax),
    limit = limit
  )
  
  res <- httr::POST(url, body = jsonlite::toJSON(body, auto_unbox = TRUE), encode = "json")
  res_json <- content(res, as = "parsed", type = "application/json")
  
  features <- res_json$features
  if(length(features) == 0) stop("No images found! Try expanding the dates or AOI.")
  
  return(features)
}

# =========================================================
# 3️⃣ Function to select a random image
# =========================================================
select_random_image <- function(features, seed = 123) {
  set.seed(seed)
  features[[sample(1:length(features), 1)]]
}

# =========================================================
# 4️⃣ Function to download bands (B04 and B08)
# =========================================================
download_bands <- function(feature) {
  b04_url <- feature$assets$B04$href
  b08_url <- feature$assets$B08$href
  
  cat("B04 URL:", b04_url, "\nB08 URL:", b08_url, "\n")
  
  b4_file <- tempfile(fileext = ".tif")
  b8_file <- tempfile(fileext = ".tif")
  
  # Increase global timeout
  options(timeout = 300)
  
  GET(b04_url, write_disk(b4_file, overwrite = TRUE), progress())
  GET(b08_url, write_disk(b8_file, overwrite = TRUE), progress())
  
  list(red = rast(b4_file), nir = rast(b8_file))
}

# =========================================================
# 5️⃣ Function to calculate NDVI
# =========================================================
calculate_ndvi <- function(red_band, nir_band) {
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  names(ndvi) <- "NDVI"
  return(ndvi)
}

# =========================================================
# 6️⃣ Main pipeline
# =========================================================
main <- function() {
  features <- search_sentinel2(aoi)
  selected_image <- select_random_image(features)
  bands <- download_bands(selected_image)
  
  ndvi <- calculate_ndvi(bands$red, bands$nir)
  
  # Save NDVI
  output_file <- "NDVI_RJ.tif"
  writeRaster(ndvi, output_file, overwrite = TRUE)
  cat("NDVI saved to:", output_file, "\n")
  
  # Visualize NDVI
  plot(ndvi, col = rev(terrain.colors(100)), main = "Sentinel-2 NDVI RJ")
}

# =========================================================
# Run script
# =========================================================
main()
