
fortify2 <- function(model, data, region = NULL, ...) {
  attr <- as.data.frame(model)
  # If not specified, split into regions based on polygons
  if (is.null(region)) {
    coords <- ldply(model@polygons,fortify)
    message("Regions defined for each Polygons")
  } else {
    cp <- polygons(model)
    require("maptools")
    
    # Union together all polygons that make up a region
    unioned <- unionSpatialPolygons(cp, attr[, region])
    coords <- fortify(unioned)
    coords$order <- 1:nrow(coords)
  }
  coords
}
