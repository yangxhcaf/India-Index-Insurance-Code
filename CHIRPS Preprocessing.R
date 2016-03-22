
library(rgdal)
library(R.utils)
library(foreach)
library(doParallel)
library(maptools)
library(raster)
registerDoParallel(5)



# Unzip files -------------------------------------------------------------
  
  setwd('G:\\Faculty\\Mann\\Projects\\India_Index_Insurance\\Data\\CHIRPS')
  
  files = list.files(".", pattern="*.tif.gz", full.names=T, recursive=T)
  head(files)
  tail(files)
  
  # unzip chirps data and delete gz files
  junk= foreach(x = 1:length(files),.packages = 'R.utils') %dopar% {
    gunzip(files[x])
    if (file.exists(files[x])) file.remove(files[x])
    }
  


# Limit rasters to bounding box -------------------------------------------
  #westlimit=71.94; southlimit=26.12; eastlimit=79.98; northlimit=33.1  
  
  # read in global rain data
  files = list.files(".", pattern="*.tif", full.names=T, recursive=T)
  head(files)
  tail(files)
  
  # create mask 
  modis = raster('../../Data/India/MOD13Q1_2002193_h24v05.250m_16_days_EVI.tif')
  r =raster(files[1])
  r =crop(r,extent(71.94,79.98,26.12,33.1))
  proj = proj4string(readOGR("../Admin Boundaries","PunjabHaryana"))
  states = readShapePoly("../Admin Boundaries/PunjabHaryana.shp", proj4string=CRS(proj))
  states =  spTransform(states,CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
  states = rasterize(states,r,field=c(1,1),fun='first')
  states = projectRaster(states,modis,method='bilinear')
  plot(states)
  
  
  # crop to punjab and haryana
  junk= foreach(x = 1:length(files),.packages = 'R.utils') %dopar% {
    r = raster(files[x])
    r = crop(r,extent(66.84,82.44,22.96,36.77))
  }

