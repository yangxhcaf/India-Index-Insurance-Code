

rm(list=ls())
source('/groups/manngroup/scripts/SplineAndOutlierRemoval.R')
source('/groups/manngroup/India_Index/India-Index-Insurance-Code/RasterChuckProcessing.R')
library(RCurl)
library(raster)
#library(MODISTools)
#library(rgdal)
library(sp)
library(maptools)
#library(rts)
#library(gdalUtils)
library(foreach)
library(doParallel)
library(ggplot2)
registerDoParallel(8)


# Set up parameters -------------------------------------------------------

  # give path to Modis Reproduction Tool
  MRT = 'G:/Faculty/Mann/Projects/MRT/bin'

  # get list of all available modis products
  #GetProducts()

  # Product Filters
  products =  c('MYD13Q1','MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
  location = c(30.259,75.644)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
  tiles =   c('h24v05','h24v06')   # India example c('h13v12')
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02')
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/51/'    # allData/6/ for evi,
  # allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
  #strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
  out_dir = '/groups/manngroup/India_Index/Data/MODISLandCover/India/'
  setwd(out_dir)



# Rescale and set valid ranges of data  ---------------------------------------------
  # NOTE: This is run through sbatch with 128gb node
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  # setup a dataframe with valid ranges and scale factors
  valid = data.frame(stack='NDVI', fill= -3000,validL=-2000,validU=10000,scale=0.0001,stringsAsFactors=F)
  valid = rbind(valid,c('EVI',-3000,-2000,10000,0.0001))
  valid = rbind(valid,c('blue_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('red_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('MIR_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('NIR_reflectance',-1000,0,10000,0.0001))
  valid

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one



# Rescale and set valid ranges of data  ---------------------------------------------
  # NOTE: This is run through sbatch with 128gb node
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  # setup a dataframe with valid ranges and scale factors
  valid = data.frame(stack='NDVI', fill= -3000,validL=-2000,validU=10000,scale=0.0001,stringsAsFactors=F)
  valid = rbind(valid,c('EVI',-3000,-2000,10000,0.0001))
  valid = rbind(valid,c('blue_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('red_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('MIR_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('NIR_reflectance',-1000,0,10000,0.0001))
  valid

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

 # Loop through valid ranges
  products2clean = unique(valid$stack)
  tiles = c( 'h24v05','h24v06')
  for(product in products2clean){
  for( tile in tiles){
        print(paste('Working on',product,tile))
        # load product data
        lapply(dir1[grep(product,dir1)],load,.GlobalEnv)
        data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
        valid_values = valid[grep(product,valid$stack),]

        ScaleClean = function(x){
                x[x==valid_values$fill]=NA
                x[x < valid_values$validL]=NA
                x[x > valid_values$validU]=NA
                x = x * valid_values$scale
                x}

        RasterChunkProcessing(in_stack=data_stackvalues,in_stack_nm=paste(product,'_stack_',tile,sep='')
            ,block_width=10,worker_number=12,out_path='./WO Clouds Crops Scaled/',
            out_nm_postfix='WO_Clouds_Crops_Scaled', FUN=ScaleClean)

        rm(list=ls()[grep(product,ls())])
  }}




