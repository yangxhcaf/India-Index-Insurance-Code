

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
library(compiler)

registerDoParallel(12)


source('/groups/manngroup/India_Index/India-Index-Insurance-Code/Functions.R')

functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz/Profil$






# Set up parameters -------------------------------------------------------


  # give path to Modis Reproduction Tool
  MRT = 'G:/Faculty/Mann/Projects/MRT/bin'
  
  # get list of all available modis products
  #GetProducts()
  
  # Product Filters 
  products =  c('MYD13Q1','MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1' for 250m and landcover ='MCD12Q2'
  location = c(30.259,75.644)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
  tiles =   c('h24v05','h24v06')   # India example c('h24v05','h24v06'), Uruguay h13v12  
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02') 
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, 
  # allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
  #strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
  out_dir = '/groups/manngroup/India_Index/Data/India/'
  setwd(out_dir)


# Functions ---------------------------------------------------------------


# Set valid ranges of data  ---------------------------------------------
 setwd('/groups/manngroup/India_Index/Data/Data Stacks')

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  # setup a dataframe with valid ranges and scale factors
  valid = data.frame(stack='NDVI', fill= -3000,validL=-2000,validU=10000,
                scale=0.0001,stringsAsFactors=F)
  valid = rbind(valid,c('EVI',-3000,-2000,10000,0.0001))
  valid = rbind(valid,c('blue_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('red_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('MIR_reflectance',-1000,0,10000,0.0001))
  valid = rbind(valid,c('NIR_reflectance',-1000,0,10000,0.0001))
  valid

  rm(list=ls()[grep('stack',ls())]) # running into memory issues clear stacks load one by one

  # Loop through valid ranges
  products2clean = unique(valid$stack)

  for(product in products2clean){
  for( tile in tiles){
        print(paste('Working on',product,tile))
        # load product data
        lapply(dir1[grep(product,dir1)],load,.GlobalEnv)
        data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
        data_stackvalues_names = names(data_stackvalues)
        valid_values = valid[grep(product,valid$stack),]

        ScaleClean = function(x){
                x[x==as.numeric(valid_values$fill)]=NA
                x[x < as.numeric(valid_values$validL)]=NA
                x[x > as.numeric(valid_values$validU)]=NA
                # DONT SCALE THIS INCREASES DATA SIZE BY HUGE AMOUNT x = x * as.numeric(valid_values$scale)
                x}

        #  (can't do replacement inside of dopar loop)
        dir.create(file.path('../Data Stacks/WO Clouds Crops Clean/Rasters/'), showWarnings=F,recursive=T)
        junk = foreach(i=1:dim(data_stackvalues)[3]) %dopar% {
                 print(paste(product,tile,i))
                 writeRaster(ScaleClean(data_stackvalues[[i]]),
                        paste('WO Clouds Crops Clean/Rasters/','',sprintf("%04d", i),'_',product,'_',tile,
                                '_',names(data_stackvalues[[i]]),'.tif',sep=''),overwrite=T )
                 return(i) # return
                }
        data_stackvalues = stack(list.files('../Data Stacks/WO Clouds Crops Clean/Rasters/',
                pattern=paste('_',product,'_',tile,sep=''),full.names=T))
        names(data_stackvalues) = data_stackvalues_names
       # dont delete folnder unlink("../Data Stacks/WO Clouds Crops Clean/Rasters/", recursive = TRUE)

        assign(paste(product,'_stack_',tile,sep=''),data_stackvalues)
        dir.create(file.path('../Data Stacks/WO Clouds Crops Clean'), showWarnings=F,recursive=T)
        save(list=paste(product,'_stack_',tile,sep=''),
                file = paste('WO Clouds Crops Clean/',product,'_stack_',tile,'_wo_clouds_crops_clean.RData',sep=''))
        rm(list=ls()[grep(product,ls())])
  }}



