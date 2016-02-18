# This script downloads and does preprocessing of vegetation index data at 250m resolution


# From: http://r-gis.net/?q=ModisDownload
#In order to use ModisDownload, you first need:
#Download and install MRT on your machine. Keep in mind, after installing MRT, you need to restart your machine. (If you do not need to mosaic, convert, and reproject the downloaded images, you do not need MRT!).
#Download the ModisDownload script version 3.0, ( ModisDownload.R ) as well as ModisLP.RData and put them in your working directory (or alternatively, you can read it from this website; see example!).


source('G:\\Faculty\\Mann\\Projects\\India Index Insurance\\India Index Insurance Code/ModisDownload.R')
library(RCurl)
library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(doParallel)
library(maptools)
library(rts)

source('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//250m_EVI_Data//SplineAndOutlierRemoval.R')

setwd('G:/Faculty/Mann/Projects/India Index Insurance/Data')

# give path to Modis Reproduction Tool
MRT = 'G:\\Faculty\\Mann\\Projects\\India Index Insurance\\Data\\MRT\\bin'

# View Availability -------------------------------------------------------

  
  ## find dates available for two images in 2000-2013
  
  GetProducts()
  MOD13Q1_dates = GetDates(Lat=30.259, Long=75.644,'MOD13Q1')
  MYD13Q1_dates = GetDates(Lat=30.259, Long=75.644,'MYD13Q1')



# Download Images ---------------------------------------------------------

  
  # Download images
  modisProducts()
   
  # Product IDs
  x='MOD13Q1'
  y='MYD13Q1'
  # Enter tile numbers
  h=c(24,24)
  v=c(5,6)
  
  # Download all MOD13Q1
  ModisDownload(x=x,h=h,v=v,dates=c('2000.03.01', '2016.02.01'), 
                MRTpath=MRT, mosaic=F, proj=F)
  
  ModisDownload(x=y,h=h,v=v,dates=c('2000.03.01', '2016.02.01'), 
                MRTpath=MRT, mosaic=F, proj=F)

  
  
# Download Any Missing  ------------------------------------------------------------------

  # list all files
  files = list.files(pattern=".hdf", all.files=T, full.names=T)
  MOD_files = files[grepl('MOD',files)]
  MYD_files = files[grepl('MYD',files)]
  
  # list dates of files downloaded 
  MOD_files_dates = gsub("^.*A([0-9]+).*$", "\\1",MOD_files)  # Strip dates
  MYD_files_dates = gsub("^.*A([0-9]+).*$", "\\1",MYD_files)   
  
  # list dates of files available 
  MOD_dates = gsub("^.*A([0-9]+).*$", "\\1",MOD13Q1_dates)   
  MYD_dates = gsub("^.*A([0-9]+).*$", "\\1",MYD13Q1_dates)   
  
  outersect <- function(x, y) {
    sort(c(x[!x%in%y],
           y[!y%in%x]))
  }
  
  # Find missing files and convert dates to usable format 
  MOD_missing_dates = format(strptime(outersect(MOD_dates,MOD_files_dates) ,format='%Y%j'), "%Y.%m.%d")
  MYD_missing_dates = format(strptime(outersect(MYD_dates,MYD_files_dates) ,format='%Y%j'), "%Y.%m.%d")
  
  
  # Download all missing files in loop
  for(date in MOD_missing_dates){
    ModisDownload(x=x,h=h,v=v,dates=date, 
                  MRTpath=MRT, mosaic=F, proj=F)
  }
  
  for(date in MYD_missing_dates){
    ModisDownload(x=y,h=h,v=v,dates=date, 
                  MRTpath=MRT, mosaic=F, proj=F)
  }


  
  
  
  
  
  

# Mosaic ------------------------------------------------------------------
  
  
# find dates available
  
GetProducts()
dates_MOD=GetDates(Lat=30.259, Long=75.644,'MOD13Q1')
dates_MOD
dates_MYD=GetDates(Lat=30.259, Long=75.644,'MYD13Q1')
dates_MYD
 

# Find all files and mosaic
files <- list.files(pattern=".hdf", all.files=T, full.names=T)
 

# Call MRT to mosaic HDFS
for (i in (1:length(dates_MOD))){
  print(i)
  print(length(files[grep(pattern=files[i],files)]))
  mosaicHDF(files[grep(pattern=files[i],files)], filename=paste(x,'.',files[i],'.hdf',sep=''), 
            MRTpath=MRT, delete=T)
}

for (i in (1:length(dates_MYD))){
  print(i)
  print(length(files[grep(pattern=dates_MYD[i],files)]))
  mosaicHDF(files[grep(pattern=dates_MYD[i],files)], filename=paste(y,'.',dates_MYD[i],'.hdf',sep=''), 
            MRTpath=MRT, delete=T)
}




































# Emmalees Code -----------------------------------------------------------



### MODIS Tile Download
# Emmalee Dolfi
# June 2014


## Install MODIS Reprojection Tool onto computer 
## http://stevemosher.wordpress.com/modis-reprojection-tool/
## Install it in working directory (easiest)

## Move ModisDownload.R and ModisLP.RData into working directory

setwd('G:/Graduate/EDolfi/Summer2014/MODIS')

source('ModisDownload.R')
install.packages('RCurl')
library(RCurl)
library(raster)

install.packages('MODISTools')
install.packages('rgdal')
library(MODISTools)
library(rgdal)

x='MOD13Q1'    #250m 8 day products vegetation
y='MYD13Q1'    # quality flags

## Download all 4 tiles over all 4 years for MOD13Q1
ModisDownload(x=x,h=h,v=v,dates=c('2000.03.01', '2016.02.01'), 
              MRTpath='G:\\Faculty\\Mann\\Projects\\India Index Insurance\\Data\\MRT\\bin', mosaic=F, proj=F)

ModisDownload(x=y,h=h,v=v,dates=c('2000.03.01', '2016.02.01'), 
              MRTpath='G:\\Faculty\\Mann\\Projects\\India Index Insurance\\Data\\MRT\\bin', mosaic=F, proj=F)

 

# Mosaic ------------------------------------------------------------------



## find dates available for two images in 2000-2013

GetProducts()
dates_MOD=GetDates(13, 37,'MOD13Q1')

dates_MOD
dates_MYD=GetDates(13, 37,'MYD13Q1')

MOD_10_13=dates_MOD[228:317]
MYD_10_13=dates_MYD[174:263]

## missing some dates that are available 

MOD_10_13=c(MOD_10_13, 'A2013209', 'A2013225')
MOD_10_13=sort(MOD_10_13)
MYD_10_13=c(MYD_10_13, 'A2013217', 'A2013233')
MYD_10_13=sort(MYD_10_13)

MOD_10_13
MYD_10_13

files <- list.files(pattern=".hdf", all.files=T, full.names=T)

files[grep(pattern=MOD_10_13[1],files)]

paste(x,'.',MOD_10_13[1],'.hdf',sep='')



for (i in (1:length(MOD_10_13))){
  print(i)
  print(length(files[grep(pattern=MOD_10_13[i],files)]))
  mosaicHDF(files[grep(pattern=MOD_10_13[i],files)], filename=paste(x,'.',MOD_10_13[i],'.hdf',sep=''), 
            MRTpath="G:/Graduate/EDolfi/Summer2014/MODIS/MRT/bin", delete=T)
}

for (i in (1:length(MYD_10_13))){
  print(i)
  print(length(files[grep(pattern=MYD_10_13[i],files)]))
  mosaicHDF(files[grep(pattern=MYD_10_13[i],files)], filename=paste(y,'.',MYD_10_13[i],'.hdf',sep=''), 
            MRTpath="G:/Graduate/EDolfi/Summer2014/MODIS/MRT/bin", delete=T)
}


# Reproject ---------------------------------------------------------------

reprojectHDF(files2[grep(pattern=MOD_10_13[1],files2)], filename=paste(x,'.',MOD_10_13[1],'.tif',sep=''), 
             MRTpath="G:/Graduate/EDolfi/Summer2014/MODIS/MRT/bin", proj_type='SIN', 
             proj_params='6371007.181 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0', datum='NODATUM', pixel_size=250)


files2=list.files(pattern='.hdf')
files2

files2[grep(pattern=MOD_10_13[1],files2)]

# run reproject into original projection to extract individual layers

for (i in (1:length(MOD_10_13))){
  print(i)
  print(length(files2[grep(pattern=MOD_10_13[i],files2)]))
  reprojectHDF(files2[grep(pattern=MOD_10_13[i],files2)], filename=paste(x,'.',MOD_10_13[i],'.tif',sep=''), 
               MRTpath="G:/Graduate/EDolfi/Summer2014/MODIS/MRT/bin", proj_type='SIN', 
               proj_params='6371007.181 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0', datum='NODATUM', pixel_size=250)
}


for (i in (1:length(MYD_10_13))){
  print(i)
  print(length(files2[grep(pattern=MYD_10_13[i],files2)]))
  reprojectHDF(files2[grep(pattern=MYD_10_13[i],files2)], filename=paste(y,'.',MYD_10_13[i],'.tif',sep=''), 
               MRTpath="G:/Graduate/EDolfi/Summer2014/MODIS/MRT/bin", proj_type='SIN', 
               proj_params='6371007.181 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0', datum='NODATUM', pixel_size=250)
}




# Raster - Reproject ------------------------------------------------------


files_tif<-list.files(pattern='.tif$')

GetBands('MOD13Q1')

# isolate VI_quality, pixel, EVI, NDVI
MOD13Q1_bands<-GetBands('MOD13Q1')[c(4,9,10,11)]
MYD13Q1_bands<-GetBands('MYD13Q1')[c(4,9,10,11)]

#just NDVI, EVI
MOD13Q1_EVI_NDVI<-GetBands('MOD13Q1')[c(10,11)]
MYD13Q1_EVI_NDVI<-GetBands('MYD13Q1')[c(10,11)]

MOD13Q1_EVI_NDVI
MYD13Q1_EVI_NDVI

MYD_10_13

files_tif[grep(pattern=paste(MYD13Q1_bands[3],'.project', sep=''),files_tif)]

filename=paste(x,'.', MOD_10_13[1],'.', MOD13Q1_bands[3], '.tif', sep='')


Ethiopia<-shapefile('G:/Graduate/EDolfi/Summer2014/Ethiopia.shp')
ETH_proj<-spTransform(Ethiopia, CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'))


for (i in (1:length(MOD_10_13))){
  Pixel=raster(paste(x,'.', MOD_10_13[i],'.', MOD13Q1_bands[1], '.tif',sep=''))
  for (j in (1:length(MOD13Q1_EVI_NDVI))){
    Raster1=raster(paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.tif',sep=''))
    Raster1[Pixel!=0 & Pixel!=1]=NA
    Raster2=crop(Raster1, ETH_proj)
    Raster_project=projectRaster(Raster2, crs='+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs', method='bilinear')
    writeRaster(Raster_project, paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.project','.tif',sep=''), overwrite=T)
  }
}


for (i in (1:length(MYD_10_13))){
  Pixel=raster(paste(y,'.', MYD_10_13[63],'.', MYD13Q1_bands[1], '.tif',sep=''))
  for (j in (1:length(MYD13Q1_EVI_NDVI))){
    Raster1=raster(paste(y,'.', MYD_10_13[i],'.', MYD13Q1_EVI_NDVI[j], '.tif',sep=''))
    Raster1[Pixel!=0 & Pixel!=1]=NA
    Raster2=crop(Raster1, ETH_proj)
    Raster_project=projectRaster(Raster2, crs='+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs', method='bilinear')
    writeRaster(Raster_project, paste(y,'.', MYD_10_13[i],'.', MYD13Q1_EVI_NDVI[j], '.project','.tif',sep=''), overwrite=T)
  }
}



# doesn't matter
#
#
#
#
# MYD_10_13
# 
# ## crop images that were reprojected before crop in for loop
# 
# 
# # MOD
# 
# for (i in (1:31)){
#   for (j in (1:length(MOD13Q1_EVI_NDVI))){
#     Raster1=raster((paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.project','.tif',sep='')))
#     Raster2=crop(Raster1, ETH_proj)
#     writeRaster(Raster2, paste(x,'.', MOD_10_13[i],'.', MOD13Q1_EVI_NDVI[j], '.project','.tif',sep=''), overwrite=T)
#   }
# }
# 
# # MYD
# for (i in (1:24)){
#   for (j in (1:length(MYD13Q1_EVI_NDVI))){
#   Raster1=raster((paste(y,'.', MYD_10_13[i],'.', MYD13Q1_EVI_NDVI[j], '.project','.tif',sep='')))
#   Raster2=crop(Raster1, ETH_proj)
#   writeRaster(Raster2, paste(y,'.', MYD_10_13[i],'.', MYD13Q1_EVI_NDVI[j], '.project','.tif',sep=''), overwrite=T)
#   }
# }
# 
# #for (i in (14:18)){
#   for (j in (1:length(MYD13Q1_EVI_NDVI))){
#     Raster1=raster((paste(y,'.', MYD_10_13[12],'.', MYD13Q1_EVI_NDVI[j],'.tif',sep='')))
#     Raster2=crop(Raster1, ETH_proj)
#     Raster_Project=projectRaster(Raster2, crs='+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs', method='bilinear')
#     writeRaster(Raster_Project, paste(y,'.', MYD_10_13[12],'.', MYD13Q1_EVI_NDVI[j], '.project','.tif',sep=''), overwrite=T)
#   }
# #}
# 
# 

# # Stack -------------------------------------------------------------------
# 
# ## list files for stacking
# ## stacked by product, layer and date 
# files_EVI=list.files(pattern=('EVI.project.tif$'))
# EVI_MOD13Q1=files_EVI[grep('MOD13Q1', files_EVI)]
# EVI_MOD13Q1
# 
# files_NDVI=list.files(pattern=('NDVI.project.tif$'))
# NDVI_MOD13Q1=files_NDVI[grep('MOD13Q1', files_NDVI)]
# NDVI_MOD13Q1
# 
# EVI_MYD13Q1=files_EVI[grep('MYD13Q1', files_EVI)]
# EVI_MYD13Q1
# 
# NDVI_MYD13Q1=files_NDVI[grep('MYD13Q1', files_NDVI)]
# NDVI_MYD13Q1
# 
# 
# 
# EVI_MOD13Q1_stack<-stack()
# for (i in (1:length(MOD_10_13))){
#   s=(EVI_MOD13Q1[grep(pattern=c(MOD_10_13[i]),EVI_MOD13Q1)])
#   EVI_MOD13Q1_stack=stack(EVI_MOD13Q1_stack,s)
# }
# 
# names(EVI_MOD13Q1_stack)=MOD_10_13
# 
# NDVI_MOD13Q1_stack<-stack()
# for (i in (1:length(MOD_10_13))){
#   s=(NDVI_MOD13Q1[grep(pattern=c(MOD_10_13[i]),NDVI_MOD13Q1)])
#   NDVI_MOD13Q1_stack=stack(NDVI_MOD13Q1_stack,s)
# }
# 
# names(NDVI_MOD13Q1_stack)=MOD_10_13
# 
# EVI_MYD13Q1_stack<-stack()
# for (i in (1:length(MYD_10_13))){
#   s=(EVI_MYD13Q1[grep(pattern=c(MYD_10_13[i]),EVI_MYD13Q1)])
#   EVI_MYD13Q1_stack=stack(EVI_MYD13Q1_stack,s)
# }
# 
# names(EVI_MYD13Q1_stack)=MYD_10_13
# 
# NDVI_MYD13Q1_stack<-stack()
# for (i in (1:length(MYD_10_13))){
#   s=(NDVI_MYD13Q1[grep(pattern=c(MYD_10_13[i]),NDVI_MYD13Q1)])
#   NDVI_MYD13Q1_stack=stack(NDVI_MYD13Q1_stack,s)
# }
# 
# names(NDVI_MYD13Q1_stack)=MYD_10_13
# 
# NDVI_MYD13Q1_stack
# 
# plot(NDVI_MYD13Q1_stack[[10]])
# 
# ??approxNA
# ??sampleRaster
# 
# EVI_MOD13Q1_fill<-approxNA(EVI_MOD13Q1_stack, method='linear', rule=2)
# NDVI_MOD13Q1_fill<-approxNA(NDVI_MOD13Q1_stack, method='linear', rule=2)
# EVI_MYD13Q1_fill<-approxNA(EVI_MYD13Q1_stack, method='linear', rule=2)
# NDVI_MYD13Q1_fill<-approxNA(NDVI_MYD13Q1_stack, method='linear', rule=2)
# 
# randEVI_MOD13Q1=sampleRandom(EVI_MOD13Q1_fill, size=20)
# randNDVI_MOD13Q1=sampleRandom(NDVI_MOD13Q1_fill, size=20)
# randNDVI_MOD13Q1<-t(randNDVI_MOD13Q1)
# randEVI_MYD13Q1=sampleRandom(EVI_MYD13Q1_fill, size=20)
# randEVI_MYD13Q1<-t(randEVI_MYD13Q1)
# randNDVI_MYD13Q1=sampleRandom(NDVI_MYD13Q1_fill, size=20)
# randNDVI_MYD13Q1<-t(randNDVI_MYD13Q1)
# 
# warnings()
# 
# randEVI_MOD13Q1=t(randEVI_MOD13Q1)
# colnames(randEVI_MOD13Q1)<-c(1:20)
# names(randEVI_MOD13Q1[,21])<-'date'
# rownames(randEVI_MOD13Q1)<-1:92
# randEVI_MOD13Q1$date<-MOD_10_13
# randEVI_MOD13Q1<-data.frame(randEVI_MOD13Q1)
# View(randNDVI_MYD13Q1)
# 
# randEVI_MOD13Q1[,21]<-MOD_10_13
# names(randEVI_MOD13Q1[,21])<-'MOD_10_13'
# head(randEVI_MOD13Q1)
# 
# ## if you want to run it
# randEVI_MOD13Q1<-read.table('randEVI_MOD13Q1.csv')
# df2=data.frame(randEVI_MOD13Q1)
# 
# 
# names(df)[21]<-'date'
# names(df2)[1:92]<-MOD_10_13
# df2$ID<-1:20
# head(df2)
# 
# df2_melt<-melt(df2, id.var='ID')
# head(df2_melt)
# 
# 
# ggplot(randEVI_MOD13Q1, aes(randEVI_MOD13Q1[,21]))+geom_line(aes(y=randEVI_MOD13Q1[,1], color=1))
# 
# ggplot(randEVI_MOD13Q1, aes(randEVI_MOD13Q1[,21], group=randEVI_MOD13Q1[,21]))+geom_line()



