# Michael Mann
# This script uses RCurl and ModisDownload to access an ftp server and download desired modis tiles

# Run the following in bash before starting R
# module load proj.4/4.8.0
# module load gdal/gcc/1.11
# module load R/3.0.2
# module load gcc/4.9.0
# R



rm(list=ls())
source('G:\\Faculty\\Mann\\Projects\\India_Index_Insurance\\India_Index_Insurance_Code\\ModisDownload.R')
source('G:\\Faculty\\Mann/scripts/SplineAndOutlierRemoval.R')

source('/groups/manngroup/scripts/SplineAndOutlierRemoval.R')
library(RCurl)
library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(maptools)
library(rts)
library(gdalUtils)
library(foreach)
library(doParallel)
#registerDoParallel(5)



# Functions ---------------------------------------------------------------
  
  
  multi_grep_character <- function(find, inthis){ #returns location of multiple "find' elements in the vector 'inthis'
    if(class(inthis)!= "character"){break("Error: in this must be a character vector")}
    return(unlist(lapply(1:length(find), function(x) {grep(find[x],inthis)}   )))
  }
  
  outersect <- function(x, y) {
    sort(c(x[!x%in%y],
           y[!y%in%x]))
  }


  

# Set up parameters -------------------------------------------------------

  # give path to Modis Reproduction Tool
  MRT = 'G:/Faculty/Mann/Projects/MRT/bin'
  
  # get list of all available modis products
  GetProducts()
  
  # Product Filters 
  products = c('MYD13Q1','MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover = 'MCD12Q1'
  location = c(30.259,75.644)  # Lat Lon of a location of interest within your tiles listed above #India c(-31.467934,-57.101319)  #
  tiles =   c('h24v05','h24v06')   # India example c('h13v12')
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02') 
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/5/'    # allData/6/ for evi, allData/5/ for landcover 
  strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
  out_dir = 'G:/Faculty/Mann/Projects/India_Index_Insurance/Data/India/'
  setwd(out_dir)
  
  
# Download MODIS data -----------------------------------------------------
  

  
  # find all available dates for each product
  available_date_list = list()
  available_products_list = list()
  for(product in products){
    available_date_list = c(available_date_list,list(as.character(strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],product)),'%Y%j'))   ))
    available_products_list = c(available_products_list,list(rep(product,length(GetDates(location[1], location[2],product)))))
  }
  
  avail_files_df = data.frame(products=unlist(available_products_list),date=unlist(available_date_list),stringsAsFactors = F)
  avail_files_df$year = strftime(avail_files_df$date, format="%Y")
  avail_files_df$doy = strftime(avail_files_df$date, format="%j")
  avail_files_df$yeardoy = strftime(avail_files_df$date, format="%Y%j")
  
  head(avail_files_df)
  dim(avail_files_df)
  
  # list all files we need to download
  needed_files_df = avail_files_df[ avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days'))  ,] # limit available files to needed date range
  not_needed_files_df = avail_files_df[ !(avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days')) ) ,] # limit available files to needed date range
  head(needed_files_df)
  dim(needed_files_df)
  
  # find all urls for download
  urls = paste(ftp, needed_files_df$products,'/',needed_files_df$year, "/", needed_files_df$doy, "/",sep='')
  junk= foreach(j = 1:length(urls),.packages = 'RCurl') %dopar% {
  #for(j in 1:length(urls)){
      url=urls[j]
      # get urls and limit to wanted tiles
      Sys.sleep(1)
      filenames_url = tryCatch({getURL(url, ftp.use.epsv = F, dirlistonly = T)}, error = function(err) {
                 # getURL fails if you make too many queries, slow down using system pause              
                  print(paste("Your server is pathetic, pausing for 60 seconds: ",err))
                  Sys.sleep(60)
                    tryCatch({getURL(url, ftp.use.epsv = F, dirlistonly = T)}, error = function(err) {
                      # getURL fails if you make too many queries, slow down using system pause              
                      print(paste("Your server is really pathetic, pausing for 60 seconds: ",err))
                      Sys.sleep(60)
                      getURL(url, ftp.use.epsv = F, dirlistonly = T)
                    })
                  })
      filenames_url = paste(url, strsplit(filenames_url, "\r*\n")[[1]], sep = "")
      filenames_url = filenames_url[multi_grep_character(tiles,filenames_url)] # find needed files based on tiles
       
      # get file names from available urls
      write_names=unlist(lapply(1:length(strsplit(filenames_url,'/')),function(x){strsplit(filenames_url,'/')[[x]][length(strsplit(filenames_url,'/')[[x]])]}))
      
      for(i in 1:length(filenames_url)){
          # download as binary and save
          print(write_names[i])
          if(file.exists(paste(out_dir,'/',write_names[i],sep=''))==F){
              print('writing hdf file')
              bin = getBinaryURL(filenames_url[i])
              writeBin(bin, paste(out_dir,'/',write_names[i],sep='')) 
          }else{
              print('Skipping file already exists')
              next
          }
      }
  }
  
  
  
# Find any missing files and download -------------------------------------
  
  # list all files
  files = data.frame(files=list.files(out_dir,pattern=".hdf", all.files=T, full.names=T),stringsAsFactors = F)
  files$short_name =  list.files(out_dir,pattern=".hdf", all.files=T, full.names=F)
  
  # list dates of files downloaded 
  files$dates    = as.character(strptime(gsub("^.*A([0-9]+).*$", "\\1",files$files),'%Y%j'))  # Strip dates
  files$products = gsub(paste("^.*(",paste(products,collapse='|'),").*$",sep = ''), "\\1",files$files,perl=T) # strip products
  files$tiles    = gsub(paste("^.*(",paste(tiles,collapse='|'),").*$",sep = ''), "\\1",files$files,perl=T) # strip products
  files$yeardoy  = strftime(files$dates, format="%Y%j")
  files$reproj_files = paste(gsub("[.]006.*.hdf$", "\\1",files$short_name,perl=T) )
  
  # find files not listed 
  missing_dates =  outersect(paste(files$products,files$dates,files$tiles,sep=' '), apply(MARGIN=1,X=expand.grid(paste(needed_files_df$products,needed_files_df$date,sep=' '),tiles), FUN=function(x){paste(x,collapse=' ')} )  )
  missing_dates
  # check dates in year,doy
  format(strptime('2010-05-25','%Y-%m-%d'),'%Y%j')
  

  
  
# Get Names of all Layers in HDF ------------------------------------------


  get_subdatasets('./MCD12Q1.A2003001.h24v05.005.2011061231359.hdf')
  
  
  
# Reproject ---------------------------------------------------------------
 
  
  band_subset = "1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0"  # Example: first seven and last layer'1 1 1 1 1 1 1 0 0 0 0 1"
  output_pattern = 'Land_Cover_Type_1.tif' # '250m_16_days_EVI.tif' looks for existing EVI tif files to avoid repeating
   
  for (i in (1:length(files$reproj_files))){
      print(i)
      print(paste(i,'out of',length(files$reproj_files)))
      print(paste("Writing out tiffs ", list.files('.',pattern =  files$reproj_files[i] ),' for date ',files$yeardoy[i]))
      tifs = list.files(getwd(),pattern =  output_pattern)
      
      if(length(tifs[grep(tifs,pattern=paste(files$products[i],files$yeardoy[i],files$tiles[i],sep='_'))])>=1){ print('File exists')
          next
      }else{
          print(paste('Input:',files$reproj_files[i],' Output:',paste(files$products[i],'_',files$yeardoy[i],'.tif',sep='')))
          reprojectHDF(hdfName = files$short_name[i],
                     filename=paste(files$products[i],'_',files$yeardoy[i],'_',files$tiles[i],'.tif',sep=''),  
                     MRTpath=MRT, proj_type='SIN', 
                     proj_params='6371007.181 0 0 0', 
                     datum='NODATUM', pixel_size=250,
                     bands_subset=band_subset)
        }
  }
  

  
#   MODIS SINUSOIDAL PROJECITON DETAILS:
#  This is what I use in gdal:
#     uly_map = 10007554.677
#   ulx_map = -20015109.354
#   pix = 463.312716525
#   proj4 = '+proj=sinu +a=6371007.181 +b=6371007.181 +units=m' 

  

# Stack relevant data -----------------------------------------------------
  setwd('/groups/manngroup/India_Index/Data/India')

  # EVI
  tile_2_process = 'h24v06'
  # Set up data
  flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_EVI.tif$',sep='')), full.names = TRUE)
  flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  flist = flist[order(flist_dates)]  # file list in order
  # stack data and save
  EVI_stack = stack(flist)
  names(EVI_stack) = flist_dates
  save(EVI_stack,file = paste('.//EVI_stack',tile_2_process,'.RData',sep='') )
  
  tile_2_process = 'h24v05'
  # Set up data
  flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_EVI.tif$',sep='')), full.names = TRUE)
  flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  flist=flist[order(flist_dates)]  # file list in order
  # stack data and save
  EVI_stack = stack(flist)
  names(EVI_stack) = flist_dates
  save(EVI_stack,file = paste('.//EVI_stack',tile_2_process,'.RData',sep='') )
  
  # NDVI
  tile_2_process = 'h24v06'
  # Set up data
  flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_NDVI.tif$',sep='')), full.names = TRUE)
  flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  flist = flist[order(flist_dates)]  # file list in order
  # stack data and save
  NDVI_stack = stack(flist)
  names(NDVI_stack) = flist_dates
  save(NDVI_stack,file = paste('.//NDVI_stack',tile_2_process,'.RData',sep='') )
  
  tile_2_process = 'h24v05'
  # Set up data
  flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_NDVI.tif$',sep='')), full.names = TRUE)
  flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  flist=flist[order(flist_dates)]  # file list in order
  # stack data and save
  NDVI_stack = stack(flist)
  names(NDVI_stack) = flist_dates
  save(NDVI_stack,file = paste('.//NDVI_stack',tile_2_process,'.RData',sep='') )
  
  # Quality flag
  tile_2_process = 'h24v06'
  # Set up data
  flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_pixel_reliability.tif$',sep='')), full.names = TRUE)
  flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  flist = flist[order(flist_dates)]  # file list in order
  # stack data and save
  Reliability_stack = stack(flist)
  names(Reliability_stack) = flist_dates
  save(Reliability_stack,file = paste('.//PixelReliability_stack',tile_2_process,'.RData',sep='') )
  
  tile_2_process = 'h24v05'
  # Set up data
  flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_pixel_reliability.tif$',sep='')), full.names = TRUE)
  flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  flist=flist[order(flist_dates)]  # file list in order
  # stack data and save
  Reliability_stack = stack(flist)
  names(Reliability_stack) = flist_dates
  save(Reliability_stack,file = paste('.//PixelReliability_stack',tile_2_process,'.RData',sep='') )
  
  
  
# Remove low quality cells ------------------------------------------------
  
 for( tile_2_process in c( 'h24v05','h24v06'){

  	reliability_stackname = load(paste('.//PixelReliability_stack',tile_2_process,'.RData',sep=''))
 	reliability_stackvalues = get(reliability_stackname)
	
	# remove clouds from NDVI
	data_stackname = load(paste('.//NDVI_stack',tile_2_process,'.RData',sep=''))  
  	data_stackvalues = get(data_stackname)

	foreach(i=1:dim(data_stackvalues)[3]) %dopar% { data_stackvalues[[i]][reliability_stackvalues[[i]]>0]=NA}
	save(,file = paste(,'_wo_cld.RData',sep=''))


} 
  
  
  
  
# Visualize examples of smoothed data -------------------------------------
  
  load( paste('.//EVI_stack',tile_2_process,'.RData',sep='') )
  
  dates = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(EVI_stack)),format='%Y%j') # create dates to interpolate to
  pred_dates =  strptime(dates,'%Y-%m-%d') 
  
  
  EVI_v1 = getValues(EVI_stack, 1000, 1)
  EVI_v1[EVI_v1<0]=NA
  EVI_v1=EVI_v1*0.0001
  dim(EVI_v1)
  
  row = 500  # 100 is good
  plotdata = data.frame(EVI= EVI_v1[row,], 
                        dates =as.Date(strptime(dates,'%Y-%m-%d')),class = 'EVI')
  
  plotdata = rbind(plotdata, data.frame(EVI = SplineAndOutlierRemoval(x = EVI_v1[row,], 
                        dates=dates, pred_dates=pred_dates,spline_spar = 0.3), 
                        dates =as.Date(strptime(dates,'%Y-%m-%d')),class = 'EVI Smoothed'))
  
  
  ggplot(plotdata, aes(x=dates,y=EVI,group=class))+geom_point(aes(colour=class))
   
    
  # plot out 5% of non-linear distribution 
  windows()
   q = quantile(plotdata$EVI,na.rm=T, probs = seq(0.05, .1, 0.25))
   a=ggplot(plotdata, aes(EVI)) + geom_histogram(colour='blue',fill='blue',alpha=.3)+ 
         geom_vline(xintercept = q,size=2)+labs(title = "Distribution")
  
   b=ggplot(plotdata, aes(EVI)) + stat_ecdf(geom = "step",colour='blue',size=1.5)+
     geom_vline(xintercept = q,size=2)+geom_hline(yintercept = 0.05,size=2)+
     labs(title = "Cumulative Distribution")
   multiplot(a,b, cols=2)
  
  
  
