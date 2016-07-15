
# Michael Mann
# This script uses RCurl and ModisDownload to access an ftp server and download desired modis tiles

# Run the following in bash before starting R
 module load proj.4/4.8.0
 module load gdal/gcc/1.11
# module load R/3.0.2
 module load R
 module load gcc/4.9.0
 R



rm(list=ls())
#source('G:\\Faculty\\Mann\\Projects\\India_Index_Insurance\\India_Index_Insurance_Code\\ModisDownload.R')
#source('G:\\Faculty\\Mann/scripts/SplineAndOutlierRemoval.R')
source('/groups/manngroup/scripts/SplineAndOutlierRemoval.R')
source('/groups/manngroup/India_Index/India-Index-Insurance-Code/RasterChuckProcessing.R')


library(RCurl)
library(raster)
#library(MODISTools)
library(rgdal)
library(sp)
library(maptools)
#library(rts)
#library(gdalUtils)
library(foreach)
library(doParallel)
library(ggplot2)
library(compiler)
registerDoParallel(13)

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
  tiles =   c('h24v05','h24v06')  # uruguay c('h13v12')   # India example c('h24v05','h24v06'), Uruguay h13v12  
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-day') c('2002-07-04','2016-02-02') 
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'    # allData/6/ for evi, 
  # allData/51/ for landcover DOESn't WORK jUST PULL FROM FTP
  #strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
  out_dir = '/groups/manngroup/India_Index/Data/India/' #/groups/manngroup/India_Index/Data/Uruguay/
  setwd(out_dir)
  
 
  
  # Download MODIS data -----------------------------------------------------
  
  # find all available dates for each product
  available_date_list = list()
  available_products_list = list()
  for(product in products){
    available_date_list = c(available_date_list,list(as.character(strptime(gsub("^.*A([0-9]+).*$", "\\1",
            GetDates(location[1], location[2],product)),'%Y%j'))   ))
    available_products_list = c(available_products_list,list(rep(product,length(GetDates(location[1], location[2],product)))))
  }
  
  avail_files_df = data.frame(products=unlist(available_products_list),date=unlist(available_date_list),stringsAsFactors = F)
  avail_files_df$year = strftime(avail_files_df$date, format="%Y")
  avail_files_df$doy = strftime(avail_files_df$date, format="%j")
  avail_files_df$yeardoy = strftime(avail_files_df$date, format="%Y%j")
  avail_files_df
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
  files
  
  # find files not listed 
  missing_dates =  outersect(paste(files$products,files$dates,files$tiles,sep=' '), 
	apply(MARGIN=1,X=expand.grid(paste(needed_files_df$products,needed_files_df$date,sep=' '),tiles), 
	FUN=function(x){paste(x,collapse=' ')} )  )
  missing_dates
  # check dates in year,doy
  format(strptime('2010-05-25','%Y-%m-%d'),'%Y%j')
  

  
# Get Names of all Layers in HDF ------------------------------------------


  get_subdatasets('./MCD12Q1.A2002001.h24v05.051.2014287172414.hdf')
  
    
# Reproject ---------------------------------------------------------------

  
  band_subset = "1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0"  # Example: first seven and last layer'1 1 1 1 1 1 1 0 0 0 0 1" landcover= "1 1 0 0 1 1 1 0 0 1 1 0 0 0 0 0"
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
  setwd('/groups/manngroup/India_Index/Data/Uruguay')  # folder where  EVI .tifs are 
   
  # create data stack for each variable and tile 
  foreach(product =  c('blue_reflectance','MIR_reflectance','NIR_reflectance','red_reflectance',
	'EVI','NDVI','pixel_reliability')) %dopar% {  
  for( tile_2_process in tiles){
  	# Set up data
  	flist = list.files(".",glob2rx(paste('*',tile_2_process,'.250m_16_days_',product,'.tif$',sep='')), 
		full.names = TRUE)
        flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
  	flist = flist[order(flist_dates)]  # file list in order
        flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order

  	# stack data and save
  	stacked = stack(flist)
  	names(stacked) = flist_dates
  	assign(paste(product,'stack',tile_2_process,sep='_'),stacked)
  	save( list=paste(product,'stack',tile_2_process,sep='_') ,
		file = paste('../Data Stacks/Raw Stacks/',product,'_stack_',tile_2_process,'.RData',sep='') )
  }}


  # Stack land cover data NOTE: automatically fills missing years with most recent LC available
  setwd('/groups/manngroup/India_Index/Data/MODISLandCover/Uruguay')
  for(product in c('MCD12Q1')){
  for( tile in tiles){
        # Set up data
        flist = list.files(".",glob2rx(paste(product,'*',tile,'.Land_Cover_Type_2.tif$',sep='')),
                full.names = TRUE)
        flist_dates = gsub("^.*_([0-9]{7})_.*$", "\\1",flist,perl = T)  # Strip dates
        flist = flist[order(flist_dates)]  # file list in order
        flist_dates = flist_dates[order(flist_dates)]  # file_dates list in order
        #create duplicates of most recent year till end of study period
	studyperiod = format(seq(strptime(dates[1],'%Y-%m-%d'),strptime(dates[2],'%Y-%m-%d'), by='year'),'%Y%j') 
        missingyears = outersect(flist_dates, studyperiod)
	mostrecent = flist[length(flist)]
	flistfull = c(flist,rep(mostrecent,length(missingyears)))	
        # stack data and save
        stacked = stack(flistfull)
        names(stacked) = c(flist_dates,missingyears)
        assign(paste(product,'stack',tile,sep='_'),stacked)
        save( list=paste(product,'stack',tile,sep='_') ,
                file = paste('../../Data Stacks/LC Stacks/',product,'_stack_',tile,'.RData',sep='') )
  }}
  

# Limit stacks to common dates -------------------------------------------
  setwd('/groups/manngroup/India_Index/Data')

  # load data stacks from both directories
  stack_types_2_load =c('blue_reflectance', 'MIR_reflectance',
	'NIR_reflectance','red_reflectance','EVI','NDVI','pixel_reliability')
  dir1 = list.files('./Data Stacks/Raw Stacks/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)

  # limit stacks to common elements
  for(product in stack_types_2_load ){  
  for( tile in tiles){
	 # find dates that exist in all datasets for current tile
         all_dates = lapply(paste(stack_types_2_load,'stack',tile,sep='_'),function(x){names(get(x))})
	 # restrict to common dates 
	 common_dates = Reduce(intersect, all_dates)
	 # subset stacks for common dates  
	 assign(paste(product,'_stack_',tile,sep=''),subset( get(paste(product,'_stack_',tile,sep='')), 
		common_dates, drop=F) )
	 print('raster depth all equal')
	 print( all.equal(common_dates,names(get(paste(product,'_stack_',tile,sep=''))))   )
         print(dim(get(paste(product,'_stack_',tile,sep='')))[3])
  }}

  
  
# Remove low quality cells & assign projection ------------------------------------------------
  # load data in previous section and run common dates
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')

  reliability_prefix = 'pixel_reliability'
  products2removeclouds = c('blue_reflectance', 'MIR_reflectance',
        'NIR_reflectance','red_reflectance','EVI','NDVI')
  for(product in products2removeclouds){
  for( tile in tiles){
	print(paste('Working on',product,tile))
	# load quality flag
        reliability_stackvalues = get(paste(reliability_prefix,'_stack_',tile,sep=''))

	# remove clouds from produt
        data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
        crs(data_stackvalues) ='+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'

	foreach(i=1:dim(data_stackvalues)[3]) %dopar% { 
		data_stackvalues[[i]][reliability_stackvalues[[i]]!=0]=NA}
        assign(paste(product,'_stack_',tile,sep=''),data_stackvalues)
	save(list=paste(product,'_stack_',tile,sep=''),
		file = paste('WO Clouds/',product,'_stack_',tile,'_wo_clouds.RData',sep=''))
  }} 
  

# Remove non-agricultural lands ---------------------------------------------
# use MCD12Q1 landcover classification 2 (less exclusion of built up areas) 

  setwd('/groups/manngroup/India_Index/Data/Data Stacks')

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)
  dir2 = list.files('./LC Stacks/','.RData',full.names=T)
  lapply(dir2, load,.GlobalEnv)


  LandCover_product = 'MCD12Q1'
  products2removeLC = c('blue_reflectance', 'MIR_reflectance',
        'NIR_reflectance','red_reflectance','EVI','NDVI','pixel_reliability')
  for(product in products2removeLC){
  for( tile in tiles){
        print(paste('Working on',product,tile))
        # load land cover data
        LC_stackvalues = get(paste(LandCover_product,'_stack_',tile,sep=''))
	LC_dates = format(strptime( gsub("^.*X([0-9]+).*$", "\\1", names(LC_stackvalues)),format='%Y%j'),'%Y')
        # load product data 
        data_stackvalues = get(paste(product,'_stack_',tile,sep=''))
        data_dates = format(strptime( gsub("^.*X([0-9]+).*$", "\\1", names(data_stackvalues)),format='%Y%j'),'%Y')

        foreach(i=1:dim(data_stackvalues)[3]) %dopar% {
		# get the land cover data for the current product layer
		LC_value = subset(LC_stackvalues, seq(1,length(LC_dates))[LC_dates == data_dates[i]]) 
		# restrict to area with crops (code = 12) 
                data_stackvalues[[i]][LC_value!=12]=NA}
        # save data 
	assign(paste(product,'_stack_',tile,sep=''),data_stackvalues)
        save(list=paste(product,'_stack_',tile,sep=''),
                file = paste('./WO Clouds Crops/',product,'_stack_',tile,'_wo_clouds_crops.RData',sep=''))
  }}



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




# CHIRPS Rainfall Data ----------------------------------------------------

 #First data downloaded from ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_daily/tifs/p05/

 # ungz any files
 #files_gz =  list.files(path='/groups/manngroup/India_Index/Data/CHIRPS/', pattern="*.gz", full.names=T, recursive=T)
 #library(R.utils)

 #junk = foreach(file =files_gz, .inorder=F) %dopar%  {
 #	gunzip(file)}
	

 # crop and reproject
 setwd('/groups/manngroup/India_Index/Data/CHIRPS/')

 files =  list.files(path='/groups/manngroup/India_Index/Data/CHIRPS/', pattern="*.tif", full.names=T, recursive=T)
 files = files[-c(grep('cropped',files))] # remove any already processed

 EVI =  list.files(path='/groups/manngroup/India_Index/Data/Uruguay', pattern="*.tif", full.names=T, recursive=T)[1]
 if(tiles== "h13v12"){outer_extent = extent(raster(EVI))}
 h24v05 =raster('/groups/manngroup/India_Index/Data/MODISLandCover/India/MCD12Q1_2012001_h24v05.Land_Cover_Type_1.tif')
 h24v06 =raster('/groups/manngroup/India_Index/Data/MODISLandCover/India/MCD12Q1_2012001_h24v06.Land_Cover_Type_1.tif')
 if(identical(tiles,c('h24v05','h24v06'))){outer_extent = union(extent( h24v05),
    extent(h24v06))}

 junk = foreach(file =files, .inorder=F,.packages='raster') %dopar%  {
        out_file_name = paste(basename(substr(file,1,nchar(file)-4)),'_',paste(tiles,collapse='_'),'_cropped.tif',sep='')
        print(paste(file))
	#if(file.exists(out_file_name)){return(NA)}
 	example = raster(file)  # read in raster
        outer_extent = extent(projectExtent(raster(outer_extent,crs=
		CRS('+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs')), 
		projection(example) ))  #project extent to match EVI data
 	example =  crop(example, outer_extent) # crop to EVI data extent
	example[example==-9999]=NA   # remove missing data 
	# reproject raster
 	projectRaster(example,crs='+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs',
	     method="bilinear",  filename=out_file_name,overwrite=T) 
	return(NA)
 }


 # Stack rasters & save 
  files =  list.files(path='/groups/manngroup/India_Index/Data/CHIRPS/', 
	pattern=paste("*_",paste(tiles,collapse='_'),"_cropped.tif",sep=''), full.names=T, recursive=T)
  rain_stack = stack(files)
  LC_dates = strptime(   # strip dates
      gsub("^.*.([0-9]{4}){1}[.]([0-9]{2})[.]([0-9]{2}).*$",'\\1-\\2-\\3',files,perl = T) ,format='%Y-%m-%d')
  names(rain_stack) = LC_dates

  dirs = '/groups/manngroup/India_Index/Data/Data Stacks/Rain_Stacks/'
  dir.create(dirs)  
  save(rain_stack,file = paste(dirs,'Rain_Stack_',paste(tiles,collapse='_'),'.RData',sep=''))





#######################################################################################################

  
# Visualize examples of smoothed data -------------------------------------
  setwd('/groups/manngroup/India_Index/Data/India')
  
  load( paste('.//EVI_stack_','h24v05','_wo_clouds_crops.Rdata',sep='') )
  
  plot_dates = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(EVI_stack_h24v05)),format='%Y%j') # create dates to interpolate to
  pred_dates =  strptime(dates,'%Y-%m-%d') 
  
  
  EVI_v1 = getValues(NDVI_stack_h24v05, 1000, 1)
  EVI_v1[EVI_v1<=-2000]=NA
  EVI_v1=EVI_v1*0.0001
  dim(EVI_v1)
  
  row = 900  #500 100 is good
  plotdata = data.frame(EVI= EVI_v1[row,], 
                        dates =as.Date(strptime(plot_dates,'%Y-%m-%d')),class = 'EVI')
  
  plotdata = rbind(plotdata, data.frame(EVI = SplineAndOutlierRemoval(x = EVI_v1[row,], 
                        dates=dates, pred_dates=pred_dates,spline_spar = 0.2), 
                        dates =as.Date(strptime(plot_dates,'%Y-%m-%d')),class = 'EVI Smoothed'))

  # Get planting and harvest dates 
  PlantHarvestDates(dates[1],dates[2],PlantingMonth=10,PlantingDay=23,HarvestMonth=3,HarvestDay=10)

  # plot out time series with planting and harvest dates
  rects = data.frame(xstart = as.Date(planting), 
    xend = as.Date(harvest))
  
  ggplot()+geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
        ymin = -Inf, ymax = Inf), alpha = 0.4)+
	geom_point(data= plotdata, aes(x=dates,y=EVI,group=class,colour=class))
   

    
# plot out 5% of non-linear distribution --------------------------------------
  windows()
   q = quantile(plotdata$EVI,na.rm=T, probs = seq(0.05, .1, 0.25))
   a=ggplot(plotdata, aes(EVI)) + geom_histogram(colour='blue',fill='blue',alpha=.3)+ 
         geom_vline(xintercept = q,size=2)+labs(title = "Distribution")
  
   b=ggplot(plotdata, aes(EVI)) + stat_ecdf(geom = "step",colour='blue',size=1.5)+
     geom_vline(xintercept = q,size=2)+geom_hline(yintercept = 0.05,size=2)+
     labs(title = "Cumulative Distribution")
   multiplot(a,b, cols=2)
  
  
  



#       chunk_size = round(dim(data_stackvalues)[3]/4)
#        junk = foreach(i=1:chunk_size) %dopar% {
#                 return(ScaleClean(data_stackvalues[[i]]) ) # return
#                }
#        for(i in 1:chunk_size){data_stackvalues[[i]] = junk[[i]]}
#       rm(junk)
#        gc()
#        junk = foreach(i=(chunk_size+1):(chunk_size*2)) %dopar% {
#                 return(ScaleClean(data_stackvalues[[i]]) ) # return
#                }
#        for(i in (chunk_size+1):(chunk_size*2) ){data_stackvalues[[i]] = junk[[i]]}
#        rm(junk)
#
#        junk = foreach(i=(chunk_size*2+1):(chunk_size*3)) %dopar% {
#                 return(ScaleClean(data_stackvalues[[i]]) ) # return
#                }
#        for(i in (chunk_size*2+1):(chunk_size*3) ){data_stackvalues[[i]] = junk[[i]]}
#        rm(junk)
#
#        junk = foreach(i=(chunk_size*3+1):dim(data_stackvalues)[3]) %dopar% {
#                 return(ScaleClean(data_stackvalues[[i]]) ) # return
#                }
#        for(i in (chunk_size*3+1):dim(data_stackvalues)[3]) ){data_stackvalues[[i]] = junk[[i]]}
#        rm(junk)

