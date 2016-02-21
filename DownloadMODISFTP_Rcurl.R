# Michael Mann
# This script uses RCurl and ModisDownload to access an ftp server and download desired modis tiles


rm(list=ls())
source('G:\\Faculty\\Mann\\Projects\\India Index Insurance\\India Index Insurance Code/ModisDownload.R')
library(RCurl)
library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(maptools)
library(rts)
source('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//250m_EVI_Data//SplineAndOutlierRemoval.R')


library(foreach)
library(doParallel)
registerDoParallel(5)


MAYBE SOMETHING WRONG  WITH available_date_list


# Functions ---------------------------------------------------------------
  
  # give path to Modis Reproduction Tool
  MRT = 'G:\\Faculty\\Mann\\Projects\\India Index Insurance\\Data\\MRT\\bin'
  
  multi_grep_character <- function(find, inthis){ #returns location of multiple "find' elements in the vector 'inthis'
    if(class(inthis)!= "character"){break("Error: in this must be a character vector")}
    return(unlist(lapply(1:length(find), function(x) {grep(find[x],inthis)}   )))
  }
  
  outersect <- function(x, y) {
    sort(c(x[!x%in%y],
           y[!y%in%x]))
  }



# Download MODIS data -----------------------------------------------------
  
  
  # get list of all available modis products
  GetProducts()
  
  # Product Filters 
  products = c('MYD13Q1','MOD13Q1')
  location = c(30.259,75.644) # Lat Lon of a location of interest within your tiles listed above
  tiles =  c('h24v05','h24v06')   #example c('h24v05','h24v06')
  dates = c('2002-07-04','2016-02-02') # example c('year-month-day',year-month-day')
  ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'
  out_dir = 'G:/Faculty/Mann/Projects/India Index Insurance/Data/MYD13Q1'
  strptime(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],products[1])),'%Y%j') # get list of all available dates for products[1]
   
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
  head(avail_files_df)
  dim(avail_files_df)
  
  # list all files we need to download
  needed_files_df = avail_files_df[ avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days'))  ,] # limit available files to needed date range
  not_needed_files_df = avail_files_df[ !(avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days')) ) ,] # limit available files to needed date range
  head(needed_files_df)
  dim(needed_files_df)
  
  # find all urls for download
  urls = paste(ftp, needed_files_df$products,'/',needed_files_df$year, "/", needed_files_df$doy, "/",sep='')
  #junk= foreach(j = 1:length(urls),.packages = 'RCurl') %dopar% {
  for(j in 211:length(urls)){
      url=urls[j]
      # get urls and limit to wanted tiles
      Sys.sleep(1)
      filenames_url = tryCatch({getURL(url, ftp.use.epsv = F, dirlistonly = T)}, error = function(err) {
                     # getURL fails if you make too many queries, slow down using system pause              
                      print(paste("Your server is pathetic, pausing for 45 seconds: ",err))
                      Sys.sleep(45)
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
              bin = getBinaryURL(filenames_url[i])
              writeBin(bin, paste(out_dir,'/',write_names[i],sep='')) 
          }else{
              print('Skipping file already exists')
              next
          }
      }
  }
  
  # Find any missing files and download
  
  # list all files
  files = data.frame(files=list.files(out_dir,pattern=".hdf", all.files=T, full.names=T),stringsAsFactors = F)
  
  # list dates of files downloaded 
  files$dates = as.character(strptime(gsub("^.*A([0-9]+).*$", "\\1",files$files),'%Y%j'))  # Strip dates
  files$products =  gsub(paste("^.*(",paste(products,collapse='|'),").*$",sep = ''), "\\1",files$files,perl=T) # strip products
  files$tiles =  gsub(paste("^.*(",paste(tiles,collapse='|'),").*$",sep = ''), "\\1",files$files,perl=T) # strip products
  
  # find files not listed 
  missing_dates =  outersect(paste(files$products,files$dates,files$tiles,sep=' '), apply(MARGIN=1,X=expand.grid(paste(needed_files_df$products,needed_files_df$date,sep=' '),tiles), FUN=function(x){paste(x,collapse=' ')} )  )
  missing_dates
  # check dates in year,doy
  format(strptime('2010-05-25','%Y-%m-%d'),'%Y%j')
  


# Mosaic Data -------------------------------------------------------------
  
  files = data.frame(files=list.files(out_dir,pattern=".hdf", all.files=T, full.names=T),stringsAsFactors = F)
  available_date_list = c(available_date_list,list(as.character(gsub("^.*A([0-9]+).*$", "\\1",GetDates(location[1], location[2],product)))   ))
  print(length(files[grep(pattern=MOD_10_13[i],files)]))
  mosaicHDF(files[grep(pattern=MOD_10_13[i],files)], filename=paste(x,'.',MOD_10_13[i],'.hdf',sep=''), 
            MRTpath="G:/Graduate/EDolfi/Summer2014/MODIS/MRT/bin", delete=T)
  

