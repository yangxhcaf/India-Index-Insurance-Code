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
registerDoParallel(4)


setwd('G:/Faculty/Mann/Projects/India Index Insurance/Data')

# give path to Modis Reproduction Tool
MRT = 'G:\\Faculty\\Mann\\Projects\\India Index Insurance\\Data\\MRT\\bin'

# View Availability -------------------------------------------------------
multi_grep_character <- function(find, inthis){ #returns location of multiple "find' elements in the vector 'inthis'
  if(class(inthis)!= "character"){break("Error: in this must be a character vector")}
  return(unlist(lapply(1:length(find), function(x) {grep(find[x],inthis)}   )))
}

###################################################################################
###################################################################################


# get list of all available modis products
GetProducts()


# Product Filters 
products = c('MYD13Q1','MOD13Q1')
location = c(30.259,75.644) # Lat Lon of a location of interest within your tiles listed above
tiles =  c('h24v05','h24v06')   #example c('h24v05','h24v06')
dates = c('2002-07-04','2016-01-25') # example c('year-month-day',year-month-day')
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

# list all files we need to download
needed_files_df = avail_files_df[ avail_files_df$date %in% as.character(seq(as.Date(dates[1]),as.Date(dates[2]),'days'))  ,] # limit available files to needed date range
head(needed_files_df)

# find all urls for download
urls = paste(ftp, needed_files_df$products,'/',needed_files_df$year, "/", needed_files_df$doy, "/",sep='')
junk= foreach(j = 1:length(urls),.packages = 'RCurl') %dopar% {
    url=urls[j]
    # get urls and limit to wanted tiles
    filenames_url = getURL(url, ftp.use.epsv = F, dirlistonly = T)
    filenames_url = paste(url, strsplit(filenames_url, "\r*\n")[[1]], sep = "")
    filenames_url = filenames_url[multi_grep_character(tiles,filenames_url)]
    print(filenames_url)
    
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

 


