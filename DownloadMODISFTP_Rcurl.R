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
tiles =  c('h24v05','h24v06')   #example c('h24v05','h24v06')
dates = c('2015-01-01','2016-01-01') # example c('year-month-day',year-month-day')
location = c(30.259,75.644) # Lat Lon of a location of interest within your tiles listed above
ftp = 'ftp://ladsweb.nascom.nasa.gov/allData/6/'
out_dir = 'G:/Faculty/Mann/Projects/India Index Insurance/Data/FTP'

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









# list dates of files downloaded 
MOD_files_dates = data.frame(dates=gsub("^.*A([0-9]+).*$", "\\1",MOD13Q1_dates),stringsAsFactors = F)  # Strip dates
MYD_files_dates = data.frame(dates=gsub("^.*A([0-9]+).*$", "\\1",MYD13Q1_dates),stringsAsFactors = F)   
MOD_files_dates$year = format(strptime(MOD_files_dates$dates, format="%Y%j"),'%Y')
MOD_files_dates$doy = format(strptime(MOD_files_dates$dates, format="%Y%j"),'%j')
MYD_files_dates$year = format(strptime(MYD_files_dates$dates, format="%Y%j"),'%Y')
MYD_files_dates$doy = format(strptime(MYD_files_dates$dates, format="%Y%j"),'%j')

# download the file
year = MYD_files_dates$year[1]
doy = MYD_files_dates$doy[1]

# find all urls that 
url = paste('ftp://ladsweb.nascom.nasa.gov/allData/6/',product,'/',year, "/", doy, "/",sep='')
filename_url = getURL(url, ftp.use.epsv = F, dirlistonly = T)
filename_url = paste(url, strsplit(filename_url, "\r*\n")[[1]], sep = "")
filename_url = filename_url[multi_grep_character(tiles,filename_url)]
filename_url

# download as binary and save
bin = getBinaryURL(filenames[1]) 
writeBin(bin, paste(out_dir,'/test.hdf',sep=''))  

# save file names
write_names=unlist(lapply(1:length(strsplit(filenames,'/')),function(x){strsplit(filenames,'/')[[x]][length(strsplit(filenames,'/')[[x]])]}))


  

dwd_dirlist <- function(url, full = TRUE){
  dir <- unlist(
    strsplit(
      getURL(url,
             ftp.use.epsv = FALSE,
             dirlistonly = TRUE),
      "\n")
  )
  if(full) dir <- paste0(url, dir)
  return(dir)
}


dwd_dirlist(paste('ftp://ladsweb.nascom.nasa.gov/allData/6/',product,'/',sep=''))









CheckMODISTile <- function(tile, year, doy, data_dir){
  matched_files <- Sys.glob(file.path(data_dir, year, doy, paste("*", tile, "*", sep="")))
  return(matched_files)
}

# for application via parApply() to the all_tiles_df, returns the number of matched files
CheckApply <- function(x, data_dir){
  # matched_tiles <- CheckMODISTile(x[1], x[2], x[3], data_dir)
  matched_tiles <- CheckMODISTile(x[1], x[3], x[4], data_dir)
  return(length(matched_tiles))
}

# apply this function to a missing_files data.frame, and it will download to the appropriate place
DownloadFile <- function(x, data_dir , product ){
  # get the variables from data.frame row
  # tile <- x[1]
  tile <- as.character(unlist(x[1]))[1]
  year <- x[1,3]
  doy <- x[1,4]
  # doy <- formatC(as.integer(x[3]), width=3, flag="0")
  
  # create the new directory, only happens if it doesn't exist
  out_dir <- file.path(data_dir, year, doy)
  created_dir <- dir.create(out_dir, recursive=T)
  
  # download the file using wget
  url = paste('ftp://ladsweb.nascom.nasa.gov/allData/6/',product,'/',year, "/", doy, "/",sep='')
  filenames = getURL(url, ftp.use.epsv = F, dirlistonly = T)
  filenames = paste(url, strsplit(filenames, "\r*\n")[[1]], sep = "")
  filenames
  
  download.file(paste("ftp://ladsweb.nascom.nasa.gov/allData/6/",product,"/", year, "/", doy, "/", "*", tile, "*", sep=""), paste(out_dir,'/test.hdf',sep=''))
  #wget_cmd <- paste("wget -P ", out_dir, " ftp://ladsweb.nascom.nasa.gov/allData/6/",product,"/", year, "/", doy, "/", "*", tile, "*", sep="")
  #system(wget_cmd)
}

# let's get going...
# data_dir <- "/projectnb/modislc/data/mcd12_in/c6/new_nbar/mcd43a4"
# data_dir <- "/projectnb/modislc/data/mcd12_in/c6/new_nbar/mcd43a2"
tiles <-  c('h24v05','h24v06')
years <- 2015:2016 
modis_dates <- seq.Date(as.Date(paste(years[1], "-3-1", sep="")), as.Date(paste(years[length(years)], "-2-1", sep="")), by=1)
product = 'MOD13Q1'

# create a data_frame of all possible combinations of tile, year, and doy
all_files_df <- expand.grid(tiles, modis_dates)
names(all_files_df) <- c("tile", "date")
all_files_df$year <- strftime(all_files_df$date, format="%Y")
all_files_df$doy <- strftime(all_files_df$date, format="%j")

# create a cluster and apply the CheckApply function to the all_tiles data_frame
cl <- makeCluster(2)
# cl <- makeCluster(8)
clusterExport(cl, c("CheckMODISTile", "DownloadFile"))

# check for missing MCD43A4 and download; iterate until the number of missing files doesn't change
delta_num_missing <- 1
last_missing <- 0
max_iter <- 10 # maximum number of times to go through
i <- 1
data_dir <- "G:\\Faculty\\Mann\\Projects\\India Index Insurance\\Data"

while(delta_num_missing > 0 & i <= max_iter){
  system.time(all_files_df$num_files <- parApply(cl, all_files_df, 1, CheckApply, data_dir))
  # now we attempt to download missing tile dates, again using the cluster
  missing_files_df <- subset(all_files_df, num_files == 0)
  if(i == 1){
    last_missing <- dim(missing_files_df)[1]
    num_missing <- 0
    delta_num_missing <- last_missing - num_missing
  }else{
    num_missing <- dim(missing_files_df)[1]
    delta_num_missing <- last_missing - num_missing
    last_missing <- num_missing
  }
  
  system.time(parApply(cl, x=missing_files_df, 1, FUN=DownloadFile, data_dir))
  i <- i + 1
  
}

# check for missing MCD43A2 and download; iterate until the number of missing files doesn't change
delta_num_missing <- 1
last_missing <- 0
max_iter <- 10 # maximum number of times to go through
i <- 1
data_dir <- "/projectnb/modislc/data/mcd12_in/c6/new_nbar/mcd43a2"
while(delta_num_missing > 0 & i <= max_iter){
  system.time(all_files_df$num_files <- parApply(cl, all_files_df, 1, CheckApply, data_dir))
  # now we attempt to download missing tile dates, again using the cluster
  missing_files_df <- subset(all_files_df, num_files == 0)
  if(i == 1){
    last_missing <- dim(missing_files_df)[1]
    num_missing <- 0
    delta_num_missing <- last_missing - num_missing
  }else{
    num_missing <- dim(missing_files_df)[1]
    delta_num_missing <- last_missing - num_missing
    last_missing <- num_missing
  }
  
  system.time(parApply(cl, missing_files_df, 1, DownloadFile, data_dir, mcd43a4=FALSE))
  i <- i + 1
}

# # check for missing MCD43A2 and download:
# data_dir <- "/projectnb/modislc/data/mcd12_in/c6/new_nbar/mcd43a2"
# system.time(all_files_df$num_files <- parApply(cl, all_files_df, 1, CheckApply, data_dir))
# # now we attempt to download missing tile dates, again using the cluster
# missing_files_df <- subset(all_files_df, num_files == 0)
# dim(missing_files_df)
# system.time(parApply(cl, missing_files_df, 1, DownloadFile, data_dir, mcd43a4=FALSE))
#
# # identify duplicates
# # duplicate_files_df <- subset(all_files_df, num_files > 1)
