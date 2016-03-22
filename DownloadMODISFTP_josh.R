library(parallel)
library(RCurl)
  
  # returns the matched files
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
