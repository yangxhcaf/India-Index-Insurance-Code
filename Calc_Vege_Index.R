# Michael Mann    Calc_Vege_Index.R
# This script calculates a series of stastics about the wheat growing season

# Run the following in bash before starting R
 module load proj.4/4.8.0
 module load gdal/gcc/1.11
 module load R
 module load gcc/4.9.0
 R



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
library(ggplot2)
registerDoParallel(16)


# Functions        --------------------------------------------------------

 PlantHarvestDates = function(start_date,end_date,PlantingMonth,PlantingDay,HarvestMonth,HarvestDay){
    # this function takes in date range and returns planting and harvest date for time series
    # set planting
    start_end_years = c(strptime(start_date,'%Y-%m-%d'),strptime(end_date,'%Y-%m-%d'))
    names(unclass(start_end_years[1]))
    start_end_years[1]$mon=PlantingMonth-1
    start_end_years[1]$mday=PlantingDay
    planting = seq(start_end_years[1],
      length=strptime(dates[2],'%Y-%m-%d')$year-strptime(dates[1],'%Y-%m-%d')$year,
      by='year')
    # set harvest
    start_end_years[2]$year=start_end_years[1]$year+1    # set year equal to start year +1
    start_end_years[2]$mon=HarvestMonth-1
    start_end_years[2]$mday=HarvestDay
    harvest = seq(start_end_years[2],
      length=strptime(end_date,'%Y-%m-%d')$year-strptime(start_date,'%Y-%m-%d')$year,
      by='year')
    return(data.frame(planting=planting,harvest=harvest))
  }



# Load Data Layers --------------------------------------------------------
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)



# Set up parameters -------------------------------------------------------


  # Product Filters
  products =  c('MYD13Q1','MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover$
  location = c(30.259,75.644)  # Lat Lon of a location of interest within your $
  tiles =   c('h24v05','h24v06')   # India example c('h13v12')
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-$


# Calculate EVI statistics ------------------------------------------------
  
  localMaxima <- function(x) {
    if(sum(is.na(x))>0){return()}
    # Use -Inf instead if x is numeric (non-integer)
    y <- diff(c(-.Machine$integer.max, x)) > 0L
    rle(y)$lengths
    y <- cumsum(rle(y)$lengths)
    y <- y[seq.int(1L, length(y), 2L)]
    if (x[[1]] == x[[2]]) {
      y <- y[-1]
    }
    y
  }
  
  annualMaxima = function(x,dates_in){
    # returns location of maximum value by year 
    datesY = format(dates_in,'%Y')
    a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
    dates_in[x %in% a ]
  }

  annualMaximaValue = function(x,dates_in){
    # returns location of maximum value by year 
    datesY = format(dates_in,'%Y')
    a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
    a
  }
  
  annualMinumumNearDOY = function(x,dates_in,DOY_in){
    #x = EVI values, dates=dates of observation POSIX, DOY_in = '%Y%j' of rain onset
    tempDOY = strptime(DOY_in,'%Y%j')
    # avoid problems with time class
    if(is.na(tempDOY[1])){print('ERROR: convert date format to %Y%j');break}
    if(class(dates_in)[1]!= 'POSIXlt' ){dates_in=as.POSIXlt(dates_in)}
    # find all local minima, and match with DOY
    tempMINdate = dates_in[localMaxima(x*-1)]
    grid = expand.grid(tempDOY, tempMINdate)
    # find best minimal per DOY
    tempout=do.call(rbind,lapply(split(as.numeric(abs(grid[,1]-grid[,2])),
	format(grid[,1],'%Y%j')),function(x)x[which.min(x)]))
    whichwasmin =  which(as.numeric(abs(grid[,1]-grid[,2])) %in% tempout)
    grid[whichwasmin,2]
  }


 annualMinumumBeforeDOY = function(x,dates_in,DOY_in,days_before){
    # calculates the annual minimum for days_before days before each DOY for planting season
    # best to set DOY as the last expected date of planting
    if(days_before<=8){print('Using less than 8 days is dangerous, 15-30 stable')}
    #x = EVI values, dates=dates of observation POSIX, DOY_in = '%Y%j' of rain onset
    tempDOY = strptime(DOY_in,'%Y%j')
    # avoid problems with time class
    if(is.na(tempDOY[1])){print('ERROR: convert date format to %Y%j');break}
    if(class(dates_in)[1]!= 'POSIXlt' ){dates_in=format(as.POSIXlt(dates_in),'%Y%j')}
    # limit to fixed # of days before DOY
    DOY_before = tempDOY
    #names(unclass(DOY_before[1]))
    DOY_before$mday=DOY_before$mday-days_before      # set days before to doy - days_before
    DOY_table = data.frame(DOY_before=DOY_before,DOY_in=strptime(DOY_in,'%Y%j'))   #match DOY with Days_be$
    # get all days 'days_before' DOY_in in a list
    DOY_interest = unlist(lapply(1:dim(DOY_table)[1],function(h){ format(seq(DOY_table[h,1],
                DOY_table[h,2],by='day'),'%Y%j')}))
    # find all local minima, and match with DOY
    x_DOY_interest = x[dates_in %in% DOY_interest]
    dates_DOY_interest = dates_in[dates_in %in% DOY_interest]
    # get min value for this period for each year
    annualMaxima(x_DOY_interest*-1,strptime(dates_DOY_interest,'%Y%j'))
 }

 
  EVI_Stat = function(EVI_rows,DOY_rows){
      require(sp)
      require(raster)      
      require(MESS)
      #smooth new EVI data    
      smooth_holderl = lapply( 1:dim(EVI_rows)[1], function(i)  SplineAndOutlierRemoval(x = EVI_rows[i,], dates=dates, pred_dates=pred_dates,spline_spar = 0.4) )
      #calculate stats 
      corwet = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) if(!is.na(smooth_holderl[[x]][1])){cor(smooth_holderl[[x]],smooth_sample_dryagri_mean)}else{NA} )  ) ) #correlation with dry agriculture
      cordry = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) if(!is.na(smooth_holderl[[x]][1])){cor(smooth_holderl[[x]],smooth_sample_wetagri_mean)}else{NA} )  ) ) 
      mean = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) mean(smooth_holderl[[x]]) )  ) )
      sd = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) sd(smooth_holderl[[x]]) )  ) )
      max = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) max(smooth_holderl[[x]]) )  ) )
      min = as.numeric(unlist(lapply(1:length(smooth_holderl), function(x) min(smooth_holderl[[x]]) )  ) )
      localMax = lapply(1:length(smooth_holderl),function(x)  pred_dates[localMaxima(smooth_holderl[[x]])]   )
      peaks = unlist(lapply(1:length(localMax), function(x) length(localMax[[x]]))) # number of local maximums over period
      # find minimum closest to rainy season onset DOY
      colnames(DOY_rows)=format(dates2,"%Y")
      DOY = t(apply(round(DOY_rows[,],0),1,function(x) paste(names(x),x,sep='')))
      greenupdate = lapply(1:length(smooth_holderl),function(i) annualMinumumNearDOY(x=smooth_holderl[[i]],dates=pred_dates,DOY_in=DOY[i,]))
      #greenupdate2= lapply(1:length(smooth_holderl),function(i) annualMaxima((smooth_holderl[[i]]*-1),pred_dates)) # date of annual manimum 
      maxupdate   = lapply(1:length(smooth_holderl),function(i) annualMaxima(smooth_holderl[[i]],pred_dates)) # date of annual maximum 
      maxupvalue   = lapply(1:length(maxupdate),function(i) annualMaximaValue(smooth_holderl[[i]],pred_dates) ) # date of annual maximum      # calculate area under curve for total, increasing (portion), and decreasing (portion) of EVI curve
      aucer = function(row){
          hold_list= vector('list',1)
          for( elements in 1:(length(greenupdate[[row]])-1) ){     # the -1 restricts it to 2010,2011,2012
              if(length(greenupdate[[row]])==0){ # if row is empty 
                hold_list[[1]][[1]] =c(NA,NA,NA)  # store NAs if no data available
                hold_list[[1]][[2]] =c(NA,NA,NA)  
                hold_list[[1]][[3]] =c(NA,NA,NA)  
                break 
              }
            require(MESS)
            finder_TOTauc = pred_dates>=greenupdate[[row]][elements] & pred_dates <greenupdate[[row]][elements+1]
            finder_DECauc = pred_dates>=maxupdate[[row]][elements] & pred_dates <greenupdate[[row]][elements+1]
            finder_INCauc = pred_dates>=greenupdate[[row]][elements] & pred_dates <maxupdate[[row]][elements]
            # AUC is based on local minimum nearest to DOY of rainfall onset
            TOTauc = auc(pred_dates[finder_TOTauc],smooth_holderl[[row]][finder_TOTauc], type = 'spline')*0.000001  # auc in millions
            DECauc = auc(pred_dates[finder_DECauc],smooth_holderl[[row]][finder_DECauc], type = 'spline')*0.000001
            INCauc = auc(pred_dates[finder_INCauc],smooth_holderl[[row]][finder_INCauc], type = 'spline')*0.000001
            # AUC2 is based on annual minimum
            hold_list[[1]][[elements]] =c(TOTauc,DECauc,INCauc) #,TOTaucB,DECaucB,INCaucB
          }
          return(hold_list)
       }
      outer = lapply(1:length(greenupdate),function(x) aucer(x))
      TOTauc1 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[1]][1])))
      TOTauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[2]][1])))
      TOTauc3 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[3]][1])))
      DECauc1 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[1]][2])))
      DECauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[2]][2])))
      DECauc3 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[3]][2])))
      INCauc1 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[1]][3])))
      INCauc2 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[2]][3])))
      INCauc3 = as.numeric(unlist(lapply(1:length(outer), function(x) outer[[x]][[1]][[3]][3])))
      max1 = as.numeric(unlist(lapply(1:length(maxupvalue), function(x) maxupvalue[[x]]['2010',])))
      max2 = as.numeric(unlist(lapply(1:length(maxupvalue), function(x) maxupvalue[[x]]['2011',])))
      max3 = as.numeric(unlist(lapply(1:length(maxupvalue), function(x) maxupvalue[[x]]['2012',])))
      if(length(max1)==0){
        max1 = NA
        max2 = NA
        max3 = NA
      }
      return(cbind(corwet,cordry,mean,sd,max,min,peaks,TOTauc1,TOTauc2,TOTauc3,DECauc1,DECauc2,DECauc3,INCauc1,INCauc2,INCauc3,max1,max2,max3))#TOTaucB,DECaucB,INCaucB
  } 


# Check functions ----------------------------------------------
# Load Data Layers 
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')
  

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)


  plot_dates = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(NDVI_stack_h24v05)),format='%Y%j') # create dates to in$

  # get LC examples
  ogrInfo('../LandCoverTrainingData','IndiaLandCoverExamples')
  crops = readOGR('../LandCoverTrainingData','IndiaLandCoverExamples')
  crops = spTransform(crops, CRS('+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'))
  crops$id = 1:dim(crops@data)[1]
  # extract raster values for these locations
  registerDoParallel(10)
  EVI = foreach(i = 1:length(crops),.packages='raster') %dopar% {
     extract(NDVI_stack_h24v05,crops[crops$id ==i,])
  }
  EVI=lapply(EVI, function(x){
  x[x<=-2000]=NA
  x=x*0.0001})

  # Plot
  EVI_v1 = as.numeric(EVI[[3]])
  plotdata = data.frame(EVI= EVI_v1,
                        dates =as.Date(strptime(plot_dates,'%Y-%m-%d')),class = 'EVI')

  plotdata = rbind(plotdata, data.frame(EVI = SplineAndOutlierRemoval(x = EVI_v1,
                        dates=plot_dates, pred_dates=plot_dates,spline_spar = 0.2),
                        dates =as.Date(strptime(plot_dates,'%Y-%m-%d')),class = 'EVI Smoothed'))

  # Get planting and harvest dates
  PlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=11,
	PlantingDay=23,HarvestMonth=4,HarvestDay=30)

  # plot out time series with planting and harvest dates
  rects = data.frame(xstart = as.Date(PlantHarvest$planting),
    xend = as.Date(PlantHarvest$harvest))

  # test summary statistics 
  plotdatasmoothed = plotdata[plotdata$class=='EVI Smoothed',]
  #vertical_lines =  annualMaxima(plotdatasmoothed$EVI,plotdatasmoothed$dates)
  
  vertical_lines =  annualMinumumBeforeDOY(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_in=format(PlantHarvest$planting,'%Y%j'), days_before =30)

  ggplot()+geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
        ymin = -Inf, ymax = Inf), alpha = 0.4)+
        geom_point(data= plotdata, aes(x=dates,y=EVI,group=class,colour=class))+
	geom_vline(xintercept = as.numeric(as.Date(strptime(vertical_lines,'%Y-%m-%d')))) 







# Run functions on stacks -----------------------------------------
# newextent = extent(500000,600000,1000000,1100000)
# EVI_crop = crop(EVI_stack,newextent)
# DOY_crop = crop(DOY_stack,newextent)
# plot(EVI_crop[[1]])


#Determine optimal block size for loading in MODIS stack data
  EVI_stack_in = EVI_stack
  DOY_stack_in = DOY_stack
  block_width = 3
  nrows = dim(EVI_stack_in)[1]
  nblocks <- nrows%/%block_width
  bs_rows <- seq(1,nblocks*block_width+1,block_width)
  bs_nrows <- rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
  print('Working on the following rows')
  print(paste(bs_rows))

#Register the parallel backend
  registerDoParallel(6)
  #
  result <- foreach(i = 1:length(bs_rows), .combine = rbind) %dopar% {
    require(raster)
    require(rgdal)
    EVI_v1 = getValues(EVI_stack_in, bs_rows[i], bs_nrows[i])
    DOY_v1 = getValues(DOY_stack_in, bs_rows[i], bs_nrows[i])
    pheno_matrix =  EVI_Stat(EVI_rows=EVI_v1,DOY_rows=DOY_v1)
    print(paste("Saving pheno_matrix for row",i))
    save(pheno_matrix,file = paste("pheno2_",bs_rows[i],"_",bs_nrows[i],sep = ""))
  }
  stopImplicitCluster()


# load multicore output
  setwd('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data\\250m_EVI_Data')

  f = list.files(getwd(),'pheno2_*')
  #put files in ascending order
  row_id = as.numeric(unlist(lapply(1:length(strsplit(f,split = '_')),function(x) strsplit(f,split = '_')[[x]][2])))
  f =f[order(row_id)] 
  load(f[1])  # load first row group
  result = pheno_matrix
  i=1
  for(file in f[-1]){
    load(file)
    if(dim(pheno_matrix)[2]!=19){print(paste(file,'dim:', dim(pheno_matrix)[1],dim(pheno_matrix)[2],'row:',i))}
    result = rbind(result, pheno_matrix)  # rbind in remaining row
    i=i+1
  }

# put back into raster
  for( layer in 1:dim(result)[2]){
    r = EVI_stack[[1]]
    r = setValues(r, matrix(result[,layer],nrow=dim(r)[1],byrow=T))
    names(r) = colnames(result)[layer]
    writeRaster(r,paste('result',colnames(result)[layer],'b.tif',sep='_'),overwrite=T)
  }



