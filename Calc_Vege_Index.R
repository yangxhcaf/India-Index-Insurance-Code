# Michael Mann    Calc_Vege_Index.R
# This script calculates a series of stastics about the wheat growing season

# Run the following in bash before starting R
 module load proj.4/4.8.0
 module load gdal/gcc/1.11
 module load R
 module load gcc/4.9.0
 R



rm(list=ls())
#source('G:\\Faculty\\Mann\\Projects\\India_Index_Insurance\\India_Index_Insurance_Code\\ModisDownload.R')
#source('G:\\Faculty\\Mann/scripts/SplineAndOutlierRemoval.R')
source('/groups/manngroup/India_Index/India-Index-Insurance-Code/SummaryFunctions.R')

source('/groups/manngroup/scripts/SplineAndOutlierRemoval.R')
library(RCurl)
library(raster)
library(MODISTools)
library(rgdal)
library(sp)
library(maptools)
#library(rts)
#library(gdalUtils)
library(foreach)
library(doParallel)
library(ggplot2)
library(MESS)
registerDoParallel(16)




# Set up parameters -------------------------------------------------------


  # Product Filters
  products =  c('MYD13Q1','MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover$
  location = c(30.259,75.644)  # Lat Lon of a location of interest within your $
  tiles =   c('h24v05','h24v06')   # India example c('h13v12')
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-$



# Check functions ----------------------------------------------
# Load Data Layers 
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')
  

  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops Clean/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)


  plot_dates = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(NDVI_stack_h24v05)),format='%Y%j') # create dates to in$

  # get LC examples
  ogrInfo('../LandCoverTrainingData','IndiaLandCoverExamples')
  crops = readOGR('../LandCoverTrainingData','IndiaLandCoverExamples')
  crops = spTransform(crops, CRS('+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'))
  crops$id = 1:dim(crops@data)[1]


# Extract data for plotting and checking functions ----------------------------- 
  # extract raster values for these locations
  registerDoParallel(16)
  EVI = foreach(i = 1:length(crops),.packages='raster') %dopar% {
     extract(NDVI_stack_h24v05,crops[crops$id ==i,])
  }
  endCluster()
  EVI=lapply(EVI, function(x){
  	x[x<=-2000]=NA
  	#x=x*0.0001
	})

  #save( EVI, file = paste('/groups/manngroup/India_Index/Data/Intermediates/EVIHOLDER.RData',sep='') )
  load('/groups/manngroup/India_Index/Data/Intermediates/EVIHOLDER.RData')

  # Plot
  EVI_v1 = as.numeric(EVI[[4]])
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
  


# Example function calls ---------------------------------------------------

  plant_lines =  AnnualMinumumBeforeDOY(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_in=PlantHarvest$planting,days_shift=30,dir='before')

  harvest_lines =  AnnualMinumumBeforeDOY(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_in=PlantHarvest$harvest,days_shift=30,dir='after')

  # correct the number of elements in each date vector (assigns last day if no final harvest date available) 
  plant_lines = correct_dates(dates_in= plotdatasmoothed$dates, dates_str=plant_lines, dates_end=harvest_lines)[[1]]
  harvest_lines = correct_dates(dates_in= plotdatasmoothed$dates, dates_str=plant_lines, dates_end=harvest_lines)[[2]]

  max_lines =  PeriodAggregatorDates(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        date_range_st=plant_lines, date_range_end=harvest_lines,
        by_in='days',FUN=function(x)max(x,na.rm=T))

  AnnualMaxima(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates)

  AnnualMaximaValue(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates)

  AnnualAggregator(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,FUN=function(x)mean(x,na.rm=T))
  AnnualAggregator(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,FUN=function(x)min(x,na.rm=T))
  AnnualAggregator(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,FUN=function(x)max(x,na.rm=T))

  PeriodAggregator(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        date_range_st=plant_lines, date_range_end=harvest_lines,
        by_in='days',FUN=function(x)max(x,na.rm=T))

  AnnualMinumumNearDOY(x,dates_in,DOY_in=PlantHarvest$planting[1])

  AnnualAverageDOYvalues(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates)

  AnnualAUC(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates)

  AnnualMinumumBeforeDOY(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_in=PlantHarvest$planting,days_shift=30,dir='before')

  # estimates AUC for whole growing season
  growing_AUC = PeriodAUC(x_in = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_start_in=plant_lines,DOY_end_in=harvest_lines)


  # estimates AUC for first 1/2 of growing season
  leading_AUC = PeriodAUC(x_in = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_start_in=plant_lines,DOY_end_in=max_lines)

  # estimates AUC for second 1/2 of growing season
  trailing_AUC = PeriodAUC(x_in = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        DOY_start_in=max_lines,DOY_end_in=harvest_lines)

  # compare each year to mean AUC
  growing_AUC-mean(growing_AUC,na.rm=T)

  # calculate global quantiles for periods of interest
  GlobalPeriodAggregator(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        date_range_st=plant_lines, date_range_end=harvest_lines,
        by_in='days',FUN=function(x)quantile(x,0.05,type=8,na.rm=T))



  ggplot()+geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
        ymin = -Inf, ymax = Inf), alpha = 0.4)+
        geom_point(data= plotdata, aes(x=dates,y=EVI,group=class,colour=class))+
        geom_vline(colour='blue',xintercept = as.numeric(as.Date(strptime(plant_lines,'%Y-%m-%d'))))+
        geom_vline(colour='red',xintercept = as.numeric(as.Date(strptime(harvest_lines,'%Y-%m-%d'))))+
        geom_vline(colour='orange',xintercept = as.numeric(as.Date(strptime(max_lines,'%Y-%m-%d'))))+
        annotate("text", x =(PlantHarvest$planting[1]+60), y = 0.375, label = "Wheat")+
        annotate("text", x =(PlantHarvest$harvest[1]+90), y = 0.3, label = "Rice")



# Extract polygon or points data from stacks ------------------------

  xys= c(76.27797,28.39791,
	76.30543,28.39761,
	76.30548,28.40236,
	76.27668,28.40489)
  poly = matrix(xys, ncol=2, byrow=TRUE)
  poly = SpatialPolygons(list(Polygons(list(Polygon(poly)), ID="a"),
	Polygons(list(Polygon(poly)), ID="b")));
  proj4string(poly) <-"+proj=longlat +datum=WGS84 +ellps=WGS84"
  poly = spTransform(poly, CRS("+proj=sinu +a=6371007.181 +b=6371007.181 +units=m"))


  # extract values croped to point or polygon
  out2 = extract_value_point_polygon(poly,NDVI_stack_h24v06,16)
  out3 = extract_value_point_polygon(crops,NDVI_stack_h24v06,16)



  # Get planting and harvest dates
  PlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=11,
        PlantingDay=23,HarvestMonth=4,HarvestDay=30)

  # Get summary statistics lists
  extr_values=out2
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  a= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile)
  a2= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile,aggregate=T)

  extr_values=out3
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  b= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile)
  b2 = Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T)




 max_lines =  PeriodAggregatorDates(x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates,
        date_range_st=plant_lines, date_range_end=harvest_lines,
        by_in='days',FUN=function(x)max(x,na.rm=T))


 Annual_Summary_Functions=function(extr_values, PlantHarvestTable,Quant_percentile,aggregate=F){
     # take in values from extract_value_point_polygon and create annual and global summary statistics
     # returns a list where elements are composed of annual and growing season statistics
     # if aggregate=T, pixels comprising a polygon are smoothed and then the average signal is obtained, statistics are run from that

     # iterate between spatial objects
     result_summary=foreach(i = 1:length(extr_values),.packages='raster',.inorder=T) %dopar%{
        if(is.na(extr_values[[i]])){ print('Empty Object');return(NA)} # avoid empties

        # Get dates from stack names
        dats = strptime( gsub("^.*X([0-9]+).*$", "\\1", names(extr_values[[i]])),format='%Y%j')
        # Calculate smoothed values
        smooth = lapply(1:dim(extr_values[[i]])[1],function(z){SplineAndOutlierRemoval(
            x = as.numeric(extr_values[[i]][z,]),
            dates=as.Date(dats),
            pred_dates=as.Date(dats),spline_spar = 0.2)})

	# if aggregate = T, summarize multiple pixels per polygon into one smooth time series
        # create a mean value for smoothed data 
	if(aggregate==T){smooth = list(Reduce("+", smooth) / length(smooth))}

        # estimate planting and harvest dates
        plant_dates = lapply(1:length(smooth),function(z){ AnnualMinumumBeforeDOY(x = smooth[[z]],
            dates_in = dats, DOY_in=PlantHarvestTable$planting,days_shift=30,dir='before')})
        harvest_dates = lapply(1:length(smooth),function(z){ AnnualMinumumBeforeDOY(x = smooth[[z]],
            dates_in = dats, DOY_in=PlantHarvestTable$harvest,days_shift=30,dir='after')})
        # correct the number of elements in each date vector (assigns last day if no final harvest date available)
        plant_dates = lapply(1:length(plant_dates),function(z){ correct_dates(dates_in= dats, dates_str=plant_dates[[z]],
                dates_end=harvest_dates[[z]])[[1]] })
        harvest_dates = lapply(1:length(plant_dates),function(z){ correct_dates(dates_in= dats, dates_str=plant_dates[[z]],
                dates_end=harvest_dates[[z]])[[2]] })

        # Annual statistics
        A_mn = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)mean(x,na.rm=T))})
        A_min = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)min(x,na.rm=T))})
        A_max = lapply(1:length(smooth),function(z){AnnualAggregator(x = smooth[[z]],
                dates_in = dats, FUN=function(x)max(x,na.rm=T))})
        A_AUC = lapply(1:length(smooth),function(z){ AnnualAUC(x = smooth[[z]],dates_in = dats) })

        A_Qnt = lapply(1:length(smooth),function(z){quantile(x = smooth[[z]],p=Quant_percentile,type=8,na.rm=T) })

        # Growing season statistics
        G_mx_dates = lapply(1:length(smooth),function(z){ PeriodAggregatorDates(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) max(x,na.rm=T))})
        G_mn = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) mean(x,na.rm=T)) })
        G_min = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) min(x,na.rm=T)) })
        G_mx = lapply(1:length(smooth),function(z){ PeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x) max(x,na.rm=T)) })
        G_AUC = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=plant_dates[[z]],DOY_end_in=harvest_dates[[z]]) })

        G_AUC_leading  = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=plant_dates[[z]],DOY_end_in=G_mx_dates[[z]]) })
        G_AUC_trailing = lapply(1:length(smooth),function(z){ PeriodAUC(x_in = smooth[[z]],dates_in = dats,
                DOY_start_in=G_mx_dates[[z]],DOY_end_in=harvest_dates[[z]]) })
        G_AUC_diff_mn = lapply(1:length(smooth),function(z){ G_AUC[[z]] - mean(G_AUC[[z]],na.rm=T) })

        G_Qnt = lapply(1:length(smooth),function(z){ GlobalPeriodAggregator(x = smooth[[z]],
                dates_in = dats, date_range_st=plant_dates[[z]],
                date_range_end=harvest_dates[[z]], by_in='days',FUN=function(x)
                quantile(x,p=Quant_percentile,type=8,na.rm=T)) })

        # collect all data products
        list(smooth_stat = smooth,plant_dates=plant_dates,harvest_dates=harvest_dates,A_mn=A_mn,A_min=A_min,
                A_max=A_max,A_AUC=A_AUC,A_Qnt=A_Qnt,G_mx_dates=G_mx_dates,G_mn=G_mn,G_min=G_min,G_mx=G_mx,G_AUC=G_AUC,
                G_AUC_leading=G_AUC_leading,G_AUC_trailing=G_AUC_trailing,G_AUC_diff_mn=G_AUC_diff_mn,G_Qnt=G_Qnt)

     }
  }





# Working demo extact values ---------------------------------------

  # Run functions on list of points 
  ptm <- proc.time()
  cell = cellFromXY(NDVI_stack_h24v05, crops[crops$id ==2,])
  r = rasterFromCells(NDVI_stack_h24v05, cell,values=F)
  registerDoParallel(16)
  result = foreach(i = 1:dim(NDVI_stack_h24v05)[3],.packages='raster',.inorder=T) %dopar% {
     crop(NDVI_stack_h24v05[[i]],r)
  }
  result2 = getValues(stack(result))
  endCluster()
  proc.time() - ptm

  plot(1:length(result2),result2)

  ptm <- proc.time()
  result3 =  extract(NDVI_stack_h24v05,crops[crops$id ==2,])
  proc.time() - ptm

  identical(as.numeric(result2),as.numeric(result3))


# Run functions on stacks -----------------------------------------


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



