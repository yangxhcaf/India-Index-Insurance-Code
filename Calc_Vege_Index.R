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
library(compiler)
library(plyr)
registerDoParallel(16)

functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz/Profiling.html#vectorise




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

  # get polygon data (all agriculture (one in afganastan to check for errors))
  ogrInfo('../LandCoverTrainingData','Plot_polys')
  Polys = readOGR('../LandCoverTrainingData','Plot_polys')
  Polys = spTransform(Polys, CRS('+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'))
  Polys$id = 1:dim(Polys@data)[1]




# Extract data for plotting and checking functions ----------------------------- 
  # extract raster values for these locations
#  registerDoParallel(16)
#  EVI = foreach(i = 1:length(crops),.packages='raster') %dopar% {
#     extract(NDVI_stack_h24v05,crops[crops$id ==i,])
#  }
#  endCluster()
#  EVI=lapply(EVI, function(x){
#  	x[x<=-2000]=NA
#  	#x=x*0.0001
#	})
#
#  #save( EVI, file = paste('/groups/manngroup/India_Index/Data/Intermediates/EVIHOLDER.RData',sep='') )
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


  # extract values croped to point or polygon

  out2 = extract_value_point_polygon(Polys,list(NDVI_stack_h24v06,NDVI_stack_h24v05),16)
  out3 = extract_value_point_polygon(crops,list(NDVI_stack_h24v06,NDVI_stack_h24v05),16)

  # Get planting and harvest dates
  PlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=11,
        PlantingDay=23,HarvestMonth=4,HarvestDay=30)

  # Get summary statistics lists
  extr_values=out2
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  a= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile)
  a2= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile,aggregate=T)
  a3 =  Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T, return_df=T)

  extr_values=out3
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  b= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile)
  b2 = Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T)
  b3 = Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T, return_df=T)


# Extract data to district level 

  # Get planting and harvest dates
  PlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=11,
        PlantingDay=23,HarvestMonth=4,HarvestDay=30)

  # get District outlines
  ogrInfo('../Admin Boundaries/','PunjabHaryanaDistricts')
  districts = readOGR('../Admin Boundaries/','PunjabHaryanaDistricts')
  districts = spTransform(districts, CRS('+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'))
  districts$NAME_2 = toupper(as.character(districts$NAME_2)) 
  
  # get districut yeild data 
  yield = read.csv('/groups/manngroup/India_Index/Data/LandCoverTrainingData/Yield_by_district.csv',stringsAsFactors =F)
  yield$district = as.character(yield$district)

  locales = unique(districts$NAME_2)
  locales_v = unique(yield$district)

  # change names to match 
  # find partial matches
  for(i in 1:length(unique(voltage$location))){
  	print(paste(locales[i],' -MATCH- ',locales_v[pmatch(locales[i], locales_v)]))}
  # fill in holes create a dataframe as a lookup table
  look_up = data.frame(locales =locales,
  	locales_v = apply(data.frame(locales),1,function(x) locales_v[pmatch(x, locales_v)]),
	stringsAsFactors=F)
  look_up
  locales_v

  locales_v[!(locales_v %in% look_up$locales_v)]

  look_up[6,2] = locales_v[13]
  look_up[11,2] = locales_v[4]
  look_up[12,2] = locales_v[21]
  look_up[20,2] = locales_v[8]
  look_up[21,2] = locales_v[9]
  look_up[24,2] = locales_v[23]
  look_up[34,2] = locales_v[30]
  look_up[36,2] = locales_v[32]
  look_up[39,2] = locales_v[35]

  # switch names out use non-voltage data names 
  for(i in 1:length(look_up$locales)){
  	print(i)
  	yield$district[yield$district == look_up$locales_v[i] ] = look_up$locales[i]
  }
  
  # double check that spellings are same on both sheets
  sort(unique(yield$district))
  sort(unique(districts$NAME_2))

  
# Extract data for yeild districts

  #evi_district = extract_value_point_polygon(districts,list(NDVI_stack_h24v06,NDVI_stack_h24v05),16)
  #save(evi_district, file = paste('/groups/manngroup/India_Index/Data/Intermediates/evi_district.RData',sep='') )
  load('/groups/manngroup/India_Index/Data/Intermediates/evi_district.RData')

  evi_summary = Annual_Summary_Functions(evi_district, PlantHarvest,Quant_percentile=0.05, aggregate=T, return_df=T,
	num_workers=13)

# Merge EVI data with yields 
  districts$i = 1:length(districts)
  districts$district = districts$NAME_2

  for(i in 1:length(evi_summary)){
   	evi_summary[[i]]=join(evi_summary[[i]], districts@data[,c('i','district','NAME_0','NAME_1','NAME_2')])
	evi_summary[[i]]$year = paste(format(evi_summary[[i]]$plant_dates,'%Y'),format(evi_summary[[i]]$harvest_dates,'%y'),sep='-') 
        evi_summary[[i]]=join(evi_summary[[i]], yield[yield$crop=='Wheat'& yield$season=="Rabi",],type='left') #Rabi Kharif Rice Wheat
  }

  yield_evi = na.omit(do.call(rbind,evi_summary))
  yield_evi$season_length = as.numeric(yield_evi$harvest_dates -yield_evi$plant_dates)
  yield_evi$plant_dates = as.numeric(format(yield_evi$plant_dates,'%j'))
  yield_evi$harvest_dates = as.numeric(format(yield_evi$harvest_dates,'%j'))
  yield_evi$G_mx_dates = as.numeric(format(yield_evi$G_mx_dates,'%j'))
  yield_evi$year_trend = as.numeric(  yield_evi$row)
  write.csv(yield_evi,'/groups/manngroup/India_Index/Data/Intermediates/yield_evi.csv')

  lm1=  lm((yield_tn_ha) ~factor(i)+A_mn+A_min+A_max+A_AUC+G_mx_dates+G_mn+G_min+G_mx+G_AUC+G_AUC_leading
	+G_AUC_trailing+season_length+year_trend,data=yield_evi)
  summary(lm1)
   


# Ridge regression?
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
  library(glmnet)
  form = as.formula(yield_tn_ha ~i+A_mn+A_min+A_max+A_AUC+G_mx_dates+G_mn+G_min+G_mx+G_AUC+G_AUC_leading
        +G_AUC_trailing+season_length+year_trend)
  X = as.matrix(model.frame(form, data=yield_evi))
  Y = X[,1]
  X = X[,3:dim(X)[2]]

  fit = glmnet(X, Y)

  cvfit = cv.glmnet(X, Y)
  plot(cvfit)

  cvfit$lambda.min
  coef(cvfit, s = "lambda.min")
  rmse_ridge = mean((Y - predict(cvfit, newx = X, s = "lambda.min"))^2)
  rmse_ridge


# LASSO http://machinelearningmastery.com/penalized-regression-in-r/
  # load the package
  library(lars)
  # fit model
  fit <- lars(X, Y, type="lasso")
  # summarize the fit
  summary(fit)
  # select a step with a minimum error
  best_step <- fit$df[which.min(fit$RSS)]
  # make predictions
  predictions <- predict(fit, X, s=best_step, type="fit")$fit
  # summarize accuracy
  rmse <- mean((Y - predictions)^2)
  print(rmse)

# OLS
  lm2=  lm((yield_tn_ha) ~A_mn+A_min+A_max+A_AUC+G_mx_dates+G_mn+G_min+G_mx+G_AUC+G_AUC_leading
        +G_AUC_trailing+season_length+year_trend,data=yield_evi)
  summary(lm2)

  rmse_ols = mean((yield_evi$yield_tn_ha - predict(lm2, yield_evi))^2)
  rmse_ols



# Basic Scatterplot Matrix
pairs(yield_tn_ha~A_mn+A_min+A_max+A_AUC+G_mx_dates+G_mn+G_min+G_mx+G_AUC+G_AUC_leading
        +G_AUC_trailing+season_length+year_trend, data =yield_evi,main="Simple Scatterplot Matrix")







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



