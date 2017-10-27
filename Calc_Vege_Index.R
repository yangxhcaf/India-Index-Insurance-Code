

# r regression variance inflation factor - tells contributions to multicolinearity inflation of SE
# https://onlinecourses.science.psu.edu/stat501/node/347


# Michael Mann    Calc_Vege_Index.R
# This script calculates a series of stastics about the wheat growing season


# Run the following in bash before starting R
 module load proj.4/4.8.0
 module load gdal/gcc/1.11
 module load R/3.1.1
 #module use /home/mmann1123/local/modulefiles 
 #module load R/3.2.2
 module load gcc/4.9.0
 R






rm(list=ls())
#source('H:\\Projects\\India_Index_Insurance\\India_Index_Insurance_Code\\ModisDownload.R')
#source('H:\\scripts/SplineAndOutlierRemoval.R')
source('/groups/manngroup/India_Index/India-Index-Insurance-Code/SummaryFunctions.R')
source('/groups/manngroup/scripts/SplineAndOutlierRemoval.R')
source('H:/Projects/India_Index_Insurance/India_Index_Insurance_Code/SummaryFunctions.R')

#install.packages("RCurl",repos="http://cran.cnr.berkeley.edu/")
#install.packages("raster",repos="http://cran.cnr.berkeley.edu/")
#install.packages("MODISTools",repos="http://cran.cnr.berkeley.edu/")
#install.packages("rgdal",repos="http://cran.cnr.berkeley.edu/")
#install.packages("sp",repos="http://cran.cnr.berkeley.edu/")
#install.packages("maptools",repos="http://cran.cnr.berkeley.edu/")
#install.packages("foreach",repos="http://cran.cnr.berkeley.edu/")
#install.packages("doParallel",repos="http://cran.cnr.berkeley.edu/")
#install.packages("ggplot2",repos="http://cran.cnr.berkeley.edu/")
#install.packages("MESS",repos="http://cran.cnr.berkeley.edu/")
#install.packages("compiler",repos="http://cran.cnr.berkeley.edu/")
#install.packages("plyr",repos="http://cran.cnr.berkeley.edu/")
#install.packages("plm",repos="http://cran.cnr.berkeley.edu/")
#install.packages("zoo",repos="http://cran.cnr.berkeley.edu/")
#install.packages("rgeos",repos="http://cran.cnr.berkeley.edu/")


library(raster)
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
library(zoo)
library(plm)
library(randomForest)
library(e1071)
library(stats)
library(plm) # works on desktop at school

registerDoParallel(7)

functions_in = lsf.str()
lapply(1:length(functions_in), function(x){cmpfun(get(functions_in[[x]]))})  # byte code compile all functions http://adv-r.had.co.nz/Profiling.html#vectorise




# Set up parameters -------------------------------------------------------


  # Product Filters
  products =  c('MYD13Q1','MOD13Q1')  #EVI c('MYD13Q1','MOD13Q1')  , land cover$
  location = c(30.259,75.644)  # Lat Lon of a location of interest within your $
  tiles =   c('h24v05','h24v06')   # India example c('h13v12')
  dates = c('2002-01-01','2016-02-02') # example c('year-month-day',year-month-$
  setwd('/groups/manngroup/India_Index/Data/Data Stacks')
  #setwd('H:/Projects/India_Index_Insurance/Data/Data Stacks')
  
  # load data stacks from both directories
  dir1 = list.files('./WO Clouds Crops Clean/','.RData',full.names=T)
  lapply(dir1, load,.GlobalEnv)



###############################################################
# Demonstrate  functions ----------------------------------------------
###############################################################

# Load Data Layers 


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

  AnnualMinumumNearDOY(x=plotdatasmoothed$EVI,dates_in=plotdatasmoothed$dates,DOY_in=PlantHarvest$planting[1])

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

  # extracted data is actually NDVI from above So changing names
  names(plotdata) = c('NDVI','Dates','Legend')
  plotdata$Legend=as.character(plotdata$Legend)
  plotdata$Legend[plotdata$Legend=='EVI'] ='NDVI'
  plotdata$Legend[plotdata$Legend=='EVI Smoothed'] ='NDVI Smoothed'

  # Write out data for publication figure
  write.csv(plotdata, '/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/NDVI_smooth_timeseries.csv')
  write.csv(rects, '/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/rects.csv')
  write.csv(plant_lines, '/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/plant_lines.csv')
  write.csv(harvest_lines, '/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/harvest_lines.csv')
  write.csv(PlantHarvest, '/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/PlantHarvest.csv')
  write.csv(max_lines, '/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/max_lines.csv')

#  ggplot()+geom_rect(data = rects, aes(xmin = xstart, xmax = xend,
 #       ymin = -Inf, ymax = Inf), alpha = 0.4)+
 #       geom_point(data= plotdata, aes(x=Dates,y=NDVI,group=Legend,colour=Legend))+
 #       geom_vline(colour='blue',xintercept = as.numeric(as.Date(strptime(plant_lines,'%Y-%m-%d'))))+
 #       geom_vline(colour='red',xintercept = as.numeric(as.Date(strptime(harvest_lines,'%Y-%m-%d'))))+
 #       geom_vline(colour='orange',xintercept = as.numeric(as.Date(strptime(max_lines,'%Y-%m-%d'))))+
 #       annotate("text", x =(PlantHarvest$planting[1]+61), y = 0.37, label = "Wheat")+
 #       annotate("text", x =(PlantHarvest$harvest[1]+93), y = 0.31, label = "Rice")

 # ggsave(file="../../India-Index-Insurance-Code/WriteUp/PlantHarvestDates_NDVI.png")


# Extract polygon or points data from stacks ------------------------


  # extract values croped to point or polygon

  #out2 = extract_value_point_polygon(Polys,list(EVI_stack_h24v06,EVI_stack_h24v05),16)
  #out3 = extract_value_point_polygon(crops,list(EVI_stack_h24v06,EVI_stack_h24v05),16)
  #save( out2, file = paste('/groups/manngroup/India_Index/Data/Intermediates/EVI2.RData',sep='') )
  #save( out3, file = paste('/groups/manngroup/India_Index/Data/Intermediates/EVI3.RData',sep='') )
  load('/groups/manngroup/India_Index/Data/Intermediates/EVI2.RData')
  load('/groups/manngroup/India_Index/Data/Intermediates/EVI3.RData')

  # test performance of extract 
  extract_times = list()
       # check default extract time for comparison
        ptm <- proc.time()
        a = extract(EVI_stack_h24v06, Polys)
        times = proc.time() - ptm
        extract_times=c(extract_times,times[1])
        remove(a)
  for(worker in seq(1,32,by=2)){ 
	ptm <- proc.time()
	print(worker)
   	a=extract_value_point_polygon(Polys,list(EVI_stack_h24v06),worker)
	times = proc.time() - ptm
	extract_times=c(extract_times,times[1]) # get user time
  	remove(a)
  }

  bench = data.frame(times = unlist(extract_times), cores=c('Extract',seq(1,32,by=2)))
  write.csv(bench,'/groups/manngroup/India_Index/India-Index-Insurance-Code/WriteUp/FigureTableData/benchmark_extract.csv')

  # Get planting and harvest dates
  PlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=11,
        PlantingDay=23,HarvestMonth=4,HarvestDay=30)

  # Get summary statistics lists
  extr_values=out3
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  num_workers = 16
  spline_spar = 0
  a= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile)
  a2= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile,aggregate=T)
  a3 =  Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T, return_df=T)

 # get quantiles based on polygon values
  Neighborhood_quantile(extr_values, PlantHarvestTable,Quant_percentile=0.05,num_workers=13,spline_spar = 0)


  

  # Get summary statistics lists
  extr_values=out3
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  b= Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile)
  b2 = Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T)
  b3 = Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T, return_df=T)





###############################################################
# Extract data to district level link with yield data
###############################################################



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
  yield$years_id = as.numeric(substr(yield$year,1,4))
  
  #  ggplot(data=yield[yield$season=='Rabi'& yield$crop=='Wheat',],aes(x=years_id,y=yield_tn_ha,colour=district))+
  #	geom_point() + facet_wrap( ~ district )+xlab('Year')+ylab('Wheat Tons / ha')+ theme(legend.position="none")
  # remove two outliers
 
# DO WE NEED TO DO THIS?  They seem like outliers, but maybe not?  Can remove them later if needed
# yield$yield_tn_ha[yield$yield_tn_ha<1 |yield$yield_tn_ha>6]=NA

	
  # get names to match
  locales = unique(districts$NAME_2)
  locales_v = unique(yield$district)

  # change names to match 
  # find partial matches
  for(i in 1:length(unique(locales))){
  	print(paste(locales[i],' -MATCH- ',locales_v[pmatch(locales[i], locales_v)]))}
  # fill in holes create a dataframe as a lookup table
  look_up = data.frame(locales =locales,
  	locales_v = apply(data.frame(locales),1,function(x) locales_v[pmatch(x, locales_v)]),
	stringsAsFactors=F)
  
  lookmeup = function(){ print(look_up)
          print(locales_v)
          print('++++++++still missing++++++++')
          print(locales_v[!(locales_v %in% look_up$locales_v)])}# find mismatches
  lookmeup()


  # find mismatches
  locales_v[!(locales_v %in% look_up$locales_v)]
  # change mispelled names to match
  look_up[6,2] = locales_v[13]
  look_up[11,2] = locales_v[4]
  look_up[12,2] = locales_v[21]
  look_up[20,2] = locales_v[8]
  look_up[21,2] = locales_v[9]
  look_up[24,2] = locales_v[23]
  look_up[34,2] = locales_v[30]
  look_up[36,2] = locales_v[32]
  look_up[39,2] = locales_v[35]

  lookmeup()

  # switch names out use non-voltage data names 
  for(i in 1:length(look_up$locales)){
  	yield$district[yield$district == look_up$locales_v[i] ] = look_up$locales[i]
  }
  

  # convert to uppercast PROBLEM ONLY APPLY TO CHARACTER COLUMNS CONVERTS NUMBER TO CHARACTER FOR SOME REASON
  library(dplyr)
  upper_it = function(X){X %>% mutate_each_( funs(as.character(.)), names( .[sapply(., is.factor)] )) %>%
       mutate_each_( funs(toupper), names( .[sapply(., is.character)] ))}   # convert factor to character then upperca 

  districts@data[,c('ISO','NAME_0','NAME_1','NAME_2','HASC_2','TYPE_2')] = 
		upper_it(districts@data[,c('ISO','NAME_0','NAME_1','NAME_2','HASC_2','TYPE_2')] ) 
  sapply(districts@data,class) # PROBLEM CLASS SWITCH FOR NUMERIC DATA EVEN THOUGH IT WORKS PERFECTLY FOR EXAMPLE BELOW
  yield[,c('state','crop','season')] = upper_it(yield[,c('state','crop','season')])
  sapply(yield, class)



# Import second wheat yeild data (punjab only)

  yield_punjab = read.csv('/groups/manngroup/India_Index/Data/YieldData/Chandigarh Wheat Yields for Punjab.csv',
	stringsAsFactors=F)
  yield_punjab$district = toupper(yield_punjab$Districts)
  yield_punjab$state = toupper(yield_punjab$State)
  yield_punjab$years_id = as.numeric(substr(yield_punjab$Year,1,4))
  yield_punjab$year = yield_punjab$Year

  # get names to match
  locales = sort(unique(districts$NAME_2[districts$NAME_1=='PUNJAB']))
  locales_v = sort(unique(yield_punjab$district))

  # change names to match
  # find partial matches
  for(i in 1:length(unique(districts$NAME_2))){
        print(paste(locales[i],' -MATCH- ',locales_v[pmatch(locales[i], locales_v)]))}
  # fill in holes create a dataframe as a lookup table
  look_up = data.frame(locales =locales,
        locales_v = apply(data.frame(locales),1,function(x) locales_v[pmatch(x, locales_v)]),
        stringsAsFactors=F)

  lookmeup()
  # change mispelled names to match
  look_up[5,2] = locales_v[5]
  look_up[18,2] = locales_v[17]
  look_up[19,2] = locales_v[19] # found this on wikipedia
  look_up[21,2] = locales_v[15] # found this on google maps 
  
  lookmeup()

  # switch names out use district data names
  for(i in 1:length(look_up$locales)){
        yield_punjab$district[yield_punjab$district == look_up$locales_v[i] ] = look_up$locales[i]
  }


  # upper case all characters
  yield_punjab[,c('State','Districts','district','state')]  =  upper_it(yield_punjab[,c('State','Districts','district','state')])
  sapply(yield_punjab,class)


# Extract data for yeild districts for NDVI

  #evi_district = extract_value_point_polygon(districts,list(NDVI_stack_h24v06,NDVI_stack_h24v05),15)
  #save(evi_district, file = paste('/groups/manngroup/India_Index/Data/Intermediates/NDVI_district.RData',sep='') )
  load('/groups/manngroup/India_Index/Data/Intermediates/NDVI_district.RData')

  # find the best spar value
  #spar_find()  # returns spar,adj R2, RMSE/meanvalue

	# [1,]  0.0 0.6717329 0.04968169
	# [2,]  0.1 0.6200211 0.05372909
	# [3,]  0.2 0.5574208 0.05409262
	# [4,]  0.3 0.6637620 0.04951915
	# [5,]  0.4 0.6283626 0.05173671
	# [6,]  0.5 0.6473474 0.05062672
	# [7,]  0.6 0.6457232 0.05119712
	# [8,]  0.7 0.6459546 0.05140148
	# [9,]  0.8 0.6793824 0.05028132
	#[10,]  0.9 0.6820132 0.05018398
	#[11,]  1.0 0.6959981 0.04822815
	#[12,]  1.1 0.6923934 0.04875287
	#[13,]  1.2 0.6957356 0.04838682
	#[14,]  1.3 0.6963612 0.04839690
	#[15,]  1.4 0.6955228 0.04831201
	#[16,]  1.5 0.6930620 0.04853615
	#[17,]  1.6 0.6932628 0.04759016
	#[18,]  1.7 0.6922859 0.04834266
	#[19,]  1.8 0.6847449 0.04826328
	#[20,]  1.9 0.6819941 0.04862113
	#[21,]  2.0 0.6867824 0.04808403
	#[22,]  2.1 0.6851710 0.04891847
	#[23,]  2.2 0.6834663 0.04883971
	#[24,]  2.3 0.6811296 0.04896917
	#[25,]  2.4 0.6812064 0.04885636
	#[26,]  2.5 0.6812533 0.04897854
	#[27,]  2.6 0.6850948 0.04858550
	#[28,]  2.7 0.6872224 0.04838028
	#[29,]  2.8 0.6846268 0.04860193
	#[30,]  2.9 0.6839818 0.04874246
	#[31,]  3.0 0.6841829 0.04866848



  # Get planting and harvest dates for rice (inverse of dates for wheat)
  RicePlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=4,
        PlantingDay=30,HarvestMonth=11,HarvestDay=23)
  RicePlantHarvest$harvest = RicePlantHarvest$harvest-360  # fix harvest year
  RicePlantHarvest = RicePlantHarvest[2:dim(RicePlantHarvest)[1],] # remove 2002 since we don't have rice season for that year

  evi_summary = Annual_Summary_Functions(extr_values=evi_district,PlantHarvestTable=list(PlantHarvest,RicePlantHarvest),
	Quant_percentile=0.95, aggregate=T, return_df=T,num_workers=17,spline_spar=0)

  # get mean of all variables for all years all districts
  evi_summary_annual_av = lapply(evi_summary,function(x) aggregate(A_mn ~i,data=x,function(y){mean(y,na.rm=T) }))
  evi_summary_annual_av = lapply(evi_summary_annual_av,function(x) x[2] ) # drop id


  # returns quantile for all cells within polygon
  # neigh_quan =  Neighborhood_quantile(extr_values=evi_district, PlantHarvestTable=PlantHarvest,Quant_percentile=0.95,
  #	 num_workers=17,spline_spar = 0)
  #save(neigh_quan, file = paste('/groups/manngroup/India_Index/Data/Intermediates/NDVI_neigh_quan.RData',sep='') )
  load('/groups/manngroup/India_Index/Data/Intermediates/NDVI_neigh_quan.RData')

  
  # make district maps
  districts@data = cbind(districts@data,unlist(neigh_quan))   # import neighborhood quantile values
  districts@data = cbind(districts@data,unlist(evi_summary_annual_av))
  districts = spTransform(districts, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  districts@data$id = rownames(districts@data)
  districts.df =  fortify(districts) 
  districts.df =  join(districts.df, districts@data, by="id",type = 'left')
  names(districts.df)[dim(districts.df)[2]]='A_mn'
  names(districts.df)[dim(districts.df)[2]-1]='neigh_quan'
  districts.df$neigh_quan = districts.df$neigh_quan * 0.0001
  districts.df$A_mn = districts.df$A_mn * 0.0001

  # create plots of NDVI stats 
  ggplot() +  geom_polygon(data=districts.df[districts.df$hole==F,],aes(long,lat,fill= neigh_quan,group=as.factor(id) )) +
  coord_equal() +
  scale_fill_gradient2('5th Percentile NDVI ', low = 'red',mid='purple',high = 'green', midpoint=0.17) 
 
  ggplot() +  geom_polygon(data=districts.df[districts.df$hole==F,],aes(long,lat,fill= A_mn,group=as.factor(id) )) +
  coord_equal() +
  scale_fill_gradient2('Mean NDVI', low = 'red',mid='purple',high = 'green', midpoint=0.42)

  #ggsave(file="../../India-Index-Insurance-Code/WriteUp/Mean_district_NDVI.png")


# Merge EVI data with yields 
  districts$i = 1:length(districts)
  districts$district = districts$NAME_2

  for(i in 1:length(evi_summary)){
   	evi_summary[[i]]=join(evi_summary[[i]], districts@data[,c('i','district','NAME_0','NAME_1','NAME_2')]) # join shp to vegetation data
	evi_summary[[i]]$year = paste(format(evi_summary[[i]]$plant_dates,'%Y'),format(evi_summary[[i]]$harvest_dates,'%y'),sep='-')  #
        evi_summary[[i]]=join(evi_summary[[i]], yield[yield$crop=='WHEAT'& yield$season=="RABI",],type='left') #Rabi Kharif Rice Wheat
        evi_summary[[i]]$state =  evi_summary[[i]]$NAME_1  # deal with missing state and district names
        evi_summary[[i]]$district =  evi_summary[[i]]$NAME_2  # deal with missing state and district names
        evi_summary[[i]]=join(evi_summary[[i]], yield_punjab[,c('state','district','year','Whe_Yeild_kgha')],
		by=c('state','district','year'),type='left') #Rabi Kharif Rice Wheat
  }

  yield_evi = (do.call(rbind,evi_summary)) # merge all evi list elements
  yield_evi$season_length = as.numeric(yield_evi$harvest_dates -yield_evi$plant_dates)
  yield_evi$plant_dates = as.numeric(format(yield_evi$plant_dates,'%j'))
  yield_evi$harvest_dates = as.numeric(format(yield_evi$harvest_dates,'%j'))
  yield_evi$G_mx_dates = as.numeric(format(yield_evi$G_mx_dates,'%j'))
  # only works if you provided rice planting dates also
  yield_evi$rice_plant_dates = as.numeric(format(yield_evi$rice_plant_dates,'%j'))
  yield_evi$rice_harvest_dates = as.numeric(format(yield_evi$rice_harvest_dates,'%j'))
  yield_evi$R_mx_dates = as.numeric(format(yield_evi$R_mx_dates,'%j'))

  yield_evi$year_trend = as.numeric(yield_evi$row)
  # generate yield that combines both datasets National/PunjabState yeilds
  yield_evi$yield_tn_ha_dual = yield_evi$yield_tn_ha 
  yield_evi$yield_tn_ha_dual[yield_evi$NAME_1=='PUNJAB'] = yield_evi$Whe_Yeild_kgha[yield_evi$NAME_1=='PUNJAB'] / 1000 # convert from kg to m tons

  yield_evi = yield_evi[,c('i','years_id','NAME_0','NAME_1','district','season','area','production_tonnes','yield_tn_ha',
	'plant_dates','harvest_dates','season_length','A_mn','A_min','A_max','A_AUC',
	'A_Qnt','A_sd','A_max_Qnt','A_AUC_Qnt','G_mx_dates',
	'G_mn','G_min','G_mx','G_AUC','G_Qnt','G_mx_Qnt',
	'G_AUC_Qnt','G_AUC2','G_AUC_leading','G_AUC_trailing',
	'G_AUC_diff_mn','G_AUC_diff_90th','T_G_Qnt','G_sd','Whe_Yeild_kgha','yield_tn_ha_dual',
	"rice_plant_dates","rice_harvest_dates","R_mx_dates","R_mn","R_min","R_mx","R_AUC",
        "R_Qnt","R_mx_Qnt","R_AUC_Qnt","R_AUC2","R_AUC_leading","R_AUC_trailing"
	)]

  names(yield_evi)=c('i','years','country','state','district','season','area','production_tonnes','yield_tn_ha',
        'plant_dates','harvest_dates','season_length','VEG_annual_mean','VEG_annual_min','VEG_annual_max','VEG_annual_AUC',
	'VEG_annual_95th_prct','VEG_annual_sd','VEG_annual_max_95th_prct','VEG_annual_AUC_95th_prct','VEG_growing_max_date',
	'VEG_growing_mean','VEG_growing_min','VEG_growing_max','VEG_growing_AUC','VEG_growing_95th_prct','VEG_growing_max_95th_prct',
	'VEG_growing_AUC_95th_prct','VEG_growing_AUC_v2','VEG_growing_AUC_leading','VEG_growing_AUC_trailing',
        'VEG_growing_AUC_diff_mn','VEG_growing_AUC_diff_90th','VEG_all_growing_95th_prct','VEG_growing_sd','Whe_Yeild_kgha','yield_tn_ha_dual',
	"rice_plant_dates","rice_harvest_dates","R_mx_dates",'rice_growing_mean','rice_growing_min','rice_growing_max','rice_growing_AUC',
	'rice_growing_95th_prct','rice_growing_max_95th_prct','rice_growing_AUC_95th_prct','rice_growing_AUC_v2','rice_growing_AUC_leading',
	'rice_growing_AUC_trailing'
	)
  
  # evi or ndvi determined by original extracted data 
  write.csv(yield_evi,'/groups/manngroup/India_Index/Data/Intermediates/yield_ndvi.csv')
  write.csv(yield_evi,'/groups/manngroup/India_Index/India-Index-Insurance-Code/yield_ndvi.csv')

  # plot yields
  # aggregate yields by district
  yield_evi$country_state_district = paste(yield_evi$country,yield_evi$state,yield_evi$district,sep='')
  yield_tn_ha_dist = aggregate(yield_tn_ha~ country_state_district, data = yield_evi,FUN=mean)
  yield_tn_ha_dist2 = aggregate(Whe_Yeild_kgha~ country_state_district, data = yield_evi,FUN=function(x){mean(x,na.rm=T)})

  # make district maps
  districts@data$country_state_district = paste(districts@data$NAME_0,districts@data$NAME_1,districts@data$NAME_2,sep='')
  districts@data =  join(districts@data, yield_tn_ha_dist, by="country_state_district",type = 'left')
  districts@data =  join(districts@data, yield_tn_ha_dist2, by="country_state_district",type = 'left')
  districts.df2 =  fortify(districts)
  districts.df2 =  join(districts.df2, districts@data, by="id",type = 'left')

  # create plots of yield stats
  ggplot() +  geom_polygon(data=districts.df2[districts.df2$hole==F,],aes(long,lat,fill= yield_tn_ha,group=as.factor(id) )) +
  coord_equal() +
  scale_fill_gradient2('Yields (tons per hectare)', low = 'red',mid='orange',high = 'green', midpoint=4.25)

  ggplot() +  geom_polygon(data=districts.df2[districts.df2$hole==F,],aes(long,lat,fill= Whe_Yeild_kgha,group=as.factor(id) )) +
  coord_equal() +
  scale_fill_gradient2('Yields (tons per hectare)', low = 'red',mid='orange',high = 'green', midpoint=4250)

  y_plot = ggplot() +  geom_polygon(data=districts.df2[districts.df2$hole==F,],color='white',aes(long,lat,fill= yield_tn_ha,group=as.factor(id))) +
  coord_equal() +scale_fill_gradient2('Yields \n(T/ha)', low = 'grey10',mid='grey50',high = 'white',midpoint=3.75)

  ggsave(y_plot,file="../../India-Index-Insurance-Code/WriteUp/Yield_tn_ha_bw.png")


################################################ 
# machine learning 
source('..//..//India-Index-Insurance-Code//mctune.R')

set.seed(10)
yield_evi$countrystatedistrict=paste(yield_evi$country,yield_evi$state,yield_evi$district,sep='')
#yield_evi$yield_tn_ha[yield_evi$yield_tn_ha<1 |yield_evi$yield_tn_ha>6]=NA  # remove possible outliers


  formula = yield_tn_ha ~plant_dates+harvest_dates+season_length+VEG_annual_mean+VEG_annual_min+VEG_annual_max+VEG_annual_AUC+
    VEG_annual_95th_prct+VEG_annual_sd+VEG_annual_max_95th_prct+VEG_annual_AUC_95th_prct+VEG_growing_max_date+
    VEG_growing_mean+VEG_growing_min+VEG_growing_max+VEG_growing_AUC+VEG_growing_95th_prct+VEG_growing_max_95th_prct+
    VEG_growing_AUC_95th_prct+VEG_growing_AUC_v2+VEG_growing_AUC_leading+VEG_growing_AUC_trailing+
    VEG_all_growing_95th_prct+VEG_growing_sd +rice_plant_dates+rice_harvest_dates+R_mx_dates+rice_growing_mean+
    rice_growing_min+rice_growing_max+rice_growing_AUC+rice_growing_95th_prct+rice_growing_max_95th_prct+
    rice_growing_AUC_95th_prct+rice_growing_AUC_v2+rice_growing_AUC_leading+rice_growing_AUC_trailing +i

  set.seed(10)
  fit <- randomForest(formula, data= na.omit(model.frame(formula,yield_evi)), importance=T, ntree=2000)
  fit
  varImpPlot(fit)

  rf_ranges = list(ntree=c(seq(1,100,5),seq(1000,5000,500)),mtry=seq(5,25,1))
  set.seed(10)
  tuned.rf = tune(randomForest, train.x = formula, data = na.omit(model.frame(formula,yield_evi)),
         tunecontrol = tune.control(sampling = "cross",cross = 5), ranges=rf_ranges,
         mc.control=list(mc.cores=16, mc.preschedule=T),confusionmatrizes=T )


  tuned.rf$best.model
  plot(tuned.rf)
  #EVI % Var explained: 40.86  # 44.42 if punjab data is inserted   or if outliers removed 49.31
  #NDVI % Var explained: 46.94    47% with rice stats


  
  
#######################################################
# Panel regressions on yields -------------------------------
#######################################################
  
  
  yield_evi = read.csv('H://Projects/India_Index_Insurance/India_Index_Insurance_Code/yield_ndvi.csv',stringsAsFactors = F)
 # yield_evi = read.csv('C://Users/mmann/Downloads/yield_ndvi (1).csv')
  yield_evi = yield_evi[!is.na(yield_evi$years),]
  
  table(paste(yield_evi$i,yield_evi$years,sep='-'))
  yield_evi[yield_evi$i ==26 &yield_evi$years==2006,][2,] =NA
  yield_evi = yield_evi[!is.na(yield_evi$year),]
  yield_evi[ yield_evi$yield_tn_ha<1 | yield_evi$yield_tn_ha>6,'yield_tn_ha']= NA 
   
  
 # IMPORTANT: ORDER TO AVOID PROBLEMS WITH INDEX LATER  - plm sorts by name and year 
 yield_evi=yield_evi[with(yield_evi, order(district, years)), ]
  
  
  # formula_lm = yield_tn_ha ~plant_dates+harvest_dates+season_length+VEG_annual_mean+VEG_annual_min+VEG_annual_max+VEG_annual_AUC+
  #   VEG_annual_95th_prct+VEG_annual_sd+VEG_annual_max_95th_prct+VEG_annual_AUC_95th_prct+VEG_growing_max_date+
  #   VEG_growing_mean+VEG_growing_min+VEG_growing_max+VEG_growing_AUC+VEG_growing_95th_prct+VEG_growing_max_95th_prct+
  #   VEG_growing_AUC_95th_prct+VEG_growing_AUC_v2+VEG_growing_AUC_leading+VEG_growing_AUC_trailing+
  #   VEG_all_growing_95th_prct+VEG_growing_sd +rice_plant_dates+rice_harvest_dates+R_mx_dates+rice_growing_mean+
  #   rice_growing_min+rice_growing_max+rice_growing_AUC+rice_growing_95th_prct+rice_growing_max_95th_prct+
  #   rice_growing_AUC_95th_prct+rice_growing_AUC_v2+rice_growing_AUC_leading+rice_growing_AUC_trailing +factor(i)
  # 
  # lm1=  lm(formula_lm,data=yield_evi)
  # summary(lm1)
  # mean((yield_evi$yield_tn_ha - predict(lm1, yield_evi))^2)/mean(yield_evi$yield_tn_ha)  
  # 
  # lm_0=  lm(yield_tn_ha ~factor(i),data=yield_evi)
  # summary(lm_0)

  formula = yield_tn_ha ~plant_dates+harvest_dates+season_length+
    VEG_annual_sd+VEG_annual_max_95th_prct+VEG_annual_AUC_95th_prct+VEG_growing_max_date+
    VEG_growing_mean+VEG_growing_min+VEG_growing_max+VEG_growing_AUC+VEG_growing_95th_prct+VEG_growing_max_95th_prct+
    VEG_growing_AUC_95th_prct+VEG_growing_AUC_v2+VEG_growing_AUC_leading+VEG_growing_AUC_trailing+
    VEG_all_growing_95th_prct+VEG_growing_sd+R_mx_dates+rice_growing_mean+
    rice_growing_min+rice_growing_max+rice_growing_AUC+rice_growing_95th_prct+rice_growing_max_95th_prct+
    rice_growing_AUC_95th_prct+rice_growing_AUC_v2+rice_growing_AUC_leading+rice_growing_AUC_trailing
  
  fixed <- plm(formula, data=yield_evi, index=c("district", "years"), model="within")
  summary(fixed)  

  random <- plm(formula, data=yield_evi, index=c("district", "years"), model="random")
  summary(random)  
  
  phtest(fixed, random) # use fixed if significant
  
  
  formula2 = yield_tn_ha ~plant_dates+harvest_dates+
    VEG_annual_max_95th_prct+VEG_annual_AUC_95th_prct+VEG_growing_max_date+
    VEG_growing_mean+VEG_growing_min+VEG_growing_max+VEG_growing_95th_prct+VEG_growing_max_95th_prct+
    VEG_growing_AUC_95th_prct+VEG_growing_AUC_v2+
    R_mx_dates+rice_growing_mean+
    rice_growing_min+rice_growing_AUC+rice_growing_95th_prct+
    rice_growing_AUC_v2+rice_growing_AUC_leading+rice_growing_AUC_trailing+I(as.numeric(years))
  
  formula2_dataframe = yield_tn_ha ~plant_dates+harvest_dates+
    VEG_annual_max_95th_prct+VEG_annual_AUC_95th_prct+VEG_growing_max_date+
    VEG_growing_mean+VEG_growing_min+VEG_growing_max+VEG_growing_95th_prct+VEG_growing_max_95th_prct+
    VEG_growing_AUC_95th_prct+VEG_growing_AUC_v2+
    R_mx_dates+rice_growing_mean+
    rice_growing_min+rice_growing_AUC+rice_growing_95th_prct+
    rice_growing_AUC_v2+rice_growing_AUC_leading+rice_growing_AUC_trailing+I(as.numeric(years))+district+years
  
  
  #removed  season_length VEG_annual_sd VEG_growing_AUC_leading+VEG_growing_AUC_trailing++VEG_growing_AUC rice_growing_max VEG_growing_sd rice_growing_AUC_95th_prct rice_growing_max_95th_prct VEG_all_growing_95th_prct
  
  random <- plm(formula2, data=yield_evi, index=c("district", "years"), model="random")
  summary(random) 

  # plot fitted vs actual
  library(reshape)
  fitted = data.frame(fitted = random$model[[1]] - random$residuals)
  model_data = cbind(random$model,fitted)
  model_data = cbind(model_data,na.omit(model.frame(formula2_dataframe,yield_evi)))  
  model_data$district = as.character(model_data$district)
  model_data$years_id = as.numeric(substr(model_data$year,1,4))
  model_data = model_data[,c('district','years_id','yield_tn_ha','fitted')]
  model_data = melt(model_data,id = c('years_id','district'))
  windows()
  ggplot(data=model_data,aes(x=years_id,y=value,colour=variable,alpha=0.5))+
    geom_point(size=2) + facet_wrap( ~ district )+xlab('Year')+ylab('Wheat Tons / ha')+ theme(legend.position="none")
  
  
  # USE PCA Panel Regression -----------------------------------------
  
  pca_input = na.omit(model.frame(formula2_dataframe,yield_evi))
  pca_data = pca_input[,sapply(pca_input,is.numeric)] # limit to numeric number data
  pca_data = pca_data[,!(names(pca_data) %in% c('area','production_tonnes','yield_tn_ha'))] # remove dependent variable data
  pca = prcomp( pca_data, scale = T,center = T ) 
  summary(pca)
  pca_pred = as.data.frame(stats::predict(pca))
  dim(pca_pred);dim(pca_input)
  pca_pred$district = pca_input$district
  pca_pred$years = pca_input$years
  pca_pred$yield_tn_ha =pca_input$yield_tn_ha
  
  # IMPORTANT: ORDER TO AVOID PROBLEMS WITH INDEX LATER 
  pca_pred=pca_pred[with(pca_pred, order(district, years)), ]
  
  # add a time lag 
  pca_pred <- pdata.frame(pca_pred, index = c("district", "years"))
  pca_pred$lag1_yield_tn_ha = lag(pca_pred$yield_tn_ha,1)
  pca_pred$diff1_yield_tn_ha = diff(pca_pred$yield_tn_ha,1)
  pca_pred$diff1_lag1_yield_tn_ha = lag(pca_pred$diff1_yield_tn_ha,1)
  
  pca_pred$diff1_PC1 = diff(pca_pred$PC1,1)
  pca_pred$lag1_PC1 = lag(pca_pred$PC1,1)
  pca_pred$lag1_PC2 = lag(pca_pred$PC2,1)
  pca_pred$lag1_PC3 = lag(pca_pred$PC3,1)
  pca_pred$lag1_PC4 = lag(pca_pred$PC4,1)
  pca_pred$lag1_PC5 = lag(pca_pred$PC5,1)
  
   # clusters = hclust(dist(pca_pred[,1:3])^2,method = 'complete') # no improvement 
  # plot(clusters)
  # clusterCut <- cutree(clusters, 2)
    
  # formulas
  formula_PCA =yield_tn_ha~lag1_PC1+ rcs(PC1,4)+rcs(PC2,4)+rcs(PC3,4)+rcs(PC4,4)+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19 +PC20+PC21
  formula_PCA_dataframe = yield_tn_ha~lag1_PC1+ rcs(PC1,4)+rcs(PC2,4)+rcs(PC3,4)+rcs(PC4,4)+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21 +district+years # add id and year for model.frame
  # estimate
  
  random_pca <- plm(formula_PCA, data=pca_pred, index=c("district", "years"), model="random")
  summary(random_pca) 
  
  # get prediction and and model.frame 
  fitted_pca = data.frame(fitted = random_pca$model[[1]] - random_pca$residuals)
  model_data_pca = cbind(as.data.frame(as.matrix(random_pca$model)),fitted_pca)
  model_data_pca = cbind(model_data_pca,na.omit(model.frame(formula_PCA_dataframe,pca_pred)))  
  model_data_pca$district = as.character(model_data_pca$district)
  model_data_pca$years_id = as.numeric(substr(model_data_pca$year,1,4))
  model_data_pca = model_data_pca[,c('district','years_id','yield_tn_ha','fitted')]
  model_data_pca = melt(model_data_pca,id = c('years_id','district'))
  model_data_pca$variable= as.character(model_data_pca$variable)
  model_data_pca$variable[model_data_pca$variable=='fitted']='Fitted Temporal'
  
  windows()
  ggplot(data=model_data_pca,aes(x=as.factor(years_id),y=value,colour=variable,alpha=0.5))+
    geom_point(size=2) + facet_wrap( ~ district )+xlab('Year')+ylab('Wheat Tons / ha')+ theme(legend.position="none")+ 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  
  # calculate within R2 http://forums.eviews.com/viewtopic.php?t=4709
  SSR_FULL = sum(random_pca$residuals^2)
  SSR_FE = sum( plm(yield_tn_ha ~ 1 +as.factor(district) , data=pca_pred, index=c("district", "years"), model="pooling")$residuals^2)
  Witin_R2 =  1 - (SSR_FULL/SSR_FE)  
  Witin_R2   #0.6632103 with lag pc1  # 0.532181 with lag of pc1 and pc2 #0.6712662 with pc1 lag and yield 1dif lag
  
  
  # project new data onto the PCA space another way
  #pca_pred2=scale(newdata, pca$center, pca$scale) %*% pca$rotation 
  
  
# Spatial Panel Regression  -----------------------------------------------
  library(spdep)
  library(rgoes)
  library(splm)
  # splm only use balanced panel, dataframe[id,time,Y,X]
  
  # get District outlines
  districts_plm = readOGR('H:/Projects/India_Index_Insurance/Data/Admin Boundaries','PunjabHaryanaDistricts')
  districts_plm = spTransform(districts_plm, CRS('+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'))
  districts_plm$NAME_2 = toupper(as.character(districts_plm$NAME_2)) 
  
  table(pca_pred$district)
  pca_pred_splm = as.data.frame(pca_pred)
  
  # find all districts with 10 years
  balanced_panel = as.character(as.data.frame(table(pca_pred_splm$district))$Var1[as.data.frame(table(pca_pred_splm$district))$Freq ==10])
  # confirm balanced 
  table(pca_pred$years[pca_pred$district %in% balanced_panel])
  pca_pred_balanced = pca_pred_splm[pca_pred_splm$district %in% balanced_panel,]
  
  # IMPORTANT: ORDER TO AVOID PROBLEMS WITH INDEX LATER 
  pca_pred_balanced=pca_pred_balanced[with(pca_pred_balanced, order(district, years)), ]
  
  # Remove all districts not in balanced panel
  districts_plm = districts_plm[as.character(districts_plm@data$NAME_2) %in% as.character(unique(pca_pred_balanced[,'district'])),]
  districts_polyNB = poly2nb(districts_plm,row.names = row.names(districts_plm)) # polygon continuity$GEOID10
  Wneigh = nb2mat(districts_polyNB, style='W')
  districts_polyListw = nb2listw(districts_polyNB) 
  
  # effected by neighbor values  
  mn_neigh=by(pca_pred_balanced$yield_tn_ha,INDICES=pca_pred_balanced$years, FUN = function(x){mean_neighbors(x,sweights=Wneigh)})
  mn_neigh=as.numeric(unlist(mn_neigh))
  pca_pred_balanced$splag_yield_tn_ha = mn_neigh
  #row.names(pca_pred_balanced) = 1:dim(pca_pred_balanced)[1]
  
  #pca_pred_balanced$district=as.character(pca_pred_balanced$district)
  #pca_pred_balanced$years=as.character(pca_pred_balanced$years)
  
  
  # panel with spatial neighbors lag 
  formula_PCA_splag = yield_tn_ha~ splag_yield_tn_ha+rcs(PC1,4)+rcs(PC2,4)+rcs(PC3,4)+rcs(PC4,4)+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19 +PC20+PC21
  formula_PCA_splag_dataframe = yield_tn_ha~splag_yield_tn_ha+rcs(PC1,4)+rcs(PC2,4)+rcs(PC3,4)+rcs(PC4,4)+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19 +PC20+PC21+district+years 
  
  random_pca_splag = plm(formula_PCA_splag, data=pca_pred_balanced, index=c("district", "years"), model="random")
  summary(random_pca_splag) 
  

  
  # calculate within R2 http://forums.eviews.com/viewtopic.php?t=4709
  SSR_FULL = sum(random_pca_splag$residuals^2)
  SSR_FE = sum( plm(yield_tn_ha ~ 1 +as.factor(district) , data=pca_pred_balanced, index=c("district", "years"), model="pooling")$residuals^2)
  Witin_R2 =  1 - (SSR_FULL/SSR_FE) # 0.5760017
  Witin_R2  #0.7345237
  
  # plot spatial lag panel regression 
  fitted_pca_splag = data.frame(fitted = random_pca_splag$model[[1]] - random_pca_splag$residuals)
  model_data_pca_splag = cbind(as.data.frame(as.matrix(random_pca_splag$model)),fitted_pca_splag)
  model_data_pca_splag = cbind(model_data_pca_splag,na.omit(model.frame(formula_PCA_splag_dataframe,pca_pred_balanced)))  
  model_data_pca_splag$district = as.character(model_data_pca_splag$district)
  model_data_pca_splag$years_id = as.numeric(substr(model_data_pca_splag$year,1,4))
  model_data_pca_splag = model_data_pca_splag[,c('district','years_id','yield_tn_ha','fitted')]
  model_data_pca_splag = melt(model_data_pca_splag,id = c('years_id','district'))
  
  windows()
  ggplot(data=model_data_pca_splag,aes(x=as.factor(years_id),y=value,colour=variable,alpha=0.5))+
    geom_point(size=2) + facet_wrap( ~ district )+xlab('Year')+ylab('Wheat Tons / ha')+ theme(legend.position="none")+ 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  
  # SPLM - Spatial lag estimation  -------------------------------------------------
  formula_PCA_splag2 = yield_tn_ha~ rcs(PC1,4)+rcs(PC2,4)+rcs(PC3,4)+rcs(PC4,4)+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19 +PC20+PC21
  formula_PCA_splag2_dataframe = yield_tn_ha~ rcs(PC1,4)+rcs(PC2,4)+rcs(PC3,4)+rcs(PC4,4)+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+district+years
  
  sararremod <- spml(formula_PCA_splag2, data = pca_pred_balanced, index = c("district", "years"),
                     listw = districts_polyListw, model = "random", lag = TRUE, spatial.error = "b")
  summary(sararremod)
  summary(sararremod)$rsqr
  # calculate within R2 http://forums.eviews.com/viewtopic.php?t=4709
  SSR_FULL = sum(sararremod$residuals^2)
  SSR_FE = sum( spml(yield_tn_ha~1 +as.factor(district), data = pca_pred_balanced, index = c("district", "years"),
                     listw = districts_polyListw, model = "random", lag = F, spatial.error = "none")$residuals^2)
  Witin_R2 =  1 - (SSR_FULL/SSR_FE)    
  Witin_R2  
  
  # plot spatial lag panel regression 
  fitted_pca_sararremod = data.frame(fitted = sararremod$model[[1]] - sararremod$residuals)
  fitted_pca_sararremod = cbind(as.data.frame(as.matrix(sararremod$model)),fitted_pca_splag)
  fitted_pca_sararremod = cbind(fitted_pca_sararremod,na.omit(model.frame(formula_PCA_splag2_dataframe,pca_pred_balanced)))  
  fitted_pca_sararremod$district = as.character(fitted_pca_sararremod$district)
  fitted_pca_sararremod$years_id = as.numeric(substr(fitted_pca_sararremod$year,1,4))
  fitted_pca_sararremod = fitted_pca_sararremod[,c('district','years_id','yield_tn_ha','fitted')]
  fitted_pca_sararremod = melt(fitted_pca_sararremod,id = c('years_id','district'))
  fitted_pca_sararremod$variable= as.character(fitted_pca_sararremod$variable)
  fitted_pca_sararremod$variable[fitted_pca_sararremod$variable=='fitted']='Fitted Spatial'
  
  windows()
  ggplot(data=fitted_pca_sararremod,aes(x=as.factor(years_id),y=value,colour=variable,alpha=0.5))+
    geom_point(size=2) + facet_wrap( ~ district )+xlab('Year')+ylab('Wheat Tons / ha')+ theme(legend.position="none")+ 
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))

  # Plot temporal and spatial fit  ------------------------------------------
  
  space_time_fit = rbind(model_data_pca,fitted_pca_sararremod)
  space_time_fit = space_time_fit[space_time_fit$district %in% unique(fitted_pca_sararremod$district),]
  
  windows()
  ggplot(data=space_time_fit,aes(x=as.factor(years_id),y=value,colour=variable,alpha=0.5))+
    geom_point(size=2)+ scale_colour_manual(values = c("purple","#66d9ff", "#ff8080")) + facet_wrap( ~ district )+xlab('Year')+ylab('Wheat Tons / ha')+  
    theme(axis.text.x  = element_text(angle=90, vjust=0.5))
  
  
  
  
  
  
  
  
  # Basic Scatterplot Matrix
  pairs(yield_tn_ha~A_mn+A_min+A_max+A_AUC+G_mx_dates+G_mn+G_min+G_mx+G_AUC+G_AUC_leading
        +G_AUC_trailing+season_length+year_trend, data =yield_evi,main="Simple Scatterplot Matrix")












#####################################################################
# Summarize subdistricts 
  load('./WO Clouds Crops Clean/EVI_stack_h24v05_wo_clouds_crops_clean.RData')
  load('./WO Clouds Crops Clean/EVI_stack_h24v06_wo_clouds_crops_clean.RData')

  load('./WO Clouds Crops Clean/NDVI_stack_h24v05_wo_clouds_crops_clean.RData')
  load('./WO Clouds Crops Clean/NDVI_stack_h24v06_wo_clouds_crops_clean.RData')



  # get District outlines
  ogrInfo('../Admin Boundaries/IND_adm_shp/','IND_adm3')
  sub_districts = readOGR('../Admin Boundaries/IND_adm_shp/','IND_adm3')
  sub_districts = spTransform(sub_districts, CRS('+proj=sinu +a=6371007.181 +b=6371007.181 +units=m'))
  sub_districts$NAME_2 = toupper(as.character(sub_districts$NAME_2))
  sub_districts$NAME_3 = toupper(as.character(sub_districts$NAME_3))
  sub_districts=sub_districts[sub_districts$NAME_1=='Haryana' |sub_districts$NAME_1=='Punjab',]
  sub_districts=sub_districts[sub_districts$NAME_2 %in% c("FATEHABAD","SIRSA","YAMUNANAGAR",
	"FATEHGARH SAHIB","LUDHIANA","PATIALA"),]


  # extract raster data for districts and save
  sub_dist_NDVI = extract_value_point_polygon(sub_districts,list(NDVI_stack_h24v06,NDVI_stack_h24v05),16)
  save(sub_dist_NDVI, file = paste('/groups/manngroup/India_Index/Data/Intermediates/out_subdistricts_ndvi.RData',sep='') )
  sub_dist_EVI = extract_value_point_polygon(sub_districts,list(EVI_stack_h24v06,EVI_stack_h24v05),16)
  save(sub_dist_EVI, file = paste('/groups/manngroup/India_Index/Data/Intermediates/out_subdistricts_evi.RData',sep='') )
  load('/groups/manngroup/India_Index/Data/Intermediates/out_subdistricts_evi.RData' )
  load('/groups/manngroup/India_Index/Data/Intermediates/out_subdistricts_ndvi.RData' )


  # Get planting and harvest dates
  PlantHarvest = PlantHarvestDates(dates[1],dates[2],PlantingMonth=11,
        PlantingDay=23,HarvestMonth=4,HarvestDay=30)

  # Get summary statistics lists
  extr_values=sub_dist_NDVI
  PlantHarvestTable = PlantHarvest
  Quant_percentile=0.05
  num_workers = 16
  spline_spar = 0
  evi_summary =  Annual_Summary_Functions(extr_values, PlantHarvestTable,Quant_percentile, aggregate=T, return_df=T)


 # Merge EVI data with polygons
  sub_districts$i = 1:length(sub_districts)
#  sub_districts$sub_district = sub_districts$NAME_3

  for(i in 1:length(evi_summary)){
        evi_summary[[i]]=join(evi_summary[[i]], sub_districts@data[,c('i','NAME_0','NAME_1','NAME_2','NAME_3')])
        evi_summary[[i]]$year = paste(format(evi_summary[[i]]$plant_dates,'%Y'),
	format(evi_summary[[i]]$harvest_dates,'%y'),sep='-')
  }

  evi_summary = do.call(rbind,evi_summary)
  evi_summary = evi_summary[,c('i','year','NAME_0','NAME_1','NAME_2','NAME_3',
        'plant_dates','harvest_dates','G_mx','G_mx_dates', 'G_mn')]

  names(evi_summary)=c('i','year','country','state','district','sub_district',
	'plant_dates','harvest_dates','EVI_growing_max','EVI_growing_max_date','EVI_growing_mean')
  evi_summary$year = as.numeric(substr(evi_summary$year,1,4))


  # Export mean 8 day values by subdistrict
  EVI_mean_sub_dist = lapply(1:length(extr_values), function(x) as.data.frame(colMeans(extr_values[[x]])))
  EVI_mean_sub_dist = lapply(1:length(EVI_mean_sub_dist), function(x) cbind(EVI_mean_sub_dist[[x]],
	strptime( gsub("^.*X([0-9]+).*$", "\\1", row.names(EVI_mean_sub_dist[[x]])),format='%Y%j')))   # assign dates
  for(x in 1:length(EVI_mean_sub_dist)){names(EVI_mean_sub_dist[[x]])=c('EVI_MN','date') # clean names
  	EVI_mean_sub_dist[[x]]$year =format( EVI_mean_sub_dist[[x]]$date,'%Y')
	EVI_mean_sub_dist[[x]]$i = x
	}
  EVI_mean_sub_dist = na.omit(do.call(rbind,EVI_mean_sub_dist))

  EVI_mean_sub_dist_join = join(EVI_mean_sub_dist,evi_summary)
  EVI_mean_sub_dist_join = EVI_mean_sub_dist_join[,c('i',"date","year","country","state","district","sub_district",
	"plant_dates","harvest_dates","EVI_MN","EVI_growing_max","EVI_growing_max_date","EVI_growing_mean")]

  #names(EVI_mean_sub_dist_join)=c('i',"date","year","country","state","district","sub_district",
        "plant_dates","harvest_dates","NDVI_MN","NDVI_growing_max","NDVI_growing_max_date","NDVI_growing_mean")

  write.csv(EVI_mean_sub_dist_join,'/groups/manngroup/India_Index/India-Index-Insurance-Code/yield_ndvi_simplified.csv')


  # Figure out january outlier issue
  FATEHABAD = sub_districts[sub_districts$NAME_2=='FATEHABAD'& sub_districts$NAME_3=='FATEHABAD',]

  par(mfrow=c(1,3),oma=c(0,0,2,0))
  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2002361')]],add=T)
  plot(FATEHABAD,add=T)
  title('2002361')
 
  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2003001')]],add=T) 
  plot(FATEHABAD,add=T)
  title('2003001')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2003009')]],add=T)
  plot(FATEHABAD,add=T)
  title('2003009')
  title("FATEHABAD,FATEHABAD", outer=TRUE)
  
  #----
  FATEHABAD = sub_districts[sub_districts$NAME_2=='FATEHABAD'& sub_districts$NAME_3=="FATEHABAD",]

  par(mfrow=c(1,3),oma=c(0,0,2,0))
  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2015353')]],add=T)
  plot(FATEHABAD,add=T)
  title('2015353')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2015361')]],add=T)
  plot(FATEHABAD,add=T)
  title('2015361')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2016001')]],add=T)
  plot(FATEHABAD,add=T)
  title('2016001')
  title("FATEHABAD,FATEHABAD", outer=TRUE)

 #----
  FATEHABAD = sub_districts[sub_districts$NAME_2=='FATEHABAD'& sub_districts$NAME_3=="TOHANA",]

  par(mfrow=c(1,3),oma=c(0,0,2,0))
  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2002361')]],add=T)
  plot(FATEHABAD,add=T)
  title('2015353')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2003001')]],add=T)
  plot(FATEHABAD,add=T)
  title('2015361')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2003009')]],add=T)
  plot(FATEHABAD,add=T)
  title('2016001')
  title("FATEHABAD,TOHANA", outer=TRUE)

#----
  FATEHABAD = sub_districts[sub_districts$NAME_2=='FATEHABAD'& sub_districts$NAME_3=="TOHANA",]

  par(mfrow=c(1,3),oma=c(0,0,2,0))
  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2008353')]],add=T)
  plot(FATEHABAD,add=T)
  title('2015353')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2008361')]],add=T)
  plot(FATEHABAD,add=T)
  title('2015361')

  plot(FATEHABAD)
  plot(EVI_stack_h24v06[[which(names(EVI_stack_h24v06)=='X2009001')]],add=T)
  plot(FATEHABAD,add=T)
  title('2016001')
  title("FATEHABAD,TOHANA", outer=TRUE)


##############################################################################################

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



