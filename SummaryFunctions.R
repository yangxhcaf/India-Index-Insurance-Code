# Functions        --------------------------------------------------------

 PlantHarvestDates = function(start_date,end_date,PlantingMonth,PlantingDay,HarvestMonth,HarvestDay){
    # this function takes in date range and returns planting and harvest date for time series
    # set planting
    start_end_years = c(strptime(start_date,'%Y-%m-%d'),strptime(end_date,'%Y-%m-%d'))
    names(unclass(start_end_years[1]))
    start_end_years[1]$mon=PlantingMonth-1
    start_end_years[1]$mday=PlantingDay
    planting = as.Date(seq(start_end_years[1],
      length=strptime(dates[2],'%Y-%m-%d')$year-strptime(dates[1],'%Y-%m-%d')$year,
      by='year'))
    # set harvest
    start_end_years[2]$year=start_end_years[1]$year+1    # set year equal to start year +1
    start_end_years[2]$mon=HarvestMonth-1
    start_end_years[2]$mday=HarvestDay
    harvest = as.Date(seq(start_end_years[2],
      length=strptime(end_date,'%Y-%m-%d')$year-strptime(start_date,'%Y-%m-%d')$year,
      by='year'))
    return(data.frame(planting=planting,harvest=harvest))
  }

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
    y  }

  AnnualMaxima = function(x,dates_in){
    # returns location of maximum value by year
    datesY = format(dates_in,'%Y')
    a=do.call(rbind,lapply(split(x,datesY),function(x)x[which.max(x)]))
    dates_in[x %in% a ]
  }

  AnnualMaximaValue = function(x,dates_in){
    # returns location of maximum value by year
    datesY = format(dates_in,'%Y')
    a=do.call(c,lapply(split(x,datesY),function(x)x[which.max(x)]))
    a}


  AnnualAggregator = function(x,dates_in,FUN){
    # returns an annual summary statistic of any function
    # Example AnnualAggregator(x=  plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates, FUN = function(y){mean($
    datesY = format(dates_in,'%Y')
    do.call(c,lapply(split(x,datesY),FUN))}


  PeriodAggregator = function(x,dates_in,date_range_st, date_range_end,by_in='days',FUN){
    # returns a summary statistic of x for the period defined by date_range_st, date_range_end
    if(class(date_range_st)[1]== "POSIXct" ){date_range_st = as.Date(date_range_st)
                                             date_range_end = as.Date(date_range_end)}
    dataout=lapply(1:length(date_range_st),function(z){
    	DateRange = seq(date_range_st[z],date_range_end[z],by=by_in)
    	x=x[dates_in %in% DateRange]
    	dates_in=dates_in[dates_in %in% DateRange]
    	FUN(x)})
    	dataout = do.call(c,dataout)
	names(dataout)=format(date_range_st,'%Y')
	dataout
    }


  AnnualMinumumNearDOY = function(x,dates_in,DOY_in){
    #x = EVI values, dates=dates of observation POSIX, DOY_in = DOY of rain onset as.Date
    tempDOY = as.POSIXlt(DOY_in)
    # avoid problems with time class
    if(is.na(tempDOY[1])){print('ERROR: convert date format to as.Date()');break}
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


  AnnualAverageDOYvalues = function(x,dates_in){
    # calculates the average value for DOY for the whole series
    datesj = format(dates_in,'%j')
    do.call(c,lapply(split(x,datesj),function(y){mean(y,na.rm=T)}))}



  AnnualAUC = function(x,dates_in){
         # calculate area under the curve by year
         FUN = function(q,w){auc(q,w,type='spline')}
         datesY = format(dates_in,'%Y')
         data.split = split(x,datesY)
         date.split = split(as.numeric(dates_in),datesY)
         dataout = do.call(c,lapply(1:length(data.split),function(z){
                FUN(q=date.split[[z]],w=data.split[[z]])} ))
         names(dataout)=unique(datesY)
         dataout
	}


 AnnualMinumumBeforeDOY = function(x,dates_in,DOY_in,days_before){
    # calculates the annual minimum for days_before days before each DOY for planting season
    # best to set DOY as the last expected date of planting
    if(days_before<=8){print('Using less than 8 days is dangerous, 15-30 stable')}
    #x = EVI values, dates=dates of observation POSIX, DOY_in = DOY of planting harvest
    tempDOY = as.POSIXlt(DOY_in)
    # avoid problems with time class
    if(is.na(tempDOY[1])){print('ERROR: convert date format to %Y%j');break}
    if(class(dates_in)[1]!= 'POSIXlt' ){dates_in=as.POSIXlt(dates_in)}
    # limit to fixed # of days before DOY
    DOY_before = tempDOY
    #names(unclass(DOY_before[1]))
    DOY_before$mday=DOY_before$mday-days_before      # set days before to doy - days_before
    DOY_table = data.frame(DOY_before=DOY_before,DOY_in=as.POSIXlt(DOY_in))   #match DOY with Days_be$
    # get all days 'days_before' DOY_in in a list
    DOY_interest = as.POSIXlt(unlist(lapply(1:dim(DOY_table)[1],function(h){format(seq(DOY_table[h,1],
                DOY_table[h,2],by='day'),'%Y-%m-%d')})),tz='UTC')
    # find all local minima, and match with DOY
    x_DOY_interest = x[dates_in %in% DOY_interest]
    dates_DOY_interest = dates_in[dates_in %in% DOY_interest]
    # get min value for this period for each year
    sort(AnnualMaxima(x_DOY_interest*-1,as.Date(dates_DOY_interest)))
 }


   PeriodAUC = function(x_in,dates_in,DOY_start_in,DOY_end_in){
         # calculate area under the curve by period of the year
         # x = data, dates_in=asDate(dates),DOY_start=asDate(list of start periods),DOY_end=asDate(list of end per$
         # x = plotdatasmoothed$EVI,dates_in = plotdatasmoothed$dates , DOY_start=annualMinumumBeforeDOY(x = plotd$

         dates_group = rep(0,length(dates_in))    # create storage for factors of periods
  	 # get sequences of periods of inerest
         seq_interest = lapply(1:length(DOY_start_in),function(z){seq(DOY_start_in[z],DOY_end_in[z],by='days')})
         # switch dates-group to period group
         years_avail = sort(as.numeric(unique(unlist(
                lapply(seq_interest,function(z) format(z,'%Y'))))))
         for(z in 1:length(seq_interest)){        #assigns year for beginging of planting season
		dates_group[dates_in %in% seq_interest[[z]]]=years_avail[z]
                assign('dates_group',dates_group,envir = .GlobalEnv) }  # assign doesn't work in lapply using for loop instead
	 # calculate AUC for periods of interest
         FUN = function(q,w){auc(q,w,type='spline')}
         datesY = format(dates_in,'%Y')
         data.split = split(x_in,dates_group)
         d = do.call(c,lapply(2:length(data.split),function(z){   # start at 2 to avoid group=0
		FUN(q=1:length(data.split[[z]]),w=data.split[[z]]) }))
         names(d) = names(data.split)[2:length(data.split)]
	 #print(cbind(names(data.split)[2:length(data.split)], d))
         d
	}




