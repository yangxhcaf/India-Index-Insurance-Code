# this function breaks stacks into managable sections to process 
# example values


RasterChunkProcessingSave = function(in_stack,in_stack_nm, block_width,
	worker_number,out_path,out_nm_postfix, FUN){
  #Determine optimal block size for loading  stack data
    nrows = dim(in_stack)[1]
    nblocks = nrows%/%block_width 
    bs_rows = seq(1,nblocks*block_width+1,block_width)
    bs_nrows =  rbind(matrix(block_width,length(bs_rows)-1,1),nrows-bs_rows[length(bs_rows)]+1)
    print('Working on the following rows')
    print(paste(bs_rows))

  # Register parallel backend
    stopImplicitCluster()
    registerDoParallel(worker_number)

#############################################
  print('getting values and running custom function')
  result = foreach(i = 1:length(bs_rows), .combine = rbind,.packages='raster') %dopar% {
      # get values, run function, and save
      value = getValues(in_stack, row= bs_rows[i], nrow= bs_nrows[i])
      value = FUN(value)
      #return(value)
      print(paste("Saving values for row",i))
      save(value,file = paste(out_path,"CleanDataSave_",sprintf("%05d", bs_rows[i]),sep = ""))
    }
    stopImplicitCluster()
    registerDoParallel(worker_number)


#############################################
  print('Load saved data outputs and put in order WRITE CSV ATTEMPT')
  f = list.files(out_path,'CleanDataSave*')
  #put files in ascending order
  row_id = gsub("^.*_([0-9]{5}).*$", "\\1",f,perl = T)
  f =f[order(row_id)]
  load(paste(out_path,f[1],sep=''))  # load first row group
  write.table(value,file=paste(out_path,'CleanCSV.csv',sep=''),col.names=T,sep=',')

  lapply(2:length(f),function(h){ load(paste(out_path,f[h],sep=''))
    print(paste(f[h]))
    write.table(value,file=paste(out_path,'CleanCSV.csv',sep=''),col.names=F,sep=',',append=T)
    rm(value) })


#############################################
  print('Load saved data outputs and put in order MERGE RDATA FILES')
  f = list.files(out_path,'CleanDataSave*')
  #put files in ascending order
  row_id = gsub("^.*_([0-9]{5}).*$", "\\1",f,perl = T)
  all.files = f[order(row_id)]
  all.files.s = split(all.files, ceiling(seq_along(all.files)/50)) # break data into managable chunks

  # load data into workspace 
  registerDoParallel(worker_number)  
  require(ff)
  require(ffbase)
  load(file = paste(out_path,all.files.s[[1]][1],sep=''))
  valueff = as.ffdf(as.data.frame(value))

  for(i in 1:length(all.files.s)){
    # work on first 1/2 of files 
    mylist = foreach(xxx = all.files.s[[i]], .inorder=T, .errorhandling='stop',.packages='ff') %do% {
       print(xxx)
       load(file = paste(out_path,xxx,sep=''))
       value = as.data.frame(value)
       valueff = ffdfappend(valueff,value,adjustvmode=F)
      #return(as.ffdf(as.data.frame(value)))
      # return('1')
    }
    print(' loading into env ')
    names(mylist) <- all.files.s[[i]]
    

    #mylist2 = rbindlist(mylist)   #use data.table
    #save(mylist2,file= paste(out_path,'combinedata',i,'.RData',sep='') )
    #rm(list=c(mylist,mylist2))
    # list2env(mylist ,.GlobalEnv)  #write all objects to env
  }


  # work on second 1/2 of files
  mylist2 = foreach(xxx = all.files.s[[2]], .inorder=T) %dopar% {
    print(xxx)
    load(file = paste(out_path,xxx,sep=''))
    return(value)
  }
  names(mylist2) <- all.files.s[[2]]
  list2env(mylist2 ,.GlobalEnv)





########################################
  # put back into raster
  print('Putting data back into stacks')
  value_list =  foreach( layer = 1:dim(result)[2],.packages='raster') %dopar% {
      r = in_stack[[1]]
      r = setValues(r, matrix(result[,layer],nrow=dim(r)[1],byrow=T))
      names(r) = colnames(result)[layer]
      return(r)
    }

#######################################
  # stack and save rasters
  print('Saving stacks')
  out_stack =  stack(value_list)
  assign(paste(in_stack_nm,'_',out_nm_postfix,sep=''),out_stack)
  save(out_stack, file = paste(out_path,in_stack_nm,'_',out_nm_postfix,'.RData',sep=''))
}



in_stack = NDVI_stack_h24v06
in_stack_nm = 'NDVI_stack_h24v06'
block_width = 10
worker_number = 16
out_path= './WO Clouds Crops Scaled/'
out_nm_postfix = 'WO_Clouds_Crops_Scaled'
setwd('/groups/manngroup/India_Index/Data/Data Stacks/')

ScaleClean = function(x){
        x[x==-3000]=NA
        x[x < -2000]=NA
        x[x > 1000]=NA
        x = x * 1e-04
        x
  }
 FUN = ScaleClean
#RasterChunkProcessingSave(in_stack,in_stack_nm, block_width,
#        worker_number,out_path,out_nm_postfix, FUN=ScaleClean)


 
