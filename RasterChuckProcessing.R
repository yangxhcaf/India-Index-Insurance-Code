# this function breaks stacks into managable sections to process 
# example values


RasterChunkProcessing = function(in_stack,in_stack_nm, block_width,
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

  print('getting values and running custom function')
  result = foreach(i = 1:length(bs_rows), .combine = rbind,.packages='raster') %dopar% {
      # get values, run function, and save
      print(paste('Get values apply function',i))
      value = getValues(in_stack, row= bs_rows[i], nrow= bs_nrows[i])
      value = FUN(value)
      return(value)
    }

  # put back into raster
  print('Putting data back into stacks')
  value_list =  foreach( layer = 1:dim(result)[2],.packages='raster') %dopar% {
      print(paste('Put back into raster',i))
      r = in_stack[[1]]
      r = setValues(r, matrix(result[,layer],nrow=dim(r)[1],byrow=T))
      names(r) = colnames(result)[layer]
      return(r)
    }

  # Write out rasters  
    junk =  foreach(j = 1:length(value_list),.packages='raster') %dopar% {
      print(paste('write out raster ',j))
      writeRaster(value_list[[j]],paste(out_path,in_stack_nm,'_',out_nm_postfix,
	   gsub("^.*([0-9]{7}).*$", "\\1",names(value_list[[j]]),perl = T),
	   '.tif',sep=''),overwrite=T)
    }

  # stack and save rasters
  #print('Saving stacks')
  #out_stack =  stack(value_list)
  #assign(paste(in_stack_nm,'_',out_nm_postfix,sep=''),out_stack)
  #save(out_stack, file = paste(out_path,in_stack_nm,'_',out_nm_postfix,'.RData',sep=''))

}



#in_stack = EVI_stack_h24v06[[1:10]]
#in_stack_nm = 'EVI_stack_h24v06'
#block_width = 10
#worker_number = 8
#out_path= './WO Clouds Crops Scaled/'
#out_nm_postfix = 'WO_Clouds_Crops_Scaled'
#
#ScaleClean = function(x){
#        x[x==-3000]=NA
#        x[x < -2000]=NA
#        x[x > 1000]=NA
#        x = x * 1e-04
#        x
#  }
#
#RasterChunkProcessing(in_stack,in_stack_nm, block_width,
#        worker_number,out_path,out_nm_postfix, FUN=ScaleClean)
