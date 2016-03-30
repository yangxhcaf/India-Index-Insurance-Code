# Toy Example of using foreach to process a raster in parallel (cores, not jobs)

stack_loc = "bs_churchill"

options(error = recover) #setting the error option

require(raster)
.libPaths('/usr3/graduate/emelaas/R/x86_64-unknown-linux-gnu-library/2.14')
require(doParallel)

#Register the parallel backend
registerDoParallel(16)

#Grab directory of images and extract year, doy from each image
setwd(paste("/projectnb/modislc/users/dsm/landsat_stacks/",stack_loc,"/OVERLAP/",sep=""))
dir1 <- list.files(getwd(),pattern="^[L]")
yr <- unlist(lapply(dir1, function(x) as.numeric(substr(x, 10, 13))))
doy <- unlist(lapply(dir1, function(x) as.numeric(substr(x, 14, 16))))

#Extract number of rows and columns for Landsat stack
setwd(dir1[1])
stack_name <- Sys.glob(file.path("*_stack"))
hdr <- raster(stack_name)
ncols <- matrix(ncol(hdr))
nrows <- matrix(nrow(hdr))

#Stack all bands in raster cloud before loading into workspace
tmp_files <- unlist(lapply(dir1, function(x) 
  Sys.glob(paste("/projectnb/modislc/users/dsm/landsat_stacks/",stack_loc,"/OVERLAP/",
                 x, "/*_stack",sep=""))))

#Control number of landsat images in the stack
stack_height <- length(doy)

print('Band stacks are now loading...')
b1 <- stack(tmp_files[1:stack_height], bands=1) #stack all blue reflectance bands
b3 <- stack(tmp_files[1:stack_height], bands=3) #stack all red reflectance bands
b4 <- stack(tmp_files[1:stack_height], bands=4) #stack all NIR reflectance bands
b8 <- stack(tmp_files[1:stack_height], bands=8) #stack all fmask bands
print('Finished uploading band stacks to raster cloud.')

#Determine optimal block size for loading in Landsat stack data
bs <- blockSize(b1)

#Phenology algorithm
Landsat_Phenology <- function(evi_mat,doy,yr){
  
  #normalized EVI threshold for long-term mean phenology date used to calculate annual phenology
  thresh <- 0.5
  
  #Initialize Phenology matrices
  phenoSPR <- matrix(0,nrow(evi_mat),31) #annual spring phenology (1982-2012)
  phenoAUT <- matrix(0,nrow(evi_mat),31) #annual autumn phenology
  ltmSPR <- matrix(0,nrow(evi_mat),1) #long-term mean spring phenology date
  ltmAUT <- matrix(0,nrow(evi_mat),1) #long-term mean autumn phenology date
  bline <- matrix(0,nrow(evi_mat),1) #long-term mean winter background EVI
  hline <- matrix(0,nrow(evi_mat),1) #long-term mean summer maximum EVI
  rsmooth <- matrix(0,nrow(evi_mat),1) #correlation between observed and smoothed EVI
  nobs <- matrix(0,nrow(evi_mat),1) #number of available EVI observations used
  
  SPR_thresh = matrix(0,1,1)
  AUT_thresh = matrix(0,1,1)
  SPRsmooth_max = matrix(0,1,1)
  SPRsmooth_min = matrix(0,1,1)
  AUTsmooth_max = matrix(0,1,1)
  AUTsmooth_min = matrix(0,1,1)
  
  #Process phenology dates for each pixel in EVI matrix
  for (pix in 1:nrow(evi_mat)){
    #print(pix)
    
    EVI <- evi_mat[pix,]
    nobs[pix] <- length(which(is.na(EVI)==0)) #count number of available observations
    
    if (nobs[pix] > 100){
      pos <- which(doy>60 & doy<340 & is.na(EVI)==0 & EVI>0 & EVI<1)
      EVI <- EVI[pos]
      DOY <- doy[pos]
      
      #Sort DOY/EVI by DOY and compute winter background EVI for improved smoothing spline fit
      x <- cbind(DOY,EVI) #Spring background
      x <- x[order(x[,1]),]
      EVI <- c(EVI,matrix(mean(x[1:10,2]),11,1))
      DOY <- c(DOY,seq(1,51,5))
      
      x <- cbind(DOY,EVI) #Autumn background
      x <- x[order(-x[,1]),]
      EVI <- c(EVI,matrix(mean(x[1:10,2]),6,1))
      DOY <- c(DOY,seq(340,365,5))
      
      #Fit smoothing spline through EVI and DOY data
      fit <- smooth.spline(DOY,EVI,spar=0.65)
      EVIsmooth <- data.frame(predict(fit,x=1:365))
      rsmooth[pix] <- cor(EVIsmooth[DOY,2],EVI)
      
      if (rsmooth[pix]>0.75){
        #Separate spline into spring and autumn segments using annual maximum      
        pkval <- which.max(EVIsmooth[,2])
        SPRsmooth <- EVIsmooth[1:pkval,2]
        AUTsmooth <- EVIsmooth[(pkval+1):365,2];
    
        #Compute half-maximum of spring logistic for "ruling in" image dates (points) for
        #anamoly calculation
        SPR_thresh <- which.min(abs((SPRsmooth-min(SPRsmooth,na.rm=TRUE))/
                                     (max(SPRsmooth,na.rm=TRUE)-min(SPRsmooth,na.rm=TRUE))-thresh))
        SPR_halfmax <- which.min(abs((SPRsmooth-min(SPRsmooth,na.rm=TRUE))/
                                      (max(SPRsmooth,na.rm=TRUE)-min(SPRsmooth,na.rm=TRUE))-0.5))
        
        #Find anomalies inside of designated box
        SPRsmooth_max <- max(SPRsmooth,na.rm=TRUE)
        SPRsmooth_min <- min(SPRsmooth,na.rm=TRUE)
        box_max <- SPRsmooth_max-0.2*(SPRsmooth_max-SPRsmooth_min)
        box_min <- SPRsmooth_min+0.2*(SPRsmooth_max-SPRsmooth_min)
        
        #Generate a matrix with candidate spring phenology observations
        if (is.na(box_max)==0 && is.na(box_min)==0){
          info <- cbind(yr,doy,evi_mat[pix,],matrix(0,length(doy),1))
          pos <- which(is.na(info[,3])==0 & info[,3]>box_min & info[,3]<box_max &
                        info[,2]<SPR_halfmax+20 & info[,2]>SPR_halfmax-20)
          if (length(pos)>0){
            k <- 1
            for (p in 1:length(pos)){
              smooth_ratio <- abs(SPRsmooth-info[pos[p],3])
              info[pos[p],4] <- which.min(smooth_ratio)
              
              if (k==1)
                info_spr <- info[pos[p],]
              else
                info_spr <- rbind(info_spr,info[pos[p],])
              
              k <- k+1
            }
          }
        }
        
        #Compute half-maximum of spring logistic for "ruling in" image dates (points) for
        #anamoly calculation
        AUT_thresh <- which.min(abs((AUTsmooth-min(AUTsmooth,na.rm=TRUE))/
                                     (max(AUTsmooth,na.rm=TRUE)-min(AUTsmooth,na.rm=TRUE))-thresh))+pkval
        AUT_halfmax <- which.min(abs((AUTsmooth-min(AUTsmooth,na.rm=TRUE))/
                                      (max(AUTsmooth,na.rm=TRUE)-min(AUTsmooth,na.rm=TRUE))-0.5))+pkval
        
        #Find anomalies inside of designated box
        AUTsmooth_max <- max(AUTsmooth,na.rm=TRUE)
        AUTsmooth_min <- min(AUTsmooth,na.rm=TRUE)
        box_max <- AUTsmooth_max-0.4*(AUTsmooth_max-AUTsmooth_min)
        box_min <- AUTsmooth_min+0.2*(AUTsmooth_max-AUTsmooth_min)
        
        #Generate matrix with candidate autumn phenology observations
        if (is.na(box_max)==0 && is.na(box_min)==0){
          info <- cbind(yr,doy,evi_mat[pix,],matrix(0,length(doy),1))
          pos <- which(is.na(info[,3])==0 & info[,3]>box_min & info[,3]<box_max &
                        info[,2]<AUT_halfmax+20 & info[,2]>AUT_halfmax-20)
          if (length(pos)>0){
            k <- 1
            for (p in 1:length(pos)){
              smooth_ratio <- abs(AUTsmooth-info[pos[p],3])
              info[pos[p],4] <- which.min(smooth_ratio)+pkval
              
              if (k==1)
                info_aut <- info[pos[p],]
              else
                info_aut <- rbind(info_aut,info[pos[p],])
              
              k <- k+1
            }
          }
        }
        
        #Calculate interannual phenology dates by taking the distance
        #between each candidate observation and where the same magnitude 
        #of EVI occurs on the spline
        if (exists('info_spr') == 1 && exists('info_aut') == 1){
          if (length(info_spr) > 4 & length(info_aut) > 4) {
            info_spr <- cbind(info_spr,SPR_thresh+(info_spr[,2]-info_spr[,4]))
            info_aut <- cbind(info_aut,AUT_thresh+(info_aut[,2]-info_aut[,4]))
            
            for (y in 1982:2012){
              pos1 <- which(info_spr[,1] == y)
              pos2 <- which(info_aut[,1] == y)
              
              if (length(pos1) > 0 && ncol(info_spr)==5){
                phenoSPR[pix,y-1981] <- ceiling(mean(info_spr[pos1,5]))
              }
              if (length(pos2) > 0 && ncol(info_aut)==5){
                phenoAUT[pix,y-1981] <- ceiling(mean(info_aut[pos2,5]))
              }
            }
          }
          
          ltmSPR[pix] <- SPR_thresh
          ltmAUT[pix] <- AUT_thresh
          bline[pix] <- min(cbind(SPRsmooth_min,AUTsmooth_min))
          hline[pix] <- SPRsmooth_max
          
          remove(info_spr,info_aut) 
        }
      }
    }
  }
  
  pheno_matrix <- cbind(nobs,rsmooth,bline,hline,ltmSPR,ltmAUT,phenoSPR,phenoAUT)
  
  return(pheno_matrix)
}

#Use foreach to apply function(s) to r, each node will process a block until there are no more
result <- foreach(i = 1:length(bs$row), .combine = c) %dopar% {
  print(i)

  v1 <- getValues(b1, bs$row[i], bs$nrows[i])
  v3 <- getValues(b3, bs$row[i], bs$nrows[i])
  v4 <- getValues(b4, bs$row[i], bs$nrows[i])
  v8 <- getValues(b8, bs$row[i], bs$nrows[i])
  
  #Compute EVI time series and filter clouds/snow/ice
  evi_mat = 2.5*(v4/10000-v3/10000)/(v4/10000+6.*v3/10000-7.5*v1/10000+1)
  pos = which(v8 != 0)
  evi_mat[pos] = NA
  
  #setwd(paste("/projectnb/modislc/users/emelaas/scratch15/pheno_calc_smooth_50/",stack_loc,"/",sep=""))
  #save(evi_mat,doy,yr,file = paste("evi_",bs$row[i],"_",bs$nrow[i],sep = ""))
    
  pheno_matrix <- try(Landsat_Phenology(evi_mat,doy,yr))
  if(!inherits(pheno_matrix,"try-error")){ 
    print(paste("Row",i,"was compiled successfully."))
  } else{
    print(paste("Row",i,"had an error."))
    pheno_matrix <- matrix(NA,nrow(evi_mat),68)
  }
  
  print(paste("Saving pheno_matrix for row",i))
  setwd(paste("/projectnb/modislc/users/emelaas/scratch15/pheno_calc_smooth_50/",stack_loc,"/",sep=""))
  save(pheno_matrix,file = paste("pheno_",bs$row[i],"_",bs$nrow[i],sep = ""))
}

