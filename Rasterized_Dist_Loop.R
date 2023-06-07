#Rasterized growth loop:
#Matt Harris 
#5/30/2023

#Note for Jared: In this script I'm trying to use the series of loops that you constructed previously for the transects to produce rasterized distance for each DOB/DOBLOB. I have kept each of the loops that try to address the variety of DOBs we see such as multiple flaming fronts, spot fires, etc. I'm not great with loops and thus haven't been able to integrate the rasterized distance into many of these cases, so far the first day is working from the centroid, but I haven't had as much luck with the following days. A brief note of your recommended changes would be greatly appreciated for my learning! Thanks, Matt. 

#Load Packages: 
library(terra)
library(sf)
library(dssd)
library(raster)
library(dplyr)
library(spdep)
library(spData)
library(landscapemetrics)
library(sjmisc)

#Begin by reworking transect loop to incorporate rasterized distance: If you start on 2005 it will depict what it is able to process well (first day and unburned islands, but not sequential days). 
for (year in 2005:2020) {
  
  ## Use the DOB interpolation output directory to get a list of all successfully interpolated fires for current year.
  ## You'll have to modify the path within the call to paste0() to point this to where you've stored the data.
  ## Note year is added to the path via paste0(); this function concatenates text strings separated by commas.
  fire.list <- list.dirs(path = paste0('C:/Users/matth/Desktop/DOB/', year, '/'),
                         full.names = FALSE, recursive = TRUE)
  
  # drop empty leading value. I dunno why it exists? 
  fire.list <- fire.list[-1]  # This syntax specifically means "drop list element with index = 1". 
  
  ## Use our fire list to grab data for individual fires. This inner "fire loop" iterates using a variable called 
  ## "fire" across all elements of fire.list
  for (fire in fire.list) {
    
    # Enter a fire's folder. Again you'll need to modify the file pathway here to where you've stored the data.
    # Now in addition to year, paste0() is adding both year and a fire's MTBS ID to the path. 
    # to debug without running this loop, you'll need to manually define a fire, e.g.
    # fire <- fire.list[1] # defines fire using the first index of fire.list (numerical position in the list)
    setwd(paste0('C:/Users/matth/Desktop/DOB/', year, '/', fire))
    
    # Load DOB interpolation 
    if (file.exists(paste0(fire,"_dob.QC.tif"))) {
      dob.rast <- rast(paste0(fire,"_dob.QC.tif")) # if the fire was reinterpolated after QCAC, use that interpolation.
    } else {
      dob.rast <- rast(paste0(fire,"_dob.tif")) # Otherwise grab default interpolation if the fire was not pinged by QCAC.
    }
    
    ## Create DOBLOB raster of spatially discretized Day of Burn patches, or contiguous pixels burning on the same day.
    # To do this we'll use the get_patches() function from the landscapemetrics package.
    doblob.rast <- get_patches(dob.rast, class = "all", directions = 8, return_raster = TRUE) 
    # get_patches() actually returns a list of raster layers (1 layer for each DOB containing its DOBLOB patches). 
    # Let's rename these layers for what they are; DOBs. (Default is something nondescript like "layer_1")
    names(doblob.rast) <- "DOB"
    # Next let's squish those DOB layers containing their DOBLOB patches into a single layer with merge(). We'll also
    # use Reduce() to hit all layers at once. Finally, use rast() to make this object a spatraster again.
    doblob.rast <- rast(Reduce(merge, doblob.rast$DOB)) 
    # Let's rename this newly merged single layer for what it contains; DOBLOBS
    names(doblob.rast) <- "DOBLOB"
    
    ## Great, now we need to convert our rasters to polygons for spatial analyses
    # Use as.polygons() to convert DOBLOB and DOB spatrasters to spatvector polygons
    doblob.poly <- as.polygons(doblob.rast)
    dob.poly <- as.polygons(dob.rast)
    # Convert spatvectors to sf objects. 
    doblob.poly <- st_as_sf(doblob.poly)
    dob.poly <- st_as_sf(dob.poly)
    # Reproject doblobs to desired coordinate system, if needed
    doblob.poly <- st_transform(doblob.poly, crs(dob.poly))
    
    #### Loop through each DOB within current fire
    for (i in 1:dim(dob.poly)[1]) { 
      
      #subset DOBLOBs for current DOB
      doblobs <- doblob.poly[dob.poly[i,], , op=st_within]
      # plot(st_geometry(doblobs))
      #plot(st_geometry(doblobs[1,])) #if there are multiple DOBLOBS, plot separately
      # plot(st_geometry(dob.sf[i-1,]), add=T, col='black', lwd=2)
      
      ### for i == 1 (the first DOB), which has an unknown starting point, use circular growth.
      if (i == 1) {
        
        # Prep an object to store valid rasterized distances 
        output.rast <- doblob.rast
        values(output.rast) <- NA
        
        # iterate over DOBLOB belonging to DOB==1 using a variable called z 
        for (z in 1:dim(doblobs)[1]) {
          
          # Because there is no prior DOB for the first DOB and the starting point of the fire is unknown,
          # we use the assumption of distance from the centroid to the perimeter as a standardized estimate 
          doblob.cent <- st_centroid(doblobs[z,])
          # plot(st_geometry(doblobs[z,]))
          plot(dob.rast)
          plot(st_geometry(doblob.cent), pch=21, bg='grey', add=T)
          
          # Define shared border for DOB == 1 as the DOBLOB perimeter
          shared.border <- st_cast(doblobs[(z),],"MULTILINESTRING")
          plot(st_geometry(shared.border), add=T, col='red', lwd=3)
          
##########Add in Rasterized growth from centroid:          
          FirstDOB.rast <- mask(dob.rast, dob.poly[(1),], touches = FALSE)
          cent_dist_rast <- distance(FirstDOB.rast, doblob.cent)
          cent_dist_rast <- mask(cent_dist_rast, FirstDOB.rast)
          plot(cent_dist_rast)
          
          # add this to the output raster
          output.rast <- cover(output.rast, cent_dist_rast)
          
        }
        
        
      } else { 
        
        ### Else, for all i != 1 (i.e. all but first DOB), we need to know whether DOBLOBs are contiguous or disjoint
        
        # Subset DOBLOBs that grew from an adjacent DOBLOB and those that didn't (i.e. are disjoint; spotfires et al.)
        # fire growth in contiguous polygons can be modeled from the shared perimeter.
        # Whereas, fire growth from disjoint polygons must be modeled using the assumption of circular growth.
        poly.cont <- doblobs[dob.poly[i-1,], op=st_touches]
        poly.dis <- doblobs[!doblobs[[1]] %in% poly.cont[[1]],]
        # plot(poly.cont, add=T, col='grey60', lwd=2)
        # plot(poly.dis, add=T, col='white', lwd=2)
        
        ### Disjoint DOBLOB polygons use the circular growth model like the first DOB 
        if (dim(poly.dis)[1] > 0) {
          
          for (z in 1:dim(poly.dis)[1]) { # again use z to iterate over these disjoint DOBLOBs 
            
            # Because the starting point of the fire is unknown, we use the assumption of distance from the centroid 
            # to the perimeter as a standardized estimate 
            doblob.cent <- st_centroid(poly.dis[z,])
            # plot(st_geometry(doblobs[z,]))
            plot(dob.rast)
            plot(st_geometry(doblob.cent), pch=21, bg='grey', add=T)
            
            # Define shared border for disjoint DOBLOB as the DOBLOB perimeter
            shared.border <- st_cast(poly.dis[(z),],"MULTILINESTRING")
            plot(st_geometry(shared.border), add=T, col='red', lwd=3)
            
          
 ###########Rasterize from centroid:
            FirstDOB.rast <- mask(dob.rast, poly.dis[(1),], touches = FALSE)
            cent_dist_rast <- distance(FirstDOB.rast, doblob.cent)
            cent_dist_rast <- mask(cent_dist_rast, FirstDOB.rast)
            plot(cent_dist_rast)
            
            # add this to the output raster
            output.rast <- cover(output.rast, cent_dist_rast)
            
            
          }
        }
        
        ### Contiguous DOBLOB polygons use the code we wrote a few weeks ago, with some special cases
        if (dim(poly.cont)[1] > 0) {
          for (z in 1:dim(poly.cont)[1]) { # again use z to iterate over these contiguous DOBLOBs 
            
            ## Find the shared border between each DOBLOB and the prior DOBLOB. 
            # This shared.border represents the flaming front along which fire spread occurred
            shared.border <- st_intersection(poly.cont[z,], dob.poly[(i-1),]) 
            Current.DOBLOB <- st_cast(poly.cont[(z),],"MULTILINESTRING")
            nonshared.border <- st_difference(Current.DOBLOB, shared.border)
            
            plot(dob.rast)
            plot(st_geometry(shared.border), add=T, col='red', lwd=3)
            plot(st_geometry(nonshared.border), add=T, col='blue', lwd=3)
    
############Shared and nonshared borders are being produced correctly.             
          
            ### Several "weird" scenarios to control for here.
            # 1) was regarding points for transects, so it was disregard as I believe that it won't influence raster pixels. 
            
            ## 2) Multiple flaming fronts "grow" into the current DOBLOB (ie, current DOBLOB borders 2+ DOBLOBs of the previous DOB) 
            # IMO these should have the opportunity to meet somewhere in the middle, right? Rasterized gradient will address this well. 
            # IDK the best way to identify these, so here's a kludgy attempt that works by checking how many of the previous
            # DOB's DOBLOBs touch the current DOBLOB. If there's more than 1, then we should have multiple flaming fronts moving 
            # into the current DOBLOB.
            if (length(st_touches(shared.border, doblob.poly[dob.poly[i-1,], , op=st_within])[[1]]) > 1)  {
############ERROR: This is where I start to run into a slew of errors from [as,sf] coercion failed to Error in x[[1]] : subscript out of bounds. I have tinkered with the SequentialDOB.rast and have been able to avoid these errors, but I haven't got it to produce a reliable result. 
            
              
              
##############dob.rast is a SpatRaster and dob.poly is a sf data.frame, this matches the script that was working before to produce rasterized distance based on the SequentialDOB.rast on the individual fire level. It is now producing Error: [as,sf] coercion failed. I'm unsure why this is the case as transforming to a sp dataframe doesn't seem to resolve the issue. I think the issue may be due to the dob.rast having a being incorectly filled, but I'm unsure. 
              
              
              
              # Mask distance raster to the sequential DOB and crop to its extent to speed up processing. 
              SequentialDOB.rast <- mask(dob.rast, dob.poly[(i+1),], touches = FALSE)
              # Calculate distance raster and mask to sequential DOB
              dist_rast <- distance(SequentialDOB.rast, shared.border)
              dist_rast <- mask(dist_rast, SequentialDOB.rast)
              
              # add this to the output raster
              output.rast <- cover(output.rast, dist_rast)
              
            } else if (nrow(nonshared.border) < 1) { 
              ## 3) We didn't find a nonshared.border, indicating this DOBLOB was an "unburned island" that is now burning several days later.
              # I think all we can do is assume circular growth? Agree 
              
##############Rasterize from centroid: I'm unsure how to designate the DOBLOB that needs to be addressed as the unburend island, should the Current.DOBLOB be used or should a new variable such as UI_DOB.rast? 
              
              UI_DOB.rast <- mask(dob.rast, dob.poly[(i+1),], touches = FALSE)
              cent_dist_rast <- distance(UI_DOB.rast, doblob.cent)
              cent_dist_rast <- mask(cent_dist_rast, UI_DOB.rast)
              plot(cent_dist_rast)
              
              # add this to the output raster
              output.rast <- cover(output.rast, cent_dist_rast)
            
              
              
            } else {
###############I believe that this final else should address anything not caught in the above sinerious. For this I'm sticking to the traditional way of rasterising distance to the shared.border. 
              
              # Mask distance raster to the sequential DOB and crop to its extent to speed up processing.
              SequentialDOB.rast <- mask(dob.rast, dob.poly[(i+1),], touches = FALSE)
              
              # Calculate distance raster and mask to sequential DOB
              dist_rast <- distance(SequentialDOB.rast, shared.border)
              dist_rast <- mask(dist_rast, SequentialDOB.rast)
              
              # add this to the output raster
              output.rast <- cover(output.rast, dist_rast)
              
            }
            
            
          }
        }
        
      }
    }
    
    #### Plot all valid transects for current fire
    plot(dob.rast)
    plot(output.rast)
    
    
  }
}
