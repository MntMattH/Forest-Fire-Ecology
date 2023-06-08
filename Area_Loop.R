#Script to generate DOB Area metric for all fires. 
#Matt Harris 5/24/2023 (Updated 6/7/2023)

#Design a loop that can iterate through all fires calculating area per DOB.
#This will then be used to calculate radial growth in the same fashion. 
library(terra)
library(sf)
library(dplyr)
library(landscapemetrics)
library(sjmisc)
library(raster)
library(RColorBrewer)

#DOB Directory: C:/Users/matth/Desktop/DOB
#Stage 1: Create an interative loop

# Outermost loop iterates using a variable called "year" across the range 2002-2021
# to debug without running this loop you'll need to manually define year, e.g
DOB_lst<-list()
Area_lst<-list()

for (year in 2002:2020) {
  
  ## Use the DOB interpolation output directory to get a list of all successfully interpolated fires for current year.
  ## Note year is added to the path via paste0(); this function concatenates text strings separated by commas.
  fire.list <- list.dirs(path = paste0('C:/Users/matth/Desktop/DOB/', year, '/'),
                         full.names = FALSE, recursive = TRUE)
  
  # drop empty leading value. I dunno why it exists? 
  fire.list <- fire.list[-1]  # This syntax specifically means "drop list element with index = 1". 
  
  ## Use our fire list to grab data for individual fires. This inner "fire loop" iterates using a variable called 
  ## "fire" across all elements of fire.list
  for (fire in fire.list) {
    
    # Enter a fire's folder. 
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
    
    #Convert doblob.rast to polygon
    dob.poly <- st_as_sf(as.polygons(dob.rast))
    
    #Convert to multipolygon in order to calc area metric per DOB: 
    mp_dob<-st_cast(dob.poly,"MULTIPOLYGON")
    
    #Use st_area to calc DOB area in m^2
    mp_dob$AREA<-NA
    mp_dob$AREA<-st_area(mp_dob)
    
    #Plot to see result. 
    plot(mp_dob[,3], main= "Area (m^2)")
    
######################Start Radial growth loop prep: For now I'm not going to develop the radial growth metric since it doesn't seem to present much value for comparison. 
    
    #rg_dob<-mp_dob
    #rg_dob$RadialGrowth<-NA
    #rg_dob$RadialGrowth[1]<-sqrt(rg_dob$AREA[1]/pi)
    
#####I'm unsure how to add + rg_dob$AREA[x] for every successive day of the fire? Maybe make this formula an object that is written based on the number of days per fire? I then also need each rg_dob$RadialGrowth[[x]] to correspond with the same object based on the day. Finally I need the ending that subtracts each prior days radius to correlate with each day, but not counting the day being evaluated, eg. -(r1+r2) for day 3. I'm excited to see what your recommendations are here Jared as this will have to be a rather dynamic formula! 
    
    #rg_dob$RadialGrowth[2]<-sqrt((rg_dob$AREA[1]+rg_dob$AREA[2])/pi)-r1

    
####Save each fire to maintain AREA. 
    DOB_lst[fire]<-list(mp_dob$DOB)
    Area_lst[fire]<-list(mp_dob$AREA)
    
    
    
  }
}

#Melt lists to dataframes
DOB_M<-melt(DOB_lst)
Area_M<-melt(Area_lst)
#Create area df
Area_df<-data.frame(DOB_M, Area_M)

Area_df$Area<-(Area_df$value.1) 
#Make AreaID metric by combining fire ID and DOB, in order to allow area to be matched to points from perimeter analysis. 

Area_df$AreaID<-paste(Area_df$L1, Area_df$value, sep = "_")

#Set WD: 
setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Cleaned_Points")
#60
#Load data: 
C60M<-read.csv("Clean_MP_60m.csv")
#Create AreaID
C60M$AreaID<-paste(C60M$Event_ID,C60M$DOB, sep = "_")
#Match AreaID
C60M$DOB_Area<- Area_df$Area[match(C60M$AreaID, Area_df$AreaID)]

#120
#Load data: 
C120M<-read.csv("Clean_MP_120m.csv")
#Create AreaID
C120M$AreaID<-paste(C120M$Event_ID,C120M$DOB, sep = "_")
#Match AreaID
C120M$DOB_Area<- Area_df$Area[match(C120M$AreaID, Area_df$AreaID)]

#240
#Load data: 
C240M<-read.csv("Clean_MP_240m.csv")
#Create AreaID
C240M$AreaID<-paste(C240M$Event_ID,C240M$DOB, sep = "_")
#Match AreaID
C240M$DOB_Area<- Area_df$Area[match(C240M$AreaID, Area_df$AreaID)]


#Save area df:
library(data.table)

fwrite(Area_df, "C:\\Users\\matth\\Desktop\\Thesis Dev\\Area_Metric\\Area_df.csv", append= FALSE)

#Save MP data sets:
#Export C3 files with Dob_Area 
#60
fwrite(C60M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Cleaned_Points\\Clean_MP_60m.csv", append= FALSE)
#120
fwrite(C120M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Cleaned_Points\\Clean_MP_120m.csv", append= FALSE)
#240
fwrite(C240M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Cleaned_Points\\Clean_MP_240m.csv", append= FALSE)




