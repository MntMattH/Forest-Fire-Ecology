library(dplyr)

#Set WD: 
setwd("C:/Users/matth/Desktop/Method_1_Analysis/Perimeter_Analysis_V4/Cleaned_Points")

#Load data: 
C60M<-read.csv("Clean_MP_60m.csv")
C120M<-read.csv("Clean_MP_120m.csv")
C240M<-read.csv("Clean_MP_240m.csv")
# Peek at column names
head(C60M) # ok, want to compare FireYear to All_Burns.

# Create a custom function to find the most recent year (maximum) among All_Burn years prior to the FireYear
FindMostRecentBurnYear <- function(All_Burns, FireYear) {
  
  # First use the strsplit function to separate All_Burns strings into individual elements and coerce to numeric
  years <- as.numeric(strsplit(All_Burns, ",")[[1]])
  
  # filter out years greater than the FireYear
  prior_years <- years[years < FireYear]
  
  # if there are any prior years, we want to select the maximum among them
  if (length(prior_years) > 0) {
    return(max(prior_years))
  # alternatively if there aren't any prior burn years, we're done here!
  } else { 
    # more specifically the most recent burn year should be NA
    return(NA) 
  }
}

# Define a new column in your dataframe by using mapply to apply our function to each row.
C60M$MostRecentPriorBurn <- mapply(FindMostRecentBurnYear, # apply our custom function
                                   C60M$All_Burns, # first argument to our custom function
                                   C60M$FireYear) # second argument to our custom function
# holy shit I think it works?

C60M$FireYear1<-C60M$FireYear

#Matt tinkering to fix the selection issue: 

years <- as.numeric(strsplit(C60M$All_Burns, " ")[[29826]])

?strsplit

years <- as.numeric(strsplit(C60M$All_Burns, ",")[[29826]])
########Using a comma in the split function was able to separate the years correctly. 

#Load All_Burns category from the organization script:
C120M$All_Burns<- D_points_120$All_Burns

C240M$All_Burns<- D_points_240$All_Burns

#Now I can make my years since fire metric: 
C60M$MostRecentPriorBurn <- mapply(FindMostRecentBurnYear, # apply our custom function
                                   C60M$All_Burns, # first argument to our custom function
                                   C60M$FireYear)


C120M$MostRecentPriorBurn <- mapply(FindMostRecentBurnYear, # apply our custom function
                                    C120M$All_Burns, # first argument to our custom function
                                    C120M$FireYear)


C240M$MostRecentPriorBurn <- mapply(FindMostRecentBurnYear, # apply our custom function
                                    C240M$All_Burns, # first argument to our custom function
                                    C240M$FireYear)

C60M$Yrs_Since_Fire<- C60M$FireYear - C60M$MostRecentPriorBurn
C120M$Yrs_Since_Fire<- C120M$FireYear - C120M$MostRecentPriorBurn
C240M$Yrs_Since_Fire<- C240M$FireYear - C240M$MostRecentPriorBurn

#Years since fire is a success! 
#Extract to cleaned data sets:

library(data.table)
#Export C3 files with LC descriptions, fixed prior burn, and aridity metric: 
#60
fwrite(C60M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Points_C3\\C3_Points_60m.csv", append= FALSE)
#120
fwrite(C120M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Points_C3\\C3_Points_120m.csv", append= FALSE)
#240
fwrite(C240M, "C:\\Users\\matth\\Desktop\\Method_1_Analysis\\Perimeter_Analysis_V4\\Points_C3\\C3_Points_240m.csv", append= FALSE)





