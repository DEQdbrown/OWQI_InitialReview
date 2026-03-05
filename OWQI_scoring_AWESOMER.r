# Created by Shannon Hubler and Robin Leferink with contributions by Peter Bryant/ last modified by PTB 01/24/2014
        # modified by SH 1/15/15: changed order of loading lubridate and wq packages

#   Function: owqi.calc
#      - calculates OWQI, subindices and conditions;
#      - calculates 10 year trends for OWQI, subindices and raw data
#      - calculates 10yr seasonal averages;
#      - calculates 30yr annual averages by water year
#   Function: owqi.plot
#      - generates 30yr graphs for OWQI/subindices

#   REQUIREMENTS:
#     Set working directory. If you have this file open in R-studio follow Session-->Set Working Directory-->To Source File Location
#     
#     Source files "FUNCTION_owqi.calc_AWESOMER.r" & "FUNCTION_owqi.plot.r" must be saved in working directory
#     
#     Must have installed packages 'wq', 'Hmisc', 'xlsx', 'plyr','ggplot2', 'reshape2' and 'lubridate'  
#     To do this in R-Studio follow Tools-->Install Packages. Type the package name and click Install for each package.
#     
#     Input file must use the following field names: "Station", "combo_name", "OWQI_basin", "Date", "temp",  
#     "d_o", "do_sat", "bod", "ph", "ts", "nh3", "no2", "p", "ecoli", "fecal" (date format "mm/dd/yyyy")


# USER INPUT REQUIRED
# set ending water year (endWY) for data set
# set min/max dates (start.date, end.date) and water year range (WY,range)
# set filename for the data source with all raw data for all sites for all years. MUST BE a .csv file.
endWY      <- 2025                                   #the ending water year
start.date <- "2016/10/01"                           #min date for 10yr calcs
end.date   <- "2025/09/30"                           #max date for 10yr calcs
WY.range   <- "2016-2025"                            #set 'Water Year' based on min/max dates
filename   <- "//deqlab1/WQM/OWQI/R/Final/InitialDataProcessing/DATA_WY1980_WY2025_ALLDATA.csv"       #filename of the raw data for all sites for all years. MUST BE .csv

#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!   NO EDITS  TO  THE REST OF THE CODE **Except Lines 98, 100, and 102-106 then...JUST RUN IT ALL   !!!!!!        #
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!


#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!   NO EDITS  TO  THE REST OF THE CODE **Except Lines 98, 100, and 102-106 then...JUST RUN IT ALL   !!!!!!        #
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!


#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!   NO EDITS  TO  THE REST OF THE CODE **Except Lines 98, 100, and 102-106 then...JUST RUN IT ALL   !!!!!!        #
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!
#   !!!!!         !!!!!!!!          !!!!!!        !!!!!!!!!!





################################################################################
################################################################################

# load packages and functions

require(lubridate);  require(wql)  # lubridate has 'year' and wq has 'years'.  
# The order you bring them in affects line 38 'years' call, in owqi.calc.  Bringing in lubridate 1st resolves this.

require(Hmisc)
#require(xlsx)
require(plyr)
require(reshape2)
require(ggplot2)

## set working 
setwd("//deqlab1/WQM/OWQI/R/Final")

source("//deqlab1/WQM/OWQI/R/Final/FUNCTION_owqi.calc_AWESOMER.r")
source("//deqlab1/WQM/OWQI/R/Final/FUNCTION_owqi.plot_Lesley_is_needy.r")    # For Lesley's 
source("//deqlab1/WQM/OWQI/R/Final/FUNCTION_owqi.plot.AWESOMER.r")

options(scipen = 100)

################################################################################
#############    calculate OWQI, subindices and conditions   ###################
#############        calculate 10yr seasonal averages        ###################
#############  calculate 30yr annual averages by water year  ###################
################################################################################

# import data
# if there are NAs in any of the columns of this import file, then you will get this message "Error in data.frame: arguments imply differing number of rows"
chem<-read.csv("//deqlab1/WQM/OWQI/R/Final/InitialDataProcessing/DATA_WY1980_WY2025_ALLDATA.csv", fileEncoding="UTF-8-BOM")

endwy<-2025  # LAM 2/11/2020 - don't use quotes and create a folder for the water year in the 30yr graphs folder

owqi.calc(chem, 
          min.d<-"2016/10/01",                 
          max.d<-"2025/09/30",                 
          WY="2016-2025",                         
          End.YR="2025"                          
          )

#   This generates the message "There were 50 or more warnings (use warnings() to see the first 50)
#      In.mean.default... argument is not numeric or logical: returning NA"
#      This results from applying 'mean' function to data set which includes site names; DISREGARD WARNING.


#   Next steps: import csv files into database
#      "OWQI_subindices_cond_thru_WY..." goes into: "RESULTS: indices + condition_thru: WY2009"
#      "OWQI_10yr_seasonal..." goes into:           "RESULTS: 10yr Aves + Conditions"

#   To generate csv file with 30yr annual OWQI/subindex averages by water year, uncomment next line
#   write.csv("//deqlab1/WQM/OWQI/R/Final/wy.avgs,OWQI_subindices_avgs_by_WY_30yr.csv")

#   To genenerate csv file with 10 year Seasonal Kendall trend analysis run the following line. This will generate
#   a csv in the Water_Year_20XX folder based on the endWY defined above


################################################################################
#############                                                ###################
#############    generate 30yr graphs for OWQI/subindices    ###################
#############                                                ###################
################################################################################

#sites<-wy.avgs$Station

#for(i in sites) {
# subset(wy.avgs, wy.avgs$Station == i)->x1
#  owqi.plot(x1, x1$water_yr, x1$owqi, endWY=endWY)
# rm(x1)
# }
# 
# #For Lesley's OWQI only .png plots
# sites<-wy.avgs$Station

# for(i in sites) {
#    subset(wy.avgs, Station==i)->x1
#     owqi.plot.needy(x1, x1$water_yr, x1$owqi, endWY=endWY)
#     rm(x1)
#    }

# One pdf for each site should appear in working directory
