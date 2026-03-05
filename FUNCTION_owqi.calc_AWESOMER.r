assign.condition<-function (x){
            ifelse(x >= 89.5, "Excellent",
              ifelse(x >84.5 &  x <89.5, "Good",
                ifelse(x >=79.5 & x <=84.5, "Fair",
                  ifelse(x >=59.5 & x <79.5, "Poor",
                    ifelse(x < 59.5, "Very Poor", -999)))))
 }

midpoint <- function(x){
  is.even <- (month(x) %% 2) == 0
  mid <- ifelse(is.even, 
                ISOdate(year(x), month(x), 1, 0, 0, 0, tz = tz(x)),
                ISOdatetime(year(x), month(x) + 1, 1, 0, 0, 0, tz = tz(x)))
  class(mid) <- class(x)
  return(mid)
}
midpoint(ymd('2001-01-01'))


selectClosestToMidpoint <- function(x){
  xd <- data.frame(date     = x, 
                   midpoint = midpoint(x))
  f <- function(x){
    x[which.min(abs(x$date - unique(x$midpoint))), ]
  }
  ddply(xd, .(midpoint), f)
}

source('FUNCTION_seaKen.wZ.R')

owqi.calc<-function(data, min.d, max.d, WY, End.YR){ 
        
        chem$Date<-mdy(chem$Date)  ### LAM updated 1/14/2020 - other method is removing leap year samples 
        #chem$Date<-as.Date(chem$Date, "%m/%d/%y")
        Date1<-as.Date(chem$Date, format='%m/%d/%y') ##DTB 1/15/2020 uncommented line after receiving error Date1 not found
        print('Chem Summary - Do dates look correct?')
        print(summary(chem))
        
        year<-wql::years(Date1)    # Lubridate = 'year', wql = 'years'  -- call out that using wql package for years.  ## Changed by SH on 1/15/14 ## Changed to wql by DTB on 1/9/18
        month<-monthNum(Date1)
        
        water_yr<-ifelse(month<10,year,year+1)
        
        
        temp_si<-ifelse(chem$temp<=11,100,        #if temp <= 11, score it 100
          ifelse(chem$temp>29, 10,       #if temp > 29, score it 10
          76.54 + 4.172*chem$temp - 0.1623*chem$temp^2 - 0.0020557*chem$temp^3)  #if  11 < temp <= 29, use scoring formula
          )


##############################  BOD-5 -- all sites   ###############################################################


        bod_si<-ifelse(chem$bod>8, 10, 100*exp(-0.1993*chem$bod))          #if  bod>8, score as '10', otherwise use equation


##############################  pH  -- all sites   #################################################################

        ph_si<-ifelse(chem$ph<4 | chem$ph>11, 10,                                        #if  pH<4 or pH>11, score as '10'
                    ifelse(chem$ph>=7 & chem$ph<=8, 100,                                 # pH between 7 and 8, score as 100
                    ifelse(chem$ph>=4 & chem$ph<7 , 2.628*exp(chem$ph*0.52),             #if pH between 4 and 7, use equation
                    ifelse(chem$ph>8 & chem$ph<=11, 100*exp(-0.5188*(chem$ph-8)), 0)     #if pH between 8 and 11, use equation
            )))



##############################  Ammonia + Nitrate  -- all sites   ##################################################

        nh3.no2<-chem$nh3 + chem$no2

        n_si<-ifelse(nh3.no2>3, 10, 100*exp(nh3.no2*-0.4605))



##############################  total Phophorus  --  all sites    ##################################################

        p_si<-ifelse(chem$p>0.25, 10, 100 - 299.5*chem$p - 0.1384*chem$p^2)




##############################  BACTERIA = two different sub-indices   #############################################

        ##############    Fecal coliform  --  all sites    #####################

        fec_si<-ifelse(chem$fecal <=50, 98,
            ifelse(chem$fecal>1600, 10, 98*exp((chem$fecal - 50) * -9.9178e-4)))



        ##############    e. coli index   ######################################

        ecoli_si<-ifelse(chem$ecoli <=33, 98,
            ifelse(chem$ecoli>1300, 10, 98*exp(0.0496-(0.00181*(chem$ecoli^0.946)))
            ))


        ######### calculate "bacteria" subindex, from fecal and ecoli indices    ###################################
        #if ecoli.si <> null, use ecoli_si, if not then use fecal_si
        bact_si<-ifelse(is.na(ecoli_si)=='FALSE', ecoli_si, fec_si)



#####################      Dissolved Oxygen  -- All sites          ##############################################
### complicated: requires use of conditional statements on two different parameters: DO concentration + DO saturation

        # if DOsat is greater than 275%, score as '10'
        do_si<-ifelse(chem$do_sat > 275, 10,
        # if DOsat is between 100 and  <= 275%, use equation
          ifelse(chem$do_sat>100 & chem$do_sat<=275,
          100*exp((chem$do_sat-100)*-1.197e-2),
        # if DOsat <= 100%
          ifelse(chem$do_sat<=100 & chem$d_o<=3.3, 10, # if DO <= 3.3, then score as '10'
          ifelse(chem$do_sat<=100 & chem$d_o>3.3 & chem$d_o<10.5,
          (-80.29 + 31.88*chem$d_o - 1.401*chem$d_o^2), #if DO is between 3.3 and 10.5, use formula
          ifelse(chem$do_sat<=100 & chem$d_o>=10.5, 100, -999)))  # ifelse DO >= 10.5, score as '100'
          ))



######################  Total solids -- by basins  ###############################################

#### complicated: combine different criteria for 7 different basins

        ts_si<-ifelse(chem$OWQI_basin=='Coast Range',
                  ifelse(chem$OWQI_basin=='Coast Range' &  chem$ts<=40, 100,
                    ifelse(chem$OWQI_basin=='Coast Range' & chem$ts>220, 10,
                      ifelse(chem$OWQI_basin=='Coast Range' & chem$ts>40 & chem$ts<=220,
                      142.6*exp(chem$ts*-8.862e-3), -999)
                      )),
        
            ifelse(chem$OWQI_basin=='Will_Sand_Hood',
                  ifelse(chem$OWQI_basin=='Will_Sand_Hood' &  chem$ts<=40, 100,
                    ifelse(chem$OWQI_basin=='Will_Sand_Hood' & chem$ts>280, 10,
                      ifelse(chem$OWQI_basin=='Will_Sand_Hood' & chem$ts>40 & chem$ts<=280,
                      123.4*exp(chem$ts*-5.296e-3), -999)
                      )),
        
              ifelse(chem$OWQI_basin=='Umpqua',
                    ifelse(chem$OWQI_basin=='Umpqua' &  chem$ts<=40, 100,
                      ifelse(chem$OWQI_basin=='Umpqua' & chem$ts>300, 10,
                        ifelse(chem$OWQI_basin=='Umpqua' & chem$ts>40 & chem$ts<=300,
                        124.7*exp(chem$ts*-5.552e-3), -999)
                        )),
        
                ifelse(chem$OWQI_basin=='Rogue',
                      ifelse(chem$OWQI_basin=='Rogue' &  chem$ts<=50, 100,
                        ifelse(chem$OWQI_basin=='Rogue' & chem$ts>350, 10,
                          ifelse(chem$OWQI_basin=='Rogue' & chem$ts>50 & chem$ts<=350,
                          127.1*exp(chem$ts*-4.818e-3), -999)
                          )),
        
                  ifelse(chem$OWQI_basin=='Deschutes',
                        ifelse(chem$OWQI_basin=='Deschutes' &  chem$ts<=80, 100,
                          ifelse(chem$OWQI_basin=='Deschutes' & chem$ts>300, 10,
                            ifelse(chem$OWQI_basin=='Deschutes' & chem$ts>80 & chem$ts<=300,
                            179.5*exp(chem$ts*-7.326e-3), -999)
                            )),
        
                    ifelse(chem$OWQI_basin=='Klamath',
                          ifelse(chem$OWQI_basin=='Klamath' &  chem$ts<=100, 100,
                            ifelse(chem$OWQI_basin=='Klamath' & chem$ts>450, 10,
                              ifelse(chem$OWQI_basin=='Klamath' & chem$ts>100 & chem$ts<=450,
                              144.9*exp(chem$ts*-3.580e-3), -999)
                              )),
        
                      ifelse(chem$OWQI_basin=='JD_Umat_GR_Crook',
                            ifelse(chem$OWQI_basin=='JD_Umat_GR_Crook' &  chem$ts<=100, 100,
                              ifelse(chem$OWQI_basin=='JD_Umat_GR_Crook' & chem$ts>800, 10,
                                ifelse(chem$OWQI_basin=='JD_Umat_GR_Crook' & chem$ts>100 & chem$ts<=800,
                                116.3*exp(chem$ts*-1.4980e-3), -999)
                                )),
        
                        ifelse(chem$OWQI_basin=='Pow_Bur_Mal_Owy',
                                ifelse(chem$OWQI_basin=='Pow_Bur_Mal_Owy' &  chem$ts<=200, 100,
                                  ifelse(chem$OWQI_basin=='Pow_Bur_Mal_Owy' & chem$ts>1600, 10,
                                    ifelse(chem$OWQI_basin=='Pow_Bur_Mal_Owy' & chem$ts>200 & chem$ts<=1600,
                                    116.3*exp(chem$ts*-7.489e-4), -999)
                                    )), -999
                        )
                      )
                    )
                  )
                )
            )
          )
        )

        summary(ts_si)

        ts_si<-ifelse(ts_si>100, 100, ts_si)   #one value was coming up as 101--convert to 100



###################################################################################################
###########  Calculate OWQI from sub-indices       ##############################################
##################################################################################################

        owqi<-sqrt(8/(
          (1/(temp_si^2)) +
          (1/(bod_si^2)) +
          (1/(ph_si^2)) +
          (1/(n_si^2)) +
          (1/(p_si^2)) +
          (1/(bact_si^2)) +
          (1/(do_si^2)) +
          (1/(ts_si^2))
                         ))
        




######################################################################################
##########   Assign condition classes             ####################################
######################################################################################



        temp_cond<-assign.condition(temp_si)
        bod_cond<-assign.condition(bod_si)
        ph_cond<-assign.condition(ph_si)
        n_cond<-assign.condition(n_si)
        p_cond<-assign.condition(p_si)
        do_cond<-assign.condition(do_si)
        ts_cond<-assign.condition(ts_si)
        bact_cond<-assign.condition(bact_si)
        owqi_cond<-assign.condition(owqi)



#############    Combine chem data + Sub-indices + OWQI      ###################

        si.owqi<-cbind(water_yr, chem, temp_si,bod_si,ph_si,n_si,p_si,do_si,ts_si,ecoli_si, fec_si, bact_si,owqi, temp_cond, bod_cond, ph_cond,
          n_cond, p_cond, do_cond, ts_cond, bact_cond, owqi_cond)
        
        ## LAM - yearly means?    
        calc.avgs<-aggregate(x = si.owqi, 
                by = list(si.owqi$combo_name, si.owqi$water_yr),
                FUN = "mean")
        
        combo_name<-calc.avgs$Group.1
        x.wy.avgs<-calc.avgs[ ,c(3,4, 8:29)]    
        wy.avgs<<-cbind(combo_name,x.wy.avgs)            
        
        write.csv(si.owqi, file=paste("OWQI_subindices_cond_thru_WY", End.YR, 'AWESOMER',".csv", sep=""))

################################################################################
#############                                                ###################
#############       Calculate 10year seasonal trends         ###################
#############                                                ###################
################################################################################

si.owqi<-subset(si.owqi, Date>=as.Date(min.d) & Date<=as.Date(max.d))

#data <- si.owqi
data <- si.owqi[, c(1:14,17:23,26:36)]

data$date <- ymd(as.character(data$Date), tz = 'America/Los_angeles') 
mid <- ddply(unique(data[,c('Station', 'date')]), .(Station), function(x) selectClosestToMidpoint(x$date)) 
data.midpoint <- merge(mid, data, by = c('Station', 'date'), all.x = T)

for (i in 1:length(unique(data.midpoint$Station))) {
  SeaKen.one.station <- data.frame(Station = rep(unique(data.midpoint$Station)[i], 18), #22
                                   Parameter = c('temp','d_o','do_sat','bod','ph','ts','nh3','no2','p','temp_si','bod_si','ph_si','n_si','p_si','do_si',
                                                 'ts_si','bact_si','owqi'),  #'ecoli','fecal',,'ecoli_si','fec_si'
                                   Slope = 'none',
                                   pvalue = 'none',
                                   stringsAsFactors = FALSE)
  ifelse(i == 1, SeaKen <- SeaKen.one.station, SeaKen <- rbind(SeaKen, SeaKen.one.station))
}

## esimate Seasonal Kendall by station
for(ii in 1:nrow(SeaKen)) { 
  ## specify current station and parameter
  tmp.one.station <- SeaKen$Station[ii]
  tmp.one.Parameter <- SeaKen$Parameter[ii]
  tmp.data.raw <- data.midpoint[data.midpoint$Station == tmp.one.station,]
  # reshape and manipulate data to convert to wqData-class
  tmp.data <- data.frame(date=tmp.data.raw$date,
                         time="0000",
                         stn=as.character(tmp.one.station),
                         depth=1,
                         variable=tmp.one.Parameter,
                         value=as.numeric(tmp.data.raw[,tmp.one.Parameter]), 
                         stringsAsFactors=FALSE)
  if (nrow(tmp.data) > 6 & nrow(tmp.data[!is.na(tmp.data$value),]) > 2) {
    # Create wqData object
    tmp.wq <- wqData(tmp.data, c(1,3,4), c(5,6), site.order = TRUE, type = "long",time.format = "%m/%d/%Y")
    tmp.ts <- tsMake(tmp.wq, focus = tmp.one.Parameter, layer = 1) 
    tmp.result <- seaKen.wZ(tmp.ts)
    SeaKen$pvalue[ii] <- tmp.result$p.value
    SeaKen$Slope[ii] <- tmp.result$sen.slope
    SeaKen$N[ii] <- length(tmp.ts[!is.na(tmp.ts)])
    SeaKen$Z[ii] <- tmp.result$Z
    SeaKen$Years[ii] <- paste(substr(min(tmp.data$date),1,4),substr(max(tmp.data$date),3,4),sep = '-')
    rm(list=ls(pattern="tmp.*"))
  }
}

SeaKen$pvalue <- as.numeric(SeaKen$pvalue)
SeaKen$Slope <- as.numeric(SeaKen$Slope)

SeaKen$Signif.80 <- ifelse(SeaKen$pvalue > 0.2,'N',ifelse(SeaKen$Slope < 0,'YES-','YES+'))

SeaKen$End_YR <- endWY

SeaKen$Magnitude <- 10*SeaKen$Slope

SeaKen$Trend <- ifelse(SeaKen$Signif.80 == 'N', 'NT', 
                       ifelse(substr(SeaKen$Signif.80, nchar(SeaKen$Signif.80), nchar(SeaKen$Signif.80)) == '+','INC','DEC'))

SeaKen[SeaKen$N <= 30,'Trend'] <- 'NT'

SeaKen <- SeaKen[,c('Station', 'Parameter', 'Years', 'End_YR', 'N', 'Slope', 'Z','pvalue', 'Signif.80', 'Magnitude', 'Trend')]

#SeaKen <- rename(SeaKen, c('Signif.80' = 'Signif 80%', 'pvalue' = '2*P')) This step needs to be done manually or else an error occurs when running "making big tables.R" script

SeaKen <- SeaKen[!is.na(SeaKen$Slope),]

write.csv(SeaKen, file=paste('//deqlab1/WQM/OWQI/R/Final/Trend/',"Trend_OWQI_Subs", WY, 'AWESOMER',".csv", sep=""))
#DTB - I changed the file path to save the .csv in the correct folder 1/9/18#
#write.csv(SeaKen, file.path = "//deqlab1/WQM/OWQI/R/Final/Trend", file=paste("Trend_OWQI_Subs", WY, 'AWESOMER',".csv", sep=""))

# write.csv(SeaKen, file=paste('//deqlab01/wqm/owqi/Water_Year_',
#                              endWY,
#                              '/',
#                              ifelse(nchar(as.numeric(substr(endWY, 3, 4))-9) == 1, 'WY0', 'WY'), #if the WY - 9 = 1 character, then add 'WY0' to file name
#                              as.character(as.numeric(substr(endWY, 3, 4))-9),                    # add WY - 9 to the file name
#                              substr(endWY, 3, 4),
#                              '_Trend',
#                              '_AWESOMER',
#                              ".csv", sep=""), row.names = FALSE)
############# GPC - I changed the code above from "write.csv(SeaKen, file=paste('//deqlead01/wqm/owqi/Water_Year_'," to "write.csv(SeaKen, file=paste('//deqlab01/wqm/owqi/Water_Year_'," #######
################################################################################
#############                                                ###################
#############       Calculate 10year seasonal averages       ###################
#############                                                ###################
################################################################################


print.noquote("            ")
print.noquote("            ")
print.noquote("            ")
print('10 Year Calcs - Do dates look correct?')
print(summary(si.owqi$Date))


#1)  Classify sample by season
#      a) June, July, August, September: Summer = 'S'
#      b) all other dates: Fall, Winter, Spring = 'FWS'

si.owqi$month<-months(si.owqi$Date)     #get month names, and store back into dataset

si.owqi$season<-ifelse(si.owqi$month %in% c("June","July","August","September"),"S","FWS")#assign season by month; store into dataset

#rename, to keep datasets separate
si.owqi.sea<-si.owqi


#2)  Calculate seasonal 10yr averages for a station

si.agg<-aggregate(si.owqi.sea[,c(17:23, 26:27)],list(si.owqi.sea$Station,si.owqi.sea$season),mean,na.rm=T)
colnames(si.agg)[1:2]<-c("Station","season")
#####
#####  SH 11/29/12: Looked like columns selected above were wrong.  Was 15-21 & 24-25.  


    #pull out seasonal owqi values and store for comparisons
    #source("Q://R Stats/matrify.r")
    #s.fws<-matrify(si.agg, owqi~Station+season)
    #colnames(s.fws)[1:2]<-c("FWS.10yr","S.10yr")


#3)  Record 10yr average for a station as the minimum of either FWS or S 10yr averages - LAM would be nice to retain season
si.agg.min<-aggregate(si.agg[,3:11],list(si.agg$Station),min, na.rm=T)  

si.agg.min$Water_year<-WY   #add in column with 'Water Year'
si.agg.min$End.YR<-End.YR  #add in column with 'End Year' from 10yr window
si.agg.min<-si.agg.min[,c(1,11:12,2:10)]


colnames(si.agg.min)<-c('Station','Water_year','End_YR','temp_10yr_min','bod_10yr_min','ph_10yr_min','n_10yr_min','p_10yr_min','do_10yr_min',
                          'ts_10yr_min','bact_10yr_min','owqi_10yr_min')


#4) assign condition to all indices for 10 year period

      #use assign.condition function -- created above


si.agg.min$temp_10yr_cond<-assign.condition(si.agg.min$temp_10yr_min)
si.agg.min$bod_10yr_cond<-assign.condition(si.agg.min$bod_10yr_min)
si.agg.min$ph_10yr_cond<-assign.condition(si.agg.min$ph_10yr_min)
si.agg.min$n_10yr_cond<-assign.condition(si.agg.min$n_10yr_min)
si.agg.min$p_10yr_cond<-assign.condition(si.agg.min$p_10yr_min)
si.agg.min$do_10yr_cond<-assign.condition(si.agg.min$do_10yr_min)
si.agg.min$ts_10yr_cond<-assign.condition(si.agg.min$ts_10yr_min)
si.agg.min$bact_10yr_cond<-assign.condition(si.agg.min$bact_10yr_min)
si.agg.min$owqi_10yr_cond<-assign.condition(si.agg.min$owqi_10yr_min)


# end of calculations


write.csv(si.agg.min, file=paste('//deqlab1/WQM/OWQI/R/Final/10yr/',"OWQI_10yr_seasonal_", WY, 'AWESOMER',".csv", sep=""))
#DTB - I changed the file path to save the .csv in the correct folder 1/9/18#

 }









