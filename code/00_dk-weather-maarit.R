#Libraries
#For data wrangling
library(lubridate)
library(tidyr)
library(dplyr)
#For DMI API handling
library(httr)
library(jsonlite)

##This script carries out dynamic wind and wetting corrections for DAILY rain gauge data, after DMI's methodology.
#See Allerup et al. 1997, Nordic Hydrology, 28
#See Korrigeret Nedbør 1989-2010, 2011-2012 & 2013, Technical Report 14-13, Flemming Vejen, Kenan Vilic, Hanne Jensen 2014, DMI.

##Download the following variables: Precipitation, Mean Temperature and Mean Wind Speed @ 10 meters.
#Other variables may not be carried safely through this script. Check and make modification accordingly.

#________________________________________
#Part 1: Loading and wrangling data

#Option A: Pull data from DMI's API 
Request_base <- "https://dmigw.govcloud.dk/v2/"
Service_ID <- "climateData/" #(climatedata contains quality-controlled  station and grid data: https://confluence.govcloud.dk/pages/viewpage.action?pageId=41717434) #For raw data use "metObs"
Collection_ID <- "10kmGridValue/" #Also available in climateData: "20kmGridValue" and "stationValue"
Station_ID <- "" #Look up the DMI station Id's here: https://confluence.govcloud.dk/pages/viewpage.action?pageId=41717446
Grid_ID <- "10km_625_53" #Find your grid cell ID here: https://dmidk.github.io/Climate-Data-Grid-Map/
Date_from <- "2022-01-01T00:00:00Z" #Always format yyyy-mm-ddThh:mm:ssZ
Date_to <- "2023-01-01T00:00:00Z" #Always format yyyy-mm-ddThh:mm:ssZ
API_key <- "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" #Your climateData API key (or metObs API key for raw data without quality control)
Request <- paste(Request_base,Service_ID,"collections/",Collection_ID,"items?cellId=",Grid_ID,"&datetime=",Date_from,"/",Date_to,"&timeResolution=day&limit=100000&api-key=",API_key,sep="")

#Send request to DMI's API and process the response. Might take a few seconds for big datasets. 
#There seems to be a limit of 100,000 data points per request.
#We are pulling ALL variables, then selecting locally. 
#Downloading all the variables every time is heavy, but it's not prohibitive with daily data over 10 years or less.
#There is an option to download one variable at a time but it is not implemented here. Will use it for hourly data when I get to it.
#There is NO option to specify a subset of variables.
#There is a service for Bulk Downloads, but haven't checked it out yet.
APIraw <- GET(Request) #Test with this: APIraw <- GET("https://dmigw.govcloud.dk/v2/climateData/collections/10kmGridValue/items?cellId=10km_613_65&datetime=2018-02-12T00:00:00Z/2018-03-18T00:00:00Z&parameterId=mean_wind_speed&timeResolution=day&limit=10&offset=0&api-key=84d32930-d250-457d-8791-bc60c099007e") #This one works
APIlist <- data.frame(fromJSON(rawToChar(APIraw$content), flatten = TRUE)$features) #Check for errors with this: fromJSON(rawToChar(APIraw$content), flatten = TRUE)$error
meteo <- APIlist %>% select(properties.from,properties.cellId,properties.parameterId,properties.value) %>% filter(properties.parameterId %in% c("mean_radiation","mean_temp","mean_wind_speed","acc_precip")) %>% pivot_wider(names_from=properties.parameterId,values_from=properties.value) %>% data.frame() #mean_radiation has units of MJ/m^2, so it must be total radiation (305-2800 nm)

#Wrangle and rename
meteo$date <- meteo$properties.from %>% substr(1,13) %>% ymd_h() %>% date()
meteo <- select(meteo,date,"metp"=mean_temp,"meanwv"=mean_wind_speed,"glorad"=mean_radiation,"prec"=acc_precip) #Here I selected the variables I'm usually interested in. Modify locally according to your needs.
rm(API_key,Collection_ID,Date_from,Date_to,Grid_ID,Request_base,Service_ID,Station_ID,Request,APIraw,APIlist)
meteo <- meteo[order(meteo$date),]
row.names(meteo) <- c(1:nrow(meteo))

#Option B: read your own weather data file. Need at least daily precipitation, mean temperature and mean wind speed.
meteo2 <- read.csv(file="your file path",stringsAsFactors = F,header=T)#If you have the file in .csv, plug the file path here
meteo <- mutate(meteo,date=date(dmy(date)))
#Check column names. MUST BE: Date = "date", precipitation = "prec", temperature = "metp, wind speed = "meanwv"

#________________________________________

#Part 2: Cleanup of missing data. Note: The latest data in AGRO database requires minimal to no cleanup.
#ONLY if you distrust your dataset. Otherwise, skip to Part 3.
str(meteo)

meteo <- mutate(meteo,mnd=month(date))

meteo$prec <- as.numeric(meteo$prec)
meteo$temp <- as.numeric(meteo$temp)
meteo$nwv <- as.numeric(meteo$meanwv)

#We grab monthly averages, stratified by hours, of some variables to fill gaps
averages <- meteo %>% group_by(mnd) %>% summarise(m_temp=mean(temp,na.rm=T),meanwv=mean(wv,na.rm=T)) %>% data.frame()

#We may have rows missing altogether, so this is a list of all the days and hours in the time period
thetimes <- data.frame(date=seq(meteo$date[1],meteo$date[nrow(meteo)],"days"))
meteo <- full_join(thetimes,meteo,by="date")
meteo <- mutate(meteo,mnd=month(date))
meteo <- mutate(meteo,d=mday(date))
length(meteo$date[is.na(meteo$date)==T]) #Check here how many rows are missing from extracted data
missing_rows <- filter(meteo,is.na(date)==T) #Check here which rows are missing from extracted data

#Filling gaps
meteo$prec[is.na(meteo$prec)==T] <- 0 #When row is missing altogether we assume precipitation = 0

#Loop to fill missing rows with month-hourly average values
for(m in (1:12)) { 
  for (d in 0:31) {
    meteo$metp[meteo$mnd==m & meteo$d==d & is.na(meteo$metp)==T] <- averages$m_metp[averages$mnd==m]
    meteo$meanwv[meteo$mnd==m & meteo$d==d & is.na(meteo$meanwv)==T] <- averages$m_meanwv[averages$mnd==m]
  }
}

#Check that these return nothing
meteo$datum[is.na(meteo$prec)==T]
meteo$datum[is.na(meteo$metp)==T]
meteo$datum[is.na(meteo$meanwv)==T]

#And clean up the memory
meteo$station <- NULL
meteo$date <- NULL
rm(averages)
rm(meteo1)
#rm(meteo2)
rm(thetimes)
missing_rows <- select(missing_rows,datum)

#________________________________________

#Part 3: Calculating parameters and preliminary variables.
#Adjust Wind data to 1.5 m using the log wind profile law
#Make sure you don't already have wind speed at 1.5 m
displacement <- 0
rough_height <- 0.023
corr_wind = log((1.5-displacement)/rough_height)/log((10-displacement)/rough_height)
meteo <- mutate(meteo,wind_150cm=meanwv*corr_wind) 

#Constrain Wind, Temp and Precipitation data to max and min valid for the model (based on Stisen et al. 2012, Hydrol. Earth Syst. Sci, 16)
meteo$wind_150cm[meteo$metp>=0&meteo$wind_150cm>15] <- 15
meteo$wind_150cm[meteo$metp<0&meteo$wind_150cm>7] <- 7
meteo$metp[meteo$metp< (-12)] <- -12
meteo$prec_capped <- meteo$prec
meteo$prec_capped[meteo$prec_capped>15] <- 15 # The capped precipitation is only used in the k_rain factor

#Shelter correction for rain gauge
##OBS! No shelter. The rain gauge is in an open field. Less than 1% reduction from Foulumgaard towards the north

#Calculate alpha 
#We use the simple rule: T > 2 -> all rain, 0 < T < 2 -> 50-50 rain and snow, T < 0 -> All snow
meteo <- mutate(meteo,alpha=0)
meteo$alpha[meteo$metp<0] <- 1
meteo$alpha[meteo$metp>=0 & meteo$metp<2] <- 1-0.5*meteo$metp[meteo$metp>=0 & meteo$metp<2]

#Reported Rain constants for Hellmann, Pluvio, Rimco. DMI Technical Report 14-13, 2014.
gamma0 <- 0.007697
gamma1 <- 0.034331
gamma2 <- -0.00101
gamma3 <- -0.012177

#Reported Snow constants for Hellmann, Pluvio, Rimco. DMI Technical Report 14-13, 2014.
beta0 <- 0.04587
beta1 <- 0.23677
beta2 <- 0.017979
beta3 <- -0.015407

#Wetting parameters. DMI Technical Report 14-13, 2014.
##OBS! slow process.
wetting_rain <- c(0.16,0.18,0.25,0.33,0.23,0.25,0.25,0.23,0.20,0.16,0.22,0.17)
wetting_snow <- c(0.17,0.19,0.27,0.35,0,0,0,0,0,0,0.23,0.18)

meteo <- mutate(meteo,w_r=0)
meteo <- mutate(meteo,w_s=0)
meteo$dy <- mday(meteo$date)
meteo$yr <- year(meteo$date)

#Fast version. Only works if there are NO GAPS in dates
i <- 1
ptm <- proc.time()
for (y in c(min(year(meteo$date)):max(year(meteo$date)))){
  for (m in c(min(month(meteo$date)[year(meteo$date)==y]):max(month(meteo$date)[year(meteo$date)==y]))){
    wet_r <- wetting_rain[m]
    wet_s <- wetting_snow[m]
    for (d in c(min(mday(meteo$date)[year(meteo$date)==y & month(meteo$date)==m]):max(mday(meteo$date)[year(meteo$date)==y & month(meteo$date)==m]))){
      if(meteo$prec[i]>0 & meteo$metp[i]<0){
        meteo$w_r[i] <- 0
        meteo$w_s[i] <- wet_s
      }
      else if(meteo$prec[i]>0 & meteo$metp[i]>=0){
        meteo$w_r[i] <- wet_r
        meteo$w_s[i] <- 0
      }
      i <- i+1
    }
  }
}
proc.time() - ptm

#Part 4: Applying the corrections.
#Allerup precipitation correction
meteo <- mutate(meteo,k_rain=1)
meteo <- mutate(meteo,k_snow=1)

meteo$k_rain <- exp(gamma0+(gamma1*meteo$wind_150cm)+(gamma2*log(meteo$prec_capped))+(gamma3*meteo$wind_150cm*log(meteo$prec_capped)))
meteo$k_snow <- exp(beta0+(beta1*meteo$wind_150cm)+(beta2*meteo$metp)+(beta3*meteo$wind_150cm*meteo$metp))

meteo$k_rain[meteo$prec==0] <- 1 #God, this is inelegant
meteo$k_snow[meteo$prec==0] <- 1 #But it works

meteo <- mutate(meteo,k_Allerup=(1-alpha)*k_rain+alpha*k_snow)
meteo <- mutate(meteo,Allerup_precip=prec*k_Allerup)

#Allerup + wetting precipitation correction
##OBS: Only if you ran the slow calculation before
meteo <- mutate(meteo,Allerup_wetting_precip=prec)
meteo$Allerup_wetting_precip <- (1-meteo$alpha)*((meteo$k_rain*meteo$prec)+(meteo$metp>=0)*meteo$w_r) + (meteo$alpha*meteo$k_snow)*(meteo$prec+(meteo$metp<0)*meteo$w_s)
meteo$Allerup_wetting_precip[meteo$prec==0] <- 0


##General files

# ALWAYS PLOT CORRECTION FIRST. Look for outliers.
plot(meteo$prec,meteo$Allerup_wetting_precip,main="Dynamic precipitation correction",xlab="Uncorrected precip. (mm/h)",ylab="Wind + wetting corrected precip. (mm/h)")
abline(a=0,b=1)

#Export as .csv
Final <- meteo %>% select(date,prec,Allerup_wetting_precip,metp)
colnames(Final) <- c("datetime","uncorrected_prec","Allerup_wetting_prec","mean_temp")
write.csv(Final,file="../your file path here")

#Other plots to check
plot(meteo$date,meteo$metp,type="l",main="Mean daily temperature",xlab="Date",ylab="(deg C)")
plot(meteo$date,meteo$prec,type="l",main="Uncorrected daily prec.",xlab="Date",ylab="(mm)")
plot(meteo$date,meteo$wind_150cm,type="l",main="Mean wind speed @ 1.5 m",xlab="Date",ylab="(m/s)")

#Compute yearly totals and differences
meteo_yr <- meteo %>% group_by(yr) %>% summarize(tot_precip=sum(prec),tot_precip_Allerup=sum(Allerup_precip),tot_precip_Allerup_wetting=sum(Allerup_wetting_precip)) %>% data.frame()
meteo_yr$diff_Allerup <- meteo_yr$tot_precip_Allerup-meteo_yr$tot_precip
meteo_yr$diff_Allerup_wetting <- meteo_yr$tot_precip_Allerup_wetting-meteo_yr$tot_precip
