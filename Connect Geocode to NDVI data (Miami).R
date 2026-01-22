#Load packages for script
library(tidyr)
library("anytime")   
library("lubridate")
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)
library("anytime")  


# Read in the SOL CSV file, specifying headers and preventing FIPS codes from being read as numbers

df_before_2015<- read.csv("P:/HCHS/drjulius/Florida_Participants.csv", header=TRUE, stringsAsFactors=FALSE)
# split dataframe into two based on date column
#df_before_2015<-subset(master_dataframe, DATE <as.Date("2015-01-01"))    
df_before_2015$CLINDATE_V2<-NULL
df_before_2015$CLINDATE<-NULL
unique_IDs_beginning<-unique(df_before_2015$ID)
#_______________________________________________


df_before_2015$FIPS<-as.character(df_before_2015$BLOCKFIPS)
df_before_2015$DATE<-NULL
df_before_2015$SEQUENTIAL_NUM<-NULL
#remove all dates that exceed the endpoint of the NDVI data
df_before_2015<-df_before_2015[as.Date(df_before_2015$DATE_FROM)<=as.Date("2019-12-31"),]

# ****extract a single participant as a TEST****

#df_before_2015<-head(df_before_2015,7)

# Convert date columns to R-recognizable date formats
df_before_2015$DATE_TO[is.na(df_before_2015$DATE_TO)]<-"2019-12-31"
df_before_2015$DATE_FROM<-anydate(df_before_2015$DATE_FROM)
df_before_2015$DATE_TO<-anydate(df_before_2015$DATE_TO)

# Create a shortened version of the date columns with only year and month
df_before_2015$DATE_FROM2<-substr(df_before_2015$DATE_FROM,1,7)
df_before_2015$DATE_TO2<-substr(df_before_2015$DATE_TO,1,7)
df_before_2015$DATE_FROM2<-anydate(df_before_2015$DATE_FROM2)
df_before_2015$DATE_TO2<-anydate(df_before_2015$DATE_TO2)

# Assign a unique ID for each row in the data set
df_before_2015$Uni<-seq.int(nrow(df_before_2015))

# Create new entries in the data set for each month a participant lived at an address

df_before_2015_2 <- setDT(df_before_2015)[, list(ID = ID, LAT_WGS84 = LAT_WGS84, LON_WGS84 = LON_WGS84, FIPS = FIPS, Uni=Uni,
                                                 SEP_DATE2 = seq.Date(as.Date(DATE_FROM2), as.Date(DATE_TO2), by = "month")), by = 1:nrow(df_before_2015)]

# Merge the new data with the original data
df_before_2015_2 <- merge(df_before_2015_2, df_before_2015, by = c("Uni"), all = TRUE)

# Remove unnecessary columns
df_before_2015_2 = subset(df_before_2015_2, select = -c(Uni,MATCHTYPE,BASECENTER,DATE_TO2,DATE_FROM2,DATE_TO,FIPS.y,NORTHING,EASTING,UTMZONE,LAT_NAD83,LON_NAD83,LAT_WGS84.y,LON_WGS84.y,ID.y,LAT_WGS84.x,LON_WGS84.x) )

# Rename columns for clarity
df_before_2015_2$SEP_DATE<-df_before_2015_2$DATE_FROM
df_before_2015_2$DATE_FROM<-NULL
df_before_2015_2$ID<-df_before_2015_2$ID.x
df_before_2015_2$ID.x<-NULL
df_before_2015_2$FIPS<-df_before_2015_2$FIPS.x
df_before_2015_2$FIPS.x<-NULL
df_before_2015_2$X<-NULL
df_before_2015_2$BREAKPOINT<-ifelse(substr(df_before_2015_2$SEP_DATE2,1,7)==substr(df_before_2015_2$SEP_DATE,1,7),as.character(df_before_2015_2$SEP_DATE),NA)

# Determine the seasonal period for each month based on its month value
df_before_2015_2$YEAR <- as.numeric(format(df_before_2015_2$SEP_DATE2, "%Y"))
df_before_2015_2$MONTH <- as.numeric(format(df_before_2015_2$SEP_DATE2, "%m"))
df_before_2015_2$DAY <- as.numeric(format(df_before_2015_2$SEP_DATE2, "%d"))
yq <- as.yearqtr(as.yearmon(df_before_2015_2$SEP_DATE2, "%m/%d/%Y") + 1/12)
df_before_2015_2$SEASON <- factor(format(yq, "%q"), levels = 1:4, 
                                  labels = c("winter", "spring", "summer", "fall"))
df_before_2015_2$YEAR2 <- df_before_2015_2$YEAR
df_before_2015_2$YEAR2[df_before_2015_2$MONTH==12] <- df_before_2015_2$YEAR[df_before_2015_2$MONTH==12]+1
df_before_2015_2$PERIOD<-paste(df_before_2015_2$YEAR2,df_before_2015_2$SEASON)
df_before_2015_2<-arrange(df_before_2015_2,YEAR,MONTH)

# Create a new data set with a sequence of dates for each participant's residence at an address
#new_data<-setDT(df_before_2015)[ , list(ID = ID,LAT_WGS84=LAT_WGS84, LON_WGS84=LON_WGS84, FIPS=FIPS, SEP_DATE = seq(as.Date(DATE_FROM), as.Date(DATE_TO), by = "month")), by = 1:nrow(df_before_2015)]
#new_data<-setDT(new_data)[,list(SEP_DATE=seq(as.Date(SEP_DATE[1]),as.Date(SEP_DATE[length(SEP_DATE)]),by="month")),by=.(ID,LAT_WGS84,LON_WGS84,FIPS)]
#new_data<-merge(new_data,df_before_2015,by=c("ID","LAT_WGS84","LON_WGS84","FIPS","SEP_DATE"),all = TRUE)



# Set Days_at_location column based on month: 30 days for even months, 31 days for odd months (except February)

df_before_2015_2$Days_at_location<- ifelse(df_before_2015_2$MONTH %% 2 == 0, "30", "31")
df_before_2015_2$Days_at_location <- ifelse(df_before_2015_2$MONTH == 8, 31, df_before_2015_2$Days_at_location)
df_before_2015_2$Days_at_location <- ifelse(df_before_2015_2$MONTH == 9, 30, df_before_2015_2$Days_at_location)
df_before_2015_2$Days_at_location <- ifelse(df_before_2015_2$MONTH == 10, 31, df_before_2015_2$Days_at_location)
df_before_2015_2$Days_at_location <- ifelse(df_before_2015_2$MONTH == 11, 30, df_before_2015_2$Days_at_location)
df_before_2015_2$Days_at_location <- ifelse(df_before_2015_2$MONTH == 12, 31, df_before_2015_2$Days_at_location)

# Determine if the year is a leap year
df_before_2015_2$is_leap_year <- ifelse((df_before_2015_2$YEAR %% 4 == 0 & df_before_2015_2$YEAR %% 100 != 0) | df_before_2015_2$YEAR %% 400 == 0, TRUE, FALSE)

# Adjust the number of days in February based on whether the year is a leap year or not
df_before_2015_2$Days_at_location[df_before_2015_2$MONTH == 2] <- 28
df_before_2015_2$Days_at_location[df_before_2015_2$MONTH == 2 & df_before_2015_2$is_leap_year == TRUE] <- 29

# Remove the is_leap_year column
df_before_2015_2$is_leap_year<- NULL

# Set the SEP_DATE column of df_before_2015_2 to the SEP_DATE column of new_data   

df_before_2015_2$SEP_DATE<-paste(format(df_before_2015_2$SEP_DATE2,"%Y-%m"),format(df_before_2015_2$SEP_DATE,"%d"),sep = "-")

# Rename the third column of df_before_2015_2 to DATE_FROM    
colnames(df_before_2015_2)[5] <- "DATE_FROM"

# Convert DATE_FROM column of df_before_2015 and df_before_2015_2 to character    
df_before_2015$DATE_FROM<-as.character(df_before_2015$DATE_FROM)
df_before_2015_2$DATE_FROM<-as.character(df_before_2015_2$DATE_FROM)

# Merge df_before_2015_2 and df_before_2015 using DATE_FROM and ID columns, keep all rows from both datasets 
df_before_2015_test <- merge(df_before_2015_2, df_before_2015, by = c("DATE_FROM", "ID"), all = TRUE)

# Convert Days_at_location and DAY columns of df_before_2015_test to numeric
df_before_2015_test$Days_at_location <- as.numeric(df_before_2015_test$Days_at_location)
df_before_2015_test$DAY <- as.numeric(substr(df_before_2015_test$DATE_FROM,9,17))

# If BASECENTER column is NA, set Days_at_location to its original value, otherwise adjust it based on DAY
df_before_2015_test$Days_at_location<-as.numeric(df_before_2015_test$Days_at_location)
df_before_2015_test$Days_at_location <- ifelse(is.na(df_before_2015_test$BASECENTER), df_before_2015_test$Days_at_location, (df_before_2015_test$Days_at_location-df_before_2015_test$DAY+1))    

#Remove unnessecary columns
df_before_2015_test[, c("MATCHTYPE","FIPS.y", "BLOCKFIPS.x", "NORTHING", "BLOCKFIPS.y", "EASTING", "UTMZONE", "LAT_NAD83", "LON_NAD83", "DATE_TO", "BASECENTER", "Uni", "DATE_TO2", "LAT_WGS84", "LON_WGS84", "DATE_FROM2")] <- NULL

# Rename column FIPS.x to FIPS  
colnames(df_before_2015_test)[6] <- "FIPS"   

# Add leading zeros to MONTH column and create a YEAR_MONTH column
df_before_2015_test$MONTH2 <- sprintf("%02d", df_before_2015_test$MONTH)
df_before_2015_test$YEAR_MONTH<-paste(df_before_2015_test$YEAR,"-",df_before_2015_test$MONTH2,sep="")

# Create a YEAR_MONTH column in df_before_2015
df_before_2015$YEAR_MONTH<-substr(as.character(df_before_2015$DATE_TO),1,nchar(as.character(df_before_2015$DATE_TO))-3)

# Select relevant columns from df_before_2015 and rename some columns   

df_before_2015_select <- df_before_2015[, .(YEAR_MONTH, ID, FIPS, DATE_TO)]
df_before_2015_select$BREAKPOINT<-NA
# Merge df_before_2015_test and df_before_2015_select by YEAR_MONTH, ID, and FIPS
df_before_2015_test <- merge(df_before_2015_test, df_before_2015_select, by = c("YEAR_MONTH", "ID", "FIPS","BREAKPOINT"), all = TRUE)

# Arrange df_before_2015_test by DATE_FROM and ID
df_before_2015_test <- arrange(df_before_2015_test, DATE_FROM, ID)

df_before_2015_test$Days_2<-as.numeric(substr(as.character(df_before_2015_test$DATE_TO),9,nchar(as.character(df_before_2015_test$DATE_TO))))

df_before_2015_test$Days_at_location <- ifelse(is.na(df_before_2015_test$Days_2), df_before_2015_test$Days_at_location, (df_before_2015_test$Days_2))#<-change the last part here
df_before_2015_test$Days_2<-NULL
df_before_2015_test$MONTH2<-NULL
df_before_2015_test$DATE_TO<-NULL
#create a new dataframe that removes redundant date columns and keep the period (season+year) column
df_before_2015_3 = subset(df_before_2015_test, select = -c(DATE_FROM,MONTH,YEAR,DAY,YEAR_MONTH,BREAKPOINT,X) )

#ensure FIPS stays as characters and not scientific notation 
df_before_2015_final<-df_before_2015_3
df_before_2015_final$FIPS<-as.character(df_before_2015_final$FIPS)

df_after_2015_final<-subset(df_before_2015_final, FIPSCENYR.x=='2020')   
df_before_2015_final<-subset(df_before_2015_final, FIPSCENYR.x =='2010')   
#using the NDVI census blocks prepare the data so that it can match with the SOL geodata table


#attach walkability scores.

Walk<-read.csv("P:/HCHS/drjulius/South Florida Walkability.csv")
Walk<-subset(Walk,select=c("GEOID10","NatWalkInd"))
Walk$GEOID10<-as.character(Walk$GEOID10)
df_after_2015_final$GEOID10<-substr(df_after_2015_final$FIPS,1,nchar(df_after_2015_final$FIPS)-3)
df_before_2015_final$GEOID10<-substr(df_before_2015_final$FIPS,1,nchar(df_before_2015_final$FIPS)-3)

df_after_2015_final <- df_after_2015_final %>% left_join(Walk, by=c("GEOID10"))
df_before_2015_final <- df_before_2015_final %>% left_join(Walk, by=c("GEOID10"))

write.csv(df_before_2015_final, file = "P:/HCHS/drjulius/Miami_before_2015.csv")

#Perform date attribution#merge the different NDVI csv tables.
NDVI_data_1 <- read.csv("P:/HCHS/drjulius/Miami_CB_2010_NDVI_1.csv", header=TRUE, stringsAsFactors=FALSE)
NDVI_data_2 <- read.csv("P:/HCHS/drjulius/Miami_CB_2010_NDVI_2.csv", header=TRUE, stringsAsFactors=FALSE)
NDVI_data_3 <- read.csv("P:/HCHS/drjulius/Miami_CB_2010_NDVI_3.csv", header=TRUE, stringsAsFactors=FALSE)
NDVI_data_2010<-rbind(NDVI_data_1,NDVI_data_2,NDVI_data_3)

NDVI_data_2010$StdTime <- gsub('/', '-', NDVI_data_2010$StdTime)
NDVI_data_2010$StdTime_Ma <- gsub('/', '-', NDVI_data_2010$StdTime_Ma)

NDVI_data_2010$StdTime<- substring(NDVI_data_2010$StdTime, 1, nchar(NDVI_data_2010$StdTime)-5)
NDVI_data_2010$StdTime_Ma<- substring(NDVI_data_2010$StdTime_Ma, 1, nchar(NDVI_data_2010$StdTime_Ma)-5)

NDVI_data_2010$StdTime<-as.Date(NDVI_data_2010$StdTime,"%m-%d-%Y")
NDVI_data_2010$StdTime_Ma<-as.Date(NDVI_data_2010$StdTime_Ma,"%m-%d-%Y")


NDVI_data_2010$YEAR <- as.numeric(format(NDVI_data_2010$StdTime_Ma, "%Y"))
NDVI_data_2010$MONTH <- as.numeric(format(NDVI_data_2010$StdTime_Ma, "%m"))
yq <- as.yearqtr(as.yearmon(NDVI_data_2010$StdTime_Ma, "%Y-%m-%d") + 1/12)
NDVI_data_2010$SEASON <- factor(format(yq, "%q"), levels = 1:4, 
                                labels = c("winter", "spring", "summer", "fall"))

NDVI_data_2010$PERIOD<-paste(NDVI_data_2010$YEAR,NDVI_data_2010$SEASON)

NDVI_data_2010$FIPS<-as.character(NDVI_data_2010$GEOID10)

# join geodata and NDVI data based on FIPS and period

df_before_2015_4 <- df_before_2015_final %>% left_join(NDVI_data_2010, by=c("FIPS","PERIOD"))


#Test for summary using date level accuracy
df_before_2015_4$ID<-as.character(df_before_2015_4$ID)
df_before_2015_4$SEASON2<-as.numeric(df_before_2015_4$StdTime)
df_before_2015_4 = subset(df_before_2015_4, select = -c(SEASON.y,MONTH,YEAR,StdTime_Ma,Dimensions,Variable,AREA,COUNT,ZONE_CODE) )
df_before_2015_4$Cum_NDVI<-df_before_2015_4$Days_at_location*df_before_2015_4$MEDIAN
df_before_2015_4$Cum_NatWalkInd<-df_before_2015_4$NatWalkInd*df_before_2015_4$Days_at_location
FinalDataFrame_2015<-df_before_2015_4%>%group_by(ID, PERIOD)%>%summarise_at(vars("Cum_NDVI","Days_at_location","Cum_NatWalkInd"),sum)
FinalDataFrame_2015$NDVI<-FinalDataFrame_2015$Cum_NDVI/FinalDataFrame_2015$Days_at_location
FinalDataFrame_2015$Season<-substr(FinalDataFrame_2015$PERIOD,5,nchar(as.character(FinalDataFrame_2015$PERIOD)))
FinalDataFrame_2015$Year<-substr(FinalDataFrame_2015$PERIOD,1,4)

FinalDataFrame_2015$SEASON_NUM <- ifelse(FinalDataFrame_2015$Season == ' winter', 1,
                                         ifelse(FinalDataFrame_2015$Season == ' spring', 2,
                                                ifelse(FinalDataFrame_2015$Season == ' summer', 3, 4)))
FinalDataFrame_2015 <- arrange(FinalDataFrame_2015,ID, Year, SEASON_NUM)
FinalDataFrame_2015$TIME<-paste(FinalDataFrame_2015$Year,"-",FinalDataFrame_2015$SEASON_NUM,sep = "")


#example of getting a NDVI plot over time for a single participant

FinalDataFrame_2015<-FinalDataFrame_2015 %>%drop_na()

#using the NDVI census blocks for the 2020 census FIPS prepare the data so that it can match with the SOL geodata table
NDVI_data_1 <- read.csv("P:/HCHS/drjulius/Miami_CB_2020_NDVI_1.csv", header=TRUE, stringsAsFactors=FALSE)
NDVI_data_2 <- read.csv("P:/HCHS/drjulius/Miami_CB_2020_NDVI_2.csv", header=TRUE, stringsAsFactors=FALSE)
#NDVI_data_3 <- read.csv("P:/HCHS/drjulius/Miami_CB_2020_NDVI_3.csv", header=TRUE, stringsAsFactors=FALSE)
NDVI_data_2020<-rbind(NDVI_data_1,NDVI_data_2)

NDVI_data_2020$StdTime <- gsub('/', '-', NDVI_data_2020$StdTime)
NDVI_data_2020$StdTime_Ma <- gsub('/', '-', NDVI_data_2020$StdTime_Ma)

NDVI_data_2020$StdTime<- substring(NDVI_data_2020$StdTime, 1, nchar(NDVI_data_2020$StdTime)-5)
NDVI_data_2020$StdTime_Ma<- substring(NDVI_data_2020$StdTime_Ma, 1, nchar(NDVI_data_2020$StdTime_Ma)-5)

NDVI_data_2020$StdTime<-as.Date(NDVI_data_2020$StdTime,"%m-%d-%Y")
NDVI_data_2020$StdTime_Ma<-as.Date(NDVI_data_2020$StdTime_Ma,"%m-%d-%Y")


NDVI_data_2020$YEAR <- as.numeric(format(NDVI_data_2020$StdTime_Ma, "%Y"))
NDVI_data_2020$MONTH <- as.numeric(format(NDVI_data_2020$StdTime_Ma, "%m"))
yq <- as.yearqtr(as.yearmon(NDVI_data_2020$StdTime_Ma, "%Y-%m-%d") + 1/12)
NDVI_data_2020$SEASON <- factor(format(yq, "%q"), levels = 1:4, 
                                labels = c("winter", "spring", "summer", "fall"))

NDVI_data_2020$PERIOD<-paste(NDVI_data_2020$YEAR,NDVI_data_2020$SEASON)

NDVI_data_2020$FIPS<-as.character(NDVI_data_2020$GEOID20)

# join geodata and NDVI data based on FIPS and period

df_after_2015_4 <- df_after_2015_final %>% left_join(NDVI_data_2020, by=c("FIPS","PERIOD"))


#Test for summary using date level accuracy
df_after_2015_4$ID<-as.character(df_after_2015_4$ID)
df_after_2015_4$SEASON2<-as.numeric(df_after_2015_4$StdTime)
df_after_2015_4 = subset(df_after_2015_4, select = -c(SEASON.y,MONTH,YEAR,StdTime_Ma,Dimensions,Variable,AREA,COUNT,ZONE_CODE) )
df_after_2015_4$Cum_NDVI<-df_after_2015_4$Days_at_location*df_after_2015_4$MEDIAN
df_after_2015_4$Cum_NatWalkInd<-df_after_2015_4$NatWalkInd*df_after_2015_4$Days_at_location
FinalDataFrame_2020<-df_after_2015_4%>%group_by(ID, PERIOD)%>%summarise_at(vars("Cum_NDVI","Days_at_location","Cum_NatWalkInd"),sum)
FinalDataFrame_2020$NDVI<-FinalDataFrame_2020$Cum_NDVI/FinalDataFrame_2020$Days_at_location
FinalDataFrame_2020$Season<-substr(FinalDataFrame_2020$PERIOD,5,nchar(as.character(FinalDataFrame_2020$PERIOD)))
FinalDataFrame_2020$Year<-substr(FinalDataFrame_2020$PERIOD,1,4)

FinalDataFrame_2020$SEASON_NUM <- ifelse(FinalDataFrame_2020$Season == ' winter', 1,
                                         ifelse(FinalDataFrame_2020$Season == ' spring', 2,
                                                ifelse(FinalDataFrame_2020$Season == ' summer', 3, 4)))
FinalDataFrame_2020 <- arrange(FinalDataFrame_2020,ID, Year, SEASON_NUM)
FinalDataFrame_2020$TIME<-paste(FinalDataFrame_2020$Year,"-",FinalDataFrame_2020$SEASON_NUM,sep = "")


#example of getting a NDVI plot over time for a single participant

FinalDataFrame_2020<-FinalDataFrame_2020 %>%drop_na()

FINAL_COMBINED<-rbind(FinalDataFrame_2015,FinalDataFrame_2020)
FINAL_COMBINED$NatWalkInd<-FINAL_COMBINED$Cum_NatWalkInd/FINAL_COMBINED$Days_at_location

write.csv(FINAL_COMBINED, file = "P:/HCHS/drjulius/Miami_Participant_Greenness_Scores_2009-2019.csv")

#test to see if data worked.

unique_IDs<-unique(FINAL_COMBINED$ID)
unique_IDs_list<-as.list(unique_IDs)
n_unique_IDs<-length(unique_IDs_list)
n_sample<-round(n_unique_IDs*0.001)
sample_unique_IDs<-sample(unique_IDs_list,n_sample)

#____________________________
example<-subset(FINAL_COMBINED, ID %in% sample_unique_IDs)

List<-example %>%
  group_by(ID) %>%
  summarise(example=any(TIME=="2019-1"))

ggplot(example, aes(x=TIME, y=NDVI, group=ID, color=ID)) + geom_line()+xlab("Year")+ylab("NDVI")+ylim(0,1)
