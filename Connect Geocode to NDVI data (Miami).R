
###############################################################################
# NDVI Temporal Attribution Pipeline
#
# Description:
# This script demonstrates a geospatial data engineering workflow for linking
# remotely sensed NDVI data to longitudinal location histories and aggregating
# exposure metrics across time and seasons. 
#
# Key concepts demonstrated:
# - Temporal expansion of residence intervals
# - Partial-period weighting and leap year handling
# - Entity-based joins between geospatial units and time-series data
# - Scalable aggregation of NDVI exposure metrics
#
# Notes:
# - All file paths are configurable
# - Intended as a portfolio demonstration
###############################################################################

# =============================
#Load packages for script
# =============================

library(tidyr)
library(anytime)   
library(lubridate)
library(dplyr)
library(zoo)
library(data.table)
library(ggplot2)

# =============================
# Read in the participant residential history CSV file, specifying headers and preventing Census FIPS codes from being read as numbers
# =============================

      participant_location_data<- read.csv("Path to partipant location data", header=TRUE, stringsAsFactors=FALSE)
      participant_location_data$CLINDATE_V2<-NULL
      participant_location_data$CLINDATE<-NULL
      unique_IDs_beginning<-unique(participant_location_data$ID)
      participant_location_data$FIPS<-as.character(participant_location_data$BLOCKFIPS)
      participant_location_data$DATE<-NULL
      participant_location_data$SEQUENTIAL_NUM<-NULL
      
# =============================
# Remove all dates that exceed the endpoint of the NDVI data
# =============================
      
      participant_location_data<-participant_location_data[as.Date(participant_location_data$DATE_FROM)<=as.Date("2019-12-31"),]

# =============================      
# Convert date columns to R-recognizable date formats
# =============================
      
      participant_location_data$DATE_TO[is.na(participant_location_data$DATE_TO)]<-"2019-12-31"
      participant_location_data$DATE_FROM<-anydate(participant_location_data$DATE_FROM)
      participant_location_data$DATE_TO<-anydate(participant_location_data$DATE_TO)
      
# =============================
# Create a shortened version of the date columns with only year and month
# =============================
      
      participant_location_data$DATE_FROM2<-substr(participant_location_data$DATE_FROM,1,7)
      participant_location_data$DATE_TO2<-substr(participant_location_data$DATE_TO,1,7)
      participant_location_data$DATE_FROM2<-anydate(participant_location_data$DATE_FROM2)
      participant_location_data$DATE_TO2<-anydate(participant_location_data$DATE_TO2)

# =============================
# Assign a unique ID for each row in the data set
# =============================
      
			participant_location_data$Uni<-seq.int(nrow(participant_location_data))

# =============================      
# Create new entries in the data set for each month a participant lived at an address
# =============================
      
			participant_location_data_new <- setDT(participant_location_data)[, list(ID = ID, LAT_WGS84 = LAT_WGS84, LON_WGS84 = LON_WGS84, FIPS = FIPS, Uni=Uni,
                                                 SEP_DATE2 = seq.Date(as.Date(DATE_FROM2), as.Date(DATE_TO2), by = "month")), by = 1:nrow(participant_location_data)]

# =============================
# Merge the new data with the original data
# =============================
      
      participant_location_data_new <- merge(participant_location_data_new, participant_location_data, by = c("Uni"), all = TRUE)

# =============================      
# Remove unnecessary columns
# =============================
      
			participant_location_data_new = subset(participant_location_data_new, select = -c(Uni,MATCHTYPE,BASECENTER,DATE_TO2,DATE_FROM2,DATE_TO,FIPS.y,NORTHING,EASTING,UTMZONE,LAT_NAD83,LON_NAD83,LAT_WGS84.y,LON_WGS84.y,ID.y,LAT_WGS84.x,LON_WGS84.x) )

# =============================      
# Rename columns for clarity
# =============================
			      
			participant_location_data_new$SEP_DATE<-participant_location_data_new$DATE_FROM
			participant_location_data_new$DATE_FROM<-NULL
			participant_location_data_new$ID<-participant_location_data_new$ID.x
			participant_location_data_new$ID.x<-NULL
			participant_location_data_new$FIPS<-participant_location_data_new$FIPS.x
			participant_location_data_new$FIPS.x<-NULL
			participant_location_data_new$X<-NULL
			participant_location_data_new$BREAKPOINT<-ifelse(substr(participant_location_data_new$SEP_DATE2,1,7)==substr(participant_location_data_new$SEP_DATE,1,7),as.character(participant_location_data_new$SEP_DATE),NA)

# =============================			
# Determine the seasonal period for each month based on its month value
# =============================
						
			participant_location_data_new$YEAR <- as.numeric(format(participant_location_data_new$SEP_DATE2, "%Y"))
			participant_location_data_new$MONTH <- as.numeric(format(participant_location_data_new$SEP_DATE2, "%m"))
			participant_location_data_new$DAY <- as.numeric(format(participant_location_data_new$SEP_DATE2, "%d"))
			yq <- as.yearqtr(as.yearmon(participant_location_data_new$SEP_DATE2, "%m/%d/%Y") + 1/12)
			participant_location_data_new$SEASON <- factor(format(yq, "%q"), levels = 1:4, 
			                                  labels = c("winter", "spring", "summer", "fall"))
			participant_location_data_new$YEAR2 <- participant_location_data_new$YEAR
			participant_location_data_new$YEAR2[participant_location_data_new$MONTH==12] <- participant_location_data_new$YEAR[participant_location_data_new$MONTH==12]+1
			participant_location_data_new$PERIOD<-paste(participant_location_data_new$YEAR2,participant_location_data_new$SEASON)
			participant_location_data_new<-arrange(participant_location_data_new,YEAR,MONTH)

# =============================
# Set Days_at_location column based on month: 30 days for even months, 31 days for odd months (except February)
# =============================

			participant_location_data_new$Days_at_location<- ifelse(participant_location_data_new$MONTH %% 2 == 0, "30", "31")
			participant_location_data_new$Days_at_location <- ifelse(participant_location_data_new$MONTH == 8, 31, participant_location_data_new$Days_at_location)
			participant_location_data_new$Days_at_location <- ifelse(participant_location_data_new$MONTH == 9, 30, participant_location_data_new$Days_at_location)
			participant_location_data_new$Days_at_location <- ifelse(participant_location_data_new$MONTH == 10, 31, participant_location_data_new$Days_at_location)
			participant_location_data_new$Days_at_location <- ifelse(participant_location_data_new$MONTH == 11, 30, participant_location_data_new$Days_at_location)
			participant_location_data_new$Days_at_location <- ifelse(participant_location_data_new$MONTH == 12, 31, participant_location_data_new$Days_at_location)

# =============================
# Determine if the year is a leap year
# =============================

			participant_location_data_new$is_leap_year <- ifelse((participant_location_data_new$YEAR %% 4 == 0 & participant_location_data_new$YEAR %% 100 != 0) | participant_location_data_new$YEAR %% 400 == 0, TRUE, FALSE)

# =============================			
# Adjust the number of days in February based on whether the year is a leap year or not
# =============================
			
			participant_location_data_new$Days_at_location[participant_location_data_new$MONTH == 2] <- 28
			participant_location_data_new$Days_at_location[participant_location_data_new$MONTH == 2 & participant_location_data_new$is_leap_year == TRUE] <- 29

# =============================
# Remove the is_leap_year column
# =============================

			participant_location_data_new$is_leap_year<- NULL
			
# =============================
# Set the SEP_DATE column of participant_location_data_new to the SEP_DATE column of new_data   
# =============================
			
			participant_location_data_new$SEP_DATE<-paste(format(participant_location_data_new$SEP_DATE2,"%Y-%m"),format(participant_location_data_new$SEP_DATE,"%d"),sep = "-")

# =============================
# Rename the third column of participant_location_data_new to DATE_FROM 
# =============================
			
			colnames(participant_location_data_new)[5] <- "DATE_FROM"

# =============================			
# Convert DATE_FROM column of participant_location_data and participant_location_data_new to character
# =============================
			
			participant_location_data$DATE_FROM<-as.character(participant_location_data$DATE_FROM)
			participant_location_data_new$DATE_FROM<-as.character(participant_location_data_new$DATE_FROM)
			
# =============================
# Merge participant_location_data_new and participant_location_data using DATE_FROM and ID columns, keep all rows from both datasets 
# =============================
			
			participant_location_data_test <- merge(participant_location_data_new, participant_location_data, by = c("DATE_FROM", "ID"), all = TRUE)

# =============================			
# Convert Days_at_location and DAY columns of participant_location_data_test to numeric
# =============================
			
			participant_location_data_test$Days_at_location <- as.numeric(participant_location_data_test$Days_at_location)
			participant_location_data_test$DAY <- as.numeric(substr(participant_location_data_test$DATE_FROM,9,17))

# =============================			
# If BASECENTER column is NA, set Days_at_location to its original value, otherwise adjust it based on DAY
# =============================
			
			participant_location_data_test$Days_at_location<-as.numeric(participant_location_data_test$Days_at_location)
			participant_location_data_test$Days_at_location <- ifelse(is.na(participant_location_data_test$BASECENTER), participant_location_data_test$Days_at_location, (participant_location_data_test$Days_at_location-participant_location_data_test$DAY+1))    

# =============================
#Remove unnessecary columns
# =============================
			
		participant_location_data_test[, c("MATCHTYPE","FIPS.y", "BLOCKFIPS.x", "NORTHING", "BLOCKFIPS.y", "EASTING", "UTMZONE", "LAT_NAD83", "LON_NAD83", "DATE_TO", "BASECENTER", "Uni", "DATE_TO2", "LAT_WGS84", "LON_WGS84", "DATE_FROM2")] <- NULL

# =============================			
# Rename column FIPS.x to FIPS 
# =============================
			
		colnames(participant_location_data_test)[6] <- "FIPS"   
			
# =============================
# Add leading zeros to MONTH column and create a YEAR_MONTH column
# =============================
			
		participant_location_data_test$MONTH2 <- sprintf("%02d", participant_location_data_test$MONTH)
		participant_location_data_test$YEAR_MONTH<-paste(participant_location_data_test$YEAR,"-",participant_location_data_test$MONTH2,sep="")

# =============================		
# Create a YEAR_MONTH column in participant_location_data
# =============================
		
		participant_location_data$YEAR_MONTH<-substr(as.character(participant_location_data$DATE_TO),1,nchar(as.character(participant_location_data$DATE_TO))-3)

# =============================
# Select relevant columns from participant_location_data and rename some columns   
# =============================
		
		participant_location_data_select <- participant_location_data[, .(YEAR_MONTH, ID, FIPS, DATE_TO)]
		participant_location_data_select$BREAKPOINT<-NA
		
# =============================
# Merge participant_location_data_test and participant_location_data_select by YEAR_MONTH, ID, and FIPS
# =============================
		
		participant_location_data_test <- merge(participant_location_data_test, participant_location_data_select, by = c("YEAR_MONTH", "ID", "FIPS","BREAKPOINT"), all = TRUE)

# =============================		
# Arrange participant_location_data_test by DATE_FROM and ID
# =============================
		
		participant_location_data_test <- arrange(participant_location_data_test, DATE_FROM, ID)
		participant_location_data_test$Days_2<-as.numeric(substr(as.character(participant_location_data_test$DATE_TO),9,nchar(as.character(participant_location_data_test$DATE_TO))))
		participant_location_data_test$Days_at_location <- ifelse(is.na(participant_location_data_test$Days_2), participant_location_data_test$Days_at_location, (participant_location_data_test$Days_2))#<-change the last part here
		participant_location_data_test$Days_2<-NULL
		participant_location_data_test$MONTH2<-NULL
		participant_location_data_test$DATE_TO<-NULL
		
# =============================		
#create a new dataframe that removes redundant date columns and keep the period (season+year) column
# =============================		
		
		participant_location_data_3 = subset(participant_location_data_test, select = -c(DATE_FROM,MONTH,YEAR,DAY,YEAR_MONTH,BREAKPOINT,X) )

# =============================	
#ensure FIPS stays as characters and not scientific notation 
# =============================
		
		participant_location_data_final<-participant_location_data_3
		participant_location_data_final$FIPS<-as.character(participant_location_data_final$FIPS)
		location_data_after_2015_final<-subset(participant_location_data_final, FIPSCENYR.x=='2020')   
		participant_location_data_final<-subset(participant_location_data_final, FIPSCENYR.x =='2010')   

# =============================		
#using the NDVI census blocks prepare the data so that it can match with the SOL geodata table
# =============================
				
		NDVI_data_1 <- read.csv("1989-2010 NDVI data location #1", header=TRUE, stringsAsFactors=FALSE)
		NDVI_data_2 <- read.csv("1989-2010 NDVI data location #2", header=TRUE, stringsAsFactors=FALSE)
		NDVI_data_3 <- read.csv("1989-2010 NDVI data location #3", header=TRUE, stringsAsFactors=FALSE)
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

# =============================		
# join geodata and NDVI data based on FIPS and period
# =============================

		participant_location_data_4 <- location_data_before_2015_final %>% left_join(NDVI_data_2010, by=c("FIPS","PERIOD"))

# =============================
#Test for summary using date level accuracy
# =============================
		
		participant_location_data_4$ID<-as.character(participant_location_data_4$ID)
		participant_location_data_4$SEASON2<-as.numeric(participant_location_data_4$StdTime)
		participant_location_data_4 = subset(participant_location_data_4, select = -c(SEASON.y,MONTH,YEAR,StdTime_Ma,Dimensions,Variable,AREA,COUNT,ZONE_CODE) )
		participant_location_data_4$Cum_NDVI<-participant_location_data_4$Days_at_location*participant_location_data_4$MEDIAN
		participant_location_data_4$Cum_NatWalkInd<-participant_location_data_4$NatWalkInd*participant_location_data_4$Days_at_location
		FinalDataFrame_2015<-participant_location_data_4%>%group_by(ID, PERIOD)%>%summarise_at(vars("Cum_NDVI","Days_at_location","Cum_NatWalkInd"),sum)
		FinalDataFrame_2015$NDVI<-FinalDataFrame_2015$Cum_NDVI/FinalDataFrame_2015$Days_at_location
		FinalDataFrame_2015$Season<-substr(FinalDataFrame_2015$PERIOD,5,nchar(as.character(FinalDataFrame_2015$PERIOD)))
		FinalDataFrame_2015$Year<-substr(FinalDataFrame_2015$PERIOD,1,4)
		FinalDataFrame_2015$SEASON_NUM <- ifelse(FinalDataFrame_2015$Season == ' winter', 1,
		                                         ifelse(FinalDataFrame_2015$Season == ' spring', 2,
		                                                ifelse(FinalDataFrame_2015$Season == ' summer', 3, 4)))
		FinalDataFrame_2015 <- arrange(FinalDataFrame_2015,ID, Year, SEASON_NUM)
		FinalDataFrame_2015$TIME<-paste(FinalDataFrame_2015$Year,"-",FinalDataFrame_2015$SEASON_NUM,sep = "")

# =============================
#example of getting a NDVI plot over time for a single participant
# =============================
		
		FinalDataFrame_2015<-FinalDataFrame_2015 %>%drop_na()
		
# =============================
#using the NDVI census blocks for the 2020 census FIPS prepare the data so that it can match with the SOL geodata table
# =============================
		
		NDVI_data_1 <- read.csv("2010-2019 NDVI data location #1", header=TRUE, stringsAsFactors=FALSE)
		NDVI_data_2 <- read.csv("2010-2019 NDVI data location #2", header=TRUE, stringsAsFactors=FALSE)
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
		
# =============================
# join geodata and NDVI data based on FIPS and period
# =============================
		
		location_data_after_2015_final_4 <- location_data_after_2015_final_final %>% left_join(NDVI_data_2020, by=c("FIPS","PERIOD"))

# =============================
#Test for summary using date level accuracy
# =============================
		
		location_data_after_2015_final_4$ID<-as.character(location_data_after_2015_final_4$ID)
		location_data_after_2015_final_4$SEASON2<-as.numeric(location_data_after_2015_final_4$StdTime)
		location_data_after_2015_final_4 = subset(location_data_after_2015_final_4, select = -c(SEASON.y,MONTH,YEAR,StdTime_Ma,Dimensions,Variable,AREA,COUNT,ZONE_CODE) )
		location_data_after_2015_final_4$Cum_NDVI<-location_data_after_2015_final_4$Days_at_location*location_data_after_2015_final_4$MEDIAN
		location_data_after_2015_final_4$Cum_NatWalkInd<-location_data_after_2015_final_4$NatWalkInd*location_data_after_2015_final_4$Days_at_location
		FinalDataFrame_2020<-location_data_after_2015_final_4%>%group_by(ID, PERIOD)%>%summarise_at(vars("Cum_NDVI","Days_at_location","Cum_NatWalkInd"),sum)
		FinalDataFrame_2020$NDVI<-FinalDataFrame_2020$Cum_NDVI/FinalDataFrame_2020$Days_at_location
		FinalDataFrame_2020$Season<-substr(FinalDataFrame_2020$PERIOD,5,nchar(as.character(FinalDataFrame_2020$PERIOD)))
		FinalDataFrame_2020$Year<-substr(FinalDataFrame_2020$PERIOD,1,4)
		
		FinalDataFrame_2020$SEASON_NUM <- ifelse(FinalDataFrame_2020$Season == ' winter', 1,
		                                         ifelse(FinalDataFrame_2020$Season == ' spring', 2,
		                                                ifelse(FinalDataFrame_2020$Season == ' summer', 3, 4)))
		FinalDataFrame_2020 <- arrange(FinalDataFrame_2020,ID, Year, SEASON_NUM)
		FinalDataFrame_2020$TIME<-paste(FinalDataFrame_2020$Year,"-",FinalDataFrame_2020$SEASON_NUM,sep = "")

# =============================
#FINISH AND SAVE THE RESULTING FILE
# =============================
		
		FinalDataFrame_2020<-FinalDataFrame_2020 %>%drop_na()
		
		FINAL_COMBINED<-rbind(FinalDataFrame_2015,FinalDataFrame_2020)
		FINAL_COMBINED$NatWalkInd<-FINAL_COMBINED$Cum_NatWalkInd/FINAL_COMBINED$Days_at_location
		
		write.csv(FINAL_COMBINED, file = "Output location/ Miami_Participant_Greenness_Scores_2009-2019.csv")

# =============================
#test to see if data worked.
# =============================
				
		unique_IDs<-unique(FINAL_COMBINED$ID)
		unique_IDs_list<-as.list(unique_IDs)
		n_unique_IDs<-length(unique_IDs_list)
		n_sample<-round(n_unique_IDs*0.001)
		sample_unique_IDs<-sample(unique_IDs_list,n_sample)
		example<-subset(FINAL_COMBINED, ID %in% sample_unique_IDs)
		List<-example %>%
		  group_by(ID) %>%
		  summarise(example=any(TIME=="2019-1"))
		ggplot(example, aes(x=TIME, y=NDVI, group=ID, color=ID)) + geom_line()+xlab("Year")+ylab("NDVI")+ylim(0,1)
