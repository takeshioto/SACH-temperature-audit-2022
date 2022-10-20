#Norman Pang 2022

getwd()

all_data <- read.csv("sach_temp_audit_all.csv", na.strings="")

# change date to date format
all_data$Date <- as.Date(all_data$Date)

all_data
head(all_data)
summary(all_data)
str(all_data)

#Task:
#1. write a generic algorithm to determine average temperature change throughout patient journey
#         make the above only valid if at minimum a admission/anaes room temp and a recovery/ward temp
#2. subgroup analysis of ASA grade
#3. subgroup analysis of anaesthesia method


# TASK 1
# picks rows that have (admission temp OR ward depart temp OR anaes room temp) AND (recovery temp or ward return temp)
filled <- subset((all_data) , (!is.na(all_data$Admission.temp) | !is.na(all_data$Ward.depart.temp) | !is.na(all_data$Anaes.room.temp)) & (!is.na(all_data$Ward.arrival.temp) | !is.na(all_data$Recovery.temp)))
# filled_all also includes criteria for an intraop temperature
filled_all <- subset((all_data) , (!is.na(all_data$Admission.temp) | !is.na(all_data$Ward.depart.temp) | !is.na(all_data$Anaes.room.temp)) & (!is.na(all_data$Intraop.temp)) & (!is.na(all_data$Ward.arrival.temp) | !is.na(all_data$Recovery.temp)))

# calculating the mean pre-op temp (defined as mean of those available from (admission temp OR ward depart temp OR anaes room temp)) 
# and the mean post-op temp (defined as mean of those available from (recovery temp or ward arrival temp))
preop_temp_df <- data.frame(filled$Admission.temp , filled$Ward.depart.temp , filled$Anaes.room.temp)
preop_temp_mean <- rowMeans(preop_temp_df , na.rm=TRUE)
postop_temp_df <- data.frame(filled$Recovery.temp , filled$Ward.arrival.temp)
postop_temp_mean <- rowMeans(postop_temp_df , na.rm=TRUE)
# finding the absolute temp difference between the 2 mean temperatures
mean_temp_difference <- round(postop_temp_mean - preop_temp_mean , digits=2)
#DELIVERABLE
mean(mean_temp_difference)


# finding the difference between mean preop and recovery temperature
surg_diff_df <- subset(filled , !is.na(filled$Recovery.temp))
surg_preop_temp_mean_df <- data.frame(surg_diff_df$Admission.temp , surg_diff_df$Ward.depart.temp , surg_diff_df$Anaes.room.temp)
surg_preop_temp_mean <- rowMeans(surg_preop_temp_mean_df , na.rm=TRUE)
# finding the absolute temp difference between the 2 temperatures
surgical_difference <- round(surg_diff_df$Recovery.temp - surg_preop_temp_mean , digits=2)
#DELIVERABLE
mean(surgical_difference)


#finding the difference between mean preop and intraop temperature
intraop_diff_df <- subset(all_data , (!is.na(all_data$Admission.temp) | !is.na(all_data$Ward.depart.temp) | !is.na(all_data$Anaes.room.temp)) & !is.na(all_data$Intraop.temp) ) 
intraop_diff_preop_temp_df <- data.frame(intraop_diff_df$Admission.temp , intraop_diff_df$Ward.depart.temp , intraop_diff_df$Anaes.room.temp)
intraop_diff_preop_temp_mean <- rowMeans(intraop_diff_preop_temp_df , na.rm=TRUE)

intraop_temp_difference <- round(intraop_diff_df$Intraop.temp - intraop_diff_preop_temp_mean , digits=2)
#DELIVERABLE
mean(intraop_temp_difference)


#finding the temp difference with GA
ga_diff <- subset(filled , !is.na(filled$GA))
ga_preop_temp_df <- data.frame(ga_diff$Admission.temp , ga_diff$Ward.depart.temp , ga_diff$Anaes.room.temp)
ga_preop_mean <- rowMeans(ga_preop_temp_df , na.rm=TRUE)
ga_postop_temp_df <- data.frame(ga_diff$Recovery.temp , ga_diff$Ward.arrival.temp)
ga_postop_mean <- rowMeans(ga_postop_temp_df , na.rm=TRUE)

ga_temp_difference <- round(ga_postop_mean - ga_preop_mean , digits=2)
#DELIVERABLE
mean(ga_temp_difference)


#finding the temp difference with LA (or sedation)
la_diff <- subset(filled , !is.na(filled$LA))
la_preop_temp_df <- data.frame(la_diff$Admission.temp , la_diff$Ward.depart.temp , la_diff$Anaes.room.temp)
la_preop_mean <- rowMeans(la_preop_temp_df , na.rm=TRUE)
la_postop_temp_df <- data.frame(la_diff$Recovery.temp, la_diff$Ward.arrival.temp)
la_postop_mean <- rowMeans(la_postop_temp_df , na.rm=TRUE)

la_temp_difference <- round(la_postop_mean - la_preop_mean , digits=2)
#DELIVERABLE
mean(la_temp_difference)


#finding the temp difference with ASA 1
asa1_diff <- subset(filled , filled$ASA==1)
asa1_preop_temp_df <- data.frame(asa1_diff$Admission.temp , asa1_diff$Ward.depart.temp , asa1_diff$Anaes.room.temp)
asa1_preop_mean <- rowMeans(asa1_preop_temp_df , na.rm=TRUE)
asa1_postop_temp_df <- data.frame(asa1_diff$Recovery.temp, asa1_diff$Ward.arrival.temp)
asa1_postop_mean <- rowMeans(asa1_postop_temp_df , na.rm=TRUE)

asa1_temp_difference <- round(asa1_postop_mean - asa1_preop_mean , digits=2)
#DELIVERABLE
mean(asa1_temp_difference)


#finding the temp difference with ASA 2
asa2_diff <- subset(filled , filled$ASA==2)
asa2_preop_temp_df <- data.frame(asa2_diff$Admission.temp , asa2_diff$Ward.depart.temp , asa2_diff$Anaes.room.temp)
asa2_preop_mean <- rowMeans(asa2_preop_temp_df , na.rm=TRUE)
asa2_postop_temp_df <- data.frame(asa2_diff$Recovery.temp, asa2_diff$Ward.arrival.temp)
asa2_postop_mean <- rowMeans(asa2_postop_temp_df , na.rm=TRUE)

asa2_temp_difference <- round(asa2_postop_mean - asa2_preop_mean , digits=2)
#DELIVERABLE
mean(asa2_temp_difference)


#finding the temp difference with ASA 3
asa3_diff <- subset(filled , filled$ASA==3)
asa3_preop_temp_df <- data.frame(asa3_diff$Admission.temp , asa3_diff$Ward.depart.temp , asa3_diff$Anaes.room.temp)
asa3_preop_mean <- rowMeans(asa3_preop_temp_df , na.rm=TRUE)
asa3_postop_temp_df <- data.frame(asa3_diff$Recovery.temp, asa3_diff$Ward.arrival.temp)
asa3_postop_mean <- rowMeans(asa3_postop_temp_df , na.rm=TRUE)

asa3_temp_difference <- round(asa3_postop_mean - asa3_preop_mean , digits=2)
#DELIVERABLE
mean(asa3_temp_difference)


#finding the number of cases that were commenced despite a preop temp <36
# finds cases where most up to date temp is <36
str( subset(all_data , (all_data$Anaes.room.temp<36) | (all_data$Ward.depart.temp<36 & is.na(all_data$Anaes.room.temp)) | (all_data$Admission.temp<36 & is.na(all_data$Ward.depart.temp) & is.na(all_data$Anaes.room.temp)) ) )
#DELIVERABLE
# from str() above, 94 cases started where most recent temp on questionnaire is <36


# finds the number of cases that have a ward arrival temp <36
str(subset(all_data , all_data$Ward.arrival.temp<36))
#DELIVERABLE
# from str() above, 59 cases where ward arrival <36


#finds the number that used a bair hugger
str(subset(all_data , all_data$Bair.hugger=="Y"))
# DELIVERABLE = 209 used bair hugger

#finds the number that didn't use a bair hugger
str(subset(all_data , all_data$Bair.hugger=="N"))
# DELIVERABLE = 64 didn't use a bair hugger

#finds the number left the bair hugger box unfilled
str(subset(all_data , is.na(all_data$Bair.hugger)))
# DELIVERABLE = 507 didn't fill in bair hugger


#finds the number that used a fluid warmer
str(subset(all_data , all_data$IV.fluid.warming=="Y"))
# DELIVERABLE = 13 used fluid warmer

#finds the number that didn't use a fluid warmer
str(subset(all_data , all_data$IV.fluid.warming=="N"))
# DELIVERABLE = 158 didn't use a fluid warmer

#finds the number left the fluid warmer box unfilled
str(subset(all_data , is.na(all_data$IV.fluid.warming)))
# DELIVERABLE = 608 didn't fill in fluid warmer box


