# working directory: Masters for sharing
# Install 'dplyr' (for 'rename' function)
# Install 'reshape2' (for 'melt' function)
install.packages("reshape2")
library(reshape2)
# If reshape2 isnt working any longer: function melt can be found in package data.table too


# GUMCAD data spec: file:///C:/Users/Elena%20Ardines/Documents/Reports/NHS%20Reports/GUMCAD/GUMCAD_Data_Specification_and_Technical_Guidance.pdf

Gumcad <- orders[ ,c('SH24.UID','Customer.ID','Postcode','LSOA.name','Default.LA','LA.of.residence','Site',
          'Age','Gender','Genitals','Gender.Identity','Sexual.preference','Ethnicity',"Reason.for.visit",
          "Created.at",'Created.at.month.year',"Notified.at","Notified.at.month.year","Lab.results.at","Lab.results.at.month.year",
          'Syphilis','HIV','Chlamydia','Gonorrhoea','Hep.B','Hep.C','Test.for.Hiv','Test.for.Syphilis.EIA',"Test.for.Syphilis.RPR",'Test.for.Chlamydia.Urine',
          'Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal','Test.for.Chlamydia.Vaginal','Test.for.Gonorrhoea.Urine','Test.for.Gonorrhoea.Oral',
          'Test.for.Gonorrhoea.Rectal','Test.for.Gonorrhoea.Vaginal','Test.for.Hepatitis.B','Test.for.Hepatitis.C')]


# Data frame with the months we need for the relevant submission, and exclude FETTLE
# Orders requested on the relevant quarter, without Notified date
# GumcadQ <- Gumcad [((Gumcad$Created.at.month.year=="2020-01"| Gumcad$Created.at.month.year=="2020-02"| Gumcad$Created.at.month.year=="2020-03")
#          & (Gumcad$Site != "Fettle Hub") & (Gumcad$Site != "ARAS Bucuresti") & (Gumcad$Notified.at.month.year != "N/A")),]

#GumcadQNotified <- Gumcad [((Gumcad$Notified.at.month.year=="2020-04"| Gumcad$Notified.at.month.year=="2020-05"| Gumcad$Notified.at.month.year=="2020-06")
#          & (Gumcad$Site != "Fettle Hub") & (Gumcad$Site != "ARAS Bucuresti") & (Gumcad$Notified.at.month.year != "N/A")),]

GumcadQ <- Gumcad [((Gumcad$Lab.results.at.month.year=="2021-10"| Gumcad$Lab.results.at.month.year=="2021-11"| Gumcad$Lab.results.at.month.year=="2021-12")
           & (Gumcad$Default.LA != "Fettle") & (Gumcad$Default.LA != "Romania") 
           & (Gumcad$Default.LA != "Ireland - Cork") & (Gumcad$Default.LA != "Ireland - Dublin") & (Gumcad$Default.LA != "Ireland - Kerry")
           & (Gumcad$Default.LA != "Ireland - Kildare") & (Gumcad$Default.LA != "Ireland - Wicklow")
           & (Gumcad$Lab.results.at.month.year != "N/A")),]

# ClinicID: create variables we don't have
GumcadQ$ClinicID <- "YGMDR"    
# PatientID: rename(new variable name = existing variable name) ----
GumcadQ <- rename(GumcadQ, PatientID = SH24.UID)

# Gender: 1 Male(including trans man), 2 Female(including trans woman), 3 Non-binary, 4 Other, Z Not Stated, X Not Known
GumcadQ$Gender[GumcadQ$Gender=="male" | GumcadQ$Genitals=="Penis"] <- 1     
GumcadQ$Gender[GumcadQ$Gender=="female" | GumcadQ$Genitals=="Vagina"] <- 2
GumcadQ$Gender[GumcadQ$Gender=="non_binary"] <- 3

table(GumcadQ$Gender,GumcadQ$Genitals, useNA = "always")
table(GumcadQ$Gender, useNA = "always")

# Age: we have age in admin

# Sexual orientation
table(GumcadQ$Sexual.preference,GumcadQ$Gender)
GumcadQ <- rename(GumcadQ, Gender_Identity = Gender.Identity)
table(GumcadQ$Sexual.preference,GumcadQ$Gender_Identity)



GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="1" & GumcadQ$Sexual.preference=="women"] <- 1
GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="1" & GumcadQ$Sexual.preference=="men"] <- 2
GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="1" & GumcadQ$Sexual.preference=="both"] <- 3
GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="2" & GumcadQ$Sexual.preference=="men"] <- 1
GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="2" & GumcadQ$Sexual.preference=="women"] <- 2
GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="2" & GumcadQ$Sexual.preference=="both"] <- 3
GumcadQ$Sex_Ori [GumcadQ$Gender_Identity=="3"] <- 4

# Create tables with totals to check that 'Sex_Ori' is correctly calculated
table(GumcadQ$Sex_Ori, GumcadQ$Gender, useNA = "always")
tab <- table(GumcadQ$Sex_Ori, GumcadQ$Gender, useNA = "always")
# add a column with totals with 'cbind'
tab <- cbind(tab, Total = rowSums(tab))
# DOESN'T WORK: add a row with totals with 'colSums'
#tab.test <- tab
#tab.test["Total",(2:5)] <- colSums(tab.test[,2:5], na.rm=TRUE)
tab1 <- table(GumcadQ$Sexual.preference, GumcadQ$Gender)
tab1 <- cbind(tab1, Total = rowSums(tab1))


# Ethnicity ----
table(GumcadQ$Ethnicity)
GumcadQ$Eth <- 0
GumcadQ$Eth[GumcadQ$Ethnicity=="african"] <- "N"
GumcadQ$Eth[GumcadQ$Ethnicity=="arab"] <- "S"
GumcadQ$Eth[GumcadQ$Ethnicity=="bangladeshi"] <- "K"
GumcadQ$Eth[GumcadQ$Ethnicity=="caribbean"] <- "M"
GumcadQ$Eth[GumcadQ$Ethnicity=="chinese"] <- "R"
GumcadQ$Eth[GumcadQ$Ethnicity=="gypsy_or_irish_traveller"] <- "C"
GumcadQ$Eth[GumcadQ$Ethnicity=="indian"] <- "H"
GumcadQ$Eth[GumcadQ$Ethnicity=="irish"] <- "B"
GumcadQ$Eth[GumcadQ$Ethnicity=="latin_american"] <- "S"
GumcadQ$Eth[GumcadQ$Ethnicity=="not_asked"] <- "Z"
GumcadQ$Eth[GumcadQ$Ethnicity=="not_known"] <- "99"
GumcadQ$Eth[GumcadQ$Ethnicity=="other_asian_asian_british"] <- "L"
GumcadQ$Eth[GumcadQ$Ethnicity=="other_black_african_caribbean_black_british"] <- "P"
GumcadQ$Eth[GumcadQ$Ethnicity=="other_ethnic_group"] <- "S"
GumcadQ$Eth[GumcadQ$Ethnicity=="other_mixed_multiple_ethnic_groups"] <- "G"
GumcadQ$Eth[GumcadQ$Ethnicity=="other_white"] <- "C"
GumcadQ$Eth[GumcadQ$Ethnicity=="pakistani"] <- "J"
GumcadQ$Eth[GumcadQ$Ethnicity=="prefer_not_to_say"] <- "Z"
GumcadQ$Eth[GumcadQ$Ethnicity=="white_and_asian"] <- "F"
GumcadQ$Eth[GumcadQ$Ethnicity=="white_and_black_african"] <- "E"
GumcadQ$Eth[GumcadQ$Ethnicity=="white_and_black_caribbean"] <- "D"
GumcadQ$Eth[GumcadQ$Ethnicity=="white_english_welsh_scottish_northern_irish_british"] <- "A"
table(GumcadQ$Eth)

GumcadQ$Ethnicity = NULL
GumcadQ <- rename(GumcadQ, Ethnicity = Eth)
#End Ethnicity----

# Country_Birth: unknown
GumcadQ$Country_Birth <- "XXX"   

# Data item 11 and 12: Vlookup LSOA and LA codes from LSOA name 
GumcadQMerge <- merge(GumcadQ, LSOA[,c('LSOA11CD',"LAD19CD",'LSOA11NM')], by.x = 'LSOA.name', by.y = 'LSOA11NM', all.x = TRUE, all.y = FALSE)

# rename LA and LSOA (new variable name = existing variable name) ----
GumcadQMerge <- rename(GumcadQMerge, LA = LAD19CD)
GumcadQMerge <- rename(GumcadQMerge, LSOA = LSOA11CD)

# FirstAttendance
GumcadQMerge$First_Attendance <- "1"

# AttendanceDate
GumcadQMerge$AttendanceDate <- GumcadQMerge$Lab.results.at

# Episode activity: TYPES OF TEST - use SHHAPT codes
# Include tests that have been processed: include all results per test type, except when the returned kit didn't have a sample 
# Don't include 'no result' which means the Lab couldn't process the sample or there was an error in processing, and the user is asked to re-send sample
table(GumcadQMerge$Test.for.Syphilis.EIA)
table(GumcadQMerge$Test.for.Syphilis.RPR)
table(GumcadQMerge$Test.for.Hiv)
table(GumcadQMerge$Chlamydia)
table(GumcadQMerge$Gonorrhoea)
table(GumcadQMerge$Test.for.Hepatitis.B)
table(GumcadQMerge$Test.for.Hepatitis.C)

GumcadQMerge$SyphTest <- 0
GumcadQMerge$SyphTest[GumcadQMerge$Test.for.Syphilis.EIA=="haemolysed" | GumcadQMerge$Test.for.Syphilis.EIA=="insufficient" | 
                        GumcadQMerge$Test.for.Syphilis.EIA=="negative" | GumcadQMerge$Test.for.Syphilis.EIA=="reactive" |
                        GumcadQMerge$Test.for.Syphilis.RPR=="haemolysed" | GumcadQMerge$Test.for.Syphilis.RPR=="insufficient" | 
                        GumcadQMerge$Test.for.Syphilis.RPR=="negative" | GumcadQMerge$Test.for.Syphilis.RPR=="reactive"   ] <- 1

GumcadQMerge$HivTest <- 0
GumcadQMerge$HivTest[GumcadQMerge$Test.for.Hiv=="haemolysed" | GumcadQMerge$Test.for.Hiv=="insufficient" |
                        GumcadQMerge$Test.for.Hiv=="negative" | GumcadQMerge$Test.for.Hiv=="reactive"] <- 1

GumcadQMerge$CTTest <- 0
GumcadQMerge$CTTest[GumcadQMerge$Chlamydia=="negative" | GumcadQMerge$Chlamydia=="positive"] <- 1

GumcadQMerge$GCTest <- 0
GumcadQMerge$GCTest[GumcadQMerge$Gonorrhoea=="negative" | GumcadQMerge$Gonorrhoea=="positive"] <- 1

GumcadQMerge$HepBTest <- 0
GumcadQMerge$HepBTest[GumcadQMerge$Test.for.Hepatitis.B=="haemolysed" | GumcadQMerge$Test.for.Hepatitis.B=="insufficient" |
                        GumcadQMerge$Test.for.Hepatitis.B=="negative" | GumcadQMerge$Test.for.Hepatitis.B=="positive"] <- 1

GumcadQMerge$HepCTest <- 0
GumcadQMerge$HepCTest[GumcadQMerge$Test.for.Hepatitis.C=="haemolysed" | GumcadQMerge$Test.for.Hepatitis.C=="insufficient" |
                        GumcadQMerge$Test.for.Hepatitis.C=="negative" | GumcadQMerge$Test.for.Hepatitis.C=="positive"] <- 1

table(GumcadQMerge$SyphTest)
table(GumcadQMerge$HivTest)
table(GumcadQMerge$CTTest)
table(GumcadQMerge$GCTest)
table(GumcadQMerge$HepBTest)
table(GumcadQMerge$HepCTest)

# TT: triple testing of men (urine, rectal, oral) and women (vaginal, rectal, oral) ----
GumcadQMerge$Urin = ifelse((GumcadQMerge$Test.for.Chlamydia.Urine=="negative"|GumcadQMerge$Test.for.Chlamydia.Urine=="positive" |
                              GumcadQMerge$Test.for.Gonorrhoea.Urine=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Urine=="positive"), "1", "0")

GumcadQMerge$Vagi = ifelse((GumcadQMerge$Test.for.Chlamydia.Vaginal=="negative"|GumcadQMerge$Test.for.Chlamydia.Vaginal=="positive" |
                              GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="positive"), "1", "0")

GumcadQMerge$Rec = ifelse((GumcadQMerge$Test.for.Chlamydia.Rectal=="negative"|GumcadQMerge$Test.for.Chlamydia.Rectal=="positive" |
                              GumcadQMerge$Test.for.Gonorrhoea.Rectal=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Rectal=="positive"), "1", "0")

GumcadQMerge$Oral = ifelse((GumcadQMerge$Test.for.Chlamydia.Oral=="negative"|GumcadQMerge$Test.for.Chlamydia.Oral=="positive" |
                              GumcadQMerge$Test.for.Gonorrhoea.Oral=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Oral=="positive"), "1", "0")

table(GumcadQMerge$Urin)
table(GumcadQMerge$Vagi)
table(GumcadQMerge$Rec)
table(GumcadQMerge$Oral)

#End TT ----

### EPISODE ACTIVITY
# Check with Gillian/Paula if we shouldn't get any T1, as we test CT & GC jointly 
# Create each one of the episodes (T1, T2, T3, etc) separately: one varible each
GumcadQMerge$Episode_Activity_1 <- ifelse(((GumcadQMerge$SyphTest=="0" & GumcadQMerge$HivTest=="0" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="0") |
                                             (GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="0")), "T1", "")

GumcadQMerge$Episode_Activity_2 <- ifelse(((GumcadQMerge$SyphTest=="0" & GumcadQMerge$HivTest=="0" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="1") |
                                             (GumcadQMerge$SyphTest=="0" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="1")), "T2", "")

GumcadQMerge$Episode_Activity_3 <- ifelse(((GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="0" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="1")), "T3", "")

GumcadQMerge$Episode_Activity_4 <- ifelse(((GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="1") |
                                   (GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="0" & GumcadQMerge$GCTest=="1")), "T4", "")


GumcadQMerge$Episode_Activity_5 <- ifelse(((GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="0" & GumcadQMerge$GCTest=="0") | 
                                   (GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="0") |
                                   (GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="0" & GumcadQMerge$CTTest=="0" & GumcadQMerge$GCTest=="0")), "T7", "")


GumcadQMerge$Episode_Activity_6 <- ifelse(((GumcadQMerge$SyphTest=="0" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="0" & GumcadQMerge$GCTest=="0") |
                                   (GumcadQMerge$SyphTest=="0" & GumcadQMerge$HivTest=="1" & GumcadQMerge$CTTest=="1" & GumcadQMerge$GCTest=="1")), "P1A", "")

GumcadQMerge$Episode_Activity_7 <- ifelse((GumcadQMerge$SyphTest=="1" & GumcadQMerge$HivTest=="0" & GumcadQMerge$CTTest=="0" & GumcadQMerge$GCTest=="0"), "P1B", "")

GumcadQMerge$Episode_Activity_8 <- ifelse(((GumcadQMerge$HepBTest=="1") | (GumcadQMerge$HepCTest=="1") | (GumcadQMerge$HepBTest=="1" & GumcadQMerge$HepCTest=="1")), "T6", "")

GumcadQMerge$Episode_Activity_9 <- ifelse((GumcadQMerge$Urin=="1" & GumcadQMerge$Rec=="1" & GumcadQMerge$Oral=="1") | 
                                (GumcadQMerge$Vagi=="1" & GumcadQMerge$Rec=="1" & GumcadQMerge$Oral=="1"), "TT", "0")

table(GumcadQMerge$Episode_Activity_9)


### DIAGNOSTIC CODES: create variables that show positivity of CT and GC per body site.
# Positive CT: genital (urine, vaginal), rectal, oral
GumcadQMerge$Episode_Activity_10 <- ifelse((GumcadQMerge$Test.for.Chlamydia.Urine=="positive" | GumcadQMerge$Test.for.Chlamydia.Vaginal=="positive"), "C4", "")
GumcadQMerge$Episode_Activity_11 <- ifelse(GumcadQMerge$Test.for.Chlamydia.Rectal=="positive", "C4R", "")
GumcadQMerge$Episode_Activity_12 <- ifelse(GumcadQMerge$Test.for.Chlamydia.Oral=="positive", "C4O", "")

# Positive GC: genital (urine, vaginal), rectal, oral
GumcadQMerge$Episode_Activity_13 <- ifelse((GumcadQMerge$Test.for.Gonorrhoea.Urine=="positive" | GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="positive"), "B", "")
GumcadQMerge$Episode_Activity_14 <- ifelse(GumcadQMerge$Test.for.Gonorrhoea.Rectal=="positive", "BR", "")
GumcadQMerge$Episode_Activity_15 <- ifelse(GumcadQMerge$Test.for.Gonorrhoea.Oral=="positive", "BO", "")

table(GumcadQMerge$Episode_Activity_1)
table(GumcadQMerge$Episode_Activity_2)
table(GumcadQMerge$Episode_Activity_3)
table(GumcadQMerge$Episode_Activity_4)
table(GumcadQMerge$Episode_Activity_5)
table(GumcadQMerge$Episode_Activity_6)
table(GumcadQMerge$Episode_Activity_7)
table(GumcadQMerge$Episode_Activity_8)
table(GumcadQMerge$Episode_Activity_9)
table(GumcadQMerge$Episode_Activity_10)
table(GumcadQMerge$Episode_Activity_11)
table(GumcadQMerge$Episode_Activity_12)
table(GumcadQMerge$Episode_Activity_13)
table(GumcadQMerge$Episode_Activity_14)
table(GumcadQMerge$Episode_Activity_15)

# CT TREATMENTS from Backing_Data----
# Remove blank 'Dispatched.at', only keep treatments that have been delivered
TreatmDispatched <- Treatments

TreatmDispatched <- TreatmDispatched [(TreatmDispatched$dispatched_at!=""),]
#What's this FOR?: TreatmDispatched$Treated <- TreatmDispatched$dispatched_at

# merge Gumcad dataframe we've been working on with Treatments delivered
GumcadMergeTreatm <- merge(GumcadQMerge, TreatmDispatched[, c('sh24_uid','dispatched_at')], by.x = 'PatientID', by.y = 'sh24_uid', all.x = TRUE)

#convert factor to character
class(GumcadMergeTreatm$dispatched_at)
GumcadMergeTreatm$dispatched_at <- as.character(GumcadMergeTreatm$dispatched_at)
# replace missing values with something (i.e. 0) using 'is.na' ----
GumcadMergeTreatm$dispatched_at[is.na(GumcadMergeTreatm$dispatched_at)] <- "0"
table(GumcadMergeTreatm$dispatched_at=="0")

# create variable that shows CT treatments as 'C4M'
GumcadMergeTreatm$Episode_Activity_16 <- ifelse(GumcadMergeTreatm$dispatched_at=="0", "", "C4M")
table(GumcadMergeTreatm$Episode_Activity_16)
#End CT Treatments----

# 'Melt' (install package RESHAPE2) from wide to long, so all of those new variables called Episode_Activity 1, 2, 3, etc 'melt' into one column. 
GumcadQMergeLong <- GumcadMergeTreatm[,grep("^episode|clinic|Patient|Gender_Identity|Age|Sex|Ethnicity|Country|LA|LSOA|attendance|Site|residence|Created.at.month.year",
                                            colnames(GumcadMergeTreatm),
                                            perl = T,value = T,
                                            ignore.case = T)]

GumcadQMergeLong1 <- melt (GumcadQMergeLong,id.vars = c("ClinicID","PatientID","Gender_Identity","Age","Sex_Ori",
                                                        "Ethnicity","Country_Birth","LA","LSOA","First_Attendance","AttendanceDate"),
                          measure.vars = c("Episode_Activity_1","Episode_Activity_2","Episode_Activity_3","Episode_Activity_4","Episode_Activity_5","Episode_Activity_6",
                                           "Episode_Activity_7","Episode_Activity_8","Episode_Activity_9","Episode_Activity_10","Episode_Activity_11","Episode_Activity_12",
                                           "Episode_Activity_13","Episode_Activity_14","Episode_Activity_15","Episode_Activity_16"),
                          variable.name = "Episode",
                          value.name = "Episode_Activity")


# Exclude blank 'Episode_Activity'
GumcadQMergeLong1 <- GumcadQMergeLong1[(GumcadQMergeLong1$Episode_Activity!=""),]
GumcadQMergeLong1 <- GumcadQMergeLong1[(GumcadQMergeLong1$Episode_Activity!="0"),]
table(GumcadQMergeLong1$Episode_Activity, useNA = "always")


# order columns
GumcadQMergeLong1 <- GumcadQMergeLong1 [c("ClinicID","PatientID","Episode_Activity","Gender_Identity","Age","Sex_Ori","Ethnicity","Country_Birth","LA","LSOA","First_Attendance","AttendanceDate")]
# export outcome
write.table (GumcadQMergeLong1, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\NHS_Reports\\GUMCAD\\YGMDR_Q4_2021.csv", row.names=F, sep=",")


### DERBYSHIRE GUMCAD REPORT----
GumcadDerbyshire <- melt (GumcadQMergeLong,id.vars = c("PatientID","Gender_Identity","Age","Sex_Ori","Ethnicity",
                                                        "LA","LSOA","AttendanceDate","LA.of.residence","Site","Created.at.month.year"),
                           measure.vars = c("Episode_Activity_1","Episode_Activity_2","Episode_Activity_3","Episode_Activity_4","Episode_Activity_5","Episode_Activity_6",
                                            "Episode_Activity_7","Episode_Activity_8","Episode_Activity_9","Episode_Activity_10","Episode_Activity_11","Episode_Activity_12",
                                            "Episode_Activity_13","Episode_Activity_14","Episode_Activity_15","Episode_Activity_16"),
                           variable.name = "Episode",
                           value.name = "Episode_Activity")
# remove blanks and select
GumcadDerbyshire1 <- GumcadDerbyshire[((GumcadDerbyshire$Episode_Activity!="") 
                                       & (GumcadDerbyshire$Site=="Wheatbridge Clinic")
                                       & (GumcadDerbyshire$Created.at.month.year=="2019-06")),]
# remove not needed variables 
GumcadDerbyshire1$Episode=NULL
write.table (GumcadDerbyshire1, file="\\Users\\Elena Ardines\\Documents\\Reports\\1. Monthly Reports\\Invoicing\\2019\\2019 07\\Backing data\\2019 06 Derbyshire GUMCAD.csv", row.names=F, sep=",")
### End DERBYSHIRE GUMCAD REPORT----
