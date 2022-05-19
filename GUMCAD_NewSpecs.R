# working directory: Masters for sharing
# Install 'dplyr' (for 'rename' function)
# Install 'reshape2' (for 'reshape' function)

# GUMCAD data spec: file:///C:/Users/Elena%20Ardines/Documents/Reports/NHS%20Reports/GUMCAD/GUMCAD_Data_Specification_and_Technical_Guidance.pdf

Gumcad <- orders[ ,c('SH24.UID','Customer.ID','Postcode','LSOA.name','Default.LA','LA.of.residence','Date','Site',
                           'Age','Gender','Sexual.preference','Ethnicity',"Reason.for.visit",
                           "Created.at",'Created.at.month.year',"Notified.at","Notified.at.month.year",
                           'Syphilis','HIV','Chlamydia','Gonorrhoea','Hep.B','Hep.C','Test.for.HIV','Test.for.Syphilis.EIA','Test.for.Chlamydia.Urine',
                           'Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal','Test.for.Chlamydia.Vaginal','Test.for.Gonorrhoea.Urine','Test.for.Gonorrhoea.Oral',
                           'Test.for.Gonorrhoea.Rectal','Test.for.Gonorrhoea.Vaginal','Test.for.Hepatitis.B','Test.for.Hepatitis.C'
)]


# Data frame with the months we need for the relevant submission, and exclude FETTLE
# USE 'CREATED' OR 'NOTIFIED' ? ? ? ?
GumcadQ <- Gumcad [((Gumcad$Created.at.month.year=="2019-07"| Gumcad$Created.at.month.year=="2019-08"| Gumcad$Created.at.month.year=="2019-09")
                    & (Gumcad$Site != "Fettle Hub") & (Gumcad$Site != "ARAS Bucuresti") & (Gumcad$Notified.at.month.year != "N/A")),]


table(Gumcad$Notified.at.month.year)


#Create variables we don't have
GumcadQ$ClinicID <- "YGMDR"    #Data item 1
GumcadQ$Clinic_Type <- "03"    #Data item 2
#Data item 3: rename(new variable name = existing variable name) ----
GumcadQ <- rename(GumcadQ, PatientID = SH24.UID)


GumcadQ$Patient_Type <- 'NA'   #Data item 4

#Data item 5
GumcadQ$Gender_Identity[GumcadQ$Gender=="male"] <- 1     
GumcadQ$Gender_Identity[GumcadQ$Gender=="female"] <- 2
table(GumcadQ$Gender)
table(GumcadQ$Gender_Identity)

GumcadQ$Gender_Birth <- 'Y'    #Data item 6

#Data item 7: Age

# Data item 8: Sexual orientation
table(GumcadQ$Sexual.preference,GumcadQ$Gender)
GumcadQ$Sex_Ori [GumcadQ$Gender=="male" & GumcadQ$Sexual.preference=="women"] <- 1
GumcadQ$Sex_Ori [GumcadQ$Gender=="male" & GumcadQ$Sexual.preference=="men"] <- 2
GumcadQ$Sex_Ori [GumcadQ$Gender=="male" & GumcadQ$Sexual.preference=="both"] <- 3
GumcadQ$Sex_Ori [GumcadQ$Gender=="female" & GumcadQ$Sexual.preference=="men"] <- 1
GumcadQ$Sex_Ori [GumcadQ$Gender=="female" & GumcadQ$Sexual.preference=="women"] <- 2
GumcadQ$Sex_Ori [GumcadQ$Gender=="female" & GumcadQ$Sexual.preference=="both"] <- 3

# Create tables with totals to check that 'Sex_Ori' is correctly calculated
tab <- table(GumcadQ$Sex_Ori, GumcadQ$Gender)
tab <- cbind(tab, Total = rowSums(tab))

tab1 <- table(GumcadQ$Sexual.preference, GumcadQ$Gender)
tab1 <- cbind(tab1, Total = rowSums(tab1))
rm(tab1)

# Data item 9: Ethnicity ----
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
#Finish Ethnicity----


GumcadQ$Country_Birth <- "ZZZ"   # Data item 10

# Data item 11 and 12: Vlookup LSOA and LA codes from LSOA name 
GumcadQMerge <- merge(GumcadQ, LSOA[,c('LSOA11CD',"LAD18CD",'LSOA11NM')], by.x = 'LSOA.name', by.y = 'LSOA11NM', all.x = TRUE, all.y = FALSE)

# rename LA and LSOA (new variable name = existing variable name) ----
GumcadQMerge <- rename(GumcadQMerge, LA = LAD18CD)
GumcadQMerge <- rename(GumcadQMerge, LSOA = LSOA11CD)


# Merge with the 2 million postcodes dataset doesn't work: I get more than 3 million  results
# MergeLSOA <- merge(GumcadQ, PostcodesToWork[,c('lsoa11cd',"ladcd",'lsoa11nm')], by.x = 'LSOA.name', by.y = 'lsoa11nm', all.x = TRUE, all.y = FALSE)


GumcadQMerge$Consultation_Referral <- '1082321000000109'    # Data item 13


# Data item 14: Consultation_Date - use "Created at", which is the date when the user ordered the kit. Format shd be yyyy-mm-dd.
GumcadQMerge$Consultation_Date <- GumcadQMerge$Created.at

GumcadQMerge$Consultation_Medium <- "07"    # Data item 15

# Data item 16    ######## REVIEW ! ! ! ! this could change when the 'split order' process is implemented. Talk to Chris on Wed 26th June. 
GumcadQMerge$Consultation_Type <- "01"

# Data item 17    ######## REVIEW ! ! ! ! Ask Gillian/Paula, are we 01 or 02 ?
GumcadQMerge$Consultation_Speciality <- "02"

# Data item 18    ######## REVIEW ! ! ! ! I think doens't apply? Or is it "No" ?
GumcadQMerge$Consultation_PN <- "NA"

# Data item 19
GumcadQMerge$Consultation_Symptomatic <- ifelse(GumcadQMerge$Reason.for.visit=="STI symptoms", "Y", "N")

# Check that item 19 is correctly calculated
table(GumcadQ$Reason.for.visit)
table(GumcadQMerge$Consultation_Symptomatic)


# Data item 20 - Episode activity: TYPES OF TEST - use SHHAPT codes
#Include tests that have been processed: include all results per test type, except when the returned kit didn't have a sample 
#(we don't include "incomplete", "No sample received", "regret, "sample not received" etc). Do not include 'no result' which means the Lab didn't received anything.

table(GumcadQMerge$Test.for.Syphilis.EIA)
table(GumcadQMerge$Test.for.HIV)
table(GumcadQMerge$Chlamydia)
table(GumcadQMerge$Gonorrhoea)
table(GumcadQMerge$Test.for.Hepatitis.B)
table(GumcadQMerge$Test.for.Hepatitis.C)

GumcadQMerge$SyphTest <- 0
GumcadQMerge$SyphTest[GumcadQMerge$Test.for.Syphilis.EIA=="haemolysed" |
                        GumcadQMerge$Test.for.Syphilis.EIA=="insufficient" |
                        GumcadQMerge$Test.for.Syphilis.EIA=="negative" |
                        GumcadQMerge$Test.for.Syphilis.EIA=="reactive"] <- 1

GumcadQMerge$HivTest <- 0
GumcadQMerge$HivTest[GumcadQMerge$Test.for.HIV=="haemolysed" |
                        GumcadQMerge$Test.for.HIV=="insufficient" |
                        GumcadQMerge$Test.for.HIV=="negative" |
                        GumcadQMerge$Test.for.HIV=="reactive"] <- 1

GumcadQMerge$CTTest <- 0
GumcadQMerge$CTTest[GumcadQMerge$Chlamydia=="negative" |
                        GumcadQMerge$Chlamydia=="positive" |
                      GumcadQMerge$Chlamydia=="no result"] <- 1

GumcadQMerge$GCTest <- 0
GumcadQMerge$GCTest[GumcadQMerge$Gonorrhoea=="negative" |
                      GumcadQMerge$Gonorrhoea=="positive" |
                      GumcadQMerge$Gonorrhoea=="no result"] <- 1

GumcadQMerge$HepBTest <- 0
GumcadQMerge$HepBTest[GumcadQMerge$Test.for.Hepatitis.B=="haemolysed" |
                        GumcadQMerge$Test.for.Hepatitis.B=="insufficient" |
                        GumcadQMerge$Test.for.Hepatitis.B=="negative" |
                        GumcadQMerge$Test.for.Hepatitis.B=="positive"] <- 1

GumcadQMerge$HepCTest <- 0
GumcadQMerge$HepCTest[GumcadQMerge$Test.for.Hepatitis.C=="haemolysed" |
                        GumcadQMerge$Test.for.Hepatitis.C=="insufficient" |
                        GumcadQMerge$Test.for.Hepatitis.C=="negative" |
                        GumcadQMerge$Test.for.Hepatitis.C=="positive"] <- 1

table(GumcadQMerge$SyphTest)
table(GumcadQMerge$HivTest)
table(GumcadQMerge$CTTest)
table(GumcadQMerge$GCTest)
table(GumcadQMerge$HepBTest)
table(GumcadQMerge$HepCTest)


# TT: triple testing of men (urine, rectal, oral) and women (vaginal, rectal, oral) 
GumcadQMerge$Uri = ifelse((GumcadQMerge$Test.for.Chlamydia.Urine=="negative"|GumcadQMerge$Test.for.Chlamydia.Urine=="positive"|
                             GumcadQMerge$Test.for.Chlamydia.Urine=="no result"), "1", "0")
GumcadQMerge$Uri1 = ifelse((GumcadQMerge$Test.for.Gonorrhoea.Urine=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Urine=="positive"|
                              GumcadQMerge$Test.for.Gonorrhoea.Urine=="no result"), "1", "0")
GumcadQMerge$UriBoth = ifelse((GumcadQMerge$Uri=="1"|GumcadQMerge$Uri1=="1"), "1", "0")

table(GumcadQMerge$UriBoth)

GumcadQMerge$Vag = ifelse((GumcadQMerge$Test.for.Chlamydia.Vaginal=="negative"|GumcadQMerge$Test.for.Chlamydia.Vaginal=="positive"|
                             GumcadQMerge$Test.for.Chlamydia.Vaginal=="no result"), "1", "0")
GumcadQMerge$Vag1 = ifelse((GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="positive"|
                              GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="no result"), "1", "0")
GumcadQMerge$VagBoth = ifelse((GumcadQMerge$Vag=="1"|GumcadQMerge$Vag1=="1"), "1", "0")

table(GumcadQMerge$VagBoth)

GumcadQMerge$Rect = ifelse((GumcadQMerge$Test.for.Chlamydia.Rectal=="negative"|GumcadQMerge$Test.for.Chlamydia.Rectal=="positive"|
                              GumcadQMerge$Test.for.Chlamydia.Rectal=="no result"), "1", "0")
GumcadQMerge$Rect1 = ifelse((GumcadQMerge$Test.for.Gonorrhoea.Rectal=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Rectal=="positive"|
                               GumcadQMerge$Test.for.Gonorrhoea.Rectal=="no result"), "1", "0")
GumcadQMerge$RectBoth = ifelse((GumcadQMerge$Rect=="1"|GumcadQMerge$Rect1=="1"), "1", "0")

table(GumcadQMerge$RectBoth)

GumcadQMerge$Ora = ifelse((GumcadQMerge$Test.for.Chlamydia.Oral=="negative"|GumcadQMerge$Test.for.Chlamydia.Oral=="positive"|
                             GumcadQMerge$Test.for.Chlamydia.Oral=="no result"), "1", "0")
GumcadQMerge$Ora1 = ifelse((GumcadQMerge$Test.for.Gonorrhoea.Oral=="negative"|GumcadQMerge$Test.for.Gonorrhoea.Oral=="positive"|
                              GumcadQMerge$Test.for.Gonorrhoea.Oral=="no result"), "1", "0")
GumcadQMerge$OraBoth = ifelse((GumcadQMerge$Ora=="1"|GumcadQMerge$Ora1=="1"), "1", "0")

table(GumcadQMerge$OraBoth)


# Create intermediate variable that shows when there's triple site testing in CT and/or GC:
GumcadQMerge$Triple <- ifelse((GumcadQMerge$UriBoth=="1" & 
                                GumcadQMerge$RectBoth=="1" & 
                                GumcadQMerge$OraBoth=="1") | 
                               (GumcadQMerge$VagBoth=="1" & 
                                  GumcadQMerge$RectBoth=="1" & 
                                  GumcadQMerge$OraBoth=="1"), "1", "0")

table(GumcadQMerge$Triple)

### EPISODE ACTIVITY
# Check with Gillian/Paula if we shouldn't get any T1, as we test CT & GC jointly ##### REVIEW ! ! ! ! !
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

GumcadQMerge$Episode_Activity_9 <- ifelse((GumcadQMerge$Triple=="1"), "TT", "")


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


# Data item 21 - Diagnosis_Confirmed: 01 (Code 01 = 'Confirmed (at this service)', Code 03 = Initial reactive, Code NA = Not Applicable)
GumcadQMerge$Diagnosis_Confirmed <- NA
GumcadQMerge$Diagnosis_Confirmed [GumcadQMerge$Episode_Activity_10=="C4"
                                  | GumcadQMerge$Episode_Activity_11=="C4R"
                                  | GumcadQMerge$Episode_Activity_12=="C4O"
                                  | GumcadQMerge$Episode_Activity_13=="B" 
                                  | GumcadQMerge$Episode_Activity_14=="BR"
                                  | GumcadQMerge$Episode_Activity_15=="BO"
                                  ] <- "01"

GumcadQMerge$Diagnosis_Confirmed [GumcadQMerge$Syphilis=="reactive" 
                                  | GumcadQMerge$HIV=="reactive" ] <- "03"
# table including NA values
table(GumcadQMerge$Diagnosis_Confirmed, useNA = "always")


# Data item 22 - Diagnosis_Site: 01-genital, 02-rectal, 03-oral, NA-not applicable
GumcadQMerge$Diagnosis_Site <- NA
GumcadQMerge$Diagnosis_Site [GumcadQMerge$Test.for.Chlamydia.Urine=="positive" | GumcadQMerge$Test.for.Chlamydia.Vaginal=="positive" |
                               GumcadQMerge$Test.for.Gonorrhoea.Urine=="positive" | GumcadQMerge$Test.for.Gonorrhoea.Vaginal=="positive"] <- "01"
GumcadQMerge$Diagnosis_Site [GumcadQMerge$Test.for.Chlamydia.Rectal=="positive" | GumcadQMerge$Test.for.Gonorrhoea.Rectal=="positive"] <- "02"
GumcadQMerge$Diagnosis_Site [GumcadQMerge$Test.for.Chlamydia.Oral=="positive" | GumcadQMerge$Test.for.Gonorrhoea.Oral=="positive"] <- "03"

table(GumcadQMerge$Diagnosis_Site, useNA = "always")


# Data item 23 - Diagnosis_Treated: (01-Yes,treatment provided , NA-not applicable)
# CT TREATMENTS: merge like an Excel VLOOKUP, using merge()----
GumcadQMergeTreatments <- merge(GumcadQMerge, Treatments [, c('SH.24.UID',"Prescription.at","Dispatched.at","No.of.contact.for.partners")]
                                , by.x = 'PatientID', by.y = 'SH.24.UID', all.x = TRUE)
# create new variable for Diagnosis_Treated (01-Yes,treatment provided , NA-not applicable)
GumcadQMergeTreatments$Diagnosis_Treated <- NA
GumcadQMergeTreatments$Diagnosis_Treated <- ifelse(GumcadQMergeTreatments$Dispatched.at!="", "01", "NA")
table(GumcadQMergeTreatments$Diagnosis_Treated, useNA = "always")

# create variable that shows CT treatments as 'C4M'
GumcadQMergeTreatments$Episode_Activity_16 <- ifelse(GumcadQMergeTreatments$Dispatched.at=="", "", "C4M")
table(GumcadQMergeTreatments$Episode_Activity_16, useNA = "always")



# Data Items 24 to 32
GumcadQMergeTreatments$OSP <- NA
GumcadQMergeTreatments$OSP_New <- NA
GumcadQMergeTreatments$OSP_CL <- NA
GumcadQMergeTreatments$MSM <- NA
GumcadQMergeTreatments$MSM_HIV_Pos <- NA
GumcadQMergeTreatments$MSM_CL <- NA
GumcadQMergeTreatments$MSM_CL_Rec <- NA
GumcadQMergeTreatments$WSW <- NA
GumcadQMergeTreatments$WSW_New <- NA

names(Treatments)

# Data item 33 - PN_Date 
# date fields are imported as factor, convert factor to date----
class(GumcadQMergeTreatments$Prescription.at)
GumcadQMergeTreatments$Prescription.at1 <- as.Date(GumcadQMergeTreatments$Prescription.at, format = "%Y-%m-%d")
class(GumcadQMergeTreatments$Prescription.at1)
# PN_Date (date partner notification was discussed/initiated for the current diagnosis): use Prescription.at
GumcadQMergeTreatments$PN_Date <- GumcadQMergeTreatments$Prescription.at1


# Data item 34 - PN_Partners
GumcadQMergeTreatments$PN_Partners <- NA

# Data item 35 - PN_Partners
GumcadQMergeTreatments$PN_Contacts <- GumcadQMergeTreatments$No.of.contact.for.partners

# Data Items 36 to 64
GumcadQMergeTreatments$PN_Contacts_Att_Rep <- NA
GumcadQMergeTreatments$PN_Contacts_Att_Ver <- NA
GumcadQMergeTreatments$PrEP_Eligibility <- NA
GumcadQMergeTreatments$PrEP_Uptake <- NA
GumcadQMergeTreatments$PrEP_Regimen <- NA
GumcadQMergeTreatments$PrEP_Prescription <- NA
GumcadQMergeTreatments$PrEP_Stop_Reason <- NA
GumcadQMergeTreatments$Alcohol_1 <- NA
GumcadQMergeTreatments$Alcohol_2 <- NA
GumcadQMergeTreatments$Drugs_Used <- NA
GumcadQMergeTreatments$Drugs_1 <- NA
GumcadQMergeTreatments$Drugs_2 <- NA
GumcadQMergeTreatments$Drugs_3 <- NA
GumcadQMergeTreatments$Drugs_4 <- NA
GumcadQMergeTreatments$Drugs_5 <- NA
GumcadQMergeTreatments$Drugs_6 <- NA
GumcadQMergeTreatments$Drugs_7 <- NA
GumcadQMergeTreatments$Drugs_8 <- NA
GumcadQMergeTreatments$Drugs_9 <- NA
GumcadQMergeTreatments$Drugs_10 <- NA
GumcadQMergeTreatments$Drugs_11 <- NA
GumcadQMergeTreatments$Drugs_12 <- NA
GumcadQMergeTreatments$Drugs_13 <- NA
GumcadQMergeTreatments$Drugs_14 <- NA
GumcadQMergeTreatments$Drugs_15 <- NA
GumcadQMergeTreatments$Drugs_16 <- NA
GumcadQMergeTreatments$Drugs_Inject <- NA
GumcadQMergeTreatments$Drugs_Share_Eqp <- NA
GumcadQMergeTreatments$Drugs_Sex <- NA


# NEED TO DO?? remove variables not needed in the file ----
GumcadQMergeTreatments$LSOA.name = NULL
GumcadQMergeTreatments$Customer.ID = NULL
GumcadQMergeTreatments$Postcode = NULL
GumcadQMergeTreatments$Default.LA = NULL
GumcadQMergeTreatments$LA.of.residence = NULL
GumcadQMergeTreatments$Date = NULL
GumcadQMergeTreatments$Site = NULL
GumcadQMergeTreatments$Gender = NULL
GumcadQMergeTreatments$Sexual.preference = NULL
GumcadQMergeTreatments$Reason.for.visit = NULL
GumcadQMergeTreatments$Created.at = NULL
GumcadQMergeTreatments$Created.at.month.year = NULL
GumcadQMergeTreatments$Notified.at = NULL
GumcadQMergeTreatments$Notified.at.month.year = NULL
GumcadQMergeTreatments$Syphilis = NULL
GumcadQMergeTreatments$HIV = NULL
GumcadQMergeTreatments$Chlamydia = NULL
GumcadQMergeTreatments$Gonorrhoea = NULL
GumcadQMergeTreatments$Hep.B = NULL
GumcadQMergeTreatments$Hep.C = NULL
GumcadQMergeTreatments$Test.for.HIV = NULL
GumcadQMergeTreatments$Test.for.Syphilis.EIA = NULL
GumcadQMergeTreatments$Test.for.Chlamydia.Urine = NULL
GumcadQMergeTreatments$Test.for.Chlamydia.Oral = NULL
GumcadQMergeTreatments$Test.for.Chlamydia.Rectal = NULL
GumcadQMergeTreatments$Test.for.Chlamydia.Vaginal = NULL
GumcadQMergeTreatments$Test.for.Gonorrhoea.Urine = NULL
GumcadQMergeTreatments$Test.for.Gonorrhoea.Oral = NULL
GumcadQMergeTreatments$Test.for.Gonorrhoea.Rectal = NULL
GumcadQMergeTreatments$Test.for.Gonorrhoea.Vaginal = NULL
GumcadQMergeTreatments$Test.for.Hepatitis.B = NULL
GumcadQMergeTreatments$Test.for.Hepatitis.C = NULL
GumcadQMergeTreatments$SyphTest = NULL
GumcadQMergeTreatments$HivTest = NULL
GumcadQMergeTreatments$CTTest = NULL
GumcadQMergeTreatments$GCTest = NULL
GumcadQMergeTreatments$HepBTest = NULL
GumcadQMergeTreatments$HepCTest = NULL
GumcadQMergeTreatments$Uri = NULL
GumcadQMergeTreatments$Uri1 = NULL
GumcadQMergeTreatments$UriBoth = NULL
GumcadQMergeTreatments$Vag = NULL
GumcadQMergeTreatments$Vag1 = NULL
GumcadQMergeTreatments$VagBoth = NULL
GumcadQMergeTreatments$Rect = NULL
GumcadQMergeTreatments$Rect1 = NULL
GumcadQMergeTreatments$RectBoth = NULL
GumcadQMergeTreatments$Ora = NULL
GumcadQMergeTreatments$Ora1 = NULL
GumcadQMergeTreatments$OraBoth = NULL
GumcadQMergeTreatments$Triple = NULL
# End: NEED TO DO?? remove variables----


# Reshape (install package RESHAPE2) from wide to long, so all of those new variables called Episode_Activity 1, 2, 3, etc 'melt' into one column. 
names(GumcadQMergeTreatments)
GumcadQMergeLong <- GumcadQMergeTreatments[,grep("^episode|clinic|Patient|Gender|Age|Sex|Ethnicity|Country|LA|LSOA|Consultation|diagnosis|osp|msm|wsw|pn_|PrEP_|Alcohol_|Drugs_",
                                          colnames(GumcadQMergeTreatments),
                                          perl = T,value = T,
                                          ignore.case = T)]

names(GumcadQMergeLong)
GumcadQMergeLong1 <- melt (GumcadQMergeLong,id.vars = c("ClinicID","Clinic_Type","PatientID","Patient_Type","Gender_Identity","Gender_Birth","Age","Sex_Ori",
                                                        "Ethnicity","Country_Birth","LA","LSOA",
                                                        "Consultation_Referral","Consultation_Date","Consultation_Medium","Consultation_Type","Consultation_Speciality","Consultation_PN",
                                                        "Consultation_Symptomatic","Diagnosis_Confirmed","Diagnosis_Site","Diagnosis_Treated","OSP","OSP_New","OSP_CL",
                                                        "MSM","MSM_HIV_Pos","MSM_CL","MSM_CL_Rec","WSW","WSW_New","PN_Date","PN_Partners","PN_Contacts","PN_Contacts_Att_Rep","PN_Contacts_Att_Ver",
                                                        "PrEP_Eligibility","PrEP_Uptake","PrEP_Regimen","PrEP_Prescription","PrEP_Stop_Reason",
                                                        "Alcohol_1","Alcohol_2","Drugs_Used","Drugs_1","Drugs_2","Drugs_3","Drugs_4","Drugs_5","Drugs_6","Drugs_7","Drugs_8","Drugs_9","Drugs_10",
                                                        "Drugs_11","Drugs_12","Drugs_13","Drugs_14","Drugs_15","Drugs_16","Drugs_Inject","Drugs_Share_Eqp","Drugs_Sex"
                                                       ),
           
                          measure.vars = c("Episode_Activity_1","Episode_Activity_2","Episode_Activity_3","Episode_Activity_4","Episode_Activity_5","Episode_Activity_6",
                                           "Episode_Activity_7","Episode_Activity_8","Episode_Activity_9","Episode_Activity_10","Episode_Activity_11","Episode_Activity_12",
                                           "Episode_Activity_13","Episode_Activity_14","Episode_Activity_15","Episode_Activity_16"),
                          variable.name = "Episode",
                          value.name = "Episode_Activity")


# Exclude blank 'Episode_Activity'
# First set <NA> to blank ----
GumcadQMergeLong1$Episode_Activity <- as.character(GumcadQMergeLong1$Episode_Activity)
GumcadQMergeLong1$Episode_Activity[is.na(GumcadQMergeLong1$Episode_Activity)] <- ""
# Second remove <blanks>
GumcadQMergeLong1 <- GumcadQMergeLong1[(GumcadQMergeLong1$Episode_Activity!=""),]
table(GumcadQMergeLong1$Episode_Activity, useNA = "always")

# Remove the intermediate variable 'Episode'
GumcadQMergeLong1$Episode = NULL

# export to excel
write.table (GumcadQMergeLong1, file="\\Users\\Elena Ardines\\Documents\\Reports\\NHS Reports\\GUMCAD\\YGMDR_Q3_2019_NewSpecs.csv", row.names=F, sep=",")


# Extract only columns requested ---- KEEP GOING AND RESHAPE
GumcadReport <- GumcadMergeTreatm [c("ClinicID","Clinic_Type","PatientID","Patient_Type","Gender_Identity","Gender_Birth",
                                     "Age","Sex_Ori","Ethnicity","Country_Birth","LA","LSOA","Consultation_Referral","Consultation_Date")]

