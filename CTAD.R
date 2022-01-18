# working directory: Masters for sharing
# CTAD data spec: https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/493978/CTAD_Standard_publication_template_CT2a.pdf
# read last update of the Performance csv 

Ctad <- orders[ ,c('SH24.UID','Lab.UID','Postcode','Default.LA',
                     'Gender','Genitals','Gender.Identity','Ethnicity',"Lab","Test.regime",
                     'Created.at.month.year',"Lab.results.at","Lab.results.at.month.year","Lab.receipt.at",
                    'Test.for.Chlamydia.Vaginal','Test.for.Chlamydia.Urine','Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal')]

# Data frame with the months we need for the relevant submission, and exclude FETTLE
# Orders requested on the relevant quarter, without LabResults date OR orders that have a LabResults date within the quarter?!?!?!?

# Select orders for relevant quarter
Ctad1 <- Ctad [((Ctad$Created.at.month.year=="2021-07"| Ctad$Created.at.month.year=="2021-08"| Ctad$Created.at.month.year=="2021-09")
                           & (Ctad$Default.LA != "Fettle") & (Ctad$Default.LA != "Romania") 
                & (Ctad$Default.LA != "Ireland - Cork") & (Ctad$Default.LA != "Ireland - Dublin") & (Ctad$Default.LA != "Ireland - Kerry")
                & (Ctad$Default.LA != "Ireland - Kildare") & (Ctad$Default.LA != "Ireland - Wicklow")
                & (Ctad$Lab.results.at.month.year != "")),]

# Create variable Lab ID, with the Lab Codes sent by TDL and SPS
Ctad1$Lab_ID <- 0
Ctad1$Lab_ID[Ctad1$Lab=='TDL'] <- "69720"
Ctad1$Lab_ID[Ctad1$Lab=='SPS'] <- "697M0"
table (Ctad1$Lab_ID, Ctad1$Lab)
Ctad1$Lab=NULL


# Create variable: Lab.UID capitalised,as per CTAD specs. And remove Lab.UID
Ctad1$Test_ID <- toupper(Ctad1$Lab.UID)
Ctad1$Lab.UID=NULL

# Change name of variables in admin to CTAD submission names, with function 'rename' from the 'dplyr' package (install 'dplyr')
# rename: new variable name = existing variable name ----
Ctad1 <- rename(Ctad1, Patient_ID = SH24.UID)
Ctad1 <- rename(Ctad1, Postcode_Residence = Postcode)
Ctad1 <- rename(Ctad1, Receipt_date = Lab.receipt.at)
Ctad1 <- rename(Ctad1, Date_Result_authorised = Lab.results.at)


# create relevant CTAD variables
Ctad1$NHS_Number <- ""
Ctad1$NHS_Number_Status_Indicator <- 7
Ctad1$DOB <- ""
Ctad1$Postcode_GP <- ""
Ctad1$Registered_GP_Code <- "V81999"
Ctad1$Postcode_Testing_Service <- "SE1 7JB"
Ctad1$Venue_code <- "YGMDR"
Ctad1$Testing_Service_Type="06"
Ctad1$NCSP_Clinic_Code <- "YGMDR"
Ctad1$Specimen_Date <- ""


# Gender
table(Ctad1$Gender) # SH24 users are asked gender up until mid Aug.2020
table(Ctad1$Genitals) # freetesting users get asked genitals
table(Ctad1$Gender,Ctad1$Genitals, useNA = "always")
# use 'droplevels' to remove the rows from the table function output which have 0 counts 
table(droplevels(Ctad1$Gender.Identity))

# Create intermediate gender variable
Ctad1$Gend <- 0
Ctad1$Gend[Ctad1$Gender=="male" | Ctad1$Genitals=="Penis"] <- 1     
Ctad1$Gend[Ctad1$Gender=="female" | Ctad1$Genitals=="Vagina"] <- 2
Ctad1$Gend[Ctad1$Gender=="non_binary"] <- 3
table(Ctad1$Gend, useNA = "always")
Ctad1$Gender =NULL # remove original variable
Ctad1 <- rename(Ctad1, Gender = Gend) # rename intermediate variable


# Ethnicity ----
table(Ctad1$Ethnicity)
Ctad1$Eth <- 0
Ctad1$Eth[Ctad1$Ethnicity=="african"] <- "N"
Ctad1$Eth[Ctad1$Ethnicity=="arab"] <- "S"
Ctad1$Eth[Ctad1$Ethnicity=="bangladeshi"] <- "K"
Ctad1$Eth[Ctad1$Ethnicity=="caribbean"] <- "M"
Ctad1$Eth[Ctad1$Ethnicity=="chinese"] <- "R"
Ctad1$Eth[Ctad1$Ethnicity=="gypsy_or_irish_traveller"] <- "C"
Ctad1$Eth[Ctad1$Ethnicity=="indian"] <- "H"
Ctad1$Eth[Ctad1$Ethnicity=="irish"] <- "B"
Ctad1$Eth[Ctad1$Ethnicity=="latin_american"] <- "S"
Ctad1$Eth[Ctad1$Ethnicity=="not_asked"] <- "Z"
Ctad1$Eth[Ctad1$Ethnicity=="not_known"] <- "Z"
Ctad1$Eth[Ctad1$Ethnicity=="other_asian_asian_british"] <- "L"
Ctad1$Eth[Ctad1$Ethnicity=="other_black_african_caribbean_black_british"] <- "P"
Ctad1$Eth[Ctad1$Ethnicity=="other_ethnic_group"] <- "S"
Ctad1$Eth[Ctad1$Ethnicity=="other_mixed_multiple_ethnic_groups"] <- "G"
Ctad1$Eth[Ctad1$Ethnicity=="other_white"] <- "C"
Ctad1$Eth[Ctad1$Ethnicity=="pakistani"] <- "J"
Ctad1$Eth[Ctad1$Ethnicity=="prefer_not_to_say"] <- "Z"
Ctad1$Eth[Ctad1$Ethnicity=="white_and_asian"] <- "F"
Ctad1$Eth[Ctad1$Ethnicity=="white_and_black_african"] <- "E"
Ctad1$Eth[Ctad1$Ethnicity=="white_and_black_caribbean"] <- "D"
Ctad1$Eth[Ctad1$Ethnicity=="white_english_welsh_scottish_northern_irish_british"] <- "A"
table(Ctad1$Eth)

Ctad1$Ethnicity = NULL
Ctad1 <- rename(Ctad1, Ethnicity = Eth)
#End Ethnicity----

# create new dataframe just in case
Ctad2 <- Ctad1

# create variables using 'grep' for oral, anal, urine and vaginal
Ctad2$SpecimenType_1 <- 0
Ctad2$SpecimenType_1 <- ifelse(grepl("Urine|urine|Male|MSM", Ctad2$Test.regime), 1, "")

Ctad2$SpecimenType_2 <- 0
Ctad2$SpecimenType_2 <- ifelse(grepl("Vaginal|vaginal|Female", Ctad2$Test.regime), 2, "")

Ctad2$SpecimenType_3 <- 0
Ctad2$SpecimenType_3 <- ifelse(grepl("Anal|anal|Anal Swab|MSM", Ctad2$Test.regime), 3, "")

Ctad2$SpecimenType_4 <- 0
Ctad2$SpecimenType_4 <- ifelse(grepl("Oral|oral|Oral Swab|MSM", Ctad2$Test.regime), 4, "")


# create variables coding the results
Ctad2$CTResult_1 <- 0
Ctad2$CTResult_1[Ctad2$Test.for.Chlamydia.Urine=='positive'] <- 1
Ctad2$CTResult_1[Ctad2$Test.for.Chlamydia.Urine=='negative'] <- 2
Ctad2$CTResult_1[Ctad2$Test.for.Chlamydia.Urine=='no_results' | Ctad2$Test.for.Chlamydia.Urine=='not_requested'
               | Ctad2$Test.for.Chlamydia.Urine=='missing'] <- "XX"
table(Ctad2$CTResult_1)

Ctad2$CTResult_2 <- 0
Ctad2$CTResult_2[Ctad2$Test.for.Chlamydia.Vaginal=='positive'] <- 1
Ctad2$CTResult_2[Ctad2$Test.for.Chlamydia.Vaginal=='negative'] <- 2
Ctad2$CTResult_2[Ctad2$Test.for.Chlamydia.Vaginal=='no_results' |
                   Ctad2$Test.for.Chlamydia.Vaginal=='not_requested'| Ctad2$Test.for.Chlamydia.Vaginal=='missing'] <- "XX"
table(Ctad2$CTResult_2)

Ctad2$CTResult_3 <- 0
Ctad2$CTResult_3[Ctad2$Test.for.Chlamydia.Rectal=='positive'] <- 1
Ctad2$CTResult_3[Ctad2$Test.for.Chlamydia.Rectal=='negative'] <- 2
Ctad2$CTResult_3[Ctad2$Test.for.Chlamydia.Rectal=='no_results' | Ctad2$Test.for.Chlamydia.Rectal=='not_requested'
                 | Ctad2$Test.for.Chlamydia.Rectal=='missing'] <- "XX"
table(Ctad2$CTResult_3)

Ctad2$CTResult_4 <- 0
Ctad2$CTResult_4[Ctad2$Test.for.Chlamydia.Oral=='positive'] <- 1
Ctad2$CTResult_4[Ctad2$Test.for.Chlamydia.Oral=='negative'] <- 2
Ctad2$CTResult_4[Ctad2$Test.for.Chlamydia.Oral=='no_results' | Ctad2$Test.for.Chlamydia.Oral=='not_requested'
                 | Ctad2$Test.for.Chlamydia.Oral=='missing'] <- "XX"
table(Ctad2$CTResult_4)

Ctad3 <- Ctad2
# Replace zero with blank (is this for 'patterns' to work?)----
Ctad3[Ctad3 == 0] <- NA
# remove variables not needed
Ctad3$Test.regime = NULL
Ctad3$Created.at.month.year = NULL
Ctad3$Lab.results.at.month.year = NULL
Ctad3$Test.for.Chlamydia.Vaginal = NULL
Ctad3$Test.for.Chlamydia.Urine = NULL
Ctad3$Test.for.Chlamydia.Oral = NULL
Ctad3$Test.for.Chlamydia.Rectal = NULL

# convert data frame to a data.table for the function 'patterns' to work
# install 'data.table' package for function 'melt' and 'patterns'
class(Ctad3)
Ctad3 <- as.data.table(Ctad3)
Ctad3Long = melt(Ctad3, measure = patterns("^SpecimenType", "^CTResult"), value.name = c("Specimen_Type", "CT_Result"))

# Exclude blank 'Specimen_Type'
Ctad3Long1 <- Ctad3Long[(Ctad3Long$Specimen_Type!=""),]

table(Ctad3Long$Specimen_Type)
table(Ctad3Long$CT_Result)
table(Ctad3Long$Specimen_Type,Ctad3Long$CT_Result,useNA = "always")

#order columns to match CTAD specs
#transform back to data.frame for 'order' to work
Ctad3Long1 <- as.data.frame(Ctad3Long1)
Ctad3Long1 <- Ctad3Long1 [c("Lab_ID","Test_ID","Patient_ID","NHS_Number","NHS_Number_Status_Indicator","Gender","DOB","Ethnicity","Postcode_Residence","Postcode_GP"
         ,"Registered_GP_Code","Postcode_Testing_Service","Venue_code","Specimen_Type","Testing_Service_Type","NCSP_Clinic_Code","Specimen_Date","Receipt_date"
         ,"Date_Result_authorised","CT_Result")]

write.table (Ctad3Long1, file="/Users/ElenaArdines1/Documents/Reports/NHS_Reports/CTAD/2021/YGMDR_Q3_2021.csv", row.names=F, sep=",")

