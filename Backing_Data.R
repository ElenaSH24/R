
#Install the relevant libraries - do this one time
install.packages("data.table")
install.packages("dplyr")
install.packages("reshape2")
library(reshape2)
install.packages('tidyverse')

# use ~ as a shortcut to the Home Directory, to avoid portability issues (i.e. changing Operating Systems, versions of R, or programming language)
setwd("~/Reports/1.Monthly_Reports/Performance_Reports/2022/2022_09")


# to know the destination file used by the language
path.expand("~")
# PATH JOIN: gold standard on setting working directory, to avoid syntax issues and address portability concerns (i.e. moving Operating Systems, different languages, etc)
file.path("~", "Reports","1.Monthly_Reports","Invoicing","2022","2022_04","Xero_Quantities_2022.April_v9.csv")

orders = read.csv("20221003_sti_order_report.csv")

feedback = read.csv("20201020_Feedback_tokens.csv")

Treatments = read.csv("20221003_CT_Treatments.csv")
COC = read.csv("20221003_COC.csv")
POP = read.csv("20221003_POP.csv")
ECFuture = read.csv("20221003_ECFuture.csv")
ECNow = read.csv("20221003_ECNow.csv")
PhotoConsult = read.csv("20221003_PD_consultations.csv")
PhotoTreatm = read.csv("20221003_PD_treatments.csv")
Injectable = read.csv("20221003_Injectable.csv")
Patch = read.csv("20221003_Patch.csv")  
Ring = read.csv("20221003_Ring.csv")


# Invoicing data set from sti_invoice_detail.sql query
invSTI = read.csv("20221003_invoicing.csv")
# bolt-ons data for invoicing
bolts = read.csv("20221003_CombinedBoltOns.csv")


# reactivity levels from the 'miscellaneous' query in DataGrip, for freetesting
reactivity = read.csv("20210506_Reactivity_levels_PHE.csv")

FranECNow = read.csv("20220802_ECNow.csv")
FranECFut = read.csv("20220802_ECFuture.csv")

LSOA = read.csv("Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2019)_Lookup_in_England_and_Wales.csv") # England and Wales


LSOA.UpperTier = read.csv("Lower_Layer_Super_Output_Area_2011_to_Upper_Tier_Local_Authorities_2017_Lookup_in_England_and_Wales.csv")
Stripe = read.csv("Stripe_unified_payments.csv")
FreetestFeedb <- read.csv("freetesting user feedback 2020.01.21..csv")

# ANA HERB (PHE) PHE/freetesting backing data----
freetesting <- orders[ ,c('SH24.UID','Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                          'Age','Gender',"Gender.at.birth",'Ethnicity','Sexual.preference','Unprotected.sex.in.last.5.days',
                          'Sexuality',"Sites.tested",'Test.regime','Clinic.visited','Clinic.visited.12','Attended.clinic',
                          "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Lab.receipt.at","Notified.at","Notified.at.month.year",
                          "Lab.results.at","Lab.results.at.month.year","PrEP.impact.trial.number","Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis",'Syphilis','HIV',"Chlamydia","Gonorrhoea","Hep.B","Hep.C",
                          "Test.for.Chlamydia.Urine","Test.for.Chlamydia.Oral","Test.for.Chlamydia.Rectal","Test.for.Chlamydia.Vaginal",
                          "Test.for.Gonorrhoea.Urine","Test.for.Gonorrhoea.Oral","Test.for.Gonorrhoea.Rectal","Test.for.Gonorrhoea.Vaginal")]

# Subset using 'grep' and 'or' | (if 'Default LA' includes the string 'freetesting', or 'phe' or prep')
freetesting1 <- freetesting[grep('Freetesting|PHE|PrEP', freetesting$Default.LA),]
# users who belong to SH:24 areas (no freetesting or PrEP) and complete their Impact.trial.number
freetesting11 <- freetesting[(freetesting$PrEP.impact.trial.number!=""),]
# Stack two data sets one (grep) on top of the other (completed impact trial numbers) with 'rbind'
freetesting2 <- rbind(freetesting1, freetesting11)
# remove DUPLICATES, keep unique SH24 numbers
freetesting.Unique = freetesting2[!duplicated(freetesting2$SH24.UID),]
# include LSOA Code with 'merge' getting all the observations from the data set on the left (all.x = TRUE)
#### freetesting3 <- merge(freetesting.Unique, LSOA[,c('LSOA11NM',"LSOA11CD")], by.x = "LSOA.name", by.y = "LSOA11NM", all.x = TRUE)
freetesting4 <- merge(freetesting.Unique, LSOA.UpperTier[,c('LSOA11NM',"UTLA18NM")], by.x = "LSOA.name", by.y = "LSOA11NM", all.x = TRUE)
write.table (freetesting4, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022.09 freetesting.PHE.ImpactPrEP.csv", row.names=F, sep=",")
# End PHE/freetesting backing data----


OrdersToWork <- orders[ ,c('SH24.UID','Customer.ID',"Feedback.token",'Reason.for.visit','Postcode','LSOA.name','Default.LA','LA.of.residence',
                           'Site',"Distribution.center", "Distribution.method",
                           'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",'Sexual.preference',
                           'Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.5.days',
                           'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                           'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                           'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                           "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Lab.receipt.at","Notified.at","Notified.at.month.year",
                           "Lab.results.at","Lab.results.at.month.year","Previously.diagnosed.with.HIV",
                           "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                           "Previously.treated.for.Syphilis",
                           'Syphilis','HIV','Chlamydia','Gonorrhoea','Hep.B','Hep.C','Test.for.Hiv','Test.for.Syphilis.EIA','Test.for.Chlamydia.Urine',
                           'Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal','Test.for.Chlamydia.Vaginal','Test.for.Gonorrhoea.Urine','Test.for.Gonorrhoea.Oral',
                           'Test.for.Gonorrhoea.Rectal','Test.for.Gonorrhoea.Vaginal','Test.for.Hepatitis.B','Test.for.Hepatitis.C',"Test.for.Syphilis.RPR")]
# add this variable to keep historical structure -though this question isn't asked any longer
OrdersToWork$Unprotected.sex.in.last.3.days <- ""
 

#Create Area variable and set to 0
OrdersToWork$Area <- 0
#run "recode Area" function and save output in Area; important to write your variable names in colons (but not the data frame name)
OrdersToWork$Area <- recodeArea(DF=OrdersToWork,varname="Area",varname2="Site",varname3 = "LA.of.residence", varname4="Referred.from",varname5="Default.LA")

# reporting month
v1 <- '2022-09'

# check that all orders are allocated to an Area
Zero <- OrdersToWork[(OrdersToWork$Area==0),]
rm(Zero)

table(OrdersToWork$Dispatched.at.month.year== v1)
table(OrdersToWork$Lab.results.at.month.year== v1)


# Backing data STI ######################################### ----
# ESSEX and Thurrock (they need more variables)----
OrdersToWork$AgeSplit <- 0
OrdersToWork$AgeSplit = ifelse(OrdersToWork$Age>24,"25+","Under 25")
table (OrdersToWork$AgeSplit)

BackingMaximum <- OrdersToWork[,c('SH24.UID','Customer.ID','Reason.for.visit','Postcode','LSOA.name','Default.LA','LA.of.residence','Site',
                                  'Age','AgeSplit','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                  'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.3.days','Unprotected.sex.in.last.5.days',
                                  'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                  'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                  'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                                  "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Lab.receipt.at","Notified.at","Notified.at.month.year",
                                  "Lab.results.at","Lab.results.at.month.year","Previously.diagnosed.with.HIV",
                                  "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                  "Previously.treated.for.Syphilis",
                                  'Syphilis','HIV','Chlamydia','Gonorrhoea','Hep.B','Hep.C','Test.for.Hiv','Test.for.Syphilis.EIA','Test.for.Chlamydia.Urine',
                                  'Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal','Test.for.Chlamydia.Vaginal','Test.for.Gonorrhoea.Urine','Test.for.Gonorrhoea.Oral',
                                  'Test.for.Gonorrhoea.Rectal','Test.for.Gonorrhoea.Vaginal','Test.for.Hepatitis.B','Test.for.Hepatitis.C',"Test.for.Syphilis.RPR")]
Data_Essex <- BackingMaximum [BackingMaximum$Site == "Essex Hub",]
Data_Thurrock <- BackingMaximum [BackingMaximum$Default.LA == "Thurrock",]
# End ESSEX and Thurrock----

# Areas needing the SH24.UID: LLR and Derby/shire----
BackingMin_WithSH24 <- OrdersToWork[,c('SH24.UID','Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                                       "Distribution.center", "Distribution.method",
                                       'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                       'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.3.days','Unprotected.sex.in.last.5.days',
                                       'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                       'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                       'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                                       "Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at","Previously.diagnosed.with.HIV",
                                       "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                       "Previously.treated.for.Syphilis",
                                       'Syphilis','HIV','Chlamydia','Gonorrhoea','Area',"Test.for.Syphilis.RPR")]

# Include LSOA Code in DerbyshireDerby
Data_DerbyshireDerby <- BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Derby" | BackingMin_WithSH24$Default.LA=="Derbyshire"),]
Data_DerbyshireDerby <- merge(Data_DerbyshireDerby, LSOA[,c('LSOA11NM',"LSOA11CD")], by.x = "LSOA.name", by.y = "LSOA11NM")
Data_DerbyshireDerby$Distribution.center = NULL
Data_DerbyshireDerby$Distribution.method = NULL


# LLR file needs: 'Country of Birth' as 'unknown', SH24 number (instead of 'Customer.ID'), and fields with testing per site 
BackingLLR.MPFT <- OrdersToWork[,c('SH24.UID','Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                                   'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                   'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.3.days','Unprotected.sex.in.last.5.days',
                                   'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                   'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                   'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                                   "Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at","Previously.diagnosed.with.HIV",
                                   "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                   "Previously.treated.for.Syphilis",
                                   'Syphilis','HIV','Chlamydia','Gonorrhoea','Test.for.Chlamydia.Urine',
                                   'Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal','Test.for.Chlamydia.Vaginal','Test.for.Gonorrhoea.Urine','Test.for.Gonorrhoea.Oral',
                                   'Test.for.Gonorrhoea.Rectal','Test.for.Gonorrhoea.Vaginal',"Test.for.Hepatitis.B","Test.for.Hepatitis.C","Test.for.Syphilis.RPR")]

Data_LLR.MPFT <- BackingLLR.MPFT [(BackingLLR.MPFT$Default.LA=="Leicester" | BackingLLR.MPFT$Default.LA=="Leicestershire" | BackingLLR.MPFT$Default.LA=="Rutland" |
                                     BackingLLR.MPFT$Default.LA=="Shropshire" | BackingLLR.MPFT$Default.LA=="Telford and Wrekin" | BackingLLR.MPFT$Default.LA=="Staffordshire" |
                                     BackingLLR.MPFT$Default.LA=="North Staffordshire" | BackingLLR.MPFT$Default.LA=="Stoke-on-Trent" ),]
Data_LLR.MPFT$Country.of.Birth <- 'unknown'
# Include LSOA Code in LLR
Data_LLR.MPFT <- merge(Data_LLR.MPFT, LSOA[,c('LSOA11NM',"LSOA11CD")], by.x = "LSOA.name", by.y = "LSOA11NM")

Data_Dorset <- BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Dorset"),]

# Bromley file needs 'Distribution Center' and 'Distribution Method' for offline kits
BackingBromley <- OrdersToWork[,c('SH24.UID','Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence'
                                  ,"Distribution.center",'Site',
                                  'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                  'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.3.days','Unprotected.sex.in.last.5.days',
                                  'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                  'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                  'Sexuality','Click.and.collect',"Distribution.method",'Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                                  "Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at","Previously.diagnosed.with.HIV",
                                  "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                  "Previously.treated.for.Syphilis",
                                  'Syphilis','HIV','Chlamydia','Gonorrhoea','Test.for.Chlamydia.Urine',
                                  'Test.for.Chlamydia.Oral','Test.for.Chlamydia.Rectal','Test.for.Chlamydia.Vaginal','Test.for.Gonorrhoea.Urine','Test.for.Gonorrhoea.Oral',
                                  'Test.for.Gonorrhoea.Rectal','Test.for.Gonorrhoea.Vaginal',"Test.for.Syphilis.RPR")]
Data_Bromley <- BackingBromley [(BackingBromley$Default.LA=="Bromley"),]
# END areas with SH24.UID----

# REST OF AREAS----
BackingMin_noSH24 <- OrdersToWork[,c('Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                                     'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                     'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.5.days',
                                     'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                     'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                     'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                                     "Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at","Previously.diagnosed.with.HIV",
                                     "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                     "Previously.treated.for.Syphilis",
                                     'Syphilis','HIV','Chlamydia','Gonorrhoea',"Test.for.Syphilis.RPR")]


Data_Berkshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="East Berkshire"),]
Data_Buckinghamshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Buckinghamshire"),]
#Buckinghamshire data only from 1st Jan 2021
class(Data_Buckinghamshire$Created.at)
Data_Buckinghamshire$Created.at <- as.Date(Data_Buckinghamshire$Created.at, format = "%Y-%m-%d")
Data_Buckinghamshire <- Data_Buckinghamshire[(Data_Buckinghamshire$Created.at > "2020-12-31"),]


# 2022.06.22 Include Hep.B and Hep.C in Cornwall (Blackburn request - Dimitrious email)
Data_Cornwall <- OrdersToWork [(OrdersToWork$Default.LA=="Cornwall and Isles of Scilly PCT"| OrdersToWork$Default.LA=="Southend-on-Sea"
                                     |OrdersToWork$Default.LA=="Blackburn with Darwen"),
                                    c('Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                                      'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                      'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.5.days',
                                      'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                      'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                      'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                                      "Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at","Previously.diagnosed.with.HIV",
                                      "Prep.user","Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                      "Previously.treated.for.Syphilis",
                                      'Syphilis','HIV','Chlamydia','Gonorrhoea','Hep.B','Hep.C',"Test.for.Syphilis.RPR")]


Data_Hillingdon <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Hillingdon"),]

Data_NIreland <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Northern Ireland Belfast PCT" | BackingMin_noSH24$Default.LA=="Northern Ireland Northern PCT" |
                                       BackingMin_noSH24$Default.LA=="Northern Ireland South Eastern PCT" | BackingMin_noSH24$Default.LA=="Northern Ireland Southern PCT" |
                                       BackingMin_noSH24$Default.LA=="Northern Ireland Western PCT"),]

Data_Worces_Hereford <- BackingMin_noSH24 [(BackingMin_noSH24$Site=="Worcestershire Hub" | BackingMin_noSH24$Site=="iSH Hereford"),]
# End REST OF AREAS----


# Ireland backing data Justin----
Data_Ireland <- orders [(orders$Default.LA=="Ireland - Carlow" | orders$Default.LA=="Ireland - Cavan" |
                           orders$Default.LA=="Ireland - Clare" | orders$Default.LA=="Ireland - Cork" |
                           orders$Default.LA=="Ireland - Donegal" | orders$Default.LA=="Ireland - Dublin" |
                           orders$Default.LA=="Ireland - Galway" | orders$Default.LA=="Ireland - Kerry" | 
                           orders$Default.LA=="Ireland - Kildare" | orders$Default.LA=="Ireland - Kilkenny" |
                           orders$Default.LA=="Ireland - Laois" | orders$Default.LA=="Ireland - Leitrim" |
                           orders$Default.LA=="Ireland - Limerick" | orders$Default.LA=="Ireland - Longford" |
                           orders$Default.LA=="Ireland - Louth" | orders$Default.LA=="Ireland - Mayo" |
                           orders$Default.LA=="Ireland - Meath" | orders$Default.LA=="Ireland - Monaghan" |
                           orders$Default.LA=="Ireland - Offaly" | orders$Default.LA=="Ireland - Roscommon" |
                           orders$Default.LA=="Ireland - Sligo" | orders$Default.LA=="Ireland - Tipperary" |
                           orders$Default.LA=="Ireland - Westmeath" | orders$Default.LA=="Ireland - Wicklow"),]


#remove unwanted variables
#Data_Ireland$SH24.UID = NULL
Data_Ireland$id = NULL
Data_Ireland$Lab.UID = NULL
Data_Ireland$Feedback.token = NULL
Data_Ireland$Customer.ID = NULL
Data_Ireland$Unprotected.sex.in.last.3.days = NULL
Data_Ireland$New.or.follow.up = NULL
Data_Ireland$Distribution.center = NULL
Data_Ireland$Click.and.collect = NULL
Data_Ireland$Referred.from = NULL
Data_Ireland$Referred.to = NULL
Data_Ireland$Referred.via = NULL
Data_Ireland$Charge.token = NULL
Data_Ireland$S.code = NULL
Data_Ireland$PrEP.impact.trial.number = NULL
table(Data_Ireland$Dispatched.at.month.year, Data_Ireland$Default.LA)

write.table (Data_Ireland, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022.10.06_Negar_Ireland_STI.csv", row.names=F, sep=",")
# END Ireland Justin----

#Remove 'Area' from the files (no need to include it)
Data_DerbyshireDerby$Area=NULL

#export data to csv (export the data of the ordered file). Use double \\ when setting destination file----
write.table (Data_Berkshire, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Berkshire STI.csv", row.names=F, sep=",")
write.table (Data_Bromley, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Bromley STI.csv"
             , row.names=F, sep=",")

write.table (Data_Buckinghamshire, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Buckinghamshire STI.csv", row.names=F, sep=",")

write.table (Data_Cornwall, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Cornwall_Southe_Blackb STI.csv", row.names=F, sep=",")


write.table (Data_DerbyshireDerby, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month DerbyshireDerby STI.csv", row.names=F, sep=",")
write.table (Data_Dorset, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Dorset STI.csv", row.names=F, sep=",")
write.table (Data_Essex, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Essex STI.csv", row.names=F, sep=",")
write.table (Data_Thurrock, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Thurrock STI.csv", row.names=F, sep=",")
write.table (Data_Hillingdon, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Hillingdon STI.csv", row.names=F, sep=",")
write.table (Data_LLR.MPFT, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month LLR.MPFT STI_Hep.csv", row.names=F, sep=",")
write.table (Data_NIreland, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month NIreland STI.csv", row.names=F, sep=",")
write.table (Data_Worces_Hereford, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Worces_Hereford STI.csv", row.names=F, sep=",")
# End Backing Data STI ----



# CT TREATMENTS----
#Include 'Area', 'Site' and 'LA of residence' from 'orders' file. Like an Excel VLOOKUP, using merge()
TreatmentsMerge <- merge(OrdersToWork [, c('SH24.UID','Area','Site','LSOA.name','LA.of.residence','Sexual.preference',"Notified.at")], 
                         Treatments, by.x = 'SH24.UID', by.y = 'sh24_uid')
table(TreatmentsMerge$Region)
#CT treatment files  
Treatment.Essex <- TreatmentsMerge [(TreatmentsMerge$Site=="Essex Hub"), 
                                    c("SH24.UID","customer_id",'Site','Area',"created_at","offered_at","prescription_at","dispatched_at","Notified.at")]

Treatment.Thurrock <- TreatmentsMerge [(TreatmentsMerge$Region=="Thurrock"), 
                                       c("SH24.UID","customer_id",'Site','Area',"created_at","offered_at","prescription_at","dispatched_at","Notified.at")]

Treatment.DerbyshireDerby <- TreatmentsMerge [(TreatmentsMerge$Site=="Wheatbridge Clinic" | TreatmentsMerge$Site=="London Road Community Hospital"), 
                                              c("SH24.UID",'Site','Area',"created_at","offered_at","prescription_at","dispatched_at","Notified.at")]

Treatment.LLR.MPFT <- TreatmentsMerge [(TreatmentsMerge$Area=="Rutland" | TreatmentsMerge$Area=="Leicester" | TreatmentsMerge$Area=="Leicestershire" |
                                          TreatmentsMerge$Area=="Shropshire" | TreatmentsMerge$Area=="Telford and Wrekin" | TreatmentsMerge$Area=="Staffordshire" | 
                                          TreatmentsMerge$Area=="North Staffordshire" | TreatmentsMerge$Area=="Stoke")
                                       , c("SH24.UID",'LSOA.name','Site','Area',"created_at","offered_at","prescription_at","dispatched_at",'Age','Gender','Sexual.preference.x','Ethnicity')]
# Include LSOA Code in LLR.MPFT
Treatment.LLR.MPFT <- merge(Treatment.LLR.MPFT, LSOA[,c('LSOA11NM',"LSOA11CD")], by.x = "LSOA.name", by.y = "LSOA11NM")


Treatment.Berkshire <- TreatmentsMerge [(TreatmentsMerge$Area=="East Berkshire")
                                        , c("SH24.UID",'LSOA.name','Site','Area',"created_at","offered_at","prescription_at","dispatched_at",'Age','Gender','Sexual.preference.x','Ethnicity')]

Treatment.NIreland <- TreatmentsMerge [(TreatmentsMerge$Area=="Northern Ireland")
                                       , c("SH24.UID",'LSOA.name','Site','Area',"created_at","offered_at","prescription_at","dispatched_at",'Age','Gender','Sexual.preference.x','Ethnicity')]

Treatment.Dorset <- TreatmentsMerge [(TreatmentsMerge$Area=="Dorset")
                                     , c("SH24.UID",'LSOA.name','Site','Area',"created_at","offered_at","prescription_at","dispatched_at",'Age','Gender'
                                         ,'Sexual.preference.x','Ethnicity',"no_of_partners_last_6_months","no_of_contact_for_partners","no_of_partners_notified")]

Treatment.Bromley <- TreatmentsMerge [(TreatmentsMerge$Area=="Bromley")
                                      , c("SH24.UID",'LSOA.name','Site','Area',"created_at","offered_at","prescription_at","dispatched_at",
                                          'Age','Gender','Sexual.preference.x','Ethnicity',"no_of_partners_last_6_months",            
                                          "no_of_contact_for_partners","no_of_partners_notified")]

write.table (Treatment.Berkshire, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Berkshire Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Bromley, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Bromley Treatments.csv", row.names=F, sep=",")
write.table (Treatment.DerbyshireDerby, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month DerbyshireDerby Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Dorset, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Dorset Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Essex, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Essex Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Thurrock, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Thurrock Treatments.csv", row.names=F, sep=",")
write.table (Treatment.LLR.MPFT, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month LLR.MPFT Treatments.csv", row.names=F, sep=",")
write.table (Treatment.NIreland, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month NIreland Treatments.csv", row.names=F, sep=",")



# Backing data CONTRACEPTION----
# run "recodeContraception" function and save output in Area
COC$Area <- 0
COC$Area <- recodeContraception(DF=COC,varname="Area",varname2="Region")
POP$Area <- 0
POP$Area <- recodeContraception(DF=POP,varname="Area",varname2="Region")

# check that all orders are allocated to an Area
Zero <- COC[(COC$Area==0),]
Zero <- POP[(POP$Area==0),]
rm(Zero)


# OC backing data files Dorset
# 8th Sep 2022: Lisa confirms they don't need these variables: "Taken.POP.before.","Ordered.POP.from.SH.24.before.","Ordered.OC.from.SH.24.before."
COC_Dorset <- COC[ (COC$Area=="Dorset")
                   ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year","Prescription.at","Prescription.at.month.year"
                   ,"Dispatched.at","Dispatched.at.month.year","Months.prescribed","Clinic","Region","LSOA.name")]

POP_Dorset <- POP [(POP$Area=="Dorset")
                   ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year","Prescription.at","Prescription.at.month.year"
                   ,"Dispatched.at","Dispatched.at.month.year","Months.prescribed","Clinic","Region","LSOA.name")]

write.table (COC_Dorset, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Dorset COC.csv", row.names=F, sep=",")
write.table (POP_Dorset, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Dorset POP.csv", row.names=F, sep=",")



# OC backing data files Derby and Derbyshire
COC_DerbyshireDerby <- COC [(COC$Clinic=="Wheatbridge Clinic" | COC$Clinic=="London Road Community Hospital")
                              ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year",
                                 "Prescription.at","Prescription.at.month.year","Dispatched.at","Dispatched.at.month.year",
                                 "Months.prescribed","Clinic","Region",
                                 "Taken.COC.before.","Ordered.COC.from.SH.24.before.","LSOA.name")]
# 11.09.2022:add the columns we removed in Sep.2022
# 11.09.2022: Sarah needs files with usual formatting, though they don't use those columns for reporting - they may be ok left blank: confirm with Sarah
COC_DerbyshireDerby$Ordered.OC.from.SH.24.before. <- ""
COC_DerbyshireDerby$Area <- "Region" #add this variable here to keep original order of columns


POP_DerbyshireDerby <- POP [(POP$Clinic=="Wheatbridge Clinic" | POP$Clinic=="London Road Community Hospital")
                            ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year",
                               "Prescription.at","Prescription.at.month.year","Dispatched.at","Dispatched.at.month.year",
                               "Months.prescribed","Clinic","Region","LSOA.name")]
POP_DerbyshireDerby$Taken.POP.before. <- ""
POP_DerbyshireDerby$Ordered.POP.from.SH.24.before. <- ""
POP_DerbyshireDerby$Ordered.OC.from.SH.24.before. <- ""
POP_DerbyshireDerby$Area <- "Region"

# Merge contraception data with LSOA file, to get LSOA Code for LLR
COC_DerbyshireDerby <- merge(COC_DerbyshireDerby, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")
POP_DerbyshireDerby <- merge(POP_DerbyshireDerby, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")
names(POP_DerbyshireDerby)

write.table (COC_DerbyshireDerby, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month DerbyshireDerby COC.csv", row.names=F, sep=",")
write.table (POP_DerbyshireDerby, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month DerbyshireDerby POP.csv", row.names=F, sep=",")


# OC backing data files LLR.MPFT
COC_LLR.MPFT <- COC[ (COC$Area=="Leicester" | COC$Area=="Leicestershire" | COC$Area=="Rutland" | COC$Area=="Shropshire" |
                      COC$Area=="Telford and Wrekin" | COC$Area=="Staffordshire" | COC$Area=="North Staffordshire" | COC$Area=="Stoke")
                   ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year","Prescription.at","Prescription.at.month.year"
                      ,"Dispatched.at","Dispatched.at.month.year","Months.prescribed","Clinic","Region","LSOA.name")]


POP_LLR.MPFT <- POP [(POP$Area=="Leicester" | POP$Area=="Leicestershire" |POP$Area=="Rutland" | POP$Area=="Shropshire" |
                              POP$Area=="Telford and Wrekin" | POP$Area=="Staffordshire" | POP$Area=="North Staffordshire" | POP$Area=="Stoke")
                     ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year",
                        "Prescription.at","Prescription.at.month.year","Dispatched.at","Dispatched.at.month.year",
                        "Months.prescribed","Clinic","Region","LSOA.name")]

# Merge contraception data with LSOA file, to get LSOA Code for LLR
COC_LLR.MPFT <- merge(COC_LLR.MPFT, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")
POP_LLR.MPFT <- merge(POP_LLR.MPFT, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")



# OC backing data files Cornwall, Blackburn, Southend
COC_Cornwall <- COC [(COC$Region=="Cornwall and Isles of Scilly PCT" | COC$Region=="Blackburn with Darwen" | COC$Region=="Southend-on-Sea"),
                     c('ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year","Prescription.at","Prescription.at.month.year"
                       ,"Dispatched.at","Dispatched.at.month.year","Months.prescribed","Clinic","Region","LSOA.name")]

POP_Cornwall <- POP [(POP$Region=="Cornwall and Isles of Scilly PCT" | POP$Region=="Blackburn with Darwen" | POP$Region=="Southend-on-Sea"),
                     c('ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year",
                       "Prescription.at","Prescription.at.month.year","Dispatched.at","Dispatched.at.month.year",
                       "Months.prescribed","Clinic","Region","LSOA.name")]

write.table (COC_LLR.MPFT, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month LLR.MPFT COC.csv", row.names=F, sep=",")
write.table (POP_LLR.MPFT, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month LLR.MPFT POP.csv", row.names=F, sep=",")
write.table (COC_Cornwall, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Cornwall_Southe_Blackb COC.csv", row.names=F, sep=",")
write.table (POP_Cornwall, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022 Month Cornwall_Southe_Blackb POP.csv", row.names=F, sep=",")
# End Backing Data CONTRACEPTION ----


# Invoicing backing data Essex----
invSTIessexThurr <- invSTI[(invSTI$default_la == 'Essex' | invSTI$default_la == 'Thurrock'),]
# create MonthYear column
invSTIessexThurr$MonthYear <- as.Date(invSTIessexThurr$processed_at, "%Y-%m-%d")
invSTIessexThurr$MonthYear <- format(as.Date(invSTIessexThurr$MonthYear), "%Y-%m")
# extract data for reporting month and relevant columns
invSTIessexThurrMonth <- invSTIessexThurr[(invSTIessexThurr$MonthYear == v1), c("overall_type","default_la","repeat_kit","processed_at","MonthYear","invoice_category_all","invoice_category_billable")]  

write.table (invSTIessexThurrMonth, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022.09_invoic_EssexThurrock.csv", row.names=F, sep=",")
# END Invoicing backing data Essex



#clean up the environment
rm(list =ls())



# One-off ----
# 2022.01.31 Negar/Justin add sh24 uids to Bucks data and CT treatment data with sh24 uids
Data_Bucks <- BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Buckinghamshire"),]
Data_Bucks$Created.at <- as.Date(Data_Bucks$Created.at, format = "%Y-%m-%d")
Data_Bucks <- Data_Bucks[(Data_Bucks$Created.at > "2020-12-31"),]
Treatment_Bucks <- TreatmentsMerge [(TreatmentsMerge$Region=="Buckinghamshire"), 
                                    c("SH24.UID","customer_id",'Site','Area',"created_at","offered_at","prescription_at","dispatched_at","Notified.at")]
write.table (Data_Bucks, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_02\\backing_data\\2022.01.31 Bucks_NegarSTI.csv", row.names=F, sep=",")
write.table (Treatment_Bucks, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_02\\backing_data\\2022.01.31 Bucks_NegarCTtreatm.csv", row.names=F, sep=",")

# 2022.01.31 Justin Wirral data to check safeguarding
Data_Wirral <- BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Wirral"),]
write.table (Data_Wirral, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_02\\backing_data\\2022.01.31 Wirral_Justin.csv", row.names=F, sep=",")

# 2022.02.15 Blake Freetesting - Warwickshire
Data_Warwickshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Freetesting - Warwickshire"),]
write.table (Data_Warwickshire, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_02\\backing_data\\2022.02.15 Freetesting_Warwickshire.csv", row.names=F, sep=",")

# 2022.02.17 Blake Nottinghamshire
Data_Notting <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Nottingham" | 
                                      BackingMin_noSH24$Default.LA=="Freetesting - Nottingham" |
                                      BackingMin_noSH24$Default.LA=="Freetesting - Nottinghamshire" ),]

class(Data_Notting$Created.at)
Data_Notting$Created.at <- as.Date(Data_Notting$Created.at, format = "%Y-%m-%d")
Data_Notting <- Data_Notting[(Data_Notting$Created.at >= "2021-10-01" & 
                                Data_Notting$Created.at <= "2021-12-31"),]
Data_Notting$SplitAge <- 0
Data_Notting$SplitAge = ifelse(Data_Notting$Age>24,"25+","Under 25")
table(Data_Notting$SplitAge)
write.table (Data_Notting, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_02\\backing_data\\2022.02.17 Data_Notting.csv", row.names=F, sep=",")


# 19.02.2021 Blake: PHE and freetesting Manchester----
Data_Manchester <- BackingMin_WithSH24[grep('PHE|Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan', 
                                            BackingMin_WithSH24$Default.LA),]

Data_Manchester1 <- merge(Data_Manchester, LSOA.UpperTier[,c('LSOA11NM',"UTLA18NM")], by.x = "LSOA.name", by.y = "LSOA11NM", all.x = TRUE)
# Subset using 'grep' and 'or' | 
Data_Manchester1 <- Data_Manchester1[grep('Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan',Data_Manchester1$UTLA18NM),]
# 'droplevels' to remove the rows from the table function output which have 0 counts 
table(droplevels(Data_Manchester1$Default.LA))
Data_Manchester1$Area = NULL
Data_Manchester1$LA.of.residence = NULL
write.table (Data_Manchester1, file="/Users/ElenaArdines1/Documents/Reports/2.Ad-hoc-reports/2022.01.25_PHE.Freetesting.Manchester_Blake.csv", row.names=F, sep=",")
# END PHE and freetesting Manchester----

# Freetesting Bedford----
Data_Bedford <- BackingMinimum [(BackingMinimum$Default.LA=="Freetesting - Bedford" |
                                   BackingMinimum$Default.LA=="Freetesting - Central Bedfordshire" |
                                   BackingMinimum$Default.LA=="Freetesting - Milton Keynes"),]
Data_Bedford$Area = NULL
Data_Bedford$LA.of.residence = NULL
Data_Bedford <- merge(Data_Bedford, LSOA[,c('LSOA11NM',"LAD19NM")], by.x = "LSOA.name", by.y = "LSOA11NM")
table(Data_Bedford$LAD19NM)
write.table (Data_Bedford, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2020\\2020 06\\backing_data\\2020 freetesting.Bedford_Blake.csv", row.names=F, sep=",")
# END Freetesting Bedford----

# RoyalLiverpool Justin----
Data_RoyalLiverpool <- orders[,c('Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence',
                                 'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
                                 'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.5.days',
                                 'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
                                 'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                                 'Sexuality',"Sites.tested",'Test.regime',
                                 "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Lab.receipt.at","Notified.at",
                                 "Lab.results.at","Lab.results.at.month.year",
                                 "Previously.diagnosed.with.HIV",
                                 "Vaccinated.against.hepatitis.B","Injected.drugs.groups.chems.fisting","Paid.sex.work",
                                 "Previously.treated.for.Syphilis",
                                 'Syphilis','HIV','Chlamydia','Gonorrhoea',"Test.for.Syphilis.RPR")]


Data_RoyalLiverpool1 <-  Data_RoyalLiverpool [(Data_RoyalLiverpool$Default.LA=="Warrington" | Data_RoyalLiverpool$Default.LA=="Halton" |
                                    Data_RoyalLiverpool$Default.LA=="Liverpool" | Data_RoyalLiverpool$Default.LA=="Cheshire East" |
                                    Data_RoyalLiverpool$Default.LA=="Knowsley"),]

Data_RoyalLiverpool2 <- merge(Data_RoyalLiverpool1, LSOA.UpperTier[,c('LSOA11NM',"UTLA18NM")], by.x = "LSOA.name", by.y = "LSOA11NM", all.x = TRUE)

write.table (Data_RoyalLiverpool2, file="~/Reports/1.Monthly_Reports/Invoicing/2022/2022_09/BackingData/2022.09_RoyalLiverpool.csv", row.names=F, sep=",")



Data_RoyalLiverpool <-  BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Liverpool"),]
Data_RoyalLiverpool1 <- Data_RoyalLiverpool[(Data_RoyalLiverpool$Created.at > '2020-10-01'),]
write.table (Data_CheshireEast1, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_01\\backing_data\\2021.05.10 Justin CheshireEast STI.csv", row.names=F, sep=",")

Data_CheshireEast <-  BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Cheshire East"),]
Data_CheshireEast1 <- Data_CheshireEast[(Data_CheshireEast$Created.at > '2020-09-20'),]
write.table (Data_CheshireEast1, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_01\\backing_data\\2021.05.10 Justin CheshireEast STI.csv", row.names=F, sep=",")
# END RoyalLiverpool----

#CT treatment for Darlington
Treatment.Durham.Darlington <- TreatmentsMerge [(TreatmentsMerge$Site=="Bishop Auckland Hospital, Centre for Sexual Health" |
                                                   TreatmentsMerge$Site=="GUM Department, University Hospital of North Durham" |
                                                   TreatmentsMerge$Site=="Darlington GUM Clinic" )
                                                , c('Site','LA.of.residence',"Created.at","Offered.at","Prescription.at","Dispatched.at","customer.ID")]


Treatment.Durham.Darlington <- TreatmentsMerge [(TreatmentsMerge$Area=="Darlington")
                                                , c('Site','LA.of.residence',"Created.at","Offered.at","Prescription.at","Dispatched.at","Customer.ID")]


table(TreatmentsMerge$Area=="Darlington")
names(TreatmentsMerge)
write.table (Treatment.Durham.Darlington, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2020\\2020 05\\backing_dataata\\2020.01.Treatment.Durham.Darlington.csv", row.names=F, sep=",")

# END One-off

