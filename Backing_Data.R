# working directory: Masters for sharing
# run recoding
#install.packages("DBI")
#install.packages("RPostgreSQL")
#drv <- dbDriver("PostgreSQL")
install.packages("reshape2")
library(reshape2)

# install openxlsx package: Read, Write and Edit XLSX File, no need to install Java.
#library("openxlsx", lib.loc="~/R/win-library/4.0.4")

setwd("/Users/ElenaArdinesTomas/Documents/Reports/2022-03")
setwd("~/Documents/Reports/0. Master for sharing")
setwd("~/Documents/Reports/2. Ad-hoc reports/Feedback")

orders = read.csv("20220302_sti_orders.csv")
inv = read.csv("Invoic_Test.csv")

# REPEATS INVESTIGATION----
orders = read.csv("20210706_Sti_IncludingRepeats.csv")
orders1 <- orders[,c("Region","test_kit_index","dispatched_at")]
orders1$Dispatched_day <- as.Date(orders1$dispatched_at, "%Y-%m-%d")
orders1$DispatchedMonthYr <- format(as.Date(orders1$dispatched_at), "%Y-%m")
names(orders)
table(orders1$test_kit_index,orders1$DispatchedMonthYr=="2021-06")
# End Repeats investigation----  

feedback = read.csv("20201020_Feedback_tokens.csv")

Treatments = read.csv("20220301_CT_Treatments.csv")
ContCOC = read.csv("20220301_COC.csv")
ContPOP = read.csv("20220301_POP.csv")
ECFuture = read.csv("20220302_EC_future_month.csv")
ECNow = read.csv("20220302_EC_now_month.csv")
PhotoConsult = read.csv("20220301_PD_consultations.csv")
PhotoTreatm = read.csv("20220301_PD_treatments.csv")
Injectable = read.csv("20220301_Injectable.csv")
Patch = read.csv("20220301_Patch.csv")  
Ring = read.csv("20220301_Ring.csv")


# reactivity levels from the 'miscellaneous' query in DataGrip, for freetesting
reactivity = read.csv("20210506_Reactivity_levels_PHE.csv")

FranECNow = read.csv("20220301_ECNow_disaggregated.csv")
FranECFut = read.csv("20220301_ECFuture_disaggregat.csv")

LSOA = read.csv("Lower_Layer_Super_Output_Area_(2011)_to_Ward_(2019)_Lookup_in_England_and_Wales.csv") # England and Wales

LSOA_All = read.csv("National_Statistics_Postcode_Lookup_UK.csv") # all UK (inc Northern Ireland and Scotland)
# LSOA_All is a huge list because includes all postcodes. Remove postcodes and duplicated LSOAs.
LSOA_All_1 <- LSOA_All
LSOA_All_1 <- LSOA_All_1[ ,c("Local.Authority.Code","Local.Authority.Name","Ward.Code","Ward.Name","Lower.Super.Output.Area.Code","Lower.Super.Output.Area.Name")]
LSOA_All_1 <- LSOA_All_1[!duplicated(LSOA_All_1$Lower.Super.Output.Area.Name),]


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
#CHECK THIS: ABOUT ADDING REACTIVITY LEVELS
        #split reactivity data frame in two, to get separate columns in the backing data - otherwise the SH24 number would show as duplicate
        #reactivityHIV <- reactivity[(reactivity$test_klass=="Hiv::Blood"),]
        #reactivityHIV.Unique = reactivityHIV[!duplicated(reactivityHIV$sh24_uid),] #get only unique SH24 uids
        #reactivitySyph <- reactivity[(reactivity$test_klass=="Syphilis::Treponemal"),]
        #freetesting5 <- merge(freetesting4, reactivityHIV.Unique[,c('result_value')], by.x = "SH24.UID", by.y = "sh24_uid", all.x = TRUE)

write.table (freetesting4, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.01 freetesting.PHE.ImpactPrEP.csv", row.names=F, sep=",")
write.table (freetesting1, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\20210901_freetesting_draft.csv", row.names=F, sep=",")
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
######### include from Marh: 

#Create Area variable and set to 0
OrdersToWork$Area <- 0
#run "recode Area" function and save output in Area; important to write your variable names in colons (but not the data frame name)
OrdersToWork$Area <- recodeArea(DF=OrdersToWork,varname="Area",varname2="Site",varname3 = "LA.of.residence", varname4="Referred.from",varname5="Default.LA")


   Bucks <- OrdersToWork[(OrdersToWork$Default.LA=='Buckinghamshire'),]
   table(Bucks$Created.at.month.year)
   
table(OrdersToWork$Default.LA)
-----------------------------------------

# check that all orders are allocated to an Area
Zero <- OrdersToWork[(OrdersToWork$Area==0),]
rm(Zero)

table(OrdersToWork$Dispatched.at.month.year=="2022-01")
table(OrdersToWork$Lab.results.at.month.year=="2022-01")



#FETTLE ORDERS############################## ----
#Select variables you need, and then extract only those Site=Fettle
OrdersFettle <- orders[(orders$Site=="Fettle Hub"),]
OrdersFettle <- OrdersFettle[,c("SH24.UID",'Customer.ID','LA.of.residence',"LSOA.name","Sites.tested","Test.regime",'Site','Age','Gender'
                                ,"Sexual.preference","Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C","Charge.token","Feedback.token","Discount.code",
                                "Requested.time","Created.at.month.year","Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year",
                                "Lab.results.at","Lab.results.at.month.year")]

class(OrdersFettle$Requested.time)
structure(OrdersFettle$Requested.time)

OrdersFettleTime <- OrdersFettle
OrdersFettleTime <- tribble(~timestamp,OrdersFettle$Requested.time)
                            
OrdersFettle$TimeGrouped <- 0
OrdersFettle$TimeGrouped[OrdersFettle$Requested.time=="Beckenham Beacon Sexual Health Clinic"|
                    OrdersToWork$Site=="Camberwell"|
                    OrdersToWork$Site=="Waldron Health Centre"] <- "London"
#End FETTLE ORDERS##########################

############################################

# Stack two Stripe data sets one (April-17July) on top of the other (Jan-18June)
StripeStack <- rbind(Stripe, Stripe1)  
# new data frame with unique (no duplicate) 'customer IDs'id'
StripeStackUnique = StripeStack[!duplicated(StripeStack$id),]

# Merge both data sets: OrdersFettle (from admin) and Stripe (payments from Stripe). Like an Excel VLOOKUP, using merge()----
# Use all.y = TRUE at the end to do Right Outer Join (with merge), for all rows from the right table, and any rows with matching keys from the left table
names(Stripe)
StripeMerge <- merge(OrdersFettle, Stripe[,c("id","Description","Created..UTC.","Created.MonthYear","Amount","Amount.Refunded","Fee","Status")],
                     by.x = 'Charge.token', by.y = 'id', all.y = TRUE)

# Include columns with "1" for the calculated field to calculate average value per day for Graham 
StripeMerge$NumOrders <- 1

#table per sites tested, and then get the proportions of the table----
table(StripeMerge$Gender)
prop.table(table(StripeMerge$Sites.tested))

write.table (StripeMerge, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\Stripe_Fettle_SinceStart.csv", row.names=F, sep=",")

#Fettle backing data to Graham
FettleGraham <- OrdersFettle [(OrdersFettle$Site == "Fettle Hub") & (OrdersFettle$Created.at >"2018-12-31" & OrdersFettle$Created.at <"2019-01-31"),]
write.table (FettleGraham, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\Fettle_Graham_2019_01.csv", row.names=F, sep=",")
#End FETTLE ORDERS----


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





### Slack with Justin 2021: SHALL WE ADD DISTRIBUTION CENTER & METHOD TO ALL AREAS?!?!?!?!


Data_Berkshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="East Berkshire"),]
Data_Buckinghamshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Buckinghamshire"),]
  #Buckinghamshire data only from 1st Jan 2021
  class(Data_Buckinghamshire$Created.at)
  Data_Buckinghamshire$Created.at <- as.Date(Data_Buckinghamshire$Created.at, format = "%Y-%m-%d")
  Data_Buckinghamshire <- Data_Buckinghamshire[(Data_Buckinghamshire$Created.at > "2020-12-31"),]

Data_Cornwall <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Cornwall and Isles of Scilly PCT"
                                     | BackingMin_noSH24$Default.LA=="Southend-on-Sea"
                                     |BackingMin_noSH24$Default.LA=="Blackburn with Darwen"),]
Data_Hillingdon <- BackingMin_noSH24 [(BackingMin_noSH24$Site=="Hesa Primary Care Centre"),]
Data_Medway <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Medway"),]
Data_NIreland <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Northern Ireland Belfast PCT" | BackingMin_noSH24$Default.LA=="Northern Ireland Northern PCT" |
                 BackingMin_noSH24$Default.LA=="Northern Ireland South Eastern PCT" | BackingMin_noSH24$Default.LA=="Northern Ireland Southern PCT" |
                 BackingMin_noSH24$Default.LA=="Northern Ireland Western PCT"),]
Data_Worces_Hereford <- BackingMin_noSH24 [(BackingMin_noSH24$Site=="Worcestershire Hub" | BackingMin_noSH24$Site=="iSH Hereford"),]
# End REST OF AREAS----


# RoyalLiverpool Justin----
Data_RoyalLiverpool <-  BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Warrington" | BackingMin_WithSH24$Default.LA=="Halton" |
               BackingMin_WithSH24$Default.LA=="Liverpool" | BackingMin_WithSH24$Default.LA=="Cheshire East" |
               BackingMin_WithSH24$Default.LA=="Knowsley"),]

Data_RoyalLiverpool <-  BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Liverpool"),]
Data_RoyalLiverpool1 <- Data_RoyalLiverpool[(Data_RoyalLiverpool$Created.at > '2020-10-01'),]

Data_CheshireEast <-  BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Cheshire East"),]
Data_CheshireEast1 <- Data_CheshireEast[(Data_CheshireEast$Created.at > '2020-09-20'),]
write.table (Data_CheshireEast1, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_01\\Backing data\\2021.05.10 Justin CheshireEast STI.csv", row.names=F, sep=",")
# END RoyalLiverpool----


# Ireland backing data Justin----
Data_Ireland <- orders [(orders$Default.LA=="Ireland - Dublin" | orders$Default.LA=="Ireland - Cork" 
                         | orders$Default.LA=="Ireland - Kerry" | orders$Default.LA=="Ireland - Kildare" 
                         | orders$Default.LA=="Ireland - Wicklow" | orders$Default.LA=="Ireland - Cavan" |
                      orders$Default.LA=="Ireland - Louth" | orders$Default.LA=="Ireland - Meath" |
                      orders$Default.LA=="Ireland - Monaghan" | orders$Default.LA=="Ireland - Offaly" |
                      orders$Default.LA=="Ireland - Galway" | orders$Default.LA=="Ireland - Mayo" |
                      orders$Default.LA=="Ireland - Laois" | orders$Default.LA=="Ireland - Roscommon" |
                        orders$Default.LA=="Ireland - Donegal" | orders$Default.LA=="Ireland - Sligo" |
                        orders$Default.LA=="Ireland - Leitrim" | orders$Default.LA=="Ireland - Limerick" |
                        orders$Default.LA=="Ireland - Clare" | orders$Default.LA=="Ireland - Tipperary" ),]

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

write.table (Data_Ireland, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.02.02_Justin_Ireland_STI.csv", row.names=F, sep=",")

# END Ireland Justin----

#Remove 'Area' from the files (no need to include it)
Data_DerbyshireDerby$Area=NULL

#export data to csv (export the data of the ordered file). Use double \\ when setting destination file----
write.table (Data_Berkshire, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Berkshire STI.csv", row.names=F, sep=",")
write.table (Data_Bromley, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2022\\2022_01\\BackingData\\2022.02.22 Bromley STI.csv"
             , row.names=F, sep=",")

write.table (Data_Buckinghamshire, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Buckinghamshire STI.csv", row.names=F, sep=",")
write.table (Data_Cornwall, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Cornwall_Southe_Blackb STI.csv", row.names=F, sep=",")
write.table (Data_DerbyshireDerby, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month DerbyshireDerby STI.csv", row.names=F, sep=",")
write.table (Data_Dorset, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Dorset STI.csv", row.names=F, sep=",")
write.table (Data_Essex, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Essex STI.csv", row.names=F, sep=",")
write.table (Data_Thurrock, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Thurrock STI.csv", row.names=F, sep=",")
write.table (Data_Hillingdon, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Hillingdon STI.csv", row.names=F, sep=",")
write.table (Data_LLR.MPFT, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month LLR.MPFT STI_Hep.csv", row.names=F, sep=",")
write.table (Data_Medway, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Medway STI.csv", row.names=F, sep=",")
write.table (Data_NIreland, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month NIreland STI.csv", row.names=F, sep=",")
write.table (Data_Worces_Hereford, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Worces_Hereford STI.csv", row.names=F, sep=",")
# End Backing Data STI ----


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

### Data_PHEManchester <- merge(Data_PHEManchester, LSOA[,c('LSOA11NM',"LAD19NM")], by.x = "LSOA.name", by.y = "LSOA11NM")
### Data_PHEManchester <- rename(Data_PHEManchester, LA.Name = LAD19NM)
### Subset using 'grep' and 'or' | 
### Data_PHEManchester1 <- Data_PHEManchester[grep('Bolton|Bury|Manchester|Oldham|Rochdale|Salford|Stockport|Tameside|Trafford|Wigan', 
###                                               Data_PHEManchester$LA.Name),]
# END PHE and freetesting Manchester----

# Freetesting Bedford----
Data_Bedford <- BackingMinimum [(BackingMinimum$Default.LA=="Freetesting - Bedford" |
                                   BackingMinimum$Default.LA=="Freetesting - Central Bedfordshire" |
                                   BackingMinimum$Default.LA=="Freetesting - Milton Keynes"),]
Data_Bedford$Area = NULL
Data_Bedford$LA.of.residence = NULL
Data_Bedford <- merge(Data_Bedford, LSOA[,c('LSOA11NM',"LAD19NM")], by.x = "LSOA.name", by.y = "LSOA11NM")
table(Data_Bedford$LAD19NM)
write.table (Data_Bedford, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2020\\2020 06\\Backing data\\2020 freetesting.Bedford_Blake.csv", row.names=F, sep=",")
# END Freetesting Bedford----


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

write.table (Treatment.Berkshire, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Berkshire Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Bromley, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Bromley Treatments.csv", row.names=F, sep=",")
write.table (Treatment.DerbyshireDerby, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month DerbyshireDerby Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Dorset, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Dorset Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Essex, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Essex Treatments.csv", row.names=F, sep=",")
write.table (Treatment.Thurrock, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Thurrock Treatments.csv", row.names=F, sep=",")
write.table (Treatment.LLR.MPFT, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month LLR.MPFT Treatments.csv", row.names=F, sep=",")
write.table (Treatment.NIreland, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month NIreland Treatments.csv", row.names=F, sep=",")

#specific request
Data_Durham <- BackingMin_noSH24[(BackingMin_noSH24$LA.of.residence=="County Durham" &
                                 (BackingMin_noSH24$Created.at >"2017-12-31" & BackingMin_noSH24$Created.at <"2019-01-01")),]
Data_Darlington <- BackingMin_noSH24[(BackingMin_noSH24$LA.of.residence=="Darlington" &
                                     (BackingMin_noSH24$Created.at >"2017-12-31" & BackingMin_noSH24$Created.at <"2019-01-01")),]

Data_DarlingtonDurham <- BackingMin_noSH24[(BackingMin_noSH24$Default.LA=="Darlington" | BackingMin_noSH24$Default.LA=="County Durham"),]
Data_DarlingtonDurham$Customer.ID = NULL

table(BackingMin_noSH24$Default.LA)

write.table (Data_DarlingtonDurham, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2020.06.10 DarlingtonDurham.csv", row.names=F, sep=",")


#CT treatment for Darlington
Treatment.Durham.Darlington <- TreatmentsMerge [(TreatmentsMerge$Site=="Bishop Auckland Hospital, Centre for Sexual Health" |
                                                   TreatmentsMerge$Site=="GUM Department, University Hospital of North Durham" |
                                                   TreatmentsMerge$Site=="Darlington GUM Clinic" )
                                                , c('Site','LA.of.residence',"Created.at","Offered.at","Prescription.at","Dispatched.at","customer.ID")]


Treatment.Durham.Darlington <- TreatmentsMerge [(TreatmentsMerge$Area=="Darlington")
                   , c('Site','LA.of.residence',"Created.at","Offered.at","Prescription.at","Dispatched.at","Customer.ID")]



table(TreatmentsMerge$Area=="Darlington")
names(TreatmentsMerge)
write.table (Treatment.Durham.Darlington, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2020\\2020 05\\Backing data\\2020.01.Treatment.Durham.Darlington.csv", row.names=F, sep=",")

#CT treatment for Cheshire East 2021.05.10 Justin
Treatment.CheshireEast <- TreatmentsMerge [(TreatmentsMerge$Area=="Cheshire East")
                    , c("SH24.UID",'LSOA.name','Site','Area',"created_at","offered_at","prescription_at","dispatched_at",'Age','Gender','Sexual.preference.x','Ethnicity')]
write.table (Treatment.CheshireEast, file="\\Users\\Elena Ardines\\Documents\\Reports\\1.Monthly_Reports\\Invoicing\\2021\\2021 06\\Backing data\\2021.05.10 Treatment.CheshireEast.Justin.csv", row.names=F, sep=",")
# END CT TREATMENTS----


# Backing data CONTRACEPTION----
COCToWork <- ContCOC[ ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year","Prescription.at","Prescription.at.month.year"
                        ,"Dispatched.at","Dispatched.at.month.year","Months.prescribed","Clinic","Region",
                        "Taken.COC.before.","Ordered.COC.from.SH.24.before.","Ordered.OC.from.SH.24.before.","LSOA.name")]

POPToWork <- ContPOP[ ,c("SH.24.UID",'ID','Customer.ID',"Age","Ethnicity","Sexuality","Created.at","Created.at.month.year","Prescription.at","Prescription.at.month.year"
                     ,"Dispatched.at","Dispatched.at.month.year","Months.prescribed","Clinic","Region",
                     "Taken.POP.before.","Ordered.POP.from.SH.24.before.","Ordered.OC.from.SH.24.before.","LSOA.name")]



# run "recodeContraception" function and save output in Area
COCToWork$Area <- 0
COCToWork$Area <- recodeContraception(DF=COCToWork,varname="Area",varname2="Region")
POPToWork$Area <- 0
POPToWork$Area <- recodeContraception(DF=POPToWork,varname="Area",varname2="Region")

# check that all orders are allocated to an Area
Zero <- COCToWork[(COCToWork$Area==0),]
Zero <- POPToWork[(POPToWork$Area==0),]
rm(Zero)


COC_LLR.MPFT <- COCToWork [(COCToWork$Area=="Leicester" | COCToWork$Area=="Leicestershire" | COCToWork$Area=="Rutland" | COCToWork$Area=="Shropshire" |
                              COCToWork$Area=="Telford and Wrekin" | COCToWork$Area=="Staffordshire" | COCToWork$Area=="North Staffordshire" | COCToWork$Area=="Stoke"),]
POP_LLR.MPFT <- POPToWork [(POPToWork$Area=="Leicester" | POPToWork$Area=="Leicestershire" |POPToWork$Area=="Rutland" | POPToWork$Area=="Shropshire" |
                              POPToWork$Area=="Telford and Wrekin" | POPToWork$Area=="Staffordshire" | POPToWork$Area=="North Staffordshire" | POPToWork$Area=="Stoke"),]

COC_DerbyshireDerby <- COCToWork [(COCToWork$Clinic=="Wheatbridge Clinic" | COCToWork$Clinic=="London Road Community Hospital"),]
POP_DerbyshireDerby <- POPToWork [(POPToWork$Clinic=="Wheatbridge Clinic" | POPToWork$Clinic=="London Road Community Hospital"),]

COC_Dorset <- COCToWork [(COCToWork$Area=="Dorset"),]
POP_Dorset <- POPToWork [(POPToWork$Area=="Dorset"),]

COC_Cornwall <- COCToWork [(COCToWork$Region=="Cornwall and Isles of Scilly PCT" | COCToWork$Region=="Blackburn with Darwen" 
                            | COCToWork$Region=="Southend-on-Sea"),]
POP_Cornwall <- POPToWork [(POPToWork$Region=="Cornwall and Isles of Scilly PCT" | POPToWork$Region=="Blackburn with Darwen"
                            | POPToWork$Region=="Southend-on-Sea"),]
COC_Cornwall$SH.24.UID = NULL
POP_Cornwall$SH.24.UID = NULL


# Merge contraception data with LSOA file, to get LSOA Code for LLR
COC_LLR.MPFT <- merge(COC_LLR.MPFT, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")
POP_LLR.MPFT <- merge(POP_LLR.MPFT, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")

COC_DerbyshireDerby <- merge(COC_DerbyshireDerby, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")
POP_DerbyshireDerby <- merge(POP_DerbyshireDerby, LSOA[,c("LSOA11CD",'LSOA11NM')], by.x = "LSOA.name", by.y = "LSOA11NM")

write.table (COC_LLR.MPFT, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month LLR.MPFT COC.csv", row.names=F, sep=",")
write.table (POP_LLR.MPFT, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month LLR.MPFT POP.csv", row.names=F, sep=",")
write.table (COC_DerbyshireDerby, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month DerbyshireDerby COC.csv", row.names=F, sep=",")
write.table (POP_DerbyshireDerby, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month DerbyshireDerby POP.csv", row.names=F, sep=",")
write.table (COC_Dorset, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Dorset COC.csv", row.names=F, sep=",")
write.table (POP_Dorset, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Dorset POP.csv", row.names=F, sep=",")
write.table (COC_Cornwall, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Cornwall_Southe_Blackb COC.csv", row.names=F, sep=",")
write.table (POP_Cornwall, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022 Month Cornwall_Southe_Blackb POP.csv", row.names=F, sep=",")
# End Backing Data CONTRACEPTION ----

#clean up the environment
rm(list =ls())



# One-off ----
# 2021.11.18 Hertfordshire Blake
Data_Hertfordshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Hertfordshire"),]
class (Data_Hertfordshire$Created.at)
Data_Hertfordshire$Created.at <- as.Date(Data_Hertfordshire$Created.at, format = "%Y-%m-%d")
Data_Hertfordshire <- Data_Hertfordshire [(Data_Hertfordshire$Created.at>'2021-07-01'),]
Data_Hertfordshire$Customer.ID = NULL
write.table (Data_Hertfordshire, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2021.10 Hertfordshire.csv", row.names=F, sep=",")

# 2022.01.31 Negar/Justin add sh24 uids to Bucks data and CT treatment data with sh24 uids
Data_Bucks <- BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Buckinghamshire"),]
Data_Bucks$Created.at <- as.Date(Data_Bucks$Created.at, format = "%Y-%m-%d")
Data_Bucks <- Data_Bucks[(Data_Bucks$Created.at > "2020-12-31"),]
Treatment_Bucks <- TreatmentsMerge [(TreatmentsMerge$Region=="Buckinghamshire"), 
                                    c("SH24.UID","customer_id",'Site','Area',"created_at","offered_at","prescription_at","dispatched_at","Notified.at")]
write.table (Data_Bucks, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.01.31 Bucks_NegarSTI.csv", row.names=F, sep=",")
write.table (Treatment_Bucks, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.01.31 Bucks_NegarCTtreatm.csv", row.names=F, sep=",")

# 2022.01.31 Justin Wirral data to check safeguarding
Data_Wirral <- BackingMin_WithSH24 [(BackingMin_WithSH24$Default.LA=="Wirral"),]
write.table (Data_Wirral, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.01.31 Wirral_Justin.csv", row.names=F, sep=",")

# 2022.02.15 Blake Freetesting - Warwickshire
Data_Warwickshire <- BackingMin_noSH24 [(BackingMin_noSH24$Default.LA=="Freetesting - Warwickshire"),]
write.table (Data_Warwickshire, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.02.15 Freetesting_Warwickshire.csv", row.names=F, sep=",")

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
write.table (Data_Notting, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/Invoicing/2022/2022_01/BackingData/2022.02.17 Data_Notting.csv", row.names=F, sep=",")


# END One-off

