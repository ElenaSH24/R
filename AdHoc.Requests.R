# Fran Fettle repeat cross-product: Stack all data sets on top of each other: STI, Stripe, CT.Treatments, COC, POP----
# For rbind function to operate, both data frames need to have the same number of columns and the same column names
#Create Area variable in all datasets and set to 0
Orders1 <- orders
#add type of order per data set
Orders1$Type <- 'STI'

# extract only columns needed
Orders1 <- Orders1[,grep("^created|dispa|customer|SH24.UID|Default|customer|Type",colnames(Orders1),perl = T,value = T,ignore.case = T)] 

#merge Orders with Treatments to get the Area
Treatments1 <- merge(Treatments[,c("sh24_uid","customer_id","created_at","dispatched_at")], Orders1[,c("SH24.UID","Default.LA")]
                     , by.x = "sh24_uid", by.y = "SH24.UID" , all.x = TRUE)
Treatments1$Type <- 'CT Treatment'

#add a variable month-year to the Treatments dataset, so all data sets have the same columns/columns names
#convert long dates to MonthYear in 2 steps: first convert factor to date with 'as.Date', second amend format to MonthYear with 'format'----
Treatments1$Created.at.month.year <- as.Date(Treatments1$created_at, format = "%Y-%m-%d")
Treatments1$Dispatched.at.month.year <- as.Date(Treatments1$dispatched_at, format = "%Y-%m-%d")

Treatments1$Created.at.month.year <- format(Treatments1$Created.at.month.year, "%Y-%m")
Treatments1$Dispatched.at.month.year <- format(Treatments1$Dispatched.at.month.year, "%Y-%m")
#rename column names (install dplyr and data.table packages), they should be identical in all data sets to use 'rbind'
Treatments1 <- rename(Treatments1, SH24.UID = sh24_uid)
Treatments1 <- rename(Treatments1, Customer.ID = customer_id)
Treatments1 <- rename(Treatments1, Created.at = created_at)
Treatments1 <- rename(Treatments1, Dispatched.at = dispatched_at)

# create contraception datasets
ContCOC1 <- ContCOC
ContCOC1$Type <- 'COC'
ContCOC1 <- ContCOC1[,grep("^customer|SH.24.UID|created|dispa|region|Type",colnames(ContCOC1),perl = T,value = T,ignore.case = T)] 
#rename column names, they should be identical in all data sets to be 'rbind'
ContCOC1 <- rename(ContCOC1, SH24.UID = SH.24.UID, Default.LA = Region)

ContPOP1 <- ContPOP
ContPOP1$Type <- 'POP'
ContPOP1 <- ContPOP1[,grep("^customer|SH.24.UID|created|dispa|region|Type",colnames(ContPOP1),perl = T,value = T,ignore.case = T)] 
#rename column names, they should be identical in all data sets to be 'rbind'
ContPOP1 <- rename(ContPOP1, SH24.UID = SH.24.UID, Default.LA = Region)

FranECNow1 <- FranECNow [ ,c("customer_id","sh24_uid","Region","Created.at","Created.at.month.year","Dispatched.at","Dispatched.at.month.year")]
FranECFut1 <- FranECFut [ ,c("customer_id","sh24_uid","Region","Created.at","Created.at.month.year","Dispatched.at","Dispatched.at.month.year")] 

# stack both EC datasets one on top of the other one
EC1 <- rbind(FranECNow1, FranECFut1)
#add type of order
EC1$Type <- 'EC'
#rename column names (install dplyr and data.table packages), they should be identical in all data sets to use 'rbind'
EC1 <- rename(EC1, SH24.UID = sh24_uid)
EC1 <- rename(EC1, Customer.ID = customer_id)
EC1 <- rename(EC1, Default.LA = Region)

# stack all data sets one of top of each other
DataStack <- rbind(Orders1, Treatments1,ContCOC1,ContPOP1,EC1)

DataStackFettle <- DataStack[(DataStack$Default.LA == "Fettle"),]


# metrics
nrow(DataStackFettle) #number of orders from all Fettle products
#number of orders per product
Fettle.OrdersCreated <- as.data.frame(table(DataStackFettle$Type))
colnames(Fettle.OrdersCreated)[1] <- "Product"
colnames(Fettle.OrdersCreated)[2] <- "Orders created"  
  
# number of unique users per Fettle product
Fettle.Unique <- aggregate(x = DataStackFettle$Customer.ID, # Specify data column
                        by = list(DataStackFettle$Type), # Specify group indicator
                        FUN = function(x) length(unique(x))) #Desired function

colnames(Fettle.Unique)[1] <- "Product"
colnames(Fettle.Unique)[2] <- "Count unique users"
        # print the new data frame to paste into Excel for Fran
        print(Fettle.Unique)

# put both data frames together:
Fettle.OrdersAndUnique = merge(x = Fettle.OrdersCreated, y = Fettle.Unique, by = "Product", all = TRUE)
# print to copy and paste in Excel
print(Fettle.OrdersAndUnique)





# remove SH24 number!
DataStackFettle$SH24.UID = NULL
write.table (DataStackFettle, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.03.14.DataStack_Fettle.csv", row.names=F, sep=",")


# Cross-product orders
# Reshape the data frame
################## CONTINUE FROM HERE 14.MARCH.2022 ###############
DataStackFettle.Wide <- DataStackFettle[,c('Customer.ID','Type')]

DataStackFettle.Wide1 <- reshape(DataStackFettle.Wide, idvar = "Customer.ID", timevar = "Type", direction = "wide")
reshape(dat1, idvar = "name", timevar = "numbers", direction = "wide")

COCandPOP$COC.POP[COCandPOP$Gender=="male" | GumcadQ$Genitals=="Penis"] <- 1 
GumcadQ$Gender[GumcadQ$Gender=="male" | GumcadQ$Genitals=="Penis"] <- 1 

# TRY THIS?: convert to data frame to get the frequency of ordering per user
DataStackFettle_1 <- as.data.frame(table(DataStackFettle$Customer.ID,DataStackFettle$Type))

# End Stack all data sets----



# Fran REPEAT USERS Fettle----
# To be able to work on repeat users, order file in chronological date, use FormatDate (class = Date)
OrdersRepeat <- orders[(order(as.Date(orders$Created.at))),]
# Subset for Fettle and dispatched orders (dispatched different than blank)
FettleDispatched <- OrdersRepeat[(OrdersRepeat$Site == "Fettle Hub" & OrdersRepeat$Dispatched.at != ''),]
# new data frame with unique (no duplicate) customer IDs
Fettle.Users = FettleDispatched[!duplicated(FettleDispatched$Customer.ID),]
# create data frame with Customer.ID grouped (i.e. how many times each Customer.ID shows up) - need package dplyr
Fettle.UsersGrouped <- FettleDispatched %>% group_by(Customer.ID) %>% summarise(How.Many.Times=n())
# count frequency: those who bought less than twice (= they bought once)
Bought.Fettle.Once <- Fettle.UsersGrouped[(Fettle.UsersGrouped$How.Many.Times<2),]
Bought.Fettle.Twice <- Fettle.UsersGrouped[(Fettle.UsersGrouped$How.Many.Times<3 & Fettle.UsersGrouped$How.Many.Times>1),]
Bought.Fettle.Three <- Fettle.UsersGrouped[(Fettle.UsersGrouped$How.Many.Times<4 & Fettle.UsersGrouped$How.Many.Times>2),]
Bought.Fettle.FourPlus <- Fettle.UsersGrouped[(Fettle.UsersGrouped$How.Many.Times>3),]
# number of columns and rows on dataframes----
length(FettleDispatched)
nrow(FettleDispatched)
nrow(Fettle.Users)
nrow(Bought.Fettle.Once)
nrow(Bought.Fettle.Twice)
nrow(Bought.Fettle.Three)
nrow(Bought.Fettle.FourPlus)

# Fran REPEAT USERS COC Fettle----
FettleCOC <- ContCOC[ ,c('SH.24.UID','Customer.ID',"Region","Created.at","Created.at.month.year","Dispatched.at")]
# order files in chronological date, use FormatDate (class = Date)
FettleCOC <- FettleCOC[(order(as.Date(FettleCOC$Created.at))),]
# Subset for Fettle and dispatched orders (dispatched different than blank)
FettleDispatchedCOC <- FettleCOC[(FettleCOC$Region == "Fettle" & FettleCOC$Dispatched.at != ''),]
# convert Created.at into date to be able to select by date

# # run ONLY if need data only to a certain date----
# class(FettleDispatchedCOC$Created.at)
# FettleDispatchedCOC$Created.at <- as.Date(FettleDispatchedCOC$Created.at, format = "%Y-%m-%d")
# ## run only if need data to a certain date: 
# FettleDispatchedCOC <- FettleDispatchedCOC[(FettleDispatchedCOC$Created.at < "2020-08-31"),]
# # END run only if need data only to a certain date----


# new data frame with unique (no duplicate) customer IDs
FettleCOC.Users = FettleDispatchedCOC[!duplicated(FettleDispatchedCOC$Customer.ID),]
# create data frame with Customer.ID grouped (i.e. how many times each Customer.ID shows up) - need package dplyr
FettleCOC.UsersGrouped <- FettleDispatchedCOC %>% group_by(Customer.ID) %>% summarise(How.Many.Times=n())
# count frequency: those who bought less than twice (= they bought once)
Bought.COC.Once <- FettleCOC.UsersGrouped[(FettleCOC.UsersGrouped$How.Many.Times<2),]
Bought.COC.Twice <- FettleCOC.UsersGrouped[(FettleCOC.UsersGrouped$How.Many.Times<3 & FettleCOC.UsersGrouped$How.Many.Times>1),]
Bought.COC.Three <- FettleCOC.UsersGrouped[(FettleCOC.UsersGrouped$How.Many.Times<4 & FettleCOC.UsersGrouped$How.Many.Times>2),]
Bought.COC.FourPlus <- FettleCOC.UsersGrouped[(FettleCOC.UsersGrouped$How.Many.Times>3),]
# number of columns and rows on dataframes----
nrow(FettleDispatchedCOC)
nrow(FettleCOC.Users)
nrow(Bought.COC.Once)
nrow(Bought.COC.Twice)
nrow(Bought.COC.Three)
nrow(Bought.COC.FourPlus)
# END Fran REPEAT USERS COC Fettle----

# Fran REPEAT USERS POP Fettle----
FettlePOP <- ContPOP[ ,c('Customer.ID',"Region","Created.at","Created.at.month.year","Dispatched.at")]
# order files in chronological date, use FormatDate (class = Date)
FettlePOP <- FettlePOP[(order(as.Date(FettlePOP$Created.at))),]
# Subset for Fettle and dispatched orders (dispatched different than blank)
FettleDispatchedPOP <- FettlePOP[(FettlePOP$Region == "Fettle" & FettlePOP$Dispatched.at != ''),]
# convert Created.at into date to be able to select by date

# # run ONLY if need data only to a certain date----
# class(FettleDispatchedPOP$Created.at)
# FettleDispatchedPOP$Created.at <- as.Date(FettleDispatchedPOP$Created.at, format = "%Y-%m-%d")
# FettleDispatchedPOP <- FettleDispatchedPOP[(FettleDispatchedPOP$Created.at < "2020-08-31"),]
# # END run only if need data only to a certain date----

# new data frame with unique (no duplicate) customer IDs
FettlePOP.Users = FettleDispatchedPOP[!duplicated(FettleDispatchedPOP$Customer.ID),]
# create data frame with Customer.ID grouped (i.e. how many times each Customer.ID shows up) - need package dplyr
FettlePOP.UsersGrouped <- FettleDispatchedPOP %>% group_by(Customer.ID) %>% summarise(How.Many.Times=n())
# count frequency: those who bought less than twice (= they bought once)
Bought.POP.Once <- FettlePOP.UsersGrouped[(FettlePOP.UsersGrouped$How.Many.Times<2),]
Bought.POP.Twice <- FettlePOP.UsersGrouped[(FettlePOP.UsersGrouped$How.Many.Times<3 & FettlePOP.UsersGrouped$How.Many.Times>1),]
Bought.POP.Three <- FettlePOP.UsersGrouped[(FettlePOP.UsersGrouped$How.Many.Times<4 & FettlePOP.UsersGrouped$How.Many.Times>2),]
Bought.POP.FourPlus <- FettlePOP.UsersGrouped[(FettlePOP.UsersGrouped$How.Many.Times>3),]

# number of columns and rows on dataframes----
nrow(FettleDispatchedPOP)
nrow(FettlePOP.Users)
nrow(Bought.POP.Once)
nrow(Bought.POP.Twice)
nrow(Bought.POP.Three)
nrow(Bought.POP.FourPlus)
# END Fran REPEAT USERS POP Fettle----

# Fran REPEAT USERS EC Fettle----
FettleECNow <- FranECNow[ ,c('customer_id',"Region","Created.at","Dispatched.at")]
FettleECFuture <- FranECFut[ ,c('customer_id',"Region","Created.at","Dispatched.at")]
# stack Now and Future into one EC file with 'rbind'. Column names have to be identical. 
FettleEC <- rbind(FettleECNow,FettleECFuture)
# order files in chronological date, use FormatDate (class = Date)
FettleEC <- FettleEC[(order(as.Date(FettleEC$Created.at))),]
# Subset for Fettle and dispatched orders (dispatched different than blank)
FettleDispatchedEC <- FettleEC[(FettleEC$Region == "Fettle" & FettleEC$Dispatched.at != ''),]
# unique (no duplicate) customer IDs
FettleEC.Users = FettleDispatchedEC[!duplicated(FettleDispatchedEC$customer_id),]
# create data frame with Customer.ID grouped (i.e. how many times each Customer.ID shows up) - need package dplyr
FettleEC.UsersGrouped <- FettleDispatchedEC %>% group_by(customer_id) %>% summarise(How.Many.Times=n())
# count frequency: those who bought less than twice (= they bought once)
Bought.EC.Once <- FettleEC.UsersGrouped[(FettleEC.UsersGrouped$How.Many.Times<2),]
Bought.EC.Twice <- FettleEC.UsersGrouped[(FettleEC.UsersGrouped$How.Many.Times<3 & FettleEC.UsersGrouped$How.Many.Times>1),]
Bought.EC.Three <- FettleEC.UsersGrouped[(FettleEC.UsersGrouped$How.Many.Times<4 & FettleEC.UsersGrouped$How.Many.Times>2),]
Bought.EC.FourPlus <- FettleEC.UsersGrouped[(FettleEC.UsersGrouped$How.Many.Times>3),]
# number of columns and rows on dataframes----
nrow(FettleDispatchedEC)
nrow(FettleEC.Users)
nrow(Bought.EC.Once)
nrow(Bought.EC.Twice)
nrow(Bought.EC.Three)
nrow(Bought.EC.FourPlus)
# END REPEAT USERS EC Fettle----


# Blake PrEP impact trial 19.01.2021 ----
PrepTrial <- orders[ ,c("PrEP.impact.trial.number",'Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                          "Sites.tested",'Test.regime','Clinic.visited','Clinic.visited.12','Attended.clinic',
                          "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Lab.receipt.at","Notified.at","Notified.at.month.year",
                          "Lab.results.at","Lab.results.at.month.year","Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis",'Syphilis','HIV',"Chlamydia","Gonorrhoea","Hep.B","Hep.C",
                          "Test.for.Chlamydia.Urine","Test.for.Chlamydia.Oral","Test.for.Chlamydia.Rectal","Test.for.Chlamydia.Vaginal",
                          "Test.for.Gonorrhoea.Urine","Test.for.Gonorrhoea.Oral","Test.for.Gonorrhoea.Rectal","Test.for.Gonorrhoea.Vaginal")]

# Subset using 'grep' and 'or' | (if 'Default LA' includes the string 'freetesting', or 'phe' or prep')
PrepTrial1 <- PrepTrial[grep('Freetesting|PHE|PrEP', PrepTrial$Default.LA),]
# users who belong to SH:24 areas (no freetesting or PrEP) and complete their Impact.trial.number
PrepTrial2 <- PrepTrial[(PrepTrial$PrEP.impact.trial.number!=""),]
# include UppeTier Name with 'merge' getting all the observations from the data set on the left (all.x = TRUE)
PrepTrial3 <- merge(PrepTrial2, LSOA.UpperTier[,c('LSOA11NM',"UTLA18NM")], by.x = "LSOA.name", by.y = "LSOA11NM", all.x = TRUE)
PrepTrial3$LSOA.name = NULL
write.table (PrepTrial3, file="\\Users\\Elena Ardines\\Documents\\Reports\\1. Monthly Reports\\Invoicing\\2020\\2020 12\\Backing data\\20210119.PrepTrial.csv", row.names=F, sep=",")
# END PrEP impact trial 19.01.2021 ----

# 1 million kits posted----
OrdersPosted <- orders[(orders$Dispatched.at != ""),]
nrow(OrdersPosted)
table(OrdersPosted$Test.for.Chlamydia.Vaginal != "not_requested", useNA = "always")
table(OrdersPosted$Test.for.Chlamydia.Oral != "not_requested", useNA = "always")
table(OrdersPosted$Test.for.Chlamydia.Rectal != "not_requested", useNA = "always")
table(OrdersPosted$Test.for.Chlamydia.Urine != "not_requested", useNA = "always")
table(OrdersPosted$Test.for.Hiv != "not_requested", useNA = "always")

table(OrdersPosted$Chlamydia== "positive", useNA = "always")
table(OrdersPosted$Gonorrhoea== "positive", useNA = "always")
table(OrdersPosted$HIV== "reactive", useNA = "always")
table(OrdersPosted$Syphilis== "reactive", useNA = "always")
table(OrdersPosted$Hep.B== "positive", useNA = "always")
table(OrdersPosted$Hep.C== "positive", useNA = "always")
# END 1 million kits posted----

# insufficient and haemolised blood samples----
InsuffHaem <- orders[(orders$Site=="Garden Clinic"),c("SH24.UID",'Customer.ID','LA.of.residence',"LSOA.name","Sites.tested","Test.regime",'Site','Age','Gender'
                                                   ,"Sexual.preference","Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C","Charge.token","Feedback.token","Discount.code",
                                                   "Requested.time","Created.at.month.year","Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year")]

InsuffHaem <- InsuffHaem [((InsuffHaem$HIV=="haemolysed"|InsuffHaem$HIV=="insufficient") & (grepl('2019', InsuffHaem$Notified.at))),]

InsuffHaem2019 <- grepl('2019', InsuffHaem$Notified.at)
i1 <- grepl('Bill Payment', df1$Product) & grepl('Mobile', df1$Product)

plot(InsuffHaem$HIV=="haemolysed", InsuffHaem$HIV=="insufficient")
# End insufficient and haemolised----

# Justin_Gurdees_Worcestershire CT/GC----
Gurdees <- orders[ ,c('SH24.UID','Customer.ID','LSOA.name','Default.LA','LA.of.residence','Site',
                      'Age','Gender','Ethnicity','Sexual.preference', "Sites.tested",'Test.regime',
                      "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Lab.receipt.at",
                      "Lab.results.at","Lab.results.at.month.year",
                      "Chlamydia","Gonorrhoea",'Syphilis','HIV','Test.for.Hiv','Test.for.Syphilis.EIA',"Test.1.for.Syphilis.RPR")]

Gurdees <- Gurdees [(Gurdees$Default.LA=='Worcestershire'),]
Gurdees$AgeSplit <- 0
Gurdees$AgeSplit = ifelse(Gurdees$Age>24,"25+","Under 25")
table(Gurdees$Dispatched.at.month.year)

Gurdees.UpperTier <- merge(Gurdees, LSOA.UpperTier[,c('LSOA11NM',"UTLA18NM",'lsoa')], by.x = "LSOA.name", by.y = "LSOA11NM")

write.table (Gurdees.UpperTier, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\Gurdees.UpperTier.LAs.202007.csv", row.names=F, sep=",")
# END Justin_Gurdees 2020_03_18 CT/GC----


# Cornwall data request 2020.06.29----
Cornwall <- OrdersToWork[,c('LSOA.name','Default.LA','LA.of.residence','Age','Gender',
                            "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year")]
Cornwall <- Cornwall [(Cornwall$Default.LA == "Cornwall and Isles of Scilly PCT"),]   

#Cornwall <- Cornwall [(Cornwall$Default.LA == "Cornwall and Isles of Scilly PCT" & 
#                          (Cornwall$Dispatched.at.month.year == "2019-12" | Cornwall$Dispatched.at.month.year == "2020-01" |
#                           Cornwall$Dispatched.at.month.year == "2020-02" | Cornwall$Dispatched.at.month.year == "2020-03" |
#                           Cornwall$Dispatched.at.month.year == "2020-04" | Cornwall$Dispatched.at.month.year == "2020-05")),]

Cornwall <- merge(Cornwall, LSOA[,c('LSOA11NM',"WD19NM")], by.x = "LSOA.name", by.y = "LSOA11NM")
#rename(new variable name = existing variable name) DPLYR package
Cornwall <- rename(Cornwall, Ward = WD19NM)
Cornwall$LSOA.name = NULL
write.table (Cornwall, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\202007_Cornwall.csv", row.names=F, sep=",")
# END Cornwall data request----


# Alice data request ----
OrdersAlice <- orders[ ,c('UUID','SH24.UID','Customer.ID',"LSOA.name",'Default.LA','LA.of.residence','Site',
                          'Age','Gender','Sexual.preference','Ethnicity',"Sexually.assaulted.risk.assessment","Unprotected.sex.in.last.3.days","Unprotected.sex.in.last.5.days",    
                          "Pressured.into.sex","Paid.for.sex","Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner",
                          "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year","Lab.results.at","Lab.results.at.month.year",
                          'Chlamydia','Gonorrhoea',"Syphilis","HIV","Hep.B","Hep.C","Sites.tested","Test.regime","Reason.for.visit","Clinic.visited","Clinic.visited.12")]

OrdersAliceNI <- OrdersAlice[(OrdersAlice$Default.LA == "Northern Ireland Belfast PCT" | OrdersAlice$Default.LA == "Northern Ireland Northern PCT" |
                                OrdersAlice$Default.LA == "Northern Ireland South Eastern PCT" | OrdersAlice$Default.LA == "Northern Ireland Southern PCT" |
                                OrdersAlice$Default.LA == "Northern Ireland Western PCT"), ]

OrdersAlice <- merge(OrdersAlice, LSOA[,c('LSOA11NM',"LAD18NM")], by.x = "LSOA.name", by.y = "LSOA11NM")

OrdersAlice$Created.at1 <- as.Date(OrdersAlice$Created.at, format = "%Y-%m-%d")
OrdersAlice1 <- OrdersAlice [(OrdersAlice$Created.at1 > "2019-09-30"),]
OrdersAlice2 <- OrdersAlice1 [(OrdersAlice1$Created.at1 < "2020-02-01"),]
write.table (OrdersAlice2, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020_03 OrdersAlice.csv", row.names=F, sep=",")
write.table (OrdersAliceNI, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020_03 OrdersAliceNI.csv", row.names=F, sep=",")
names(OrdersAlice2)
# END Alice data request ----

# Jen HIV reactives 2020.07.15----
ReactHIV <- orders[ ,c('SH24.UID','Customer.ID','Default.LA',"LSOA.name", "Lab","Reason.for.visit",
        "Age","Gender","Gender.at.birth","Sexual.preference","Ethnicity","Sexuality",
        "Sexually.assaulted.risk.assessment","Unprotected.sex.in.last.3.days","Unprotected.sex.in.last.5.days",
        "Pressured.into.sex","Paid.for.sex","Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner",
        "Clinic.visited","Clinic.visited.12","Attended.clinic","Sites.tested","Test.regime",
        "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year",
        "Lab.results.at","Lab.results.at.month.year","PrEP.impact.trial.number",'Syphilis','HIV',"Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis",
        "Test.for.Hiv","Test.1.for.Hiv")]

ReactHIV1 <- ReactHIV[grep('2018-12|2019|2020', ReactHIV$Lab.results.at.month.year),]
write.table (ReactHIV1, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020.07.16.ReactiveHIV_Jen.csv", row.names=F, sep=",")

names(orders$Previously.treated.for.Syphilis)
# END Jen HIV reactives----

# Ahimza Essex 2020.07.22----
# STI
AhimzaEssex <- orders[(orders$Default.LA=="Essex"),
                 c('SH24.UID','Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
               'Age','Gender','Sexual.preference','Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
               'Sexuality','Click.and.collect',"Sexually.assaulted.risk.assessment","Unprotected.sex.in.last.3.days","Unprotected.sex.in.last.5.days","Pressured.into.sex",                
               "Paid.for.sex","Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner",
                "Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis",'Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                "Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at", 
               "Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C")]

AhimzaEssex$Created.at1 <- as.Date(AhimzaEssex$Created.at, format = "%Y-%m-%d")
AhimzaEssexNov <- AhimzaEssex [(AhimzaEssex$Created.at1 > "2019-10-31"),]
# CT treatments
CT.TreatmentsAhimzaEssex <- TreatmentsMerge [(TreatmentsMerge$Area=="Essex")
                                             , c("SH24.UID",'LSOA.name','Site','Area',"Created.at","Offered.at","Prescription.at","Dispatched.at",'Age','Gender','Sexual.preference','Ethnicity')]
CT.TreatmentsAhimzaEssex$Created.at1 <- as.Date(CT.TreatmentsAhimzaEssex$Created.at, format = "%Y-%m-%d") 
CT.TreatmentsAhimzaEssexNov <- CT.TreatmentsAhimzaEssex [(CT.TreatmentsAhimzaEssex$Created.at1 > "2019-10-31"),]
# Contraception
COCAhimza <- ContCOC
COCAhimza$Created.at1 <- as.Date(COCAhimza$Created.at, format = "%Y-%m-%d")
COCAhimzaEssex <- COCAhimza[(COCAhimza$Region=="Essex" & COCAhimza$Created.at1 > "2020-01-31"),]

POPAhimza <- ContPOP
POPAhimza$Created.at1 <- as.Date(POPAhimza$Created.at, format = "%Y-%m-%d")
POPAhimzaEssex <- POPAhimza[(POPAhimza$Region=="Essex" & POPAhimza$Created.at1 > "2019-10-31"),]

write.table (AhimzaEssexNov, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020_08_18 STIAhimzaEssex.csv", row.names=F, sep=",")
write.table(CT.TreatmentsAhimzaEssexNov, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020_07 CTtreatment.AhimzaEssex.csv", row.names=F, sep=",")
write.table(COCAhimzaEssex, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020_09 COCAhimzaEssex.csv", row.names=F, sep=",")
write.table(POPAhimzaEssex, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2020_09 POPAhimzaEssex.csv", row.names=F, sep=",")
# END Ahimza Essex----


# Herts KPI's Blake 20200722----
HertsKPI <- orders[(orders$Default.LA=="Hertfordshire"),
                      c('SH24.UID','Customer.ID','Reason.for.visit','LSOA.name','Default.LA','LA.of.residence','Site',
                        'Age','Gender','Sexual.preference','Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
                        'Sexuality','Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',
                        "Created.at.month.year","Created.at","Dispatched.at","Lab.receipt.at","Notified.at","Lab.results.at",
                        "Syphilis","HIV","Chlamydia","Gonorrhoea")]

# convert dates from 'factor' to 'date' to operate
HertsKPI$Created_FormatDate <- as.Date(HertsKPI$Created.at, "%Y-%m-%d")
HertsKPI$Dispatched_FormatDate <- as.Date(HertsKPI$Dispatched.at, "%Y-%m-%d")
HertsKPI$Notified_FormatDate <- as.Date(HertsKPI$Notified.at, "%Y-%m-%d")
HertsKPI$LabReceipt_FormatDate <- as.Date(HertsKPI$Lab.receipt.at, "%Y-%m-%d")
HertsKPI$LabResults_FormatDate <- as.Date(HertsKPI$Lab.results.at, "%Y-%m-%d")

# extract date range and remove orders not dispatched
HertsKPI1 <- HertsKPI[((HertsKPI$Created.at.month.year == "2019-10" | HertsKPI$Created.at.month.year == "2019-11" | HertsKPI$Created.at.month.year == "2019-12")
                       & HertsKPI$Dispatched.at != ''),]

# create new variable: Percentage of kits packaged and posted to Service User within 3 working days of request
HertsKPI1$Delivered3d <- 0
HertsKPI1$Delivered3d <- (HertsKPI1$Dispatched_FormatDate) - (HertsKPI1$Created_FormatDate)
class(HertsKPI1$Delivered3d)
# group "within 3 days" or "not within 3 days"
HertsKPI1$Delivered3d.Group = ifelse(HertsKPI1$Delivered3d>3,"No","Yes, delivered within 3 days")
table(HertsKPI1$Delivered3d.Group)
prop.table(table(HertsKPI1$Delivered3d.Group))*100

# data frame with negative results and remove kits with no lab.receipt: Percentage of negative results communicated to User within 3 working days of receiving sample
HertsKPI2 <- HertsKPI[((HertsKPI1$Syphilis=="negative" | HertsKPI1$HIV=="negative" | HertsKPI1$Chlamydia=="negative" | HertsKPI1$Gonorrhoea=="negative") ),]
HertsKPI2 <- HertsKPI2[(HertsKPI2$Lab.receipt.at != '' | HertsKPI2$Notified.at != ''),]
  
HertsKPI2$NegatResults3d <- 0
HertsKPI2$NegatResults3d <-  (HertsKPI2$Notified_FormatDate) - HertsKPI2$LabReceipt_FormatDate
HertsKPI2$NegatResults3d.Group  = ifelse(HertsKPI2$NegatResults3d>3,"No","Yes, delivered within 3 days")
table(HertsKPI2$NegatResults3d.Group)
prop.table(table(HertsKPI2$NegatResults3d.Group))*100

# data frame with negative results and remove kits with no lab.receipt: Percentage of reactive/positive results communicated to Users in 5 days of receiving sample
HertsKPI3 <- HertsKPI[((HertsKPI1$Syphilis=="reactive" | HertsKPI1$HIV=="reactive" | HertsKPI1$Chlamydia=="positive" | HertsKPI1$Gonorrhoea=="positive") ),]
HertsKPI3 <- HertsKPI3[(HertsKPI3$Lab.receipt.at != '' | HertsKPI3$Notified.at != ''),]
HertsKPI3$ReactPositResults5d <- 0
HertsKPI3$NegatResults3d <-  (HertsKPI3$Notified_FormatDate) - HertsKPI3$LabReceipt_FormatDate
HertsKPI3$NegatResults3d.Group  = ifelse(HertsKPI3$NegatResults3d>5,"No","Yes, delivered within 3 days")
table(HertsKPI3$NegatResults3d.Group)
prop.table(table(HertsKPI3$NegatResults3d.Group))*100
# END Herts KPI's Blake 20200722


# Justin 20200724: STI data from april for Halton, Warrington and Chehsire East----
OrdersJustin <- orders
OrdersJustin <- orders[(orders$Default.LA == "Halton" | orders$Default.LA == "Warrington" | orders$Default.LA == "Cheshire East"),]

OrdersJustin$Created.at1 <- as.Date(OrdersJustin$Created.at, format = "%Y-%m-%d")
OrdersJustin <- OrdersJustin [(OrdersJustin$Created.at1 > "2020-03-31"),]

write.table (OrdersJustin, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\202007 Halton_Warrington_CheshireEast Justin.csv", row.names=F, sep=",")
# END Justin 20200724: STI data






# 25.08.2020 Dorset Justin----
Data_Dorset <- BackingMinimum [(BackingMinimum$Area=="Dorset"),]
Data_Dorset1 <- merge(Data_Dorset, LSOA[,c('LSOA11NM',"LSOA11CD")], by.x = "LSOA.name", by.y = "LSOA11NM")
Data_Dorset2 <- merge(Data_Dorset1, LSOA.UpperTier[,c('LSOA11NM',"UTLA18NM")], by.x = "LSOA.name", by.y = "LSOA11NM", all.x = TRUE)

write.table (Data_Dorset2, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\Data_Dorset2.csv", row.names=F, sep=",")

# END Dorset Justin----

# 2020.10.20 Feedback SH:24 STI Justin/Blake----
feedback = read.csv("20201020_Feedback_tokens.csv") # produce with 'feedback' query in DataGrip
rm(FeedbackMerge1)
FeedbackMerge <- merge(orders[,c('SH24.UID','Default.LA','Age','Gender',"Gender.at.birth","Genitals",
                                 "Gender.identity.same.as.birth.sex","Gender.Identity",'Sexual.preference','Ethnicity',"Lab.results.at.month.year")], 
                       feedback[,c('feedback_token',"sh24_uid")], by.x = "SH24.UID", by.y = "sh24_uid", all.y = TRUE)

FeedbackMerge$AgeSplit <- 0
FeedbackMerge$AgeSplit = ifelse(FeedbackMerge$Age>24,"25+","Under 25")

FeedbackMerge$EthnicityGroup <- "Non white"
FeedbackMerge$EthnicityGroup[FeedbackMerge$Ethnicity=='not_asked'] = "Not asked"
FeedbackMerge$EthnicityGroup[FeedbackMerge$Ethnicity=='not_known' | FeedbackMerge$Ethnicity=='prefer_not_to_say'] = "Not known/Prefer not to say"
FeedbackMerge$EthnicityGroup[FeedbackMerge$Ethnicity=='white_english_welsh_scottish_northern_irish_british' | FeedbackMerge$Ethnicity=='other_white' |
                             FeedbackMerge$Ethnicity=='irish' | FeedbackMerge$Ethnicity=='gypsy_or_irish_traveller'] = "White"

### FIX EXTRACT OF RELEVANT MONTH
FeedbackMerge1 <- FeedbackMerge[(FeedbackMerge$Lab.results.at.month.year=="2020-07"|FeedbackMerge$Lab.results.at.month.year=="2020-08"|FeedbackMerge$Lab.results.at.month.year=="2020-09")
                   ,c('feedback_token','SH24.UID',"Default.LA","Age",'"AgeSplit')]


write.table (FeedbackMerge, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\FeedbackMerge.csv", row.names=F, sep=",")
# END 2020.10.20 Feedback SH:24 STI ----



# 2021.04.15 Amber - include feedback AND THEN REMOVE ALL UIDS----
#recode Area in 'orders' data frame. Use 'orders' instead of 'OrdersToWork' cause it has all the variables requested
#run "recode Area" function and save output in Area; important to write your variable names in colons (but not the data frame name)
orders1 <- orders
orders1$Area <- 0
orders1$Area <- recodeArea(DF=OrdersToWork,varname="Area",varname2="Site",varname3 = "LA.of.residence", varname4="Referred.from",varname5="Default.LA")

Data_Ireland <- orders1[,c("Area","Feedback.token","New.or.follow.up",'Reason.for.visit','Postcode','LSOA.name','Default.LA','LA.of.residence','Site',
             'Age','Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",'Sexual.preference','Sexually.assaulted.risk.assessment','Unprotected.sex.in.last.5.days',
             'Pressured.into.sex','Paid.for.sex','Drink.or.Drugs','Depression.or.low.mood','Older.or.younger.partner',
             'Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity','Sexuality',
             'Click.and.collect','Referred.from','Referred.to','Referred.via',"Sites.tested",'Test.regime',"Test.kit.code","Test.kit.color",
             "Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis",
             'Syphilis','HIV','Chlamydia','Gonorrhoea',"Hep.B","Hep.C","Test.for.Hiv","Test.for.Syphilis.EIA",
             "Test.for.Chlamydia.Urine","Test.for.Chlamydia.Oral","Test.for.Chlamydia.Rectal","Test.for.Chlamydia.Vaginal",
             "Test.for.Gonorrhoea.Urine","Test.for.Gonorrhoea.Oral","Test.for.Gonorrhoea.Rectal","Test.for.Gonorrhoea.Vaginal",
             "Test.for.Hepatitis.B","Test.for.Hepatitis.C","Test.for.Syphilis.RPR","Test.1.for.Hiv","Test.1.for.Syphilis.EIA",
             "Test.1.for.Chlamydia.Urine","Test.1.for.Chlamydia.Oral","Test.1.for.Chlamydia.Rectal","Test.1.for.Chlamydia.Vaginal",
             "Test.1.for.Gonorrhoea.Urine","Test.1.for.Gonorrhoea.Oral","Test.1.for.Gonorrhoea.Rectal","Test.1.for.Gonorrhoea.Vaginal",
             "Test.1.for.Hepatitis.B","Test.1.for.Hepatitis.C","Test.1.for.Syphilis.RPR")]

Data_Ireland <-  Data_Ireland [(Data_Ireland$Default.LA=="Ireland - Dublin" 
                                | Data_Ireland$Default.LA=="Ireland - Cork" 
                                | Data_Ireland$Default.LA=="Ireland - Kerry"),]

#download feedback from Typform. Merge with data set
feedbackIreland = read.csv("20210415_SH24_Ireland_STI_feedback.csv")
Data_IrelandMerge <- merge(Data_Ireland, feedbackIreland, by.x = "Feedback.token", by.y = "uid", all.x = TRUE)
#remove uids and variables not needed
Data_IrelandMerge$Feedback.token = NULL 
Data_IrelandMerge$X. = NULL
Data_IrelandMerge$uid = NULL
Data_IrelandMerge$Network.ID = NULL
Data_IrelandMerge$Start.Date..UTC. = NULL
Data_IrelandMerge$Submit.Date..UTC. = NULL
write.table (Data_IrelandMerge, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2021.04.15_Amber_Ireland_STI.csv", row.names=F, sep=",")
# END Ireland Amber----

# 2021.04.25 Anamika Syphilis and HIV----
OrdersAnamika <- orders[ ,c('SH24.UID',"PrEP.impact.trial.number",'Default.LA','Site',
       'Age','Gender',"Gender.at.birth","Genitals","Gender.Identity","Gender.identity.same.as.birth.sex",
       'Sexual.preference','Sexuality',"Unprotected.sex.in.last.5.days",
       "Clinic.visited","Clinic.visited.12","Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis",
       "Sites.tested","Test.regime",
       "Created.at",'Created.at.month.year',"Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year","Lab.results.at","Lab.results.at.month.year",
       "HIV","Syphilis","Test.for.Hiv","Test.for.Syphilis.EIA","Test.for.Syphilis.RPR")]

# Period: 01/05/2019 to 31/08/2019, and 01/05/2020 to 31/08/2020
# Only SH:24 and freetesting
OrdersAnamika <- OrdersAnamika [((OrdersAnamika$Created.at.month.year=="2019-05"| OrdersAnamika$Created.at.month.year=="2019-06"|
                                  OrdersAnamika$Created.at.month.year=="2019-07"| OrdersAnamika$Created.at.month.year=="2019-08"|
                                  OrdersAnamika$Created.at.month.year=="2020-05"| OrdersAnamika$Created.at.month.year=="2020-06"|  
                                  OrdersAnamika$Created.at.month.year=="2020-07"| OrdersAnamika$Created.at.month.year=="2020-08")
                                  & (OrdersAnamika$Default.LA != "Fettle") & (OrdersAnamika$Default.LA != "Romania") 
                                  & (OrdersAnamika$Lab.results.at != "N/A")),]
# exclude tests with no Syphilis 
OrdersAnamika1 <- OrdersAnamika [(OrdersAnamika$Syphilis != "not_requested" & OrdersAnamika$Syphilis != "missing" ),]
table(OrdersAnamika1$Syphilis)
table(OrdersAnamika1$HIV)

# change PrEP.impact.trial.number for YES/NO
OrdersAnamika1$PrEP.impact.trial = ifelse(OrdersAnamika1$PrEP.impact.trial.number != "", "There is a PrEP Impact Trial number","No number")
table(OrdersAnamika1$PrEP.impact.trial)
#remove PrEP.impact.trial.number - it's PID ! !
OrdersAnamika1$PrEP.impact.trial.number = NULL

# encrypt SH24 number
x <- "Encrypted_uid_"
OrdersAnamika1$Encrypted_SH24 <- sprintf("%s%0*d", x, 25 - nchar(x), 1:nrow(OrdersAnamika1))
# order columns
OrdersAnamika1 <- OrdersAnamika1 [c("SH24.UID","Encrypted_SH24","Default.LA","Site",
                  "Age","Gender","Gender.at.birth","Genitals","Gender.Identity","Gender.identity.same.as.birth.sex","Sexual.preference","Sexuality",
                  "Unprotected.sex.in.last.5.days","Clinic.visited","Clinic.visited.12",
                  "Previously.diagnosed.with.HIV","Previously.treated.for.Syphilis","Sites.tested","Test.regime",
                  "Created.at","Created.at.month.year","Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year",
                  "Lab.results.at","Lab.results.at.month.year",
                  "HIV","Syphilis","Test.for.Hiv","Test.for.Syphilis.EIA","Test.for.Syphilis.RPR","PrEP.impact.trial")]

write.table (OrdersAnamika1, file="\\Users\\Elena Ardines\\Documents\\Reports\\2. Ad-hoc reports\\2021.04_Syphilis_Anamika.csv", row.names=F, sep=",")
# END Anamika----

#20.12.2021 Fran Bury: all orders for all products from under 18s, safeguarding flags----
# upload files from Backing_Data tab
BurySTI <- orders [(orders$Age<18),(c('Default.LA',"Created.at","Created.at.month.year","Dispatched.at",'Dispatched.at.month.year',
           "Lab.results.at","Lab.results.at.month.year",
           "Age","Gender","Genitals","Ethnicity","Sexual.preference","Sexuality","Sites.tested",
           "Unprotected.sex.in.last.5.days","Sexually.assaulted.risk.assessment",
           "Pressured.into.sex","Paid.for.sex","Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner",
           "Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C"))]

BuryCOC <- ContCOC[(ContCOC$Age<18),(c("Region","Created.at","Created.at.month.year",
                "Prescription.at","Prescription.at.month.year","Dispatched.at","Dispatched.at.month.year",
                "Age","Ethnicity","Sexuality","Risk.of.pregnancy",
                "Sexually.assaulted","Pressured","Bribed","Inebriated","Depression","Older.or.younger.partner"))]

BuryPOP <- ContPOP[(ContPOP$Age<18),(c("Region","Created.at","Created.at.month.year",
               "Prescription.at","Prescription.at.month.year","Dispatched.at","Dispatched.at.month.year",
               "Age","Ethnicity","Sexuality","Risk.of.pregnancy",
               "Sexually.assaulted","Pressured","Bribed","Inebriated","Depression","Older.or.younger.partner"))]
BuryCOC <- rename(BuryCOC, Unprotected.sex.Risk.pregnancy = Risk.of.pregnancy)
BuryPOP <- rename(BuryPOP, Unprotected.sex.Risk.pregnancy = Risk.of.pregnancy)

# include sexuality in the sql query
ECNowBury = read.csv("20201220_FranBury_EC.csv")
BuryECNow <- ECNowBury[(ECNowBury$Age<18) , (c("Region","Created.at","Created.at.month.year",
                "Dispatched.at","Dispatched.at.month.year","prescription_day",
                "Age","Ethnicity","Sexuality",
                "Sexually.assaulted","Pressured.into.sex","Paid.for.sex",
                "Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner"))]

Patch = read.csv("FranBury_Patch.csv")
BuryPatch <- Patch[(Patch$Age<18) , (c("region","Created.day","Created.Month.Year",
              "Dispatched.day","Dispatched.Month.Year",
               "Age","Ethnicity","Sexuality","Unprotected.sex",
               "Sexually.assaulted",'Bribed',"Inebriated","Depression","Pressured","Older.or.younger.partner"))]

Ring = read.csv("FranBury_Ring.csv")
BuryRing <- Ring[(Ring$Age<18) , (c("region","Created.day","Created.Month.Year",
                                       "Dispatched.day","Dispatched.Month.Year",
                                       "Age","Ethnicity","Sexuality","Unprotected.sex",
                                       "Sexually.assaulted",'Bribed',"Inebriated","Depression","Pressured","Older.or.younger.partner"))]

Injectable = read.csv("Injectable_Test_v1.csv")
BuryInjectable <- Injectable[(Injectable$Age<18),(c("region","Created.day","Created.Month.Year",
                             "Dispatched.day","Dispatched.Month.Year",
                             "Age","Ethnicity","Sexuality","Unprotected.sex",
                             "Sexually.assaulted",'Bribed',"Inebriated","Depression","Pressured","Older.or.younger.partner"))]

write.table (BurySTI, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_STI.csv", row.names=F, sep=",")
write.table (BuryCOC, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_COC.csv", row.names=F, sep=",")
write.table (BuryPOP, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_POP.csv", row.names=F, sep=",")
write.table (BuryECNow, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_ECNow.csv", row.names=F, sep=",")
write.table (BuryPatch, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_Patch.csv", row.names=F, sep=",")
write.table (BuryRing, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_Ring.csv", row.names=F, sep=",")
write.table (BuryInjectable, file="/Users/ElenaArdines1/Documents/Reports/2. Ad-hoc reports/PublicRegistrars/2021.12.20.FranB_Injectable.csv", row.names=F, sep=",")
# END Fran Bury

# EXPERIMENT Invoicing with testing service----
orders = read.csv("20220103_StiOrders.csv")
inv = read.csv("Invoic_Test.csv")
table(inv$overall_type)

Dispatched <- inv[(inv$overall_type=="kits_sent"),]
Returns <- inv[(inv$overall_type=="kits_tested"),]
table (Dispatched$processed_month_year, Dispatched$repeat_kit, useNA = "always")
table (Returns$processed_month_year, Returns$repeat_kit, useNA = "always")

ggplot(Dispatched) + geom_bar(mapping = aes(x = processed_month_year, fill = repeat_kit))

# Assign character values to testing_service
Dispatched$service <- ""
Dispatched$service [Dispatched$testing_service=="0" ] <- "SH:24"
Dispatched$service [Dispatched$testing_service=="1" ] <- "Fettle"
Dispatched$service [Dispatched$testing_service=="3" ] <- "Romania"
Dispatched$service [Dispatched$testing_service=="4" ] <- "freetesting"
Dispatched$service [Dispatched$testing_service=="5" ] <- "Ireland"
# Assign character values to repeat_kit
Dispatched$rep <- ""
Dispatched$rep [Dispatched$repeat_kit=="0" ] <- "original kit"
Dispatched$rep [Dispatched$repeat_kit=="1" ] <- "repeat kit"

ggplot(Dispatched, aes(x = processed_month_year, fill = rep)) + geom_bar()
ggplot(Dispatched, aes(x = processed_month_year, fill = service)) + geom_bar()
ggplot(Dispatched) + geom_bar(mapping = aes(x = testing_service))

table(Dispatched$service,Dispatched$processed_month_year)
# END Invoicing with testing service----



# Georgia request 2022.04.06 ----
# contraception and STI data 
# data only for orders created Feb and March 2022
STI <- orders
class(STI$Created.at)
STI$Created.at <- as.Date(STI$Created.at, format = "%Y-%m-%d")
STI <- STI[(STI$Created.at > "2022-01-31" & STI$Created.at < "2022-04-01"),]
# check data is only for Feb, Mar
table(STI$Created.at.month.year)
# remove uids
names(STI)
STI$id = NULL
STI$Lab.UID = NULL
STI$SH24.UID = NULL
STI$Feedback.token = NULL
STI$Customer.ID = NULL

COC <- ContCOC
class(COC$Created.at)
COC$Created.at <- as.Date(COC$Created.at, format = "%Y-%m-%d")
COC <- COC[(COC$Created.at > "2022-01-31" & COC$Created.at < "2022-04-01"),]
table(COC$Created.at.month.year)
names(COC)
COC$Feedback.token = NULL
COC$SH.24.UID = NULL
COC$ID = NULL
COC$Customer.ID = NULL


POP <- ContPOP
class(POP$Created.at)
POP$Created.at <- as.Date(POP$Created.at, format = "%Y-%m-%d")
POP <- POP[(POP$Created.at > "2022-01-31" & POP$Created.at < "2022-04-01"),]
table(POP$Created.at.month.year)
names(POP)
POP$Feedback.token = NULL
POP$SH.24.UID = NULL
POP$ID = NULL
POP$Customer.ID = NULL


EC <- ECNow
class(EC$Created.at)
EC$Created.at <- as.Date(EC$Created.at, format = "%Y-%m-%d")
EC <- EC[(EC$Created.at > "2022-01-31" & EC$Created.at < "2022-04-01"),]
table(EC$Created.at.month.year)



write.table (STI, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.04.06_STI_Feb_Mar.csv", row.names=F, sep=",")




# END Georgia request 2022.04.06



# Build a Model----
Model <- OrdersToWork %>%
  select(SH24.UID, Created.at, Area)

####################################################################################



# list of packages we need
list.of.packages <- c("DBI", "odbc", "config", "prophet", "lubridate", "tidyverse", "forecast")      

# check if any of these packages are not already installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

## Packages to 
#installed on 14.03.2021: 
install.packages('DBI') 
#installed on 14.03.2021: 
install.packages('odbc')

install.packages('tidyverse')


#installed on 12.03.2021: 
install.packages('prophet')
#installed on 12.03.2021: 
install.packages('lubridate')
#installed on 12.03.2021: 
install.packages('forecast')

#installed on 14.03.2021 - disn't work. Tried this and didn't work:
#cat(".Rprofile: Setting R repository:")
#repo = getOption("repos") 
## set up the server from which you will download the package.
#repo["CRAN"] = "http://cran.case.edu" 
#options(repos = repo)
#rm(repo)


install.packages("tidyverse",dependencies=TRUE)





# libraries for database connection
library(DBI)
library(odbc)
library(config)

# all other libraries for this notebook
library(prophet)
library(lubridate)
library(tidyverse)
library(forecast)


#run the function library() without arguments, to get the list of packages installed in different libraries on your computer
library()





