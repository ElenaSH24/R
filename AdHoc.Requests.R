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
write.table (DataStackFettle, file="~/Reports/2. Ad-hoc reports/2022.08.18.DataStack_Fettle.csv", row.names=F, sep=",")


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

# adjust data to orders created by end of a given month
v1 <- '2022-03-31'
class(OrdersRepeat$Created.at)
OrdersRepeat$Created.at <- as.Date(OrdersRepeat$Created.at, format = "%Y-%m-%d")
# extract data up to the end of the relevant month
OrdersRepeat <- OrdersRepeat[(OrdersRepeat$Created.at <= v1),]


# Subset for Fettle and dispatched orders (dispatched different than blank)
FettleDispatched <- OrdersRepeat[(OrdersRepeat$Site == "Fettle Hub" & OrdersRepeat$Dispatched.at != ''),]
# new data frame with unique (no duplicate) customer IDs
Fettle.Users = FettleDispatched[!duplicated(FettleDispatched$Customer.ID),]
# create data frame with Customer.ID grouped (i.e. how many times each Customer.ID shows up) 
# load dplyr for function %>%
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

# convert Created.at into date to be able to select by date, if need data up to a certain date----
class(FettleDispatchedCOC$Created.at)
FettleDispatchedCOC$Created.at <- as.Date(FettleDispatchedCOC$Created.at, format = "%Y-%m-%d")
FettleDispatchedCOC <- FettleDispatchedCOC[(FettleDispatchedCOC$Created.at <= v1),]
# END if need data up to a certain date----


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

# convert Created.at into date to be able to select by date, if need data up to a certain date----
class(FettleDispatchedPOP$Created.at)
FettleDispatchedPOP$Created.at <- as.Date(FettleDispatchedPOP$Created.at, format = "%Y-%m-%d")
FettleDispatchedPOP <- FettleDispatchedPOP[(FettleDispatchedPOP$Created.at <= v1),]
# END if need data up to a certain date----

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

# convert Created.at into date to be able to select by date, if need data up to a certain date----
class(FettleDispatchedEC$Created.at)
FettleDispatchedEC$Created.at <- as.Date(FettleDispatchedEC$Created.at, format = "%Y-%m-%d")
FettleDispatchedEC <- FettleDispatchedEC[(FettleDispatchedEC$Created.at <= v1),]
# END if need data up to a certain date----

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





# insufficient and haemolised blood samples----
InsuffHaem <- orders[(orders$Site=="Garden Clinic"),c("SH24.UID",'Customer.ID','LA.of.residence',"LSOA.name","Sites.tested","Test.regime",'Site','Age','Gender'
                                                      ,"Sexual.preference","Syphilis","HIV","Chlamydia","Gonorrhoea","Hep.B","Hep.C","Charge.token","Feedback.token","Discount.code",
                                                      "Requested.time","Created.at.month.year","Dispatched.at","Dispatched.at.month.year","Notified.at","Notified.at.month.year")]

InsuffHaem <- InsuffHaem [((InsuffHaem$HIV=="haemolysed"|InsuffHaem$HIV=="insufficient") & (grepl('2019', InsuffHaem$Notified.at))),]

InsuffHaem2019 <- grepl('2019', InsuffHaem$Notified.at)
i1 <- grepl('Bill Payment', df1$Product) & grepl('Mobile', df1$Product)

plot(InsuffHaem$HIV=="haemolysed", InsuffHaem$HIV=="insufficient")
# End insufficient and haemolised----



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
names(EC)
EC$customer_id = NULL
EC$sh24_uid = NULL



write.table (STI, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.04.06_Georgia_STI_Feb_Mar.csv", row.names=F, sep=",")
write.table (COC, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.04.06_Georgia_COC_Feb_Mar.csv", row.names=F, sep=",")
write.table (POP, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.04.06_Georgia_POP_Feb_Mar.csv", row.names=F, sep=",")
write.table (EC, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.04.06_Georgia_EC_Feb_Mar.csv", row.names=F, sep=",")
# END Georgia request 2022.04.06


# 2022.04.28 Graham messages per customer (conversation)
setwd("/Users/ElenaArdinesTomas/Documents/Reports/2.Ad-hoc-reports")
mssg <- read.csv("20220428_MessagesPerConversation.csv")

# 2022.05.17 Abi CT diagnosis and treatments period 15/7/21 to 14/1/22
setwd("/Users/ElenaArdinesTomas/Documents/Reports/1.Monthly_Reports/Performance_Reports/2022/2022_04")
orders = read.csv("20220502_sti_order_report.csv")
Treatments = read.csv("20220503_CT_Treatments_signed.csv")

Abi <- orders
# extract data for CT testing and tests that have been returned
Abi <- Abi[(Abi$Chlamydia != 'not_requested' & Abi$Lab.results.at != ""), 
           c("SH24.UID",'Default.LA','Age','Gender','Gender.at.birth','Genitals','Gender.identity.same.as.birth.sex','Gender.Identity',
           'Sexual.preference','Sexuality','Clinic.visited','Clinic.visited.12','Attended.clinic','Ethnicity',
           'Sites.tested','Created.at',"Created.at","Created.at.month.year","Dispatched.at","Dispatched.at.month.year",
           "Lab.results.at","Lab.results.at.month.year","Chlamydia",
           "Test.for.Chlamydia.Urine","Test.for.Chlamydia.Oral","Test.for.Chlamydia.Rectal","Test.for.Chlamydia.Vaginal")]

# extract results for given period
class(Abi$Lab.results.at)
Abi$Lab.results.at <- as.Date(Abi$Lab.results.at, format = "%Y-%m-%d")

table(Abi$Lab.results.at.month.year)
Abi <- Abi[(Abi$Lab.results.at >= "2021-07-15" & Abi$Lab.results.at <= "2022-01-14"),]

# get CT treatments
Abi_1 <- merge(Abi, Treatments[,c('sh24_uid',"dispatched_at")], by.x = "SH24.UID", by.y = "sh24_uid", all.x = TRUE)
Abi_1 <- rename(Abi_1, Region = Default.LA)
Abi_1 <- rename(Abi_1, Treatment_dispatched_at = dispatched_at)
# remove UIDs
Abi_1$SH24.UID = NULL

table(Abi_1$Chlamydia)
write.table(Abi_1, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\\\PublicRegistrars\\20220517_Abi_CT_Diagnosis_Treatments.csv", row.names=F, sep=",")
# end Abi CT



# Build a Model----
Model <- OrdersToWork %>%
  select(SH24.UID, Created.at, Area)

####################################################################################



install.packages('tidyverse')


# 2022.06.12 Freetesting - Suffolk MSM
Suf <- orders[(orders$Default.LA=='Freetesting - Suffolk'),]
table(Suf$Dispatched.at.month.year)
table(Suf$Gender,Suf$Sexual.preference)
table(Suf$Gender.Identity,Suf$Sexual.preference)
table(Suf$Genitals,Suf$Sexual.preference)

write.table (Suf, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.06.12.Suffolk.csv", row.names=F, sep=",")


# 2022.06.15 check if LA of Residence is being populated in May 2022
residence <- orders[(orders$Dispatched.at.month.year == "2022-05"),]

hill <- orders[(orders$Dispatched.at.month.year == "2022-04" & 
                   orders$Default.LA == "Hillingdon"),]
table(hill$LA.of.residence)


# 2022.06.20 HIV reactives from Manchester, Salford, Bolton, Trafford, Stockport, Tameside, Oldham, Rochdale, Bury & Wigan
# Dates: 2019 - End Sept 2021
Stuart <- orders[(orders$Default.LA == "Freetesting - Bolton" | orders$Default.LA == "Freetesting - Bury" |
                    orders$Default.LA == "Freetesting - Manchester" |orders$Default.LA == "Freetesting - Oldham" |
                    orders$Default.LA == "Freetesting - Rochdale" |orders$Default.LA == "Freetesting - Salford" |
                    orders$Default.LA == "Freetesting - Stockport" |orders$Default.LA == "Freetesting - Tameside" |
                    orders$Default.LA == "Freetesting - Trafford" |orders$Default.LA == "Freetesting - Wigan" ),]


write.table (Stuart, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\2.Ad-hoc-reports\\2022.06.20_freetesting_Stuart.csv", row.names=F, sep=",")


# 2022.08.25 Blake - freetesting-Hertfordshire offline kits
ordersFreeHertsOffline <- orders[(orders$Default.LA=='Freetesting - Hertfordshire' & orders$Distribution.method=='offline_kits'),]
table(ordersFreeHertsOffline$Dispatched.at.month.year)


#########################
install.packages("tidyverse",dependencies=TRUE)
