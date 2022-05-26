# Hertfordshire quarterly KPIs

# upload most recent outcome of huge query
setwd("/Users/ElenaArdinesTomas/Documents/Reports/1.Monthly_Reports/Performance_Reports/2022/2022_04")
orders = read.csv("20220502_sti_order_report.csv")

# extract data for region, quarter and columns needed
Herts <- orders[,c("Customer.ID","Default.LA","Age","AgeSplit","Created.at","Created.at.month.year","Dispatched.at","Dispatched.at.month.year",
                   "Lab.receipt.at","Lab.receipt.at.month.year","Notified.at","Notified.at.month.year","Lab.results.at","Lab.results.at.month.year",
                   "Syphilis","HIV","Chlamydia","Gonorrhoea")]

#-------------------------------------------------------------------------------------------------------------------------

# KPI 1: Monitor percentage of first time Service User

  # unique users (no duplicate) customer IDs
HertsDispat <- Herts[(Herts$Default.LA=="Hertfordshire")
                & (Herts$Dispatched.at.month.year == "2022-01" | Herts$Dispatched.at.month.year == "2022-02" | Herts$Dispatched.at.month.year == "2022-03"),]
# count non-repeat users
Herts.Users = HertsDispat[!duplicated(HertsDispat$Customer.ID),]
# group Customer.ID (i.e. how many times each Customer.ID shows up) - install dplyr
Herts.UsersGrouped <- HertsDispat %>% group_by(Customer.ID) %>% summarise(How.Many.Times=n())
# count frequency: those who bought less than twice (= they bought once)
Bought.Once <- Herts.UsersGrouped[(Herts.UsersGrouped$How.Many.Times<2),]
# KPI 1:
(nrow(Bought.Once)/nrow(Herts.UsersGrouped))*100

#-------------------------------------------------------------------------------------------------------------------------

# KPI 2: Percentage of kits packaged and posted to user within 3 working days of request
  
# TOT Turnaround Time between requested and posted----
# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
HertsDispat$Created.at_Formatted <- as.Date(HertsDispat$Created.at, "%Y-%m-%d")
HertsDispat$Dispatched.at_Formatted <- as.Date(HertsDispat$Dispatched.at, "%Y-%m-%d")

# calculate TOT for this KPI
HertsDispat$day3 <- 0
HertsDispat$day3 <- (HertsDispat$Dispatched.at_Formatted) - (HertsDispat$Created.at_Formatted)

# convert to numeric to be able to group----
class(HertsDispat$day3)
HertsDispat$day3 <- as.numeric(HertsDispat$day3)
#Group in bands
HertsDispat$day3.Bands <- 0
HertsDispat$day3.Bands[HertsDispat$day3 <= 3] <- "Within.3d"
HertsDispat$day3.Bands[HertsDispat$day3 > 3] <- "More.than.3.days"

# KPI 2: 
table(HertsDispat$day3.Bands)
prop.table(table(HertsDispat$day3.Bands))*100

#-------------------------------------------------------------------------------------------------------------------------

# KPI 3: Percentage of specimens to be returned to the laboratory by Service User for processing within 30 days of receipt
# note: counting kits returned, not samples
# extract kits that have been returned in the quarter
HertsReturns <- Herts[(Herts$Default.LA=="Hertfordshire")
                     & (Herts$Lab.results.at.month.year == "2022-01" | Herts$Lab.results.at.month.year == "2022-02" | Herts$Lab.results.at.month.year == "2022-03"),]

# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
HertsReturns$Dispatched.at_Formatted <- as.Date(HertsReturns$Dispatched.at, "%Y-%m-%d")
HertsReturns$Lab.results.at_Formatted <- as.Date(HertsReturns$Lab.results.at, "%Y-%m-%d")

# calculate TOT for this KPI
HertsReturns$day35 <- 0
HertsReturns$day35 <- (HertsReturns$Lab.results.at_Formatted) - (HertsReturns$Dispatched.at_Formatted)

# convert to numeric to be able to group----
class(HertsReturns$day35)
HertsReturns$day35 <- as.numeric(HertsReturns$day35)
#Group in bands
HertsReturns$day35.Bands <- 0
HertsReturns$day35.Bands[HertsReturns$day35 <= 35] <- "Within.35d"
HertsReturns$day35.Bands[HertsReturns$day35 > 35] <- "More.than.35.days"

# KPI3
table(HertsReturns$day35)
table(HertsReturns$day35.Bands)
prop.table(table(HertsReturns$day35.Bands))*100

#-------------------------------------------------------------------------------------------------------------------------

# KPI 4: Percentage of specimens that could not be processed by the laboratory due to sampling error
# add the cases where samples weren't processed per STI
# use data frame HertsReturns
NotProcessed <- HertsReturns

# separate data frames per STI, for samples not processed due to error
NotProcessed.CT <- NotProcessed[(NotProcessed$Chlamydia == 'no_results'),]
NotProcessed.GC <- NotProcessed[(NotProcessed$Gonorrhoea == 'no_results'),]
NotProcessed.Syph <- NotProcessed[(NotProcessed$Syphilis == 'haemolysed' | NotProcessed$Syphilis == 'insufficient' | NotProcessed$Syphilis == 'no_results'),]
NotProcessed.HIV <- NotProcessed[(NotProcessed$HIV == 'haemolysed' | NotProcessed$HIV == 'insufficient' | NotProcessed$HIV == 'no_results'),]

# total samples returned per STI
Processed.CT <- NotProcessed[(NotProcessed$Chlamydia != 'not_requested'),]
Processed.GC <- NotProcessed[(NotProcessed$Gonorrhoea != 'not_requested'),]
Processed.Syph <- NotProcessed[(NotProcessed$Syphilis != 'not_requested'),]
Processed.HIV <- NotProcessed[(NotProcessed$HIV != 'not_requested'),]

# calculates % of the count of not_processed samples vs the count of all returned samples
countNotProcessed <- nrow(NotProcessed.CT)+nrow(NotProcessed.GC)+nrow(NotProcessed.Syph)+nrow(NotProcessed.HIV) 
countProcessed <- nrow(Processed.CT)+nrow(Processed.GC)+nrow(Processed.Syph)+nrow(Processed.HIV)
# KPI 4
(countNotProcessed / countProcessed)*100

#-------------------------------------------------------------------------------------------------------------------------

# KPI 5 and 6: Percentage of results communicated within X days of receiving sample
# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
Results <- HertsReturns
Results$Notified.at_Formatted <- as.Date(Results$Notified.at, "%Y-%m-%d")
Results$Lab.receipt.at_Formatted <- as.Date(Results$Lab.receipt.at, "%Y-%m-%d")

# KPI5: Percentage of negative results communicated within 3 working days of receiving sample
Results$day3sample <- 0
Results$day3sample <- (Results$Notified.at_Formatted) - (Results$Lab.receipt.at_Formatted)

# convert to numeric to be able to group----
class(Results$day3sample)
Results$day3sample <- as.numeric(Results$day3sample)

# from the Results data frame, extract negative results per STI
ResultsNegat.CT <- Results[(Results$Chlamydia == 'negative'),]
ResultsNegat.GC <- Results[(Results$Gonorrhoea == 'negative'),]
ResultsNegat.Syp <- Results[(Results$Syphilis == 'negative'),]
ResultsNegat.HIV <- Results[(Results$HIV == 'negative'),]
# and then those communicated Within 3 days
ResultsNegat.CT.3d <- ResultsNegat.CT[(ResultsNegat.CT$day3sample <= 3),]
ResultsNegat.GC.3d <- ResultsNegat.GC[(ResultsNegat.GC$day3sample <= 3),]
ResultsNegat.Syp.3d <- ResultsNegat.Syp[(ResultsNegat.Syp$day3sample <= 3),]
ResultsNegat.HIV.3d <- ResultsNegat.HIV[(ResultsNegat.HIV$day3sample <= 3),]

# calculates % of the count of negatives notified within 3 days vs all negative results
countNegat.3d <- nrow(ResultsNegat.CT.3d)+nrow(ResultsNegat.GC.3d)+nrow(ResultsNegat.Syp.3d)+nrow(ResultsNegat.HIV.3d)
countNegat <- nrow(ResultsNegat.CT)+nrow(ResultsNegat.GC)+nrow(ResultsNegat.Syp)+nrow(ResultsNegat.HIV)
# KPI 5
(countNegat.3d / countNegat)*100


# KPI 6: Percentage of reactive/positive results communicated within 5 working days of receiving sample.
Results$day5sample <- 0
Results$day5sample <- (Results$Notified.at_Formatted) - (Results$Lab.receipt.at_Formatted)
# convert to numeric to be able to group----
class(Results$day5sample)
Results$day5sample <- as.numeric(Results$day5sample)

# from the Results data frame, extract positive/reactive results per STI
ResultsPosit.CT <- Results[(Results$Chlamydia == 'positive'),]
ResultsPosit.GC <- Results[(Results$Gonorrhoea == 'positive'),]
ResultsPosit.Syp <- Results[(Results$Syphilis == 'reactive'),]
ResultsPosit.HIV <- Results[(Results$HIV == 'reactive'),]
# and then those communicated Within 5 days
ResultsPosit.CT.5d <- ResultsPosit.CT[(ResultsPosit.CT$day5sample <=5),]
ResultsPosit.GC.5d <- ResultsPosit.GC[(ResultsPosit.GC$day5sample <=5),]
ResultsPosit.Syp.5d <- ResultsPosit.Syp[(ResultsPosit.Syp$day5sample <=5),]
ResultsPosit.HIV.5d <- ResultsPosit.HIV[(ResultsPosit.HIV$day5sample <=5),]

# calculates % of the count of postiv/reactiv notified within 5 days vs all postiv/reactiv notified results
countPosit.5d <- nrow(ResultsPosit.CT.5d)+nrow(ResultsPosit.GC.5d)+nrow(ResultsPosit.Syp.5d)+nrow(ResultsPosit.HIV.5d)
countPosit <- nrow(ResultsPosit.CT)+nrow(ResultsPosit.GC)+nrow(ResultsPosit.Syp)+nrow(ResultsPosit.HIV)

# KPI 6
(countPosit.5d / countPosit)*100


#-------------------------------------------------------------------------------------------------------------------------


# KPI 7, 8, 9, 10: Percentage of reactive/positive results from the total tests returned for the following STI’s
# note: counting kits returned, not samples

# KPI 7: Percentage of positive from total returned CT
CT <- HertsReturns[(HertsReturns$Chlamydia == 'negative' | HertsReturns$Chlamydia == 'positive'),]
CT.Positive <- HertsReturns[(HertsReturns$Chlamydia == 'positive'),]
(nrow(CT.Positive)/nrow(CT))*100

# KPI 8: Percentage of positive from total returned GC
GC <- HertsReturns[(HertsReturns$Gonorrhoea == 'negative' | HertsReturns$Gonorrhoea == 'positive'),]
GC.Positive <- HertsReturns[(HertsReturns$Gonorrhoea == 'positive'),]
(nrow(GC.Positive)/nrow(GC))*100

# KPI 9: Percentage of reactive from total returned Syphilis
Syph <- HertsReturns[(HertsReturns$Syph == 'haemolysed' | HertsReturns$Syph == 'insufficient' | HertsReturns$Syph == 'negative' | HertsReturns$Syph == 'reactive'),]
Syph.Positive <- HertsReturns[(HertsReturns$Syph == 'reactive'),]
(nrow(Syph.Positive)/nrow(Syph))*100

# KPI 10: Percentage of reactive from total returned HIV
HIV <- HertsReturns[(HertsReturns$HIV == 'haemolysed' | HertsReturns$HIV == 'insufficient' | HertsReturns$HIV == 'negative' | HertsReturns$HIV == 'reactive'),]
HIV.Positive <- HertsReturns[(HertsReturns$HIV == 'reactive'),]
(nrow(HIV.Positive)/nrow(HIV))*100

#-------------------------------------------------------------------------------------------------------------------------


# KPI 11: Percentage of 16–24 completing a chlamydia test
Under25CT <- HertsReturns
Under25CT <- Under25CT[((Under25CT$Age >15 & Under25CT$Age <25) & (Under25CT$Chlamydia == 'negative' | Under25CT$Chlamydia == 'positive')),]
table(Under25CT$Chlamydia)
# KPI 11:
(nrow(Under25CT)/nrow(CT))*100

# KPI 12: Percentage of 16–24 years per annum with a positive chlamydia result
# Change year
Under25CT.Yr <- orders[(orders$Default.LA=="Hertfordshire" 
                        & (orders$Age >15 & orders$Age <25) 
                        & (orders$Chlamydia == 'positive' | orders$Chlamydia == 'negative')
                        & (grepl('2022',orders$Lab.results.at.month.year)))
                       ,]

Under25CT.Yr.Posit <- orders[(orders$Default.LA=="Hertfordshire" 
                           & (orders$Age >15 & orders$Age <25) 
                           & (orders$Chlamydia == 'positive')
                           & (grepl('2022',orders$Lab.results.at.month.year)))
                          ,]
# KPI 12:
(nrow(Under25CT.Yr.Posit)/nrow(Under25CT.Yr))*100
