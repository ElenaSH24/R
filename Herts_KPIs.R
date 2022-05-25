# Hertfordshire quarterly KPIs

# upload most recent outcome of huge query
setwd("/Users/ElenaArdinesTomas/Documents/Reports/1.Monthly_Reports/Performance_Reports/2022/2022_04")
orders = read.csv("20220502_sti_order_report.csv")

# extract data for region and quarter
Herts <- orders[(orders$Default.LA=="Hertfordshire")
                & (orders$Dispatched.at.month.year == "2022-01" | orders$Dispatched.at.month.year == "2022-02" | orders$Dispatched.at.month.year == "2022-03"),]


# KPI 1: Monitor percentage of first time Service User
# unique users (no duplicate) customer IDs
# no need to order by date in this case
Herts.Users = Herts[!duplicated(Herts$Customer.ID),]

# group Customer.ID (i.e. how many times each Customer.ID shows up) - install dplyr
Herts.UsersGrouped <- Herts %>% group_by(Customer.ID) %>% summarise(How.Many.Times=n())

# count frequency: those who bought less than twice (= they bought once)
Bought.Once <- Herts.UsersGrouped[(Herts.UsersGrouped$How.Many.Times<2),]

# KPI 1:
(nrow(Bought.Once)/nrow(Herts.UsersGrouped))*100


# KPI 2: Percentage of kits packaged and posted to user within 3 working days of request
# TOT Turnaround Time between requested and posted----
# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
KPI2 <- Herts

KPI2$Created.at_Formatted <- as.Date(Herts$Created.at, "%Y-%m-%d")
KPI2$Dispatched.at_Formatted <- as.Date(Herts$Dispatched.at, "%Y-%m-%d")

KPI2$day3 <- 0
KPI2$day3 <- (KPI2$Dispatched.at_Formatted) - (KPI2$Created.at_Formatted)

# convert to numeric to be able to group----
class(KPI2$day3)
KPI2$day3 <- as.numeric(KPI2$day3)
#Group in bands
KPI2$day3.Bands <- 0
KPI2$day3.Bands[KPI2$day3 <= 3] <- "Within.3d"
KPI2$day3.Bands[KPI2$day3 > 3] <- "More.than.3.days"

# KPI 2: 
table(KPI2$day3.Bands)
prop.table(table(KPI2$day3.Bands))*100



# KPI 3: Percentage of specimens to be returned to the laboratory by Service User for processing within 30 days of receipt
# note: counting kits returned, not samples
###### GOT IT FROM TABLAEU PERF REPORT
# extract kits that have been returned
        KPI3 <- orders[(orders$Default.LA=="Hertfordshire")
      & (orders$Lab.results.at.month.year == "2022-01" | orders$Lab.results.at.month.year == "2022-02" | orders$Lab.results.at.month.year == "2022-03")
      & (orders$Dispatched.at != ""),]

KPI3 <- orders[(orders$Default.LA=="Hertfordshire")
               & (orders$Dispatched.at.month.year == "2022-01" | orders$Dispatched.at.month.year == "2022-02" | orders$Dispatched.at.month.year == "2022-03")
               & (orders$Lab.results.at != ""),]


# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
KPI3$Lab.results.at_Formatted <- as.Date(KPI3$Lab.results.at, "%Y-%m-%d")
KPI3$Dispatched.at_Formatted <- as.Date(KPI3$Dispatched.at, "%Y-%m-%d")

# tip from Blake: calculate within 35 days of receipt (instead of 30d) to allow for Royal Mail postage from kit dispatch to receipt of kit to be 5 days 
KPI3$day35 <- 0
KPI3$day35 <- (KPI3$Lab.results.at_Formatted) - (KPI3$Dispatched.at_Formatted)

# convert to numeric to be able to group----
class(KPI3$day35)
KPI3$day35 <- as.numeric(KPI3$day35)
#Group in bands
KPI3$day35.Bands <- 0
KPI3$day35.Bands[KPI3$day35 <= 35] <- "Within.35d"
KPI3$day35.Bands[KPI3$day35 > 35] <- "More.than.35.days"

# KPI 3: 
table(KPI3$day35)
table(KPI3$day35.Bands)
prop.table(table(KPI3$day35.Bands))*100


# KPI 4: 
# add the cases where samples weren't processed per STI
df <- orders[((orders$Default.LA=="Hertfordshire") & 
            (orders$Lab.results.at.month.year =='2022-01' | orders$Lab.results.at.month.year =='2022-02' | orders$Lab.results.at.month.year =='2022-03')
                    ),]

Error.CT <- df[(df$Chlamydia == 'no_results'),]
Error.GC <- df[(df$Gonorrhoea == 'no_results'),]
Error.Syph <- df[(df$Syphilis == 'haemolysed' | df$Syphilis == 'insufficient' | df$Syphilis == 'no_results'),]
Error.HIV <- df[(df$HIV == 'haemolysed' | df$HIV == 'insufficient' | df$HIV == 'no_results'),]

All.CT <- df[(df$Chlamydia != 'not_requested'),]
All.GC <- df[(df$Gonorrhoea != 'not_requested'),]
All.Syph <- df[(df$Syphilis != 'not_requested'),]
All.HIV <- df[(df$HIV != 'not_requested'),]

((nrow(Error.CT)+nrow(Error.GC)+nrow(Error.Syph)+nrow(Error.HIV)) /(nrow(All.CT)+nrow(All.GC)+nrow(All.Syph)+nrow(All.HIV)))*100


# KPI 5 and 6: 
# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
Results$Lab.results.at_Formatted <- as.Date(Results$Lab.results.at, "%Y-%m-%d")
Results$Lab.receipt.at_Formatted <- as.Date(Results$Lab.receipt.at, "%Y-%m-%d")

Results$day3sample <- 0
Results$day3sample <- (Results$Lab.results.at_Formatted) - (Results$Lab.receipt.at_Formatted)

# convert to numeric to be able to group----
class(Results$day3sample)
Results$day3sample <- as.numeric(Results$day3sample)
#Group in bands
Results$day3sample.Bands <- 0
Results$day3sample.Bands[Results$day3sample <= 3] <- "Within.3d"
Results$day3sample.Bands[Results$day3sample > 3] <- "More.than.3.days"

table(Results$day3sample.Bands)
prop.table(table(Results$day3sample.Bands))*100

# KPI5: Percentage of negative results communicated to user within 3 working days of receiving sample
table(Results$day3sample.Bands, Results$Chlamydia)
#using CT as an average ### REVIEW AND INCLUDE ALL NEGATIVE RESULTS!!!
KPI5_1 <- Results[(Results$Chlamydia=="negative"),]
prop.table(table(KPI5_1$day3sample.Bands))*100

    # KPI5_2 <- Results[(KPI5$Gonorrhoea=="negative"),]
    # prop.table(table(KPI5_2$day3sample.Bands))*100

    # KPI5_3 <- Results[(Results$Syphilis=="negative"),]
    # prop.table(table(KPI5_3$day3sample.Bands))*100

    # KPI5_4 <- Results[(Results$HIV=="negative"),]
    # prop.table(table(KPI5_4$day3sample.Bands))*100


# KPI 6: Percentage of reactive/positive results communicated to users within 5 working days of receiving sample.
#using CT as an average ### REVIEW AND INCLUDE ALL NEGATIVE RESULTS!!!
KPI6_1 <- Results[(Results$Chlamydia=="positive"),]
prop.table(table(KPI6_1$day3sample.Bands))*100


# KPI 7, 8, 9, 10: Percentage of reactive/positive results from the total tests returned for the following STI’s
# note: counting kits returned, not samples

# KPI 7: Percentage of positive from total returned CT
CT <- df[(df$Chlamydia == 'negative' | df$Chlamydia == 'positive'),]
CT.Positive <- df[(df$Chlamydia == 'positive'),]
(nrow(CT.Positive)/nrow(CT))*100

# KPI 8: Percentage of positive from total returned GC
GC <- df[(df$Gonorrhoea == 'negative' | df$Gonorrhoea == 'positive'),]
GC.Positive <- df[(df$Gonorrhoea == 'positive'),]
(nrow(GC.Positive)/nrow(GC))*100

# KPI 9: Percentage of reactive from total returned Syphilis
Syph <- df[(df$Syph == 'haemolysed' | df$Syph == 'insufficient' | df$Syph == 'negative' | df$Syph == 'reactive'),]
Syph.Positive <- df[(df$Syph == 'reactive'),]
(nrow(Syph.Positive)/nrow(Syph))*100

# KPI 10: Percentage of reactive from total returned HIV
HIV <- df[(df$HIV == 'haemolysed' | df$HIV == 'insufficient' | df$HIV == 'negative' | df$HIV == 'reactive'),]
HIV.Positive <- df[(df$HIV == 'reactive'),]
(nrow(HIV.Positive)/nrow(HIV))*100


# KPI 11: Percentage of 16–24 completing a chlamydia test
Under25CT <- df
Under25CT <- Under25CT[((Under25CT$Age >15 & Under25CT$Age <25) & (Under25CT$Chlamydia == 'negative' | Under25CT$Chlamydia == 'positive')),]

table(Under25CT$Chlamydia)

(nrow(Under25CT)/nrow(CT))*100

# KPI 12: Percentage of 16–24 years per annum with a positive chlamydia result
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

(nrow(Under25CT.Yr.Posit)/nrow(Under25CT.Yr))*100
