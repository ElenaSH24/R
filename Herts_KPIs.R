# Hertfordshire quarterly KPIs

# upload most recent outcome of huge query
setwd("/Users/ElenaArdinesTomas/Documents/Reports/1.Monthly_Reports/Performance_Reports/2022/2022_04")
orders = read.csv("20220502_sti_order_report.csv")

# extract data for region and quarter
Herts <- orders[(orders$Default.LA=="Hertfordshire")
                & (orders$Dispatched.at.month.year == "2021-10" | orders$Dispatched.at.month.year == "2021-11" | orders$Dispatched.at.month.year == "2021-12"),]


# KPI 1: Monitor percentage of first time Service User
# unique users (no duplicate) customer IDs
# no need to order by date in this case
Herts.Users = Herts[!duplicated(Herts$Customer.ID),]

# group Customer.ID (i.e. how many times each Customer.ID shows up) - install dplyr
Herts.UsersGrouped <- Herts %>% group_by(Customer.ID) %>% summarise(How.Many.Times=n())

# count frequency: those who bought less than twice (= they bought once)
Bought.Once <- Herts.UsersGrouped[(Herts.UsersGrouped$How.Many.Times<2),]
Bought.Twice <- Herts.UsersGrouped[(Herts.UsersGrouped$How.Many.Times<3 & Herts.UsersGrouped$How.Many.Times>1),]
Bought.Three <- Herts.UsersGrouped[(Herts.UsersGrouped$How.Many.Times<4 & Herts.UsersGrouped$How.Many.Times>2),]
Bought.FourPlus <- Herts.UsersGrouped[(Herts.UsersGrouped$How.Many.Times>3),]

# KPI 1:
(nrow(Bought.Once)/nrow(Herts.UsersGrouped))*100


# KPI 2: Percentage of kits packaged and posted to Service User within 3 working days of request
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
table(KPI2$day3)
prop.table(table(KPI2$day3.Bands))*100


# KPI 3: Percentage of specimens to be returned to the laboratory by Service User for processing within 30 days of receipt
# note: counting kits returned, not samples
KPI3 <- orders
# extract kits that have been returned

              #KPI3 <- Herts[(Herts$Lab.results.at != ""), ]

KPI3 <- orders[(orders$Default.LA=="Hertfordshire")
        & (orders$Created.at.month.year == "2021-10" | orders$Created.at.month.year == "2021-11" | orders$Created.at.month.year == "2021-12")
        & (orders$Dispatched.at != "") & (orders$Lab.results.at != ""),]


KPI3 <- orders[(orders$Default.LA=="Hertfordshire")
               & (orders$Lab.results.at == "2021-10" | orders$Lab.results.at == "2021-11" | orders$Lab.results.at == "2021-12")
               ,]

KPI3 <- orders[((orders$Default.LA=="Hertfordshire") & (orders$Lab.results.at == "2021-10")),]




# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
KPI3$Lab.results.at_Formatted <- as.Date(KPI3$Lab.results.at, "%Y-%m-%d")
KPI3$Dispatched.at_Formatted <- as.Date(KPI3$Dispatched.at, "%Y-%m-%d")

KPI3$day30 <- 0
KPI3$day30 <- (KPI3$Lab.results.at_Formatted) - (KPI3$Dispatched.at_Formatted)

# convert to numeric to be able to group----
class(KPI3$day30)
KPI3$day30 <- as.numeric(KPI3$day30)
#Group in bands
KPI3$day30.Bands <- 0
KPI3$day30.Bands[KPI3$day30 <= 30] <- "Within.30d"
KPI3$day30.Bands[KPI3$day30 > 30] <- "More.than.30.days"

# KPI 3: 
table(KPI3$day30)
table(KPI3$day30.Bands)
prop.table(table(KPI3$day30.Bands))*100


# KPI 4: 
# add the cases where samples weren't processed per STI
Sampling <- orders[((orders$Default.LA=="Hertfordshire") & 
                      (orders$Lab.results.at.month.year =='2021-10' | orders$Lab.results.at.month.year =='2021-11' | orders$Lab.results.at.month.year =='2021-12')
                    ),]


SamplingError.CT <- Sampling[(Sampling$Chlamydia == 'no_results'),]
SamplingError.GC <- Sampling[(Sampling$Chlamydia == 'no_results'),]
SamplingError.Syph <- Sampling[(Sampling$Syphilis == 'haemolysed' | Sampling$Syphilis == 'insufficient' | Sampling$Syphilis == 'no_results'),]
SamplingError.HIV <- Sampling[(Sampling$HIV == 'haemolysed' | Sampling$HIV == 'insufficient' | Sampling$HIV == 'no_results'),]

All.CT <- Sampling[(Sampling$Chlamydia != 'no_requested'),]
All.GC <- Sampling[(Sampling$Gonorrhoea != 'no_requested'),]
All.Syph <- Sampling[(Sampling$Syphilis != 'no_requested' | Sampling$Syphilis == 'negative'),]





nrow(SamplingError.CT)+nrow(SamplingError.GC)+nrow(SamplingError.Syph)+nrow(SamplingError.HIV)








# KPI 7, 8, 9, 10: Percentage of reactive/positive results from the total tests returned for the following STI’s
# note: counting kits returned, not samples
Diagnoses <- orders

Diagnoses <- orders[(orders$Default.LA=="Hertfordshire" & 
                  (orders$Lab.results.at.month.year =='2021-10' | orders$Lab.results.at.month.year =='2021-11' | orders$Lab.results.at.month.year =='2021-12'))
               ,]

# KPI 7: Percentage of positive from total returned CT
CT <- Diagnoses[(Diagnoses$Chlamydia == 'negative' | Diagnoses$Chlamydia == 'positive'),]
CT.Positive <- Diagnoses[(Diagnoses$Chlamydia == 'positive'),]
(nrow(CT.Positive)/nrow(CT))*100

# KPI 8: Percentage of positive from total returned GC
GC <- Diagnoses[(Diagnoses$Gonorrhoea == 'negative' | Diagnoses$Gonorrhoea == 'positive'),]
GC.Positive <- Diagnoses[(Diagnoses$Gonorrhoea == 'positive'),]
(nrow(GC.Positive)/nrow(GC))*100

# KPI 9: Percentage of reactive from total returned Syphilis
Syph <- Diagnoses[(Diagnoses$Syph == 'haemolysed' | Diagnoses$Syph == 'insufficient' | Diagnoses$Syph == 'negative' | Diagnoses$Syph == 'reactive'),]
Syph.Positive <- Diagnoses[(Diagnoses$Syph == 'reactive'),]
(nrow(Syph.Positive)/nrow(Syph))*100

# KPI 10: Percentage of reactive from total returned HIV
HIV <- Diagnoses[(Diagnoses$HIV == 'haemolysed' | Diagnoses$HIV == 'insufficient' | Diagnoses$HIV == 'negative' | Diagnoses$HIV == 'reactive'),]
HIV.Positive <- Diagnoses[(Diagnoses$HIV == 'reactive'),]
(nrow(HIV.Positive)/nrow(HIV))*100


# KPI 11: Percentage of 16–24 completing a chlamydia test
Under25CT <- orders[(orders$Default.LA=="Hertfordshire" 
                      & (orders$Age >15 & orders$Age <25) 
                      & (orders$Chlamydia == 'negative' | orders$Chlamydia == 'positive')
                 & (orders$Lab.results.at.month.year =='2021-10' | orders$Lab.results.at.month.year =='2021-11' | orders$Lab.results.at.month.year =='2021-12'))
              ,]

(nrow(Under25CT)/nrow(CT))*100

# KPI 12: Percentage of 16–24 years per annum with a positive chlamydia result
Under25CT <- orders[(orders$Default.LA=="Hertfordshire" 
                        & (orders$Age >15 & orders$Age <25) 
                        & (orders$Chlamydia == 'positive' | orders$Chlamydia == 'negative')
                        & (grepl('2021',orders$Lab.results.at.month.year)))
                       ,]

Under25CT.Posit <- orders[(orders$Default.LA=="Hertfordshire" 
                           & (orders$Age >15 & orders$Age <25) 
                           & (orders$Chlamydia == 'positive')
                           & (grepl('2021',orders$Lab.results.at.month.year)))
                          ,]

(nrow(Under25CT.Posit)/nrow(Under25CT))*100




