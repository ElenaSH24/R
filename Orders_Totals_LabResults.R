# read files from 'Backing.Data' Tab
# Install 'dplyr' (for 'rename' function)
# Install 'data.table' (for setDT, to convert row names to a column)

#Install the relevant libraries - do this one time
install.packages("data.table")
install.packages("dplyr")

#clean up the environment----
rm(list = ls())

#create data frame with selected columns----
OrdersMonth <- orders[,c('SH24.UID','Customer.ID','Reason.for.visit','LA.of.residence','Default.LA','Site','Age',
          'Gender',"Gender.at.birth","Genitals","Gender.identity.same.as.birth.sex","Gender.Identity",
          "Sexual.preference","Sexually.assaulted.risk.assessment","Unprotected.sex.in.last.5.days",
          "Pressured.into.sex","Paid.for.sex","Drink.or.Drugs","Depression.or.low.mood","Older.or.younger.partner",
          "Clinic.visited","Clinic.visited.12","Ethnicity","Sexuality","Click.and.collect","Referred.from","Sites.tested",'Test.regime',
          "Created.at","Created.at.month.year","Dispatched.at","Dispatched.at.month.year","Lab.receipt.at","Lab.receipt.at.month.year",
          "Notified.at","Notified.at.month.year","Lab.results.ongoing.at","Lab.results.ongoing.at.month.year","Lab.results.at","Lab.results.at.month.year",
          "Syphilis","HIV","Chlamydia","Gonorrhoea",'Hep.B','Hep.C',"Test.for.Hiv","Test.for.Syphilis.RPR","Lab")]

#Create Area variable and set to 0
OrdersMonth$Area <- 0
#run "recode Area" function and save output in Area; important to write your variable names in colons (but not the data frame name)
OrdersMonth$Area <- recodeArea(DF=OrdersMonth,varname="Area",varname2="Site",varname3 = "LA.of.residence", varname4="Referred.from",varname5="Default.LA")

# set reporting month
v1 <- '2022-03'


table(OrdersMonth$Dispatched.at.month.year == v1, useNA = "always")
table(OrdersMonth$Lab.results.at.month.year == v1)
table(OrdersMonth$Area, OrdersMonth$Dispatched.at.month.year == v1, useNA = "always")

#TOT Turnaround Time: 'Lab.results.at' minus 'Lab.receipt.at'----
# convert dates from 'factor' to 'date' to calculate TOT, and then to 'character' to group
OrdersMonth$LabReceipt_FormatDate <- as.Date(OrdersMonth$Lab.receipt.at, "%Y-%m-%d")
OrdersMonth$LabResults_FormatDate <- as.Date(OrdersMonth$Lab.results.at, "%Y-%m-%d")

OrdersMonth$TOT <- 0
OrdersMonth$TOT <- (OrdersMonth$LabResults_FormatDate) - (OrdersMonth$LabReceipt_FormatDate)

#TOT is a difftime field. Convert to numeric----
class(OrdersMonth$TOT)
OrdersMonth$TOT.Numeric <- as.numeric(OrdersMonth$TOT)
class(OrdersMonth$TOT.Numeric)
#Group TOT in bands
OrdersMonth$TOT.Bands <- 0
OrdersMonth$TOT.Bands[OrdersMonth$TOT==0 | OrdersMonth$TOT==1] <- "TOT.Within.24h"  
OrdersMonth$TOT.Bands[OrdersMonth$TOT==2] <- "TOT.2.days"
OrdersMonth$TOT.Bands[OrdersMonth$TOT==3] <- "TOT.3.days"
OrdersMonth$TOT.Bands[OrdersMonth$TOT>3] <- "TOT.More.than.3.days"

#TDL lab
OrdersMonth$TOT.Bands.TDL <- 0
OrdersMonth$TOT.Bands.TDL[(OrdersMonth$TOT==0 | OrdersMonth$TOT==1) & OrdersMonth$Lab=="TDL"] <- "TOT.Within.24h"  
OrdersMonth$TOT.Bands.TDL[OrdersMonth$TOT==2 & OrdersMonth$Lab=="TDL"] <- "TOT.2.days"
OrdersMonth$TOT.Bands.TDL[OrdersMonth$TOT==3 & OrdersMonth$Lab=="TDL"] <- "TOT.3.days"
OrdersMonth$TOT.Bands.TDL[OrdersMonth$TOT>3 & OrdersMonth$Lab=="TDL"] <- "TOT.More.than.3.days"

#SPS lab 
OrdersMonth$TOT.Bands.SPS <- 0
OrdersMonth$TOT.Bands.SPS[(OrdersMonth$TOT==0 | OrdersMonth$TOT==1) & OrdersMonth$Lab=="SPS"] <- "TOT.Within.24h"  
OrdersMonth$TOT.Bands.SPS[OrdersMonth$TOT==2 & OrdersMonth$Lab=="SPS"] <- "TOT.2.days"
OrdersMonth$TOT.Bands.SPS[OrdersMonth$TOT==3 & OrdersMonth$Lab=="SPS"] <- "TOT.3.days"
OrdersMonth$TOT.Bands.SPS[OrdersMonth$TOT>3 & OrdersMonth$Lab=="SPS"] <- "TOT.More.than.3.days"
#End TOT----

#ORDERS----
KitsDispatched <- OrdersMonth[(OrdersMonth$Dispatched.at.month.year == v1 & OrdersMonth$Area!="MTV event"),]
table (KitsDispatched$Dispatched.at.month.year == v1, useNA = "always")

#RETURNS: change methodology in June 2020 from returns based on 'Notified' date to based on 'LabResults' date----
KitsReturned <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == v1 & OrdersMonth$Area!="MTV event"),]
table (KitsReturned$Lab.results.at.month.year == v1, useNA = "always")

# find out those orders with no Area, if any (there shouldn't be any!)
Zero <- KitsDispatched[(KitsDispatched$Area==0),]

Zero <- KitsReturned[(KitsReturned$Area==0),]
rm(Zero)


#put together Orders, Returns and calculate Return Rate----
#1st extract data for the orders, and name the columns. Second, extract data for the returns and name those columns.
ordersRR = as.data.frame(table (KitsDispatched$Area))
#name the columns
colnames(ordersRR)[1] <- "Area"
colnames(ordersRR)[2] <- "Orders"

returnsRR = as.data.frame(table(KitsReturned$Area))
colnames(returnsRR)[1] <- "Area"
colnames(returnsRR)[2] <- "Returns"

#Third, put both bits together:
OrdersReturns = merge(x = ordersRR, y = returnsRR, by = "Area", all = TRUE)

#Calculate RETURN RATE, round to 2 decimals, and format as percent. Use function 'paste' to paste the '%' symbol ----
OrdersReturns$ReturnRate <- paste(round((OrdersReturns$Returns/OrdersReturns$Orders)*100,digits=2),"%",sep="")
class(OrdersReturns$ReturnRate)


#INSUFFICIENT BLOOD SAMPLE: split between insufficient and haemolysed in 'Test HIV' from 2nd.Nov.2020----
Insufficient <- KitsReturned[((KitsReturned$Test.for.Hiv=="insufficient") & KitsReturned$Area!="London"),]
#Convert into a data frame with two columns (Area and frequency)
Insufficient = as.data.frame(table (Insufficient$Area))
#name the columns
colnames(Insufficient)[1] <- "Area"
colnames(Insufficient)[2] <- "Insufficient"
# Repeat for Haemolysed
Haemolysed <- KitsReturned[((KitsReturned$Test.for.Hiv=="haemolysed") & KitsReturned$Area!="London"),]
Haemolysed = as.data.frame(table (Haemolysed$Area))
colnames(Haemolysed)[1] <- "Area"
colnames(Haemolysed)[2] <- "Haemolysed"
# put Insufficient and Haemolysed together
Insuff.Haemol = merge(x = Insufficient, y = Haemolysed, by = "Area", all = TRUE)


# Insufficient and Haemolysed per Lab
Insuff.TDL <- KitsReturned[(KitsReturned$Test.for.Hiv=="insufficient" & KitsReturned$Area!="London" & KitsReturned$Lab =="TDL") ,]
Insuff.TDL = as.data.frame(table (Insuff.TDL$Area))
colnames(Insuff.TDL)[1] <- "Area"
colnames(Insuff.TDL)[2] <- "Insufficient.TDL"

Haemol.TDL <- KitsReturned[(KitsReturned$Test.for.Hiv=="haemolysed" & KitsReturned$Area!="London" & KitsReturned$Lab =="TDL") ,]
Haemol.TDL = as.data.frame(table (Haemol.TDL$Area))
colnames(Haemol.TDL)[1] <- "Area"
colnames(Haemol.TDL)[2] <- "Haemolysed.TDL"

Insuff.SPS <- KitsReturned[(KitsReturned$Test.for.Hiv=="insufficient" & KitsReturned$Area!="London" & KitsReturned$Lab =="SPS") ,]
Insuff.SPS = as.data.frame(table (Insuff.SPS$Area))
colnames(Insuff.SPS)[1] <- "Area"
colnames(Insuff.SPS)[2] <- "Insufficient.SPS"

Haemol.SPS <- KitsReturned[(KitsReturned$Test.for.Hiv=="haemolysed" & KitsReturned$Area!="London" & KitsReturned$Lab =="SPS") ,]
Haemol.SPS = as.data.frame(table (Haemol.SPS$Area))
colnames(Haemol.SPS)[1] <- "Area"
colnames(Haemol.SPS)[2] <- "Haemolysed.SPS"

# put Insufficient and Haemolysed per lab together
Ins.Haem.TDL = merge(x = Insuff.TDL, y = Haemol.TDL, by = "Area", all = TRUE)
Ins.Haem.SPS = merge(x = Insuff.SPS, y = Haemol.SPS, by = "Area", all = TRUE)
Ins.Haem.Labs = merge(x = Ins.Haem.TDL, y = Ins.Haem.SPS, by = "Area", all = TRUE)

# put together both data sets, total and per lab
InsuffHaemolAll = merge(x = Insuff.Haemol, y = Ins.Haem.Labs, by = "Area", all = TRUE)


#put insuff+halemol together, anc alculate insuff+halemol rate (in %):
Insuff.Haemol.Rate = merge(x = InsuffHaemolAll, y = returnsRR, by = "Area", all = TRUE)
#Calculate INSUFF+HAEMOL RATE (insuff+halemol divided by returns), round to 2 decimals, and format as percent ----
Insuff.Haemol.Rate$Insuf.Haem.Rate <- paste(round(((Insuff.Haemol.Rate$Insufficient + Insuff.Haemol.Rate$Haemolysed) 
                                                   / Insuff.Haemol.Rate$Returns)*100,digits=2),"%",sep="")

#Put orders-returns-RR together with insuff+haemol data set
Summary1 <- merge(x = OrdersReturns, y = Insuff.Haemol.Rate, by = "Area", all = TRUE)
Summary1$Returns.y = NULL

#Change name of 'Returns.x' to 'Returned.Tests' with function 'rename' from the 'dplyr' package (install 'dplyr')
#rename(new variable name = existing variable name) ----
Summary1 <- rename(Summary1, Returned.Tests = Returns.x)


# TO INPUT IN SUMMARY MANUALLY: TOT per Lab, based on 'LabResults' (change of methodology in June 2020, from 'LabReceipt' to 'LabResults') and for TDL and SPS separately----
#Calculate overall TOT for current and previous month
OverallTOTPrev <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == "2022-02" 
                           & OrdersMonth$Area != "London"
                           & OrdersMonth$TOT.Bands != "0"),]
table(OverallTOTPrev$TOT.Bands)
prop.table(table(OverallTOTPrev$TOT.Bands))*100


OverallTOT <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == v1 
                           & OrdersMonth$Area != "London"
                           & OrdersMonth$TOT.Bands != "0"),]
table(OverallTOT$TOT.Bands)
prop.table(table(OverallTOT$TOT.Bands))*100

#TDL 
TOT.TDL <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == v1 
                           & OrdersMonth$Area != "London"
                           & OrdersMonth$TOT.Bands != "0" & OrdersMonth$Lab=="TDL"),]
table(TOT.TDL$TOT.Bands)
prop.table(table(TOT.TDL$TOT.Bands))*100

#SPS
TOT.SPS <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == v1 
                           & OrdersMonth$Area != "London"
                           & OrdersMonth$TOT.Bands != "0" & OrdersMonth$Lab=="SPS"),]
table(TOT.SPS$TOT.Bands)
prop.table(table(TOT.SPS$TOT.Bands))*100

#Medlab
TOT.Medlab <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == v1 
                        & OrdersMonth$Area != "London"
                        & OrdersMonth$TOT.Bands != "0" & OrdersMonth$Lab=="Medlab"),]
table(TOT.Medlab$TOT.Bands)
prop.table(table(TOT.Medlab$TOT.Bands))*100
# End TOT per Lab----


#Create data set for relevant month, excluding London, and with no '0'category in TOT.Bands (which is the category relating to NA dates)
OrdersMonthTOT <- OrdersMonth[(OrdersMonth$Lab.results.at.month.year == v1 
                               & OrdersMonth$Area != "London"
                               & OrdersMonth$TOT.Bands != "0"),]

#Calculate TOT for relevant month per area 
table(OrdersMonthTOT$Area,OrdersMonthTOT$TOT.Bands)
table(OrdersMonthTOT$TOT.Bands, useNA = "always")

#Use 'prop.table' to get proportions. Margin=1 means that R calculates the proportions across rows, while margin=2 is down columns
#define on previous line number of digits of the proportions
options(digits = 3)
prop.table(table(OrdersMonthTOT$Area,OrdersMonthTOT$TOT.Bands),margin=1)*100

#Convert the proportions table to a data frame in form of a matrix (to have each TOT band in a different column)
TOT <- as.data.frame.matrix(prop.table(table(OrdersMonthTOT$Area,OrdersMonthTOT$TOT.Bands),margin=1)*100)

#Add % symbol and 2 decimals to the TOTs
TOT$TOT.Within.24h <- paste(round((TOT$TOT.Within.24h),digits=2),"%",sep="")
TOT$TOT.2.days <- paste(round((TOT$TOT.2.days),digits=2),"%",sep="")
TOT$TOT.3.days <- paste(round((TOT$TOT.3.days),digits=2),"%",sep="")
TOT$TOT.More.than.3.days <- paste(round((TOT$TOT.More.than.3.days),digits=2),"%",sep="")

#Reorder columns ----
TOT <- TOT[,c(4,1,2,3)]

#Convert row names to a column----
#You can both remove row names and convert them to a column by reference (without reallocating memory using ->) using setDT 
#and its keep.rownames = TRUE argument from the data.table package
#Install package 'data.table'
setDT(TOT, keep.rownames = "Area")
#End of TOT ----

# Merge TOTs with the rest of the summary
Summary2 = merge(x = Summary1, y = TOT, by = "Area", all = TRUE)

# Blank rows to include TOT break down later in Excel
Summary2$Overall.TOT.24h <- ""
Summary2$Overall.TOT.Over3d <- ""
Summary2$TDL.TOT.24h <- ""
Summary2$TDL.TOT.Over3d <- ""
Summary2$SPS.TOT.24h <- ""
Summary2$SPS.TOT.Over3d <- ""
Summary2$Overall.Prev.TOT.24h <- ""
Summary2$Overall.Prev.TOT.Over3d <- ""


#DIAGNOSIS: Create one data frame for each one of the 4 STIs. Merge two by two. Can't rbind/cbind cause no even number of rows----
Diagn.Syphilis <- KitsReturned[(KitsReturned$Syphilis=="reactive"),]
Diagn.Syphilis = as.data.frame(table(Diagn.Syphilis$Area), useNA = "always")
colnames(Diagn.Syphilis)[1] <- "Area"
colnames(Diagn.Syphilis)[2] <- "Syphilis.Reactive"

#amended 11th.01.2021 CHECK IT'S CORRECT
Diagn.SyphilisRPR <- KitsReturned[(KitsReturned$Test.for.Syphilis.RPR=="reactive"),]
Diagn.SyphilisRPR = as.data.frame(table(Diagn.SyphilisRPR$Area), useNA = "always")
colnames(Diagn.SyphilisRPR)[1] <- "Area"
colnames(Diagn.SyphilisRPR)[2] <- "SyphilisRPR.Reactive"

Diagn.HIV <- KitsReturned[(KitsReturned$HIV =="reactive"),]
Diagn.HIV = as.data.frame(table(Diagn.HIV$Area), useNA = "always")
colnames(Diagn.HIV)[1] <- "Area"
colnames(Diagn.HIV)[2] <- "HIV.Reactive"

Diagn.CT <- KitsReturned[(KitsReturned$Chlamydia =="positive"),]
Diagn.CT = as.data.frame(table(Diagn.CT$Area), useNA = "always")
colnames(Diagn.CT)[1] <- "Area"
colnames(Diagn.CT)[2] <- "CT.Positive"

Diagn.GC <- KitsReturned[(KitsReturned$Gonorrhoea =="positive"),]
Diagn.GC = as.data.frame(table(Diagn.GC$Area), useNA = "always")
colnames(Diagn.GC)[1] <- "Area"
colnames(Diagn.GC)[2] <- "GC.Positive"

Diagn.HepB <- KitsReturned[(KitsReturned$Hep.B =="positive"),]
Diagn.HepB = as.data.frame(table(Diagn.HepB$Area), useNA = "always")
colnames(Diagn.HepB)[1] <- "Area"
colnames(Diagn.HepB)[2] <- "HepB.Positive"

Diagn.HepC <- KitsReturned[(KitsReturned$Hep.C =="positive"),]
Diagn.HepC = as.data.frame(table(Diagn.HepC$Area), useNA = "always")
colnames(Diagn.HepC)[1] <- "Area"
colnames(Diagn.HepC)[2] <- "HepC.Positive"

### Diagnosis whole service: only need to add HepB&C to the summary----
Diagn.Syph.WholeSs <- KitsReturned[(KitsReturned$Syphilis=="reactive"),]
Diagn.HIV.WholeSs <- KitsReturned[(KitsReturned$HIV =="reactive"),]
Diagn.CT.WholeSs <- KitsReturned[(KitsReturned$Chlamydia =="positive"),]
Diagn.GC.WholeSs <- KitsReturned[(KitsReturned$Gonorrhoea =="positive"),]
Diagn.HepB.WholeSs <- KitsReturned[(KitsReturned$Hep.B =="positive"),]
Diagn.HepC.WholeSs <- KitsReturned[(KitsReturned$Hep.C =="positive"),]
# returns per STI
Returns.Syph.WholeSs <- KitsReturned[(KitsReturned$Syphilis =="haemolysed" |KitsReturned$Syphilis =="insufficient" |KitsReturned$Syphilis =="negative"|KitsReturned$Syphilis =="reactive"),]
Returns.HIV.WholeSs <- KitsReturned[(KitsReturned$HIV =="haemolysed" |KitsReturned$HIV =="insufficient" |KitsReturned$HIV =="negative" |KitsReturned$HIV =="reactive"),]
Returns.CT.WholeSs <- KitsReturned[(KitsReturned$Chlamydia =="negative" | KitsReturned$Chlamydia =="positive"),]
Returns.GC.WholeSs <- KitsReturned[(KitsReturned$Gonorrhoea =="negative" | KitsReturned$Gonorrhoea =="positive"),]
Returns.HepB.WholeSs <- KitsReturned[(KitsReturned$Hep.B =="haemolysed" | KitsReturned$Hep.B =="insufficient" | KitsReturned$Hep.B =="negative" |KitsReturned$Hep.B =="positive"),]
Returns.HepC.WholeSs <- KitsReturned[(KitsReturned$Hep.C =="haemolysed" | KitsReturned$Hep.C =="insufficient" | KitsReturned$Hep.C =="negative" |KitsReturned$Hep.C =="positive"),]
# print rates to copy/paste in Performance Summary
print(Rate.Syph.WholeSs <- paste(round((nrow(Diagn.Syph.WholeSs) / nrow(Returns.Syph.WholeSs))*100,digits=2),"%",sep=""))
print(Rate.HIV.WholeSs <- paste(round((nrow(Diagn.HIV.WholeSs) / nrow(Returns.HIV.WholeSs))*100,digits=2),"%",sep=""))
print(Rate.CT.WholeSs <- paste(round((nrow(Diagn.CT.WholeSs) / nrow(Returns.CT.WholeSs))*100,digits=2),"%",sep=""))
print(Rate.GC.WholeSs <- paste(round((nrow(Diagn.GC.WholeSs) / nrow(Returns.GC.WholeSs))*100,digits=2),"%",sep=""))
print(Rate.HepB.WholeSs <- paste(round((nrow(Diagn.HepB.WholeSs) / nrow(Returns.HepB.WholeSs))*100,digits=2),"%",sep=""))
print(Rate.HepC.WholeSs <- paste(round((nrow(Diagn.HepC.WholeSs) / nrow(Returns.HepC.WholeSs))*100,digits=2),"%",sep=""))
### End Diagnosis whole service----


#Put diagnosis together, two by two:
Diagnosis1 = merge(x = Diagn.CT, y = Diagn.GC, by = "Area", all = TRUE)
Diagnosis2 = merge(x = Diagn.Syphilis, y = Diagn.SyphilisRPR, by = "Area", all = TRUE)
Diagnosis3 = merge(x = Diagn.HIV, y = Diagn.HepB, by = "Area", all = TRUE)
Diagnosis4 = merge(x = Diagnosis3, y = Diagn.HepC, by = "Area", all = TRUE)

#Merge all the 4 diagnosis lines together
Diagnosis5 = merge(x = Diagnosis1, y = Diagnosis2, by = "Area", all = TRUE)
Diagnosis6 = merge(x = Diagnosis5, y = Diagnosis4, by = "Area", all = TRUE)

#Include returns, to calculate diagnosis rate
DiagnosisReturns = merge(x = Diagnosis6, y = returnsRR, by = "Area", all = TRUE)

# Sum number of positives/reactives per area
# Use sum(..., na.rm=T) to ignore NAs from the object----
DiagnosisReturns$SumDiagnosis <- rowSums(DiagnosisReturns[,c("Syphilis.Reactive", "HIV.Reactive", "CT.Positive","GC.Positive","HepB.Positive","HepC.Positive")], na.rm=T)
#Calculate DIAGNOSIS RATE, and round to 2 decimals ----
#DiagnosisReturns$DiagnosisRate = round ((DiagnosisReturns$SumDiagnosis/DiagnosisReturns$Returns)*100,2)
DiagnosisReturns$DiagnosisRate <- paste(round((DiagnosisReturns$SumDiagnosis / DiagnosisReturns$Returns)*100,digits=2),"%",sep="")
#remove returns from the table (we don't need it, it's already in the summary)
DiagnosisReturns$Returns = NULL
#Reorder columns
DiagnosisReturns <- DiagnosisReturns[,c(1,4,5,2,3,6,7,8,9)]
#End of DIAGNOSIS ----

#Merge diagnosis with the rest of the summary
Summary3 = merge(x = Summary2, y = DiagnosisReturns, by = "Area", all = TRUE)

#CT TREATMENTS: Include 'Area', 'Site' and 'LA of residence' from 'orders' file. Like an Excel VLOOKUP, using merge()----
TreatmentsMerge <- merge(OrdersMonth [, c('SH24.UID','Area', 'Site','LA.of.residence')], Treatments, by.x = 'SH24.UID', by.y = 'sh24_uid')

#Include MonthYear dates: first convert date fields to date in d/m/y format to MonthYear
TreatmentsMerge$Created_FormatDate <- as.Date(TreatmentsMerge$created_at, "%Y-%m-%d")
TreatmentsMerge$Dispatched_FormatDate <- as.Date(TreatmentsMerge$dispatched_at, "%Y-%m-%d")

#create MonthYear columns. class will be "character"
TreatmentsMerge$Created_MonthYear <- format(TreatmentsMerge$Created_FormatDate, "%Y-%m")
TreatmentsMerge$Dispatched_MonthYear <- format(TreatmentsMerge$Dispatched_FormatDate, "%Y-%m")

table(TreatmentsMerge$Dispatched_MonthYear == v1)
table(TreatmentsMerge$Area, TreatmentsMerge$Dispatched_MonthYear == v1)

#Export as table to Clinical Reports, as a summary of all CT treatments since the start of the service in all areas (inc London!)
write.table (TreatmentsMerge, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\1.Monthly_Reports\\Clinical_Team_Reports\\CT_Treatment\\2022.02.CT.Treatments.csv", row.names=F, sep=",")

#Prepare CT treatments to add to rest of summary: select data for relevant month
TreatmentsMonth <- TreatmentsMerge[(TreatmentsMerge$Dispatched_MonthYear == v1 & !is.na(TreatmentsMerge$Dispatched_MonthYear)),]
NumberTreatments <- as.data.frame(table(TreatmentsMonth$Area))
#Name the columns
colnames(NumberTreatments)[1] <- "Area"
colnames(NumberTreatments)[2] <- "CT.Treatments"
#End CT treatments----

#Merge treatments with the rest of the summary
Summary4 = merge(x = Summary3, y = NumberTreatments, by = "Area", all = TRUE)


#DIAGNOSIS PER STI - based on results per STI (i.e. using 'Chlamydia', 'HIV' etc fields)----
# Slack 11.Nov.2020: Perf reports in Tableau will show a diagnosis rate that is:
#   positives/reactives divided by the number of returns that are: haemolysed, insufficient, negative, reactive/positive
#   we exclude no_result, not_requested and missing
Returns.SYP <- KitsReturned[(KitsReturned$Syphilis =="haemolysed" |KitsReturned$Syphilis =="insufficient" |KitsReturned$Syphilis =="negative"|KitsReturned$Syphilis =="reactive"),]
Returns.SYP = as.data.frame(table(Returns.SYP$Area), useNA = "always")
colnames(Returns.SYP)[1] <- "Area"
colnames(Returns.SYP)[2] <- "SYP.Returns"

Returns.SYP.RPR <- KitsReturned[(KitsReturned$Test.for.Syphilis.RPR =="haemolysed" |KitsReturned$Test.for.Syphilis.RPR =="insufficient" |
                                   KitsReturned$Test.for.Syphilis.RPR =="negative"|KitsReturned$Test.for.Syphilis.RPR =="reactive"),]
Returns.SYP.RPR = as.data.frame(table(Returns.SYP.RPR$Area), useNA = "always")
colnames(Returns.SYP.RPR)[1] <- "Area"
colnames(Returns.SYP.RPR)[2] <- "SYP.RPR.Returns"

Returns.HIV <- KitsReturned[(KitsReturned$HIV =="haemolysed" |KitsReturned$HIV =="insufficient" |KitsReturned$HIV =="negative" |KitsReturned$HIV =="reactive"),]
Returns.HIV = as.data.frame(table(Returns.HIV$Area), useNA = "always")
colnames(Returns.HIV)[1] <- "Area"
colnames(Returns.HIV)[2] <- "HIV.Returns"

Returns.CT <- KitsReturned[(KitsReturned$Chlamydia =="negative" | KitsReturned$Chlamydia =="positive"),]
Returns.CT = as.data.frame(table(Returns.CT$Area), useNA = "always")
colnames(Returns.CT)[1] <- "Area"
colnames(Returns.CT)[2] <- "CT.Returns"

Returns.GC <- KitsReturned[(KitsReturned$Gonorrhoea =="negative" | KitsReturned$Gonorrhoea =="positive"),]
Returns.GC = as.data.frame(table(Returns.GC$Area), useNA = "always")
colnames(Returns.GC)[1] <- "Area"
colnames(Returns.GC)[2] <- "GC.Returns"

Returns.HepB <- KitsReturned[(KitsReturned$Hep.B =="haemolysed" | KitsReturned$Hep.B =="insufficient" | KitsReturned$Hep.B =="negative" |KitsReturned$Hep.B =="positive"),]
Returns.HepB = as.data.frame(table(Returns.HepB$Area), useNA = "always")
colnames(Returns.HepB)[1] <- "Area"
colnames(Returns.HepB)[2] <- "HepB.Returns"

Returns.HepC <- KitsReturned[(KitsReturned$Hep.C =="haemolysed" | KitsReturned$Hep.C =="insufficient" | KitsReturned$Hep.C =="negative" |KitsReturned$Hep.C =="positive"),]
Returns.HepC = as.data.frame(table(Returns.HepC$Area), useNA = "always")
colnames(Returns.HepC)[1] <- "Area"
colnames(Returns.HepC)[2] <- "HepC.Returns"


#Second, merge the relevant returns with the diagnosis of each STI
Rate.SYP = merge(x = Diagn.Syphilis, y = Returns.SYP, by = "Area", all = TRUE)
Rate.SYP.RPR = merge(x = Diagn.SyphilisRPR, y = Returns.SYP.RPR, by = "Area", all = TRUE)
Rate.HIV = merge(x = Diagn.HIV, y = Returns.HIV, by = "Area", all = TRUE)
Rate.CT = merge(x = Diagn.CT, y = Returns.CT, by = "Area", all = TRUE)
Rate.GC = merge(x = Diagn.GC, y = Returns.GC, by = "Area", all = TRUE)
Rate.HepB = merge(x = Diagn.HepB, y = Returns.HepB, by = "Area", all = TRUE)
Rate.HepC = merge(x = Diagn.HepC, y = Returns.HepC, by = "Area", all = TRUE)


#Calculate rates (dividing positive/reactive by returns that include the relevant samples), round to 2 decimals, and format as percent
Rate.SYP$SYPRate <- paste(round((Rate.SYP$Syphilis.Reactive / Rate.SYP$SYP.Returns)*100,digits=1),"%",sep="")
Rate.SYP.RPR$SYPRPRRate <- paste(round((Rate.SYP.RPR$SyphilisRPR.Reactive / Rate.SYP.RPR$SYP.RPR.Returns)*100,digits=1),"%",sep="")
Rate.HIV$HIVRate <- paste(round((Rate.HIV$HIV.Reactive / Rate.HIV$HIV.Returns)*100,digits=1),"%",sep="")
Rate.CT$CTRate <- paste(round((Rate.CT$CT.Positive / Rate.CT$CT.Returns)*100,digits=1),"%",sep="")
Rate.GC$GCRate <- paste(round((Rate.GC$GC.Positive / Rate.GC$GC.Returns)*100,digits=1),"%",sep="")
Rate.HepB$HepBRate <- paste(round((Rate.HepB$HepB.Positive / Rate.HepB$HepB.Returns)*100,digits=1),"%",sep="")
Rate.HepC$HepBRate <- paste(round((Rate.HepC$HepC.Positive / Rate.HepC$HepC.Returns)*100,digits=1),"%",sep="")


# Remove positive/reactive columns 
# Leave the returns (as these returns are calculated differently than the overall returns) and the DiagnosisRates
Rate.SYP$Syphilis.Reactive=NULL
Rate.SYP.RPR$SyphilisRPR.Reactive=NULL
Rate.HIV$HIV.Reactive=NULL
Rate.CT$CT.Positive=NULL
Rate.GC$GC.Positive=NULL
Rate.HepB$HepB.Positive=NULL
Rate.HepC$HepC.Positive=NULL


#Put diagnosis rates together, two by two:
Rates1 = merge(x = Rate.SYP, y = Rate.SYP.RPR, by = "Area", all = TRUE)
Rates11 = merge(x = Rates1, y = Rate.HIV, by = "Area", all = TRUE)
Rates2 = merge(x = Rate.CT, y = Rate.GC, by = "Area", all = TRUE)
Rates3 = merge(x = Rates11, y = Rates2, by = "Area", all = TRUE)
#End DIAGNOSIS PER STI - based on results per STI----

#Merge diagnosis rates with the rest of the summary
Summary5 = merge(x = Summary4, y = Rates3, by = "Area", all = TRUE)

# Other PERFORMANCE METRICS----
# Asymptomatic
MetricAsymptomatic <- KitsDispatched [(KitsDispatched$Reason.for.visit=="Asymptomatic screen"),]
table (MetricAsymptomatic$Area)
MetricAsymptomatic = as.data.frame(table (MetricAsymptomatic$Area))
colnames(MetricAsymptomatic)[1] <- "Area"
colnames(MetricAsymptomatic)[2] <- "Asymptomatic"

# Unprotected sex in last 5 days
MetricUnprotected5d <- KitsDispatched [(KitsDispatched$Unprotected.sex.in.last.5.days=="yes"),]
MetricUnprotected5d = as.data.frame(table (MetricUnprotected5d$Area))
colnames(MetricUnprotected5d)[1] <- "Area"
colnames(MetricUnprotected5d)[2] <- "Unprotected.5d"

# Clinic visited before
MetricClinic <- KitsDispatched [(KitsDispatched$Clinic.visited=="yes"),]
MetricClinic = as.data.frame(table (MetricClinic$Area))
colnames(MetricClinic)[1] <- "Area"
colnames(MetricClinic)[2] <- "Clinic.visited.before"

# Clinic visited in the past 12 months
MetricClinic12m <- KitsDispatched [(KitsDispatched$Clinic.visited.12=="yes"),]
MetricClinic12m = as.data.frame(table (MetricClinic12m$Area))
colnames(MetricClinic12m)[1] <- "Area"
colnames(MetricClinic12m)[2] <- "Clinic.visited.past.12.months"
# End PERFORMANCE METRICS----

#Merge the other perf metrics with the rest of the summary
Metric1 = merge(x = MetricAsymptomatic, y = MetricUnprotected5d, by = "Area", all = TRUE)
Metric2 = merge(x = Metric1, y = MetricClinic, by = "Area", all = TRUE)
Metric3 = merge(x = Metric2, y = MetricClinic12m, by = "Area", all = TRUE)

Summary6 = merge(x = Summary5, y = Metric3, by = "Area", all = TRUE)


#Demographics----
#BAME
BAME <- KitsDispatched [(KitsDispatched$Ethnicity != 'white_english_welsh_scottish_northern_irish_british' &
                      KitsDispatched$Ethnicity != 'other_white' &
                      KitsDispatched$Ethnicity != 'irish' &
                      KitsDispatched$Ethnicity != 'gypsy_or_irish_traveller' &
                      KitsDispatched$Ethnicity != 'not_asked' &
                      KitsDispatched$Ethnicity != 'not_known' &
                      KitsDispatched$Ethnicity != 'prefer_not_to_say'),]
BAME = as.data.frame(table (BAME$Area))
#name the columns
colnames(BAME)[1] <- "Area"
colnames(BAME)[2] <- "BAME.Users"

#Young people
YP <- KitsDispatched [(KitsDispatched$Age <18),]
YP = as.data.frame(table (YP$Area))
#name the columns
colnames(YP)[1] <- "Area"
colnames(YP)[2] <- "Young.People"

#Under 25s
Under25 <- KitsDispatched [(KitsDispatched$Age <25),]
Under25 = as.data.frame(table (Under25$Area))
#name the columns
colnames(Under25)[1] <- "Area"
colnames(Under25)[2] <- "Under.25"

#MSM: REVIEW, IS IT CALCULATED WITH GENITALS FROM oCT 2020?
MSM <- KitsDispatched [(KitsDispatched$Genitals=='Penis' 
                     & (KitsDispatched$Sexual.preference=='men' | KitsDispatched$Sexual.preference=='both')),]
MSM = as.data.frame(table (MSM$Area))
#name the columns
colnames(MSM)[1] <- "Area"
colnames(MSM)[2] <- "MSM"
#END Demographics----

#Put demographics together, two by two:
Demog1 = merge(x = BAME, y = YP, by = "Area", all = TRUE)
Demog2 = merge(x = Under25, y = MSM, by = "Area", all = TRUE)
#Merge all the 4 demographics lines together
Demog3 = merge(x = Demog1, y = Demog2, by = "Area", all = TRUE)
#Include orders (dispatched), to calculate demographics percentages
DemogRates = merge(x = Demog3, y = ordersRR, by = "Area", all = TRUE)

DemogRates$BAME.Users.Percentage <- paste(round((DemogRates$BAME.Users/DemogRates$Orders)*100,digits=2),"%",sep="")
DemogRates$Young.People.Percentage <- paste(round((DemogRates$Young.People/DemogRates$Orders)*100,digits=2),"%",sep="")
DemogRates$Under25.Percentage <- paste(round((DemogRates$Under.25/DemogRates$Orders)*100,digits=2),"%",sep="")
DemogRates$MSM.Percentage <- paste(round((DemogRates$MSM/DemogRates$Orders)*100,digits=2),"%",sep="")

#remove returns from the table
DemogRates$Orders = NULL

#Merge demographics with the rest of the summary
Summary7 = merge(x = Summary6, y = DemogRates, by = "Area", all = TRUE)

#SAFEGUARDING----
#Create safeguarding data sets for young people and adults
table(OrdersMonth$Created.at.month.year == v1)

SafeguardingYP <- OrdersMonth [(OrdersMonth$Age<18 & OrdersMonth$Created.at.month.year == v1),]

#create new variable (columns) flags that - first - equal the safeguarding columns and - then - assign 1 or 0 to those safeguarding flags
SafeguardingYP$Flag_SA <- SafeguardingYP$Sexually.assaulted.risk.assessment
SafeguardingYP$Flag_PS <- SafeguardingYP$Pressured.into.sex
SafeguardingYP$Flag_PAID <- SafeguardingYP$Paid.for.sex
SafeguardingYP$Flag_DD <- SafeguardingYP$Drink.or.Drugs
SafeguardingYP$Flag_DEP <- SafeguardingYP$Depression.or.low.mood
SafeguardingYP$Flag_PARTNER <- SafeguardingYP$Older.or.younger.partner

#Code the safeguarding-flag variables just created by giving them a number, 1 for true/yes, and 0 for false/no:
SafeguardingYP$Flag_SA = ifelse(SafeguardingYP$Flag_SA=="yes", 1,0)
SafeguardingYP$Flag_PS = ifelse(SafeguardingYP$Flag_PS=="yes", 1,0)
SafeguardingYP$Flag_PAID = ifelse(SafeguardingYP$Flag_PAID=="yes",1,0)
SafeguardingYP$Flag_DD = ifelse(SafeguardingYP$Flag_DD=="yes",1,0)
SafeguardingYP$Flag_DEP = ifelse(SafeguardingYP$Flag_DEP=="yes",1,0)
SafeguardingYP$Flag_PARTNER = ifelse(SafeguardingYP$Flag_PARTNER=="yes",1,0)

#create a new variable that gives a summary of all the other flags:
SafeguardingYP$Any_Flag = ifelse((SafeguardingYP$Flag_SA==1 |
                                    SafeguardingYP$Flag_PS==1|
                                    SafeguardingYP$Flag_PAID==1|
                                    SafeguardingYP$Flag_DD==1|
                                    SafeguardingYP$Flag_DEP==1|
                                    SafeguardingYP$Flag_PARTNER), 1,0)

table(SafeguardingYP$Any_Flag)

#create a column that sums up flags
SafeguardingYP$Sum.Flags <- rowSums(SafeguardingYP[,c("Flag_SA",
                                                "Flag_PS",
                                                "Flag_PAID",
                                                "Flag_DD",
                                                "Flag_DEP",
                                                "Flag_PARTNER")])
table(SafeguardingYP$Sum.Flags)


#create subset with only users with flags
SafeguardingYP1 <- SafeguardingYP [SafeguardingYP$Any_Flag==1,]

#Create dataframe for YP users with flag
SafeguardingYPUsers = as.data.frame(table (SafeguardingYP1$Area))
#name the columns
colnames(SafeguardingYPUsers)[1] <- "Area"
colnames(SafeguardingYPUsers)[2] <- "Safeguarding.YP.Users"


# Sum the flags per Area, as 1 user may have several flags (i.e. 20 Flags = 14 YP users) 
SafeguardingYPFlags <- SafeguardingYP1
# sum a variable (Sum.Flags) by group (by Area) using aggregate----
SafeguardingYPFlags <- aggregate(SafeguardingYPFlags$Sum.Flags, by=list(Area=SafeguardingYPFlags$Area), FUN=sum)
#name the columns
colnames(SafeguardingYPFlags)[1] <- "Area"
colnames(SafeguardingYPFlags)[2] <- "Safeguarding.YP.Flags"

# merge both safeguarding YP sets
SafegYP = merge(x = SafeguardingYPFlags, y = SafeguardingYPUsers,  by = "Area", all.x = TRUE)
SafegYP[is.na(SafegYP)] <- 0

# subset ADULT FLAGS
SafeguardAdults <- OrdersMonth [(OrdersMonth$Age>17 & OrdersMonth$Created.at.month.year == v1),]

#In ADULTS there's only one safeguarding flag
SafeguardAdults$Flag <- SafeguardAdults$Sexually.assaulted.risk.assessment
#give value 1 if there is a flag, and 0 if there's no flag
SafeguardAdults$Flag = ifelse(SafeguardAdults$Flag=="yes",1,0)
table(SafeguardAdults$Sexually.assaulted.risk.assessment, SafeguardAdults$Flag)

#create new subset with only adult users with flags
SafeguardAdultsFlags <- SafeguardAdults [SafeguardAdults$Flag==1,]
table(SafeguardAdultsFlags$Area)
#Create dataframe for only YP with flag
SafeguardAdultsFlags = as.data.frame(table (SafeguardAdultsFlags$Area))
#name the columns
colnames(SafeguardAdultsFlags)[1] <- "Area"
colnames(SafeguardAdultsFlags)[2] <- "Safeguarding.Adults.Flags"

#Put safeguarding YP and adults together
Safeg = merge(x = SafeguardAdultsFlags, y = SafegYP, by = "Area", all = TRUE)
#End SAFEGUARDING----

#Merge safeguarding with the rest of the summary
Summary8 = merge(x = Summary7, y = Safeg, by = "Area", all = TRUE)

# CONTRACEPTION----
# Add 'Area' in COC and POP from recoding 'function' in Tab "Saskia_Code_Area.R"
# Create Area variable in both data sets, for COC and for POP, and set to 0
ContCOC$Area <- 0
ContPOP$Area <- 0
Injectable$Area <- 0
Patch$Area <- 0
Ring$Area <- 0

#run function and save output in Area; important to write your variable names in colons (but not the data frame name)
ContCOC$Area <- recodeContraception(DF= ContCOC,varname="Area",varname2="Region")
ContPOP$Area <- recodeContraception(DF= ContPOP,varname="Area",varname2="Region")
Injectable$Area <- recodeContraception(DF= Injectable,varname="Area",varname2="region")
Patch$Area <- recodeContraception(DF= Patch,varname="Area",varname2="region")
Ring$Area <- recodeContraception(DF= Ring,varname="Area",varname2="region")
table(ContCOC$Dispatched.at.month.year == v1)
table(ContPOP$Dispatched.at.month.year == v1)
table(Injectable$Dispatched.Month.Year == v1)
table(Patch$Dispatched.Month.Year == v1)
table(Ring$Dispatched.Month.Year == v1)

# subset for relevant month
ContCOCMonth <- ContCOC [(ContCOC$Dispatched.at.month.year ==  v1),]
ContPOPMonth <- ContPOP [(ContPOP$Dispatched.at.month.year == v1),]
InjectableMonth <- Injectable [(Injectable$Dispatched.Month.Year == v1),]
PatchMonth <- Patch [(Patch$Dispatched.Month.Year == v1),]
RingMonth <- Ring [(Ring$Dispatched.Month.Year == v1),]

# and convert into a data frame
ContCOCMonth1 = as.data.frame(table(ContCOCMonth$Area), useNA = "always")
colnames(ContCOCMonth1)[1] <- "Area"
colnames(ContCOCMonth1)[2] <- "COC"
ContPOPMonth1 = as.data.frame(table(ContPOPMonth$Area), useNA = "always")
colnames(ContPOPMonth1)[1] <- "Area"
colnames(ContPOPMonth1)[2] <- "POP"
InjectableMonth = as.data.frame(table(InjectableMonth$Area), useNA = "always")
colnames(InjectableMonth)[1] <- "Area"
colnames(InjectableMonth)[2] <- "Injectable"
PatchMonth = as.data.frame(table(PatchMonth$Area), useNA = "always")
colnames(PatchMonth)[1] <- "Area"
colnames(PatchMonth)[2] <- "Patch"
RingMonth = as.data.frame(table(RingMonth$Area), useNA = "always")
colnames(RingMonth)[1] <- "Area"
colnames(RingMonth)[2] <- "Ring"

Contraception1 = merge(x = ContCOCMonth1, y = ContPOPMonth1, by = "Area", all = TRUE)
Contraception2 = merge(x = Contraception1, y = InjectableMonth, by = "Area", all = TRUE)
Contraception3 = merge(x = Contraception2, y = PatchMonth, by = "Area", all = TRUE)
Contraception4 = merge(x = Contraception3, y = RingMonth, by = "Area", all = TRUE)

Summary9 = merge(x = Summary8, y = Contraception4, by = "Area", all = TRUE)

## Emergency Contraception - create Area. csv are exported from DataGrip for the relevant month----
ECFuture$Area <- 0
ECFuture$Area[ECFuture$region=="Fettle"] <- "Fettle"
ECFuture$Area <- 0
#rename 'region'
ECFuture <- rename(ECFuture, Region = region)
#run Recode_Area function and save output in Area; important to write your variable names in colons (but not the data frame name)
ECFuture$Area <- recodeContraception(DF= ECFuture,varname="Area",varname2="Region")

ECNow$Area <- 0
#rename 'region'
ECNow <- rename(ECNow, Region = region)
#run Recode_Area function and save output in Area; important to write your variable names in colons (but not the data frame name)
ECNow$Area <- recodeContraception(DF= ECNow,varname="Area",varname2="Region")



# subset per drug 
ECFutureLevonelle <- ECFuture [(ECFuture$Drug=="Levonelle"),c('Area','Drug')]
ECFutureEllaone <- ECFuture [(ECFuture$Drug=="EllaOne"),c('Area','Drug')]
ECNowLevonelle <- ECNow [(ECNow$Drug=="Levonelle"),c('Area','Drug')]
ECNowEllaone <- ECNow [(ECNow$Drug=="EllaOne"),c('Area','Drug')]
ECNowLevonor <- ECNow [(ECNow$Drug=="Levonorgestrel 1.5mg"),c('Area','Drug')]

# convert into data frame
ECFutureLevonelle = as.data.frame(table(ECFutureLevonelle$Area), useNA = "always")
ECFutureEllaone = as.data.frame(table(ECFutureEllaone$Area), useNA = "always")
ECNowLevonelle = as.data.frame(table(ECNowLevonelle$Area), useNA = "always")
ECNowEllaone = as.data.frame(table(ECNowEllaone$Area), useNA = "always")
ECNowLevonor = as.data.frame(table(ECNowLevonor$Area), useNA = "always")

# rename second column
colnames(ECFutureLevonelle)[1] <- "Area"
colnames(ECFutureLevonelle)[2] <- "EC.Levonelle"
colnames(ECFutureEllaone)[1] <- "Area"
colnames(ECFutureEllaone)[2] <- "EC.EllaOne"
colnames(ECNowLevonelle)[1] <- "Area"
colnames(ECNowLevonelle)[2] <- "EC.Levonelle"
colnames(ECNowEllaone)[1] <- "Area"
colnames(ECNowEllaone)[2] <- "EC.EllaOne"
colnames(ECNowLevonor)[1] <- "Area"
colnames(ECNowLevonor)[2] <- "EC.Levonor"

# sum Levonells together (future and now), and EllaOne together, set 'NA' to '0'
# convert character to integer, and sum columns
ECLevonelle = merge(x = ECFutureLevonelle, y = ECNowLevonelle, by = "Area", all = TRUE)
ECLevonelle[is.na(ECLevonelle)] <- "0"
ECLevonelle$EC.Levonelle.x <- as.integer(ECLevonelle$EC.Levonelle.x)
ECLevonelle$EC.Levonelle <- ECLevonelle$EC.Levonelle.x + ECLevonelle$EC.Levonelle.y

ECEllaOne = merge(x = ECFutureEllaone, y = ECNowEllaone, by = 'Area', all = TRUE)
ECEllaOne[is.na(ECEllaOne)] <- "0" #give NAs value 0 for + below to work 
ECEllaOne$EC.EllaOne.x <- as.integer(ECEllaOne$EC.EllaOne.x)
ECEllaOne$EC.EllaOne <- ECEllaOne$EC.EllaOne.x + ECEllaOne$EC.EllaOne.y


# # Also possible to sum with 'rowSums' 
# # ECLevonelle$EC.Levonelle <- rowSums(ECLevonelle[,-1])
#remove columns we don't need 
ECLevonelle$EC.Levonelle.x = NULL
ECLevonelle$EC.Levonelle.y = NULL
ECEllaOne$EC.EllaOne.x = NULL
ECEllaOne$EC.EllaOne.y = NULL


#put together all the different datasets, first Levonelle and EllaOne, and then with Levonor
EC = merge(x = ECEllaOne, y = ECLevonelle, by = 'Area', all = TRUE)
EC = merge(x = EC, y = ECNowLevonor, by = 'Area', all = TRUE)
#give NAs value 0
EC[is.na(EC)] <- "0"
# End EMERGENCY CONTRACEPTION----

Summary91 = merge(x = Summary9, y = EC, by = "Area", all = TRUE)


# PHOTO DIAGNOSIS----
PhotoConsult$Area <- 0
PhotoTreatm$Area <- 0
#rename 'region' as 'Region'
PhotoConsult <- rename(PhotoConsult, Region = region)
PhotoTreatm <- rename(PhotoTreatm, Region = name)
#run recoding function (in Saskia Tab) and save output in Area; important to write your variable names in colons (but not the data frame name)
PhotoConsult$Area <- recodeContraception(DF= PhotoConsult,varname="Area",varname2="Region")
PhotoTreatm$Area <- recodeContraception(DF= PhotoTreatm,varname="Area",varname2="Region")

PhotoConsult1 <- PhotoConsult
class(PhotoConsult1$diagnosed_at)
PhotoConsult1$diagnosed_at <- as.Date(PhotoConsult1$diagnosed_at, format = "%Y-%m-%d")
PhotoConsult1 <- PhotoConsult1[(PhotoConsult1$diagnosed_at >= "2022-02-01" & PhotoConsult1$diagnosed_at <= "2022-02-28"),]

#convert to data.frame
PhotoConsult1 = as.data.frame(table(PhotoConsult1$Area), useNA = "always")
colnames(PhotoConsult1)[1] <- "Area"
colnames(PhotoConsult1)[2] <- "Photo.Diagnosis.Consultations"

PhotoTreatm1 <- PhotoTreatm
PhotoTreatm1 <- PhotoTreatm [(PhotoTreatm$dispatched_month_year=="2022-02"),]
# CAN'T USE WITHOUT A 'COUNT' COLUMN - group by 'Area' with 'aggregate' function. You can preserve column names this way:
# PhotoTreatm1 <- aggregate(PhotoTreatm1["PD.Treatments"], by=PhotoTreatm1["Area"], sum)
# convert into a data frame
PhotoTreatm1 = as.data.frame(table(PhotoTreatm1$Area), useNA = "always")
colnames(PhotoTreatm1)[1] <- "Area"
colnames(PhotoTreatm1)[2] <- "Photo.Diagnosis.Treatments"

PD <- merge(x = PhotoConsult1, y = PhotoTreatm1, by = "Area", all = TRUE)

Summary92 = merge(x = Summary91, y = PD, by = "Area", all = TRUE)


#kits dispatched and returned that include Syphilis RPR (for the invoicing report)----
RPR.Dispatched <- KitsDispatched[grep('RPR',KitsDispatched$Test.regime),]
RPR.Dispatched <- as.data.frame(table (RPR.Dispatched$Area))
#name the columns
colnames(RPR.Dispatched)[1] <- "Area"
colnames(RPR.Dispatched)[2] <- "RPR Syphilis dispatched"

RPR.Returned <- KitsReturned[grep('RPR',KitsReturned$Test.regime),]
RPR.Returned <- as.data.frame(table (RPR.Returned$Area))
#name the columns
colnames(RPR.Returned)[1] <- "Area"
colnames(RPR.Returned)[2] <- "RPR Syphilis returned"
#Third, put both bits together:
RPR = merge(x = RPR.Dispatched, y = RPR.Returned, by = "Area", all = TRUE)
# END count of Syphilis RPR----

Summary93 = merge(x = Summary92, y = RPR, by = "Area", all = TRUE)

# remove rows with no activity----
Summary93 <- Summary93[!(Summary93$Area=="0" | Summary93$Area=="Gilead" | Summary93$Area=="Gogodoc" | Summary93$Area=="MTV event" | Summary93$Area=="Herefordshire"),]

# Replace <NA> with blank ----
Summary93 <- sapply(Summary93, as.character)
Summary93[is.na(Summary93)] <- "0"

#Transpose the table at the end, after all calculations are done. Otherwise, calculations won't work in the columns, and you get lots of NAs----
Summary93 = t(Summary93)

write.table (Summary93, file="\\Users\\ElenaArdinesTomas\\Documents\\Reports\\1.Monthly_Reports\\Performance_Reports\\2022\\2022_02\\SummaryPerformance.2022.02.csv", col.names = F, row.names=T, sep=",")



