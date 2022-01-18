# upload dplyr package for function: left_join

# set working directory
setwd("~/Documents/Reports/1.Monthly_Reports/SAM")

SAM_Original = read.csv("SAM_orders_20220111_0112.csv")

#clean up the environment
rm(list = ls())

#use all columns in the original file
SAMOrdersFile <- SAM_Original

#remove testing orders (not real customers). Customer numbers 44, 23, 3, 4, 5, 6  (Gillian Davies' colleagues testing the payment platform)
SAMOrdersFile <- SAMOrdersFile[(SAMOrdersFile$Customer.number != 44
                                & SAMOrdersFile$Customer.number != 3
                                & SAMOrdersFile$Customer.number != 4
                                & SAMOrdersFile$Customer.number != 5
                                & SAMOrdersFile$Customer.number != 6
                                & SAMOrdersFile$Customer.number != 23),]

#convert date fields to date in d/m/y format to MonthYear ----
SAMOrdersFile$Created_FormatDate <- as.Date(SAMOrdersFile$Created.at..date.of.registration.order., format = "%Y-%m-%d")
SAMOrdersFile$Consultation_FormatDate <- as.Date(SAMOrdersFile$Consultation..date., "%Y-%m-%d")
SAMOrdersFile$Dispatched_FormatDate <- as.Date(SAMOrdersFile$Dispatched.date, "%Y-%m-%d")
SAMOrdersFile$Lab.receipt_FormatDate <- as.Date(SAMOrdersFile$Lab.receipt, "%Y-%m-%d")
SAMOrdersFile$Lab.results_FormatDate <- as.Date(SAMOrdersFile$Lab.results..returned.kit.samples., "%Y-%m-%d")
SAMOrdersFile$Notified_FormatDate <- as.Date(SAMOrdersFile$Notified, "%Y-%m-%d")

#create MonthYear columns. class will be "character" ----
SAMOrdersFile$Created_MonthYear <- format(SAMOrdersFile$Created_FormatDate, "%Y-%m")
SAMOrdersFile$Consultation_MonthYear <- format(SAMOrdersFile$Consultation_FormatDate, "%Y-%m")
SAMOrdersFile$Dispatched_MonthYear <- format(SAMOrdersFile$Dispatched_FormatDate, "%Y-%m")
SAMOrdersFile$Lab.receipt_MonthYear <- format(SAMOrdersFile$Lab.receipt_FormatDate, "%Y-%m")
SAMOrdersFile$Lab.results_MonthYear <- format(SAMOrdersFile$Lab.results_FormatDate, "%Y-%m")
SAMOrdersFile$Notified_MonthYear <- format(SAMOrdersFile$Notified_FormatDate, "%Y-%m")

#Turnaround Time
SAMOrdersFile$TOT.dispatched.created <- SAMOrdersFile$Dispatched_FormatDate - SAMOrdersFile$Created_FormatDate
SAMOrdersFile$TOT.Notified.LabReceipt <- SAMOrdersFile$Notified_FormatDate - SAMOrdersFile$Lab.receipt_FormatDate

table(SAMOrdersFile$TOT.dispatched.created)

#### ONLY 17th.02.2021 submission to Gillian: exclude February 2021 orders 
# SAMOrdersFile <- SAMOrdersFile[(SAMOrdersFile$Created_MonthYear!="2021-02"),]
# SAMOrdersFile1 <- SAMOrdersFile[(SAMOrdersFile$Consultation_MonthYear!="2021-02"),]
# SAMOrdersFile2 <- SAMOrdersFile1[(SAMOrdersFile1$Dispatched_MonthYear!="2021-02"),]
# SAMOrdersFile3 <- SAMOrdersFile2[(SAMOrdersFile2$Lab.receipt_MonthYear!="2021-02"),]
# SAMOrdersFile4 <- SAMOrdersFile3[(SAMOrdersFile3$Lab.results_MonthYear!="2021-02"),]
# SAMOrdersFile5 <- SAMOrdersFile4[(SAMOrdersFile4$Notified_MonthYear!="2021-02"),]
# 
# table(SAMOrdersFile3$Lab.results_MonthYear)
# 
# SAMOrdersFile1 <- SAMOrdersFile[(SAMOrdersFile$Created_MonthYear!="2021-02") &
#                                   (SAMOrdersFile$Consultation_MonthYear!="2021-02") &
#                                   (SAMOrdersFile$Dispatched_MonthYear!="2021-02") &
#                                   (SAMOrdersFile$Lab.receipt_MonthYear!="2021-02") &
#                                   (SAMOrdersFile$Lab.results_MonthYear!="2021-02") &
#                                   (SAMOrdersFile$Notified_MonthYear!="2021-02"),]
# 
# table(SAMOrdersFile1$Created_MonthYear)
# table(SAMOrdersFile1$Consultation_MonthYear)
# table(SAMOrdersFile1$Dispatched_MonthYear)
# table(SAMOrdersFile1$Lab.receipt_MonthYear)
# table(SAMOrdersFile1$Lab.results_MonthYear)
#table(SAMOrdersFile1$Notified_MonthYear)

#SAMOrdersFile <- SAMOrdersFile1


#create new variables (columns) that - first - equal the risk-assessment columns and, then, code those risk-flags with 1 or 0----
SAMOrdersFile$Flag_Drugs <- SAMOrdersFile$Recreation.drugs
SAMOrdersFile$Flag_Partners <- SAMOrdersFile$Multiple.partners
SAMOrdersFile$Flag_Unprotected <- SAMOrdersFile$Unprotected.anal.vaginal.sex
SAMOrdersFile$Flag_Self <- SAMOrdersFile$Self.perceived.risk


#Code the risk-flags just created, by giving them a number, 1 for true, and 0 for false:
SAMOrdersFile$Flag_Drugs <- ifelse(SAMOrdersFile$Flag_Drugs=="drug_use_yes",1,0)
SAMOrdersFile$Flag_Partners <- ifelse(SAMOrdersFile$Flag_Partners=="partners_3_5"|
                                        SAMOrdersFile$Flag_Partners=="partners_6_10"|
                                        SAMOrdersFile$Flag_Partners=="partners_11",1,0) 
SAMOrdersFile$Flag_Unprotected <- ifelse(SAMOrdersFile$Flag_Unprotected=="Yes",1,0)
SAMOrdersFile$Flag_Self <- ifelse(SAMOrdersFile$Flag_Self=="high",1,0)

#create a new variable that gives a summary
SAMOrdersFile$Any.Flag <- ifelse(SAMOrdersFile$Flag_Drugs==1|
                                   SAMOrdersFile$Flag_Partners==1|
                                   SAMOrdersFile$Flag_Unprotected==1|
                                   SAMOrdersFile$Flag_Self==1,1,0)

#create a column that sums-up flags
SAMOrdersFile$Sum.Flags <- rowSums(SAMOrdersFile[,c("Flag_Drugs",
                                                    "Flag_Partners",
                                                    "Flag_Unprotected",
                                                    "Flag_Self")])

# to calculate eligibility, create variable with 'today date' ----
SAMOrdersFile$Today <- Sys.Date()

# Create 'maturity' variable
SAMOrdersFile$MaturityDays <- SAMOrdersFile$Today - SAMOrdersFile$Consultation_FormatDate


# ifelse clause to define eligibility
SAMOrdersFile$Eligible.Gillians =
  ifelse((SAMOrdersFile$Kit.subscription.term=="3" & SAMOrdersFile$MaturityDays >91) |
           (SAMOrdersFile$Kit.subscription.term=="6" & SAMOrdersFile$MaturityDays >182) |
           (SAMOrdersFile$Kit.subscription.term=="12" & SAMOrdersFile$MaturityDays >365),
         "Eligible", "Not eligible")

table(SAMOrdersFile$Eligible.Gillians, useNA = "always")
table(SAMOrdersFile$Eligible.Gillians,SAMOrdersFile$Kit.subscription.term, useNA = "always")
table(SAMOrdersFile$Kit.subscription.term)
prop.table(table(SAMOrdersFile$Kit.subscription.term))

#order columns to match original csv:
SAMOrdered <- SAMOrdersFile [c("Created.at..date.of.registration.order.","Created_MonthYear","Created_FormatDate",
                               "Did.not.attend..date.","Consultation..date.","Consultation_MonthYear","Consultation_FormatDate",
                               "Dispatched.date","Dispatched_MonthYear","Dispatched_FormatDate","Opted.out.of.service",
                               "Lab.receipt","Lab.receipt_MonthYear","Lab.receipt_FormatDate",
                               "Lab.results..returned.kit.samples.","Lab.results_MonthYear","Lab.results_FormatDate",
                               "Notified","Notified_MonthYear","Notified_FormatDate",
                               "TOT.dispatched.created","TOT.Notified.LabReceipt",
                               "Payment.type","Payment.date","Kit.subscription.term","Next.kit.due.date..buy.your.kit.now.prompt.SMS.",
                               "Today",'MaturityDays',"Eligible.Gillians","Order.type","Customer.number",
                               "Refused.order","Test.centre..assigned.","Age","Gender","Sexual.pref","Genitals","Kit.type","Rural.Urban",
                               "Recreation.drugs","Multiple.partners","Unprotected.anal.vaginal.sex","Self.perceived.risk","PrEP.status",
                               "Flag_Drugs","Flag_Partners","Flag_Unprotected","Flag_Self","Any.Flag","Sum.Flags",
                               "Chlamydia","Gonorrhoea","Syphilis","HIV","Processing.errors",
                               "Last.STI.test","How.did.you.hear.about.SAM","Previous.HIV.diagnosis","Previous.Syphilis.diagnosis","Incoming.SMS","Did.not.return.flag")]

#export data to csv. Use double \\ when setting destination file----
write.table (SAMOrdered, file="/Users/ElenaArdines1/Documents/Reports/1.Monthly_Reports/SAM/SAM_RFile_NewDate_v1.csv", row.names=F, sep=",")

######REPEAT USERS----
#To be able to work on repeat users, order file in chronological date created, use FormatDate (class = Date)
SAMOrdersFile <- SAMOrdersFile[(order(as.Date(SAMOrdersFile$Created_FormatDate))),]

#Consider only clients who had a consultation = subset by values different than blank is.na----
Consultation <- SAMOrdersFile[!is.na(SAMOrdersFile$Consultation_FormatDate),]

#new data frame with unique (no duplicate) customer IDs
FirstOrders = Consultation[!duplicated(Consultation$Customer.number),]
#All of those unique customers should be OrderType = Registration (no repeat). There's 1 repeat, processing error.
table(FirstOrders$Order.type)
#From the unique customers, table to find out how many are eligible
table(FirstOrders$Eligible,FirstOrders$Kit.subscription.term, useNA = "always")

#Calculate how many ordered a second, third, fourth... times (second orders plus)
SecondPlusOrders = Consultation[duplicated(Consultation$Customer.number),]

#Check for duplicates within second data frame
#how many observations are on the file?
length(SecondPlusOrders$Customer.number)
#how many of those observations are unique?
length(unique(SecondPlusOrders$Customer.number))

#data frame with second orders
SecondOrders <- SecondPlusOrders[!duplicated(SecondPlusOrders$Customer.number),]

#data frame with third orders, and fourth orders, etc
ThirdPlusOrders <- SecondPlusOrders[duplicated(SecondPlusOrders$Customer.number),]
#and again: how many observations
length(ThirdPlusOrders$Customer.number)
#how many of those are duplicate
length(unique(ThirdPlusOrders$Customer.number))
#data frame with third orders
ThirdOrders <- ThirdPlusOrders[!duplicated(ThirdPlusOrders$Customer.number),]


#data frame with fourth orders, and fifth orders, etc
FourthPlusOrders <- ThirdPlusOrders[duplicated(ThirdPlusOrders$Customer.number),]
#and again: how many observations
length(FourthPlusOrders$Customer.number)
#how many of those are duplicate
length(unique(FourthPlusOrders$Customer.number))
#data frame with third orders
FourthOrders <- FourthPlusOrders[!duplicated(FourthPlusOrders$Customer.number),]


#data frame with fifth orders, and sixth orders, etc
FifthPlusOrders <- FourthOrders[duplicated(FourthOrders$Customer.number),]
#and again: how many observations
length(FifthPlusOrders$Customer.number)
#how many of those are duplicate
length(unique(FifthPlusOrders$Customer.number))
#data frame with third orders
FifthOrders <- FifthPlusOrders[!duplicated(FifthPlusOrders$Customer.number),]


# merge FirstOrders and SecondOrders to find out how many eligible have reordered
# load dplyr
SAM_Join = left_join(FirstOrders, SecondOrders, by= "Customer.number")
table(SAM_Join$Eligible.Gillians.x)

SAM_Join$Progressed = ifelse((SAM_Join$Eligible.Gillians.x=="Eligible" &
                                !is.na(SAM_Join$Consultation..date..y)),
                             "Progressed", "Not progressed")

table(SAM_Join$Progressed, SAM_Join$Kit.subscription.term.x)
table(SAM_Join$Eligible.Gillians.x, SAM_Join$Progressed)
#get the same table, but with percentages
prop.table(table(SAM_Join$Progressed, SAM_Join$Kit.subscription.term.x), margin = 2)

table(SAMOrdersFile$Created_MonthYear, useNA = "always")

#Gillians progression percentage: those who are eligible and have progressed, divided by all who are eligible
#Two opposite approaches for eligibility:
(61/81)*100        #March: 75.3%  Eligible.Gillians with unique clients
(78/95)*100        #April: 82.10526% 
(93/117)*100       #May: 79.5%
(118/(31+118))*100  #June: 79.1946%
(140/(40+140))*100  #17th July (1 yr since start of ss): 77.77%
(163/(66+163))*100  #mid Aug: 71.18%
(190/(72+190))*100  #mid Sep: 72.52%
(446/(222+446))*100  #2020.May: 66.77%
(525/(303+525))*100  #2020.Jul: 63.41%
(890/(452+890))*100  #2020.Nov: 66.32%
(795/(757+795))*100  #2021.Feb: 51.22% Not sure this one is right
(2473/(739+2473))*100  #2021.Aug: 76.99%
(3815/(1219+3815))*100  #2022.Jan: 75.78%

