# producing Xero file

invoicing = read.csv("20220303_Invoicing_AprToFeb.csv")

names(invoicing)
table(invoicing$Region)

# Create Region, to group freetesting, Ireland etc together
invoicing$Service <- invoicing$default_la
invoicing$Service[grepl("Freetesting -", invoicing$default_la)] <- "Freetesting"
invoicing$Service[grepl("Ireland -", invoicing$default_la)] <- "Ireland"
invoicing$Service[grepl("Northern Ireland ", invoicing$default_la)] <- "Northern Ireland"
invoicing$Service[grepl("PrEP Trial -", invoicing$default_la)] <- "PrEP Trial"

table(invoicing$ContactName)

# Create ContactName, to name regions as per invoicing requirements
invoicing$ContactName <- invoicing$Service
invoicing$ContactName[invoicing$default_la == "Blackburn with Darwen"] <- "Blackburn"
invoicing$ContactName[invoicing$default_la == "Cornwall and Isles of Scilly PCT"] <- "Cornwall and Isles of Scilly"
invoicing$ContactName[invoicing$default_la == "County Durham"] <- "County Durham and Darlington NHS Foundation Trust"
invoicing$ContactName[invoicing$default_la == "Derby"] <- "Derby City"
invoicing$ContactName[invoicing$default_la == "Derbyshire"] <- "Derbyshire Community Health Services NHS Foundation Trust"
invoicing$ContactName[invoicing$default_la == "East Berkshire"] <- "Berkshire"
invoicing$ContactName[invoicing$default_la == "Hillingdon"] <- "London Northwest Healthcare"
invoicing$ContactName[invoicing$default_la == "Leicester"] <- "Leicester City"
invoicing$ContactName[invoicing$default_la == "Nottingham"] <- "Nottingham City Council"
invoicing$ContactName[invoicing$default_la == "PHE"] <- "UKHSA"
invoicing$ContactName[invoicing$default_la == "PrEP Trial"] <- "UKHSA PrEP Trial"
invoicing$ContactName[invoicing$default_la == "Southend-on-Sea"] <- "Southend"
invoicing$ContactName[invoicing$default_la == "Staffordshire"] <- "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"
invoicing$ContactName[invoicing$default_la == "Stoke-on-Trent"] <- "Stoke on Trent"

class(invoicing$order_created_at)
class(invoicing$processed_at)

table(invoicing$ContactName)

# convert character to date, first set the format the date is shown 
invoicing$order_created_at <- as.Date(invoicing$order_created_at,"%Y-%m-%d")
invoicing$processed_at <- as.Date(invoicing$processed_at,"%Y-%m-%d")

# extract month from day date
invoicing$Created_Month <- format(as.Date(invoicing$order_created_at),"%Y-%m")
invoicing$Processed_Month <- format(as.Date(invoicing$processed_at),"%Y-%m")

table(invoicing$Processed_Month, invoicing$overall_type=='kits_sent')

# assign values to 'overall_type' that align with invoicing
invoicing$overall_type[invoicing$overall_type == 'kits_sent'] <- 'Orders'
invoicing$overall_type[invoicing$overall_type == 'kits_tested'] <- 'Returns'
# concatenate values of both variables (type and category) to create the invoicing Description
invoicing$Description <- paste(invoicing$overall_type, invoicing$invoice_category_billable, sep=" - ")

table(invoicing$Description, invoicing$overall_type=='Orders')
# some returns are blank, which shows in data.frame as 'Returns -'
# assimilate those 'blank' categories to CT/GC (single site) - just cause there are only a few every month, and not sure how to allocate to their relevant category, 
# they relate to categories not interpreted by the mapping table in the DB
invoicing$Description[invoicing$Description == "Returns - "] <- "Returns - CT/GC (single site)"

# extract data for the relevant month, and columns needed for invoicing
x <- '2022-02'
invMonth <- invoicing[(invoicing$Processed_Month == x),c("ContactName","Processed_Month",'Description')]


# Include CT treatments. Read the file. Get needed columns
names(Treatments)
invTreatments <- Treatments[Treatments$Dispatched.MonthYear == x, c("Region","Dispatched.MonthYear","drug_name")]










# convert to data.frame to get the count of the number of tests
invMonth_1 = as.data.frame(table (invMonth$ContactName,invMonth$Description))
#name the columns
colnames(invMonth_1)[1] <- "ContactName"
colnames(invMonth_1)[2] <- "Description"
colnames(invMonth_1)[3] <- "Quantity"

# create invoice number column----
# first half of the invoice number is the month
invMonth_1$Invoice <- 'Inv '
# second half of the invoice number is associated to the specific Trust/Region, which we call ContactName in this script
invMonth_1$Number <- ""
invMonth_1$Number[invMonth_1$ContactName == "Essex"] <- "0001"
invMonth_1$Number[invMonth_1$ContactName == "Shropshire"] <- "0002"
invMonth_1$Number[invMonth_1$ContactName == "Telford and Wrekin"] <- "0003"
invMonth_1$Number[invMonth_1$ContactName == "Worcestershire"] <- "0005"
invMonth_1$Number[invMonth_1$ContactName == "London Northwest Healthcare"] <- "0006"
invMonth_1$Number[invMonth_1$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "0007"
invMonth_1$Number[invMonth_1$ContactName == "Derbyshire Community Health Services NHS Foundation Trust"] <- "0008"
invMonth_1$Number[invMonth_1$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "0009"
invMonth_1$Number[invMonth_1$ContactName == "Hertfordshire"] <- "00010"
invMonth_1$Number[invMonth_1$ContactName == "Berkshire"] <- "00012"
invMonth_1$Number[invMonth_1$ContactName == "Bradford"] <- "00013"
invMonth_1$Number[invMonth_1$ContactName == "Knowsley"] <- "00014"
invMonth_1$Number[invMonth_1$ContactName == "Darlington"] <- "00015"
invMonth_1$Number[invMonth_1$ContactName == "Leicester City"] <- "00016"
invMonth_1$Number[invMonth_1$ContactName == "Rutland"] <- "00017"
invMonth_1$Number[invMonth_1$ContactName == "Leicestershire"] <- "00018"
invMonth_1$Number[invMonth_1$ContactName == "Derby City"] <- "00019"
invMonth_1$Number[invMonth_1$ContactName == "North Staffordshire"] <- "00020"
invMonth_1$Number[invMonth_1$ContactName == "Gateshead"] <- "00021"
invMonth_1$Number[invMonth_1$ContactName == "Cheshire East"] <- "00022"
invMonth_1$Number[invMonth_1$ContactName == "Halton"] <- "00023"
invMonth_1$Number[invMonth_1$ContactName == "Warrington"] <- "00024"
invMonth_1$Number[invMonth_1$ContactName == "Northern Ireland"] <- "00025"
invMonth_1$Number[invMonth_1$ContactName == "UKHSA"] <- "00026"
invMonth_1$Number[invMonth_1$ContactName == "Cornwall and Isles of Scilly"] <- "00027"
invMonth_1$Number[invMonth_1$ContactName == "Liverpool"] <- "00028"
invMonth_1$Number[invMonth_1$ContactName == "Stoke on Trent"] <- "00029"
invMonth_1$Number[invMonth_1$ContactName == "Sunderland"] <- "00030"
invMonth_1$Number[invMonth_1$ContactName == "Nottingham City Council"] <- "00031"
invMonth_1$Number[invMonth_1$ContactName == "Buckinghamshire"] <- "00032"
invMonth_1$Number[invMonth_1$ContactName == "Dorset"] <- "00033"
invMonth_1$Number[invMonth_1$ContactName == "Kirklees"] <- "00035"
invMonth_1$Number[invMonth_1$ContactName == "South Tyneside"] <- "00036"
invMonth_1$Number[invMonth_1$ContactName == "East Sussex"] <- "00037"
invMonth_1$Number[invMonth_1$ContactName == "UKHSA PrEP Trial"] <- "00038"
invMonth_1$Number[invMonth_1$ContactName == "Bromley"] <- "00039"
invMonth_1$Number[invMonth_1$ContactName == "Blackburn"] <- "00040"
invMonth_1$Number[invMonth_1$ContactName == "Southend"] <- "00041"
invMonth_1$Number[invMonth_1$ContactName == "Teesside"] <- "00042"
invMonth_1$Number[invMonth_1$ContactName == "Thurrock"] <- "00043"
invMonth_1$Number[invMonth_1$ContactName == "Wirral"] <- "0056"


# create InvoiceNumber as concatenate of 'Invoice' and 'Number'
invMonth_1$InvoiceNumber <- paste(invMonth_1$Invoice,x,invMonth_1$Number, sep=" ")

# some Trusts have a Purchase Order
invMonth_1$PO <- ""
invMonth_1$PO[invMonth_1$ContactName == "Berkshire"] <- "PO 40157114"
invMonth_1$PO[invMonth_1$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "PO RXP0003834180"
invMonth_1$PO[invMonth_1$ContactName == "Bradford"] <- "PO L024942"
invMonth_1$PO[invMonth_1$ContactName == "Cheshire East"] <- "PO RQ6N400039417"
invMonth_1$PO[invMonth_1$ContactName == "Gateshead"] <- "PO RLNN400008230"
invMonth_1$PO[invMonth_1$ContactName == "Halton"] <- "PO RQ6N400039417"
invMonth_1$PO[invMonth_1$ContactName == "Hertfordshire"] <- "PO 000037852"
invMonth_1$PO[invMonth_1$ContactName == "Kirklees"] <- "PO L024941"
invMonth_1$PO[invMonth_1$ContactName == "Knowsley"] <- "PO RQ6N400039417"
invMonth_1$PO[invMonth_1$ContactName == "Leicester City"] <- "PO SS139020"
invMonth_1$PO[invMonth_1$ContactName == "Leicestershire"] <- "PO SS139020"
invMonth_1$PO[invMonth_1$ContactName == "North Staffordshire"] <- "PO SS139114"
invMonth_1$PO[invMonth_1$ContactName == "Nottingham City Council"] <- "PO NCC7156225"
invMonth_1$PO[invMonth_1$ContactName == "UKHSA PrEP Trial"] <- "PO 6695926"
invMonth_1$PO[invMonth_1$ContactName == "Rutland"] <- "PO SS139020"
invMonth_1$PO[invMonth_1$ContactName == "Shropshire"] <- "PO SS105992"
invMonth_1$PO[invMonth_1$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "PO SS124036"
invMonth_1$PO[invMonth_1$ContactName == "Stoke on Trent"] <- "PO SS139114"
invMonth_1$PO[invMonth_1$ContactName == "Telford and Wrekin"] <- "PO SS80163"
invMonth_1$PO[invMonth_1$ContactName == "Warrington"] <- "PO RQ6N400039417"

# create reference. Use 'Region' instead of 'ContactName' as a better reference to our records
invMonth_1$Reference <- paste("SH:24 ",invMonth_1$ContactName, " activity 01.02.2022-28.02.2022", invMonth_1$PO)
# invoice dates----
invMonth_1$InvoiceDate <- "28/02/2022"
invMonth_1$DueDate <- "31/03/2022"

        
# remove dataframe rows based on zero values in one column
invMonth_2 <- invMonth_1[invMonth_1$Quantity != 0, ]


# create price data frames
Description <- c('Orders - All STIs (dual site)','Orders - All STIs (single site)','Orders - All STIs (triple site)',
                'Orders - CT/GC (dual site)','Orders - CT/GC (single site)','Orders - CT/GC (triple site)',
                'Orders - HIV',
                'Orders - HIV & CT/GC (dual site)','Orders - HIV & CT/GC (single site)','Orders - HIV & CT/GC (triple site)',
                'Orders - HIV & Syphilis',
                'Orders - Syphilis',
                'Orders - Syphilis & CT/GC (dual site)','Orders - Syphilis & CT/GC (single site)','Orders - Syphilis & CT/GC (triple site)',
                'Orders - Hep: 1 blood',
                'Orders - Hep: 1 blood & CT/GC (dual site)',
                'Orders - Hep: 1 blood & CT/GC (single site)',
                'Orders - Hep: 1 blood & CT/GC (triple site)',
                'Orders - Hep: 2 bloods',
                'Orders - Hep: 2 bloods & CT/GC (dual site)',
                'Orders - Hep: 2 bloods & CT/GC (single site)',
                'Orders - Hep: 2 bloods & CT/GC (triple site)',
                'Orders - Hep: 3 bloods',
                'Orders - Hep: 3 bloods & CT/GC (dual site)',
                'Orders - Hep: 3 bloods & CT/GC (single site)',
                'Orders - Hep: 3 bloods & CT/GC (triple site)',
                'Orders - Hep: 4 bloods',
                'Orders - Hep: 4 bloods & CT/GC (dual site)',
                'Orders - Hep: 4 bloods & CT/GC (single site)',
                'Orders - Hep: 4 bloods & CT/GC (triple site)',
                'Returns - All STIs (dual site)',
                'Returns - All STIs (single site)',
                'Returns - All STIs (triple site)',
                'Returns - CT/GC (dual site)',
                'Returns - CT/GC (single site)',
                'Returns - CT/GC (triple site)',
                'Returns - HIV',
                'Returns - HIV & CT/GC (dual site)',
                'Returns - HIV & CT/GC (single site)',
                'Returns - HIV & CT/GC (triple site)',
                'Returns - HIV & Syphilis',
                'Returns - Syphilis',
                'Returns - Syphilis & CT/GC (dual site)',
                'Returns - Syphilis & CT/GC (single site)',
                'Returns - Syphilis & CT/GC (triple site)',
                'Returns - Hep: 1 blood',
                'Returns - Hep: 1 blood & CT/GC (dual site)',
                'Returns - Hep: 1 blood & CT/GC (single site)',
                'Returns - Hep: 1 blood & CT/GC (triple site)',
                'Returns - Hep: 2 bloods',
                'Returns - Hep: 2 bloods & CT/GC (dual site)',
                'Returns - Hep: 2 bloods & CT/GC (single site)',
                'Returns - Hep: 2 bloods & CT/GC (triple site)',
                'Returns - Hep: 3 bloods',
                'Returns - Hep: 3 bloods & CT/GC (dual site)',
              'Returns - Hep: 3 bloods & CT/GC (single site)',
              'Returns - Hep: 3 bloods & CT/GC (triple site)',
               'Returns - Hep: 4 bloods',
                'Returns - Hep: 4 bloods & CT/GC (dual site)',
                'Returns - Hep: 4 bloods & CT/GC (single site)',
                'Returns - Hep: 4 bloods & CT/GC (triple site)',
              'CT.Treatments',
              'COC.3mth Levonorgestrel/Ethinylestradiol',
              'COC.3mth Desogestrel / Ethinyloestradiol 20',
              'COC.3mth Desogestrel / Ethinyloestradiol 30',
              'COC.3mth Gestodene / Ethinyloestradiol',
              'COC.3mth Norgestimate / Ethinyloestradiol',
              'COC.6mth Levonorgestrel/Ethinylestradiol',
              'COC.6mth Desogestrel / Ethinyloestradiol 20',
              'COC.6mth Desogestrel / Ethinyloestradiol 30',
              'COC.6mth Gestodene / Ethinyloestradiol',
              'COC.6mth Norgestimate / Ethinyloestradiol',
              'COC.12mth Levonorgestrel/Ethinylestradiol',
              'COC.12mth Desogestrel / Ethinyloestradiol 20',
              'COC.12mth Desogestrel / Ethinyloestradiol 30',
              'COC.12mth Gestodene / Ethinyloestradiol',
              'COC.12mth Norgestimate / Ethinyloestradiol',
              'POP.3mth Desogestrel',
              'POP.3mth Levonorgestrel',
              'POP.3mth Noriday',
              'POP.6mth Desogestrel',
              'POP.6mth Levonorgestrel',
              'POP.6mth Noriday',
              'POP.12mth Desogestrel',
              'POP.12mth Levonorgestrel',
              'POP.12mth Noriday',
              'POP bolt-on Desogestrel 3 months',
              'Bolt-on condoms',
              'Bolt-on lube',
              'Bolt-on pregnancy test',
              'EC.EllaOne',
              'EC.Levonelle',
              'EC.Levonor',
              'Photo.Diagnosis.Consultations',
              'Photo.Treatments (3005) imiquimod',
              'Photo.Treatments (3006) podophyllotoxin',
              'Photo.Treatments (3007) aciclovir_episodic 800mg',
              'Photo.Treatments (3008) aciclovir_suppressive 400mg',
              'Photo.Treatments (3030) condyline',
              'RPR Syphilis dispatched',
              'RPR Syphilis returned',
              'Nuva Ring 3 month',
              'Nuva Ring 6 month',
              'Nuva Ring 12 month',
              'Evra Patch 3 month',
              'Evra Patch 6 month',
              'Evra Patch 12 month',
              'Sayana Press 104mg / 0.65ml 3 month',
              'Sayana Press 104mg / 0.65ml 6 month',
              'Sayana Press 104mg / 0.65ml 12 month')

Fee1DiscountRM <-c(3.60,3.04,4.18,2.99,2.34,3.16,2.72,3.60,3.04,4.18,2.72,2.72,3.60,3.04,4.18, 	
                   2.72,3.60,3.04,4.18,2.72,3.60,3.04,4.18,2.72,3.60,3.04,4.18,2.72,3.60,3.04,4.18,
                   46.13,31.60,51.10,29.06,13.71,34.03,9.64,37.60,23.62,42.56,18.17,9.64,37.60,23.62,42.56,
                   9.64,37.60,23.62,42.56,18.17,46.13,31.60,51.10,26.70,54.66,40.13,59.63,35.23,63.19,48.66,68.16,
                   21.11,
                   16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                   17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,3.48,0.52,0.79,0.65,
                   28.50,15.00,15.00,
                   18.95,64.54,33.77,22.87,18.79,33.77,
                   0.00,12.10,
                   0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)

Fee2Standard <- c(6.54,5.52,7.60,5.44,4.25,5.85,4.94,6.54,5.52,7.60,4.94,
                  4.94,6.54,5.52,7.60,4.94,6.54,5.52,7.60,4.94,6.54,5.52,
                  7.60,4.94,5.52,6.54,7.60,4.94,5.52,6.54,7.60,66.60,38.48,
                  72.82,37.22,19.16,55.28,16.62,54.19,33.85,61.41,22.82,13.52,
                  54.19,33.85,61.41,16.62,54.19,33.85,61.41,22.82,66.60,38.48,
                  72.82,33.54,44.68,72.80,79.02,39.74,50.88,79.00,85.22,
                  20.05,
                  16.30,16.65,16.65,16.65,16.65,19.61,26.50,26.50,26.50,26.50,26.22,36.35,36.35,36.35,36.35,
                  17.92,14.92,17.61,22.85,20.00,20.37,32.70,25.08,23.13,3.48,0.52,0.79,0.65,
                  28.50,15.00,15.00,
                  18.95,64.54,33.77,22.87,18.79,33.77,
                  0.00,12.10,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)

Fee3Discount <- c(3.60,3.04,4.18,2.99,2.34,3.16,2.72,3.60,3.04,4.18,2.72,
                  2.72,3.60,3.04,4.18,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,45.03,30.50,
                  50.00,27.96,12.61,32.93,8.54,36.50,22.52,41.46,
                  17.07,8.54,36.50,22.52,41.46,0.00,0.00,0.00,0.00,0.00,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,
                  0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00)



Fee4DiscountRM5 <- c()

Fee5Freetesting

eSRH

eSRH5





# create data frame with prices per invoicing category
fee1 <- data.frame(Description, Fee1DiscountRM)
fee2 <- data.frame(Description, Fee2Standard)
fee3 <- data.frame(Description, Fee3Discount)



# break down the main invoicing data set as per the fee contracted in each area 
InvFee1 <- invMonth_2 [(invMonth_2$ContactName=="Blackburn" | invMonth_2$ContactName=="Bradford" | invMonth_2$ContactName=="Bromley"
                        | invMonth_2$ContactName=="Cheshire East" | invMonth_2$ContactName=="Cornwall and Isles of Scilly"
                        | invMonth_2$ContactName=="County Durham and Darlington NHS Foundation Trust" | invMonth_2$ContactName=="Darlington"
                        | invMonth_2$ContactName=="Derby City" | invMonth_2$ContactName=="Derbyshire Community Health Services NHS Foundation Trust"
                        | invMonth_2$ContactName=="Dorset" | invMonth_2$ContactName=="Halton"
                        | invMonth_2$ContactName=="Kirklees" | invMonth_2$ContactName=="Knowsley" | invMonth_2$ContactName=="Liverpool"),]

InvFee2 <- invMonth_2 [(invMonth_2$ContactName=="Gateshead" | invMonth_2$ContactName=="Glasgow" | invMonth_2$ContactName=="London Northwest Healthcare"),]

InvFee3 <- invMonth_2 [(invMonth_2$ContactName=="Berkshire"),]



# merge each price data set with its correspondent areas
InvoicesFee1 = merge(x = InvFee1, y = fee1, by = "Description", all.x = TRUE)
InvoicesFee2 = merge(x = InvFee2, y = fee2, by = "Description", all.x = TRUE)
InvoicesFee3 = merge(x = InvFee3, y = fee3, by = "Description", all.x = TRUE)


# rename variables in the data frames that include prices. Variables have to be called the same to be able to stack data frames
#rename(new variable name = existing variable name) ----
InvoicesFee1 <- rename(InvoicesFee1, Unit.Price = Fee1DiscountRM)
InvoicesFee2 <- rename(InvoicesFee2, Unit.Price = Fee2Standard)
InvoicesFee3 <- rename(InvoicesFee3, Unit.Price = Fee3Discount)



# Stack data sets one on top of the other 
InvoicesStack <- rbind(InvoicesFee1, InvoicesFee2, InvoicesFee3)  


# select and order the columns we need
InvoicesStack <- InvoicesStack [c("ContactName","InvoiceNumber","Reference","InvoiceDate","DueDate","Description","Quantity","Unit.Price")]
# order data first by Area (=ContactName) and second by the invoicing category (=Description)
InvoicesStack <- InvoicesStack[order(InvoicesStack$ContactName,InvoicesStack$Description),]





