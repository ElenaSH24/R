# producing Xero file

invoicing = read.csv("20220303_Invoicing_AprToFeb.csv")

names(invoicing)
table(invoicing$Region)

# Create Region, to group freetesting, Ireland etc together
invoicing$Region <- invoicing$default_la
invoicing$Region[grepl("Freetesting -", invoicing$default_la)] <- "Freetesting"
invoicing$Region[grepl("Ireland -", invoicing$default_la)] <- "Ireland"
invoicing$Region[grepl("Northern Ireland ", invoicing$default_la)] <- "Northern Ireland"
invoicing$Region[grepl("PrEP Trial -", invoicing$default_la)] <- "PrEP Trial"

# Create ContactName, to name regions as per invoicing requirements
invoicing$ContactName <- invoicing$Region
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
table(invoicing$Region, invoicing$overall_type ,invoicing$Processed_Month=='2022-02')

# assign values to 'overall_type' that align with invoicing
invoicing$overall_type[invoicing$overall_type == 'kits_sent'] <- 'Orders'
invoicing$overall_type[invoicing$overall_type == 'kits_tested'] <- 'Returns'
# concatenate values of both variables (type and category) to create the invoicing Description
invoicing$Description <- paste(invoicing$overall_type, invoicing$invoice_category_billable, sep=" - ")


# extract data for the relevant month, and columns needed for invoicing
x <- '2022-02'
invMonth <- invoicing[(invoicing$Processed_Month == x),
                      c('overall_type',"referred_from_token","invoice_category_billable",'Region',"ContactName","Processed_Month",'Description')]

table(invMonth$overall_type)
table(invMonth$Description, invMonth$overall_type=='Orders')

# create invoice number column
# first half of the invoice number is the month
invMonth$Invoice <- 'Inv '
# second half of the invoice number is associated to the specific Trust/Region, which we call ContactName in this script
invMonth$Number <- ""
invMonth$Number[invMonth$ContactName == "Essex"] <- "0001"
invMonth$Number[invMonth$ContactName == "Shropshire"] <- "0002"
invMonth$Number[invMonth$ContactName == "Telford and Wrekin"] <- "0003"
invMonth$Number[invMonth$ContactName == "Worcestershire"] <- "0005"
invMonth$Number[invMonth$ContactName == "London Northwest Healthcare"] <- "0006"
invMonth$Number[invMonth$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "0007"
invMonth$Number[invMonth$ContactName == "Derbyshire Community Health Services NHS Foundation Trust"] <- "0008"
invMonth$Number[invMonth$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "0009"
invMonth$Number[invMonth$ContactName == "Hertfordshire"] <- "00010"
invMonth$Number[invMonth$ContactName == "Berkshire"] <- "00012"
invMonth$Number[invMonth$ContactName == "Bradford"] <- "00013"
invMonth$Number[invMonth$ContactName == "Knowsley"] <- "00014"
invMonth$Number[invMonth$ContactName == "Darlington"] <- "00015"
invMonth$Number[invMonth$ContactName == "Leicester City"] <- "00016"
invMonth$Number[invMonth$ContactName == "Rutland"] <- "00017"
invMonth$Number[invMonth$ContactName == "Leicestershire"] <- "00018"
invMonth$Number[invMonth$ContactName == "Derby City"] <- "00019"
invMonth$Number[invMonth$ContactName == "North Staffordshire"] <- "00020"
invMonth$Number[invMonth$ContactName == "Gateshead"] <- "00021"
invMonth$Number[invMonth$ContactName == "Cheshire East"] <- "00022"
invMonth$Number[invMonth$ContactName == "Halton"] <- "00023"
invMonth$Number[invMonth$ContactName == "Warrington"] <- "00024"
invMonth$Number[invMonth$ContactName == "Northern Ireland"] <- "00025"
invMonth$Number[invMonth$ContactName == "UKHSA"] <- "00026"
invMonth$Number[invMonth$ContactName == "Cornwall and Isles of Scilly"] <- "00027"
invMonth$Number[invMonth$ContactName == "Liverpool"] <- "00028"
invMonth$Number[invMonth$ContactName == "Stoke on Trent"] <- "00029"
invMonth$Number[invMonth$ContactName == "Sunderland"] <- "00030"
invMonth$Number[invMonth$ContactName == "Nottingham City Council"] <- "00031"
invMonth$Number[invMonth$ContactName == "Buckinghamshire"] <- "00032"
invMonth$Number[invMonth$ContactName == "Dorset"] <- "00033"
invMonth$Number[invMonth$ContactName == "Kirklees"] <- "00035"
invMonth$Number[invMonth$ContactName == "South Tyneside"] <- "00036"
invMonth$Number[invMonth$ContactName == "East Sussex"] <- "00037"
invMonth$Number[invMonth$ContactName == "UKHSA PrEP Trial"] <- "00038"
invMonth$Number[invMonth$ContactName == "Bromley"] <- "00039"
invMonth$Number[invMonth$ContactName == "Blackburn"] <- "00040"
invMonth$Number[invMonth$ContactName == "Southend"] <- "00041"
invMonth$Number[invMonth$ContactName == "Teesside"] <- "00042"
invMonth$Number[invMonth$ContactName == "Thurrock"] <- "00043"
invMonth$Number[invMonth$ContactName == "Wirral"] <- "0056"


# create InvoiceNumber as concatenate of 'Invoice' and 'Number'
invMonth$InvoiceNumber <- paste(invMonth$Invoice,x,invMonth$Number, sep=" ")

# some Trusts have a Purchase Order
invMonth$PO <- ""
invMonth$PO[invMonth$ContactName == "Berkshire"] <- "PO 40157114"
invMonth$PO[invMonth$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "PO RXP0003834180"
invMonth$PO[invMonth$ContactName == "Bradford"] <- "PO L024942"
invMonth$PO[invMonth$ContactName == "Cheshire East"] <- "PO RQ6N400039417"
invMonth$PO[invMonth$ContactName == "Gateshead"] <- "PO RLNN400008230"
invMonth$PO[invMonth$ContactName == "Halton"] <- "PO RQ6N400039417"
invMonth$PO[invMonth$ContactName == "Hertfordshire"] <- "PO 000037852"
invMonth$PO[invMonth$ContactName == "Kirklees"] <- "PO L024941"
invMonth$PO[invMonth$ContactName == "Knowsley"] <- "PO RQ6N400039417"
invMonth$PO[invMonth$ContactName == "Leicester City"] <- "PO SS139020"
invMonth$PO[invMonth$ContactName == "Leicestershire"] <- "PO SS139020"
invMonth$PO[invMonth$ContactName == "North Staffordshire"] <- "PO SS139114"
invMonth$PO[invMonth$ContactName == "Nottingham City Council"] <- "PO NCC7156225"
invMonth$PO[invMonth$ContactName == "UKHSA PrEP Trial"] <- "PO 6695926"
invMonth$PO[invMonth$ContactName == "Rutland"] <- "PO SS139020"
invMonth$PO[invMonth$ContactName == "Shropshire"] <- "PO SS105992"
invMonth$PO[invMonth$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation Trust"] <- "PO SS124036"
invMonth$PO[invMonth$ContactName == "Stoke on Trent"] <- "PO SS139114"
invMonth$PO[invMonth$ContactName == "Telford and Wrekin"] <- "PO SS80163"
invMonth$PO[invMonth$ContactName == "Warrington"] <- "PO RQ6N400039417"

# create reference. Use 'Region' instead of 'ContactName' as a better reference to our records
invMonth$Reference <- paste("SH:24 ",invMonth$ContactName, " activity 01.02.2022-28.02.2022", invMonth$PO)
# invoice dates
invMonth$InvoiceDate <- "28/02/2022"
invMonth$DueDate <- "31/03/2022"

# convert to data.frame to get the count of the number of tests
invMonth_1 = as.data.frame(table (invMonth$ContactName,invMonth$Description))
#name the columns
colnames(invMonth_1)[1] <- "ContactName"
colnames(invMonth_1)[2] <- "Description"
colnames(invMonth_1)[3] <- "Quantity"

# order data frist by Area (=ContractName) and then by the invoicing category (=Description)
invMonth_1 <- invMonth_1[order(invMonth_1$ContactName,invMonth_1$Description),]

# remove dataframe rows based on zero values in one column
invMonth_1 <- invMonth_1[invMonth_1$Quantity != 0, ]

# some returns are blank, which shows in data.frame as 'Returns -'
table(invMonth_1$Description, invMonth_1$Description == 'Returns -')


# price data frames

categories <- c('Orders - All STIs (dual site)',
                'Orders - All STIs (single site)',
                'Orders - All STIs (triple site)',
                'Orders - CT/GC (dual site)',
                'Orders - CT/GC (single site)',
                'Orders - CT/GC (triple site)',
                'Orders - HIV',
                'Orders - HIV & CT/GC (dual site)', 
                'Orders - HIV & CT/GC (single site)',
                'Orders - HIV & CT/GC (triple site)',
                'Orders - HIV & Syphilis',
                'Orders - Syphilis',
                'Orders - Syphilis & CT/GC (dual site)',
                'Orders - Syphilis & CT/GC (single site)',
                'Orders - Syphilis & CT/GC (triple site)',
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
                'Returns - Hep: 4 bloods & CT/GC (triple site)')

Discount <- 



multiply_by_fee <- function(Quantity) {
  temp_C <- (Quantity * )
  return(temp_C)
}



fahrenheit_to_celsius <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
}


