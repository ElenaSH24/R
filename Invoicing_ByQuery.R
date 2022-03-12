#producing Xero file


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
invMonth <- invoicing[(invoicing$Processed_Month == '2022-02'),
                      c('overall_type',"referred_from_token","invoice_category_billable","ContactName","Processed_Month",'Description')]

table(invMonth$overall_type)
table(invMonth$Description, invMonth$overall_type=='Orders')

# create invoice number column
# first half of the invoice number is the month
invMonth$Invoice <- 'Inv2022.02-'
# second half of the invoice number is associated to the specific Trust/Region, which we call ContactName in this script
invMonth$Number <- ""
invMonth$Number[invMonth$ContactName == "Essex"] <- "0001"
invMonth$Number[invMonth$ContactName == "Shropshire"] <- "0002"
invMonth$Number[invMonth$ContactName == "Telford and Wrekin"] <- "0003"
invMonth$Number[invMonth$ContactName == "Worcestershire"] <- "0005"
invMonth$Number[invMonth$ContactName == "London Northwest Healthcare"] <- "0006"
invMonth$Number[invMonth$ContactName == "County Durham and Darlington NHS Foundation Trust"] <- "0007"
invMonth$Number[invMonth$ContactName == "Derbyshire Community Health Services NHS Foundation Trust"] <- "0008"
invMonth$Number[invMonth$ContactName == "South Staffordshire and Shropshire Healthcare NHS Foundation  Trust"] <- "0009"
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
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""
invMonth$Number[invMonth$ContactName == ""] <- ""

# create InvoiceNumber as concatenate of 'Invoice' and 'Number'
invMonth$InvoiceNumber <- paste(invMonth$Invoice, invMonth$Number, sep="")
