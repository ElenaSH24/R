###Create Function
#DF = name of data frame
#varname = variable that needs to be recoded (Area)
#varname1 = variable used for conditioning (Default LA)
recodeArea <- function(DF,varname,varname1){
  DF[,varname] <- as.character(DF[,varname]) #change class to character
  
  DF[,varname][DF[,varname1]=="Bexley"] <- "Bexley"
  DF[,varname][DF[,varname1]=="Blackburn with Darwen"] <- "Blackburn"
  DF[,varname][DF[,varname1]=="Bradford"] <- "Bradford"
  DF[,varname][DF[,varname1]=="Bromley"] <- "Bromley"
  DF[,varname][DF[,varname1]=="Buckinghamshire"] <- "Buckinghamshire"
  DF[,varname][DF[,varname1]=="Cheshire East"] <- "Cheshire East"
  DF[,varname][DF[,varname1]=="Cornwall and Isles of Scilly PCT"] <- "Cornwall"
  DF[,varname][DF[,varname1]=="County Durham"] <- "County Durham"
  DF[,varname][DF[,varname1]=="Darlington"] <- "Darlington"
  DF[,varname][DF[,varname1]=="Derby"] <- "Derby"
  DF[,varname][DF[,varname1]=="Derbyshire"] <- "Derbyshire"
  DF[,varname][DF[,varname1]=="Dorset"] <- "Dorset"
  DF[,varname][DF[,varname1]=="East Berkshire"] <- "East Berkshire"
  DF[,varname][DF[,varname1]=="Essex"] <- "Essex"
  DF[,varname][DF[,varname1]=="Fettle"] <- "Fettle"
  DF[,varname][DF[,varname1]=="Gateshead"] <- "Gateshead"
  DF[,varname][DF[,varname1]=="Gilead"] <- "Gilead"
  DF[,varname][DF[,varname1]=="Glasgow"] <- "Glasgow"
  DF[,varname][DF[,varname1]=="Gogodoc"] <- "Gogodoc"
  DF[,varname][DF[,varname1]=="Greater Manchester"] <- "Greater Manchester"
  DF[,varname][DF[,varname1]=="Halton"] <- "Halton"
  DF[,varname][DF[,varname1]=="Herefordshire"] <- "Herefordshire"
  DF[,varname][DF[,varname1]=="Hertfordshire"] <- "Hertfordshire"
  DF[,varname][DF[,varname1]=="Hillingdon"] <- "Hillingdon"
  DF[,varname][DF[,varname1]=="Kirklees"] <- "Kirklees"
  DF[,varname][DF[,varname1]=="Knowsley"] <- "Knowsley"
  DF[,varname][DF[,varname1]=="Leicester"] <- "Leicester"
  DF[,varname][DF[,varname1]=="Leicestershire"] <- "Leicestershire"
  DF[,varname][DF[,varname1]=="Liverpool"] <- "Liverpool"
  DF[,varname][DF[,varname1]=="Medway"] <- "Medway"
  DF[,varname][DF[,varname1]=="MTV"] <- "MTV event"
  DF[,varname][DF[,varname1]=="North Staffordshire"] <- "North Staffordshire"
  DF[,varname][DF[,varname1]=="Nottingham"] <- "Nottingham"
  DF[,varname][DF[,varname1]=="PHE"] <- "PHE"
  DF[,varname][DF[,varname1]=="Romania"] <- "Romania"
  DF[,varname][DF[,varname1]=="Rotherham"] <- "Rotherham"
  DF[,varname][DF[,varname1]=="Rutland"] <- "Rutland"
  DF[,varname][DF[,varname1]=="Shropshire"] <- "Shropshire"
  DF[,varname][DF[,varname1]=="South Tyneside"] <- "South Tyneside"
  DF[,varname][DF[,varname1]=="Southend-on-Sea"] <- "Southend"
  DF[,varname][DF[,varname1]=="Staffordshire"] <- "Staffordshire"
  DF[,varname][DF[,varname1]=="Stockport"] <- "Stockport"
  DF[,varname][DF[,varname1]=="Stoke-on-Trent"] <- "Stoke"
  DF[,varname][DF[,varname1]=="Sunderland"] <- "Sunderland"
  DF[,varname][DF[,varname1]=="Tameside"] <- "Tameside"
  DF[,varname][DF[,varname1]=="Telford and Wrekin"] <- "Telford and Wrekin"
  DF[,varname][DF[,varname1]=="Thurrock"] <- "Thurrock"
  DF[,varname][DF[,varname1]=="Warrington"] <- "Warrington"
  DF[,varname][DF[,varname1]=="Wirral"] <- "Wirral"
  DF[,varname][DF[,varname1]=="Worcestershire"] <- "Worcestershire"
  DF[,varname][DF[,varname1]=="HSE Hep C"] <- "HSE Hep C"
  
  DF[,varname][DF[,varname1]=="Oldham" | DF[,varname1]=="Rochdale" | DF[,varname1]=="Bury"] <- "Orbish"
  
  DF[,varname][(DF[,varname1]=="Lambeth"| DF[,varname1]=="Lewisham"| DF[,varname1]=="Southwark")] <- "London"
  
  DF[,varname][DF[,varname1]=="Northern Ireland Belfast PCT" |
                 DF[,varname1]=="Northern Ireland Northern PCT" |
                 DF[,varname1]=="Northern Ireland South Eastern PCT" |
                 DF[,varname1]=="Northern Ireland Southern PCT" |
                 DF[,varname1]=="Northern Ireland Western PCT" | 
                 DF[,varname1]=="Northern Ireland Offline: Southern"] <- "Northern Ireland"
  
  DF[,varname][DF[,varname1]=="Ireland - Dublin" |  DF[,varname1]=="Ireland - Cork" |
                 DF[,varname1]=="Ireland - Kerry" | DF[,varname1]=="Ireland - Wicklow" | 
                 DF[,varname1]=="Ireland - Kildare" | DF[,varname1]=="Ireland - Cavan" | 
                 DF[,varname1]=="Ireland - Louth" | DF[,varname1]=="Ireland - Meath" |
                 DF[,varname1]=="Ireland - Monaghan" | DF[,varname1]=="Ireland - Offaly" |
                 DF[,varname1]=="Ireland - Galway" | DF[,varname1]=="Ireland - Mayo" |
                 DF[,varname1]=="Ireland - Laois" | DF[,varname1]=="Ireland - Roscommon" |
                 DF[,varname1]=="Ireland - Donegal" | DF[,varname1]=="Ireland - Sligo" |
                 DF[,varname1]=="Ireland - Leitrim" | DF[,varname1]=="Ireland - Limerick" |
                 DF[,varname1]=="Ireland - Clare" | DF[,varname1]=="Ireland - Tipperary" |
                 DF[,varname1]=="Ireland - Carlow" | DF[,varname1]=="Ireland - Kilkenny" | 
                 DF[,varname1]=="Ireland - Westmeath" | DF[,varname1]=="Ireland - Longford" |
                 DF[,varname1]=="Ireland - Waterford" | DF[,varname1]=="Ireland - Wexford"] <- "Ireland"
  
  DF[,varname][ DF[,varname1]=="PrEP Trial - Barnsley Integrated Sexual Health" |
                  DF[,varname1]=="PrEP Trial - Bishop Auckland General Hospital" |
                  DF[,varname1]=="PrEP Trial - Boston Sexual Health Clinic" |
                  DF[,varname1]=="PrEP Trial - Coventry & Warwickshire Hospital" |
                  DF[,varname1]=="PrEP Trial - King's Mill Hospital" |
                  DF[,varname1]=="PrEP Trial - Kingston Hospital" |
                  DF[,varname1]=="PrEP Trial - Lindon House Sexual Health Clinic" |
                  DF[,varname1]=="PrEP Trial - Long Eaton Health Centre (Integrated)" |
                  DF[,varname1]=="PrEP Trial - Sexual Health @ London Road" |
                  DF[,varname1]=="PrEP Trial - St Helier Hospital (C&W Integrated)" |
                  DF[,varname1]=="PrEP Trial - St Marks Hospital" |
                  DF[,varname1]=="PrEP Trial - University Hospital Birmingham" |
                  DF[,varname1]=="PrEP Trial - Wakefield Integrated Sexual Health Services"] <- "PrEP Trial"
  
  DF[,varname][ DF[,varname1]=="Freetesting - Bedford" |
                  DF[,varname1]=="Freetesting - Blackburn with Darwen" |
                  DF[,varname1]=="Freetesting - Blackpool" |
                  DF[,varname1]=="Freetesting - Bolton" |
                  DF[,varname1]=="Freetesting - Brighton and Hove" |
                  DF[,varname1]=="Freetesting - Bury" |
                  DF[,varname1]=="Freetesting - Camden" |
                  DF[,varname1]=="Freetesting - Central Bedfordshire" |
                  DF[,varname1]=="Freetesting - County Durham" |
                  DF[,varname1]=="Freetesting - Coventry" |
                  DF[,varname1]=="Freetesting - Darlington" |
                  DF[,varname1]=="Freetesting - Doncaster" |
                  DF[,varname1]=="Freetesting - Gateshead" |
                  DF[,varname1]=="Freetesting - Hampshire" |
                  DF[,varname1]=="Freetesting - Hartlepool" |
                  DF[,varname1]=="Freetesting - Hertfordshire" |
                  DF[,varname1]=="Freetesting - Islington" |
                  DF[,varname1]=="Freetesting - Kent" |
                  DF[,varname1]=="Freetesting - Kingston upon Thames" |
                  DF[,varname1]=="Freetesting - Lancashire" |
                  DF[,varname1]=="Freetesting - Leeds" |
                  DF[,varname1]=="Freetesting - Leicester" |
                  DF[,varname1]=="Freetesting - Leicestershire" |
                  DF[,varname1]=="Freetesting - Manchester" |
                  DF[,varname1]=="Freetesting - Middlesbrough" |
                  DF[,varname1]=="Freetesting - Milton Keynes" |
                  DF[,varname1]=="Freetesting - Newcastle upon Tyne" |
                  DF[,varname1]=="Freetesting - Norfolk" |
                  DF[,varname1]=="Freetesting - North Tyneside" |
                  DF[,varname1]=="Freetesting - Northumberland" |
                  DF[,varname1]=="Freetesting - Nottingham" |
                  DF[,varname1]=="Freetesting - Nottinghamshire" |
                  DF[,varname1]=="Freetesting - Oldham" |
                  DF[,varname1]=="Freetesting - Oxfordshire" |
                  DF[,varname1]=="Freetesting - Portsmouth" |
                  DF[,varname1]=="Freetesting - Redbridge" |
                  DF[,varname1]=="Freetesting - Redcar and Cleveland" |
                  DF[,varname1]=="Freetesting - Richmond upon Thames" |
                  DF[,varname1]=="Freetesting - Rochdale" |
                  DF[,varname1]=="Freetesting - Rotherham" |
                  DF[,varname1]=="Freetesting - Salford" |
                  DF[,varname1]=="Freetesting - South Tyneside" |
                  DF[,varname1]=="Freetesting - Southampton" |
                  DF[,varname1]=="Freetesting - Stockport" |
                  DF[,varname1]=="Freetesting - Stockton-on-Tees" |
                  DF[,varname1]=="Freetesting - Suffolk" |
                  DF[,varname1]=="Freetesting - Sunderland" |
                  DF[,varname1]=="Freetesting - Swindon" |
                  DF[,varname1]=="Freetesting - Tameside" |
                  DF[,varname1]=="Freetesting - Telford and Wrekin" |
                  DF[,varname1]=="Freetesting - Trafford" |
                  DF[,varname1]=="Freetesting - Wakefield" |
                  DF[,varname1]=="Freetesting - Wandsworth" |
                  DF[,varname1]=="Freetesting - Warwickshire" |
                  DF[,varname1]=="Freetesting - West Sussex" |
                  DF[,varname1]=="Freetesting - Wigan"]  <- "Freetesting.hiv"
  
  DF[,varname] <- as.factor(DF[,varname])  #change class back to factor
  DF[,varname] #prints out variable 
}
# DF[,varname][DF[,varname5]==(grepl("PrEP Trial", DF$varname5))] <- "PrEP Trial"

#once the function is done, save it on a script.
# use source func to call my funct
#source("file.path")


# ------------------------------------------------------------
# DF = name of data frame // varname = variable that needs to be recoded (Area) // varname2 = variable that is used for conditioning (Region)
recodeContraception <- function(DF,varname,varname2){
  DF[,varname] <- as.character(DF[,varname]) #change class to character
  
  DF[,varname][(DF[,varname2]=="Bexley"|DF[,varname2]=="Southwark"|
                  DF[,varname2]=="Lambeth" | DF[,varname2]=="Lewisham" | DF[,varname2]=="Bromley")] <- "London"
  
  DF[,varname][DF[,varname2]=="Bradford"] <- "Bradford"
  DF[,varname][(DF[,varname2]=="Blackburn with Darwen")] <- "Blackburn"
  DF[,varname][DF[,varname2]=="Cheshire East"] <- "Cheshire East"
  DF[,varname][DF[,varname2]=="Cornwall and Isles of Scilly PCT"] <- "Cornwall"
  DF[,varname][DF[,varname2]=="County Durham"] <- "County Durham"
  DF[,varname][DF[,varname2]=="Darlington"] <- "Darlington"
  DF[,varname][DF[,varname2]=="Derby"] <- "Derby"
  DF[,varname][DF[,varname2]=="Derbyshire"] <- "Derbyshire"
  DF[,varname][DF[,varname2]=="Dorset"] <- "Dorset"
  DF[,varname][DF[,varname2]=="East Sussex"] <- "East Sussex"
  DF[,varname][DF[,varname2]=="Essex"] <- "Essex"
  DF[,varname][DF[,varname2]=="Fettle"] <- "Fettle"
  DF[,varname][DF[,varname2]=="Glasgow"] <- "Glasgow"
  DF[,varname][DF[,varname2]=="Halton"] <- "Halton"
  DF[,varname][DF[,varname2]=="Hertfordshire"] <- "Hertfordshire"
  DF[,varname][DF[,varname2]=="Hillingdon"] <- "Hillingdon"
  DF[,varname][DF[,varname2]=="Kirklees"] <- "Kirklees"
  DF[,varname][DF[,varname2]=="Knowsley"] <- "Knowsley"
  DF[,varname][DF[,varname2]=="Leicester" ] <- "Leicester"
  DF[,varname][DF[,varname2]=="Leicestershire"] <- "Leicestershire"
  DF[,varname][DF[,varname2]=="Liverpool"] <- "Liverpool"
  DF[,varname][DF[,varname2]=="Northern Ireland Belfast PCT" |
                 DF[,varname2]=="Northern Ireland Northern PCT" |
                 DF[,varname2]=="Northern Ireland South Eastern PCT" |
                 DF[,varname2]=="Northern Ireland Southern PCT" |
                 DF[,varname2]=="Northern Ireland Western PCT"] <- "Northern Ireland"
  DF[,varname][DF[,varname2]=="Nottingham"] <- "Nottingham"
  DF[,varname][DF[,varname2]=="Rotherham"] <- "Rotherham"
  DF[,varname][DF[,varname2]=="Rutland"] <- "Rutland"
  DF[,varname][(DF[,varname2]=="Southend-on-Sea")] <- "Southend"
  DF[,varname][DF[,varname2]=="Staffordshire"] <- "Staffordshire"
  DF[,varname][DF[,varname2]=="Telford and Wrekin"] <- "Telford and Wrekin"
  DF[,varname][DF[,varname2]=="Teesside"] <- "Teesside"
  DF[,varname][DF[,varname2]=="Thurrock"] <- "Thurrock"
  DF[,varname][DF[,varname2]=="Warrington"] <- "Warrington"
  DF[,varname][DF[,varname2]=="Wirral"] <- "Wirral"
  
  DF[,varname][DF[,varname2]=="Oldham" | DF[,varname2]=="Rochdale" | DF[,varname2]=="Bury"] <- "Orbish"
 
  DF[,varname] <- as.factor(DF[,varname])  #change class back to factor
  DF[,varname] #prints out variable 
}

