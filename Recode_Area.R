###Create Function
#DF = name of data frame
#varname = variable that needs to be recoded (Area)
#varname2 = variable that is used for conditioning (Site)
#varname3 = second variable used for conditioning (LA.of.residence)
#varname4 = third variable used for conditioning (Referred.from)
#varname5 = variable used for conditioning (Default LA)
recodeArea <- function(DF,varname,varname2,varname3,varname4,varname5){
  DF[,varname] <- as.character(DF[,varname]) #change class to character
  DF[,varname][(DF[,varname2]=="Beckenham Beacon Sexual Health Clinic"| 
                  DF[,varname2]=="Burrel Street (SE London)"|
                  DF[,varname2]=="Burrell Street" |
                  DF[,varname2]=="Camberwell"|
                  DF[,varname2]=="Waldron Health Centre")] <- "London"
  
  DF[,varname][DF[,varname2]=="Howard House" & DF[,varname5]=="Bradford"] <- "Bradford"
  DF[,varname][DF[,varname5]=="Bromley"] <- "Bromley"
  DF[,varname][DF[,varname5]=="Buckinghamshire"] <- "Buckinghamshire"
  
  DF[,varname][(DF[,varname2]=="Axess Sexual Health Crewe" | DF[,varname2]=="Axess Sexual Health Macclesfield") 
               & (DF[,varname5]=="Cheshire East")] <- "Cheshire East"
  
  DF[,varname][(DF[,varname5]=="Cornwall and Isles of Scilly PCT")] <- "Cornwall"
  DF[,varname][DF[,varname3]=="County Durham"] <- "County Durham"
  #DF[,varname][DF[,varname2]=="Darlington GUM Clinic" & DF[,varname3]=="Darlington"] <- "Darlington"
  DF[,varname][(DF[,varname5]=="Darlington")] <- "Darlington"
  DF[,varname][DF[,varname2]=="London Road Community Hospital"] <- "Derby"
  DF[,varname][DF[,varname2]=="Wheatbridge Clinic"] <- "Derbyshire"
  DF[,varname][(DF[,varname5]=="Dorset")] <- "Dorset"
  DF[,varname][(DF[,varname5]=="East Berkshire")] <- "East Berkshire"
  DF[,varname][DF[,varname2]=="Essex Hub"] <- "Essex"
  DF[,varname][DF[,varname2]=="Trinity Square Health Centre"] <- "Gateshead"
  DF[,varname][DF[,varname5]=="Glasgow"] <- "Glasgow"
  DF[,varname][(DF[,varname5]=="Halton")] <- "Halton"
  DF[,varname][DF[,varname2]=="iSH Hereford"] <- "Herefordshire"
  DF[,varname][DF[,varname2]=="Hertford Clinic"] <- "Hertfordshire"
  DF[,varname][DF[,varname5]=="Hillingdon"] <- "Hillingdon"
  
  DF[,varname][(DF[,varname5]=="Kirklees")] <- "Kirklees"
  DF[,varname][DF[,varname5]=="Knowsley"] <- "Knowsley"
  
  DF[,varname][(DF[,varname5]=="Leicester")] <- "Leicester"
  DF[,varname][(DF[,varname5]=="Leicestershire")] <- "Leicestershire"
  
  DF[,varname][DF[,varname5]=="Liverpool"] <- "Liverpool"
  DF[,varname][DF[,varname2]=="Clover Street Sexual Health Clinic"] <- "Medway"
  DF[,varname][DF[,varname2]=="Cobridge Sexual Health Clinic"] <- "North Staffordshire"
  
  DF[,varname][DF[,varname5]=="Northern Ireland Belfast PCT" |
                 DF[,varname5]=="Northern Ireland Northern PCT" |
                 DF[,varname5]=="Northern Ireland South Eastern PCT" |
                 DF[,varname5]=="Northern Ireland Southern PCT" |
                 DF[,varname5]=="Northern Ireland Western PCT"] <- "Northern Ireland"
  
  DF[,varname][DF[,varname5]=="Ireland - Dublin" |  DF[,varname5]=="Ireland - Cork" |
                 DF[,varname5]=="Ireland - Kerry" | DF[,varname5]=="Ireland - Wicklow" | 
                 DF[,varname5]=="Ireland - Kildare" | DF[,varname5]=="Ireland - Cavan" | 
                 DF[,varname5]=="Ireland - Louth" | DF[,varname5]=="Ireland - Meath" |
                 DF[,varname5]=="Ireland - Monaghan" | DF[,varname5]=="Ireland - Offaly" |
                 DF[,varname5]=="Ireland - Galway" | DF[,varname5]=="Ireland - Mayo" |
                 DF[,varname5]=="Ireland - Laois" | DF[,varname5]=="Ireland - Roscommon" |
                 DF[,varname5]=="Ireland - Donegal" | DF[,varname5]=="Ireland - Sligo" |
                 DF[,varname5]=="Ireland - Leitrim" | DF[,varname5]=="Ireland - Limerick" |
                 DF[,varname5]=="Ireland - Clare" | DF[,varname5]=="Ireland - Tipperary" |
                 DF[,varname5]=="Ireland - Carlow" | DF[,varname5]=="Ireland - Kilkenny" | 
                 DF[,varname5]=="Northern Ireland Offline: Southern" 
               ] <- "Ireland"

  DF[,varname][DF[,varname5]=="Nottingham"] <- "Nottingham"
  DF[,varname][DF[,varname2]=="ARAS Bucuresti"] <- "Romania"
  
  DF[,varname][(DF[,varname5]=="Rutland")] <- "Rutland"
 
  DF[,varname][DF[,varname3]=="Shropshire" | DF[,varname5]=="Shropshire"] <- "Shropshire"
  DF[,varname][DF[,varname5]=="South Tyneside"] <- "South Tyneside"
  DF[,varname][DF[,varname2]=="Cannock Chase Hospital"|DF[,varname2]=="Stafford Central Clinic"] <- "Staffordshire"
  DF[,varname][DF[,varname5]=="Stoke-on-Trent"] <- "Stoke"
  DF[,varname][DF[,varname5]=="Sunderland"] <- "Sunderland"
  DF[,varname][DF[,varname3]=="Telford and Wrekin"] <- "Telford and Wrekin"
  DF[,varname][(DF[,varname5]=="Warrington")] <- "Warrington"
  DF[,varname][DF[,varname2]=="Worcestershire Hub"] <- "Worcestershire"
  DF[,varname][(DF[,varname5]=="Southend-on-Sea")] <- "Southend"
  DF[,varname][(DF[,varname5]=="Blackburn with Darwen")] <- "Blackburn"
  
  DF[,varname][DF[,varname2]=="Fettle Hub"] <- "Fettle"
  DF[,varname][DF[,varname2]=="Gilead"] <- "Gilead"
  DF[,varname][DF[,varname2]=="MTV event"] <- "MTV event"
  DF[,varname][(DF[,varname5]=="Gogodoc")] <- "Gogodoc"
  
  DF[,varname][(DF[,varname2]=="Freetesting.hiv Hub" & (DF[,varname5]!="PHE"))] <- "Freetesting.hiv"
  DF[,varname][DF[,varname5]=="PHE"] <- "PHE"
  
  DF[,varname][DF[,varname5]=="PrEP Trial - Boston Sexual Health Clinic" |
                 DF[,varname5]=="PrEP Trial - King's Mill Hospital" |
                 DF[,varname5]=="PrEP Trial - Kingston Hospital" |
                 DF[,varname5]=="PrEP Trial - Lindon House Sexual Health Clinic" |
                 DF[,varname5]=="PrEP Trial - Wakefield Integrated Sexual Health Services" |
                 DF[,varname5]=="PrEP Trial - St Marks Hospital" |
                 DF[,varname5]=="PrEP Trial - Barnsley Integrated Sexual Health" |
                 DF[,varname5]=="PrEP Trial - St Helier Hospital (C&W Integrated)" |
                 DF[,varname5]=="PrEP Trial - Bishop Auckland General Hospital" |
                 DF[,varname5]=="PrEP Trial - Coventry & Warwickshire Hospital" |
                 DF[,varname5]=="PrEP Trial - Long Eaton Health Centre (Integrated)" |
                 DF[,varname5]=="PrEP Trial - University Hospital Birmingham" |
                 DF[,varname5]=="PrEP Trial - Sexual Health @ London Road"] <- "PrEP Trial"
  
  DF[,varname][DF[,varname5]=="Bexley"] <- "Bexley"
  DF[,varname][DF[,varname5]=="Thurrock"] <- "Thurrock"
  DF[,varname][DF[,varname5]=="Wirral"] <- "Wirral"
  DF[,varname][DF[,varname5]=="Greater Manchester"] <- "Greater Manchester"
  DF[,varname][DF[,varname5]=="Stockport" | DF[,varname5]=="Tameside"] <- "Stockport & Tameside"
  DF[,varname][DF[,varname5]=="Rotherham"] <- "Rotherham"
  
  
  DF[,varname] <- as.factor(DF[,varname])  #change class back to factor
  DF[,varname] #prints out variable 
}

# DF[,varname][DF[,varname5]==(grepl("PrEP Trial", DF$varname5))] <- "PrEP Trial"

#once the function is done, save it on a script.
# use source func to call my funct
#source("file.path")


# DF = name of data frame // varname = variable that needs to be recoded (Area) // varname2 = variable that is used for conditioning (Region)
recodeContraception <- function(DF,varname,varname2){
  DF[,varname] <- as.character(DF[,varname]) #change class to character
  
  DF[,varname][(DF[,varname2]=="Bexley"|DF[,varname2]=="Southwark"|
                  DF[,varname2]=="Lambeth" | DF[,varname2]=="Lewisham" | DF[,varname2]=="Bromley")] <- "London"
  
  DF[,varname][DF[,varname2]=="Bradford"] <- "Bradford"
  DF[,varname][(DF[,varname2]=="Blackburn with Darwen")] <- "Blackburn"
  DF[,varname][(DF[,varname2]=="Bury")] <- "Bury"
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
  DF[,varname][DF[,varname2]=="Oldham"] <- "Oldham"
  DF[,varname][DF[,varname2]=="Rochdale"] <- "Rochdale"
  DF[,varname][DF[,varname2]=="Rotherham"] <- "Rotherham"
  DF[,varname][DF[,varname2]=="Rutland"] <- "Rutland"
  DF[,varname][(DF[,varname2]=="Southend-on-Sea")] <- "Southend"
  DF[,varname][DF[,varname2]=="Staffordshire"] <- "Staffordshire"
  DF[,varname][DF[,varname2]=="Telford and Wrekin"] <- "Telford and Wrekin"
  DF[,varname][DF[,varname2]=="Teesside"] <- "Teesside"
  DF[,varname][DF[,varname2]=="Thurrock"] <- "Thurrock"
  DF[,varname][DF[,varname2]=="Warrington"] <- "Warrington"
  DF[,varname][DF[,varname2]=="Wirral"] <- "Wirral"
  
  DF[,varname] <- as.factor(DF[,varname])  #change class back to factor
  DF[,varname] #prints out variable 
}

