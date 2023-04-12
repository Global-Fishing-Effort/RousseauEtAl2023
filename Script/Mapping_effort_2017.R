# Mapping: Joining effort and catch data by functional group for mapping

library(ncdf4)
library(tidyverse)
library(foreach)
library(doParallel)
library(parallel)
library(arrow)

# Part 1: Commented out code below adds Family and Gear Code to the country-level Effort capacity data to join with Catch data.
# Creates file: "Data/Final_DataStudyFAO_AllGears_wCode.csv"

# Part 2: For each year, joins the Catch and Effort data and creates files
# in folder: /rd/gem/private/users/yannickr/effort_premapped/
# Those large files are then used in "Script/Mapping_part2.R" to carry out the spatial mapping

# -------------------------Part 1: 
# #setwd("C:/Users/yannickr/OneDrive - University of Tasmania/FAO_Work_2020")
SAUPtoC<-read.csv("Data/SAUPcode_to_Country.csv")
CtoSAUP<-read.csv("Data/Country_to_SAUPcode.csv")
# All<-read.csv("Data/Final_DataStudyFAO_AllGears.csv")
# #final version here:
# All<-read.csv(file="https://data.imas.utas.edu.au/attachments/1241a51d-c8c2-4432-aa68-3d2bae142794/CapacityCountryLevel_Detailed.csv")[,-1]
# 
#CellstoEEZ<-read.csv("C:/Users/yannickr/OneDrive - University of Tasmania/Collaborations/Reg/CellEEZ.csv")
CellstoEEZ<-read.csv("Data/Cells_LatLon_EEZ.csv")
 
#TaxFG<-read.csv("C:/Users/yannickr/OneDrive - University of Tasmania/Collaborations/Reg/TaxonUsedDesc.csv")
TaxFG<-read.csv("Data/TaxonUsedDesc.csv")

# #setwd("C:/Users/yannickr/OneDrive - University of Tasmania/FishMip/Effort")
# #CellList<-read.csv("YannickCells.csv")
CellList<-read.csv("Data/YannickCells.csv")


All<-read.csv("Data/Final_DataStudyFAO_AllGears_wCode.csv")

WorldEEZ<-read.csv("Data/WorldEEZ.csv")
# # adding the SAUP
{
  WorldEEZ$SAUP<-CtoSAUP$SAUP_Country_Nbr[match(WorldEEZ$FAOname,CtoSAUP$Country,nomatch=NA,incomparables = NULL)]


  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Amer Samoa" ,"US (Alaska)" , "N Marianas","US Virgin Is")]<-840 # USA
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Channel Is","Br Virgin Is","Cayman Is", "Falkland Is" ,"Pitcairn Is" ,"St Helena"  ,"Turks Caicos", "Br Ind Oc Tr" )]<- 826 #UK
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c( "Macau" )]<- 156 # Macao in China
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("St Pier Mq",  "Kerguelen Is")]<-250 # France
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("NethAntilles")]<-528 # Netherlands
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Tokelau")]<- 554 #NZ
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Serbia Montenegro")]<-892 # Montenegro
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Gaza Strip")]<-376 # Israel
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Norfolk I." )]<-36 # Australia
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("West Sahara")]<-504 # Morocco
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c("Svalbard Is")]<-578 # Norway
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c(  "Faeroe Is")]<-234
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c( "St Kitts Nev" )]<-659
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c( "Fr Guiana" )]<- 254
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c( "Fr Polynesia"   )]<-258
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c( "Cook Is." )]<-184
  WorldEEZ$SAUP[WorldEEZ$FAOname %in% c( "Wallis Fut I" )]<-876
  unique(WorldEEZ$FAOname[!WorldEEZ$SAUP %in% All$SAUP]) # only the high seas. ###flagging not only the high seas now
}
# 
# # adding a "Coastal" twinge to cellist
# {
#   CellList$Coastal<-"Open"
#   CellList$minDist<-CellList$LandDist-(sqrt(CellList$Area)/2)
#   CellList$Coastal[CellList$minDist<371]<-"EEZ"
#   CellList$Coastal[CellList$minDist<70]<-"SemiCoastal"
#   CellList$Coastal[CellList$minDist<45]<-"Coastal"
#   CellList$Coastal[CellList$minDist<23]<-"UP"
#   
# }
# 
# # Family of gears for the Effort
# {
#   
#   All$Family<-NA
#   All$Family[All$GearCode %in% c("HL","PL","LL")]<-"HL"
#   All$Family[All$GearCode %in% c("DR","TRS","TR")]<-"DT"
#   All$Family[All$GearCode %in% c("TP")]<-"TP"
#   All$Family[All$GearCode %in% c("PS","PST","SE")]<-"SN"
#   All$Family[All$GearCode %in% c("GN","LN","FN")]<-"NT"
#   All$Family[All$GearCode %in% c("Tuna","HLT","PLT","LLT","PST")]<-"Tuna"
#   All$Family[All$Tuna=="Yes"]<-"Tuna"
#   All$Family[All$GearCode %in% c("OT","GU","MB","Multiple")]<-"OT"
#   All$Family[is.na( All$Family)]<-"OT"
#   
#   
# }
# 
# 
# 
# All$Code<-row.names(All)
# write.csv(All,"Final_DataStudyFAO_AllGears_wCode.csv")



#could speed things up here by running the loop in parallel

cores <- detectCores()-1
cluster <- makeCluster(cores)
registerDoParallel(cluster)

#----------------- Part 2:

foreach(y=c(1980:1991)) %dopar% {

  
  # Read the Catch, add some elements
  {
  Y<-floor(y*0.2)/0.2
  yearlow<-Y
  yearhigh<-yearlow+4
  #val<-paste0("C:/Users/yannickr/OneDrive - University of Tasmania/Collaborations/Reg/Catch_2019/Catch",
  #            yearlow,"_",yearhigh,".csv")
  
  val<-paste0("https://data.imas.utas.edu.au/attachments/Watson_Global_Fisheries_2020/Catch",
              yearlow,"_",yearhigh,".csv")
  
  message("Processing data for ", y)
  
  Catch<- data.table::fread(val) |> as.data.frame() 
  
  Catch<- Catch |> filter(IYear == y)
  
  Catch$SAUP<- Catch$CNumber
  Catch$Year<-Catch$IYear
  Catch$GearCode[Catch$Gear %in% c(0:399)]<-"OT" #hand and dynamite
  Catch$GearCode[Catch$Gear %in% c(400:439)]<-"HL" #will need to separate in LL and PL after.
  Catch$GearCode[Catch$Gear %in% c(440:449)]<-"PLT"
  Catch$GearCode[Catch$Gear %in% c(450:499)]<-"LLT"
  Catch$GearCode[Catch$Gear %in% c(500:599)]<-"TP"
  Catch$GearCode[Catch$Gear %in% c(700:701)]<-"LN" #bag nets
  Catch$GearCode[Catch$Gear %in% c(830:831)]<-"DR"
  Catch$GearCode[Catch$Gear %in% c(840)]<-"TRS" #bottom trawl
  Catch$GearCode[Catch$Gear %in% c(850)]<-"TR" #midwater trawl
  Catch$GearCode[Catch$Gear %in% c(900:922)]<-"SE"
  Catch$GearCode[Catch$Gear %in% c(1000,1010,1020:1030)]<-"PS"
  Catch$GearCode[Catch$Gear %in% c(1050)]<-"PST"
  Catch$GearCode[Catch$Gear %in% c(1100:1300)]<-"LN"
  Catch$GearCode[Catch$Gear %in% c(1320)]<-"FN"
  Catch$GearCode[Catch$Gear %in% c(1400:1540)]<-"GN"
  Catch$GearCode[is.na(Catch$GearCode)]<-"GU"
  
  # associate the Tuna and billfish
  Catch$Tuna<-"No"
  Catch$Tuna[Catch$Taxonkey %in% c(100036,400416,501163,501905,600093,600094,600097,600106,
                                   600107,600142,600143,600144,600145,600146,600147,600148,
                                   614290,100036,400419,600220,603915,600096,600098,600226,
                                   600216,600217,600218,600219,600220,600223)]<-"Yes"
  
  Catch$Family<-NA
  Catch$Family[Catch$GearCode %in% c("HL","PL","LL")]<-"HL"
  Catch$Family[Catch$GearCode %in% c("DR","TRS","TR")]<-"DT"
  Catch$Family[Catch$GearCode %in% c("TP")]<-"TP"
  Catch$Family[Catch$GearCode %in% c("PS","PST","SE")]<-"SN"
  Catch$Family[Catch$GearCode %in% c("GN","LN","FN")]<-"NT"
  Catch$Family[Catch$GearCode %in% c("Tuna","HLT","PLT","LLT","PST")]<-"Tuna"
  Catch$Family[Catch$Tuna=="Yes"]<-"Tuna"
  Catch$Family[Catch$GearCode %in% c("OT","GU","MB","Multiple")]<-"OT"
  Catch$Family[is.na( Catch$Family)]<-"OT"
  
  
  
  Catch$FG<-TaxFG$Descript[match(Catch$Taxonkey,TaxFG$Taxonkey,nomatch=NA,incomparables = NULL)]
  if(length(Catch$Taxonkey[is.na(Catch$FG)])!=0){message (paste0( "Error with functional groups, ",y)  )  } #0 good


  sumCatchA<-sum(Catch$ReportedNIND,na.rm=T)
  sumCatchI<-sum(Catch$ReportedIND,na.rm=T)
  
  
  Catch<-aggregate(Catch[c( "ReportedIND" , "IUUIND","DiscardsIND",  "ReportedNIND", "IUUNIND" ,"DiscardsNIND")],
                   by=Catch[c("Cell", "Family","FG", "GearCode","SAUP" )],FUN=sum,na.rm=T )

  
}

# Yug and USSR.
# Yug (890 is (705,191,892))
# USSR (810) is c(233,268,428,440,804,643)
  Copy<-Catch
  Catch<-Copy
{
  Yug<-subset(Catch,SAUP==890)
  if (length(Yug$Cell)==0){Yug<-Catch[1,]
  Yug[,names(Yug) %in% c( "ReportedIND" , "IUUIND","DiscardsIND",  "ReportedNIND", "IUUNIND" ,"DiscardsNIND")]<-0

  }
  

  for (i in c(705,191,892)){
    
    temp<-subset(All, SAUP==i)
    tempI<-subset(temp,Sector=="I")
    tempA<-subset(temp,Sector!="I")
    
    suppressWarnings({

      try(  tempI<-aggregate(tempI[c("NomEffort")],by=tempI[c("Year")],FUN=sum,na.rm=T))
      try( tempA<-aggregate(tempA[c("NomEffort")],by=tempA[c("Year")],FUN=sum,na.rm=T))
      
      try(  x<-tempI$NomEffort[tempI$Year==y])
      try(  z<-tempA$NomEffort[tempA$Year==y])
      if(length(x)==0){x<-0}
      if(length(z)==0){z<-0}
      
      Yug$x<-x
      Yug$z<-z
      
      
      names(Yug)[names(Yug)=="x"]<-paste0("I_",i)
      names(Yug)[names(Yug)=="z"]<-paste0("A_",i)
      
      rm(x,z)
      try(rm(tempA))
      try(rm(tempI))
    })

  }
  
  Yug$sumI<-apply(Yug[,names(Yug) %in% c(paste0("I_",c(705,191,892)))],1,sum,na.rm=T)
  Yug$sumA<-apply(Yug[,names(Yug) %in% c(paste0("A_",c(705,191,892)))],1,sum,na.rm=T)
  
  Yug705<-Yug[,names(Yug) %in% names(Catch)]
  Yug705[,names(Yug705) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- Yug705[,names(Yug705) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  Yug$I_705/Yug$sumI
  Yug705[,names(Yug705) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- Yug705[,names(Yug705) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  Yug$A_705/Yug$sumA
  Yug705$SAUP<-705
  
  Yug191<-Yug[,names(Yug) %in% names(Catch)]
  Yug191[,names(Yug191) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- Yug191[,names(Yug191) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  Yug$I_191/Yug$sumI
  Yug191[,names(Yug191) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- Yug191[,names(Yug191) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  Yug$A_191/Yug$sumA
  Yug191$SAUP<-191
  
  Yug892<-Yug[,names(Yug) %in% names(Catch)]
  Yug892[,names(Yug892) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- Yug892[,names(Yug892) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  Yug$I_892/Yug$sumI
  Yug892[,names(Yug892) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- Yug892[,names(Yug892) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  Yug$A_892/Yug$sumA
  Yug892$SAUP<-892
  
  
  if(sum(Yug$ReportedIND,na.rm=T) >0.1){
    
    if( abs(1-(sum(Yug705$ReportedIND,na.rm=T)+  sum(Yug191$ReportedIND,na.rm=T)+  sum(Yug892$ReportedIND,na.rm=T)) / sum(Yug$ReportedIND,na.rm=T)) >0.001) {
      message (paste0("Problem Yugoslavia, ",y))
    }
    
    
  }

  USSR<-subset(Catch,SAUP %in%  c(233,268,428,440,804,643, 810))
  if (y>=1989){USSR<-Catch[1,]
  USSR[,names(USSR) %in% c( "ReportedIND" , "IUUIND","DiscardsIND",  "ReportedNIND", "IUUNIND" ,"DiscardsNIND")]<-0
  }

  if (length(USSR$Cell)==0){USSR<-Catch[1,]
  USSR[,names(USSR) %in% c( "ReportedIND" , "IUUIND","DiscardsIND",  "ReportedNIND", "IUUNIND" ,"DiscardsNIND")]<-0
  
  }
  {
    for (i in c(233,268,428,440,804,643)){
      temp<-subset(All, SAUP==i)
      tempI<-subset(temp,Sector=="I")
      tempA<-subset(temp,Sector!="I")
      
      suppressWarnings({
        
        try(tempI<-aggregate(tempI[c("NomEffort")],by=tempI[c("Year")],FUN=sum,na.rm=T))
        try( tempA<-aggregate(tempA[c("NomEffort")],by=tempA[c("Year")],FUN=sum,na.rm=T))
        
        try(  x<-tempI$NomEffort[tempI$Year==y])
        try(  z<-tempA$NomEffort[tempA$Year==y])
        if(length(x)==0){x<-0}
        if(length(z)==0){z<-0}
        
        USSR$x<-x
        USSR$z<-z
        
        
        names(USSR)[names(USSR)=="x"]<-paste0("I_",i)
        names(USSR)[names(USSR)=="z"]<-paste0("A_",i)
        
        rm(x,z)
        try(rm(tempA))
        try(rm(tempI))
      })
      
      
      
      
    }
    USSR$sumI<-apply(USSR[,names(USSR) %in% c(paste0("I_",c(233,268,428,440,804,643)))],1,sum,na.rm=T)
    USSR$sumA<-apply(USSR[,names(USSR) %in% c(paste0("A_",c(233,268,428,440,804,643)))],1,sum,na.rm=T)
    
    USSR233<-USSR[,names(USSR) %in% names(Catch)]
    USSR233[,names(USSR233) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- USSR233[,names(USSR233) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  USSR$I_233/USSR$sumI
    USSR233[,names(USSR233) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- USSR233[,names(USSR233) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  USSR$A_233/USSR$sumA
    USSR233$SAUP<-233
    
    USSR268<-USSR[,names(USSR) %in% names(Catch)]
    USSR268[,names(USSR268) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- USSR268[,names(USSR268) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  USSR$I_268/USSR$sumI
    USSR268[,names(USSR268) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- USSR268[,names(USSR268) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  USSR$A_268/USSR$sumA
    USSR268$SAUP<-268
    
    USSR428<-USSR[,names(USSR) %in% names(Catch)]
    USSR428[,names(USSR428) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- USSR428[,names(USSR428) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  USSR$I_428/USSR$sumI
    USSR428[,names(USSR428) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- USSR428[,names(USSR428) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  USSR$A_428/USSR$sumA
    USSR428$SAUP<-428
    
    USSR440<-USSR[,names(USSR)  %in% names(Catch)]
    USSR440[,names(USSR440) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- USSR440[,names(USSR440) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  USSR$I_440/USSR$sumI
    USSR440[,names(USSR440) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- USSR440[,names(USSR440) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  USSR$A_440/USSR$sumA
    USSR440$SAUP<-440
    
    USSR804<-USSR[,names(USSR)  %in% names(Catch)]
    USSR804[,names(USSR804) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- USSR804[,names(USSR804) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  USSR$I_804/USSR$sumI
    USSR804[,names(USSR804) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- USSR804[,names(USSR804) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  USSR$A_804/USSR$sumA
    USSR804$SAUP<-804
    
    USSR643<-USSR[,names(USSR) %in% names(Catch)]
    USSR643[,names(USSR643) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]<- USSR643[,names(USSR643) %in% c("ReportedIND" , "IUUIND","DiscardsIND")]*  USSR$I_643/USSR$sumI
    USSR643[,names(USSR643) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]<- USSR643[,names(USSR643) %in% c("ReportedNIND" , "IUUNIND","DiscardsNIND")]*  USSR$A_643/USSR$sumA
    USSR643$SAUP<-643
    
    
    
    # sum(USSR$ReportedIND,na.rm=T)
    if(sum(USSR$ReportedIND,na.rm=T) >0.1){
    if( abs(1-(sum(USSR233$ReportedIND,na.rm=T)+  sum(USSR268$ReportedIND,na.rm=T)+  sum(USSR428$ReportedIND,na.rm=T)+
               sum(USSR643$ReportedIND,na.rm=T)+  sum(USSR440$ReportedIND,na.rm=T)+  sum(USSR804$ReportedIND,na.rm=T)) /
            sum(USSR$ReportedIND,na.rm=T)) >0.001) {
      message (paste0("Problem USSR, ",y))
    }
    }
    
    
  }

  Catch<-subset(Catch,!SAUP %in%  c(890,810))
  if(y<=1988){  Catch<-subset(Catch,!SAUP %in%  c(233,268,428,440,804,643))
}
  
  
  Catch<-rbind(Catch, Yug705,Yug191,Yug892,USSR233,USSR268,USSR428,USSR440,USSR804,USSR643)
  
  if ( abs(sum(Catch$ReportedNIND,na.rm=T)-sumCatchA)>0.1  ){message (paste0("Problem sum Catch Artisanal after Yug/USSR"),y)}
  if ( abs(sum(Catch$ReportedIND,na.rm=T)  -sumCatchI)>0.1  ){message (paste0("Problem sum Catch Industrial after Yug/USSR"),y)}
  
  
}
  
  #Putting Catch Art and I together
{  
  rm(Yug,USSR,Yug191,Yug705,Yug892,USSR233,USSR268,USSR428,USSR440,USSR643,USSR804)
  
  CatchA<-Catch[,!names(Catch) %in% c( "ReportedIND",  "IUUIND"  ,"DiscardsIND" )]
  CatchI<-Catch[,!names(Catch) %in% c( "ReportedNIND",  "IUUNIND"  ,"DiscardsNIND" )]
  
  names(CatchA)[names(CatchA) %in% c("ReportedNIND", "IUUNIND","DiscardsNIND")]<-c("Reported","IUU","Discards")
  names(CatchI)[names(CatchI) %in% c("ReportedIND", "IUUIND","DiscardsIND")]<-c("Reported","IUU","Discards")
  CatchA$Sector<-"AUP"
  CatchI$Sector<-"I"
  CatchY<-rbind(CatchA,CatchI)
  }
  
  # need to recode all the ones where the country code cold be different.
{
    # 16 Am. samoa
  # 92 British Virgin Islands
  # 136 Cayman Isl
  # 238 Falklands
  # 292 Gibralatar
  # 446 Macao
  # 474 Martinique
  # 500 Montserrat
  # 532 Neth. Antilles
  # 533 Aruba
  # 580 North. Mar. Isl
  # 612 Pitcairn
  # 654 St helena
  # 666 St P. Miq. 
  # 772 Tokelau
  # 796 Turks Caicos
  # 830 Channel Isl.
  # 833 Isle of Man
  # 850 US Virgin Isl.
  
  # 70 Bosnia and Herzegovina
  # 86 British Indian Ocean Ter
  # 423 Palestine
  # 492 Monaco
  # 660 Anguilla
  # 999 Others
  
  # 574 Norfolk Isl
  # 674 San Marino
  # 732 Western sahara
  # 744 Svalbard and Jan Mayen
  # 891 Serbia / Serbia - Montenegro

  CatchY$SAUP[CatchY$SAUP %in% c(16,580,850)]<-840 # USA
  CatchY$SAUP[CatchY$SAUP %in% c(92,136, 238, 292,500,612,654,796,830,833, 86 , 660)]<- 826 #UK
  CatchY$SAUP[CatchY$SAUP %in% c(446)]<- 156 # Macao in China
  CatchY$SAUP[CatchY$SAUP %in% c(474, 666, 492)]<-250 # France
  CatchY$SAUP[CatchY$SAUP %in% c(532,533)]<-528 # Netherlands
  CatchY$SAUP[CatchY$SAUP %in% c(772)]<- 554 #NZ
  CatchY$SAUP[CatchY$SAUP %in% c(70,891)]<-892 # Montenegro
  CatchY$SAUP[CatchY$SAUP %in% c(423)]<-376 # Israel
  CatchY$SAUP[CatchY$SAUP %in% c(574)]<-36 # Australia
  CatchY$SAUP[CatchY$SAUP %in% c(674)]<-380 # Italy
  CatchY$SAUP[CatchY$SAUP %in% c(732)]<-504 # Morocco
  CatchY$SAUP[CatchY$SAUP %in% c(744)]<-578 # Norway
  
}
  
  
  # add eff effort
 {
   EffortY<-subset(All,Year==y)
   # Madeira/ Acores never have catch, so considering them to be with Portugal.
   EffortY$SAUP[EffortY$SAUP %in% c(621,622)]<-620
   
  EffortY$EffEffortLin<-EffortY$NomEffort * (1+EffortY$CreepLinCompounded/100)
  EffortY$EffEffortLog<-EffortY$NomEffort * (1+EffortY$CreepLogCompounded/100)
  
  EffortY<-EffortY[,names(EffortY) %in% c("Year","Length_Category","Sector","SAUP", "GearCode","Family","NV","GT","P",
                                          "NVActive","GTActive","PActive","NomEffort", "NomEffortActive",
                                          "EffEffortLin", "EffEffortLog","CreepLinCompounded","CreepLogCompounded","Code"  ) ]
  
  
  EffortY$Sector2<-"I"
  EffortY$Sector2[EffortY$Sector!="I"]<-"AUP"

 } 
  
  # making a list of countries where there is effort but no catch.
  # no need for separation I / AUP anymore, since use the same after.
 { 
   # Remove all the ones where there is nothing
   CatchY<-CatchY[CatchY$Reported>0 | CatchY$IUU>0 | CatchY$Discards>0,]
   # CatchI<-subset(CatchY,Sector=="I")
   # CatchA<-subset(CatchY,Sector=="AUP")

  ListCountriesNoCatch<-setdiff(unique(EffortY$SAUP),unique(CatchY$SAUP))
  MissingEffort<-subset(EffortY,SAUP %in% ListCountriesNoCatch)
  path<-file.path(paste0("/rd/gem/private/users/yannickr/Missing_Effort/Effort_",y,".csv")) #this is a potential place to shift files to research portal or to create parquest within project
  write.csv(MissingEffort,path)
  EffortY<-subset(EffortY,!SAUP %in% ListCountriesNoCatch)
  
 }
  
  
  # going to do something with the ones where there is effort bu no Catch (for a country. for a sector, if A, using only the EEZ.)
  # for now, will just 
  # will take the (mapped) effort of a later year and associate (maybe 2-3 average.)
  
  
  CatchY$Event<-rownames(CatchY)
  
  # first need to make sure that there is some artisanal (some countries don't)
  # looking at the total effort of the AUP and total effort of industrial.
  # if the ratio effort AUP/TOtal ismore than 5 times the ratio Catch AUP / Catch total, do something.
  { 
  CTot<-aggregate(CatchY[c("Reported")],by=CatchY[c("SAUP")],FUN=sum,na.rm=T)
  CAUP<-aggregate(CatchY[c("Reported")],by=CatchY[c("SAUP","Sector")],FUN=sum,na.rm=T)
  CAUP<-CAUP[CAUP$Sector=="AUP",]
  CTot$AUP<-CAUP$Reported[match(CTot$SAUP,CAUP$SAUP,nomatch=NA,incomparables = NULL)]
  CTot$ratio<-CTot$AUP/CTot$Reported
  
  ETot<-aggregate(EffortY[c("NomEffort")],by=EffortY[c("SAUP")],FUN=sum,na.rm=T)
  EAUP<-aggregate(EffortY[c("NomEffort")],by=EffortY[c("SAUP","Sector2")],FUN=sum,na.rm=T)
  EAUP<-EAUP[EAUP$Sector2=="AUP",]
  ETot$AUP<-EAUP$NomEffort[match(ETot$SAUP,EAUP$SAUP,nomatch=NA,incomparables = NULL)]
  ETot$ratio<-ETot$AUP/ETot$NomEffort
  ETot$ratioCAtch<-CTot$ratio[match(ETot$SAUP,CTot$SAUP,nomatc=NA,incomparables = NULL)]
  ETot$Ok<-"No"
  ETot$Ok[ETot$ratioCAtch>0.2 * ETot$ratio & ETot$ratioCAtch<5 * ETot$ratio]<-"Yes"
  listGoods<-unique(ETot$SAUP[ETot$Ok=="Yes"])
  CatchY$Sector[!CatchY$SAUP %in% listGoods]<-"I"
  }
  
  # doing the codes for association
  {
    
    CatchY$IDGearType<-paste0(CatchY$SAUP,"_",CatchY$GearCode,"_",CatchY$Sector)
    EffortY$IDGearType<-paste0(EffortY$SAUP,"_",EffortY$GearCode,"_",EffortY$Sector2)
    CatchY$IDFamType<-paste0(CatchY$SAUP,"_",CatchY$Family,"_",CatchY$Sector)
    EffortY$IDFamType<-paste0(EffortY$SAUP,"_",EffortY$Family,"_",EffortY$Sector2)
    CatchY$IDCountryType<-paste0(CatchY$SAUP,"_",CatchY$Sector)
    EffortY$IDCountryType<-paste0(EffortY$SAUP,"_",EffortY$Sector2)
    CatchY$IDCountryNoType<-paste0(CatchY$SAUP)
    EffortY$IDCountryNoType<-paste0(EffortY$SAUP)
  }
  
  
  # associating catch events with effort, based on gear.
  {
    EffortY$EventGear<-NA
    EffwithoutcatchGear<-setdiff(unique(EffortY$IDGearType),unique(CatchY$IDGearType)) 
    # the gear ID where there is no associated catch
    CannotuseGear<-unique(EffortY$IDFamType[EffortY$IDGearType %in% EffwithoutcatchGear]) 
    # the family ID where there is no associated catch with at least one gear.
    listeffGear<-intersect(unique(EffortY$IDGearType[!EffortY$IDFamType %in% CannotuseGear]),
                           unique(CatchY$IDGearType)) # only the gears which are valid. not that many...
    # all the Gear Id where there is something.
    CatchwithoutEffGear<-setdiff(unique(CatchY$IDGearType),
                                 unique(EffortY$IDGearType[!EffortY$IDFamType %in% CannotuseGear]))
    # giving an event to all effort that I can, but no need to give them to the ones where family is wrong.
    for (i in listeffGear[1:length(listeffGear)]){
      EffortY$EventGear[EffortY$IDGearType==i]<-list(CatchY$Event[which(!is.na(match(CatchY$IDGearType,i)))])
    }
    # remove 0.
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventGear"],1,function(i){length(unlist(i))})
    EffortY$length[is.na(EffortY$EventGear)]<-0
    EffortY$EventGear[EffortY$length==0]<-NA
    
  }
  
  # same, with family.
  {
    EffortY$EventFam<-NA
    EffwithoutcatchFam<-setdiff(unique(EffortY$IDFamType),unique(CatchY$IDFamType))
    # the fam ID where there is no associated catch
    CannotuseFam<-unique(EffortY$IDCountryType[EffortY$IDFamType %in% EffwithoutcatchFam]) 
    # the Country ID where there is no associated catch with at least one family.
    
    listeffFam<-intersect(unique(EffortY$IDFamType[!EffortY$IDCountryType %in% CannotuseFam]),
                          unique(CatchY$IDFamType))# onlythe fam that are valid.
    CatchwithoutEffFam<-setdiff(unique(CatchY$IDFamType),
                                unique(EffortY$IDFamType[!EffortY$IDCountryType %in% CannotuseFam]))
    for (i in listeffFam){
      EffortY$EventFam[EffortY$IDFamType==i]<-list(CatchY$Event[which(!is.na(match(CatchY$IDFamType,i)))])
    }
    # remove 0.
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventFam"],1,function(i){length(unlist(i))})
    EffortY$length[is.na(EffortY$EventFam)]<-0
    EffortY$EventFam[EffortY$length==0]<-NA
  }
  
  # for the rest, using country data type.
  {
    listeffCountry<-unique(EffortY$IDCountryType[EffortY$IDFamType %in% CatchwithoutEffFam | (is.na(EffortY$EventGear) & is.na(EffortY$EventFam))])
    EffortY$EventCountry<-NA
    for (i in listeffCountry){
      EffortY$EventCountry[EffortY$IDCountryType==i]<-list(CatchY$Event[which(!is.na(match(CatchY$IDCountryType,i)))])
    }
    # remove 0.
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventCountry"],1,function(i){length(unlist(i))})
    EffortY$length[is.na(EffortY$EventCountry)]<-0
    
    EffortY$EventCountry[EffortY$length==0]<-NA
    
  }
  
  # country no type 
  {
    listeffCountryNT<-unique(EffortY$IDCountryNoType[is.na(EffortY$EventCountry) & is.na(EffortY$EventGear) & is.na(EffortY$EventFam)])
    EffortY$EventCountryNT<-NA
    for (i in listeffCountryNT){
      EffortY$EventCountryNT[EffortY$IDCountryNoType==i]<-list(CatchY$Event[which(!is.na(match(CatchY$IDCountryNoType,i)))])
    }
    # remove 0.
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventCountryNT"],1,function(i){length(unlist(i))})
    EffortY$length[is.na(EffortY$EventCountryNT)]<-0
    
    EffortY$EventCountryNT[EffortY$length==0]<-NA
    
    # remove the country where using countryNT
    EffortY$EventCountry[is.na(EffortY$EventCountryNT)==F]<-NA
    
    # remove the family where using country/countryNT
    EffortY$EventFam[is.na(EffortY$EventCountry)==F | is.na(EffortY$EventCountryNT)==F]<-NA
    
    # remove gear where using country/CNT or family
    EffortY$EventGear[is.na(EffortY$EventCountry)==F | is.na(EffortY$EventFam)==F | is.na(EffortY$EventCountryNT)==F]<-NA
    
  }
  
  # and putting it together
  {
    EffortY$AllEvents<-NA
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventCountryNT"],1,function(i){length(unlist(i))})
    EffortY$AllEvents[is.na(EffortY$AllEvents) & EffortY$length!=0]<-
      EffortY$EventCountryNT[is.na(EffortY$AllEvents) & EffortY$length!=0]
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventCountry"],1,function(i){length(unlist(i))})
    EffortY$AllEvents[is.na(EffortY$AllEvents) & EffortY$length!=0]<-
      EffortY$EventCountry[is.na(EffortY$AllEvents) & EffortY$length!=0]
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventFam"],1,function(i){length(unlist(i))})
    EffortY$AllEvents[is.na(EffortY$AllEvents) & EffortY$length!=0]<-
      EffortY$EventFam[is.na(EffortY$AllEvents) & EffortY$length!=0]
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="EventGear"],1,function(i){length(unlist(i))})
    EffortY$AllEvents[is.na(EffortY$AllEvents) & EffortY$length!=0]<-
      EffortY$EventGear[is.na(EffortY$AllEvents) & EffortY$length!=0]
    EffortY$length<-NA
    EffortY$length<-apply(EffortY[names(EffortY)=="AllEvents"],1,function(i){length(unlist(i))})
    
    test<-subset(EffortY,length==0) #should be 0.
    if(length(test$Year)>0){message(paste0("problem events year ",y) )}
    # side note, the MB and MBT are still in here.

  }
  
  # Adding a Code to keep only quick and easy data
{  # listevents<-unique(unlist(EffortY$AllEvents)) 
  MissingEvents<-setdiff(unique(CatchY$Events),unique(unlist(EffortY$AllEvents))) #1475
  if(length(MissingEvents)>0){message(paste0("problem missing events year ",y) )}
  
  rm(Catch, CannotuseFam,CannotuseGear,CatchwithoutEffFam,CatchwithoutEffGear,EffwithoutcatchFam,EffwithoutcatchGear)
  
  
  
  EffortEvents<-EffortY[,!names(EffortY) %in% c("IDCountryNoType","AddedEvents2",
                                                "IDGearType","IDFamType","EventGear",        
                                                "EventFam","IDCountryType","EventCountry","EventCountryNT")]
  
  
  rm(EffortY)
  rm(Copy)
names(EffortEvents)[names(EffortEvents)=="length"]<-"Length"  
  
EffortCode<-EffortEvents
# EffortCode$Code<-rownames(EffortCode)
EffortEvents<-EffortCode[,names(EffortCode) %in% c("Length", "AllEvents","Code","SAUP")]
}

  # first, separate events
message ("about to separate events, takes a while")

for (z in 1:9){
  
  start<-(z-1)*100 +1
  end<-(z*100)
  
  EffortEventsSAUP<-subset(EffortEvents,SAUP %in% c(start:end))
  
  
  SeparatedEventsY<-EffortEventsSAUP[1,]
  SeparatedEventsY[,]<-NA
  
  for (i in 1:length(EffortEventsSAUP$SAUP)){
    temp<-EffortEventsSAUP[i,]
    length<-temp$Length
    if (length==0){
      badsY<-rbind(badsY,temp)
      next
    }
    
    AllEvents<-unlist(temp$AllEvents)
    temp2<-temp[rep(row.names(temp), length),]
    temp2$AllEvents<-AllEvents
    temp2$Length<-length
    SeparatedEventsY<-rbind(SeparatedEventsY,temp2)
    rm(temp,temp2,AllEvents,length)
    
  }
  
  SeparatedEventsY<-SeparatedEventsY[!is.na(SeparatedEventsY$Length),]
  rm(EffortEventsSAUP)
  
  SeparatedEventsY<-SeparatedEventsY[,names(SeparatedEventsY)!="SAUP"]
  
  
  
  # associate the cells, and remove the ones which are not possible.
  {
    SeparatedEventsY$Cell<-CatchY$Cell[match(SeparatedEventsY$AllEvents,CatchY$Event,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$Coastal<-CellList$Coastal[match(SeparatedEventsY$Cell,CellList$Seq,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$Distance<-CellList$minDist[match(SeparatedEventsY$Cell,CellList$Seq,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$Depth<-CellList$Bathy_Min[match(SeparatedEventsY$Cell,CellList$Seq,nomatch=NA,incomparables = NULL)]
    
    SeparatedEventsY$EEZ<-WorldEEZ$SAUP[match(SeparatedEventsY$Cell,WorldEEZ$Seq,nomatch=NA,incomparables = NULL)]
    # remove the distance pbm (distance to the land.)
    # remove the ones where fishing occurs in a different region (might remove some, but ok.)
    
    SeparatedEventsY$Sector<-EffortCode$Sector[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$Gear<-EffortCode$GearCode[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$SAUP<-EffortCode$SAUP[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    
    
    SeparatedEventsY$okEEZ<-"No"
    SeparatedEventsY$okEEZ[SeparatedEventsY$EEZ==SeparatedEventsY$SAUP]<-"Yes"
    SeparatedEventsY$okEEZ[SeparatedEventsY$Sector=="I"]<-"Yes"
    
    temp2<-SeparatedEventsY
    #remove the bad EEZ for the AUP
    temp2$okEEZ[temp2$SAUP==534]<-"Yes" #Curacao doesn't have an EEZ
    
    test<-setdiff(unique(temp2$Code),unique(temp2$Code[temp2$okEEZ=="Yes"]))
    
    # for the ones where there is no okEEZ, but an effort, using the EEZ of the country and each cell is given a value
    # need to also add a Catch equivalent.
    if(length(test)!=0){
      temp<-subset(temp2,Code %in% test & Sector %in% c("APW","UP"))
      AUPSAUP<-unique(temp$SAUP)
      Event<-1
      
      AddSeparated<-SeparatedEventsY[1,]
      AddSeparated[,]<-NA
      AddCatch<-CatchY[1,]
      AddCatch[,]<-NA
      emptyCatch<-AddCatch
      
      for (c in unique(temp$Code)){
        EventCode<-paste0("Missing_AUP_",Event)
        tempCountry<-temp[temp$Code==c,][1,]
        CellsEEZ<-subset(WorldEEZ,SAUP==tempCountry$SAUP )
        CellsEEZ<-CellList[CellList$Seq %in% CellsEEZ$Seq,]
        
        tempCountry2<-tempCountry[rep(row.names(tempCountry), length(CellsEEZ$Seq)),]
        tempCountry2$AllEvents<-EventCode
        tempCountry2$Cell<-CellsEEZ$Seq
        tempCountry2$Coastal<-CellsEEZ$Coastal
        tempCountry2$Distance<-CellsEEZ$minDist
        tempCountry2$Depth<-CellsEEZ$Bathy_Min
        tempCountry2$EEZ<-tempCountry$SAUP
        tempCountry2$okEEZ<-"Yes"
        
        AddSeparated<-rbind(AddSeparated,tempCountry2)
        
        tempCatch<-emptyCatch[rep(row.names(emptyCatch), length(CellsEEZ$Seq)),]
        tempCatch$Cell<-CellsEEZ$Seq
        tempCatch$SAUP<-tempCountry$SAUP
        tempCatch$Reported<-CellsEEZ$Area
        tempCatch$IUU<-CellsEEZ$Area
        tempCatch$Discards<-CellsEEZ$Area
        tempCatch$Event<-paste0(EventCode,rownames(tempCatch))
        tempCatch$Sector<-tempCountry$Sector
        
        AddCatch<-rbind(AddCatch,tempCatch)
        rm(tempCatch)  
        rm(tempCountry,tempCountry2)
        
        Event<-Event+1
      }
      
      
      temp2<-temp2[!temp2$Code %in% test,]
      
      AddSeparated<-AddSeparated[is.na(AddSeparated$Length)==F,]
      AddCatch<-AddCatch[is.na(AddCatch$Cell)==F,]
      
      temp2<-rbind(temp2,AddSeparated)
      CatchY<-rbind(CatchY,AddCatch)
      
      
    }
    
    temp2<-temp2[temp2$okEEZ=="Yes",] #oofty, removes about half the data
    
    temp2$Baddistance<-"No"
    temp2$Baddistance[temp2$Sector %in% c("UP") & temp2$Coastal !="UP"]<-"Yes"
    temp2$Baddistance[temp2$Sector %in% c("APW") & !temp2$Coastal %in% c("UP","Coastal","SemiCoastal","EEZ")]<-"Yes"
    
    test<-setdiff(unique(temp2$Code),unique(temp2$Code[temp2$Baddistance=="No"]))
    if(length(test)!=0){
      for (n in test){
        tempN<-subset(temp2,Code==n)
        Distance<-min(tempN$Distance,na.rm=T)
        temp2$Baddistance[temp2$Code==n & temp2$Distance ==Distance]<-"No"
        rm(tempN,Distance)
      }
    }
    
    temp2<-temp2[temp2$Baddistance=="No",] # doesn't seem to remove much
    
    # not going to care too much about the depth.
    
    temp2<-temp2[,!names(temp2) %in% c("Distance","Depth","Baddepth","Baddistance","okEEZ","Regioncell","Coastal")]
  }
  
  # Copy<-SeparatedEventsY[,names(SeparatedEventsY) %in% c("Length" ,"AllEvents", "Code" ,  "Cell" )]
  SeparatedEventsY<-temp2
  
  # cahnge.
  # need to prorate first based on the GT of each (event) , and then all events (so that the fleet doesn't compete too much)
  {
    SeparatedEventsY$GTActive<-EffortCode$GTActive[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    
    SeparatedEventsY$prorateGT<-NA
    
    SeparatedEventsY<-SeparatedEventsY[is.na(SeparatedEventsY$AllEvents)==F,] # shouldn;t be any, but just in case.
    
    NormSep<-aggregate(SeparatedEventsY[c("GTActive")],
                       by=SeparatedEventsY[c("AllEvents")],FUN=sum,na.rm=T)
    
    SeparatedEventsY$sumGTActive<-NormSep$GTActive[match(SeparatedEventsY$AllEvents,NormSep$AllEvents,
                                                         nomatch=NA,incomparables=NULL)]
    
    SeparatedEventsY$prorateGT<-SeparatedEventsY$GTActive/SeparatedEventsY$sumGTActive
    
    SeparatedEventsY<-SeparatedEventsY[,!names(SeparatedEventsY) %in% c("sumGTActive") ]
    
  }
  
  # then prorate based on each Code, including the sum of all Catch
  {
    SeparatedEventsY$CatchEventReported<-NA
    SeparatedEventsY$CatchEventDiscards<-NA
    SeparatedEventsY$CatchEventIUU<-NA
    
    
    SeparatedEventsY$CatchEventReported<-CatchY$Reported[match(SeparatedEventsY$AllEvents,CatchY$Event,
                                                               nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$CatchEventDiscards<-CatchY$Discards[match(SeparatedEventsY$AllEvents,CatchY$Event,
                                                               nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$CatchEventIUU<-CatchY$IUU[match(SeparatedEventsY$AllEvents,CatchY$Event,
                                                     nomatch=NA,incomparables = NULL)]
    
    SeparatedEventsY$sum<-apply(SeparatedEventsY[,names(SeparatedEventsY) %in% c("CatchEventReported","CatchEventDiscards","CatchEventIUU")],1,sum,na.rm=T)
    NormSep<-aggregate(SeparatedEventsY[c("sum")],
                       by=SeparatedEventsY[c("Code")],FUN=sum,na.rm=T)
    
    SeparatedEventsY$sumCatchCode<-NormSep$sum[match(SeparatedEventsY$Code,NormSep$Code,
                                                     nomatch=NA,incomparables=NULL)]
    
    SeparatedEventsY$RatioCatchCOde<-SeparatedEventsY$sum/SeparatedEventsY$sumCatchCode
    
    SeparatedEventsY$ReportedCatch<-SeparatedEventsY$CatchEventReported*SeparatedEventsY$sum/SeparatedEventsY$sumCatchCode
    SeparatedEventsY$IUUCatch<-SeparatedEventsY$CatchEventIUU*SeparatedEventsY$sum/SeparatedEventsY$sumCatchCode
    SeparatedEventsY$DiscardsCatch<-SeparatedEventsY$CatchEventDiscards*SeparatedEventsY$sum/SeparatedEventsY$sumCatchCode
    
    
  }
  
  
  # Normalize
  {
    SeparatedEventsY$RatioEffort<-SeparatedEventsY$prorateGT*SeparatedEventsY$RatioCatchCOde
    
    NormSep<-aggregate(SeparatedEventsY[c("RatioEffort")],
                       by=SeparatedEventsY[c("Code")],FUN=sum,na.rm=T)
    
    SeparatedEventsY$sumRatioEffort<-NormSep$RatioEffort[match(SeparatedEventsY$Code,NormSep$Code,
                                                               nomatch=NA,incomparables=NULL)]
    
    
    SeparatedEventsY$RatioEffortNorm<-SeparatedEventsY$RatioEffort/SeparatedEventsY$sumRatioEffort
  }
  
  
  # then catch of each (= catch * prorate GT)
  {
    SeparatedEventsY$CatchReported<-SeparatedEventsY$CatchEventReported*SeparatedEventsY$prorateGT
    SeparatedEventsY$CatchIUU<-SeparatedEventsY$CatchEventIUU*SeparatedEventsY$prorateGT
    SeparatedEventsY$CatchDiscards<-SeparatedEventsY$CatchEventDiscards*SeparatedEventsY$prorateGT
    
    
    
  }
  
  
  # and the Effort, NV and so on.
  {
    # 
    # SeparatedEventsY$NomEffortTot<-EffortCode$NomEffort[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$NomEffortReported<-(SeparatedEventsY$CatchEventReported/SeparatedEventsY$sum)*SeparatedEventsY$NomEffortTot*SeparatedEventsY$RatioEffortNorm
    # SeparatedEventsY$NomEffortIUU<-(SeparatedEventsY$CatchEventIUU/SeparatedEventsY$sum)*SeparatedEventsY$NomEffortTot*SeparatedEventsY$RatioEffortNorm
    # SeparatedEventsY$NomEffortDiscards<-(SeparatedEventsY$CatchEventDiscards/SeparatedEventsY$sum)*SeparatedEventsY$NomEffortTot*SeparatedEventsY$RatioEffortNorm
    # 
    # SeparatedEventsY$NomEffortActive<-EffortCode$NomEffortActive[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$NomEffortReportedActive<-(SeparatedEventsY$CatchEventReported/SeparatedEventsY$sum)*SeparatedEventsY$NomEffortActive*SeparatedEventsY$RatioEffortNorm
    # SeparatedEventsY$NomEffortIUUActive<-(SeparatedEventsY$CatchEventIUU/SeparatedEventsY$sum)*SeparatedEventsY$NomEffortActive*SeparatedEventsY$RatioEffortNorm
    # SeparatedEventsY$NomEffortDiscardsActive<-(SeparatedEventsY$CatchEventDiscards/SeparatedEventsY$sum)*SeparatedEventsY$NomEffortActive*SeparatedEventsY$RatioEffortNorm
    # 
    # SeparatedEventsY$NV<-EffortCode$NV[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$NV<- SeparatedEventsY$NV*SeparatedEventsY$RatioEffortNorm
    # SeparatedEventsY$NVActive<-EffortCode$NVActive[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$NVActive<- SeparatedEventsY$NVActive*SeparatedEventsY$RatioEffortNorm
    # 
    # EffortCode$GTPV<-EffortCode$GT/EffortCode$NV
    # SeparatedEventsY$GTPV<-EffortCode$GTPV[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # EffortCode$PPV<-EffortCode$P/EffortCode$NV
    # SeparatedEventsY$PPV<-EffortCode$PPV[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # 
    # SeparatedEventsY$Length_Category<-EffortCode$Length_Category[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # 
    # 
    # SeparatedEventsY$Lat<-CellList$Lat[match(SeparatedEventsY$Cell,CellList$Seq,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$Lon<-CellList$Lon[match(SeparatedEventsY$Cell,CellList$Seq,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$FCountry<-SAUPtoC$ISO3[match(SeparatedEventsY$SAUP,SAUPtoC$SAUP_Country_Nbr,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$FGroup<-CatchY$FG[match(SeparatedEventsY$AllEvents,CatchY$Event,nomatch=NA,incomparables = NULL)]
    SeparatedEventsY$FGroup[is.na( SeparatedEventsY$FGroup)]<-"Unknown_EEZ"
    
    SeparatedEventsY$CatchReported[ SeparatedEventsY$FGroup=="Unknown_EEZ"]<-NA
    SeparatedEventsY$CatchIUU[ SeparatedEventsY$FGroup=="Unknown_EEZ"]<-NA
    SeparatedEventsY$CatchDiscards[ SeparatedEventsY$FGroup=="Unknown_EEZ"]<-NA
    
    # SeparatedEventsY$CreepLinCompounded<-EffortCode$CreepLinCompounded[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$CreepLogCompounded<-EffortCode$CreepLogCompounded[match(SeparatedEventsY$Code,EffortCode$Code,nomatch=NA,incomparables = NULL)]
    # SeparatedEventsY$CreepLinCompounded<- (1+SeparatedEventsY$CreepLinCompounded/100)
    # SeparatedEventsY$CreepLogCompounded<- (1+SeparatedEventsY$CreepLogCompounded/100)
    # 
    # 
    
    SeparatedEventsY<-SeparatedEventsY[,!names(SeparatedEventsY) %in% c("Length","AllEvents","EEZ","GTActive" ,"GT",
                                                                        "prorateGT","CatchEventReported","CatchEventDiscards",
                                                                        "CatchEventIUU","sumCatchCode","RatioCatchCOde",
                                                                        "ReportedCatch","IUUCatch", "DiscardsCatch","RatioEffort",
                                                                        "sumRatioEffort","NomEffortTot","SAUP","Gear","Sector"    )]
    
    
    
    
  }
  
  
  rm(list=setdiff(ls(), c("All","CellList","CellstoEEZ","CtoSAUP","SAUPtoC","TaxFG","WorldEEZ",
                          "SeparatedEventsY","SeparatedEventsY2","SeparatedEventsY3","SeparatedEventsY4",
                          "SeparatedEventsY5","SeparatedEventsY6","SeparatedEventsY7","SeparatedEventsY8",
                          "SeparatedEventsY9","y","CatchY","EffortCode","EffortEvents","MissingEffort","z",
                          "SeparatedEventsY1")))
  
  assign(paste0("SeparatedEventsY",z),SeparatedEventsY)
  rm(SeparatedEventsY)
  gc()
  message (z)
  }


SeparatedEventsY<-rbind(SeparatedEventsY1,SeparatedEventsY2,SeparatedEventsY3,SeparatedEventsY4,
                        SeparatedEventsY5,SeparatedEventsY6,SeparatedEventsY7,SeparatedEventsY8,SeparatedEventsY9)


rm(SeparatedEventsY1,SeparatedEventsY2,SeparatedEventsY3,SeparatedEventsY4,
   SeparatedEventsY5,SeparatedEventsY6,SeparatedEventsY7,SeparatedEventsY8,SeparatedEventsY9)


# path<-file.path(paste0("C:/Users/yannickr/OneDrive - University of Tasmania/FishMip/Effort/Premapped/premapped_",y,".csv"))
path<-file.path(paste0("Output/Premapped/premapped_",y,".csv"))

write.csv(SeparatedEventsY,path)

message(y)
message("done")


rm(list=setdiff(ls(), c("All","CellList","CellstoEEZ","CtoSAUP","SAUPtoC","TaxFG","WorldEEZ")))
gc()
}

stopCluster(cluster)






# tHE COASTAL aup HAS UNKNOWN FUNCTIONAL GROUPS AND EFFORT.
# then will need to add the ones where there is nothing,
# then average based on cell
  
  







# Comments YR:
# to fix: Curacao doesn't have an EEZ in World EEZ file
# 1988-1990, pbm yug/russia. to check

