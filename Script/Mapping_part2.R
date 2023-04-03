# Post mapping fishingmip.
setwd("C:/Users/yannickr/OneDrive - University of Tasmania/FishMip/")
assoc<-read.csv("Effort/Final_DataStudyFAO_AllGears_wCode.csv")
realEffort<-read.csv("../FAO_Work_2020/Final_DataStudyFAO_AllGears_V5.csv")
Cells<-read.csv("C:/Users/yannickr/OneDrive - University of Tasmania/Collaborations/Reg/Cells_LatLon_EEZ.csv")
Missingvals<-read.csv("Effort/Missing_Effort/Missing_all.csv")

gc()
# for (y in c(2017:2011))
  y<-2017
  
  {

path<-file.path(paste0("D:/Effort_2017/Premapped/premapped_",y,".csv"))
effort<-read.csv(path)


# 1) checking if there is any effort missing and associating it

# for these ones, just having them in the EEZ of said country.
# A few orthers have missing years for I: namibia (516) => giving them the value of further years.
# code to check which ones:
{
#   
# setwd("/Documents and Settings/yannickr/OneDrive - University of Tasmania/FishMip/Effort/Missing_Effort/")
# missing<-read.csv("Effort_1950.csv")
# for (y in 1951:2017){
#   path<-file.path(paste0("Effort_",y,".csv"))
#   toadd<-read.csv(path)
#   if (length(toadd$Year)<2){next}
#   
#   missing<-rbind(missing, toadd)
# }
# 
# write.csv(missing,"Missing_all.csv")

}

# for all, missing a couple of years, except 

#SAUP 626: timor leste, APW/UP
# 534: Curacao APW/UP
# for these 2, using the EEZ.
# for the rest, using an average of the first 3 years of data.

# 2) associating the effort with the mapped cells.
{
  assoc$Code2<-paste(assoc$Year,assoc$Country,assoc$Length_Category,assoc$Sector,assoc$GearCode,sep="_")
length(unique(assoc$Code2))
realEffort$Code2<-paste(realEffort$Year,realEffort$Country,realEffort$Length_Category,realEffort$Sector,
                        realEffort$GearCode,sep="_")

length(unique(realEffort$Code2))
realEffort$Code<-assoc$Code[match(assoc$Code2,realEffort$Code2,nomatch=NA,incomparables = NULL)]


effort$NomActive<-realEffort$NomEffortActive[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$Creep<-realEffort$CreepLogCompounded[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$EffActive<-effort$NomActive*(effort$Creep/100+1)

effort$EffActive<-effort$EffActive*effort$RatioEffortNorm
effort$NomActive<-effort$NomActive*effort$RatioEffortNorm

effort$Length_Category<-realEffort$Length_Category[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$Gear<-realEffort$Gear[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$Sector<-realEffort$Sector[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$NV<-realEffort$NVActive[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$P<-realEffort$PActive[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$GT<-realEffort$GTActive[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]
effort$SAUP<-realEffort$SAUP[match(effort$Code,realEffort$Code,nomatch=NA,incomparables = NULL)]

effort$NV<-effort$NV*effort$RatioEffortNorm
effort$P<-effort$P*effort$RatioEffortNorm
effort$GT<-effort$GT*effort$RatioEffortNorm

effort$Lat<-Cells$Lat[match(effort$Cell,Cells$Seq,nomatch=NA,incomparables = NULL)]
effort$Lon<-Cells$Lon[match(effort$Cell,Cells$Seq,nomatch=NA,incomparables = NULL)]
effort$CodeKeep<-rownames(effort)
effort<-effort[,names(effort) %in% c("CodeKeep" , "NomActive", "EffActive","Length_Category","Gear", 
                                     "Sector", "NV", "P" ,"GT","SAUP", "Lat", "Lon"  )]
write.csv(effort,"D:/Effort_2017/temp_effort.csv")

}


effort<-read.csv("D:/Effort_2017/temp_effort.csv")

effort<-effort[,names(effort) %in% c("CodeKeep" , "Length_Category","SAUP", "Lat", "Lon","NomActive", 
                                     "EffActive" , "NV", "P" ,"GT","Sector","Gear","FGroup" )]
effort$Sector[!effort$Sector %in% c("UP","APW")]<-"I"

gc()
lcats<-c("24-50m","Over 50m","12-24m","6-12m","Less than 6m")
Sectors<-c("UP","APW","I")


for (s in Sectors){
  message(s)
  EffortS<-subset(effort,Sector==s)
  path<-file.path(paste0("D:/Effort_2017/temp_effort_",s,".csv"))
  write.csv(EffortS,path)

  effActS<-sum(EffortS$NomActive,na.rm=T)
  effNVS<-sum(EffortS$NV,na.rm=T)
  assign(paste0("effAct_",s),effActS)
  assign(paste0("effNV_",s),effNVS)
  
  
  
  
}

effAct<-sum(effort$NomActive,na.rm=T)
effNV<-sum(effort$NV,na.rm=T)

rm(effort)
rm(EffortS)
gc()
gc()

message(effAct)
message(effNV)


# 3) do a smoothing of the effort by cell, using the 8 cells around.
# change of plans. Going to separate by Sector and length cat.
# then do all the countries 10% at a time.



  
  
  Cells$LatLon<-paste0(Cells$Lat,"_",Cells$Lon)
  # for Industrial first
  # may need to cut that into different portions of the effort... again...
 
  # I don't need any of the values right now, I can just reassociate them afterwards.
  # F Group, Length cat, gear and sector are not necessary for calculations either, can be added after its done.


  for (s in Sectors){
    message(s)
    path<-file.path(paste0("D:/Effort_2017/temp_effort_",s,".csv"))
    EffortS<-read.csv(path)
    EffortS<-EffortS[,names(EffortS)!="X"]
    # effort<-effort[effort$Sector!=s,]
    gc()
    
    if(s=="I"){
      for (l in lcats){        
        DataLength<-subset(EffortS,Length_Category==l)
      
      path<-file.path(paste0("D:/Effort_2017/temp_effort_I",l,".csv"))
      write.csv(DataLength,path)
      
      message(l)
}
      rm(DataLength)
      gc()
      
      for (l in lcats){
        message(l)
        path<-file.path(paste0("D:/Effort_2017/temp_effort_I",l,".csv"))
        
        DataLength<-read.csv(path)
        DataLength<-DataLength[,names(DataLength)!="X"]
        if(length(DataLength$Sector)==0){next}
        # message(sum(DataLength$NomActive,na.rm=T))
        # message(sum(DataLength$NV,na.rm=T))
        
        DataLength<-DataLength[,names(DataLength) %in% c("CodeKeep", "Lat","Lon","SAUP" )]
        DataLength$CodeSmooth<-rownames(DataLength)
        gc()
        for (i in seq(100,900,100)){
          DataLength_use<-DataLength[DataLength$SAUP %in% c( (i-99) :i),]
          
          
          {
            if(length(DataLength_use$SAUP)==0){
              DataLength_f<-data.frame("NomActive"=NA,"EffActive"=NA,"NV"=NA,"P"=NA,"GT"=NA,
                                       "SAUP"=NA,"Lat"=NA,"Lon"=NA,"CodeKeep"=NA)
              assign(paste0("DataLength_",i),DataLength_f)
              
              next}
            DataLength_use<-DataLength_use[,!names(DataLength_use)=="SAUP"]
            gc()
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat-0.5
            DataLength_2$Lon<-DataLength_2$Lon+0.5
            DataLength_2$ratio<-0.05
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-DataLength_2
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat-0
            DataLength_2$Lon<-DataLength_2$Lon+0.5
            DataLength_2$ratio<-0.1
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat+0.5
            DataLength_2$Lon<-DataLength_2$Lon+0.5
            DataLength_2$ratio<-0.05
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat-0.5
            DataLength_2$Lon<-DataLength_2$Lon+0
            DataLength_2$ratio<-0.1
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat-0
            DataLength_2$Lon<-DataLength_2$Lon+0
            DataLength_2$ratio<-0.4
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat+0.5
            DataLength_2$Lon<-DataLength_2$Lon+0
            DataLength_2$ratio<-0.1
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat-0.5
            DataLength_2$Lon<-DataLength_2$Lon-0.5
            DataLength_2$ratio<-0.05
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat-0
            DataLength_2$Lon<-DataLength_2$Lon-0.5
            DataLength_2$ratio<-0.1
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
            
            DataLength_2<-DataLength_use
            DataLength_2$Lat<-DataLength_2$Lat+0.5
            DataLength_2$Lon<-DataLength_2$Lon-0.5
            DataLength_2$ratio<-0.05
            DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
            DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
            DataLength_f<-rbind(DataLength_f,DataLength_2)
          }
          rm(DataLength_use,DataLength_2)
          gc()
          
          DataLength_fAgg<-aggregate( DataLength_f[c("ratio")],by= DataLength_f[c("CodeSmooth")],FUN=sum,na.rm=T)
          # sum(DataLength_fAgg$ratio,na.rm=T)
          
          DataLength_f$ratio2<-DataLength_fAgg$ratio[match(DataLength_f$CodeSmooth,DataLength_fAgg$CodeSmooth,nomatch=NA,incomparables=NULL)]
          DataLength_f$ratio<-DataLength_f$ratio/DataLength_f$ratio2
          # sum(DataLength_f$ratio,na.rm=T)
          DataLength_f<-DataLength_f[,!names(DataLength_f)=="CodeSmooth"]
          rm(DataLength_fAgg)
          gc()
          
          DataLength_f$NomActive<-EffortS$NomActive[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
          DataLength_f$EffActive<-EffortS$EffActive[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
          DataLength_f$NV<-EffortS$NV[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
          DataLength_f$P<-EffortS$P[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
          DataLength_f$GT<-EffortS$GT[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
          DataLength_f$SAUP<-EffortS$SAUP[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
          
          DataLength_f$NomActive<-DataLength_f$NomActive*DataLength_f$ratio
          DataLength_f$EffActive<-DataLength_f$EffActive*DataLength_f$ratio
          DataLength_f$NV<-DataLength_f$NV*DataLength_f$ratio
          DataLength_f$P<-DataLength_f$P*DataLength_f$ratio
          DataLength_f$GT<-DataLength_f$GT*DataLength_f$ratio
          
          DataLength_fAgg<-aggregate( DataLength_f[c("NomActive","EffActive","NV","P","GT"   )],
                                      by= DataLength_f[c("SAUP","Lat","Lon","CodeKeep")],
                                      FUN=sum,na.rm=T)
          
          
          assign(paste0("DataLength_",i),DataLength_fAgg)
          
          
          rm(DataLength_f)
          rm(DataLength_fAgg)
          DataLength<-DataLength[!DataLength$SAUP %in% c( (i-99) :i),]
          gc()
          message(i)
        }
        
        
        DataLength<-rbind(DataLength_100,DataLength_200,DataLength_300,DataLength_400,
                          DataLength_500,DataLength_600,DataLength_700,DataLength_800,DataLength_900 )
        rm(DataLength_100,DataLength_200,DataLength_300,DataLength_400,
           DataLength_500,DataLength_600,DataLength_700,DataLength_800,DataLength_900)
        gc()
        DataLength$FGroup<-EffortS$FGroup[match(DataLength$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables=NULL)]
        DataLength$Gear<-EffortS$Gear[match(DataLength$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables=NULL)]
        DataLength$Length_Category<-l
        DataLength<-DataLength[,names(DataLength)!="CodeKeep"]
        
        # EffortS<-subset(EffortS,Length_Category!=l)
        path<-file.path(paste0("D:/Effort_2017/temp_effort_I",y,l,".csv"))
        
      write.csv(DataLength,path)
      rm(DataLength)
        gc()
        
      }
      
      
      for (l in lcats){
        message(l)
        path<-file.path(paste0("D:/Effort_2017/temp_effort_I",y,l,".csv"))
        
        DataLength<-read.csv(path)
        assign(paste0("DataLength_",l),DataLength)
        rm(DataLength)
        gc()
        }
      
      EffortMappedSector<-rbind(`DataLength_12-24m`,`DataLength_24-50m`,`DataLength_6-12m`,
                                `DataLength_Less than 6m`,`DataLength_Over 50m`)
      
      rm(`DataLength_12-24m`,`DataLength_24-50m`,`DataLength_6-12m`,
         `DataLength_Less than 6m`,`DataLength_Over 50m`)
      gc()
      
      EffortMappedSector$Sector<-s
      effActS<-sum(EffortS$NomActive,na.rm=T)
      effNVS<-sum(EffortS$NV,na.rm=T)
      assign(paste0("effAct_",s),effActS)
      assign(paste0("effNV_",s),effNVS)
      
      path<-file.path(paste0("D:/Effort_2017/Mapped_",s,"_",y,".csv"))
      write.csv(EffortMappedSector,path)
      
      rm(EffortMappedSector)
      gc()
      
      next
    }

    for (l in lcats){
      message(l)
      DataLength<-subset(EffortS,Length_Category==l)
      if(length(DataLength$Sector)==0){next}
      # message(sum(DataLength$NomActive,na.rm=T))
      # message(sum(DataLength$NV,na.rm=T))
      
      DataLength<-DataLength[,names(DataLength) %in% c("CodeKeep", "Lat","Lon","SAUP" )]
      DataLength$CodeSmooth<-rownames(DataLength)
      gc()
      for (i in seq(100,900,100)){
        DataLength_use<-DataLength[DataLength$SAUP %in% c( (i-99) :i),]
        
        
        {
          if(length(DataLength_use$SAUP)==0){
            DataLength_f<-data.frame("NomActive"=NA,"EffActive"=NA,"NV"=NA,"P"=NA,"GT"=NA,
                                     "SAUP"=NA,"Lat"=NA,"Lon"=NA,"CodeKeep"=NA)
            assign(paste0("DataLength_",i),DataLength_f)
            
            next}
          DataLength_use<-DataLength_use[,!names(DataLength_use)=="SAUP"]
          gc()
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat-0.5
          DataLength_2$Lon<-DataLength_2$Lon+0.5
          DataLength_2$ratio<-0.05
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-DataLength_2
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat-0
          DataLength_2$Lon<-DataLength_2$Lon+0.5
          DataLength_2$ratio<-0.1
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat+0.5
          DataLength_2$Lon<-DataLength_2$Lon+0.5
          DataLength_2$ratio<-0.05
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat-0.5
          DataLength_2$Lon<-DataLength_2$Lon+0
          DataLength_2$ratio<-0.1
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat-0
          DataLength_2$Lon<-DataLength_2$Lon+0
          DataLength_2$ratio<-0.4
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat+0.5
          DataLength_2$Lon<-DataLength_2$Lon+0
          DataLength_2$ratio<-0.1
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat-0.5
          DataLength_2$Lon<-DataLength_2$Lon-0.5
          DataLength_2$ratio<-0.05
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat-0
          DataLength_2$Lon<-DataLength_2$Lon-0.5
          DataLength_2$ratio<-0.1
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
          
          DataLength_2<-DataLength_use
          DataLength_2$Lat<-DataLength_2$Lat+0.5
          DataLength_2$Lon<-DataLength_2$Lon-0.5
          DataLength_2$ratio<-0.05
          DataLength_2$LatLon<-paste0(DataLength_2$Lat,"_",DataLength_2$Lon)
          DataLength_2<-DataLength_2[DataLength_2$LatLon %in% unique(Cells$LatLon),]
          DataLength_f<-rbind(DataLength_f,DataLength_2)
        }
        rm(DataLength_use,DataLength_2)
        gc()
        
        DataLength_fAgg<-aggregate( DataLength_f[c("ratio")],by= DataLength_f[c("CodeSmooth")],FUN=sum,na.rm=T)
        # sum(DataLength_fAgg$ratio,na.rm=T)
        
        DataLength_f$ratio2<-DataLength_fAgg$ratio[match(DataLength_f$CodeSmooth,DataLength_fAgg$CodeSmooth,nomatch=NA,incomparables=NULL)]
        DataLength_f$ratio<-DataLength_f$ratio/DataLength_f$ratio2
        # sum(DataLength_f$ratio,na.rm=T)
        DataLength_f<-DataLength_f[,!names(DataLength_f)=="CodeSmooth"]
        rm(DataLength_fAgg)
        gc()
        
        DataLength_f$NomActive<-EffortS$NomActive[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
        DataLength_f$EffActive<-EffortS$EffActive[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
        DataLength_f$NV<-EffortS$NV[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
        DataLength_f$P<-EffortS$P[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
        DataLength_f$GT<-EffortS$GT[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
        DataLength_f$SAUP<-EffortS$SAUP[match(DataLength_f$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables = NULL)]
        
        DataLength_f$NomActive<-DataLength_f$NomActive*DataLength_f$ratio
        DataLength_f$EffActive<-DataLength_f$EffActive*DataLength_f$ratio
        DataLength_f$NV<-DataLength_f$NV*DataLength_f$ratio
        DataLength_f$P<-DataLength_f$P*DataLength_f$ratio
        DataLength_f$GT<-DataLength_f$GT*DataLength_f$ratio
        
        DataLength_fAgg<-aggregate( DataLength_f[c("NomActive","EffActive","NV","P","GT"   )],
                                    by= DataLength_f[c("SAUP","Lat","Lon","CodeKeep")],
                                    FUN=sum,na.rm=T)
        
        
        assign(paste0("DataLength_",i),DataLength_fAgg)
        
        
        rm(DataLength_f)
        rm(DataLength_fAgg)
        DataLength<-DataLength[!DataLength$SAUP %in% c( (i-99) :i),]
        gc()
        message(i)
      }
      
      
      DataLength<-rbind(DataLength_100,DataLength_200,DataLength_300,DataLength_400,
                        DataLength_500,DataLength_600,DataLength_700,DataLength_800,DataLength_900 )
      rm(DataLength_100,DataLength_200,DataLength_300,DataLength_400,
         DataLength_500,DataLength_600,DataLength_700,DataLength_800,DataLength_900)
      gc()
      DataLength$FGroup<-EffortS$FGroup[match(DataLength$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables=NULL)]
      DataLength$Gear<-EffortS$Gear[match(DataLength$CodeKeep,EffortS$CodeKeep,nomatch=NA,incomparables=NULL)]
      DataLength$Length_Category<-l
      DataLength<-DataLength[,names(DataLength)!="CodeKeep"]
      
      # EffortS<-subset(EffortS,Length_Category!=l)
      
      assign(paste0("DataLength_",l),DataLength)
      rm(DataLength)
      gc()
      
    }

  

  EffortMappedSector<-rbind(`DataLength_12-24m`,`DataLength_24-50m`,`DataLength_6-12m`,
                      `DataLength_Less than 6m`,`DataLength_Over 50m`)
  
rm(`DataLength_12-24m`,`DataLength_24-50m`,`DataLength_6-12m`,
   `DataLength_Less than 6m`,`DataLength_Over 50m`)
gc()

EffortMappedSector$Sector<-s
effActS<-sum(EffortS$NomActive,na.rm=T)
effNVS<-sum(EffortS$NV,na.rm=T)
assign(paste0("effAct_",s),effActS)
assign(paste0("effNV_",s),effNVS)

path<-file.path(paste0("D:/Effort_2017/Mapped_",s,"_",y,".csv"))
write.csv(EffortMappedSector,path)

rm(EffortMappedSector)
gc()
}


gc()



}