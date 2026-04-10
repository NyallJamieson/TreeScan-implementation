# For epiEngage: Create Signal Linelists - Ramona Lall (3/9/2026)

# Load required libraries
library("sf")
library("ggplot2")
library("dplyr")
library("scales")
library("dplyr")
library("readr")
library("lubridate")

# For time trend we need to download some background data
source(paste0(parent_dir, "/code/6.1_download_background_for_interpretation.R"))

# For time trend table pull in current year and additional 3 prior years
yr_list <- (as.numeric(format(Sys.Date(),"%Y"))-3) : as.numeric(format(Sys.Date(),"%Y"))

# Bit of tidying up before importing data
files <- list.files(paste0(parent_dir, "/raw_data"),
                    pattern = "^NSSP_data_\\d{4}-\\d{2}-\\d{2}_to_\\d{4}-\\d{2}-\\d{2}\\.csv$",
                    full.names = TRUE
)

# extract the first date from each filename
SD <- as.Date(sub("NSSP_data_(\\d{4}-\\d{2}-\\d{2})_to_.*", "\\1", basename(files)))
file_year  <- format(SD, "%Y")

files_by_year <- split(files, file_year)

year_dfs <- lapply(files_by_year, function(x) {
  bind_rows(lapply(x, function(f) {
    read_csv(
      f,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE
    )
  }))
})

# However you import data; I use import_data_specdates function
archive_deduped <- rbind(year_dfs[[1]], year_dfs[[2]], year_dfs[[3]], year_dfs[[4]])
ed1 <- year_dfs[[1]]
ed1$date <- as.Date(ed1$C_Visit_Date_Time)
ed1 <- ed1[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]
ed2 <- year_dfs[[2]]
ed2$date <- as.Date(ed2$C_Visit_Date_Time)
ed2 <- ed2[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]
ed3 <- year_dfs[[3]]
ed3$date <- as.Date(ed3$C_Visit_Date_Time)
ed3 <- ed3[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]
ed4 <- year_dfs[[4]]
ed4$date <- as.Date(ed4$C_Visit_Date_Time)
ed4 <- ed4[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]

ed <- rbind(ed1,ed2,ed3,ed4)



# We have a step where we need to impute admit for a set of hospitals
# ed <- ed %>%
#   left_join(hosp_list, by = "hospital") %>%
#   mutate(dischargedisposition = if_else(
#     !is.na(start_imputation) & !is.na(end_imputation) &
#       date >= start_imputation & date <= end_imputation &
#       is.na(dischargedisposition),
#     "09A", dischargedisposition
#   )) %>%
#   select(-start_imputation, -end_imputation)

ed$dispo <- "V" #visit
ed$dispo[ed$DischargeDisposition %in% c("09","9","09A","Adm","ADM","20","_Expired","Expired","22","23","24","25","26","27","28","29","40", "41", "42")] <- "A" #adverse

ed$diagnosiscode1 <- gsub("\\.", "", ed$DischargeDiagnosis)

ed <- cbind(MMWRweek::MMWRweek(ed$date), ed)

mmwrwks <- unique(MMWRweek::MMWRweek(unique(ed$date))[, - 3])

# For maps
library(sf)
library(dplyr)
library(tigris)

options(tigris_use_cache = TRUE)

# National ZCTA shapefile from Census via tigris
# cb = TRUE gives the lighter generalized file; set cb = FALSE if you want full detail
modzcta <- tigris::zctas(year = 2020, cb = TRUE, class = "sf")

# Keep the downstream join working by creating the same join key name
modzcta <- modzcta %>%
  mutate(modzcta = ZCTA5CE20)

# Create the same lookup structure your old code expected:
# zipcode + MODZCTA20
df <- modzcta %>%
  st_drop_geometry() %>%
  transmute(
    zipcode = ZCTA5CE20,
    MODZCTA20 = ZCTA5CE20
  )

# call in tree that has each code and has columns Level1--Level8
icd10.2026_treefilew_FINAL <- read.delim(paste0(parent_dir, "/data/Tree_File_2026_wide_format.txt"))

# for list of max signals that have outlier trend (need for Signal Report)
sigs_maxout <- character()

# An excel workbook which has a linelist for each signal and additional analyses
# The tab sheets include: 
# 1. line-level for Cluster - identifying incident vs non-incident [Include all variables of interest but at minimum for additional tabs see below fields needed]
# 2. line-level for Baseline (if it exists) - identifying incident vs non-incident
# 3. Frequency table of other co-diagnoses for cluster and baseline [diagnosiscode]
# 4. Chief complaint word frequency for cluster and baseline [chief complaint]
# 5. Multi-year weekly trend 
# 6. Maps for cluster and baseline [zipcode/MODZCTA]
# 7. Hospital graphic [hospital]
# 8. Demographic table comparing cluster to baseline [sex, age, race, ethnicity, boro]

# you need the original dataset "archive_deduped" (includes non-incident diagnoses) with all variables you want to see in line-level: "date","time","key","hospital","zipcode","patientid","sex","age","age_group","race","ethnicity","chiefcomplaint","admitreason","diagnosiscode","diagnosistext","diagnosiscode1","modeofarrival","travelhistory","dischargedisposition","dispo","dischargedate","triagenote"
# Of these, you must have: "date","key","hospital","zipcode","sex","age","age_group","race","ethnicity","chiefcomplaint","diagnosiscode1","dispo"
archive_deduped$date <- as.Date(archive_deduped$C_Visit_Date_Time)
archive_deduped$time <- format(
  as.POSIXct(archive_deduped$C_Visit_Date_Time, tz = "UTC"),
  "%H:%M:%S"
)
archive_deduped$key <- archive_deduped$C_Unique_Patient_ID
archive_deduped$hospital <- archive_deduped$Hospital
archive_deduped$zipcode <- archive_deduped$HospitalZip
archive_deduped$patientid <- archive_deduped$C_Unique_Patient_ID
archive_deduped$sex <- archive_deduped$Sex
archive_deduped$race <- archive_deduped$C_Race
archive_deduped$ethnicity <- archive_deduped$C_Ethnicity
archive_deduped$chiefcomplaint <- archive_deduped$ChiefComplaintParsed
archive_deduped$admitreason <- archive_deduped$Admit_Reason_Code
archive_deduped$diagnosiscode <- archive_deduped$DischargeDiagnosis
archive_deduped$diagnosiscode1 <- archive_deduped$diagnosiscode
archive_deduped$diagnosistext <- archive_deduped$Diagnosis_Combo
archive_deduped$dischargedisposition <- archive_deduped$DischargeDisposition
archive_deduped$dispo <- ed$dispo
archive_deduped$modeofarrival <- archive_deduped$ModeOfArrival
archive_deduped$travelhistory <- archive_deduped$Travel_History
archive_deduped$triagenote <- archive_deduped$TriageNotesOrig
archive_deduped$age <- archive_deduped$Age                                                                                                                            
archive_deduped$dischargedate <- archive_deduped$Discharge_Date_Time

# derive age_group from age using the bins expected later in the script
archive_deduped$age_group <- cut(
  as.numeric(archive_deduped$age),
  breaks = c(-Inf, 0, 4, 12, 17, 49, 64, 79, Inf),
  labels = c("<1", "1-4", "5-12", "13-17", "18-49", "50-64", "65-79", "80+"),
  right = TRUE
)

archive_deduped$age_group <- as.character(archive_deduped$age_group)






# Load in the count file
v2 <- read.csv(paste0(parent_dir, "/data/v2/", Sys.Date(), "/lag0.csv"))

# Read in Results csv file (edit to match naming convention)
TS_Results_today <- read.csv(paste0(parent_dir, "/results/", Sys.Date(), "/Results_lag1_", Sys.Date(), ".csv"))

# Signal criteria
TS_Results_today <- TS_Results_today[is.na(TS_Results_today$Recurrence.Interval) == F, ]
TS_Results_today <- TS_Results_today[which(TS_Results_today$Relative.Risk>=1.3),]

# Admit signals have a lower threshold
TS_Results_today <- TS_Results_today[which((TS_Results_today$Recurrence.Interval >= 365)|(grepl("1\\-",TS_Results_today$Node.Identifier) & TS_Results_today$Recurrence.Interval>=100)),]

library("MMWRweek")
library("stringi")
library("lubridate")
library("openxlsx")

TS_Results_today$Node.Identifier=stri_replace_all_fixed(TS_Results_today$Node.Identifier, "\xa0", "")

# Common cause (these are for the dummy nodes that link different parts of the tree)
common_cause <- read.csv(paste0(parent_dir, "/data/common cause file final.csv"))
common_cause <- common_cause[is.na(common_cause$X4)==F,]

# check if any of the signals are for dummy node and if any, add the different linked nodes to the identifier value separated by "|"
common_cause_codes <- TS_Results_today$Node.Identifier[grepl(paste(common_cause$X2,collapse="|"),gsub("2\\-|1\\-|0\\-","",TS_Results_today$Node.Identifier))]
if(length(common_cause_codes)>0)
{
  for(i in 1:length(common_cause_codes))
  {
    TS_Results_today$Node.Name[grepl(paste(gsub("2\\-","2\\\\-",gsub("1\\-","1\\\\-",common_cause_codes[i]))),TS_Results_today$Node.Identifier)] =TS_Results_today$Node.Identifier[grepl(paste(gsub("2\\-","2\\\\-",gsub("1\\-","1\\\\-",common_cause_codes[i]))),TS_Results_today$Node.Identifier)]
    list_codes <- common_cause$X1[grepl(paste(gsub("1\\-|2\\-","",common_cause_codes[i])),common_cause$X2)]
    TS_Results_today$Node.Identifier[grepl(paste(common_cause_codes[i]),TS_Results_today$Node.Identifier)]=paste0(c(TS_Results_today$Node.Name[grepl(paste(common_cause_codes[i]),TS_Results_today$Node.Identifier)],list_codes),collapse="|")
  }
}

END_DATE <- Sys.Date() - LAG

# For cluster and baseline linelist if you want to determine which are incident vs non-incident, you will also need to use v2 (this is the study dataset where we only kept incident diagnoses)
# This has "date", "key", "dispo", "code" (in that order)
for(i in 1:nrow(TS_Results_today))
{
  # Cluster and Baseline Linelists using original unrestricted archive_deduped
  # temp=archive_deduped[which(grepl(paste0(gsub("\\.","",gsub("0\\-|1\\-|2\\-","",TS_Results_today$Node.Identifier[i]))),archive_deduped$diagnosiscode1)==T & archive_deduped$date>=as.Date(TS_Results_today$Time.Window.Start[i])),c("date","time","key","hospital","zipcode","patientid","sex","age","age_group","race","ethnicity","chiefcomplaint","admitreason","diagnosiscode","diagnosistext","diagnosiscode1","modeofarrival","travelhistory","dischargedisposition","dispo","dischargedate","triagenote")]
  # temp1=archive_deduped[which(grepl(paste0(gsub("\\.","",gsub("0\\-|1\\-|2\\-","",TS_Results_today$Node.Identifier[i]))),archive_deduped$diagnosiscode1)==T & archive_deduped$date<as.Date(TS_Results_today$Time.Window.Start[i])),c("date","time","key","hospital","zipcode","patientid","sex","age","age_group","race","ethnicity","chiefcomplaint","admitreason","diagnosiscode","diagnosistext","diagnosiscode1","modeofarrival","travelhistory","dischargedisposition","dispo","dischargedate","triagenote")]
  node <- TS_Results_today$Node.Identifier[i] |>
    gsub(pattern = "^(0-|1-|2-)", replacement = "", x = _) |>
    gsub(pattern = "\\.", replacement = "", x = _)
  
  dx_clean <- gsub("\\.", "", archive_deduped$diagnosiscode1)
  
  keep_cols <- c(
    "date","time","key","hospital","zipcode","patientid","sex","age","age_group",
    "race","ethnicity","chiefcomplaint","admitreason","diagnosiscode","diagnosistext",
    "diagnosiscode1","modeofarrival","travelhistory","dischargedisposition","dispo",
    "dischargedate","triagenote"
  )
  
  match_dx <- !is.na(dx_clean) & grepl(node, dx_clean, fixed = TRUE)
  match_date <- !is.na(archive_deduped$date)
  
  temp  <- archive_deduped[match_dx & match_date & archive_deduped$date >= as.Date(TS_Results_today$Time.Window.Start[i]), keep_cols]
  temp1 <- archive_deduped[match_dx & match_date & archive_deduped$date <  as.Date(TS_Results_today$Time.Window.Start[i]), keep_cols]
  
  # Cluster and Baseline List from study dataset restricting to incident
  # temp2 <- v2[which(grepl(paste0(gsub("\\.","",gsub("0\\-|1\\-|2\\-","",TS_Results_today$Node.Identifier[i]))),v2$code)==T & as.Date(v2$date)>=as.Date(TS_Results_today$Time.Window.Start[i])),1:3]
  # temp21 <- v2[which(grepl(paste0(gsub("\\.","",gsub("0\\-|1\\-|2\\-","",TS_Results_today$Node.Identifier[i]))),v2$code)==T & as.Date(v2$date)<as.Date(TS_Results_today$Time.Window.Start[i])),1:3]
  node <- TS_Results_today$Node.Identifier[i]
  node <- gsub("^(0-|1-|2-)", "", node)
  node <- gsub("\\.", "", node)
  
  code_clean <- gsub("\\.", "", v2$code)
  
  temp2 <- v2[
    which(
      !is.na(code_clean) &
        grepl(node, code_clean, fixed = TRUE) &
        !is.na(v2$date) &
        as.Date(v2$date) >= as.Date(TS_Results_today$Time.Window.Start[i])
    ),
  ]
  
  temp21 <- v2[
    which(
      !is.na(code_clean) &
        grepl(node, code_clean, fixed = TRUE) &
        !is.na(v2$date) &
        as.Date(v2$date) < as.Date(TS_Results_today$Time.Window.Start[i])
    ),
  ]
  
  # For multi-year trend lines for current year and prior 3 years
  # ed_keep=ed[which(grepl(paste0(gsub("\\.","",gsub("0\\-|1\\-|2\\-","",TS_Results_today$Node.Identifier[i]))),ed$diagnosiscode1)==T),]
  # ed_keep$n=1
  node <- TS_Results_today$Node.Identifier[i]
  node <- gsub("^(0-|1-|2-)", "", node)
  node <- gsub("\\.", "", node)
  
  dx_clean <- gsub("\\.", "", ed$diagnosiscode1)
  
  ed_keep <- ed[
    !is.na(dx_clean) &
      grepl(node, dx_clean, fixed = TRUE),
  ]
  
  ed_keep$n <- 1
  
  # If baseline has records (i.e., not empty)
  if(nrow(temp1)>0)
  {
    # merge linelist for cluster between original and study dataset to identify incident vs non-incident
    if(nrow(temp2)>0)
    {
      temp2$dxtype=0
      temp2$dxtype[which(temp2$dispo=="A")]=1
      # temp2=temp2[,-3]
      temp$date=as.Date(temp$date)
      temp2$date=as.Date(temp2$date)
      temp=merge(temp,temp2,by=c("date","key"),all.x=TRUE)
      temp=temp[,c(ncol(temp),1:(ncol(temp)-1))]
      temp$dxtype[is.na(temp$dxtype)==T]=-1
    }
    
    # merge linelist for baseline between original and study dataset to identify incident vs non-incident
    if(nrow(temp21)>0)
    {
      temp21$dxtype=0
      temp21$dxtype[which(temp21$dispo=="A")]=1
      # temp21=temp21[,-3]
      temp1$date=as.Date(temp1$date)
      temp21$date=as.Date(temp21$date)
      temp1=merge(temp1,temp21,by=c("date","key"),all.x=TRUE)
      temp1=temp1[,c(ncol(temp1),1:(ncol(temp1)-1))]
      temp1$dxtype[is.na(temp1$dxtype)==T]=-1
    }
    
    # if 1- then only keep Adverse visits
    if(grepl("1\\-",TS_Results_today$Node.Identifier[i])==T)
    {temp=temp[which(temp$dispo=="A"),]
    temp1=temp1[which(temp1$dispo=="A"),]
    ed_keep=ed_keep[which(ed_keep$dispo=="A"),]}
    
    # order by date and time
    temp$time=format(strptime(temp$time, "%H:%M"), "%H:%M")
    temp=temp[order(temp$date,temp$time),]
    
    # order by date and time
    temp1$time=format(strptime(temp1$time, "%H:%M"), "%H:%M")
    temp1=temp1[order(temp1$date,temp1$time),]
    
    #For multi-year trend lines
    ed_keep=cbind(MMWRweek(ed_keep$date)[,-3],ed_keep)
    ed_keep=aggregate(n~MMWRyear+MMWRweek,data=ed_keep,sum)
    ed_keep=merge(mmwrwks,ed_keep,all.x=TRUE)
    ed_keep$n[is.na(ed_keep$n)==T]=0
    ed_keep=ed_keep[ed_keep$MMWRyear %in% yr_list,]
    ed_keep=ed_keep[order(ed_keep$MMWRyear,ed_keep$MMWRweek),]
    rownames(ed_keep) <- seq_len(nrow(ed_keep))
    
    # Multi-year Weekly count graph
    
    # check is it an outlier trend for max signals
    complete_week=ifelse(weekdays(END_DATE)=="Saturday",ed_keep$MMWRweek[nrow(ed_keep)],ed_keep$MMWRweek[nrow(ed_keep)-1])
    
    hlm_weeks=which(ed_keep$MMWRweek[-c((nrow(ed_keep)-1):nrow(ed_keep))]==complete_week)
    hlm_weeks=c(hlm_weeks-1,hlm_weeks,hlm_weeks+1)
    hlm_weeks=hlm_weeks[hlm_weeks>=1]
    
    hlm_data=ed_keep[hlm_weeks,]
    hlm_data=hlm_data[order(hlm_data$MMWRyear,hlm_data$MMWRweek),]
    
    if(ed_keep$n[ed_keep$MMWRyear==max(ed_keep$MMWRyear) & ed_keep$MMWRweek==complete_week]>mean(hlm_data$n)+2*sd(hlm_data$n))
    {sigs_maxout <- c(sigs_maxout, as.character(TS_Results_today$Node.Identifier[i]))}
    
    code_trend <- tempfile(fileext = ".png")
    png(code_trend, width = 1000, height = 600, res = 150)
    par(mar = c(6, 4, 1, 2))  # Adjust bottom margin for label space
    
    plot(1:52,rep(0,52),type="n",xlim=c(1,52),ylim=c(0,ceiling(max(ed_keep$n)*1.1)),xlab="CDC_Week",ylab=gsub("2\\-","",paste(TS_Results_today$Node.Identifier[i])),xaxt="n")
    axis(1,at=seq(1,52,2),labels=seq(1,52,2),cex.axis=0.6,las=2)
    
    # Trends where last point we note if partial or full week
    if(weekdays(END_DATE)=="Saturday")
    {for(k in 1:length(unique(ed_keep$MMWRyear)))
    {lines(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
      legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep$MMWRyear),bty="n",ncol=4)}
    
    if(weekdays(END_DATE)%in% c("Wednesday", "Thursday", "Friday"))
    {for(k in 1:length(unique(ed_keep$MMWRyear)))
    {lines(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
      if(k==length(unique(ed_keep$MMWRyear))) 
      { lastpt=length(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])])
      points(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])][lastpt],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])][lastpt], pch = 21, bg = NA, col = "red", cex = 1, lwd = 1)
      legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep$MMWRyear),bty="n",ncol=4)}
      lg=legend("topright",pch=21,col="Red",legend="partial week",cex=0.5,bty="n",plot=FALSE)
      legend(x = lg$rect$left, y = lg$rect$top*0.85,  # adjust 0.05 as needed
             pch = 21, col = "red", legend = "partial week", 
             cex = 0.5, bty = "n")}
    
    if(weekdays(END_DATE) %in% c("Sunday","Monday","Tuesday"))
    {ed_keep1=ed_keep[-length(ed_keep$MMWRyear),]
    for(k in 1:length(unique(ed_keep1$MMWRyear)))
    {lines(ed_keep1$MMWRweek[which(ed_keep1$MMWRyear==unique(ed_keep1$MMWRyear)[k])],ed_keep1$n[which(ed_keep1$MMWRyear==unique(ed_keep1$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
    legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep1$MMWRyear),bty="n",ncol=4)}
    dev.off()
    
    # Maps for cluster and baseline
    temp$zipcode=substr(temp$zipcode,1,5)
    temp_z=merge(temp,df[,c("zipcode","MODZCTA20")],by="zipcode",all.x=TRUE)
    temp_z=data.frame(table(temp_z$MODZCTA20))
    temp_z$pct=round(temp_z$Freq/sum(temp_z$Freq),4)
    names(temp_z)[1]="modzcta"
    
    temp1$zipcode=substr(temp1$zipcode,1,5)
    temp1_z=merge(temp1,df[,c("zipcode","MODZCTA20")],by="zipcode",all.x=TRUE)
    temp1_z=data.frame(table(temp1_z$MODZCTA20))
    temp1_z$pct=round(temp1_z$Freq/sum(temp1_z$Freq),4)
    names(temp1_z)[1]="modzcta"
    
    modzcta1 <- left_join(modzcta, temp_z, by = "modzcta")
    
    p <- ggplot(modzcta1) +
      geom_sf(aes(fill = pct), color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "plasma", name = "Cluster",labels = label_number(accuracy = 0.001) ) +
      theme_minimal() +
      labs(title = paste("MODZCTA Choropleth Map for ",TS_Results_today$Node.Identifier[i]," cluster"))
    
    modzcta1 <- left_join(modzcta, temp1_z, by = "modzcta")
    
    p1 <- ggplot(modzcta1) +
      geom_sf(aes(fill = pct), color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "plasma", name = "Baseline",labels = label_number(accuracy = 0.001) ) +
      theme_minimal() +
      labs(title = paste("NYC MODZCTA Choropleth Map for ",TS_Results_today$Node.Identifier[i]," in baseline"))
    
    # Other co-diagnoses codes - creating frequency tables for cluster and baseline
    process <- function(x) {
      tokens <- unlist(strsplit(x, " "))
      tokens <- tokens[tokens != ""]  # remove empty strings
      unique_subs <- unique(substr(tokens, 1, 3))
      paste(unique_subs, collapse = " ")
    }
    
    process <- function(x) {
      tokens <- unlist(strsplit(gsub("\\|", "", x), ";"))
      tokens <- tokens[tokens != ""]
      unique(substr(tokens, 1, 3))
    }
    
    temp3 <- temp1[temp1$dxtype != -1, "diagnosiscode1", drop = TRUE]
    if (length(temp3) == 0) temp3 <- temp1[, "diagnosiscode1", drop = TRUE]
    
    result <- data.frame(
      dx = vapply(temp3, function(x) paste(process(x), collapse = " "), character(1)),
      stringsAsFactors = FALSE
    )
    
    temp3b <- data.frame(
      table(unlist(strsplit(result$dx, " "))),
      stringsAsFactors = FALSE
    )
    
    names(temp3b) <- c("dx", "Freq")
    temp3b <- temp3b[temp3b$dx != "", ]
    
    temp3b$dx=gsub("\\s+","",temp3b$dx)
    temp3b=merge(temp3b,icd10.2026_treefilew_FINAL[,c("Name1","Desc")],by.x="dx",by.y="Name1",all.x=T)
    temp3b=temp3b[order(-temp3b$Freq),]
    names(temp3b)[1:2]=c("Level4", "BaselineFreq")
    temp3b$BaselinePct=round((temp3b$BaselineFreq*100)/length(temp3),1)
    
    # temp3=gsub("\\|","",temp[which(temp$dxtype!=-1),"diagnosiscode1"])
    # result <- data.frame(dx = sapply(temp3, process))
    temp3 <- temp[temp$dxtype != -1, "diagnosiscode1", drop = TRUE]
    if (length(temp3) == 0) temp3 <- temp[, "diagnosiscode1", drop = TRUE]
    
    result <- data.frame(
      dx = vapply(temp3, function(x) paste(process(x), collapse = " "), character(1)),
      stringsAsFactors = FALSE
    )
    
    temp3c=data.frame(table(dx=unlist(strsplit(result[,1], " "))))
    temp3c$dx=gsub("\\s+","",temp3c$dx)
    temp3c=merge(temp3c,icd10.2026_treefilew_FINAL[,c("Name1","Desc")],by.x="dx",by.y="Name1",all.x=T)
    temp3c=temp3c[order(-temp3c$Freq),]
    names(temp3c)[1:2]=c("Level4","ClusterFreq")
    temp3c$ClusterPct=round((temp3c$ClusterFreq*100)/length(temp3),1)
    
    # Top cc words in Cluster and Baseline
    temp4=as.data.frame(table(toupper(unlist(lapply(strsplit(gsub(x=temp$chiefcomplaint[which(temp$dxtype!=-1)],pattern="[[:punct:]]",replacement=" "),split=" "),unique)))))
    temp4=temp4[!temp4$Var1 %in% c("","I10","THE","A","AN","OF","AND","TO","IN","FOR","WITH","ON","IS","WAS","ARE","BY","AT","FROM","MY","HIS","HER","HE","SHE","AS","PER","I","HAS","HAVE","PT","PATIENT","STATES"),]
    temp4=temp4[nchar(as.character(temp4$Var1))>=2,]
    #temp4=temp4[order(-temp4$Freq),]
    names(temp4)=c("CC_word","ClusterFreq")
    temp4$ClusterPct=round((temp4$ClusterFreq*100)/length(temp$chiefcomplaint[which(temp$dxtype!=-1)]),1)
    
    vals <- temp1$chiefcomplaint[which(temp1$dxtype != -1)]
    vals <- vals[!is.na(vals) & vals != ""]
    
    if (length(vals) == 0) {
      temp4b <- data.frame(
        CC_word = character(),
        BaselineFreq = integer(),
        BaselinePct = numeric(),
        stringsAsFactors = FALSE
      )
    } else {
      words <- toupper(unlist(lapply(
        strsplit(gsub("[[:punct:]]", " ", vals), " "),
        unique
      )))
      
      words <- words[!is.na(words)]
      words <- trimws(words)
      words <- words[words != ""]
      words <- words[!words %in% c("","I10","THE","A","AN","OF","AND","TO","IN","FOR","WITH","ON","IS","WAS","ARE","BY","AT","FROM","MY","HIS","HER","HE","SHE","AS","PER","I","HAS","HAVE","PT","PATIENT","STATES")]
      words <- words[nchar(words) >= 2]
      
      if (length(words) == 0) {
        temp4b <- data.frame(
          CC_word = character(),
          BaselineFreq = integer(),
          BaselinePct = numeric(),
          stringsAsFactors = FALSE
        )
      } else {
        temp4b <- as.data.frame(table(words), stringsAsFactors = FALSE)
        names(temp4b) <- c("CC_word", "BaselineFreq")
        temp4b$BaselinePct <- round((temp4b$BaselineFreq * 100) / length(vals), 1)
      }
    }
    
    temp_words=merge(temp4,temp4b,by="CC_word",all=TRUE)
    temp_words=temp_words[order(-temp_words$ClusterFreq),]
    
    # Demographic tables for comparing Cluster and Baseline
    temp_d=temp[which(temp$dxtype!=-1),c("sex","age_group", "race","ethnicity","zipcode")]
    temp_d$group <- "Cluster"
    temp1_d=temp1[which(temp1$dxtype!=-1),c("sex","age_group", "race","ethnicity","zipcode")]
    temp1_d$group <- "Baseline"
    combined=rbind(temp_d,temp1_d)
    age_levels <- c("<1","1-4", "5-12", "13-17", "18-49", "50-64", "65-79", "80+")
    combined$age_group<- factor(combined$age_group, levels = age_levels)
    combined$sex=toupper(combined$sex)
    # combined$boro="Outside"
    # combined$boro[grepl("\\b100|10128",combined$zipcode)]="Manhattan"
    # combined$boro[grepl("\\b112",combined$zipcode)]="Brooklyn"
    # combined$boro[grepl("\\b104",combined$zipcode)]="Bronx"
    # combined$boro[grepl("\\b113|\\b114|\\b116|\\b111|\\b110",combined$zipcode)]="Queens"
    # combined$boro[grepl("\\b103",combined$zipcode)]="StatenIs"
    # ref_boro=ifelse(length(combined$boro[grepl("Manhattan",combined$boro)==T])>=1,"Manhattan",intersect(c("Brooklyn","Bronx","Queens","StatenIs"),unique(combined$boro))[1])
    # combined$boro=relevel(factor(combined$boro),ref=ref_boro)
    # combined1=combined[which(combined$race!="MISSUNK"),]
    # combined1=combined1[which(combined1$ethnicity!="MISSUNK"),]
    combined$zipcode <- as.character(combined$zipcode)
    combined$zipcode <- trimws(combined$zipcode)
    combined$zipcode <- substr(combined$zipcode, 1, 5)
    
    combined$zip3 <- substr(combined$zipcode, 1, 3)
    combined$zip3[is.na(combined$zip3) | combined$zip3 == ""] <- "Unknown"
    
    combined$zip3 <- factor(combined$zip3)
    ref_zip3 <- levels(combined$zip3)[1]
    combined$zip3 <- relevel(combined$zip3, ref = ref_zip3)
    
    # List of demographic variables
    demographics <- c("sex", "age_group", "ethnicity", "race", "zip3")
    
    # Set sparse ZIPs to 'other'
    zip_counts <- table(combined$zip3)
    keep_zip3 <- names(zip_counts[zip_counts >= 10])
    
    combined$zip3_collapsed <- as.character(combined$zip3)
    combined$zip3_collapsed[!combined$zip3_collapsed %in% keep_zip3] <- "Other"
    combined$zip3_collapsed <- factor(combined$zip3_collapsed)
    
    # Initialize results list
    results <- list()
    
    # for (var in demographics) {
    #   # Create contingency table
    #   tab <- table(combined[[var]], combined$group)
    #   
    #   # Calculate proportions by group
    #   prop <- round(prop.table(table(combined[[var]], combined$group), margin = 2) * 100,2)  # Column-wise percentages
    #   
    #   # Convert to data frame
    #   combined_prop <- as.data.frame.matrix(prop)
    #   combined_prop$Variable <- rownames(combined_prop)
    #   rownames(combined_prop) <- NULL
    #   
    #   # Chi-square test or Fisher's Test if Chisq fails
    #   safe_test_p <- function(tbl) {
    #     
    #     result <- tryCatch({
    #       res <- suppressWarnings(chisq.test(tbl))
    #       if (any(is.na(res$expected)) || any(res$expected < 5)) stop("Invalid expected values")
    #       list(p.value = res$p.value, method = res$method)
    #     }, warning = function(w) {
    #       message("Chi-sq warning: ", conditionMessage(w))
    #       if (all(dim(tbl) == c(2, 2))) {
    #         res <- fisher.test(tbl)
    #         list(p.value = res$p.value, method = res$method)
    #       } else {
    #         res <- chisq.test(tbl, simulate.p.value = TRUE, B = 10000)
    #         list(p.value = res$p.value, method = "Simulated Chi-squared Test")
    #       }
    #     }, error = function(e) {
    #       message("Chi-sq error: ", conditionMessage(e))
    #       if (all(dim(tbl) == c(2, 2))) {
    #         res <- fisher.test(tbl)
    #         list(p.value = res$p.value, method = res$method)
    #       } else {
    #         res <- chisq.test(tbl, simulate.p.value = TRUE, B = 10000)
    #         list(p.value = res$p.value, method = "Simulated Chi-squared Test")
    #       }
    #     })
    #     
    #     return(result)
    #   }
    #   
    #   p_val=safe_test_p(tab)$p.value
    #   method=safe_test_p(tab)$method
    #   # Add p-value to every row (or just first row if you prefer)
    #   combined_prop$p_value <- ""
    #   combined_prop$p_value[1] <- round(p_val,3)
    #   
    #   combined_prop$method <- ""
    #   combined_prop$method[1] <-method
    #   # Add the variable name
    #   combined_prop$Demographic <- var
    #   
    #   # Reorder columns
    #   combined_prop <- combined_prop[, c("Demographic","Variable", "Baseline", "Cluster", "p_value","method")]
    #   
    #   # Append to results
    #   results[[var]] <- combined_prop
    # }
    
    # Combine all results
    # demog_table <- do.call(rbind, results)
    
    # Set sparse ZIPs to 'Other'
    zip_counts <- table(combined$zip3)
    keep_zip3 <- names(zip_counts[zip_counts >= 10])
    
    combined$zip3_collapsed <- as.character(combined$zip3)
    combined$zip3_collapsed[!combined$zip3_collapsed %in% keep_zip3] <- "Other"
    combined$zip3_collapsed <- factor(combined$zip3_collapsed)
    
    # Use collapsed ZIPs in demographics
    demographics <- c("sex", "age_group", "ethnicity", "race", "zip3_collapsed")
    
    # Safe test function
    safe_test_p <- function(tbl) {
      tbl <- as.matrix(tbl)
      
      # remove zero-sum rows/columns
      tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]
      
      # not enough data to test
      if (nrow(tbl) < 2 || ncol(tbl) < 2) {
        return(list(p.value = NA, method = "Not enough data"))
      }
      
      # use Fisher for 2x2, otherwise chi-square / simulated chi-square
      if (all(dim(tbl) == c(2, 2))) {
        res <- fisher.test(tbl)
        return(list(p.value = res$p.value, method = res$method))
      }
      
      res <- suppressWarnings(chisq.test(tbl))
      
      if (any(is.na(res$expected)) || any(res$expected < 5)) {
        res <- suppressWarnings(chisq.test(tbl, simulate.p.value = TRUE, B = 10000))
        return(list(p.value = res$p.value, method = "Simulated Chi-squared Test"))
      } else {
        return(list(p.value = res$p.value, method = res$method))
      }
    }
    
    # Initialize results list
    results <- list()
    
    for (var in demographics) {
      
      # contingency table for test
      tab <- table(combined[[var]], combined$group)
      
      # proportions by group
      prop <- round(prop.table(tab, margin = 2) * 100, 2)
      
      # convert to data frame
      combined_prop <- as.data.frame.matrix(prop)
      combined_prop$Variable <- rownames(combined_prop)
      rownames(combined_prop) <- NULL
      
      # run test once
      test_res <- safe_test_p(tab)
      p_val <- test_res$p.value
      method <- test_res$method
      
      # add p-value and method
      combined_prop$p_value <- ""
      combined_prop$p_value[1] <- ifelse(is.na(p_val), "", round(p_val, 3))
      
      combined_prop$method <- ""
      combined_prop$method[1] <- method
      
      # add variable name
      combined_prop$Demographic <- var
      
      # reorder columns
      combined_prop <- combined_prop[, c("Demographic", "Variable", "Baseline", "Cluster", "p_value", "method")]
      
      # store
      results[[var]] <- combined_prop
    }
    
    # bind all results
    demographic_results <- do.call(rbind, results)
    rownames(demographic_results) <- NULL
    
    demog_table <- demographic_results
    
    demog_table <- demog_table[!demog_table$Variable %in% c("M", "NOT HISPANIC OR LATINO"), ]
    rownames(demog_table) <- NULL
    
    demog_table$Baseline <- as.numeric(demog_table$Baseline)
    demog_table$Cluster <- as.numeric(demog_table$Cluster)
    
    demog_table$Diff <- round(demog_table$Cluster - demog_table$Baseline, 2)
    
    demog_table=demog_table[,c("Demographic","Variable","Cluster","Baseline","Diff","p_value","method")]
    names(demog_table)=c("Demographic","Variable","ClusterPercent","BaselinePercent","ClusterMinusBaselinePercent","ChiSq_Fis_Pval","Method")
    
    # Hospital Graph
    hosp_c=data.frame(table(temp$hospital[which(temp$dxtype!=-1)])/nrow(temp[which(temp$dxtype!=-1),]))
    hosp_b=data.frame(table(temp1$hospital[which(temp1$dxtype!=-1)])/nrow(temp1[which(temp1$dxtype!=-1),]))
    names(hosp_c)=c("hospital","ClusterPct")
    names(hosp_b)=c("hospital","BaselinePct")
    hosp_cb=merge(hosp_c,hosp_b,by="hospital",all=TRUE)
    hosp_cb$ClusterPct[is.na(hosp_cb$ClusterPct)==T]=0
    hosp_cb$BaselinePct[is.na(hosp_cb$BaselinePct)==T]=0
    hosp_cb$diff=hosp_cb$ClusterPct-hosp_cb$BaselinePct
    hosp_cb=hosp_cb[order(-hosp_cb$diff),]
    hosp_cb$hospital=gsub("HOSPITAL","HOSP",hosp_cb$hospital)
    hosp_cb$hospital=gsub("MEDICAL","MED",hosp_cb$hospital)
    hosp_cb$hospital=gsub("CENTER","CTR",hosp_cb$hospital)
    hospital_barplot <- tempfile(fileext = ".png")
    
    # Increase margins to fit long labels
    png(hospital_barplot, width = 1000, height = 600, res = 150)
    par(mar = c(10, 4, 4, 2))  # Adjust bottom margin for label space
    
    barplot(hosp_cb$diff*100,ylab="%",names.arg=hosp_cb$hospital,las=2,cex.names=0.35,main="Difference in percent of ED visits by hospital between cluster and baseline periods",cex.main=0.8)
    dev.off()
    
    # Create excel with tab sheets
    wb <- createWorkbook()
    
    # Add data frames to sheets
    temp$NYCres=ifelse(substr(temp$zipcode,1,5) %in% df$zipcode,"Y","N")
    temp1$NYCres=ifelse(substr(temp1$zipcode,1,5) %in% df$zipcode,"Y","N")
    
    addWorksheet(wb, "ClusterLinelist")
     writeData(wb, "ClusterLinelist", temp[,c(1:6,ncol(temp),7:16,18:(ncol(temp)-1))])
    
    addWorksheet(wb, "BaselineLinelist")
    writeData(wb, "BaselineLinelist", temp1[,c(1:6,ncol(temp1),7:16,18:(ncol(temp1)-1))])
    
    addWorksheet(wb, "Other_Codes")
    writeData(wb, "Other_Codes", temp3c[,c("Level4","Desc","ClusterFreq", "ClusterPct")], startRow = 1, startCol = 1)
    writeData(wb, "Other_Codes", temp3b[,c("Level4","Desc","BaselineFreq", "BaselinePct")], startRow = 1, startCol = 6)
    
    addWorksheet(wb, "TopCC_words")
    writeData(wb, "TopCC_words", temp_words)
    
    highlight_rows <- which(is.na(temp_words$BaselineFreq))[1:30]
    
    # Create yellow highlight style
    yellow_style <- createStyle(fgFill = "yellow")  # Yellow
    
    # Apply the style to each matching row
    for (j in highlight_rows) {
      addStyle(
        wb, 
        sheet = "TopCC_words",
        style = yellow_style,
        rows = j + 1,  # +1 to account for header row
        cols = 1:2,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    
    # Weekly trends
    addWorksheet(wb, "Trends")
    insertImage(wb, sheet = "Trends", file = code_trend, startRow = 1, startCol = 1, width = 10, height = 6)
    
  # Maps   
    addWorksheet(wb, "Maps")
    
    # Insert image into worksheet
    insertImage(wb, sheet = "Maps", file = ggsave("map.jpg", plot = p, width = 6, height = 5, dpi = 300), 
                startRow = 1, startCol = 1, width = 6, height = 5, units = "in")
    
    insertImage(wb, sheet = "Maps", file = ggsave("map1.jpg", plot = p1, width = 6, height = 5, dpi = 300), 
                startRow = 1, startCol = 8, width = 6, height = 5, units = "in")
    
    # Hospitals
    addWorksheet(wb, "Hospitals")
    insertImage(wb, sheet = "Hospitals", file = hospital_barplot, startRow = 1, startCol = 1, width = 10, height = 6)
    
    # demog table
    addWorksheet(wb, "Demographics")
    writeData(wb, "Demographics", demog_table)
    
    highlight_rows <- which(demog_table$ChiSq_Fis_Pval!="" & demog_table$ChiSq_Fis_Pval<0.05)
    
    # Create yellow highlight style
    yellow_style <- createStyle(fgFill = "yellow")  # Yellow
    
    # Apply the style to each matching row
    for (j in highlight_rows) {
      addStyle(
        wb, 
        sheet = "Demographics",
        style = yellow_style,
        rows = j + 1,  # +1 to account for header row
        cols = 6,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    writeData(wb, "Demographics", "Missing and unknown race and ethnicity categories are excluded from chi-squared tests", startRow = nrow(demog_table) + 3, startCol = 1)
    
    # Save workbook: provide folder path
    saveWorkbook(wb, paste0(parent_dir,"/signal_interpretation/", END_DATE, "_", gsub("\\|", "_",gsub("2\\-","",TS_Results_today$Node.Identifier[i])),".xlsx"), overwrite = TRUE)
      }  
  
  # If baseline is empty
  if(nrow(temp1)==0)
  {
    # Merging original and study dataset to identify incident vs. non-incident
        if(nrow(temp2)>0)
    {
      temp2$dxtype=0
      temp2$dxtype[which(temp2$dispo=="A")]=1
      temp2=temp2[,-3]
      temp$date=as.Date(temp$date)
      temp2$date=as.Date(temp2$date)
      temp=merge(temp,temp2,by=c("date","key"),all.x=TRUE)
      temp=temp[,c(ncol(temp),1:(ncol(temp)-1))]
      temp$dxtype[is.na(temp$dxtype)==T]=-1
    }
    
    if(nrow(temp21)>0)
    {
      temp21$dxtype=0
      temp21$dxtype[which(temp21$dispo=="A")]=1
      temp21=temp21[,-3]
      temp1$date=as.Date(temp1$date)
      temp21$date=as.Date(temp21$date)
      temp1=merge(temp1,temp21,by=c("date","key"),all.x=TRUE)
      temp1=temp1[,c(ncol(temp1),1:(ncol(temp1)-1))]
      temp1$dxtype[is.na(temp1$dxtype)==T]=-1
    }
    
    # if 1- then only keep Adverse visits
    if(grepl("1\\-",TS_Results_today$Node.Identifier[i])==T)
    {temp=temp[which(temp$dispo=="A"),]
    temp1=temp1[which(temp1$dispo=="A"),]
    ed_keep=ed_keep[which(ed_keep$dispo=="A"),]}
    
    # order by date and time
    temp$time=format(strptime(temp$time, "%H:%M"), "%H:%M")
    temp=temp[order(temp$date,temp$time),]
    
    # order by date and time
    temp1$time=format(strptime(temp1$time, "%H:%M"), "%H:%M")
    temp1=temp1[order(temp1$date,temp1$time),]
    
    #For multi-year trend lines
    ed_keep=cbind(MMWRweek(ed_keep$date)[,-3],ed_keep)
    ed_keep=aggregate(n~MMWRyear+MMWRweek,data=ed_keep,sum)
    ed_keep=merge(mmwrwks,ed_keep,all.x=TRUE)
    ed_keep$n[is.na(ed_keep$n)==T]=0
    ed_keep=ed_keep[ed_keep$MMWRyear %in% yr_list,]
    ed_keep=ed_keep[order(ed_keep$MMWRyear,ed_keep$MMWRweek),]
    rownames(ed_keep) <- seq_len(nrow(ed_keep))
    
    # Multi-year Weekly count graph
        # check is it an outlier trend for max signals
    complete_week=ifelse(weekdays(END_DATE)=="Saturday",ed_keep$MMWRweek[nrow(ed_keep)],ed_keep$MMWRweek[nrow(ed_keep)-1])
    
    hlm_weeks=which(ed_keep$MMWRweek[-c((nrow(ed_keep)-1):nrow(ed_keep))]==complete_week)
    hlm_weeks=c(hlm_weeks-1,hlm_weeks,hlm_weeks+1)
    hlm_weeks=hlm_weeks[hlm_weeks>=1]
    
    hlm_data=ed_keep[hlm_weeks,]
    hlm_data=hlm_data[order(hlm_data$MMWRyear,hlm_data$MMWRweek),]
    
    if(ed_keep$n[ed_keep$MMWRyear==max(ed_keep$MMWRyear) & ed_keep$MMWRweek==complete_week]>mean(hlm_data$n)+2*sd(hlm_data$n))
    {sigs_maxout <- c(sigs_maxout, as.character(TS_Results_today$Node.Identifier[i]))}
    
    code_trend <- tempfile(fileext = ".png")
    png(code_trend, width = 1000, height = 600, res = 150)
    par(mar = c(6, 4, 1, 2))  # Adjust bottom margin for label space
    
    plot(1:52,rep(0,52),type="n",xlim=c(1,52),ylim=c(0,ceiling(max(ed_keep$n)*1.1)),xlab="CDC_Week",ylab=gsub("2\\-","",paste(TS_Results_today$Node.Identifier[i])),xaxt="n")
    axis(1,at=seq(1,52,2),labels=seq(1,52,2),cex.axis=0.6,las=2)
    
    # Trends where last point we note if partial or full week
    if(weekdays(END_DATE)=="Saturday")
    {for(k in 1:length(unique(ed_keep$MMWRyear)))
    {lines(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
      legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep$MMWRyear),bty="n",ncol=4)}
    
    if(weekdays(END_DATE)%in% c("Wednesday", "Thursday", "Friday"))
    {for(k in 1:length(unique(ed_keep$MMWRyear)))
    {lines(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
      if(k==length(unique(ed_keep$MMWRyear))) 
      { lastpt=length(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])])
      points(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])][lastpt],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])][lastpt], pch = 21, bg = NA, col = "red", cex = 1, lwd = 1)
      legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep$MMWRyear),bty="n",ncol=4)}
      lg=legend("topright",pch=21,col="Red",legend="partial week",cex=0.5,bty="n",plot=FALSE)
      legend(x = lg$rect$left, y = lg$rect$top*0.85,  # adjust 0.05 as needed
             pch = 21, col = "red", legend = "partial week", 
             cex = 0.5, bty = "n")}
    
    if(weekdays(END_DATE) %in% c("Sunday","Monday","Tuesday"))
    {ed_keep1=ed_keep[-length(ed_keep$MMWRyear),]
    for(k in 1:length(unique(ed_keep1$MMWRyear)))
    {lines(ed_keep1$MMWRweek[which(ed_keep1$MMWRyear==unique(ed_keep1$MMWRyear)[k])],ed_keep1$n[which(ed_keep1$MMWRyear==unique(ed_keep1$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
    legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep1$MMWRyear),bty="n",ncol=4)}
    dev.off()
    
    # Map for Cluster
    temp$zipcode=substr(temp$zipcode,1,5)
    temp_z=merge(temp,df[,c("zipcode","MODZCTA20")],by="zipcode",all.x=TRUE)
    temp_z=data.frame(table(temp_z$MODZCTA20))
    temp_z$pct=round(temp_z$Freq/sum(temp_z$Freq),4)
    names(temp_z)[1]="modzcta"
    
    modzcta1 <- left_join(modzcta, temp_z, by = "modzcta")
    
    p <- ggplot(modzcta1) +
      geom_sf(aes(fill = pct), color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "plasma", name = "Cluster",labels = label_number(accuracy = 0.001) ) +
      theme_minimal() +
      labs(title = paste("NYC MODZCTA Choropleth Map for ",TS_Results_today$Node.Identifier[i]," cluster"))
    
    # Other co-diagnosis codes for cluster
    process <- function(x) {
      tokens <- unlist(strsplit(x, " "))
      tokens <- tokens[tokens != ""]  # remove empty strings
      unique_subs <- unique(substr(tokens, 1, 3))
      paste(unique_subs, collapse = " ")
    }
    
    temp3=gsub("\\|","",temp[which(temp$dxtype!=-1),"diagnosiscode1"])
    result <- data.frame(dx = sapply(temp3, process))
    temp3c=data.frame(table(dx=unlist(strsplit(result[,1], " "))))
    temp3c$dx=gsub("\\s+","",temp3c$dx)
    temp3c=merge(temp3c,icd10.2026_treefilew_FINAL[,c("Name1","Desc")],by.x="dx",by.y="Name1",all.x=T)
    temp3c=temp3c[order(-temp3c$Freq),]
    names(temp3c)[1:2]=c("Level4","ClusterFreq")
    temp3c$ClusterPct=round((temp3c$ClusterFreq*100)/length(temp3),1)
    
    # Top cc words for cluster
    temp4=as.data.frame(table(toupper(unlist(lapply(strsplit(gsub(x=temp$chiefcomplaint[which(temp$dxtype!=-1)],pattern="[[:punct:]]",replacement=" "),split=" "),unique)))))
    temp4=temp4[!temp4$Var1 %in% c("","THE","A","AN","OF","AND","TO","IN","FOR","WITH","ON","IS","WAS","ARE","BY","AT","FROM","MY","HIS","HER","HE","SHE","AS","PER","I","HAS","HAVE","PT","PATIENT","STATES"),]
    temp4=temp4[nchar(as.character(temp4$Var1))>=2,]
    names(temp4)=c("CC_word","ClusterFreq")
    temp4$ClusterPct=round((temp4$ClusterFreq*100)/length(temp$chiefcomplaint[which(temp$dxtype!=-1)]),1)
    
    temp_words=temp4
    temp_words=temp_words[order(-temp_words$ClusterFreq),]
    
    # Demographic tables
    temp_d=temp[which(temp$dxtype!=-1),c("sex","age_group", "race","ethnicity","zipcode")]
    temp_d$group <- "Cluster"
    combined=rbind(temp_d)
    age_levels <- c("<1","1-4", "5-12", "13-17", "18-49", "50-64", "65-79", "80+")
    combined$age_group<- factor(combined$age_group, levels = age_levels)
    combined$sex=toupper(combined$sex)
    combined$boro="Outside"
    combined$boro[grepl("\\b100|10128",combined$zipcode)]="Manhattan"
    combined$boro[grepl("\\b112",combined$zipcode)]="Brooklyn"
    combined$boro[grepl("\\b104",combined$zipcode)]="Bronx"
    combined$boro[grepl("\\b113|\\b114|\\b116|\\b111|\\b110",combined$zipcode)]="Queens"
    combined$boro[grepl("\\b103",combined$zipcode)]="StatenIs"
    ref_boro=ifelse(length(combined$boro[grepl("Manhattan",combined$boro)==T])>=1,"Manhattan",intersect(c("Brooklyn","Bronx","Queens","StatenIs"),unique(combined$boro))[1])
    combined$boro=relevel(factor(combined$boro),ref=ref_boro)
    combined1=combined[which(combined$race!="MISSUNK"),]
    combined1=combined1[which(combined1$ethnicity!="MISSUNK"),]
    # List of demographic variables
    demographics <- c("sex","age_group","ethnicity", "race","boro")
    
    # Initialize results list
    results <- list()
    
    for (var in demographics) {
      # Create contingency table
      tab <- table(combined1[[var]])
      
      # Calculate proportions by group
      prop <- round(prop.table(table(combined[[var]])) * 100,2)  # Column-wise percentages
      
      # Convert to data frame
      combined_prop <- as.data.frame(prop)
      combined_prop$Variable <- combined_prop$Var1
      rownames(combined_prop) <- NULL
      
      combined_prop$Demographic <- var
      combined_prop$Cluster <- combined_prop$Freq
      # Reorder columns
      combined_prop <- combined_prop[, c("Demographic","Variable", "Cluster")]
      
      # Append to results
      results[[var]] <- combined_prop
    }
    
    # Combine all results
    demog_table <- do.call(rbind, results)
    
    demog_table=demog_table[!demog_table$Variable %in% c("M","NOT HISPANIC OR LATINO"),]
    rownames(demog_table) <- NULL
    demog_table=demog_table[,c("Demographic","Variable","Cluster")]
    names(demog_table)=c("Demographic","Variable","ClusterPercent")
    
    # Hospital Graph for cluster percent
    hosp_c=data.frame(table(temp$hospital[which(temp$dxtype!=-1)])/nrow(temp[which(temp$dxtype!=-1),]))
    names(hosp_c)=c("hospital","ClusterPct")
    hosp_cb=hosp_c
    hosp_cb$ClusterPct[is.na(hosp_cb$ClusterPct)==T]=0
    hosp_cb=hosp_cb[order(-hosp_cb$ClusterPct),]
    hosp_cb$hospital=gsub("HOSPITAL","HOSP",hosp_cb$hospital)
    hosp_cb$hospital=gsub("MEDICAL","MED",hosp_cb$hospital)
    hosp_cb$hospital=gsub("CENTER","CTR",hosp_cb$hospital)
    hospital_barplot <- tempfile(fileext = ".png")
    
    # Increase margins to fit long labels
    png(hospital_barplot, width = 1000, height = 600, res = 150)
    par(mar = c(10, 4, 4, 2))  # Adjust bottom margin for label space
    
    barplot(hosp_cb$ClusterPct*100,ylab="%",names.arg=hosp_cb$hospital,las=2,cex.names=0.35,main="Percent of ED visits by hospital for cluster",cex.main=0.8)
    dev.off()
    
    install.packages("openxlsx")
    library("openxlsx")
    
    # Create excel with tab sheets
    wb <- createWorkbook()
    
    # Add data frames to sheets
    temp$NYCres=ifelse(substr(temp$zipcode,1,5) %in% df$zipcode,"Y","N")
    temp1$NYCres=ifelse(substr(temp1$zipcode,1,5) %in% df$zipcode,"Y","N")
    
    addWorksheet(wb, "ClusterLinelist")
    writeData(wb, "ClusterLinelist", temp[,c(1:6,ncol(temp),7:16,18:(ncol(temp)-1))])
    
    addWorksheet(wb, "BaselineLinelist")
    writeData(wb, "BaselineLinelist", temp1[,c(1:6,ncol(temp1),7:16,18:(ncol(temp1)-1))])
    
    addWorksheet(wb, "Other_Codes")
    writeData(wb, "Other_Codes", temp3c[,c("Level4","Desc","ClusterFreq", "ClusterPct")], startRow = 1, startCol = 1)
    
    addWorksheet(wb, "TopCC_words")
    writeData(wb, "TopCC_words", temp_words)
    
    # Weekly trends
    addWorksheet(wb, "Trends")
    insertImage(wb, sheet = "Trends", file = code_trend, startRow = 1, startCol = 1, width = 10, height = 6)
    
    addWorksheet(wb, "Maps")
        # Insert image into worksheet
    insertImage(wb, sheet = "Maps", file = ggsave("map.jpg", plot = p, width = 6, height = 5, dpi = 300), 
                startRow = 1, startCol = 1, width = 6, height = 5, units = "in")
    
        # Hospitals
    addWorksheet(wb, "Hospitals")
    insertImage(wb, sheet = "Hospitals", file = hospital_barplot, startRow = 1, startCol = 1, width = 10, height = 6)
    
    # demog table
    addWorksheet(wb, "Demographics")
    writeData(wb, "Demographics", demog_table)
    
    writeData(wb, "Demographics", "Missing and unknown race and ethnicity categories are excluded from chi-squared tests", startRow = nrow(demog_table) + 3, startCol = 1)
    
    # Save workbook; provide folder path
    saveWorkbook(wb, paste0(Folderpath,"/Linelist_",gsub("\\|", "_",gsub("2\\-","",TS_Results_today$Node.Identifier[i])),".xlsx"), overwrite = TRUE)
    }    
  
}
