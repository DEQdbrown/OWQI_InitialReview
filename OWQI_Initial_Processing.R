################################################################################
### This query pulls the data necessary to calculate the OWQI each year.     ###
### Update lines 29-31 upon initial run each OWQI cycle. Run line 10 to      ###
### install devtools, if you don't have it installed. Run line 11            ###
### periodically to capture any updates to the AWQMSdata package. There are  ###
### stops included in the code. These will appear as errors and prompt you   ###
### to review a dataframe. For more info see details in code.                ###
################################################################################

#install.packages("devtools")
#devtools::install_github("TravisPritchardODEQ/AWQMSdata",dependencies = TRUE, force = TRUE, upgrade = FALSE)

### Load tools and packages necessary for this script
library(tidyverse)
library(AWQMSdata)
library(readxl)
library(DBI)
library(RODBC)
library(openxlsx)
library(ggplot2)

### Disable scientific notation
options(scipen = 999)

### Set working directory
setwd("//deqlab1/WQM/OWQI/R/Final/InitialDataProcessing")

### Set the data window by changing these dates each new water year 
Start_Date <- '2023-10-01' 
End_Date <- '2024-09-30'
WaterYear <- 2024
LastWaterYear <- 2023


### Pull in outlier data for later use ###
Out_Data <- read_xlsx(str_glue("10yrOWQIRawData_{LastWaterYear}.xlsx"))

#===============================================================================
# Initial Data Pull and Organization -------------------------------------------
#===============================================================================

### Pull data from AWQMS database
Raw_Data <- AWQMS_Data(startdate = Start_Date, enddate = End_Date, 
  project = 'Surface Water Ambient Monitoring', 
  Char_Name = c('temperature, water',
          'dissolved oxygen (DO)','dissolved oxygen saturation',
          'biochemical oxygen demand, non-standard conditions','pH','total solids',
          'Ammonia', 'Nitrate + Nitrite','Total phosphorus, mixed forms','Conductivity',
          'escherichia coli'), 
  MLocID = c('10332-ORDEQ', '10339-ORDEQ', '10344-ORDEQ', '10350-ORDEQ', '10352-ORDEQ',
          '10355-ORDEQ', '10359-ORDEQ', '10363-ORDEQ', '10366-ORDEQ', '10373-ORDEQ', 
          '10376-ORDEQ', '10386-ORDEQ', '10391-ORDEQ', '10393-ORDEQ', '10404-ORDEQ', 
          '10406-ORDEQ', '10407-ORDEQ', '10410-ORDEQ', '10411-ORDEQ', '10414-ORDEQ', 
          '10418-ORDEQ', '10421-ORDEQ', '10423-ORDEQ', '10428-ORDEQ', '10437-ORDEQ', 
          '10441-ORDEQ', '10442-ORDEQ', '10443-ORDEQ', '10451-ORDEQ', '10456-ORDEQ', 
          '10458-ORDEQ', '10459-ORDEQ', '10461-ORDEQ', '10469-ORDEQ', '10480-ORDEQ', 
          '10506-ORDEQ', '10508-ORDEQ', '10511-ORDEQ', '10517-ORDEQ', '10521-ORDEQ', 
          '10523-ORDEQ', '10533-ORDEQ', '10537-ORDEQ', '10555-ORDEQ', '10596-ORDEQ', 
          '10602-ORDEQ', '10611-ORDEQ', '10616-ORDEQ', '10637-ORDEQ', '10640-ORDEQ', 
          '10662-ORDEQ', '10663-ORDEQ', '10674-ORDEQ', '10686-ORDEQ', '10688-ORDEQ', 
          '10690-ORDEQ', '10696-ORDEQ', '10719-ORDEQ', '10720-ORDEQ', '10724-ORDEQ', 
          '10728-ORDEQ', '10729-ORDEQ', '10730-ORDEQ', '10741-ORDEQ', '10759-ORDEQ', 
          '10763-ORDEQ', '10764-ORDEQ', '10765-ORDEQ', '10768-ORDEQ', '10770-ORDEQ', 
          '10792-ORDEQ', '10801-ORDEQ', '10812-ORDEQ', '10817-ORDEQ', '10917-ORDEQ', 
          '10929-ORDEQ', '10948-ORDEQ', '10996-ORDEQ', '10997-ORDEQ', '11017-ORDEQ', 
          '11020-ORDEQ', '11043-ORDEQ', '11047-ORDEQ', '11050-ORDEQ', '11051-ORDEQ', 
          '11140-ORDEQ', '11180-ORDEQ', '11182-ORDEQ', '11201-ORDEQ', '11233-ORDEQ', 
          '11241-ORDEQ', '11263-ORDEQ', '11275-ORDEQ', '11321-ORDEQ', '11386-ORDEQ', 
          '11434-ORDEQ', '11457-ORDEQ', '11476-ORDEQ', '11477-ORDEQ', '11478-ORDEQ', 
          '11479-ORDEQ', '11480-ORDEQ', '11482-ORDEQ', '11483-ORDEQ', '11484-ORDEQ', 
          '11486-ORDEQ', '11489-ORDEQ', '11490-ORDEQ', '11491-ORDEQ', '11493-ORDEQ', 
          '11494-ORDEQ', '11521-ORDEQ', '11522-ORDEQ', '11856-ORDEQ', '11904-ORDEQ', 
          '11905-ORDEQ', '12005-ORDEQ', '12012-ORDEQ', '12187-ORDEQ', '12261-ORDEQ', 
          '12264-ORDEQ', '12265-ORDEQ', '12266-ORDEQ', '12267-ORDEQ', '12552-ORDEQ', 
          '12553-ORDEQ', '12559-ORDEQ', '12590-ORDEQ', '13014-ORDEQ', '13070-ORDEQ', 
          '13411-ORDEQ', '13417-ORDEQ', '13421-ORDEQ', '13424-ORDEQ', '13433-ORDEQ', 
          '13440-ORDEQ', '13570-ORDEQ', '13574-ORDEQ', '14008-ORDEQ', '21535-ORDEQ', 
          '28333-ORDEQ', '33266-ORDEQ', '33603-ORDEQ', '33642-ORDEQ', '33644-ORDEQ', 
          '33922-ORDEQ', '33929-ORDEQ', '33930-ORDEQ', '34019-ORDEQ', '36776-ORDEQ', 
          '36778-ORDEQ', '36783-ORDEQ', '36784-ORDEQ', '36785-ORDEQ', '36786-ORDEQ', 
          '36787-ORDEQ', '36788-ORDEQ', '36790-ORDEQ', '36805-ORDEQ', '36875-ORDEQ'),
  filterQC = FALSE)

#===============================================================================
# Data Cleanup -----------------------------------------------------------------
#===============================================================================

### Filter out the columns we don't use for the OWQI and add -8888 for samples with a DQL of C, D, or E
filtered_table <- Raw_Data %>%
  select("Station" = "MLocID", "ElementID" = "act_id", "date" = "SampleStartDate", 
         Char_Name, Result_Text, Result_Numeric, DQL) %>%
  mutate(Result_Numeric = if_else(str_detect(DQL, "C|D|E"), -8888, Result_Numeric),
         date = as.Date(date)) %>%
  mutate(Result_Numeric = if_else(str_detect(Result_Text, "<") & Result_Numeric != -8888, Result_Numeric*0.5, Result_Numeric)) %>%
  select(-Result_Text) %>%
  distinct(Station, ElementID, date, Char_Name, .keep_all = TRUE)

### Create a table where each characteristic is a column and names are shortened. Add UseNOWQI column
OWQI_PVT <- filtered_table %>%
  pivot_wider(names_from = Char_Name, values_from = Result_Numeric, values_fn = list(breaks = mean)) %>%
  rename(cond = 'Conductivity', NIT = 'Nitrate + Nitrite', 
         DOSf = "Dissolved oxygen saturation", P = "Total Phosphorus, mixed forms", 
         TS = "Total solids", BOD = "Biochemical oxygen demand, non-standard conditions",
         Ecoli = "Escherichia coli", DOf = "Dissolved oxygen (DO)",
         Tempf = "Temperature, water", NH3 = 'Ammonia', ph = 'pH') %>%
  mutate(UseNOWQI = NA, Station = str_remove(Station, "-ORDEQ")) %>%
  select(Station, ElementID, date, ph, cond,  NH3, NIT, DOSf, P, TS,
         BOD, Ecoli, DOf, Tempf, UseNOWQI) 
  
### Create columns for field pH and conductivity, remove suffixes for field and reevaluation from ElementIDs, and group rows by station, ElementID and date
DataCleanup1 <- OWQI_PVT %>%
  mutate(pHf = if_else(str_detect(ElementID, "-FM|-QCFM"), ph, NA), 
         condf = if_else(str_detect(ElementID, "-FM|-QCFM"), cond, NA),
         ph = if_else(str_detect(ElementID, "-FM|-QCFM"), NA, ph), 
         cond = if_else(str_detect(ElementID, "-FM|-QCFM"), NA, cond),
         ElementID = str_remove(ElementID,"-FM|RE1|RE2|RE3|-SUB")) %>%
  select(Station, ElementID, date, pHf, ph, condf, cond,  NH3, NIT, DOSf, P, TS,
         BOD, Ecoli, DOf, Tempf, UseNOWQI)%>%
  group_by(Station, ElementID, date) %>%
  summarise(across(pHf:UseNOWQI, ~ifelse(all(is.na(.x)), NA, paste0(na.omit(.x), collapse = NULL))), .groups = "drop")

### Create a list of Quality Control samples and label them as duplicate samples
DataCleanup2 <- DataCleanup1 %>%
  filter(str_detect(ElementID, '-QCFM')) %>%
  mutate(ElementIDFD = str_replace_all(ElementID, "-QCFM","-FD"),
         ElementID = str_remove(ElementID,"-QCFM")) %>%
  select(Station, ElementID, date, ElementIDFD)

### Create a list of duplicate sample ElementIDs that can be used to identify and label the primary samples
DataCleanup3 <- DataCleanup2 %>%
  mutate(FPMatch = str_c(str_replace(Station, "-.*",""), str_replace(ElementID, "-.*", ""), date, sep = "-")) %>%
  select(Station, date, FPMatch)

### Compares the list of duplicate samples to the main list, labels them accordingly, and collapses the rows into one
DataCleanup4 <- DataCleanup1 %>%
  mutate(ElementID = str_remove(ElementID,"-QCFM")) %>%
  left_join(DataCleanup2, by = c("ElementID")) %>%
  mutate(ElementID = case_when(
           is.na(ElementIDFD) ~ ElementID,
           TRUE ~ ElementIDFD)) %>%
  rename(Station = Station.x, date = date.x) %>%
  group_by(Station, ElementID, date) %>%
  summarise(across(pHf:UseNOWQI, ~ifelse(all(is.na(.x)), NA, paste0(na.omit(.x), collapse = NULL))), .groups = "drop") #combines rows with same ElementID

### Compares the list of primary samples to the main list, labels them accordingly and removes unneeded columns
OWQIData4Review <- DataCleanup4 %>%
  mutate(Match = str_c(str_replace(Station, "-.*",""), str_replace(ElementID, "-.*", ""), date, sep = "-")) %>%
  left_join(DataCleanup3, by = c("Station","date")) %>%
  mutate(ElementIDFP = str_c(ElementID,"-FP"),
         ElementID = case_when(
          is.na(FPMatch) ~ ElementID,
          str_detect(ElementID, "-FD") ~ ElementID,
          TRUE ~ ElementIDFP),
         Exclusion = NA,
         ElementID = if_else(str_detect(ElementID, "-FP|-FD"), ElementID, str_c(ElementID,"-GS"))) %>%
  select(-Match, -FPMatch, -ElementIDFP) %>%
  mutate(across(pHf:Tempf, as.numeric)) 

#===============================================================================
# Data Review ------------------------------------------------------------------
#===============================================================================

################################################################################
### This section of the code will identify data that should be excluded from ### 
### the analysis. Data can be excluded based on the DQL, tidal influence or  ###
### sampling issues.                                                         ###
################################################################################

### Pull in historic data and use to determine potential outliers in current year's data
### The first step pulls in the data and calculates the 1st and 99th percentile
OutlierData <- Out_Data %>%
  summarize(pH_1 = quantile(pHf, probs = 0.01, na.rm = TRUE),
            pH_99 = quantile(pHf, probs = 0.99, na.rm = TRUE),
            cond_1 = quantile(condf, probs = 0.01, na.rm = TRUE),
            cond_99 = quantile(condf, probs = 0.99, na.rm = TRUE),
            NH3_1 = quantile(NH3, probs = 0.01, na.rm = TRUE),
            NH3_99 = quantile(NH3, probs = 0.99, na.rm = TRUE),
            NIT_1 = quantile(NIT, probs = 0.01, na.rm = TRUE),
            NIT_99 = quantile(NIT, probs = 0.99, na.rm = TRUE),
            DOSf_1 = quantile(DOSf, probs = 0.01, na.rm = TRUE),
            DOSf_99 = quantile(DOSf, probs = 0.99, na.rm = TRUE),
            P_1 = quantile(P, probs = 0.01, na.rm = TRUE),
            P_99 = quantile(P, probs = 0.99, na.rm = TRUE),
            TS_1 = quantile(TS, probs = 0.01, na.rm = TRUE),
            TS_99 = quantile(TS, probs = 0.99, na.rm = TRUE),
            BOD_1 = quantile(BOD, probs = 0.01, na.rm = TRUE),
            BOD_99 = quantile(BOD, probs = 0.99, na.rm = TRUE),
            Ecoli_1 = quantile(Ecoli, probs = 0.01, na.rm = TRUE),
            Ecoli_99 = quantile(Ecoli, probs = 0.99, na.rm = TRUE),
            DOf_1 = quantile(DOf, probs = 0.01, na.rm = TRUE),
            DOf_99 = quantile(DOf, probs = 0.99, na.rm = TRUE),
            Tempf_1 = quantile(Tempf, probs = 0.01, na.rm = TRUE),
            Tempf_99 = quantile(Tempf, probs = 0.99, na.rm = TRUE))

################################################################################
### This section of code will create shortened lists of potential outliers   ###
### (1st and 99th percentile). Review these data in Element to make sure     ###
### they are accurate. For field measurements check the COC. If any values   ###
### need to be corrected in Element, talk to the Laboratory Data             ###
### Coordinator. Once the Lab Data Coordinator corrects the data in Element  ###
### and AWQMS, rerun the entire script up to this point. If no values need   ###
### corrected, then proceed to the next step.                                ###
################################################################################

#### Estuary sites have not yet been removed from the dataset. There may be high cond and TS values in outliers.
#### Check line 290 to see which sites are estuary 

pH_check <- OWQIData4Review %>%
  filter(pHf != -8888, pHf < OutlierData$pH_1 | pHf > OutlierData$pH_99) %>%
  select(Station, ElementID, date, pHf, ph)

cond_check <- OWQIData4Review %>%
  filter(condf != -8888, condf < OutlierData$cond_1 | condf > OutlierData$cond_99) %>%
  select(Station, ElementID, date, condf, cond)

DOS_check <- OWQIData4Review %>%
  filter(DOSf != -8888, DOSf < OutlierData$DOSf_1 | DOSf > OutlierData$DOSf_99) %>%
  select(Station, ElementID, date, DOSf)

DO_check <- OWQIData4Review %>%
  filter(DOf != -8888, DOf < OutlierData$DOf_1 | DOf > OutlierData$DOf_99) %>%
  select(Station, ElementID, date, DOf)

Temp_check <- OWQIData4Review %>%
  filter(Tempf != -8888, Tempf < OutlierData$Tempf_1 | Tempf > OutlierData$Tempf_99) %>%
  select(Station, ElementID, date, Tempf)

NH3_check <- OWQIData4Review %>%
  filter(NH3 != -8888, NH3 < OutlierData$NH3_1 | NH3 > OutlierData$NH3_99) %>%
  select(Station, ElementID, date, NH3)

NIT_check <- OWQIData4Review %>%
  filter(NIT != -8888, NIT < OutlierData$NIT_1 | NIT > OutlierData$NIT_99) %>%
  select(Station, ElementID, date, NIT)

P_check <- OWQIData4Review %>%
  filter(P != -8888, P < OutlierData$P_1 | P > OutlierData$P_99) %>%
  select(Station, ElementID, date, P)

BOD_check <- OWQIData4Review %>%
  filter(BOD != -8888, BOD < OutlierData$BOD_1 | BOD > OutlierData$BOD_99) %>%
  select(Station, ElementID, date, BOD)

TS_check <- OWQIData4Review %>%
  filter(TS != -8888, TS < OutlierData$TS_1 | TS > OutlierData$TS_99) %>%
  select(Station, ElementID, date, TS)

Ecoli_check <- OWQIData4Review %>%
  filter(Ecoli != -8888, Ecoli < OutlierData$Ecoli_1 | Ecoli > OutlierData$Ecoli_99) %>%
  select(Station, ElementID, date, Ecoli)

stop("Check each check dataframe for outliers. See comment box above for details. Start again on line 272.")

#===============================================================================
# Mark Data for Exclusion ------------------------------------------------------
#===============================================================================

################################################################################
### This section checks for samples that should be excluded from the         ###
### analysis such as voided, missing, high conductivity or because the       ###
### primary can't be used. Be sure to check Element and the COC to confirm   ###
### the reason for exclusion is accurate. If any values need to be corrected ###
### in Element, talk to the Laboratory Data Coordinator. Once the Lab Data   ###
### Coordinator corrects the data in Element and AWQMS, rerun the entire     ###
### script up to this point. If no values need corrected, then proceed to    ###
### the next step.                                                           ###
################################################################################

### Mark any samples with voided results for exclusion

## Note to Dan and Kat 1.2025- Add something into code to remove rows that are not legitimate NAs ###
OWQIData4Review <- OWQIData4Review %>%
  mutate(UseNOWQI = if_else(str_detect(pHf, "-8888") & is.na(ph), "false", UseNOWQI),
         UseNOWQI = if_else(str_detect(condf, "-8888") & is.na(cond), "false", UseNOWQI),
         UseNOWQI = if_else(if_any(NH3:Tempf, ~ str_detect(.,"-8888")), "false", UseNOWQI),
         Exclusion = if_else(str_detect(UseNOWQI, "false") & is.na(Exclusion), "Voided Result", Exclusion))

### Mark any missing samples for review. Missing pHf or condf values are acceptable as long as there's a value for ph or cond
NA_check <- OWQIData4Review %>%
  filter(if_any(c(pHf, condf,NH3:Tempf), ~ is.na(.)), is.na(Exclusion))

stop("Check the NA_check table for missing data. See comment box above for details. Start again on line 285.")

# Based on the NA_check table, mark samples for exclusion. If no data is missing, skip this snippet.
OWQIData4Review <- OWQIData4Review %>%
  mutate(UseNOWQI = 
            if_else(ElementID %in% c("2311078-01-GS", "2311078-02-GS","2311078-03-GS","2311078-04-GS","2311078-05-GS","2311078-06-GS","2311078-07-GS","2311078-08-GS","2311078-09-GS","2311006-01-GS","2311006-02-GS","2311006-03-GS","2409057-06-GS"), "false", UseNOWQI), # The ElementIDs in this line are for example only  
         Exclusion = if_else(str_detect(UseNOWQI, "false") & is.na(Exclusion), "Missing Data", Exclusion))

### Mark any high conductivity results from estuary sites for exclusion 
OWQIData4Review <- OWQIData4Review %>%
  mutate(UseNOWQI = case_when(
           Station %in% c("10812","10817","11493","13570","13574") & condf >= 200 ~ "false",
           Station %in% c("10812","10817","11493","13570","13574") & is.na(condf) & cond >= 200 ~ "false",
           TRUE ~ UseNOWQI),
         Exclusion = if_else(str_detect(UseNOWQI, "false") & is.na(Exclusion), "High Conductivity", Exclusion))

################################################################################
### Compare primary and duplicate samples. If there's an error with the      ###
### primary sample, then the duplicate can potentially be used instead. Run  ###
### lines 312-318 after entering the ElementID(s) of the duplicates that     ###
### need to be used in the analysis in line 320. After running lines 312-318 ###
### or if no replacements are necessary, then run lines 320-325.             ###
################################################################################

FPFD <- OWQIData4Review %>%
  filter(str_detect(ElementID, 'FP|FD'))
# If you sort this table by ElementID, it is easier to review because the primaries and duplicates are paired

stop("Check the FPFD table to determine if duplicate should be used instead of primary. See comment box above for details.")

OWQIData4Review <- OWQIData4Review %>%
  mutate(UseNOWQI = case_when(
    ElementID %in% c("2407259-10-FD") ~ "True", # The ElementIDs in this line are for example only # 
    str_detect(ElementID, "FP") & is.na(UseNOWQI) ~ "True",
    str_detect(ElementID, "FD") ~ "false",
    TRUE ~ UseNOWQI),
    Exclusion = if_else(str_detect(UseNOWQI, "false") & is.na(Exclusion), "Used Primary", Exclusion))

OWQIData4Review <- OWQIData4Review %>%
  mutate(UseNOWQI = case_when(
    str_detect(ElementID, "FP") & is.na(UseNOWQI) ~ "True",
    str_detect(ElementID, "FD") & is.na(UseNOWQI) ~ "false",
    TRUE ~ UseNOWQI),
    Exclusion = if_else(str_detect(UseNOWQI, "false") & is.na(Exclusion), "Used Primary", Exclusion))
  
### Double check that everything marked false makes sense
FindFalse <- OWQIData4Review %>%
  filter(UseNOWQI == "false")

stop("Check the FindFalse table to ensure everything looks good, then proceed from line 334.")

### Once all the checks are complete, errors have been fixed, etc., then mark the remaining results for use in the OWQI
FinalOWQIData4Review <- OWQIData4Review %>%
  mutate(UseNOWQI = if_else(is.na(UseNOWQI), "True", UseNOWQI)) %>%
  select(-Exclusion) %>%
  mutate_if(is.double, as.character) %>%
  mutate(Station = as.integer(Station))

#===============================================================================
# Append Data to SQL Database --------------------------------------------------
#===============================================================================

### Connect to the Repository database and insert the current water year's data
repo.sql <- odbcDriverConnect(connection = "driver={SQL Server}; server=DEQLEAD-LIMS; database=Repository; trustedconnection=yes")
RawData_SQL <- sqlFetch(repo.sql, 'OWQIRawData') 

stop("Check for the current year's data in the RawData_SQL table, then proceed from line 326.")

################################################################################
### Check if the current year data is in the RawData_SQL table. If the data  ###
### is there but needs updated, then run lines 357 and 358 to delete the     ###
### existing data and save the new dataset. If there is no data for the      ###
### current water year in the RawData_SQL table, then run line 359 only.     ###
################################################################################

sqlQuery(repo.sql, str_glue("DELETE FROM OWQIRawData WHERE date >= '{Start_Date}'"))
sqlSave(repo.sql, FinalOWQIData4Review, tablename = "OWQIRawData", append = TRUE, rownames = FALSE)

### Pull data for OWQI score calculation
# Full_Data <- sqlFetch(repo.sql, 'vOWQIdata') # View strips last digit off the results, so don't use
PreWY2015_Data <- sqlFetch(repo.sql, 'OWQIHistorical')

### Pull data for next year's outlier calculations and close SQL connection
Hist_Data <- sqlFetch(repo.sql, 'OWQIRawData')
close(repo.sql)

### Prepare last 10 years of data for use in outlier calculation
# Pre-WY2023 data has DQLs concatenated with the results in SQL database which need to be removed. 
# Post-WY2023 data is not concatenated with the results.
# No need to update this code with the last water year.

Pre23_Data <- Hist_Data %>%
  mutate(date = as.Date(date)) %>%
  filter(UseNOWQI != 'false',
         date <= "2022-10-01") %>%
  mutate(pHf = if_else(pHf == "-8888D"|is.na(pHf)|str_detect(pHf, "C"), ph, pHf),
         condf = if_else(condf == "-8888D"|is.na(condf)|str_detect(condf, "C"), cond, condf),
         pHf = str_sub(pHf, 1, -2),
         ph = str_sub(ph, 1, -2),
         condf = str_sub(condf, 1, -2),
         cond = str_sub(cond, 1, -2),
         NH3 = str_sub(NH3, 1, -2),
         NIT = str_sub(NIT, 1, -2),
         DOSf = str_sub(DOSf, 1, -2),
         P = str_sub(P, 1, -2),
         TS = str_sub(TS, 1, -2),
         BOD = str_sub(BOD, 1, -2),
         Ecoli = str_sub(Ecoli, 1, -2),
         DOf = str_sub(DOf, 1, -2),
         Tempf = str_sub(Tempf, 1, -2)) %>%
  mutate(across(pHf:Tempf, as.double)) %>%
  select(-ph, -cond)

### From WY2023 on DQLs aren't concatenated with the results  
Post23_Data <- Hist_Data %>%
  mutate(date = as.Date(date)) %>%
  filter(UseNOWQI != 'false',
         date >= "2022-10-01") %>%
  mutate(across(pHf:Tempf, as.double),
         pHf = if_else(pHf == "-8888"|is.na(pHf)|str_detect(pHf, "C"), ph, pHf),
         condf = if_else(condf == "-8888"|is.na(condf)|str_detect(condf, "C"), cond, condf)) %>%
  select(-ph, -cond)

### Combine the two datasets and export
Data4Outlier <- rbind(Pre23_Data, Post23_Data)

write.xlsx(Data4Outlier, str_glue("10yrOWQIRawData_{WaterYear}.xlsx"))     

### Prepare last 10 years of data for OWQI calculation
### Clean data from OWQIHistoric SQL pull
Early_Data <- PreWY2015_Data %>%
  filter(AnalysisCode != 'E') %>%
  select(-Time, -AnalysisCode)

### Clean data originally pulled from OWQIRawData SQL pull
Recent_Data <- Data4Outlier %>%
  rename('Date' = date,  'temp' = Tempf, 'do_sat' = DOSf, 'bod' = BOD, 'ph' = pHf, 
         'ts' = TS, 'nh3' = NH3, 'no2' = NIT, 'p' = P, 'ecoli' = Ecoli, 'd_o' = DOf) %>%
  relocate(c(temp, d_o, do_sat, bod), .before = ph) %>%
  relocate(c(ts, nh3, no2, p), .before = ecoli) %>%
  mutate('fecal' = NA) %>%
  select(-condf, -UseNOWQI, -ElementID)

### Combine datasets
Combined_Data <- rbind(Early_Data, Recent_Data)
 
### Pull station information from the Stations database
stations <- query_stations() %>%
  filter(OrgID == 'OregonDEQ') %>%
  rename(Station = 'station_key') %>%
  select(Station, StationDes, OWRD_Basin)

### Add Station Description and Basin Name
 Full_Data <- Combined_Data %>%
   left_join(stations, by = c("Station")) %>% 
   rename('combo_name' = StationDes) %>%
   mutate(Date = as.Date(Date),
          OWQI_basin = case_when(
            OWRD_Basin %in% c('Willamette','Columbia River','Sandy','Hood') ~ 'Will_Sand_Hood',
            OWRD_Basin %in% c('Mid Coast', 'South Coast', 'North Coast') ~ 'Coast Range',
            OWRD_Basin %in% c('Umatilla', 'Grande Ronde', 'John Day') ~ 'JD_Umat_GR_Crook',
            OWRD_Basin %in% c('Malheur', 'Powder', 'Owyhee', 'Goose & Summer Lakes', 'Malheur Lake') ~ 'Pow_Bur_Mal_Owy',
            OWRD_Basin == 'Deschutes' & str_detect(StationDes, 'Crooked River') ~ 'JD_Umat_GR_Crook',
            TRUE ~ OWRD_Basin)
          ) %>%
   select(-OWRD_Basin) %>%
   relocate(c(StationDes, OWQI_basin), .before = Date) 
  
 ### Write file that will be used in scoring scripts
 write_csv(Full_Data, str_glue("DATA_WY1980_WY{WaterYear}_ALLDATA.csv"))
 
#===============================================================================
# Assessing Sample Completeness ------------------------------------------------
#===============================================================================

################################################################################
### This section of the code provides feedback to the Ambient Monitoring     ###
### Program about their sampling during the water year. The ExcludedData     ###
### file contains all the data excluded from the OWQI assessment and why.    ###
### The Parameter Completeness figure indicates which parameters met the     ###
### 95% goal for this water year. These deliverables will help the program   ###
### make an assessment of their sampling protocols.                          ###
################################################################################

# Filter out duplicates from the FindingFalse table and export
ExcludedData <- FindFalse %>%
  filter(Exclusion != "Used Primary")

write.xlsx(ExcludedData, str_glue("ExcludedData_{WaterYear}"))

# Create and export file of why data was voided
ExcludeReason <- Raw_Data %>%
  filter(DQL %in% c("C","D","E")) %>%
  select(MLocID, Activity_Type, SampleStartDate, Char_Name, Char_Speciation, 
         Sample_Fraction, Result_status, Result_Text, Result_Unit, Activity_Comment, 
         Result_Comment, DQL, QualifierAbbr, QualifierTxt)

write.xlsx(ExcludeReason, str_glue("VoidedData_{WaterYear}"))

# Rename the parameters and count the DQLs for each
ParamComp <- Raw_Data %>%
  select("Station" = "MLocID", "ElementID" = "act_id", "date" = "SampleStartDate", 
         Char_Name, Result_Text, Result_Numeric, DQL) %>%
  mutate(Char_Name = case_when(
    Char_Name == "Conductivity" ~ "Cond",
    Char_Name == "Nitrate + Nitrite" ~ "NIT",
    Char_Name == "Dissolved oxygen saturation" ~  "DOSat",
    Char_Name == "Total Phosphorus, mixed forms" ~ "P", 
    Char_Name == "Total solids" ~ "TS", 
    Char_Name == "Biochemical oxygen demand, non-standard conditions" ~ "BOD",
    Char_Name == "Escherichia coli" ~ "Ecoli",
    Char_Name == "Dissolved oxygen (DO)" ~ "DO",
    Char_Name == "Temperature, water" ~ "Temp",
    Char_Name == "Ammonia" ~ "NH3",
    Char_Name == "pH" ~ "pH")) %>%
  group_by(Char_Name, DQL) %>%
  filter(DQL != 'D') %>%
  summarise(count = n(), .groups = 'drop')

# Calculate the percent of A and B data for each parameter
PercComp <- ParamComp %>%
  group_by(Char_Name) %>%
  summarise(
    total_count = sum(count),
    count_A_B = sum(count[DQL %in% c("A","B")]),
    ratio = count_A_B / total_count
  ) %>%
  mutate(ratio = signif(ratio, 2),
         threshold = if_else(ratio >= 0.95, "Above", "Below"))

# Create a bar chart of the PercComp dataframe and export it
bar <- ggplot(PercComp, aes(x = Char_Name, y = ratio, fill = threshold)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0.95, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = str_glue("Percent Completeness by Parameter {WaterYear}"),
       x = "Parameter Name",
       y = "Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Above" = "steelblue", "Below" = "orange"))

ggsave("Parameter_Completeness.png", plot = bar, width = 10, height = 6, dpi = 300)

save.image(file = str_glue("OWQI_WY{WaterYear}.RData"))