# OWQI_InitialReview
This script is used to review ambient data collected by Oregon DEQ before used in the Oregon Water Quality Index.

In 2024, DEQ made the decision to move the initial review of ambient monitoring program data used for the OWQI to R from MS Access (MS Access was no longer supported by the vendor). Moving the initial review to R creates a consistent, reproducible, and efficient review process, reduces the potential for human errors, and means that the analysis of the OWQI can continue without the need to use MS Access.
 
As a part of this transition, DEQ identified that data used in the OWQI must have a data quality level of A or B. This brings the analysis of the OWQI in line with the Integrated Report assessment methodology and provides consistency in our analysis and reporting on DEQ data. Previously, data with a DQL of E was used in the OWQI analysis; however, these data, identified as being collected under exceptional conditions, do not reflect ambient conditions. An analysis of ambient monitoring data found that a fraction of the more than 100,000 results collected over the last decade had a DQL of E (0.05%) indicating that these results have marginal or no significant impact on the analysis. The R script automatically drops any data with a DQL of C, D, or E from the analysis reducing the chance that these results might be missed during manual review in MS Access.

Additional review steps included in the R script include:
-	outlier checks for each parameter based on the last 10 years’ worth of data
-	missing data check
-	voided data check
-	high conductivity check
-	comparison of primary and duplicate samples
-	table of excluded data for verification

The R script also:
-	adds the final dataset to the historical dataset stored in SQL
-	pulls historical data to calculate the outlier dataset
-	creates a sample completeness file of excluded data and the reason for exclusion
