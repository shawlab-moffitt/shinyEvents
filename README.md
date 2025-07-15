# shinyEvents

https://shawlab-moffitt.shinyapps.io/shinyevents/

# shinyEvents Setup

## Dependencies
The shinyEvents tool was developed under the following R Environement:
* `R` - https://cran.r-project.org/bin/windows/base/old/4.4.1/

|  |  |  |  |
| --- | --- | --- | --- |
| shiny_1.10.0 | shinythemes_1.2.0 | shinyjqui_0.4.1 | shinycssloaders_1.1.0 |
| dplyr_1.1.4 | tidyr_1.3.1 | readr_2.1.5 | ggplot2_3.5.2 |
| data.table_1.17.6 | plotly_4.11.0 | stringr_1.5.1 | ggpubr_0.6.1 |
| RColorBrewer_1.1-3 | reshape2_1.4.4 | rstatix_0.7.2 | survival_3.8-3 |
| gtsummary_2.2.0 | powerSurvEpi_0.1.5 | readxl_1.4.5 | circlize_0.4.16 |
| bslib_0.9.0 | listviewer_4.0.0 | bsicons_0.1.2 | readtext_0.91 |
| shinyWidgets_0.9.0 | DT_0.33 | ggrepel_0.9.6 | survminer_0.5.0 |
| svglite_2.2.1 | ggsankey_0.0.99999 | ComplexHeatmap_2.25.2 | InteractiveComplexHeatmap_1.11.1 |


# Required Data

## Option 1: Event Data Upload

The event data table contains all the information that is needed to power the ShinyEvents application. With only four required columns, users can upload the event data of their patients, select the columns of required data, and click a button to process and explore their longitudinal data.

### Event Data File Format

The event data file can be tab or comma delimited and has a minimum requirement of four columns: **patient ID, event name, event start time, and event end time**. Each row should annotate an event for a single patient, so a patient would typically have multiple rows of events for different clinical time points, such as diagnosis, medications, radiation event, metastases or progressions, as well as death or last contact, among numerous other events a patient may experience during their clinical journey.

A recommended, but not required, column is the event type or category column that can be used to group the events of similar nature. For instance, there may be separate events for different drugs administered. In this case you would add a column (e.g. EventType) to the event data and place the text 'Medication' in that column for each drug event row, and perform this similar annotation for different event groups.

Additional columns of event details or other further supplementary patient information may be added with the utility of being used in data filtering, timeline plot hover text, or as stratification variables for the Kaplan-Meier plot in the time-to-event analysis.

#### Table 1: Event Data Column Descriptions and examples

| Column Type | Note | Description | Examples |
| :---- | :---- | :---------- | :------ |
| Patient ID | Required | Unique identifier for each patient. | 'Patient 1','Patient 2' |
| Event Name | Required | Specific clinical events the patient encountered. | 'Diagnosis','Cisplatin treatment' |
| Event Start Time | Required | Numeric start time for the event with units of years, days, months, or hours. | 58.75, 21458.44 |
| Event End Time | Required if event with start and end time | Numeric end time for the event with units of years, days, months, or hours. Can be left blank if only one time point. | 58.75, 21458.44 |
| Event Type | Optional, single column | Term that can be used to group events of a similar nature. | 'Clinical Time Point','Medication' |
| Event Details | Optional, One or more columns | Details or notes regarding the event on the same row. | 'Stage III','Chemotherapy' |
| Patient Details | Optional, One or more columns | Patient annotations that add patient features. This can be added for all patients and repeated down the column on each event row for the patient. | Patient features such as sex, smoking status, mutation status, and more |


### Optional Supplementary Data

Any additional supplementary patient data may be uploaded to the app to aid in plot annotation, data sub-setting, and time-to-event Kaplan-Meier stratification. This data should be a tabular excel file where each tab/sheet is named properly according to the data it contains and the first column of each sheet should contain the patient identifier that can be linked to the event data.

This data is intended to be linked with the event data, whereas a specific event might have been derived or can be linked to a patient or row from one of the tables in the supplementary data. For this, there must be a column in the event data that can link the event to the proper table/sheet of the supplementary data.

Below in Figure 2 we highlight the file formats accepted by the ShinyEvents application. The Event Data table in this figure has a column named "Event Table" where the column values align with the table/sheet names in the supplementary excel data file. These two files are then linked via the user selection that informs the app which column to link by.

## Option 2: Parameter File Input

While the essential event data table is only of four required columns, the data that it is derived from can be quite extensive and include a variety of different data tables and event column variables and details. To assist in generating this data or for more advanced use cases, it may be preferable to perform some additional pre-processing steps and setup an application instance dedicated to a specific data set. 

This requires a parameter file which would be user derived and contain fields of information that will be used to generate the event data table from more raw or unprocessed data. While this is a manually made file, once it is setup properly, a very detailed event data and ShinyEvents application can be deployed.

### Parameter File

This file should be formatted as a tab or comma delimited table of 11 columns with precise column names as described here. This file is be similar to an instruction manual that guides ShiynEvents to identifying the correct events and event times within their designated data table, as well as grouping events categorically and annotating specific events as treatment or response associated.

The files this table describes should be text files that are located in the app folder, as the app will be reading over this table to read the individual files and extract the desired data to generate the event data. There could be multiple events annotated within a single file or there may also be files of purely supplementary information  that does not correlate to an event but could provide further patient information within the app.

#### Table 3: Parameter File Column Descriptions and Examples

| Column Name | Note | Description | Examples |
| :--- | :--- | :---------- | :----------- |
| Data Table Name | User Derived | Name of data table that can associate with the data file from that row. | 'ClinicalAnnotation','MedRegimen' |
| Data File | File Name | Name of file to read and obtain event information from | 'ShinyEvents_ClinicalAnnotation.txt' |
| Event Name | Column Name or User Derived | If the event is more general or uniform over all patients this would be a user derived name. If the event is linked to a column that provides further specificity this would be the column name of that column. | In the case of a uniform event, this may be 'Death' which may only be defined by the event time column. For a more specific event, such as 'Medication', a user may specify a column in the data named 'Medications' which contains multiple different medication names as a unique event row for a patient.  |
| Column Defined Event | TRUE/FALSE | This value is associated with the type of event name that is defined. If the event if defined by further specificity in a column, this will be TRUE, else this would be FALSE | Referencing the Event Name parameter example above, the event of 'Death' would be FALSE and the event 'Medications' would be TRUE, because the 'Medications' column contains a further defining event name. |
| Event Category | User Derived | Name that can be used to group events of similar nature. | 'Clinical Event','Medication' |
| Event Start Column | Column Name | Name of column in data that defines the event start time. | 'AgeAtDiagnosis','AgeAtMedStart' |
| Event End Column | Column Name | Name of column in data that defines the event end time. Can be left blank if only one time instance for event. | 'AgeAtMedEnd' |
| Treatment | TRUE/FALSE | TRUE or FALSE if the event in this row describes a patient treatment or not. | If event is diagnosis, this would be FALSE. If event was a medication, this would be TRUE |
| Response | TRUE/FALSE | TRUE or FALSE if the event in this row describes a patient response or not. May be of interest to separate positive and negative responses into two event categories. | If event is metastasis, this would be TRUE. If event was a surgery, this would be FALSE |
| Event Start Time Units | User Defined | The unit of time used in the event start column. | Must be one of the following: 'Years','Months','Days','Hours' |
| Event End Time Units | User Defined | The unit of time used in the event end column. | Must be one of the following: 'Years','Months','Days','Hours' |

#### Optional Pre-Processing

To aid in application setup, the user can perform pre-processing steps with the prepared parameter file to generate the event data file and patient selection table prior to app start-up. This is helpful when working with larger data sets, as part of the application is event clustering which can take additional computing time to analyse, and if only the parameter file is used as input this step will run every time the application is deployed.

##### Event Data File

We have provided some helpful functions that allow users to generate an event data table from the parameter file. The `getEventData()` function is located in the `R/ShinyEvents_Functions.R` file. Upon sourcing this file with `source('R/ShinyEvents_Functions.R')` users can run this function with the parameter file as the input, which will create a data.frame object of the event data. This can be written to file and used in the app through the front-end UI data upload or the back-end application start-up.

# shinyEvents Application Key Features


# Application sessionInfo()

```{r}
> sessionInfo()
R version 4.4.1 (2024-06-14 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 10 x64 (build 19045)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] grid      stats     graphics  grDevices datasets  utils     methods   base     

other attached packages:
 [1] InteractiveComplexHeatmap_1.11.1 ComplexHeatmap_2.25.2            ggsankey_0.0.99999               svglite_2.2.1                    readtext_0.91                   
 [6] bsicons_0.1.2                    listviewer_4.0.0                 shinycssloaders_1.1.0            shinyjs_2.1.0                    shinythemes_1.2.0               
[11] bslib_0.9.0                      circlize_0.4.16                  shinyWidgets_0.9.0               tidyr_1.3.1                      readxl_1.4.5                    
[16] powerSurvEpi_0.1.5               gtsummary_2.2.0                  survminer_0.5.0                  survival_3.8-3                   rstatix_0.7.2                   
[21] reshape2_1.4.4                   ggpubr_0.6.1                     RColorBrewer_1.1-3               ggrepel_0.9.6                    plotly_4.11.0                   
[26] stringr_1.5.1                    DT_0.33                          data.table_1.17.6                ggplot2_3.5.2                    dplyr_1.1.4                     
[31] readr_2.1.5                      shinyjqui_0.4.1                  shiny_1.10.0                    

loaded via a namespace (and not attached):
 [1] gridExtra_2.3       remotes_2.5.0       rlang_1.1.6         magrittr_2.0.3      clue_0.3-66         GetoptLong_1.0.5    matrixStats_1.5.0   compiler_4.4.1     
 [9] png_0.1-8           systemfonts_1.2.3   vctrs_0.6.5         crayon_1.5.3        pkgconfig_2.0.3     shape_1.4.6.1       fastmap_1.2.0       backports_1.5.0    
[17] fontawesome_0.5.3   KMsurv_0.1-6        promises_1.3.3      rmarkdown_2.29      tzdb_0.5.0          pracma_2.4.4        purrr_1.0.4         xfun_0.52          
[25] cachem_1.1.0        jsonlite_2.0.0      later_1.4.2         cluster_2.1.6       parallel_4.4.1      broom_1.0.8         R6_2.6.1            stringi_1.8.7      
[33] car_3.1-3           jquerylib_0.1.4     cellranger_1.1.0    iterators_1.0.14    Rcpp_1.0.14         knitr_1.50          clisymbols_1.2.0    zoo_1.8-14         
[41] IRanges_2.43.0      httpuv_1.6.16       Matrix_1.7-0        splines_4.4.1       tidyselect_1.2.1    rstudioapi_0.17.1   abind_1.4-8         codetools_0.2-20   
[49] doParallel_1.0.17   curl_6.4.0          lattice_0.22-6      tibble_3.3.0        plyr_1.8.9          withr_3.0.2         askpass_1.2.1       evaluate_1.0.4     
[57] xml2_1.3.8          survMisc_0.5.6      pillar_1.10.2       BiocManager_1.30.26 carData_3.0-5       rsconnect_1.5.0     stats4_4.4.1        foreach_1.5.2      
[65] renv_1.1.4          generics_0.1.4      S4Vectors_0.47.0    hms_1.1.3           scales_1.4.0        xtable_1.8-4        glue_1.8.0          lazyeval_0.2.2     
[73] tools_4.4.1         ggsignif_0.6.4      colorspace_2.1-1    Formula_1.2-5       cli_3.6.5           km.ci_0.5-6         kableExtra_1.4.0    textshaping_1.0.1  
[81] viridisLite_0.4.2   gtable_0.3.6        sass_0.4.10         digest_0.6.37       BiocGenerics_0.55.0 rjson_0.2.23        htmlwidgets_1.6.4   farver_2.1.2       
[89] htmltools_0.5.8.1   lifecycle_1.0.4     httr_1.4.7          GlobalOptions_0.1.2 mime_0.13           openssl_2.3.3
```



Copyright 2022 Moffitt Cancer Center Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
