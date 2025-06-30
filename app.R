version_id <- paste0("v1.0.20250627")


# File Input ---------------------------------------------------------

Project_Name <- ''

Event_Param_File <- ''
Workbook_file <- ''


# OPTIONAL
Patient_Annotation_File <- ''
Patient_Event_Data_File <- ''
Event_Cluster_Window <- 1
Data_Contains_Longitudinal_Biomarkers <- FALSE

# Does user want password Protection?
Password_Protected <- FALSE
PasswordSet <- ''

# Advanced Setup
TTE_Start_Event_Col <- ''
TTE_Start_Event_Crit <- ''
TTE_End_Event_Col <- ''
TTE_End_Event_Crit <- ''



Example_event_file <- "Example_Data/AACR_Genie_NSCLC_Adenocarcinoma_Example_EventData.txt"
Example_wkbk_file <- "Example_Data/AACR_GENIE_NSCLC_Adenocarcinoma_Example.xlsx"

# Homepage files
homepage_filepath <- "ShinyEvents_Homepage/text_files/"
homepage_tutorial_list <- list.files(homepage_filepath)

# Read text file
homepage_tutorial_text_list <- list()
for (file in homepage_tutorial_list){
  filename <- gsub("\\..*", "", file)
  homepage_tutorial_text_list[[filename]] <- readtext::readtext(paste0(homepage_filepath,file))
}


set.seed(42)


# Load input data if available -------------------------------------------------

wkbk_pre <- NULL
if (file.exists(Event_Param_File) & !file.exists(Workbook_file)) {
  param_pre <- as.data.frame(fread(Event_Param_File,na.strings = c("","NA")))
  wkbk_files <- unique(param_pre[,c(1,2)])
  if (nrow(wkbk_files) > 1) {
    wkbk_pre <- list()
    for (row in seq(nrow(wkbk_files))) {
      df <- as.data.frame(fread(wkbk_files[row,2]))
      tabName <- wkbk_files[row,1]
      wkbk_pre[[tabName]] <- df
    }
  }
}

if (file.exists(Patient_Event_Data_File) & file.exists(Event_Param_File) & (file.exists(Workbook_file) | isTruthy(wkbk_pre))) {
  AllFilesReady <- TRUE
} else {
  AllFilesReady <- FALSE
}

if (exists("Project_Name")) {
  if (!isTruthy(Project_Name)) {
    Project_Name <- "ShinyEvents"
  }
} else {

  Project_Name <- "ShinyEvents"
}

#Drug_anno <- as.data.frame(fread(Drug_anno_file))

if (exists("Event_Cluster_Window")) {
  if (!isTruthy(Event_Cluster_Window)) {
    Event_Cluster_Window <- 1
  }
} else {
  Event_Cluster_Window <- 1
}

pacakges <- installed.packages()
if ("InteractiveComplexHeatmap" %in% pacakges) {
  library("InteractiveComplexHeatmap")
  heat_hover_avail <- TRUE
} else {
  heat_hover_avail <- FALSE
}

if (!isTruthy(TTE_Start_Event_Col)) {
  TTE_Start_Event_Col <- NULL
}
if (!isTruthy(TTE_Start_Event_Crit)) {
  TTE_Start_Event_Crit <- NULL
}
if (!isTruthy(TTE_End_Event_Col)) {
  TTE_End_Event_Col <- NULL
}
if (!isTruthy(TTE_End_Event_Crit)) {
  TTE_End_Event_Crit <- NULL
}

if (!exists("Data_Contains_Longitudinal_Biomarkers")) {
  Data_Contains_Longitudinal_Biomarkers <- FALSE
}

if (!Data_Contains_Longitudinal_Biomarkers) {
  newTabName <- "Patient Summary"
} else {
  newTabName <- "Change-Point Analysis"
}

#increase file upload size
options(shiny.maxRequestSize=5000*1024^2)


# Password Table ---------------------------------------------------------------
# user database for logins
if (Password_Protected) {
  user_base <- tibble::tibble(
    user = "user",
    password = PasswordSet,
    permissions = "admin",
    name = "User"
  )
}


# Login Tab -------------------------------------------------------------------
login_tab <- tabPanel(
  title = icon("lock"),
  value = "login",
  loginUI("login")
)

# Homepage Tab ----------------------------------------------------------------
Homepage_tab <- Homepage_UI("ShinyEvents1")

# Data Input Tab --------------------------------------------------------------
if (AllFilesReady) {
  proc_side_tab_sel <- 2
} else {
  proc_side_tab_sel <- 1
}
DataInput_tab <- bslib::nav_panel("Pre-Processing",
                                  value = "data_input_tab",
                                  p(
                                    bslib::page_fillable(
                                      tags$head(
                                        tags$style(HTML("
                                                        .nav-tabs {
                                                        overflow-x: auto;
                                                        overflow-y: hidden;
                                                        white-space: nowrap;
                                                        flex-wrap: nowrap !important;
                                                        display: flex;
                                                        }
                                                        "))
                                      ),
                                      tags$head(
                                        tags$style(HTML("
                                                        .selectize-input {
                                                        max-height: 82px;
                                                        overflow-y: auto;
                                                        }
                                                        #EventDataTreatmentEvents .vscomp-value {
                                                        max-height: 122px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        #EventDataResponseEvents .vscomp-value {
                                                        max-height: 122px !important;
                                                        overflow-y: auto !important;
                                                        }
                                                        .selectize-dropdown {
                                                        width: 500px !important;
                                                        }
                                                        "))
                                      ),
                                      shiny::sidebarLayout(
                                        sidebarPanel(
                                          width = 3,
                                          id = "DataInputPanel",
                                          tabsetPanel(
                                            id = "dataInput_SideTab",
                                            tabPanel("Data Input",
                                                     p(),
                                                     textInput("UserProjectName","Project Name:", value = ifelse(isTruthy(Project_Name),Project_Name,"Event Analysis")),
                                                     shinyWidgets::radioGroupButtons(
                                                       inputId = "DataInputEventFormat",
                                                       label = tooltip(
                                                         trigger = list(
                                                           "Do you have processed event data?",
                                                           bsicons::bs_icon("info-circle")
                                                           ),
                                                         "Event data must include columns defining: Patient ID, Event Name, Event Category/Type, Event Start Time and Event End Time."
                                                       ),
                                                       choices = c("Yes",
                                                                   "No"),
                                                       justified = TRUE
                                                     ),
                                                     conditionalPanel(condition = "input.DataInputEventFormat == 'Yes'",
                                                                      fileInput("EventDataFileInput","Event Data Upload", accept = c(".xlsx",".xls",".txt",".csv",".tsv")),
                                                                      conditionalPanel(condition = "output.EventDataFileIn_found",
                                                                                       hr(),
                                                                                       shinyWidgets::radioGroupButtons(
                                                                                         inputId = "SuppDataInput1",
                                                                                         label = tooltip(
                                                                                           trigger = list(
                                                                                             "Would you like to upload supplementary data?",
                                                                                             bsicons::bs_icon("info-circle")
                                                                                           ),
                                                                                           "Additional data tables with further details aligning with the event data. Can upload multiple files or an excel file with multiple sheets."
                                                                                         ),
                                                                                         choices = c("Yes",
                                                                                                     "No"),
                                                                                         selected = "No",
                                                                                         justified = TRUE
                                                                                       ),
                                                                                       conditionalPanel(condition = "input.SuppDataInput1 == 'Yes'",
                                                                                                        fileInput("SuppDataFileInput1","Supplementary Data Upload", multiple = T, accept = c(".xlsx",".xls",".txt",".csv",".tsv")),
                                                                                                        conditionalPanel(condition = "output.SuppDataFileInput1_found",
                                                                                                                         fluidRow(
                                                                                                                           column(7,
                                                                                                                                   selectizeInput("SuppEventColumnLink","Select Event Data Column to Link Supplementary Data Table Names:",
                                                                                                                                                  choices = NULL, selected = 1,
                                                                                                                                                  options = list(
                                                                                                                                                    placeholder = 'Please select an option below',
                                                                                                                                                    onInitialize = I('function() { this.setValue(""); }')
                                                                                                                                                  ))

                                                                                                                                  ),
                                                                                                                           column(5,
                                                                                                                                  radioButtons("InputBiomarkerData","Does input data contain biomarker information?",
                                                                                                                                               choices = c("Yes","No"), selected = "No", inline = T)
                                                                                                                                  )
                                                                                                                         )
                                                                                                                         )
                                                                                                        #actionButton("UseExpSuppData","Load Example Supplementary Data", width = "100%")
                                                                                                        )
                                                                      )
                                                                      ),
                                                     conditionalPanel(condition = "input.DataInputFormat1 == 'No'",
                                                                      # upload supplementary data
                                                                      # upload or create parameter file
                                                                      ),
                                                     #fileInput("dataFileInput","Data Upload", multiple = T, accept = c(".xlsx",".xls",".txt",".csv",".tsv")),
                                                     #fluidRow(
                                                     #  column(12, style = 'margin-top:-15px;',
                                                     #         actionButton("UseExpWkbk","Load Example Data")
                                                     #  )
                                                     #),
                                                     p(),
                                                     conditionalPanel(condition = "output.InputDataReady",
                                                                      hr(),
                                                                      actionButton("ProcessInputData","Click Here Finalize and Process Input Data!", icon = icon("rotate-right"), width = "100%",
                                                                                   style = "background-color: #18bc9c; border-color: #2c3e50")
                                                                      ),
                                                     actionButton("LoadExampleData","Load Example Data"),
                                                     #uiOutput("rendTabsetSidePan"),
                                                     value = 1
                                            ),
                                            tabPanel("Data Adjustment",
                                                     p(),
                                                     h4("Subset Patients"),
                                                     selectizeInput("TableToFilterMain", label = "Select Data Table to Subset:", choices = NULL,
                                                                    multiple = F, selected = 1, width = "100%",
                                                                    options = list(
                                                                      placeholder = 'Please select an option below',
                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                    )),
                                                     conditionalPanel(condition = "input.TableToFilterMain != ''",
                                                                      h4("Filter Data Table"),
                                                                      wellPanel(id = "tPanelMain",style = "overflow-y:scroll; max-height: 300px",
                                                                                uiOutput("rendTableFilterMainInput")
                                                                      ),
                                                                      actionButton("ApplyDataFilter","Apply Filters",width = "100%")
                                                     ),
                                                     hr(),
                                                     h4("Adjust Summary Cluster Window"),
                                                     numericInput("MainClusterWindowSet","Event Cluster Window (Months)", value = 1, min = 0, step = 0.5),
                                                     actionButton("UpdateSummaryClusters","Update Event Clusters",width = "100%"),
                                                     hr(),
                                                     h4("Set Unit of Time"),
                                                     selectInput("GlobalAppTimeUnit","Set Applications Interval Time Unit",
                                                                 choices = c("Days","Months","Years","Hours"), selected = "Years"),
                                                     #actionButton("UpdateGlobalAppTimeUnit","Update Event Clusters",width = "100%"),
                                                     value = 2
                                            ),
                                            selected = proc_side_tab_sel
                                          ),
                                        ),
                                        mainPanel(
                                          uiOutput("rendPreProcessingTabs")
                                          #tabsetPanel(id = "PreProcessingTabs",
                                          #            # Format input data
                                          #            tabPanel("Input Data Formatting",
                                          #                     # Event data input
                                          #                     conditionalPanel(condition = "input.DataInputEventFormat == 'Yes'",
                                          #                                      conditionalPanel(condition = "output.EventDataFileIn_found",
                                          #                                                       wellPanel(
                                          #                                                         fluidRow(
                                          #                                                           column(4,
                                          #                                                                  h3("Select Required Event Data Columns:")#,
                                          #                                                           ),
                                          #                                                           column(3,
                                          #                                                                  selectizeInput("EventDataPatientIDcol","Patient ID Column", choices = NULL, selected = 1),
                                          #                                                                  selectizeInput("EventDataEventcol","Event Name Column", choices = NULL, selected = 1)#,
                                          #                                                           ),
                                          #                                                           column(3,
                                          #                                                                  selectizeInput("EventDataEventStartcol","Event Start Time Column", choices = NULL, selected = 1),
                                          #                                                                  selectizeInput("EventDataEventEndcol","Event End Time Column", choices = NULL, selected = 1)
                                          #                                                           ),
                                          #                                                           column(2,
                                          #                                                                  selectizeInput("EventDataEventStartUnits","Start Time Units", choices = c("Days","Months","Years","Hours")),
                                          #                                                                  selectizeInput("EventDataEventEndUnits","End Time Units", choices = c("Days","Months","Years","Hours"))
                                          #                                                           )
                                          #                                                         ),
                                          #                                                         hr(),
                                          #                                                         h4("Optional Event Data Columns:"),
                                          #                                                         fluidRow(
                                          #                                                           column(4,
                                          #                                                                  selectizeInput("EventDataEventTypecol","Event Category/Type Column", choices = NULL, selected = 1,
                                          #                                                                                 options = list(
                                          #                                                                                   placeholder = 'Please select',
                                          #                                                                                   onInitialize = I('function() { this.setValue(""); }')
                                          #                                                                                 )),
                                          #                                                                  selectizeInput("EventDataEventSummary","Event Summary Column", choices = NULL, selected = 1,
                                          #                                                                                 options = list(
                                          #                                                                                   placeholder = 'Please select',
                                          #                                                                                   onInitialize = I('function() { this.setValue(""); }')
                                          #                                                                                 ))
                                          #                                                           ),
                                          #                                                           column(4,
                                          #                                                                  virtualSelectInput(
                                          #                                                                    inputId = "EventDataTreatmentEvents",
                                          #                                                                    label = "Select Treatment Defining Events:",
                                          #                                                                    choices = NULL,
                                          #                                                                    showValueAsTags = TRUE,
                                          #                                                                    search = TRUE,
                                          #                                                                    multiple = TRUE
                                          #                                                                  )
                                          #                                                           ),
                                          #                                                           column(4,
                                          #                                                                  virtualSelectInput(
                                          #                                                                    inputId = "EventDataResponseEvents",
                                          #                                                                    label = "Select Response Defining Events:",
                                          #                                                                    choices = NULL,
                                          #                                                                    showValueAsTags = TRUE,
                                          #                                                                    search = TRUE,
                                          #                                                                    multiple = TRUE
                                          #                                                                  )
                                          #                                                           )
                                          #                                                         )
                                          #                                                       ),
                                          #                                                       p(),
                                          #                                                       h3("Event Data Preivew"),
                                          #                                                       div(DT::dataTableOutput("EventDataInputPreview"), style = "font-size:12px")
                                          #                                      )
                                          #                     ),
                                          #                     # Input or generate parameter file
                                          #                     conditionalPanel(condition = "input.DataInputEventFormat == 'No'",
                                          #                                      ),
                                          #                     value = "Input Data Formatting"
                                          #                     ))#,
                                          #uiOutput("rendWorkbookTabs")
                                        )
                                      ),
                                      tagList(
                                        tags$head(
                                          tags$style(
                                            HTML("
                                     .info_box {
                                     width: auto;
                                     height: auto;
                                     color: #000000;
                                     background-color: #f5f5f5;
                                     padding: 3px 8px;
                                     font-size: 12px;
                                     z-index : 9999;
                                     }",
                                     glue::glue("#{'AppVersion'} {{
                                                position: {'fixed'};
                                                top: 0;
                                                right: 0;
                                                }}")
                                            )
                                          )
                                        ),
                                     div(id = "AppVersion", class = "info_box", version_id)
                                      )
                                    )
                                  )

)

# Patient Tab ------------------------------------------------------------------
PatientLevel_tab <- bslib::nav_panel("Patient Visual Analytics",
                                     value = "patient_visual_analytics",
                                     p(
                                       bslib::page_fillable(
                                         sidebarLayout(
                                           sidebarPanel(
                                             width = 3,
                                             tabsetPanel(
                                               id = "PatientTimeline",
                                               tabPanel("Data Input",
                                                        p(),
                                                        #tags$head(
                                                        #  tags$style(HTML('.selectize-input {
                                                        #        max-height: 82px;
                                                        #        overflow-y: auto;}'
                                                        #  )
                                                        #  )
                                                        #),
                                                        conditionalPanel(condition = "input.PatientMainPanel == '1'",
                                                                         fluidRow(
                                                                           column(9,
                                                                                  selectizeInput("SwimmerYlines","Timeline Row Filter:", choices = NULL,selected = 1, multiple = T)
                                                                           ),
                                                                           column(3, style = "margin-top:25px",
                                                                                  checkboxInput("displaySummaryRows","Display Summary Rows", value = FALSE)
                                                                           )
                                                                         ),
                                                                         div(selectizeInput("SwimmerHover","Hover-text Information:", choices = NULL,selected = 1, multiple = T), style = "margin-top:-15px"),
                                                                         div(selectizeInput("HighlightEvent","Highlight Event:",choices = NULL, selected = 1, multiple = T), style = "margin-top:-15px")
                                                        ),
                                                        conditionalPanel(condition = "input.PatientMainPanel == '2' & output.BiomarkerData",
                                                                         selectInput("LinePlotTable","Data Table:",choices = NULL,
                                                                                     selected = 1),
                                                                         div(selectizeInput("LinePlotSub","Subset Table:",choices = NULL, selected = 1), style = "margin-top:-15px"),
                                                                         conditionalPanel(condition = "input.LinePlotSub != 'Select all data'",
                                                                                          div(selectizeInput("LinePlotSubCrit","Subset criteria:",choices = NULL, selected = 1), style = "margin-top:-15px")
                                                                         ),
                                                                         fluidRow(
                                                                           column(6, #style = "margin-top:-15px",
                                                                                  selectizeInput("LinePlotX","X-Axis", choice = NULL, selected = 1),
                                                                                  selectizeInput("LinePunitCol","Y-Axis Units Column", choice = NULL, selected = 1)
                                                                           ),
                                                                           column(6, #style = "margin-top:-15px",
                                                                                  selectizeInput("LinePlotY","Y-Axis", choice = NULL, selected = 1),
                                                                                  selectizeInput("LinePunitSelect","Y-Axis Units", choice = NULL, selected = 1)
                                                                           )
                                                                         ),
                                                                         fluidRow(
                                                                           column(6,
                                                                                  numericInput("linePlotCutP","User defined cut-point:",
                                                                                               value = NULL)
                                                                           ),
                                                                           column(6,
                                                                                  textInput("linePlotCutPAnno","Cut-Point Annotation:", placeholder = "i.e. Adverse Event Name")
                                                                           )
                                                                         ),
                                                                         fluidRow(
                                                                           column(3, style = "margin-top:-10px",
                                                                                  h4("Save Annotation:")
                                                                           ),
                                                                           column(4, #style = "padding-right:2px;padding-left:2px",
                                                                                  actionButton("saveLinePlotAbvCutP","Above Cut-Point")
                                                                           ),
                                                                           column(4, #style = "padding-left:2px",
                                                                                  actionButton("saveLinePlotBelCutP","Below Cut-Point")
                                                                           )
                                                                         )
                                                        ),
                                                        h4("Patient Selection"),
                                                        div(DT::dataTableOutput("PatientSelectionTab"), style = "font-size:10px"),
                                                        p(),
                                                        fluidRow(
                                                          column(6,
                                                                 checkboxInput("RemoveUnknownNA","Remove Unknown/NA",value = T)
                                                          ),
                                                          column(6,
                                                                 downloadButton("dnldCohortEventTab","Cohort Event Table")
                                                          )
                                                        )
                                               ),
                                               tabPanel("Figure Settings",
                                                        p(),
                                                        conditionalPanel(condition = "input.PatientMainPanel == '1'",
                                                                         uiOutput("rendTimeLineTitle"),
                                                                         uiOutput("rendTimeLineXTitle"),
                                                                         h4("Font Sizes"),
                                                                         fluidRow(
                                                                           column(4,
                                                                                  numericInput("TimeLineTitleSize","Title:",
                                                                                               value = 18, step = 1)
                                                                           ),
                                                                           column(4,
                                                                                  numericInput("TimeLineXAxisSize","X-Axis:",
                                                                                               value = 14, step = 1)
                                                                           ),
                                                                           column(4,
                                                                                  numericInput("TimeLineYAxisSize","Y-Axis:",
                                                                                               value = 12, step = 1)
                                                                           )
                                                                         ),
                                                                         h4("Figure Download Parameters"),
                                                                         fluidRow(
                                                                           column(6,
                                                                                  numericInput("TimeLineHeight","Height (px)",value = 800)
                                                                           ),
                                                                           column(6,
                                                                                  numericInput("TimeLineWidth","Width (px)",value = 1000)
                                                                           )
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "input.PatientMainPanel == '2'",
                                                                         conditionalPanel(condition = "output.BiomarkerData",
                                                                                          selectInput("LinePlotTheme","Select Theme:",
                                                                                                      choices = c("Void" = "theme_void","BW" = "theme_bw","Minimal" = "theme_minimal",
                                                                                                                  "Grey" = "theme_grey","Linedraw" = "theme_linedraw","Light" = "theme_light",
                                                                                                                  "Dark" = "theme_dark","Classic" = "theme_classic","Test" = "theme_test")),
                                                                                          h4("Font Sizes"),
                                                                                          fluidRow(
                                                                                            column(4,
                                                                                                   numericInput("LinePlotTitleSize","Title:",
                                                                                                                value = 20, step = 1)
                                                                                            ),
                                                                                            column(4,
                                                                                                   numericInput("LinePlotXAxisSize","X-Axis:",
                                                                                                                value = 14, step = 1)
                                                                                            ),
                                                                                            column(4,
                                                                                                   numericInput("LinePlotYAxisSize","Y-Axis:",
                                                                                                                value = 14, step = 1)
                                                                                            )
                                                                                          )
                                                                         ),
                                                                         h4("Figure Download Parameters"),
                                                                         fluidRow(
                                                                           column(6,
                                                                                  numericInput("LinePlotHeight","Height (in)",value = 8)
                                                                           ),
                                                                           column(6,
                                                                                  numericInput("LinePlotWidth","Width (in)",value = 10)
                                                                           )
                                                                         )
                                                        )
                                               )
                                             )
                                             #)
                                           ),
                                           mainPanel(
                                             tabsetPanel(
                                               id = "PatientMainPanel",
                                               tabPanel("Patient Timeline",
                                                        p(),
                                                        shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("PatientTimelinePlot",height = "800px", width = "100%")), type = 6),
                                                        fluidRow(
                                                          column(3,
                                                                 downloadButton("dnldPatientEventTab","Patient Event Table")
                                                          )
                                                        ),
                                                        p(),
                                                        uiOutput("rendTimelineTableTabs"),
                                                        #tags$head(
                                                        #  tags$style(HTML('.selectize-dropdown {
                                                        #     width: 500px !important;}'
                                                        #  )
                                                        #  )
                                                        #),
                                                        value = 1
                                               ),
                                               tabPanel(newTabName, #"Change-Point Analysis",
                                                        p(),
                                                        uiOutput("rendSummaryLinePlots"),
                                                        value = 2
                                               )
                                             ),
                                             value = 1
                                           )
                                         ),
                                         tagList(
                                           tags$head(
                                             tags$style(
                                               HTML("
                                     .info_box {
                                     width: auto;
                                     height: auto;
                                     color: #000000;
                                     background-color: #f5f5f5;
                                     padding: 3px 8px;
                                     font-size: 12px;
                                     z-index : 9999;
                                     }",
                                     glue::glue("#{'AppVersion'} {{
                                                position: {'fixed'};
                                                top: 0;
                                                right: 0;
                                                }}")
                                               )
                                             )
                                           ),
                                     div(id = "AppVersion", class = "info_box", version_id)
                                         )
                                       )
                                     )

)

# Treatment Analytics Tab ------------------------------------------------------
TreatmentAnalytics_tab <- bslib::nav_panel("Treatment Associated Analytics",
                                           value = "treatment_associated_analytics",
                                           p(
                                             bslib::page_fillable(
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   width = 3,
                                                   tabsetPanel(id = "treatAnalyticsSide",
                                                               tabPanel("Data Input",
                                                                        p(),
                                                                        conditionalPanel(condition = "input.treatAnalytics == '1'",
                                                                                         selectInput("sankeyEvent","Event Category:",choices = NULL,
                                                                                                     selected = 1),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsCls == '1'",
                                                                                                          div(selectizeInput("sankeyXaxis","X-Axis Clusters:",
                                                                                                                             choices = NULL, selected = 1, multiple = TRUE), style = "margin-top:-15px"),
                                                                                                          checkboxInput("SankeyNAs","View NA's as category", value = F)
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsCls == '2'",
                                                                                                          fluidRow(
                                                                                                            column(4,
                                                                                                                   radioButtons("heatView","View by",choices = c("Clusters","Patients"))
                                                                                                            ),
                                                                                                            column(4, style = "margin-top:10px",
                                                                                                                   checkboxInput("HeatFlip","Flip Axes",value = FALSE)
                                                                                                            ),
                                                                                                            column(4, style = "margin-top:10px",
                                                                                                                   checkboxInput("border_op","Cell Borders",value = TRUE)
                                                                                                            )
                                                                                                          ),
                                                                                                          fluidRow(
                                                                                                            column(4,
                                                                                                                   checkboxGroupInput("HeatClusterRC","Cluster:",choices = c("Rows","Columns"), selected = "Rows")
                                                                                                            ),
                                                                                                            column(8,
                                                                                                                   conditionalPanel(condition = "input.HeatClusterRC.includes('Rows') || input.HeatClusterRC.includes('Columns')",
                                                                                                                                    selectInput("HeatClusterMethod","Cluster Method:",
                                                                                                                                                choices = c("ward.D", "ward.D2", "complete", "single", "average", "mcquitty", "median", "centroid"))
                                                                                                                   )
                                                                                                            )
                                                                                                          ),
                                                                                                          selectizeInput("HeatXchoices","X-Axis Variables:", choices = NULL, selected = NULL, multiple = T),
                                                                                                          div(selectizeInput("HeatYchoices","Y-Axis Variables:", choices = NULL, selected = NULL, multiple = T), style = "margin-top:-15px"),
                                                                                                          fluidRow(
                                                                                                            column(6, style = "margin-top:15px",
                                                                                                                   checkboxInput("HeatEventCapYN","Cap Number of Events",value = T)
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   conditionalPanel(condition = "input.HeatEventCapYN == true",
                                                                                                                                    numericInput("HeatEventCapN","Number of Events Cap:", value = 20, min = 1, step = 1)
                                                                                                                   )
                                                                                                            )
                                                                                                          ),
                                                                                                          uiOutput("HeatHoverInfo")
                                                                                         )
                                                                        ),
                                                                        conditionalPanel(condition = "input.treatAnalytics == '2'",
                                                                                         selectizeInput("durationHeatEventType","Event Type:", choices = NULL, selected = 1),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '1' | input.treatAnalyticsDur == '3'",
                                                                                                          #selectInput("BoxplotDataTable","Data Table:",choices = NULL, selected = 1),
                                                                                                          #uiOutput("rendBoxPlotEventType"),
                                                                                                          selectizeInput("BoxplotXaxis","Event:",choices = NULL, selected = 1)
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '1'",
                                                                                                          fluidRow(
                                                                                                            column(8,
                                                                                                                   radioButtons("EventDurMaxSum","Reduce duplicate patient events by:",
                                                                                                                                choices = c("Sum total duration per patient" = "sum","Max duration per patient" = "max"))
                                                                                                            ),
                                                                                                            column(4,
                                                                                                                   radioButtons("ViolinOrBoxP","View As:",choices = c("Box Plot","Violin Plot"))
                                                                                                            )
                                                                                                          ),
                                                                                                          fluidRow(
                                                                                                            column(4,
                                                                                                                   checkboxInput("BPplotsampledots","Include Dot Annotation", value = T),
                                                                                                            ),
                                                                                                            column(4,
                                                                                                                   numericInput("BPplotDotSize","Dot Size:", value = 1, step = 0.25, width = "80%")
                                                                                                            ),
                                                                                                            column(4,
                                                                                                                   checkboxInput("BPflipBP","Flip Axis", value = T),
                                                                                                            )
                                                                                                          )
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '2'",
                                                                                                          #selectizeInput("durationHeatEventType","Event Type:", choices = NULL, selected = 1),
                                                                                                          fluidRow(
                                                                                                            column(8,
                                                                                                                   radioButtons("EventDurMaxSumHeat","Reduce duplicate patient events by:",
                                                                                                                                choices = c("Sum total duration per patient" = "sum","Max duration per patient" = "max"))
                                                                                                            ),
                                                                                                            column(4,
                                                                                                                   checkboxInput("HeatFlipDist","Flip Axes",value = FALSE),
                                                                                                                   checkboxInput("border_opDist","Cell Borders",value = TRUE)
                                                                                                            )
                                                                                                          ),
                                                                                                          fluidRow(
                                                                                                            column(4,
                                                                                                                   checkboxGroupInput("HeatClusterRCDist","Cluster:",choices = c("Rows","Columns"), selected = c("Rows","Columns"))
                                                                                                            ),
                                                                                                            column(8,
                                                                                                                   conditionalPanel(condition = "input.HeatClusterRCDist.includes('Rows') || input.HeatClusterRCDist.includes('Columns')",
                                                                                                                                    selectInput("HeatClusterMethodDist","Cluster Method:",
                                                                                                                                                choices = c("ward.D", "ward.D2", "complete", "single", "average", "mcquitty", "median", "centroid"))
                                                                                                                   )
                                                                                                            )
                                                                                                          ),
                                                                                                          selectizeInput("HeatXchoicesDist","X-Axis Variables:", choices = NULL, selected = NULL, multiple = T),
                                                                                                          div(selectizeInput("HeatYchoicesDist","Y-Axis Variables:", choices = NULL, selected = NULL, multiple = T), style = "margin-top:-15px"),
                                                                                                          fluidRow(
                                                                                                            column(6, style = "margin-top:15px",
                                                                                                                   checkboxInput("HeatEventCapYNDist","Cap Duration",value = T),
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   conditionalPanel(condition = "input.HeatEventCapYNDist == true",
                                                                                                                                    numericInput("HeatEventCapNDist","Duration Cap (Days):", value = 365, min = 1, step = 1)
                                                                                                                   )
                                                                                                            )
                                                                                                          ),
                                                                                                          uiOutput("HeatHoverInfoDist")
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '3'",
                                                                                                          fluidRow(
                                                                                                            column(6,
                                                                                                                   numericInput("maxpatviewDist","Max Number of Patients", value = 60, min = 1, step = 1)
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   selectInput("swimmerSortDist","Sort Swimmer Plot By:",
                                                                                                                               choices = c("Duration - Descending","Duration - Ascending",
                                                                                                                                           "Overall Time - Descending","Overall Time - Ascending",
                                                                                                                                           "Alphabetical - Ascending","Alphabetical - Descending"))
                                                                                                            )
                                                                                                          ),
                                                                                                          fluidRow(
                                                                                                            column(9,
                                                                                                                   selectizeInput("swimmerPatSpecDist","Specify Patients to Include:",
                                                                                                                                  choices = NULL, selected = 1, multiple = TRUE)
                                                                                                            ),
                                                                                                            column(3, style = "margin-top:25px",
                                                                                                                   checkboxInput("swimmerShowAllDist","Show All",value = FALSE)
                                                                                                            )
                                                                                                          ),
                                                                                                          numericInput("SwimmPlotHeightDist","Plot Height (px)",value = 800, step = 25, min = 0)
                                                                                         )
                                                                        )
                                                               ),
                                                               tabPanel("Figure Settings",
                                                                        p(),
                                                                        conditionalPanel(condition = "input.treatAnalytics == '1'",
                                                                                         conditionalPanel(condition = "input.treatAnalyticsCls == '2'",
                                                                                                          h4("Heatmap Color Scheme"),
                                                                                                          textInput("HeatColorLow","Low:", value = "white"),
                                                                                                          textInput("HeatColorHigh","High:", value = "red"),
                                                                                                          fluidRow(
                                                                                                            column(6,
                                                                                                                   numericInput("heatmapFontCol", "Column Font Size:",
                                                                                                                                min = 5, max = 75,
                                                                                                                                value = 14, step = 1),
                                                                                                                   numericInput("heatmapHeight1","Download Height (in)",value = 10)
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   numericInput("heatmapFontRow", "Row Font Size:",
                                                                                                                                min = 5, max = 75,
                                                                                                                                value = 14, step = 1),
                                                                                                                   numericInput("heatmapWidth1","Download Width (in)",value = 15)
                                                                                                            )
                                                                                                          )
                                                                                         )
                                                                        ),
                                                                        conditionalPanel(condition = "input.treatAnalytics == '2'",
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '1'",
                                                                                                          selectInput("BPTheme","Theme:",
                                                                                                                      choices = c("Minimal" = "theme_minimal","Grey" = "theme_grey","BW" = "theme_bw",
                                                                                                                                  "Linedraw" = "theme_linedraw","Light" = "theme_light","Dark" = "theme_dark",
                                                                                                                                  "Classic" = "theme_classic","Void" = "theme_void","Test" = "theme_test")),
                                                                                                          h4("Font Sizes"),
                                                                                                          fluidRow(
                                                                                                            column(4,
                                                                                                                   numericInput("BPplot1TitleSize","Title:",
                                                                                                                                value = 20, step = 1)
                                                                                                            ),
                                                                                                            column(4,
                                                                                                                   numericInput("BPplot1XAxisSize","X-Axis:",
                                                                                                                                value = 12, step = 1)
                                                                                                            ),
                                                                                                            column(4,
                                                                                                                   numericInput("BPplot1YAxisSize","Y-Axis:",
                                                                                                                                value = 14, step = 1)
                                                                                                            )
                                                                                                          ),
                                                                                                          h4("Figure Download Parameters"),
                                                                                                          fluidRow(
                                                                                                            column(6,
                                                                                                                   numericInput("BPHeight","Height (px)",value = 800)
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   numericInput("BPWidth","Width (px)",value = 1000)
                                                                                                            )
                                                                                                          )
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '2'",
                                                                                                          h4("Heatmap Color Scheme"),
                                                                                                          textInput("HeatColorLowDist","Low:", value = "white"),
                                                                                                          textInput("HeatColorHighDist","High:", value = "red"),
                                                                                                          fluidRow(
                                                                                                            column(6,
                                                                                                                   numericInput("heatmapDistFontCol", "Column Font Size:",
                                                                                                                                min = 5, max = 75,
                                                                                                                                value = 14, step = 1),
                                                                                                                   numericInput("heatmapDistHeight1","Download Height (in)",value = 10)
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   numericInput("heatmapDistFontRow", "Row Font Size:",
                                                                                                                                min = 5, max = 75,
                                                                                                                                value = 14, step = 1),
                                                                                                                   numericInput("heatmapDistWidth1","Download Width (in)",value = 15)
                                                                                                            )
                                                                                                          )
                                                                                         ),
                                                                                         conditionalPanel(condition = "input.treatAnalyticsDur == '3'",
                                                                                                          h4("Figure Download Parameters"),
                                                                                                          fluidRow(
                                                                                                            column(6,
                                                                                                                   numericInput("DistSwimmerHeight","Height (px)",value = 800)
                                                                                                            ),
                                                                                                            column(6,
                                                                                                                   numericInput("DistSwimmerWidth","Width (px)",value = 1000)
                                                                                                            )
                                                                                                          )
                                                                                         )
                                                                        )
                                                               )
                                                   )

                                                 ),
                                                 mainPanel(
                                                   tabsetPanel(id = "treatAnalytics",
                                                               tabPanel("Treatment Clustering",
                                                                        p(),
                                                                        tabsetPanel(id = "treatAnalyticsCls",
                                                                                    tabPanel("Sankey",
                                                                                             p(),
                                                                                             shinyjqui::jqui_resizable(plotOutput("SankeyPlot",height = "700px", width = "100%")),
                                                                                             p(),
                                                                                             fluidRow(
                                                                                               column(1,
                                                                                                      downloadButton("dnldSankeyPlot","SVG")
                                                                                               ),
                                                                                               column(2,
                                                                                                      downloadButton("dnldSankeyTable","Sankey Plot Data")
                                                                                               ),
                                                                                               column(3,
                                                                                                      downloadButton("dnldSankeyTableFiltered","Sankey Plot Data Filtered")
                                                                                               )
                                                                                             ),
                                                                                             p(),
                                                                                             div(DT::dataTableOutput("SankeyPlotTable"), style = "font-size:14px"),
                                                                                             #fluidRow(
                                                                                             #  column(3,
                                                                                             #         downloadButton("dnldSankeyPlotTable", "Download Table")
                                                                                             #  )
                                                                                             #),
                                                                                             value = 1
                                                                                    ),
                                                                                    tabPanel("Heatmap",
                                                                                             p(),
                                                                                             #htmlOutput("HeatmapErrorText"),
                                                                                             shinyjqui::jqui_resizable(plotOutput("TreatClusterHeatmap",height = "800px", width = "100%", hover = "heatmap_hover")),
                                                                                             p(),
                                                                                             fluidRow(column(3,
                                                                                                             downloadButton("dnldTreatClusterHeatmap","SVG")
                                                                                             )
                                                                                             ),
                                                                                             value = 2
                                                                                    )
                                                                        ),
                                                                        value = 1
                                                               ),
                                                               tabPanel("Event Duration",
                                                                        p(),
                                                                        tabsetPanel(id = "treatAnalyticsDur",
                                                                                    tabPanel("Swimmers Plot",
                                                                                             p(),
                                                                                             uiOutput("rendDurationSwimmers"),
                                                                                             value = 3
                                                                                    ),
                                                                                    tabPanel("Box Plot",
                                                                                             p(),
                                                                                             shinyjqui::jqui_resizable(plotlyOutput("EventBoxPlot",height = "600px", width = "100%")),
                                                                                             div(DT::dataTableOutput("EventBoxPlotTable"), style = "font-size:14px"),
                                                                                             fluidRow(
                                                                                               column(3,
                                                                                                      downloadButton("dnldEventBoxPlotTable", "Download Table")
                                                                                               )
                                                                                             ),
                                                                                             value = 1
                                                                                    ),
                                                                                    tabPanel("Heatmap",
                                                                                             p(),
                                                                                             #htmlOutput("HeatmapErrorText2"),
                                                                                             shinyjqui::jqui_resizable(plotOutput("TreatDistHeatmap",height = "800px", width = "100%", hover = "heatmapDist_hover")),
                                                                                             p(),
                                                                                             fluidRow(
                                                                                               column(3,
                                                                                                      downloadButton("dnldTreatDistHeatmap", "SVG")
                                                                                               )
                                                                                             ),
                                                                                             value = 2
                                                                                    )
                                                                        ),
                                                                        value = 2
                                                               )
                                                   )
                                                 )
                                               ),
                                               tagList(
                                                 tags$head(
                                                   tags$style(
                                                     HTML("
                                     .info_box {
                                     width: auto;
                                     height: auto;
                                     color: #000000;
                                     background-color: #f5f5f5;
                                     padding: 3px 8px;
                                     font-size: 12px;
                                     z-index : 9999;
                                     }",
                                     glue::glue("#{'AppVersion'} {{
                                                position: {'fixed'};
                                                top: 0;
                                                right: 0;
                                                }}")
                                                     )
                                                   )
                                                 ),
                                     div(id = "AppVersion", class = "info_box", version_id)
                                               )
                                             )
                                           )

)


# Time-to-event Tab ------------------------------------------------------

tte_tab <- bslib::nav_panel("Time-To-Event Analysis",
                            value = "time_to_event_analysis",
                            p(
                              bslib::page_fillable(
                                sidebarLayout(
                                  sidebarPanel(
                                    width = 3,
                                    tabsetPanel(id = "ttetabsside",
                                                tabPanel("Data Input",
                                                         p(),
                                                         conditionalPanel(condition = "input.ttetabs == '1' | input.ttetabs == '2'",
                                                                          conditionalPanel(condition = "input.ttetabs == '1'",
                                                                                           fluidRow(
                                                                                             column(6,
                                                                                                    numericInput("maxpatview","Max Number of Patients", value = 60, min = 1, step = 1)
                                                                                             ),
                                                                                             column(6,
                                                                                                    selectInput("swimmerSort","Sort Swimmer Plot By:",
                                                                                                                choices = c("Time - Descending","Time - Ascending",
                                                                                                                            "Alphabetical - Ascending","Alphabetical - Descending"))
                                                                                             )
                                                                                           ),
                                                                                           fluidRow(
                                                                                             column(9,
                                                                                                    selectizeInput("swimmerPatSpec","Specify Patients to Include:",
                                                                                                                   choices = NULL, selected = 1, multiple = TRUE)
                                                                                             ),
                                                                                             column(3, style = "margin-top:25px",
                                                                                                    checkboxInput("swimmerShowAll","Show All",value = FALSE)
                                                                                             )
                                                                                           ),
                                                                                           div(h4("Data Selection"), style = "margin-top:-15px"),
                                                                          ),
                                                                          conditionalPanel(condition = "input.ttetabs == '2'",
                                                                                           h4("Data Selection"),
                                                                                           selectInput("tteStartCol2","Start Point:",
                                                                                                       choices = NULL,
                                                                                                       selected = 1),
                                                                                           conditionalPanel(condition = "output.ShowStartCrit",
                                                                                                            fluidRow(
                                                                                                              column(8,
                                                                                                                     selectInput("StartCrit2","Specified Start Point:",choices = NULL, multiple = T, selected = 1),
                                                                                                                     div(radioButtons("StartAndOr2","",choices = c("'Or' Statement","'And' Statement"), inline = T),
                                                                                                                         style = "margin-top:-30px")
                                                                                                              ),
                                                                                                              column(4, style = "margin-top:25px",
                                                                                                                     checkboxInput("SelectAllStartCrit2","Select All",value = F)
                                                                                                              )
                                                                                                            )
                                                                                           ),
                                                                                           selectInput("tteStopCol2","End Point - Progression Event:",
                                                                                                       choices = NULL,
                                                                                                       selected = 1, multiple = T),
                                                                                           conditionalPanel(condition = "output.ShowStopCrit",
                                                                                                            fluidRow(
                                                                                                              column(8,
                                                                                                                     selectInput("StopCrit2","Specified Event:",choices = NULL, multiple = T, selected = 1),
                                                                                                                     div(radioButtons("StopAndOr2","",choices = c("'Or' Statement","'And' Statement"), inline = T),
                                                                                                                         style = "margin-top:-30px")
                                                                                                              ),
                                                                                                              column(4, style = "margin-top:25px",
                                                                                                                     checkboxInput("SelectAllStopCrit2","Select All",value = F)
                                                                                                              )
                                                                                                            )
                                                                                           ),
                                                                                           conditionalPanel(condition = "input.ttetabs == '2'",
                                                                                                            div(h4("Strata Selection"), style = "margin-top:-15px"),
                                                                                                            selectizeInput("KPstrataDataTable","Data Table:",choices = NULL,
                                                                                                                           options = list(
                                                                                                                             placeholder = 'Please select an option below',
                                                                                                                             onInitialize = I('function() { this.setValue(""); }')
                                                                                                                           )),
                                                                                                            conditionalPanel(condition = "input.KPstrataDataTable > '0' && input.KPstrataDataTable != 'No Strata'",
                                                                                                                             div(selectizeInput("KPstrataCol","Strata Column:",choices = NULL, selected = 1), style = "margin-top:-15px"),
                                                                                                                             fluidRow(
                                                                                                                               column(8, style = "margin-top:-15px",
                                                                                                                                      selectizeInput("KPstrataColGroups","X-Axis Groups:",choices = NULL,
                                                                                                                                                     selected = 1, multiple = T),
                                                                                                                                      conditionalPanel(condition = "input.KPshowOtherGroup == true",
                                                                                                                                                       div(textInput("KPotherGroupName","Unselected Variables Group Name:"), style = "margin-top:-15px")
                                                                                                                                      )
                                                                                                                               ),
                                                                                                                               column(4,
                                                                                                                                      checkboxInput("KPshowOtherGroup","Group Unselected Variables", value = T)
                                                                                                                               )
                                                                                                                             )
                                                                                                            )
                                                                                           ),
                                                                                           uiOutput("rendTTEexplHeader"),
                                                                                           uiOutput("rendTTEexpl")
                                                                                           )
                                                         ),
                                                         value = 1
                                                ),
                                                tabPanel("Figure Settings",
                                                         p(),
                                                         conditionalPanel(condition = "input.ttetabs == '1' | input.ttetabs == '2'",
                                                                          p(),
                                                                          h4("Survival Plot Parameters"),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   numericInput("SurvXaxis","X-Axis Limit (years)", value = NA),
                                                                                   numericInput("SurvXaxisBreaks","X-Axis Breaks (Years):",value = 1, min = 0, step = 0.25),
                                                                                   selectInput("SurvLegendPos","Legend Position",choices = c("right","left","top","bottom","none"))
                                                                            ),
                                                                            column(6, style = "margin-top:15px",
                                                                                   radioButtons("SurvYearOrMonth","Survival X-Axis Units:",choices = c("Days","Years","Months"), inline = T, selected = "Years"),
                                                                                   checkboxInput("ShowPval","Show P.Value",value = T),
                                                                                   checkboxInput("ShowConfInt","Show Confidence Interval",value = F),
                                                                                   checkboxInput("ShowMedSurvLine","Show Median Survival Line",value = F)
                                                                            )
                                                                          ),
                                                                          h4("Kaplan Meier Plot Download Parameters"),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   numericInput("PlotDnldHight","Plot Height",value = 8, min = 0, step = 1)
                                                                            ),
                                                                            column(4,
                                                                                   numericInput("PlotDnldWidth","Plot Width",value = 8, min = 0, step = 1)
                                                                            ),
                                                                            column(4,
                                                                                   selectInput("PlotDnldUnits","Units",choices = c("in","cm","mm","px"))
                                                                            )
                                                                          ),
                                                                          h4("Swimmers Plot Download Parameters"),
                                                                          fluidRow(
                                                                            column(6,
                                                                                   numericInput("TTESwimmerHeight","Height (px)",value = 800)
                                                                            ),
                                                                            column(6,
                                                                                   numericInput("TTESwimmerWidth","Width (px)",value = 1000)
                                                                            )
                                                                          )
                                                         ),
                                                         conditionalPanel(condition = "input.ttetabs == '3'",
                                                                          p(),
                                                                          h4("Plot Download Parameters"),
                                                                          fluidRow(
                                                                            column(4,
                                                                                   numericInput("PowPlotDnldHight","Plot Height",value = 8, min = 0, step = 1)
                                                                            ),
                                                                            column(4,
                                                                                   numericInput("PowPlotDnldWidth","Plot Width",value = 8, min = 0, step = 1)
                                                                            ),
                                                                            column(4,
                                                                                   selectInput("PowPlotDnldUnits","Units",choices = c("in","cm","mm","px"))
                                                                            )
                                                                          )
                                                         ),
                                                         value = 2
                                                )
                                    )
                                  ),
                                  mainPanel(
                                    p(),
                                    span(textOutput("NoPatientsMatchError"), style="color:red"),
                                    tabsetPanel(id = "ttetabs",
                                                tabPanel("Kaplan Meier",
                                                         p(),
                                                         fluidRow(
                                                           column(8,
                                                                  shinyjqui::jqui_resizable(plotOutput("KPplot",height = "500",width = "100%"))
                                                           ),
                                                           column(4,
                                                                  shinycssloaders::withSpinner(tableOutput("KPplotHRtab"), type = 7, size = 0.5),
                                                                  verbatimTextOutput("KPplotSummary"))
                                                         ),
                                                         fluidRow(
                                                           column(3,
                                                                  downloadButton("dnldKPplot", "SVG")
                                                           )
                                                         ),
                                                         p(),
                                                         div(DT::dataTableOutput("KPplotTable"), style = "font-size:14px"),
                                                         fluidRow(
                                                           column(3,
                                                                  downloadButton("dnldKPplotTable", "Download Table")
                                                           )
                                                         ),
                                                         value = 2
                                                ),
                                                tabPanel("Swimmers Plot",
                                                         p(),
                                                         shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("swimmer_plot",height = "600px", width = "100%")), type = 6),
                                                         p(),
                                                         div(DT::dataTableOutput("swimmer_plotTable"), style = "font-size:14px"),
                                                         fluidRow(
                                                           column(3,
                                                                  downloadButton("dnldswimmer_plotTable", "Download Table")
                                                           )
                                                         ),
                                                         value = 1
                                                )
                                    )
                                  )
                                ),
                                tagList(
                                  tags$head(
                                    tags$style(
                                      HTML("
                                     .info_box {
                                     width: auto;
                                     height: auto;
                                     color: #000000;
                                     background-color: #f5f5f5;
                                     padding: 3px 8px;
                                     font-size: 12px;
                                     z-index : 9999;
                                     }",
                                     glue::glue("#{'AppVersion'} {{
                                                position: {'fixed'};
                                                top: 0;
                                                right: 0;
                                                }}")
                                      )
                                    )
                                  ),
                                  div(id = "AppVersion", class = "info_box", version_id)
                                )
                              )
                            )

)


# Cohort Level Tab -------------------------------------------------------------
CohortLevel_tab <- bslib::nav_panel("Cohort Overview",
                                    value = "cohort_overview",
                                    p(
                                      bslib::page_fillable(
                                        sidebarLayout(
                                          sidebarPanel(
                                            width = 3,
                                            conditionalPanel(condition = "input.SummaryMain == '1' | input.SummaryMain == '4'",
                                                             selectizeInput("SummaryOptions","Select Event Summaries to Include:", choices = NULL, selected = 1, multiple = TRUE)
                                            ),
                                            conditionalPanel(condition = "input.SummaryMain == '4'",
                                                             fluidRow(
                                                               column(6,
                                                                      numericInput("maxpatviewSumm","Max Number of Patients", value = 60, min = 1, step = 1)
                                                               ),
                                                               column(6,
                                                                      selectInput("swimmerSortSumm","Sort Swimmer Plot By:",
                                                                                  choices = c("Time - Descending","Time - Ascending",
                                                                                              "Alphabetical - Ascending","Alphabetical - Descending"))
                                                               )
                                                             ),
                                                             fluidRow(
                                                               column(9,
                                                                      selectizeInput("swimmerPatSpecSumm","Specify Patients to Include:",
                                                                                     choices = NULL, selected = 1, multiple = TRUE)
                                                               ),
                                                               column(3, style = "margin-top:25px",
                                                                      checkboxInput("swimmerShowAllSumm","Show All",value = FALSE)
                                                               )
                                                             ),
                                                             numericInput("SwimmPlotHeight","Plot Height (px)",value = 800, step = 25, min = 0),
                                                             h4("Download Parameters"),
                                                             fluidRow(
                                                               column(6,
                                                                      numericInput("SummSwimmerHeight","Height (px)",value = 800)
                                                               ),
                                                               column(6,
                                                                      numericInput("SummSwimmerWidth","Width (px)",value = 1000)
                                                               )
                                                             )
                                            ),
                                            conditionalPanel(condition = "input.SummaryMain == '2'",
                                                             p(),
                                                             h4("Reference Event"),
                                                             selectizeInput("RefDataTable","Reference Data Table:", choices = NULL, selected = NULL),
                                                             selectizeInput("RefEvent","Reference Event(s):", choices = NULL, selected = NULL, multiple = T),
                                                             hr(),
                                                             h4("Event of Interest"),
                                                             selectizeInput("EOIDataTable","Event of Interest Data Table:", choices = NULL, selected = NULL),
                                                             selectizeInput("EOIEvent","Event(s) of Interest:", choices = NULL, selected = NULL, multiple = T),
                                                             div(radioButtons("EOIEventAndOr","",choices = c("'Or' Statement","'And' Statement"), inline = T),
                                                                 style = "margin-top:-30px"),
                                                             hr(),
                                                             textInput("NewColName","New Column Name", placeholder = "ReferenceEvent_RelativeTo_NewEvent"),
                                                             actionButton("SaveAnnotation","Save New Annotation Columns", width = "100%")
                                            ),
                                            conditionalPanel(condition = "input.SummaryMain == '3'",
                                                             p(),
                                                             selectizeInput("TableToFilter","Data Table:", choices = NULL, selected = 1),
                                                             h4("Filter Data Table"),
                                                             wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 300px",
                                                                       uiOutput("rendTableFilterInput")
                                                             ),
                                                             div(selectizeInput("FilterTableIDcol","Sample/Patient ID Column:", choices = NULL, selected = 1), style="margin-top:15px"),
                                                             selectizeInput("FilterTabRefFeature","Reference Feature:", choices = NULL, selected = 1),
                                                             selectizeInput("FilterTabCountFeatures","Features of Interest:", choices = NULL, selected = 1, multiple = TRUE),
                                                             conditionalPanel(condition = "input.FilterTabRefFeature != 'No Reference' && input.FilterTabRefFeature != '' && input.FilterTabCountFeatures.length > 0",
                                                                              fluidRow(
                                                                                column(8,
                                                                                       selectizeInput("OddsRatioColumn","Select Reference for Odds Ratio:", choices = NULL, selected = 1)
                                                                                ),
                                                                                column(4,
                                                                                       checkboxInput("ORtest","Test for Over-Representation (Enrichment)", value = FALSE)
                                                                                )
                                                                              )
                                                             )
                                            )
                                          ),
                                          mainPanel(
                                            tabsetPanel(id = "SummaryMain",
                                                        tabPanel("Event Summary",
                                                                 p(),
                                                                 div(DT::dataTableOutput("CohortEventSummaryTable"), style = "font-size:14px"),
                                                                 fluidRow(
                                                                   column(3,
                                                                          downloadButton("dlndCohortEventSummaryTable", "Download Table"))
                                                                 ),
                                                                 value = 1
                                                        ),
                                                        tabPanel("Swimmers Plot",
                                                                 p(),
                                                                 uiOutput("rendsumm_swimmer_plot"),
                                                                 value = 4
                                                        ),
                                                        tabPanel("Event Annotation",
                                                                 p(),
                                                                 div(DT::dataTableOutput("MolecularAnnoTable"), style = "font-size:14px"),
                                                                 fluidRow(
                                                                   column(3,
                                                                          downloadButton("dlndMolecularAnnoTable", "Download Table")
                                                                   )
                                                                 ),
                                                                 value = 2
                                                        ),
                                                        tabPanel("Cohort Break-Down",
                                                                 p(),
                                                                 uiOutput("rendMolecularBreakdownTabs"),
                                                                 value = 3
                                                        )
                                            )
                                          )
                                        ),
                                        tagList(
                                          tags$head(
                                            tags$style(
                                              HTML("
                                     .info_box {
                                     width: auto;
                                     height: auto;
                                     color: #000000;
                                     background-color: #f5f5f5;
                                     padding: 3px 8px;
                                     font-size: 12px;
                                     z-index : 9999;
                                     }",
                                     glue::glue("#{'AppVersion'} {{
                                                position: {'fixed'};
                                                top: 0;
                                                right: 0;
                                                }}")
                                              )
                                            )
                                          ),
                                     div(id = "AppVersion", class = "info_box", version_id)
                                        )
                                      )
                                    )

)

Tutorial_Tab <- Tutorial_UI("ShinyEvents1")


if (Password_Protected) {
  ui <- bslib::page_navbar(
    title = paste("{ ",Project_Name," }",sep = ""),
    id = "shinyevents_tabs",
    navbar_options = bslib::navbar_options(collapsible = TRUE),
    theme = bslib::bs_theme(bootswatch = "flatly"),
    login_tab)
} else {
  if (AllFilesReady) {
    ui <- bslib::page_navbar(
      title = paste("{ ",Project_Name," }",sep = ""),
      id = "shinyevents_tabs",
      navbar_options = bslib::navbar_options(collapsible = TRUE),
      theme = bslib::bs_theme(bootswatch = "flatly"),
      Homepage_tab,
      DataInput_tab,
      PatientLevel_tab,
      TreatmentAnalytics_tab,
      tte_tab,
      CohortLevel_tab,
      Tutorial_Tab,
      selected = "patient_visual_analytics")
    #selected = "Patient Visual Analytics")
  } else {
    ui <- bslib::page_navbar(
      title = paste("{ ",Project_Name," }",sep = ""),
      id = "shinyevents_tabs",
      navbar_options = bslib::navbar_options(collapsible = TRUE),
      theme = bslib::bs_theme(bootswatch = "flatly"),
      Homepage_tab,
      DataInput_tab,
      PatientLevel_tab,
      TreatmentAnalytics_tab,
      tte_tab,
      CohortLevel_tab,
      Tutorial_Tab,
      selected = "data_input_tab")
    #selected = "Patient Visual Analytics")
  }
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  if(!interactive()) pdf(NULL)

  # Password Protection --------------------------------------------------------
  # hack to add the logout button to the navbar on app launch
  if (Password_Protected) {
    insertUI(
      selector = ".navbar .container-fluid .navbar-collapse",
      ui = tags$ul(
        class="nav navbar-nav navbar-right",
        tags$li(
          div(
            style = "padding: 10px; padding-top: 8px; padding-bottom: 0;",
            #shinyauthr::logoutUI("logout")
            logoutUI("logout")
          )
        )
      )
    )
  }

  # call the shinyauthr login and logout server modules
  if (Password_Protected) {
    #credentials <- shinyauthr::loginServer(
    credentials <- loginServer(
      id = "login",
      data = user_base,
      user_col = "user",
      pwd_col = "password",
      sodium_hashed = FALSE,
      #sodium_hashed = TRUE,
      reload_on_logout = TRUE,
      log_out = reactive(logout_init())
    )
  } else {
    credentials <- reactive({
      list(user_auth = TRUE)
    })
  }

  #logout_init <- shinyauthr::logoutServer(
  if (Password_Protected) {
    logout_init <- logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
  }

  observeEvent(credentials()$user_auth, {
    # if user logs in successfully
    if (Password_Protected) {
      if (credentials()$user_auth) {
        # remove the login tab
        removeTab("tabs", "login")
        # add home tab
        appendTab("tabs", DataInput_tab, select = ifelse(AllFilesReady,FALSE,TRUE))
        #appendTab("tabs", DataProcess_tab, select = FALSE)
        appendTab("tabs", PatientLevel_tab, select = ifelse(AllFilesReady,TRUE,FALSE))
        appendTab("tabs", TreatmentAnalytics_tab, select = FALSE)
        appendTab("tabs", tte_tab, select = FALSE)
        appendTab("tabs", CohortLevel_tab, select = FALSE)
      }
    }

    if (credentials()$user_auth) {

      # Data Input -------------------------------------------------------------

      ProjectName_react <- reactiveVal(Project_Name)
      Param_File_react <- reactiveVal(Event_Param_File)
      Workbook_file_predf_react <- reactiveVal()
      Workbook_file_react <- reactiveVal(Workbook_file)
      PatientAnno_file_react <- reactiveVal(Patient_Annotation_File)
      PatientEvent_File_react <- reactiveVal(Patient_Event_Data_File)

      #data_file_react <- reactiveVal(Data_file)

      #param_input <- reactiveVal(FALSE)
      wkbk_raw_react <- reactiveVal()
      wkbk_react <- reactiveVal()
      wkbk_react_anno <- reactiveVal()
      wkbk_react_anno_sub <- reactiveVal()
      wkbk_react_sub <- reactiveVal()
      Clin_Supp_Cols_List_react <- reactiveVal()
      pat_react <- reactiveVal()
      event_data_raw <- reactiveVal()
      event_data_summ <- reactiveVal()
      event_data_tr_clusters_clean <- reactiveVal()
      event_data <- reactiveVal()
      #event_data_anno <- reactiveVal()
      #eventWsumm_data <- reactiveVal()
      param_data <- reactiveVal()
      paramEvent_data <- reactiveVal()
      #tte_choices_react <- reactiveVal()
      GlobalAppTimeUnit_react <- reactiveVal(input$GlobalAppTimeUnit)

      # URL input
      observe({
        query <- parseQueryString(session$clientData$url_search)
        print(query)
        if (isTruthy(query[['data']])) {
          Workbook_file_react(query[['data']])
        }
        if (isTruthy(query[['proj']])) {
          ProjectName_react(query[['proj']])
        }
        if (isTruthy(query[['patient']])) {
          PatientAnno_file_react(query[['patient']])
        }
      })
      # Front end event data input --------------------------------------------------------
      observe({
        ProjectName_react(input$UserProjectName)
        if (isTruthy(input$dataFileInput$datapath)) {
          Workbook_file_predf_react(input$dataFileInput)
          Workbook_file_react(input$dataFileInput$datapath)
          Param_File_react(input$EventParamsFileIn$datapath)
          PatientEvent_File_react(input$EventDataFileIn$datapath)
        }

      })

      eventDataInput_raw <- reactiveVal()

      observeEvent(input$LoadExampleData, {
        event_data <- as.data.frame(fread(Example_event_file, na.strings = c("","NA")))
        eventDataInput_raw(event_data)
        #event_data_raw <- eventDataInput_raw()
        #col_choices <- colnames(event_data_raw)
        updateSelectizeInput(session,"EventDataEventStartUnits", selected = "Years")
        updateSelectizeInput(session,"EventDataEventEndUnits", selected = "Years")
      })


      output$EventDataFileIn_found <- reactive({
        #req(input$EventDataFileInput)
        EventDataFileInput <- input$EventDataFileInput
        if (isTruthy(EventDataFileInput) | input$LoadExampleData > 0) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "EventDataFileIn_found", suspendWhenHidden = FALSE)

      output$SuppDataInput1_found <- reactive({
        #req(input$SuppDataInput1)
        SuppDataInput1 <- input$SuppDataInput1
        if (isTruthy(SuppDataInput1)) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "SuppDataInput1_found", suspendWhenHidden = FALSE)

      observeEvent(input$EventDataFileInput, {
      #eventDataInput_raw <- eventReactive(input$EventDataFileInput, {
        event_data_file <- input$EventDataFileInput$datapath
        event_data <- as.data.frame(fread(event_data_file, na.strings = c("","NA")))
        eventDataInput_raw(event_data)
      })

      observe({
        req(eventDataInput_raw())
        event_data_raw <- eventDataInput_raw()
        col_choices <- colnames(event_data_raw)

        start_preSel <- ifelse(any(grepl("start",col_choices,ignore.case = T)),grep("start",col_choices,ignore.case = T, value = T)[1],col_choices[4])
        end_preSel <- ifelse(any(grepl("end",col_choices,ignore.case = T)),grep("end",col_choices,ignore.case = T, value = T)[1],col_choices[5])
        eventType_preSel <- ifelse(any(grepl("EventType",col_choices,ignore.case = T)),grep("EventType",col_choices,ignore.case = T, value = T)[1],1)
        summ_preSel <- ifelse(any(grepl("summary",col_choices,ignore.case = T)),grep("summary",col_choices,ignore.case = T, value = T)[1],1)

        updateSelectizeInput(session,"EventDataPatientIDcol",choices = col_choices, selected = col_choices[1], server = T)
        updateSelectizeInput(session,"EventDataEventcol",choices = col_choices, selected = col_choices[2], server = T)
        updateSelectizeInput(session,"EventDataEventStartcol",choices = col_choices, selected = start_preSel, server = T)
        updateSelectizeInput(session,"EventDataEventEndcol",choices = col_choices, selected = end_preSel, server = T)
        updateSelectizeInput(session,"EventDataEventTypecol",choices = col_choices, selected = eventType_preSel, server = T,
                             options = list(
                               placeholder = 'Please select',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
        updateSelectizeInput(session,"EventDataEventSummary",choices = col_choices, selected = summ_preSel, server = T,
                             options = list(
                               placeholder = 'Please select',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      })


      observe({
        req(eventDataInput_raw())
        #req(input$EventDataEventTypecol)
        req(input$EventDataEventcol)
        event_data_raw <- eventDataInput_raw()
        #eventtype_col <- input$EventDataEventTypecol
        eventname_col <- input$EventDataEventcol
        eventtype_col <- input$EventDataEventTypecol
        if (isTruthy(eventtype_col)) {
          treat_resp_df <- unique(event_data_raw[,c(eventtype_col,eventname_col)])
          treat_resp_choices <- lapply(split(treat_resp_df[,2,drop = F], treat_resp_df[[eventtype_col]]), function(df) {
            unname(unlist(df))
          })
          preselected_treat <- treat_resp_choices[["Treatment"]]
          preselected_resp <- c(treat_resp_choices[["Clinical Note"]],treat_resp_choices[["Metastasis"]],c("Enrolled in Hospice","Last Known Alive","Additional Diagnosis"))
        } else {
          treat_resp_choices <- unique(event_data_raw[,eventname_col])
          preselected_treat <- NULL
          preselected_resp <- NULL
        }

        #save(list = ls(), file = "trea_event_select.RData", envir = environment())

        #treat_resp_choices <- unique(paste0(event_data_raw[,eventtype_col],": ",event_data_raw[,eventname_col]))
        #updateSelectizeInput(session,"EventDataTreatmentEvents",choices = treat_resp_choices, selected = character(0), server = T)
        #updateSelectizeInput(session,"EventDataResponseEvents",choices = treat_resp_choices, selected = character(0), server = T)


        if (input$LoadExampleData > 0 & !isTruthy(input$EventDataFileInput)) {
          shinyWidgets::updateVirtualSelect(session = session,inputId = "EventDataTreatmentEvents",choices = treat_resp_choices, selected = preselected_treat)
          shinyWidgets::updateVirtualSelect(session = session,inputId = "EventDataResponseEvents",choices = treat_resp_choices, selected = preselected_resp)
        } else {
          shinyWidgets::updateVirtualSelect(session = session,inputId = "EventDataTreatmentEvents",choices = treat_resp_choices)
          shinyWidgets::updateVirtualSelect(session = session,inputId = "EventDataResponseEvents",choices = treat_resp_choices)
        }


      })

      output$EventDataInputPreview <- DT::renderDataTable({
        req(eventDataInput_raw())
        event_data_raw <- eventDataInput_raw()
        DT::datatable(event_data_raw,
                      escape = F,
                      class = "display nowrap",
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 5,
                                     scrollX = T,
                                     target = "cell"),
                      rownames = F
        )
      })



      output$InputDataReady <- reactive({
        req(eventDataInput_raw())
        req(input$EventDataPatientIDcol,input$EventDataEventcol,input$EventDataEventStartcol,input$EventDataEventEndcol)
        required_inputs <- c(input$EventDataPatientIDcol,input$EventDataEventcol,input$EventDataEventStartcol,input$EventDataEventEndcol)
        required_table <- eventDataInput_raw()
        DataInputsReady <- ifelse(all(required_inputs %in% colnames(required_table)),TRUE,FALSE)
        if (isTruthy(DataInputsReady)) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "InputDataReady", suspendWhenHidden = FALSE)
      observeEvent(input$GlobalAppTimeUnit, {
        GlobalAppTimeUnit_react(input$GlobalAppTimeUnit)
      })
      observeEvent(input$ProcessInputData, {
        req(eventDataInput_raw())
        #req(input$EventDataPatientIDcol,input$EventDataEventcol,input$EventDataEventTypecol,input$EventDataEventStartcol,input$EventDataEventEndcol)
        req(input$EventDataPatientIDcol,input$EventDataEventcol,input$EventDataEventStartcol,input$EventDataEventEndcol)
        eventDataInput_raw <- eventDataInput_raw()
        EventDataPatientIDcol <- input$EventDataPatientIDcol
        EventDataEventcol <- input$EventDataEventcol
        EventDataEventTypecol <- input$EventDataEventTypecol
        EventDataEventStartcol <- input$EventDataEventStartcol
        EventDataEventEndcol <- input$EventDataEventEndcol
        EventDataEventSummary <- input$EventDataEventSummary
        EventDataEventStartUnits <- input$EventDataEventStartUnits
        EventDataEventEndUnits <- input$EventDataEventEndUnits
        EventDataTreatmentEvents <- input$EventDataTreatmentEvents
        EventDataResponseEvents <- input$EventDataResponseEvents
        input_file <- input$EventDataFileInput$datapath
        if (!isTruthy(input_file)) {
          input_file <- "InputDataFile.txt"
        }
        if (!isTruthy(EventDataEventTypecol)) {
          EventDataEventTypecol <- EventDataEventcol
          eventtype_in <- NA
        } else { eventtype_in <- EventDataEventcol }
        cluster_window <- input$MainClusterWindowSet
        updateSelectInput(session,"GlobalAppTimeUnit", selected = EventDataEventStartUnits)


        event_data_processed <- data.frame(Name = eventDataInput_raw[,EventDataPatientIDcol],
                                           Event = eventDataInput_raw[,EventDataEventcol],
                                           EventType = eventDataInput_raw[,EventDataEventTypecol],
                                           EventTab = "InputData",
                                           #EventTab = eventDataInput_raw[,EventDataEventTypecol],
                                           EventStart = eventDataInput_raw[,EventDataEventStartcol],
                                           EventEnd = eventDataInput_raw[,EventDataEventEndcol],
                                           EventColumn = eventDataInput_raw[,EventDataEventTypecol],
                                           EventSummary = NA)
        if (isTruthy(EventDataEventSummary)) {
          event_data_processed$EventSummary <- eventDataInput_raw[,EventDataEventSummary]
          #event_data_processed <- cbind(event_data_processed,
          #                              data.frame(EventSummary = eventDataInput_raw[,EventDataEventSummary]))
          event_data_processed[which(!is.na(event_data_processed$EventSummary)),c("EventTab","EventColumn")] <- NA
        }
        if (all(c("EventStart","EventEnd") %in% colnames(event_data_processed))) {
          event_data_processed[,"EventEnd"] <- ifelse(is.na(event_data_processed[,"EventEnd"]),
                                                      event_data_processed[,"EventStart"],
                                                      event_data_processed[,"EventEnd"])
          event_data_processed[,"EventStart"] <- ifelse(is.na(event_data_processed[,"EventStart"]),
                                                        event_data_processed[,"EventEnd"],
                                                        event_data_processed[,"EventStart"])
        }
        #save(list = ls(), file = "eventdata_make.RData", envir = environment())

        if (isTruthy(input$SuppEventColumnLink)) {
          event_wkbk_link <- input$SuppEventColumnLink
          uniq_links <- unique(eventDataInput_raw[,c(EventDataEventTypecol,event_wkbk_link)])
          event_data_processed$EventTab <- uniq_links[,event_wkbk_link][match(event_data_processed$EventType, uniq_links[,EventDataEventTypecol])]
        }

        param_cols <- c("Data Table Name","Data File","Event Name","Column Defined Event","Event Category","Event Start Column",
                        "Event End Column","Treatment","Response","Event Start Time Units","Event End Time Units")


        treat_event_types <- unique(event_data_processed[which(event_data_processed$Event %in% EventDataTreatmentEvents),"EventType"])
        respn_event_types <- unique(event_data_processed[which(event_data_processed$Event %in% EventDataResponseEvents),"EventType"])


        event_params_base <- unique(event_data_processed[,c("EventTab","EventType")])
        event_params_base[which(is.na(event_params_base$EventTab)),"EventTab"] <- "InputData"

        event_params <- data.frame(data_table_name = event_params_base$EventTab,
                                   data_file = basename(input_file),
                                   event_name = event_params_base$EventType,
                                   #event_name = unique(event_data_processed$EventType),
                                   #column_defined_event = ifelse(is.na(eventtype_in),FALSE,TRUE),
                                   column_defined_event = TRUE,
                                   event_category = event_params_base$EventType,
                                   #event_category = unique(event_data_processed$EventType),
                                   event_start_col = EventDataEventStartcol,
                                   event_end_col = EventDataEventEndcol,
                                   treatment = FALSE,
                                   response = FALSE,
                                   start_time_units = EventDataEventStartUnits,
                                   end_time_units = EventDataEventEndUnits)
        colnames(event_params) <- param_cols
        event_params[which(event_params$`Event Category` %in% treat_event_types),"Treatment"] <- TRUE
        event_params[which(event_params$`Event Category` %in% respn_event_types),"Response"] <- TRUE


        #save(list = ls(), file = "eventdataparams_make.RData", envir = environment())

        wkbk <- list(InputData = eventDataInput_raw)

        #save(list = ls(), file = "eventdataparamswkbk_make.RData", envir = environment())

        #if (isTruthy(input$SuppEventColumnLink)) {
        #  event_wkbk_link <- input$SuppEventColumnLink
        #  uniq_links <- unique(eventDataInput_raw[,c(EventDataEventTypecol,event_wkbk_link)])
        #  uniq_links[which(is.na(uniq_links[,event_wkbk_link])),event_wkbk_link] <- "InputData"
        #  colnames(uniq_links) <- c("Event Category","Data Table Name")
        #  event_params2 <- merge(uniq_links,event_params[,-1],all = T, sort = F)
        #  event_params2 <- event_params2 %>%
        #    relocate(`Data Table Name`) %>%
        #    relocate(`Event Category`, .after = `Column Defined Event`) %>%
        #    as.data.frame()
        #  event_params <- unique(event_params2)
        #  rownames(event_params) <- NULL
        #}


        pat_anno <- event_count_df(event_data_processed)

        if (!is.na(eventtype_in)) {
          event_new <- apply(event_data_processed,1,function(x) {
            event <- x[["Event"]]
            eventtype <- x[["EventType"]]
            eventsumm <- x[["EventSummary"]]
            if (is.na(eventsumm)) {
              if (event == eventtype) {
                return(event)
              } else {
                if (grepl(paste0("^",eventtype,": "),event, ignore.case = T)) {
                  return(event)
                } else {
                  return(paste0(eventtype,": ",event))
                }
              }
            } else {
              return(event)
            }

          })
          event_data_processed$Event <- event_new
          #event_data_processed$Event <- paste0(event_data_processed$EventType,": ",event_data_processed$Event)
        }

        if (all(is.na(event_data_processed$EventSummary))) {
        #if (!any(grepl("summary$", colnames(event_data_processed), ignore.case = T))) {
          if (isTruthy(EventDataTreatmentEvents) | isTruthy(EventDataResponseEvents)) {
            treatment_events <- EventDataTreatmentEvents
            response_events <- EventDataResponseEvents
            if (isTruthy(treatment_events)) {
              event_data_tr <- event_data_processed[grepl(paste(treatment_events,collapse = "|"),event_data_processed$Event),]
              event_data_tr_cls <- eventDataSummary(event_data_tr, event_summary = "Treatment", verbose = F, cluster_window = cluster_window)
            } else { event_data_tr_cls <- NULL }
            if (isTruthy(response_events)) {
              event_data_re <- event_data_processed[grepl(paste(treatment_events,collapse = "|"),event_data_processed$Event),]
              event_data_re_cls <- eventDataSummary(event_data_re, event_summary = "Response", verbose = F, cluster_window = cluster_window)
            } else { event_data_re_cls <- NULL }

            event_data_cls <- rbind(event_data_tr_cls,event_data_re_cls)
            event_data_cls$Event <- gsub("Cluster \\d+$","Cluster",event_data_cls$Event)
            event_data_cls <- event_data_cls %>%
              group_by(Name) %>%
              arrange(!EventType %in% c("Full Treatment Summary","Full Response Summary"), .by_group = TRUE)
            Patient_Event_Data_cls_all <- data.table::rbindlist(list(event_data_cls,event_data_processed), fill = T)
            Patient_Event_Data_cls_all <- Patient_Event_Data_cls_all[order(Patient_Event_Data_cls_all[,1]),]
            Patient_Event_Data_cls_all <- as.data.frame(Patient_Event_Data_cls_all)
            event_data_processed <- Patient_Event_Data_cls_all
          }
        }


        #save(list = ls(), file = "eventdataparamswkbksumm_make.RData", envir = environment())



        ProjectName_react(input$UserProjectName)
        Param_File_react(NULL)
        Workbook_file_predf_react(NULL)
        Workbook_file_react(NULL)
        PatientAnno_file_react(NULL)
        PatientEvent_File_react(NULL)

        wkbk_raw_react(wkbk)
        wkbk_react(wkbk)
        wkbk_react_anno(wkbk)
        wkbk_react_anno_sub(wkbk)
        wkbk_react_sub(wkbk)
        Clin_Supp_Cols_List <- lapply(wkbk,function(x){
          return(colnames(x)[-1])
        })
        Clin_Supp_Cols_List_react(Clin_Supp_Cols_List)
        pat_react(pat_anno)
        event_data_raw(event_data_processed)
        event_data_summ(event_data_processed)
        #event_data_tr_clusters_clean()
        event_data(event_data_processed)
        param_data(event_params)
        paramEvent <- event_params[which(!is.na(event_params[,3])),]
        paramEvent_data(paramEvent)
        #tte_choices_react()

      })

      # Front End wkbk input ---------------------------------------------------


      output$SuppDataFileInput1_found <- reactive({
        req(input$SuppDataFileInput1)
        suppDataFileInput <- input$SuppDataFileInput1
        if (isTruthy(suppDataFileInput)) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "SuppDataFileInput1_found", suspendWhenHidden = FALSE)
      observe({
        req(eventDataInput_raw())
        req(input$SuppDataFileInput1)
        event_data_raw <- eventDataInput_raw()
        col_choices <- colnames(event_data_raw)
        pre_sel_col <- ifelse(any(grepl("tab|table",col_choices,ignore.case = T)),grep("tab|table",col_choices,ignore.case = T, value = T)[1],1)
        updateSelectizeInput(session,"SuppEventColumnLink",choices = col_choices,selected = pre_sel_col,server = T,
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      })

      # Workbook file(s) input
      Workbook_file_df_react <- reactive({
        file_df <- Workbook_file_predf_react()
        file_df
      })
      # Read in workbook/files to make initial workbook
      observeEvent(input$ProcessInputData, {
      #observeEvent(input$SuppDataFileInput1, {
        req(input$SuppDataFileInput1)
        file_df <- input$SuppDataFileInput1

        if (nrow(file_df) == 1) {
          if (tools::file_ext(file_df$datapath[1]) %in% c("xlsx","xls")) {
            wkbk <- read_excel_allsheets(file_df$datapath[1])
            wkbk_raw_react(c(wkbk_raw_react(),wkbk))
            #wkbk_raw_react(wkbk)
          } else {
            df <- as.data.frame(fread(file_df[row,"datapath"], na.strings = c("","NA")))
            tabName <- tools::file_path_sans_ext(file_df[row,"name"])
            wkbk[[tabName]] <- df
            wkbk_raw_react(c(wkbk_raw_react(),wkbk))
          }
          # Multiple files loaded to be made to wkbk
        } else if (nrow(file_df) > 1) {
          wkbk <- list()
          for (row in seq(nrow(file_df))) {
            df <- as.data.frame(fread(file_df[row,"datapath"], na.strings = c("","NA")))
            tabName <- tools::file_path_sans_ext(file_df[row,"name"])
            wkbk[[tabName]] <- df
          }
          wkbk_raw_react(c(wkbk_raw_react(),wkbk))
          #wkbk_raw_react(wkbk)
        }


      })
      #observe({
      #  # wkbk file loaded
      #  if (isTruthy(wkbk_pre)) {
      #    wkbk_raw_react(wkbk_pre)
      #  } else {
      #    if (isTruthy(Workbook_file_react())) {
      #      if (isTruthy(Workbook_file_df_react())) {
      #        file_df <- Workbook_file_df_react()
      #        # wkbk file loaded
      #        if (nrow(file_df) == 1) {
      #          if (tools::file_ext(file_df$datapath[1]) %in% c("xlsx","xls")) {
      #            wkbk <- read_excel_allsheets(file_df$datapath[1])
      #            wkbk_raw_react(wkbk)
      #          }
      #          # Multiple files loaded to be made to wkbk
      #        } else if (nrow(file_df) > 1) {
      #          wkbk <- list()
      #          for (row in seq(nrow(file_df))) {
      #            df <- as.data.frame(fread(file_df[row,"datapath"]))
      #            tabName <- tools::file_path_sans_ext(file_df[row,"name"])
      #            wkbk[[tabName]] <- df
      #          }
      #          wkbk_raw_react(wkbk)
      #        }
      #        # wkbk loaded in the back-end
      #      } else {
      #        if (tools::file_ext(Workbook_file_react()) %in% c("xlsx","xls")) {
      #          wkbk <- read_excel_allsheets(Workbook_file_react())
      #          wkbk_raw_react(wkbk)
      #        }
      #      }
      #    }
      #  }
      #
      #})

      ## Workbook --------------------------------------------------------------

      # Use Example data
      #observeEvent(input$UseExpWkbk, {
      #  ProjectName_react("GENIE NSCLC")
      #  wkbk <- read_excel_allsheets(Example_wkbk)
      #  wkbk_raw_react(wkbk)
      #})
      ## Workbook file(s) input
      #Workbook_file_df_react <- reactive({
      #  file_df <- Workbook_file_predf_react()
      #  file_df
      #})
      ## Read in workbook/files to make initial workbook
      #observe({
      #  # wkbk file loaded
      #  if (isTruthy(wkbk_pre)) {
      #    wkbk_raw_react(wkbk_pre)
      #  } else {
      #    if (isTruthy(Workbook_file_react())) {
      #      if (isTruthy(Workbook_file_df_react())) {
      #        file_df <- Workbook_file_df_react()
      #        # wkbk file loaded
      #        if (nrow(file_df) == 1) {
      #          if (tools::file_ext(file_df$datapath[1]) %in% c("xlsx","xls")) {
      #            wkbk <- read_excel_allsheets(file_df$datapath[1])
      #            wkbk_raw_react(wkbk)
      #          }
      #          # Multiple files loaded to be made to wkbk
      #        } else if (nrow(file_df) > 1) {
      #          wkbk <- list()
      #          for (row in seq(nrow(file_df))) {
      #            df <- as.data.frame(fread(file_df[row,"datapath"]))
      #            tabName <- tools::file_path_sans_ext(file_df[row,"name"])
      #            wkbk[[tabName]] <- df
      #          }
      #          wkbk_raw_react(wkbk)
      #        }
      #        # wkbk loaded in the back-end
      #      } else {
      #        if (tools::file_ext(Workbook_file_react()) %in% c("xlsx","xls")) {
      #          wkbk <- read_excel_allsheets(Workbook_file_react())
      #          wkbk_raw_react(wkbk)
      #        }
      #      }
      #    }
      #  }
      #
      #})
      # Render side panel when data is input
      output$rendTabsetSidePan <- renderUI({
        req(wkbk_raw_react())
        tabsetPanel(
          tabPanel("Step 1",
                   p(),
                   radioButtons("ParamInputOpt",NULL,choices = c("Manually Select Parameters","Input Parameter File"), inline = T,
                                selected = ifelse(AllFilesReady,"Input Parameter File","Manually Select Parameters")),
                   conditionalPanel(condition = "input.ParamInputOpt == 'Manually Select Parameters' & input.DataInputMain != '1'",
                                    h3("Data Parameters"),
                                    h5("Select parameters for each table of event data"),
                                    uiOutput("rendTableSpecs")
                   ),
                   conditionalPanel(condition = "input.ParamInputOpt == 'Input Parameter File'",
                                    fileInput("EventParamsFileIn","Event Parameter File Upload"),
                                    actionButton("UseExpParam","Load Example Parameter Data")
                   )
          ),
          tabPanel("Step 2",
                   p(),
                   radioButtons("EventDataOpt",NULL,choices = c("Generate Event Data","Input Event Data file"), inline = T),
                   conditionalPanel(condition = "input.EventDataOpt == 'Generate Event Data'",
                                    numericInput("SummaryLeinInput","Event Cluster Window (Months):",value = Event_Cluster_Window,min = 0, step = 0.5),
                                    actionButton("GenEventData","Generate Event Data"),
                                    p(),
                                    uiOutput("renddnldGenEventData")
                   ),
                   conditionalPanel(condition = "input.EventDataOpt == 'Input Event Data file'",
                                    fileInput("EventDataFileIn","Event Data File Upload"),
                                    actionButton("UseExpEvent","Load Example Event Data")
                   )
          )
        )

      })
      # Render event data download if event data is made
      output$renddnldGenEventData <- renderUI({
        req(event_data())
        EventData <- event_data()
        if (nrow(EventData) > 0) {
          downloadButton("dnldGenEventData","Download Event Data Table")
        }
      })
      # Render Parameter input options side panel
      output$rendTableSpecs <- renderUI({
        req(input$DataInputMain)
        req(wkbk_raw_react())
        wkbk <- wkbk_raw_react()
        tabs <- c("Event Parameters",names(wkbk))
        myPanels <- lapply(as.character(c(2:(length(tabs)+1))), function(x){
          x_tab_name <- tabs[as.numeric(x)]
          conditionalPanel(condition = paste0("input.DataInputMain == '",x,"'"),
                           textInput(paste0("DataTableName_",x),"Table Name:", value = x_tab_name),
                           fluidRow(
                             column(8,
                                    selectizeInput(paste0("StartTimeCol_",x),"Start Time Column:", choices = colnames(wkbk[[x_tab_name]]), selected = 1,
                                                   options = list(
                                                     placeholder = 'Please select',
                                                     onInitialize = I('function() { this.setValue(""); }')
                                                   ))
                             ),
                             column(4,
                                    selectInput(paste0("StartTimeColUnits_",x),"Units:",choices = c("Days","Months","Years"))
                             )
                           ),
                           fluidRow(
                             column(8,
                                    selectizeInput(paste0("StopTimeCol_",x),"Stop Time Column:", choices = colnames(wkbk[[x_tab_name]]), selected = 1,
                                                   options = list(
                                                     placeholder = 'Please select',
                                                     onInitialize = I('function() { this.setValue(""); }')
                                                   ))
                             ),
                             column(4,
                                    selectInput(paste0("StopTimeColUnits_",x),"Units:",choices = c("Days","Months","Years"))
                             )
                           ),
                           radioButtons(paste0("RadioEventCol_",x),"Specified event column?", choices = c("No","Yes"), inline = T),
                           conditionalPanel(condition = paste0("input.RadioEventCol_",x,"== 'Yes'"),
                                            selectizeInput(paste0("EventCol_",x),"Event Name", choices = colnames(wkbk[[x_tab_name]]), selected = 1,
                                                           options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            ),
                                            textInput(paste0("EventColName_",x),"Event Name:", value = "",
                                                      placeholder = "If different than column name")
                           ),
                           checkboxGroupInput(paste0("TreatResp_",x),"",choices = c("Treatment Data","Response Data"), inline = T)
          )
        })
        myPanels
      })
      # List of even parameters for each data table
      tableSpecs_list <- reactive({
        req(input$ParamInputOpt)
        if (input$ParamInputOpt == "Manually Select Parameters") {
          req(wkbk_raw_react())
          wkbk <- wkbk_raw_react()
          tabs <- names(wkbk)

          myTabs <- lapply(as.character(c(2:(length(tabs)+1))), function(x){
            DataTableName <- ifelse(isTruthy(input[[paste0("DataTableName_",x)]]),input[[paste0("DataTableName_",x)]],NA)
            StartColName <- ifelse(isTruthy(input[[paste0("StartTimeCol_",x)]]),input[[paste0("StartTimeCol_",x)]],NA)
            StartColUnit <- ifelse(isTruthy(input[[paste0("StartTimeColUnits_",x)]]),input[[paste0("StartTimeColUnits_",x)]],NA)
            StopColName <- ifelse(isTruthy(input[[paste0("StopTimeCol_",x)]]),input[[paste0("StopTimeCol_",x)]],NA)
            StopColUnit <- ifelse(isTruthy(input[[paste0("StopTimeColUnits_",x)]]),input[[paste0("StopTimeColUnits_",x)]],NA)
            EventColYN <- ifelse(isTruthy(input[[paste0("RadioEventCol_",x)]]),input[[paste0("RadioEventCol_",x)]],NA)
            EventCol <- NA
            EventColName <- NA
            if (isTruthy(EventColYN)) {
              if (EventColYN == "Yes") {
                EventCol <- ifelse(isTruthy(input[[paste0("EventCol_",x)]]),input[[paste0("EventCol_",x)]],NA)
                EventColName <- ifelse(isTruthy(input[[paste0("EventColName_",x)]]),input[[paste0("EventColName_",x)]],NA)
              }
            }
            Treatment <- ifelse("Treatment Data" %in% input[[paste0("TreatResp_",x)]],TRUE,FALSE)
            Response <- ifelse("Response Data" %in% input[[paste0("TreatResp_",x)]],TRUE,FALSE)
            return(c(DataTableName,StartColName,StartColUnit,StopColName,StopColUnit,EventColYN,EventCol,EventColName,Treatment,Response))

          })
          myTabs
        }
      })

      ## Params ----------------------------------------------------------------
      # Use Example data
      #observeEvent(input$UseExpParam, {
      #  Param_File_react(Example_param)
      #})
      # Generate or load in parameter data
      observe({
        if (isTruthy(input$ParamInputOpt)) {
          if (input$ParamInputOpt == "Manually Select Parameters") {
            req(wkbk_raw_react())
            req(tableSpecs_list())
            tabs <- names(wkbk_raw_react())
            myTabs <- lapply(tableSpecs_list(), function(x) {
              x <- as.data.frame(t(x))
              colnames(x) <- c("DataTableName","StartColName","StartColUnit","StopColName","StopColUnit","EventColYN","EventCol","EventColName","Treatment","Response")
              return(x)
            })
            params <- as.data.frame(rbindlist(myTabs, fill = T))

            params$DataTable <- params$DataTableName

            params2 <- params %>%
              select(DataTableName,DataTable,EventCol,EventColYN,EventColName,StartColName,StopColName,Treatment,Response,StartColUnit,StopColUnit) %>%
              mutate(EventColYN = case_when(
                EventColYN == "Yes" ~ TRUE,
                EventColYN == "No" ~ FALSE
              )) %>%
              as.data.frame()
            colnames(params2) <- c("Data Table Name","Data Table","Event Name","Column Defined Event","Event Category",
                                   "Event Start Column","Event End Column","Treatment","Response","Event Start Time Units","Event End Time Units")
            params2[,"Event End Column"] <- ifelse(is.na(params2[,"Event End Column"]),params2[,"Event Start Column"],params2[,"Event End Column"])
            params2[,"Event Category"] <- ifelse(is.na(params2[,"Event Category"]),params2[,"Event Name"],params2[,"Event Category"])
            param_data(params2)
            param_events <- params2[which(!is.na(params2[,3])),]
            rownames(param_events) <- NULL
            colnames(param_events) <- gsub(" ","_",colnames(param_events))
            paramEvent_data(param_events)
          }
          if (input$ParamInputOpt == "Input Parameter File") {
            if (isTruthy(Param_File_react())) {
              params <- as.data.frame(fread(Param_File_react(),na.strings = c("","NA")))
              param_data(params)
              param_events <- params[which(!is.na(params[,3])),]
              rownames(param_events) <- NULL
              paramEvent_data(param_events)
            }
          }
        } else {
          if (isTruthy(Param_File_react())) {
            params <- as.data.frame(fread(Param_File_react(),na.strings = c("","NA")))
            param_data(params)
            param_events <- params[which(!is.na(params[,3])),]
            rownames(param_events) <- NULL
            paramEvent_data(param_events)
          }
        }

      })

      ## Workbook Adj ------------------------------------------------------------
      # Adjust workbook if needed
      observe({

        req(param_data())
        req(wkbk_raw_react())
        param <- param_data()
        wkbk <- wkbk_raw_react()
        AppTimeUnit <- GlobalAppTimeUnit_react()
        AppTimeUnit_low <- tolower(AppTimeUnit)
        #save(list = ls(), file = "wkbk_react.RData", envir = environment())
        if (length(which(is.na(param[,1]) == T)) == 0) {
          #wkbk <- wkbk[unique(param[,1])]
          Clin_Supp_Cols_List <- list()
          wkbk_tabs <- unique(param[,1])

          wkbk_adj <- lapply(names(wkbk), function(df_name) {
            df <- wkbk[[df_name]]
            if (df_name %in% param[,1]) {
              param_tab <- unique(param[which(param[,1] == df_name),c(6,7,10,11)])
              rownames(param_tab) <- NULL
              start_col_names <- unique(param_tab[,"Event Start Column"])
              if (start_col_names %in% colnames(df)) {
                if (all(!is.na(start_col_names))) {
                  start_col_names <- start_col_names[which(!is.na(start_col_names))]
                  df <- df %>%
                    arrange(!!!syms(c(colnames(df)[1],start_col_names))) %>%
                    as.data.frame()

                  for (row in 1:nrow(param_tab)) {
                    row <- unlist(param_tab[row,])
                    start_col_name <- row["Event Start Column"]
                    stop_col_name <- row["Event End Column"]
                    stop_col_name <- ifelse(is.na(stop_col_name),start_col_name,stop_col_name)
                    start_unit <- row["Event Start Time Units"]
                    stop_unit <- row["Event End Time Units"]
                    stop_unit <- ifelse(is.na(stop_unit),start_unit,stop_unit)

                    #for (start_col in seq(start_col_names)) {

                    if (!is.na(start_unit)) {
                      #df[,start_col_name] <- convert_time_units(suppressWarnings(as.numeric(df[,start_col_name])),start_unit,"years")
                      df[,start_col_name] <- convert_time_units(suppressWarnings(as.numeric(df[,start_col_name])),start_unit,AppTimeUnit_low)
                    }
                    if (start_col_name != stop_col_name) {
                      if (!is.na(stop_unit)) {
                        #df[,stop_col_name] <- convert_time_units(suppressWarnings(as.numeric(df[,stop_col_name])),stop_unit,"years")
                        df[,stop_col_name] <- convert_time_units(suppressWarnings(as.numeric(df[,stop_col_name])),stop_unit,AppTimeUnit_low)
                      }
                    }
                    #}

                  }
                }
              }
            }
            return(df)

          })
          names(wkbk_adj) <- names(wkbk)
          Clin_Supp_Cols_List <- lapply(wkbk_adj,function(x){
            return(colnames(x)[-1])
          })
          wkbk_react(wkbk_adj)
          wkbk_react_anno(wkbk_adj)
          wkbk_react_anno_sub(wkbk_adj)
          wkbk_react_sub(wkbk_adj)

          Clin_Supp_Cols_List_react(Clin_Supp_Cols_List)
        }

      })

      observeEvent(input$ApplyDataFilter, {

        req(wkbk_react_anno())
        req(wkbk_raw_react())
        req(wkbk_react())
        req(input$TableToFilterMain)
        req(pat_react())
        #req(event_data())
        req(event_data_summ())
        #req(event_data_tr_clusters_clean())

        wkbk <- wkbk_raw_react()
        wkbk_anno <- wkbk_react_anno()
        wkbk_react <- wkbk_react()
        df_name <- input$TableToFilterMain
        df <- wkbk[[df_name]]
        filters <- input$TableFilterMainInput
        #event_data <- event_data()
        event_data <- event_data_summ()
        #event_data_tr <- event_data_tr_clusters_clean()
        pat_react <- pat_react()
        tree_df <- TableFilterMainInput_df()

        #save(list = ls(), file = "ApplyDataFilter.RData", envir = environment())

        if (isTruthy(filters)) {
          req(tree_df)
          filter_keys <- lapply(filters, function(x) {
            if (!x$text[[1]] %in% unique(tree_df$variable_N)) {
              id <- x[["id"]][[1]]
              val <- x[["text"]][[1]]
              if (val == "NA") {
                val <- NA
              }
              col <- gsub(paste0("_",val,"$"),"",id)
              return(c(col = col,val = val))
            }
          })
          filter_keys <- Filter(Negate(is.null),filter_keys)
          filter_keys2 <- setNames(lapply(filter_keys, `[[`, "val"), sapply(filter_keys, `[[`, "col"))
          filter_keys3 <- lapply(split(filter_keys2, names(filter_keys2)), unlist)
          filter_keys4 <- lapply(names(filter_keys3), function(x) {
            filter_vals <- filter_keys3[[x]]
            keys <- which(df[,x] %in% filter_vals)
            return(keys)
          })
          filter_keys5 <- Reduce(intersect,filter_keys4)
          df <- df[filter_keys5,]
          subset_ids <- unique(df[,1])
          subset_ids
        } else {
          subset_ids <- unique(df[,1])
          subset_ids
        }

        #wkbk_subset <- lapply(wkbk,function(df) {
        #  return(df[which(df[,1] %in% subset_ids),])
        #})
        wkbk_anno_subset <- lapply(wkbk_anno,function(df) {
          return(df[which(df[,1] %in% subset_ids),])
        })
        wkbk_react_subset <- lapply(wkbk_react,function(df) {
          return(df[which(df[,1] %in% subset_ids),])
        })
        event_data_subset <- event_data[which(event_data[,1] %in% subset_ids),]
        #event_data_tr_subset <- event_data_tr[which(event_data_tr[,1] %in% subset_ids),]
        pat_react_subset <- pat_react[which(pat_react[,1] %in% subset_ids),]

        #event_data(event_data_subset)
        event_data_summ(event_data_subset)
        #event_data_tr_clusters_clean(event_data_tr_subset)
        pat_react(pat_react_subset)
        wkbk_react_anno_sub(wkbk_anno_subset)
        wkbk_react_sub(wkbk_react_subset)


      })


      # Update certain inputs based on workbook information
      observe({
        req(wkbk_react_sub())
        wkbk_names <- names(wkbk_react_sub())
        preSelect_lineplot <- ifelse(any(grepl("lab",wkbk_names, ignore.case = T)),grep("lab",wkbk_names, ignore.case = T, value = T)[1],wkbk_names[1])
        updateSelectInput(session,"LinePlotTable",choices = names(wkbk_react_sub()),selected = preSelect_lineplot)
      })
      #observe({
      #  req(wkbk_react_sub())
      #  wkbk_names <- names(wkbk_react_sub())
      #  preSelect_sankeyplot <- ifelse(any(grepl("medication",wkbk_names, ignore.case = T)),grep("medication",wkbk_names, ignore.case = T, value = T)[1],wkbk_names[1])
      #  updateSelectInput(session,"sankeyDataTable",choices = names(wkbk_react_sub()),selected = preSelect_sankeyplot)
      #})
      observe({
        req(wkbk_react_sub())
        wkbk_names <- names(wkbk_react_sub())
        wkbk_names <- c(wkbk_names,"ShinyEvents Treatment Clusters")
        updateSelectizeInput(session,"KPstrataDataTable",choices = c("No Strata",wkbk_names),
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      })

      ## Event Data ------------------------------------------------------------

      #summary_cluster_window <- reactiveVal(as.numeric(Event_Cluster_Window))
      #summary_cluster_window <- reactiveVal(as.numeric(Event_Cluster_Window))

      observeEvent(input$SummaryLeinInput,{
        #summary_cluster_window(input$SummaryLeinInput)
        updateNumericInput(session,"MainClusterWindowSet",value = input$SummaryLeinInput)
      })
      observeEvent(input$MainClusterWindowSet,{
        #summary_cluster_window(input$MainClusterWindowSet)
        updateNumericInput(session,"SummaryLeinInput",value = input$MainClusterWindowSet)
      })


      # Use example event data
      #observeEvent(input$UseExpEvent, {
      #  PatientEvent_File_react(Example_event)
      #})
      # Generate event data when button is pressed
      observeEvent(input$GenEventData, {
        req(param_data())
        req(wkbk_react_sub())
        param <- param_data()
        wkbk <- wkbk_react_sub()
        if (input$EventDataOpt == "Generate Event Data") {
          Patient_Event_Data <- getEventData(param = param, data = wkbk, cluster_window = input$SummaryLeinInput, summary = TRUE)
          event_data_raw(Patient_Event_Data)
          param_events <- param[which(!is.na(param[,3])),]
          rownames(param_events) <- NULL
          paramEvent_data(param_events)
        }
      })
      # pre-processed event data file
      observe({
        #print("1665")
        req(PatientEvent_File_react())
        #Patient_Event_Data_raw <- reactive({
        if (isTruthy(PatientEvent_File_react())) {
          Patient_Event_Data <- as.data.frame(fread(PatientEvent_File_react(), fill = T, sep = '\t', header = T))
          colnames(Patient_Event_Data)[1] <- "Name"
          event_data_raw(Patient_Event_Data)
          #Patient_Event_Data
        }
      })

      # Upload event data file
      observe({
        #print("1677")
        req(paramEvent_data())
        req(event_data_raw())
        #req(summary_cluster_window())
        param <- paramEvent_data()
        Patient_Event_Data <- event_data_raw()
        #cluster_window <- summary_cluster_window()
        cluster_window <- Event_Cluster_Window

        # Event data already has summary columns
        if (!any(grepl("summary$", colnames(Patient_Event_Data), ignore.case = T))) {
          treatment_events <- unique(param[which(param$Treatment == TRUE),])
          treatment_events <- unique(ifelse(treatment_events$`Column Defined Event` == FALSE,treatment_events$`Event Name`,
                                            paste0(treatment_events$`Event Category`,": ")))
          response_events <- unique(param[which(param$Response == TRUE),])
          response_events <- unique(ifelse(response_events$`Column Defined Event` == FALSE,response_events$`Event Name`,
                                           paste0(response_events$`Event Category`,": ")))
          event_data_tr <- Patient_Event_Data[grepl(paste(treatment_events,collapse = "|"),Patient_Event_Data$Event),]
          event_data_re <- Patient_Event_Data[grepl(paste(response_events,collapse = "|"),Patient_Event_Data$Event),]

          event_data_tr_cls <- eventDataSummary(event_data_tr, event_summary = "Treatment", verbose = F, cluster_window = cluster_window)
          event_data_re_cls <- eventDataSummary(event_data_re, event_summary = "Response", verbose = F, cluster_window = cluster_window)
          event_data_cls <- rbind(event_data_tr_cls,event_data_re_cls)
          event_data_cls$Event <- gsub("Cluster \\d+$","Cluster",event_data_cls$Event)
          event_data_cls <- event_data_cls %>%
            group_by(Name) %>%
            arrange(!EventType %in% c("Full Treatment Summary","Full Response Summary"), .by_group = TRUE)
          Patient_Event_Data_cls_all <- data.table::rbindlist(list(event_data_cls,Patient_Event_Data), fill = T)
          Patient_Event_Data_cls_all <- Patient_Event_Data_cls_all[order(Patient_Event_Data_cls_all[,1]),]
          Patient_Event_Data_cls_all <- as.data.frame(Patient_Event_Data_cls_all)
          Patient_Event_Data <- Patient_Event_Data_cls_all
        }
        event_data_summ(Patient_Event_Data)


      })

      observe({

        param <- paramEvent_data()
        wkbk_raw <- wkbk_raw_react()
        wkbk <- wkbk_react_sub()
        wkbk_anno <- wkbk_react_anno_sub()
        cluster_window <- input$MainClusterWindowSet
        Patient_Event_Data <- event_data_raw()
        event_data <- event_data_summ()
        UpdateSummaryClusters <- input$UpdateSummaryClusters
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        event_data <- event_data()
        event_data_key <- event_data_key()

        ParamInputOpt <- input$ParamInputOpt
        Param_File_react <- Param_File_react()
        GenEventData <- input$GenEventData


        #save(list = ls(), file = "update_check.RData", envir = environment())
      })

      observeEvent(input$UpdateSummaryClusters, {
        #print("1709")
        req(paramEvent_data())
        req(wkbk_react_sub())
        req(event_data_raw())
        req(input$MainClusterWindowSet)
        param <- paramEvent_data()
        wkbk <- wkbk_react_sub()
        cluster_window <- input$MainClusterWindowSet
        Patient_Event_Data <- event_data_raw()


        #save(list = ls(), file = "UpdateSummaryClusters.RData", envir = environment())

        Patient_Event_Data <- Patient_Event_Data[!is.na(Patient_Event_Data$EventTab),]

        treatment_events <- unique(param[which(param$Treatment == TRUE),])
        treatment_events <- unique(ifelse(treatment_events$`Column Defined Event` == FALSE,treatment_events$`Event Name`,
                                          paste0(treatment_events$`Event Category`,": ")))
        response_events <- unique(param[which(param$Response == TRUE),])
        response_events <- unique(ifelse(response_events$`Column Defined Event` == FALSE,response_events$`Event Name`,
                                         paste0(response_events$`Event Category`,": ")))
        event_data_tr <- Patient_Event_Data[grepl(paste(treatment_events,collapse = "|"),Patient_Event_Data$Event),]
        event_data_re <- Patient_Event_Data[grepl(paste(response_events,collapse = "|"),Patient_Event_Data$Event),]

        event_data_tr_cls <- eventDataSummary(event_data_tr, event_summary = "Treatment", verbose = F, cluster_window = cluster_window)
        event_data_re_cls <- eventDataSummary(event_data_re, event_summary = "Response", verbose = F, cluster_window = cluster_window)
        event_data_cls <- rbind(event_data_tr_cls,event_data_re_cls)

        event_data_cls$Event <- gsub("Cluster \\d+$","Cluster",event_data_cls$Event)
        event_data_cls <- event_data_cls %>%
          group_by(Name) %>%
          arrange(!EventType %in% c("Full Treatment Summary","Full Response Summary"), .by_group = TRUE)

        Patient_Event_Data_cls_all <- data.table::rbindlist(list(event_data_cls,Patient_Event_Data), fill = T)
        Patient_Event_Data_cls_all <- Patient_Event_Data_cls_all[order(Patient_Event_Data_cls_all[,1]),]
        Patient_Event_Data_cls_all <- as.data.frame(Patient_Event_Data_cls_all)
        #event_data_summ(Patient_Event_Data_cls_all)

        # next update
        event_data <- Patient_Event_Data_cls_all
        treat_summ_events <- paste0(unique(param[which(param$Treatment == TRUE),"Event Category"])," Summary")
        event_data_treat_summ <- event_data[which(event_data$EventType %in% c(treat_summ_events,"Full Treatment Summary")),]
        event_data_treat_summ$EventType <- ifelse(event_data_treat_summ$EventType == "Full Treatment Summary","Full Treatment Summary",
                                                  gsub(" Summary$","",event_data_treat_summ$EventType))
        # Assign numeric cluster line treatments
        event_data_clusters <- event_data_treat_summ %>%
          mutate(Event = EventType) %>%
          group_by(Name,Event) %>%
          arrange(EventStart,EventEnd, .by_group = TRUE) %>%
          mutate(Treatment_Line_Cluster = paste0(unique(Event)," Cluster Line ",seq(n()))) %>%
          mutate(Event = Treatment_Line_Cluster) %>%
          select(-c(EventTab,EventColumn)) %>%
          as.data.frame()
        event_data_clusters$EventType <- ifelse(event_data_clusters$EventType == "Full Treatment Summary","Full Treatment Summary",
                                                paste0(event_data_clusters$EventType," Cluster"))
        # Clean event summary
        sankey_event_opts <- unique(event_data_clusters$EventType)
        #event_data_clusters_clean <- lapply(sankey_event_opts, function(x) {
        #  event_data_clusters_sub <- event_data_clusters[which(event_data_clusters$EventType == x),]
        #  x <- ifelse(x == "Full Treatment Summary","Full Treatment Summary",gsub(" Cluster$","",x))
        #  if (all(grepl(gsub(" Summary$|Summary$","",x),event_data_clusters_sub$EventSummary))) {
        #    event_data_clusters_sub$EventSummary <- gsub(gsub(" Summary$|Summary$","",x),"",event_data_clusters_sub$EventSummary)
        #    event_data_clusters_sub$EventSummary <- trimws(event_data_clusters_sub$EventSummary)
        #    event_data_clusters_sub$EventSummary <- gsub("\\\r\n  |\\\n  ",", ",event_data_clusters_sub$EventSummary)
        #  } else {
        #    new_col <- unname(sapply(event_data_clusters_sub$EventSummary, function(y) {
        #      check_splt <- strsplit(y,"\\\r\n|\\\n")[[1]]
        #      headers <- grep("^\\s+",check_splt,invert = T)
        #      new_col <- paste0(sapply(seq_along(headers), function(i) {
        #        header <- check_splt[headers[i]]
        #        start <- headers[i] + 1
        #        end <- if (i < length(headers)) headers[i+1] - 1 else length(check_splt)
        #        items <- trimws(check_splt[start:end])
        #        items_joined <- paste(items, collapse = ", ")
        #        result_parts <- paste0(header, ": ", items_joined)
        #        return(result_parts)
        #      }), collapse = " - ")
        #    }))
        #    event_data_clusters_sub$EventSummary <- new_col
        #  }
        #  return(event_data_clusters_sub)
        #})
        #event_data_clusters_clean <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE),
        #                                    event_data_clusters_clean)
        #event_data_tr_clusters_clean(event_data_clusters_clean)
        event_data_clusters_clean <- lapply(sankey_event_opts, function(x) {
          event_data_clusters_sub <- event_data_clusters[which(event_data_clusters$EventType == x),]
          x <- ifelse(x == "Full Treatment Summary","Full Treatment Summary",gsub(" Cluster$","",x))
          if (all(grepl(gsub(" Summary$|Summary$","",x),event_data_clusters_sub$EventSummary))) {
            event_data_clusters_sub$EventSummary <- gsub(gsub(" Summary$|Summary$","",x),"",event_data_clusters_sub$EventSummary)
            event_data_clusters_sub$EventSummary <- trimws(event_data_clusters_sub$EventSummary)
            event_data_clusters_sub$EventSummary <- gsub(", ",",",event_data_clusters_sub$EventSummary)
            event_data_clusters_sub$EventSummary <- gsub("\\\r\n  |\\\n  ",", ",event_data_clusters_sub$EventSummary)
          } else {
            new_col <- unname(sapply(event_data_clusters_sub$EventSummary, function(y) {
              # breakdown summary column into parts
              check_splt <- strsplit(y,"\\\r\n|\\\n")[[1]]
              if (length(check_splt) > 1) {
                # Look for summary event header but checking for portion without leading space
                headers <- grep("^\\s+",check_splt,invert = T)
                # if there is more than one event type in the summary
                if (length(headers) > 1) {
                  new_col <- paste0(sapply(seq_along(headers), function(i) {
                    header <- check_splt[headers[i]]
                    start <- headers[i] + 1
                    end <- if (i < length(headers)) headers[i+1] - 1 else length(check_splt)
                    items <- trimws(check_splt[start:end])
                    items_joined <- paste0(items, collapse = ", ")
                    result_parts <- paste0(header, ": ", items_joined)
                    return(result_parts)
                  }), collapse = " - ")
                } else {
                  # event summary only contains one event type
                  # remove comma's with space from original test, to differentiate from edited version
                  check_splt_tocollapse <- gsub(", ",",",check_splt[-headers])
                  new_col <- trimws(paste0(check_splt_tocollapse, collapse = ", "))
                }
              } else {
                check_splt_tocollapse <- gsub(", ",",",check_splt)
                new_col <- trimws(paste0(check_splt_tocollapse, collapse = ", "))
              }
            }))
            event_data_clusters_sub$EventSummary <- new_col
          }
          return(event_data_clusters_sub)
        })
        event_data_clusters_clean <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE),
                                            event_data_clusters_clean)
        sankey_added <- unique(paste0(event_data_clusters_clean$Event,": ",event_data_clusters_clean$EventSummary))
        #sankey_added_events(sankey_added)

        # next update
        event_data_tr_clusters_clean <- event_data_clusters_clean
        event_data <- Patient_Event_Data_cls_all
        event_data_clusters_revent <- event_data_tr_clusters_clean %>%
          mutate(EventType = Event) %>%
          mutate(Event = paste0(Event,": ",EventSummary)) %>%
          select(Name,Event,EventType,EventStart,EventEnd)
        #sankey_added_events(c(sankey_added_events,unique(event_data_clusters_revent$Event)))
        event_data2 <- as.data.frame(rbindlist(list(event_data,event_data_clusters_revent),fill = T))
        event_data3 <- event_data2 %>%
          group_by(!!sym(colnames(event_data2)[1])) %>%
          arrange(Event %in% unique(event_data2$Event), .by_group = TRUE) %>%
          as.data.frame()
        #event_data(event_data3)


        event_data_summ(Patient_Event_Data_cls_all)
        event_data_tr_clusters_clean(event_data_clusters_clean)
        sankey_added_events(sankey_added)
        event_data(event_data3)

      }, ignoreInit = TRUE)


      sankey_added_events <- reactiveVal()
      observe({
        #print("1805")
        req(event_data_summ())
        req(paramEvent_data())
        event_data <- event_data_summ()
        param <- paramEvent_data()

        #save(list = ls(), file = "event_data_tr_clusters_clean_making.RData", envir = environment())

        treatment_events <- param[which(param$Treatment == TRUE),"Event Category"]
        if (length(unique(treatment_events)) > 0) {
          treat_summ_events <- paste0(unique(param[which(param$Treatment == TRUE),"Event Category"])," Summary")
          event_data_treat_summ <- event_data[which(event_data$EventType %in% c(treat_summ_events,"Full Treatment Summary")),]
          event_data_treat_summ$EventType <- ifelse(event_data_treat_summ$EventType == "Full Treatment Summary","Full Treatment Summary",
                                                    gsub(" Summary$","",event_data_treat_summ$EventType))
          # Assign numeric cluster line treatments
          event_data_clusters <- event_data_treat_summ %>%
            mutate(Event = EventType) %>%
            group_by(Name,Event) %>%
            arrange(EventStart,EventEnd, .by_group = TRUE) %>%
            mutate(Treatment_Line_Cluster = paste0(unique(Event)," Cluster Line ",seq(n()))) %>%
            mutate(Event = Treatment_Line_Cluster) %>%
            select(-c(EventTab,EventColumn)) %>%
            as.data.frame()
          event_data_clusters$EventType <- ifelse(event_data_clusters$EventType == "Full Treatment Summary","Full Treatment Summary",
                                                  paste0(event_data_clusters$EventType," Cluster"))
          # Clean event summary
          sankey_event_opts <- unique(event_data_clusters$EventType)
          event_data_clusters_clean <- lapply(sankey_event_opts, function(x) {
            event_data_clusters_sub <- event_data_clusters[which(event_data_clusters$EventType == x),]
            x <- ifelse(x == "Full Treatment Summary","Full Treatment Summary",gsub(" Cluster$","",x))
            if (all(grepl(gsub(" Summary$|Summary$","",x),event_data_clusters_sub$EventSummary))) {
              event_data_clusters_sub$EventSummary <- gsub(gsub(" Summary$|Summary$","",x),"",event_data_clusters_sub$EventSummary)
              event_data_clusters_sub$EventSummary <- trimws(event_data_clusters_sub$EventSummary)
              event_data_clusters_sub$EventSummary <- gsub(", ",",",event_data_clusters_sub$EventSummary)
              event_data_clusters_sub$EventSummary <- gsub("\\\r\n  |\\\n  ",", ",event_data_clusters_sub$EventSummary)
            } else {
              new_col <- unname(sapply(event_data_clusters_sub$EventSummary, function(y) {
                # breakdown summary column into parts
                check_splt <- strsplit(y,"\\\r\n|\\\n")[[1]]
                if (length(check_splt) > 1) {
                  # Look for summary event header but checking for portion without leading space
                  headers <- grep("^\\s+",check_splt,invert = T)
                  # if there is more than one event type in the summary
                  if (length(headers) > 1) {
                    new_col <- paste0(sapply(seq_along(headers), function(i) {
                      header <- check_splt[headers[i]]
                      start <- headers[i] + 1
                      end <- if (i < length(headers)) headers[i+1] - 1 else length(check_splt)
                      items <- trimws(check_splt[start:end])
                      items_joined <- paste0(items, collapse = ", ")
                      result_parts <- paste0(header, ": ", items_joined)
                      return(result_parts)
                    }), collapse = " - ")
                  } else {
                    # event summary only contains one event type
                    # remove comma's with space from original test, to differentiate from edited version
                    check_splt_tocollapse <- gsub(", ",",",check_splt[-headers])
                    new_col <- trimws(paste0(check_splt_tocollapse, collapse = ", "))
                  }
                } else {
                  check_splt_tocollapse <- gsub(", ",",",check_splt)
                  new_col <- trimws(paste0(check_splt_tocollapse, collapse = ", "))
                }
              }))
              event_data_clusters_sub$EventSummary <- new_col
            }
            return(event_data_clusters_sub)
          })
          event_data_clusters_clean <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, all = TRUE),
                                              event_data_clusters_clean)
          event_data_tr_clusters_clean(event_data_clusters_clean)

          sankey_added <- unique(paste0(event_data_clusters_clean$Event,": ",event_data_clusters_clean$EventSummary))
          sankey_added_events(sankey_added)
        }

      })




      observe({
        #print("1797")
        req(event_data_summ())
        req(event_data_tr_clusters_clean())
        #req(sankey_clusters_clean_summ_full())
        event_data <- event_data_summ()
        #sankey_added_events <- sankey_added_events()
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        #event_data_clusters_clean <- sankey_clusters_clean_summ_full()
        event_data_clusters_revent <- event_data_tr_clusters_clean %>%
          mutate(EventType = Event) %>%
          mutate(Event = paste0(Event,": ",EventSummary)) %>%
          select(Name,Event,EventType,EventStart,EventEnd)
        #sankey_added_events(c(sankey_added_events,unique(event_data_clusters_revent$Event)))
        event_data2 <- as.data.frame(rbindlist(list(event_data,event_data_clusters_revent),fill = T))
        event_data3 <- event_data2 %>%
          group_by(!!sym(colnames(event_data2)[1])) %>%
          arrange(Event %in% unique(event_data2$Event), .by_group = TRUE) %>%
          as.data.frame()
        event_data(event_data3)
      })

      event_data_key <- reactive({
        #print("1818")
        #req(Patient_List_React())
        req(paramEvent_data())
        req(event_data())
        #pat_list <- Patient_List_React()
        paramEvent_data <- paramEvent_data()
        event_data <- event_data()
        added_events <- added_events()
        sankey_added_events <- sankey_added_events()
        #save(list = ls(), file = "event_data_key.RData", envir = environment())
        events_exp <- unique(paramEvent_data[which(paramEvent_data[,"Column Defined Event"] == TRUE),"Event Name"])
        event_data_key <- event_data %>%
          #filter(Name %in% pat_list) %>%
          select(Event,EventType,EventTab,EventColumn) %>%
          unique() %>%
          mutate(EventLabel = case_when(
            Event %in% sankey_added_events ~ EventType,
            Event %in% added_events ~ paste0(EventTab,": Cut-Points"),
            !is.na(EventTab) & !is.na(EventColumn) ~ paste0(EventTab,": ",EventColumn),
            is.na(EventTab) ~ EventType
          )) %>%
          #mutate(EventLabel = ifelse(!is.na(EventTab),paste0(EventTab,": ",EventColumn), Event)) %>%
          #mutate(EventExpanded = ifelse(Event %in% events_exp | Event %in% sankey_added_events | Event %in% added_events,TRUE,FALSE)) %>%
          mutate(EventExpanded = ifelse(EventColumn %in% events_exp | Event %in% sankey_added_events | Event %in% added_events,TRUE,FALSE)) %>%
          as.data.frame()
        event_data_key$EventSpecified <- str_split_fixed(event_data_key$Event,": ",2)[,-1]
        event_data_key$EventSpecified[event_data_key$EventSpecified == ""] <- NA
        event_data_key$EventSpecified <- ifelse(is.na(event_data_key$EventSpecified) & event_data_key$EventExpanded == TRUE,event_data_key$Event,event_data_key$EventSpecified)
        event_data_key$EventSpecified <- ifelse(event_data_key$Event %in% added_events,event_data_key$Event,event_data_key$EventSpecified)
        event_data_key$EventSpecified <- ifelse(event_data_key$Event %in% sankey_added_events,event_data_key$Event,event_data_key$EventSpecified)
        rownames(event_data_key) <- NULL
        event_data_key
      })

      #observe({
      #  Patient_Table_df <- Patient_Table_React()
      #  event_data_raw <- event_data_raw()
      #  event_data_summ <- event_data_summ()
      #  event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
      #  Patient_Event_Data <- event_data()
      #  Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
      #  #event_data_clusters_clean <- sankey_clusters_clean_summ_full()
      #  event_data_clusters <- event_data_tr_clusters_clean()
      #  added_events <- added_events()
      #  sankey_added_events <- sankey_added_events()
      #  wkbk <- wkbk_react()
      #  param <- param_data()
      #  save(list = ls(), file = "shiny_check2.RData", envir = environment())
      #})

      ## Render UI ----------------------------------------------------
      output$rendPreProcessingTabs <- renderUI({
        if (isTruthy(input$EventDataFileInput) | input$LoadExampleData > 0) {
          tabsetPanel(id = "PreProcessingTabs",
                      # Format input data
                      tabPanel("Input Data Formatting",
                               # Event data input
                               conditionalPanel(condition = "input.DataInputEventFormat == 'Yes'",
                                                conditionalPanel(condition = "output.EventDataFileIn_found",
                                                                 wellPanel(
                                                                   fluidRow(
                                                                     column(4,
                                                                            h3("Select Required Event Data Columns:")#,
                                                                     ),
                                                                     column(3,
                                                                            selectizeInput("EventDataPatientIDcol","Patient ID Column", choices = NULL, selected = 1),
                                                                            selectizeInput("EventDataEventcol","Event Name Column", choices = NULL, selected = 1)#,
                                                                     ),
                                                                     column(3,
                                                                            selectizeInput("EventDataEventStartcol","Event Start Time Column", choices = NULL, selected = 1),
                                                                            selectizeInput("EventDataEventEndcol","Event End Time Column", choices = NULL, selected = 1)
                                                                     ),
                                                                     column(2,
                                                                            selectizeInput("EventDataEventStartUnits","Start Time Units", choices = c("Days","Months","Years","Hours")),
                                                                            selectizeInput("EventDataEventEndUnits","End Time Units", choices = c("Days","Months","Years","Hours"))
                                                                     )
                                                                   ),
                                                                   hr(),
                                                                   h4("Optional Event Data Columns:"),
                                                                   fluidRow(
                                                                     column(4,
                                                                            selectizeInput("EventDataEventTypecol","Event Category/Type Column", choices = NULL, selected = 1,
                                                                                           options = list(
                                                                                             placeholder = 'Please select',
                                                                                             onInitialize = I('function() { this.setValue(""); }')
                                                                                           )),
                                                                            selectizeInput("EventDataEventSummary","Event Summary Column", choices = NULL, selected = 1,
                                                                                           options = list(
                                                                                             placeholder = 'Please select',
                                                                                             onInitialize = I('function() { this.setValue(""); }')
                                                                                           ))
                                                                     ),
                                                                     column(4,
                                                                            virtualSelectInput(
                                                                              inputId = "EventDataTreatmentEvents",
                                                                              label = "Select Treatment Defining Events:",
                                                                              choices = NULL,
                                                                              showValueAsTags = TRUE,
                                                                              search = TRUE,
                                                                              multiple = TRUE
                                                                            )
                                                                     ),
                                                                     column(4,
                                                                            virtualSelectInput(
                                                                              inputId = "EventDataResponseEvents",
                                                                              label = "Select Response Defining Events:",
                                                                              choices = NULL,
                                                                              showValueAsTags = TRUE,
                                                                              search = TRUE,
                                                                              multiple = TRUE
                                                                            )
                                                                     )
                                                                   )
                                                                 ),
                                                                 p(),
                                                                 h3("Event Data Preivew"),
                                                                 div(DT::dataTableOutput("EventDataInputPreview"), style = "font-size:12px")
                                                )
                               ),
                               # Input or generate parameter file
                               conditionalPanel(condition = "input.DataInputEventFormat == 'No'",
                               ),
                               value = "Input Data Formatting"
                      ))
        } else {
          tabsetPanel(id = "PreProcessingTabs")
        }

      })

      inserted_tabs <- reactiveVal(character())
      observeEvent(wkbk_react_sub(), {
        wkbk <- wkbk_react_sub()
        new_tab_names <- c("Event Parameters", names(wkbk))

        lapply(inserted_tabs(), function(tab_name) {
          removeTab(inputId = "PreProcessingTabs", target = tab_name)
        })

        lapply(new_tab_names, function(tab_name) {
          insertTab(inputId = "PreProcessingTabs",
                    tab = tabPanel(tab_name,
                                   p(),
                                   div(DT::dataTableOutput(paste0(tab_name, "_WkbkTable")), style = "font-size:12px"),
                                   downloadButton(paste0(tab_name, "_WkbkTableDnld"), "Download Table")
                    ),
                    target = "Input Data Formatting",
                    position = "after",
                    select = FALSE
          )
        })

        # Optionally insert Event Data tab
        if (isTruthy(event_data())) {
          insertTab(inputId = "PreProcessingTabs",
                    tab = tabPanel("Event Data",
                                   p(),
                                   div(DT::dataTableOutput("EventDataTable"), style = "font-size:12px"),
                                   downloadButton("EventDataTable_dnld", "Download Table")
                    ),
                    target = "Input Data Formatting",
                    position = "after",
                    select = FALSE
          )
          new_tab_names <- c(new_tab_names, "Event Data")
        }
        inserted_tabs(new_tab_names)

      })


      #output$rendWorkbookTabs <- renderUI({
      #  req(wkbk_react_sub())
      #  wkbk <- wkbk_react_sub()
      #  tabs <- c("Event Parameters",names(wkbk))
      #  myTabs <- lapply(1:length(tabs), function(x){
      #    tabPanel(paste(tabs[x]),
      #             p(),
      #             div(DT::dataTableOutput(paste0(tabs[x],"_WkbkTable")), style = "font-size:12px"),
      #             downloadButton(paste0(tabs[x],"_WkbkTableDnld"), "Download Table"),
      #             value = x
      #    )
      #  })
      #  EventDataTab <- tabPanel("Event Data",
      #                           p(),
      #                           div(DT::dataTableOutput("EventDataTable"), style = "font-size:12px"),
      #                           downloadButton("EventDataTable_dnld", "Download Table"),
      #                           value = (length(tabs)+1)
      #  )
      #  if (isTruthy(event_data())) {
      #    myTabs <- append(myTabs, list(EventDataTab), 1)
      #  }
      #
      #  do.call(tabsetPanel, c(id = "DataInputMain",myTabs))
      #})


      output$EventDataTable <- DT::renderDataTable({
        event_data <- event_data()
        #save(list = ls(), file = "event.RData", envir = environment())
        DT::datatable(event_data,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 20,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      rownames = F
        )
      })

      ## Generate Patient Clinical data tables
      shiny::observe({
        req(wkbk_react_sub())
        #req(wkbk_raw_react())
        #wkbk <- wkbk_raw_react()
        wkbk <- wkbk_react_sub()

        lapply(names(wkbk), function(i) {
          output[[paste0(i,"_WkbkTable")]] <- DT::renderDataTable({
            df <- wkbk[[i]]
            DT::datatable(df,
                          escape = F,
                          class = "display nowrap",
                          extensions = 'ColReorder',
                          options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                         pageLength = 20,
                                         scrollX = T,
                                         target = "cell",
                                         colReorder = TRUE),
                          rownames = F
            )
          })
        })
        #updateTabsetPanel(session = session,"DataInputMain",selected = "2")
        #updateTabsetPanel(session = session,"DataInputMain")
        updateTabsetPanel(session = session,"PreProcessingTabs")
      })

      output[["Event Parameters_WkbkTable"]] <- DT::renderDataTable({
        param_data <- param_data()
        DT::datatable(param_data,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 20,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      rownames = F
        )
      })

      #observe({
      #  req(paramEvent_data())
      #  param_events <- paramEvent_data()
      #  #tte_choices <- paste0(param_events[which(param_events$Treatment == TRUE | param_events$Response == TRUE),"Data Table Name"],
      #  #                      ": ",
      #  #                      param_events[which(param_events$Treatment == TRUE | param_events$Response == TRUE),"Event Name"])
      #  #
      #  tte_choices <- paste0(param_events[,"Data Table Name"],
      #                        ": ",
      #                        param_events[,"Event Name"])
      #
      #  tte_choices_react(tte_choices)
      #})

      ## Patient Anno ----------------------------------------------------------

      observe({
        if (isTruthy(PatientAnno_file_react())) {
          patAnno <- as.data.frame(fread(PatientAnno_file_react()))
          pat_react(patAnno)
        } else {
          req(event_data_raw())

          event_data <- event_data_raw()
          patAnno <- event_count_df(event_data)
          pat_react(patAnno)

          #eventTypeOpts <- unique(event_data$EventType)
          #event_data_yor <- event_data %>%
          #  group_by(!!sym(colnames(event_data)[1])) %>%
          #  mutate(Years_On_Record = round((max(EventEnd)-min(EventStart)),3))
          #event_data_stats <- event_data_yor %>%
          #  group_by(!!sym(colnames(event_data)[1]),EventType) %>%
          #  mutate(Time_Points = length(EventType),
          #         row_index = row_number()) %>%
          #  ungroup() %>%
          #  pivot_wider(
          #    id_cols = c(!!sym(colnames(event_data)[1]), row_index),
          #    names_from = EventType,
          #    values_from = Time_Points
          #  ) %>%
          #  arrange(!!sym(colnames(event_data)[1]), row_index) %>%
          #  select(-row_index) %>%
          #  arrange(rowSums(is.na(.))) %>%
          #  distinct(!!sym(colnames(event_data)[1]), .keep_all = TRUE)
          #colnames(event_data_stats)[-1] <- ifelse(grepl("time|point",colnames(event_data_stats)[-1],ignore.case = T),
          #                                         colnames(event_data_stats)[-1],
          #                                         paste0(colnames(event_data_stats)[-1]," Time Points"))
          #event_data_stats_exp <- merge(unique(event_data_yor[,c(1,ncol(event_data_yor))]),event_data_stats, all = T)
          #colnames(event_data_stats_exp) <- gsub("_"," ",colnames(event_data_stats_exp))
          #patAnno <- event_data_stats_exp
          #patAnno <- unique(event_data()[,1,drop = F])
          #pat_react(patAnno)
        }
      })



      # Append Tabs ------------------------------------------------------------
      #observe({
      #req(pat_react())
      #if (nrow(pat_react()) > 1) {
      #appendTab(inputId = "PatientMainPanel",
      #          tabPanel("Treatment Clustering",
      #                   p(),
      #                   tabsetPanel(id = "sankeyPanel",
      #                               tabPanel("Sankey",
      #                                        p(),
      #                                        shinyjqui::jqui_resizable(plotOutput("SankeyPlot",height = "700px", width = "100%")),
      #                                        p(),
      #                                        fluidRow(
      #                                          column(1,
      #                                                 downloadButton("dnldSankeyPlot","SVG")
      #                                          ),
      #                                          column(2,
      #                                                 downloadButton("dnldSankeyTable","Download Sankey Plot Table")
      #                                          )
      #                                        ),
      #                                        p(),
      #                                        div(DT::dataTableOutput("SankeyPlotTable"), style = "font-size:14px"),
      #                                        downloadButton("dnldSankeyPlotTable", "Download Table"),
      #                                        value = 1
      #                               ),
      #                               tabPanel("Heatmap",
      #                                        p(),
      #                                        shinyjqui::jqui_resizable(plotOutput("TreatClusterHeatmap",height = "800px", width = "100%", hover = "heatmap_hover")),
      #                                        p(),
      #                                        downloadButton("dnldTreatClusterHeatmap","SVG"),
      #                                        value = 2
      #                               )
      #                   ),
      #                   value = 2
      #          )
      #)
      #appendTab(inputId = "PatientMainPanel",
      #          tabPanel("Event Duration Distribution",
      #                   p(),
      #                   tabsetPanel(id = "EventDistTabs",
      #                               tabPanel("Box Plot",
      #                                        p(),
      #                                        shinyjqui::jqui_resizable(plotlyOutput("EventBoxPlot",height = "600px", width = "100%")),
      #                                        div(DT::dataTableOutput("EventBoxPlotTable"), style = "font-size:14px"),
      #                                        downloadButton("dnldEventBoxPlotTable", "Download Table"),
      #                                        value = 1
      #                               ),
      #                               tabPanel("Heatmap",
      #                                        p(),
      #                                        shinyjqui::jqui_resizable(plotOutput("TreatDistHeatmap",height = "800px", width = "100%", hover = "heatmapDist_hover")),
      #                                        p(),
      #                                        downloadButton("dnldTreatDistHeatmap", "SVG"),
      #                                        value = 2
      #                               ),
      #                               tabPanel("Swimmers Plot",
      #                                        p(),
      #                                        uiOutput("rendDurationSwimmers"),
      #                                        #shinyjqui::jqui_resizable(plotlyOutput("DurationSwimmers",height = "600px", width = "100%")),
      #                                        #div(DT::dataTableOutput("DurationSwimmersTable"), style = "font-size:14px"),
      #                                        #downloadButton("dnldDurationSwimmersTable", "Download Table"),
      #                                        value = 3
      #                               )
      #                   ),
      #                   value = 3
      #          )
      #)
      #appendTab(inputId = "PatientMainPanel",
      #          tabPanel("Time-To-Event",
      #                   p(),
      #                   span(textOutput("NoPatientsMatchError"), style="color:red"),
      #                   tabsetPanel(id = "TTEplots",
      #                               tabPanel("Kaplan Meier",
      #                                        p(),
      #                                        fluidRow(
      #                                          column(8,
      #                                                 shinyjqui::jqui_resizable(plotOutput("KPplot",height = "500",width = "100%"))
      #                                          ),
      #                                          column(4,
      #                                                 shinycssloaders::withSpinner(tableOutput("KPplotHRtab"), type = 7, size = 0.5),
      #                                                 verbatimTextOutput("KPplotSummary"))
      #                                        ),
      #                                        downloadButton("dnldKPplot", "SVG"),
      #                                        p(),
      #                                        div(DT::dataTableOutput("KPplotTable"), style = "font-size:14px"),
      #                                        downloadButton("dnldKPplotTable", "Download Table"),
      #                                        value = 2
      #                               ),
      #                               tabPanel("Swimmers Plot",
      #                                        p(),
      #                                        shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("swimmer_plot",height = "600px", width = "100%")), type = 6),
      #                                        p(),
      #                                        div(DT::dataTableOutput("swimmer_plotTable"), style = "font-size:14px"),
      #                                        downloadButton("dnldswimmer_plotTable", "Download Table"),
      #                                        value = 1
      #                               ),
      #                               tabPanel("Power Analysis",
      #                                        p(),
      #                                        shinyjqui::jqui_resizable(plotOutput(outputId = "powerCTAnalysis_steps",height = "500",width = "100%")),
      #                                        downloadButton("dnldpowerCTAnalysis"),
      #                                        p(),
      #                                        verbatimTextOutput("powerCTAnalysis_report"),
      #                                        value = 3)
      #                   ),
      #                   value = 4)
      #)
      #  }
      #})

      # Patient Analysis -------------------------------------------------------
      ## Swim --------------------------------------------------
      ## Render UI

      shiny::observe({

        Clin_Supp_Cols_List_2 <- Filter(Negate(anyNA), Clin_Supp_Cols_List_react())
        Clin_Supp_Cols_List_mod <- setNames(lapply(names(Clin_Supp_Cols_List_2), function(x){
          setNames(paste0(x, "_", Clin_Supp_Cols_List_2[[x]]), Clin_Supp_Cols_List_2[[x]])
        }), names(Clin_Supp_Cols_List_2))

        preSelectedGet <- c("Diagnosis_Histology","Diagnosis_ClinTStage","Diagnosis_ClinNStage","Diagnosis_ClinMStage",
                            "Diagnosis_PathTStage","Diagnosis_PathNStage","Diagnosis_PathMStage",
                            "Imaging_EvidenceOfLesion","Imaging_ImageLesionGrowth","Imaging_ImageLesionSiteDesc",
                            "Radiation_RadModality","Radiation_ExtBeamTechnique","Radiation_RadDose","Radiation_RadFractions",
                            "Surgery Biopsy_SurgeryBiopsyLocation","Surgery Biopsy_SiteDiagnostic","Surgery Biopsy_SiteTherapeutic","Surgery Biopsy_SitePalliative",
                            "Tumor Marker_TMarkerTest","Tumor Marker_TMarkerResultValue",
                            "Physical Assessment_BodyHeight","Physical Assessment_BodyWeight","Physical Assessment_BMI")

        updateSelectizeInput(session = session, inputId = "SwimmerHover",
                             choices = Clin_Supp_Cols_List_mod,
                             selected = preSelectedGet,
                             server = T)

        updateSelectizeInput(session = session, inputId = "SwimmerHoverTTE",
                             choices = Clin_Supp_Cols_List_mod,
                             selected = preSelectedGet,
                             server = T)

      })


      observe({
        req(event_data_single_patient())
        Patient_Event_Data_sub <- event_data_single_patient()
        displaySumm <- input$displaySummaryRows
        if (!displaySumm) {
          Patient_Event_Data_sub <- Patient_Event_Data_sub[which(!is.na(Patient_Event_Data_sub$EventTab) & !grepl("Summary$",Patient_Event_Data_sub$Event)),]
        }
        events <- unname(apply(Patient_Event_Data_sub[,c("Event","EventStart","EventEnd")],1,function(x) {
          if (is.na(unname(x[2])) | is.na(unname(x[3]))) {
            paste0(unname(x[1])," - ",sum(as.numeric(unname(x[2])),as.numeric(unname(x[3])), na.rm = T))
          } else {
            if (unname(x[2]) == unname(x[3])) {
              paste0(unname(x[1])," - ",unname(x[2]))
            } else {
              paste0(unname(x[1])," - ",unname(x[2]),"-",unname(x[3]))
            }
          }
        }))
        Selected <- ifelse(any(grepl("Metastatsis",events, ignore.case = T)),grep("Metastatsis",events,value = T, ignore.case = T),"")
        updateSelectizeInput(session,"HighlightEvent",choices = events, selected = Selected)
      })

      ## Render tabset panel below timeline
      output$rendTimelineTableTabs <- renderUI({

        tabs <- c("Patient Events",names(wkbk_react_sub()))

        myTabs <- lapply(1:length(tabs), function(x){
          tabPanel(paste(tabs[x]),
                   p(),
                   div(DT::dataTableOutput(paste0(tabs[x],"_Table")), style = "font-size:14px"),
                   downloadButton(paste0(tabs[x],"_TableDnld"), "Download Table")
          )
        })

        do.call(tabsetPanel, myTabs)

      })

      ## timeline plot customization
      output$rendTimeLineTitle <- renderUI({

        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- Patient_Table_React()
        Patient <- Patient_Table_df[Patient_Row_Selec,1]
        SwimmerTitle <- paste0("Clinical Course of Patient: ", as.character(Patient))
        textInput("TimeLineTitle","Title:",value = SwimmerTitle)

      })
      output$rendTimeLineXTitle <- renderUI({

        textInput("TimeLineXTitle","X-Axis Title:",value = paste("Age (years)"))

      })

      ## Reactive


      ## Generate Patient Clinical data tables
      shiny::observe({
        req(PatientTimelinePlot_df())
        pat_table <- PatientTimelinePlot_df()
        pat_table <- pat_table %>%
          relocate(any_of(c("highlight","EventTab")), .after = EventEnd)
        Clin_Supp_List <- c(list(`Patient Events` = pat_table),
                            wkbk_react_sub())

        lapply(names(Clin_Supp_List), function(i) {
          #lapply(names(Clin_Supp_List_react()), function(i) {
          output[[paste0(i,"_Table")]] <- DT::renderDataTable({
            Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
            Patient_Table_df <- Patient_Table_React()
            Patient <- Patient_Table_df[Patient_Row_Selec,1]
            #df <- Clin_Supp_List_react()[[i]]
            df <- Clin_Supp_List[[i]]
            df <- df[which(df[,1] == Patient),]
            if (i == "Variant Summary") {
              if (ncol(df) > 3000) {
                df <- df[1:(length(df)-3000)]
              }
            }
            DT::datatable(df,
                          escape = F,
                          class = "display nowrap",
                          extensions = 'ColReorder',
                          options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                         pageLength = 100,
                                         scrollX = T,
                                         target = "cell",
                                         colReorder = TRUE),
                          rownames = F
            )
          })
        })
      })



      ## Generate updated patient annotation table for UI
      Patient_Table_React <- reactive({

        #Patient_List_updated <- Patient_List_React()
        Patient_Annotation <- pat_react()
        #Patient_Annotation <- Patient_Annotation[which(Patient_Annotation[,1] %in% Patient_List_updated),,drop = F]
        Patient_Annotation

      })

      ## Render Patient Selection Table
      output$PatientSelectionTab <- DT::renderDataTable({

        Patient_Table_df <- Patient_Table_React()
        if (nrow(Patient_Table_df) > 10) {
          DT::datatable(Patient_Table_df,
                        extensions = 'Scroller',
                        options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                       pageLength = 20,
                                       scrollX = T,
                                       scrollY = 400),
                        selection = list(mode = 'single', selected = 1),
                        rownames = F
          )
        } else {
          DT::datatable(Patient_Table_df,
                        extensions = 'Scroller',
                        options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                       scrollX = T
                        ),
                        selection = list(mode = 'single', selected = 1),
                        rownames = F
          )
        }

      })

      ## Plot

      event_data_single_patient <- reactive({
        if (!is.null(input$PatientSelectionTab_rows_selected)) {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient_Event_Data <- event_data()
          sankey_added_events <- sankey_added_events()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          if (Patient %in% Patient_Table_df[,1]) {
            Patient_Event_Data_sub <- Patient_Event_Data[which(Patient_Event_Data[,1] == Patient),]
            Patient_Event_Data_sub <- Patient_Event_Data_sub[which(!Patient_Event_Data_sub$Event %in% sankey_added_events),]
            Patient_Event_Data_sub
          }
        }
      })

      event_data_single_patient_hover <- reactive({
        req(param_data())
        req(event_data_single_patient())
        req(wkbk_react_sub())
        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- Patient_Table_React()
        Patient <- Patient_Table_df[Patient_Row_Selec,1]
        wkbk <- wkbk_react_sub()
        param <- param_data()
        hoverCols <- input$SwimmerHover
        Patient_Event_Data_sub <- event_data_single_patient()
        event_order <- Patient_Event_Data_sub
        #save(list = ls(), file = "event_data_single_patient_hover.RData", envir = environment())
        if (nrow(Patient_Event_Data_sub) > 0) {
          if (isTruthy(hoverCols)) {
            for (col in hoverCols) {
              colAnno_dfName <- paste(strsplit(col,"_")[[1]][1], collapse = "_")
              colAnno <- paste(strsplit(col,"_")[[1]][-1], collapse = "_")
              anno_df <- wkbk[[grep(colAnno_dfName,names(wkbk),ignore.case = T)]]
              anno_df <- anno_df[which(anno_df[,1] == Patient),]
              startCol <- param[which(param[,1] == colAnno_dfName),6]
              startCol <- startCol[!is.na(startCol)]
              stopCol <- param[which(param[,1] == colAnno_dfName),7]
              stopCol[which(is.na(stopCol))] <- startCol[which(is.na(stopCol))]
              EventTab <- unique(param[which(param[,1] == colAnno_dfName),1])

              startCol <- unique(startCol)
              stopCol <- unique(stopCol)

              if (!all(is.na(startCol)) & nrow(anno_df) > 0) {
                for (timeCol in seq(startCol)) {
                  startCol_name <- startCol[timeCol]
                  #param_tab <- param[which(param[,1] == colAnno_dfName & param[,6] == startCol_name),]
                  # If event is expanded
                  if (unique(param[which(param[,1] == colAnno_dfName & param[,6] == startCol_name),4])) {
                    temp_df <- anno_df %>%
                      select(any_of(c(colnames(anno_df)[1],startCol[timeCol],stopCol[timeCol],colAnno))) %>%
                      unique() %>%
                      mutate(EventTab = EventTab) %>%
                      rename(any_of(c(EventStart = startCol[timeCol], EventEnd = stopCol[timeCol], Name = colnames(anno_df)[1]))) %>%
                      as.data.frame()
                    if (!"EventEnd" %in% colnames(temp_df) & "EventStart" %in% colnames(temp_df)) {
                      temp_df[,"EventEnd"] <- temp_df[,"EventStart"]
                    }
                    Patient_Event_Data_sub <- merge(Patient_Event_Data_sub,temp_df, all.x = T, sort = F)
                  } else {
                    # If event is simple - not expanded
                    temp_df <- anno_df %>%
                      select(any_of(c(colnames(anno_df)[1],startCol[timeCol],stopCol[timeCol],colAnno))) %>%
                      unique() %>%
                      mutate(EventTab = EventTab) %>%
                      rename(any_of(c(EventStart = startCol[timeCol], EventEnd = stopCol[timeCol], Name = colnames(anno_df)[1]))) %>%
                      as.data.frame()
                    if (nrow(temp_df) > 1) {
                      if (!"EventEnd" %in% colnames(temp_df) & "EventStart" %in% colnames(temp_df)) {
                        temp_df[,"EventEnd"] <- temp_df[,"EventStart"]
                      }
                      Patient_Event_Data_sub <- merge(Patient_Event_Data_sub,temp_df, all.x = T, sort = F)
                    } else {
                      temp_df <- temp_df %>%
                        select(-any_of(c("EventStart","EventEnd"))) %>%
                        as.data.frame()
                      Patient_Event_Data_sub <- merge(Patient_Event_Data_sub,temp_df, all.x = T, sort = F)
                    }
                  }

                }
              } else {
                annos <- paste(unique(anno_df[,colAnno]), collapse = " - ")
                Patient_Event_Data_sub[,colAnno] <- annos
              }
            }
          }
          Patient_Event_Data_sub <- merge(event_order,Patient_Event_Data_sub,all.x = T, sort = F)
          Patient_Event_Data_sub
        }

      })

      ## Generate Patient timeline plot
      PatientTimelinePlot_df <- reactive({
        req(event_data_single_patient_hover())
        EventsToHighlight <- input$HighlightEvent
        Patient_Event_Data_sub <- event_data_single_patient_hover()
        #save(list = ls(), file = "PatientTimelinePlot_df.RData", envir = environment())
        if (length(EventsToHighlight) > 0) {
          event_names <- unname(sapply(EventsToHighlight,function(x) head(strsplit(x," - ")[[1]], -1)))
          event_times_start <- as.numeric(unname(sapply(EventsToHighlight,function(x) strsplit(tail(strsplit(x," - ")[[1]], 1),"-")[[1]][1])))
          event_times_stop <- as.numeric(unname(sapply(EventsToHighlight,function(x) strsplit(tail(strsplit(x," - ")[[1]], 1),"-")[[1]][2])))
          Patient_Event_Data_sub$tempCol <- unname(apply(Patient_Event_Data_sub[,c("Event","EventStart","EventEnd")],1,function(x) {
            if (is.na(unname(x[2])) | is.na(unname(x[3]))) {
              paste0(unname(x[1])," - ",sum(as.numeric(unname(x[2])),as.numeric(unname(x[3])), na.rm = T))
            } else {
              if (unname(x[2]) == unname(x[3])) {
                paste0(unname(x[1])," - ",unname(x[2]))
              } else {
                paste0(unname(x[1])," - ",unname(x[2]),"-",unname(x[3]))
              }
            }
          }))
          Patient_Event_Data_sub$highlight <- ifelse(Patient_Event_Data_sub$tempCol %in% EventsToHighlight,TRUE,FALSE)
          Patient_Event_Data_sub <- Patient_Event_Data_sub %>%
            select(-tempCol) %>%
            as.data.frame()
        }
        Patient_Event_Data_sub
      })

      observe({
        req(event_data_single_patient())
        Patient_Event_Data_sub <- event_data_single_patient()
        displaySumm <- input$displaySummaryRows
        #print(head(Patient_Event_Data_sub))
        if (!displaySumm) {
          Patient_Event_Data_sub <- Patient_Event_Data_sub[which(!is.na(Patient_Event_Data_sub$EventTab) & !grepl("Summary$",Patient_Event_Data_sub$Event)),]
        }
        #print(head(Patient_Event_Data_sub))
        event_opts <- unique(Patient_Event_Data_sub[,2])
        updateSelectizeInput(session,"SwimmerYlines", choices = event_opts, selected = event_opts, server = T)
      })

      PatientTimelinePlot_react <- reactive({

        if (!is.null(input$PatientSelectionTab_rows_selected)) {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          if (Patient %in% Patient_Table_df[,1]) {
            RemoveUnkNA_opt <- input$RemoveUnknownNA
            SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
            SwimmerTheme <- input$TimeLineTheme
            TitleFont <- input$TimeLineTitleSize
            xFont <- input$TimeLineXAxisSize
            yFont <- input$TimeLineYAxisSize
            eventsY <- input$SwimmerYlines
            #displaySumm <- input$displaySummaryRows
            Patient_Event_Data_sub <- PatientTimelinePlot_df()
            if (isTruthy(eventsY)) {
              Patient_Event_Data_sub <- Patient_Event_Data_sub[which(Patient_Event_Data_sub[,2] %in% eventsY),]
              Patient_Event_Data_sub <- Patient_Event_Data_sub[order(match(Patient_Event_Data_sub[,2], eventsY)),]
            }
            Patient_Event_Data_sub$EventStart <- round(Patient_Event_Data_sub$EventStart,2)
            Patient_Event_Data_sub$EventEnd <- round(Patient_Event_Data_sub$EventEnd,2)
            if (length(eventsY) == length(unique(Patient_Event_Data_sub$Event))) {
              #save(list = ls(), file = "eventhighlight.RData", envir = environment())
              plot2 <- timelinePlot(data = Patient_Event_Data_sub[,-1],event_col = "Event", event_type_col = "EventType",
                                    start_col = "EventStart", stop_col = "EventEnd",unit = "Years", plotly = TRUE,
                                    title_font = TitleFont, x_font = xFont, y_font = yFont,na.rm = RemoveUnkNA_opt,
                                    title = SwimmerTitle_in,svg_name = paste0(gsub(" ","_",Project_Name),"_Patient_",Patient,"_Timeline"),
                                    svg_height = input$TimeLineHeight, svg_width = input$TimeLineWidth, highlight_col = "highlight")
              #save(list = ls(), file = "full_timeline.RData", envir = environment())
              plot2
            }

          }
        }


      })

      ## Render Patient timeline plot
      output$PatientTimelinePlot <- renderPlotly({

        p <- PatientTimelinePlot_react()
        p

      })

      ## Downloads

      output$dnldCohortEventTab <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_",Project_Name),"_Timeline_Event_Data.txt")
        },
        content = function(file) {
          #Patient_Event_Data <- event_data()
          req(cohort_EventSumm_df())
          Patient_Event_Data <- cohort_EventSumm_df()
          Patient_Event_Data <- Patient_Event_Data[,-c(9:11)]
          write.table(Patient_Event_Data,file, sep = '\t', row.names = F)
        }
      )

      output$dnldPatientTimelinePlot <- downloadHandler(
        filename = function() {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          paste0(gsub(" ","_",Project_Name),"_Patient_",Patient,"_Timeline.svg")
        },
        content = function(file) {
          p <- PatientTimelinePlot_react()
          ggsave(file,p,width = input$TimeLineWidth, height = input$TimeLineHeight, units = input$TimeLineUnits)
        }
      )

      output$dnldPatientEventTab <- downloadHandler(
        filename = function() {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          paste0(gsub(" ","_",Project_Name),"_",Patient,"_Timeline_Event_Data.txt")
        },
        content = function(file) {
          df <- PatientTimelinePlot_df()
          df <- apply(df,2,function(x) gsub("^,","",gsub("\n",",",x)))
          write.table(df,file, sep = '\t', row.names = F)
        }
      )

      ## Render download buttons for clinical data tables
      shiny::observe(
        lapply(names(wkbk_react_sub()), function(i) {
          output[[paste0(i,"_TableDnld")]] <- downloadHandler(
            filename = function() {
              Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
              Patient_Table_df <- Patient_Table_React()
              Patient <- Patient_Table_df[Patient_Row_Selec,1]
              paste0(gsub(" ","_",Project_Name),"_",i,"_Patient_",Patient,".txt")
            },
            content = function(file) {
              Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
              Patient_Table_df <- Patient_Table_React()
              Patient <- Patient_Table_df[Patient_Row_Selec,1]
              df <- wkbk_react_sub()[[i]]
              if (Patient %in% df[,1]) {
                df <- df[which(df[,1] == Patient),]
              }
              df <- apply(df,2,function(x) gsub("^,","",gsub("\n",",",x)))
              write.table(df,file,sep = '\t', row.names = F)
            }
          )
        })
      )

      ## Swim Summ --------------------------------------------------------------

      wkbk_react_cutp <- reactiveVal()
      event_data_cutp <- reactiveVal()
      added_events <- reactiveVal()
      observeEvent(input$saveLinePlotAbvCutP, {
        req(wkbk_react_anno())
        req(event_data())
        req(input$linePlotCutP)
        wkbk <- wkbk_react_anno()
        event_data <- event_data()
        added_events <- added_events()
        cutp <- input$linePlotCutP
        cutp_anno <- input$linePlotCutPAnno
        dataTab <- input$LinePlotTable
        tab_sub <- input$LinePlotSub
        tab_crit <- input$LinePlotSubCrit
        xAxis <- input$LinePlotX
        yAxis <- input$LinePlotY
        unitCol <- input$LinePunitCol
        unitSel <- input$LinePunitSelect
        plot_df_in <- wkbk[[dataTab]]
        plot_df_in[,xAxis] <- suppressWarnings(as.numeric(plot_df_in[,xAxis]))
        plot_df_in[,yAxis] <- suppressWarnings(as.numeric(plot_df_in[,yAxis]))
        plot_df <- plot_df_in
        #save(list = ls(), file = "cutp_hi.RData", envir = environment())
        if (tab_sub != "Select all data") {
          plot_df <- plot_df[which(plot_df[,tab_sub] == tab_crit),]
        } else {
          tab_crit <- NULL
        }
        plot_df <- plot_df %>%
          filter(!is.na(!!sym(xAxis))) %>%
          arrange(!!sym(colnames(plot_df)[1]),!!sym(xAxis)) %>%
          as.data.frame()

        if (!isTruthy(cutp_anno)) {
          if (isTruthy(unitCol) & isTruthy(unitSel)) {
            cutpoint_col_name <- paste0(tab_crit," ",unitSel," Above ",cutp)
          } else {
            cutpoint_col_name <- paste0(tab_crit," Above ",cutp)
          }
        } else {
          cutpoint_col_name <- cutp_anno
        }
        plot_df[,cutpoint_col_name] <- ifelse(as.numeric(plot_df[,yAxis]) >= cutp,cutpoint_col_name,NA)
        added_events <- c(added_events,paste0(dataTab,": ",cutpoint_col_name))

        plot_df_new <- merge(plot_df,plot_df_in, all = T)
        plot_df2 <- plot_df %>%
          group_by(!!sym(colnames(plot_df)[1])) %>%
          mutate(id = consecutive_id(!!sym(cutpoint_col_name))) %>%
          group_by(!!sym(colnames(plot_df)[1]),id) %>%
          mutate(EventStart = min(!!sym(xAxis), na.rm = T), EventEnd = max(!!sym(xAxis), na.rm = T),
                 Event = paste0(dataTab,": ",unique(!!sym(cutpoint_col_name))),
                 EventType = unique(!!sym(cutpoint_col_name)),
                 EventTab = dataTab,
                 EventColumn = cutpoint_col_name) %>%
          ungroup() %>%
          select(any_of(c(colnames(plot_df)[1],"Event","EventType","EventTab",
                          "EventStart","EventEnd","EventColumn"))) %>%
          unique()
        plot_df2 <- plot_df2[complete.cases(plot_df2),]
        colnames(plot_df2)[1] <- colnames(event_data)[1]
        event_data2 <- as.data.frame(rbindlist(list(event_data,plot_df2),fill = T))
        event_data3 <- event_data2 %>%
          group_by(!!sym(colnames(event_data2)[1])) %>%
          arrange(Event %in% unique(plot_df2$Event), .by_group = TRUE) %>%
          as.data.frame()

        wkbk[[dataTab]] <- plot_df_new
        wkbk_react_anno(wkbk)
        event_data(event_data3)
        added_events(added_events)

      }, ignoreInit = TRUE)

      observeEvent(input$saveLinePlotBelCutP, {
        req(wkbk_react_anno())
        req(event_data())
        req(input$linePlotCutP)
        wkbk <- wkbk_react_anno()
        event_data <- event_data()
        added_events <- added_events()
        cutp <- input$linePlotCutP
        cutp_anno <- input$linePlotCutPAnno
        dataTab <- input$LinePlotTable
        tab_sub <- input$LinePlotSub
        tab_crit <- input$LinePlotSubCrit
        xAxis <- input$LinePlotX
        yAxis <- input$LinePlotY
        unitCol <- input$LinePunitCol
        unitSel <- input$LinePunitSelect
        plot_df_in <- wkbk[[dataTab]]
        plot_df_in[,xAxis] <- suppressWarnings(as.numeric(plot_df_in[,xAxis]))
        plot_df_in[,yAxis] <- suppressWarnings(as.numeric(plot_df_in[,yAxis]))
        plot_df <- plot_df_in
        #save(list = ls(), file = "cutp_lo.RData", envir = environment())
        if (tab_sub != "Select all data") {
          plot_df <- plot_df[which(plot_df[,tab_sub] == tab_crit),]
        } else {
          tab_crit <- NULL
        }
        plot_df <- plot_df %>%
          filter(!is.na(!!sym(xAxis))) %>%
          arrange(!!sym(colnames(plot_df)[1]),!!sym(xAxis)) %>%
          as.data.frame()

        if (!isTruthy(cutp_anno)) {
          if (isTruthy(unitCol) & isTruthy(unitSel)) {
            cutpoint_col_name <- paste0(tab_crit," ",unitSel," Below ",cutp)
          } else {
            cutpoint_col_name <- paste0(tab_crit," Below ",cutp)
          }
        } else {
          cutpoint_col_name <- cutp_anno
        }
        plot_df[,cutpoint_col_name] <- ifelse(as.numeric(plot_df[,yAxis]) <= cutp,cutpoint_col_name,NA)
        added_events <- c(added_events,cutpoint_col_name)

        plot_df_new <- merge(plot_df,plot_df_in, all = T)
        plot_df2 <- plot_df %>%
          group_by(!!sym(colnames(plot_df)[1])) %>%
          mutate(id = consecutive_id(!!sym(cutpoint_col_name))) %>%
          group_by(!!sym(colnames(plot_df)[1]),id) %>%
          mutate(EventStart = min(!!sym(xAxis), na.rm = T), EventEnd = max(!!sym(xAxis), na.rm = T),
                 Event = unique(!!sym(cutpoint_col_name)),
                 EventType = unique(!!sym(cutpoint_col_name)),
                 EventTab = dataTab,
                 EventColumn = cutpoint_col_name) %>%
          ungroup() %>%
          select(any_of(c(colnames(plot_df)[1],"Event","EventType","EventTab",
                          "EventStart","EventEnd","EventColumn"))) %>%
          unique()
        plot_df2 <- plot_df2[complete.cases(plot_df2),]
        colnames(plot_df2)[1] <- colnames(event_data)[1]
        event_data2 <- as.data.frame(rbindlist(list(event_data,plot_df2),fill = T))
        event_data3 <- event_data2 %>%
          group_by(!!sym(colnames(event_data2)[1])) %>%
          arrange(Event %in% unique(plot_df2$Event), .by_group = TRUE) %>%
          as.data.frame()


        wkbk[[dataTab]] <- plot_df_new
        wkbk_react_anno(wkbk)
        event_data(event_data3)
        added_events(added_events)

      }, ignoreInit = TRUE)


      PatientTimelineSummPlot_df <- reactive({
        req(event_data_single_patient())

        if (!is.null(input$PatientSelectionTab_rows_selected)) {
          pat_event_data <- event_data_single_patient()
          added_events <- added_events()
          sankey_added_events <- sankey_added_events()

          pat_event_data <- pat_event_data[which(is.na(pat_event_data$EventTab) | pat_event_data$Event %in% added_events),]
          pat_event_data <- pat_event_data[which(!pat_event_data$Event %in% sankey_added_events),]
          pat_event_data
        }

      })

      PatientTimelineSummPlot_react <- reactive({

        if (!is.null(input$PatientSelectionTab_rows_selected)) {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          if (Patient %in% Patient_Table_df[,1]) {
            RemoveUnkNA_opt <- input$RemoveUnknownNA
            SwimmerTitle_in <- ifelse(!isTruthy(input$TimeLineTitle),paste0("Clinical Course of Patient: ", as.character(Patient)),input$TimeLineTitle)
            SwimmerTheme <- input$TimeLineTheme
            TitleFont <- input$TimeLineTitleSize
            xFont <- input$TimeLineXAxisSize
            yFont <- input$TimeLineYAxisSize
            #eventsY <- input$SwimmerYlines2
            Patient_Event_Data_sub <- PatientTimelineSummPlot_df()
            Patient_Event_Data_sub$EventStart <- round(Patient_Event_Data_sub$EventStart,2)
            Patient_Event_Data_sub$EventEnd <- round(Patient_Event_Data_sub$EventEnd,2)

            plot2 <- timelinePlot(data = Patient_Event_Data_sub[,-1],event_col = "Event", event_type_col = "EventType",
                                  start_col = "EventStart", stop_col = "EventEnd", unit = "Years", plotly = TRUE,
                                  title_font = TitleFont, x_font = xFont, y_font = yFont,na.rm = RemoveUnkNA_opt,
                                  title = SwimmerTitle_in,svg_name = paste0(gsub(" ","_",Project_Name),"_Patient_",Patient,"_Timeline"),
                                  svg_height = input$TimeLineHeight, svg_width = input$TimeLineWidth, highlight_col = "highlight")
            plot2

          }
        }


      })

      output$PatientTimelineLineSummPlot <- renderPlotly({

        if (!is.null(input$PatientSelectionTab_rows_selected)) {
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          plot_title <- paste0("Clinical Course of Patient: ", as.character(Patient))
        }

        if (isTruthy(input$LinePlotSubCrit)) {
          req(PatientTimelineSummPlot_react())
          req(PatientLinePlot_react())
          xAxis <- input$LinePlotX
          p1 <- PatientTimelineSummPlot_react()
          line_p <- PatientLinePlot_react()
          p1_xlim <- p1$x$layout$xaxis$range
          #p2_xlim <- range(line_p$data$AgeAtLabResults, na.rm = T)
          if (nrow(line_p$data[xAxis]) > 0) {
            p2_xlim <- range(line_p$data[xAxis], na.rm = T)
            line_p <- line_p +
              ggplot2::scale_x_continuous(limits=c(min(c(p1_xlim,p2_xlim), na.rm = T), max(c(p1_xlim,p2_xlim), na.rm = T)))
            p2 <- plotly::ggplotly(line_p)
            subply <- subplot(p1, p2, nrows = 2, shareY = TRUE, shareX = TRUE,
                              titleY = TRUE, titleX = TRUE, which_layout = 1)
            #save(list = ls(), file = "SummaryTimeline_Plot_Env.RData", envir = environment())
            subply <- subply %>%
              layout(margin = list(t = 50))
          }
        } else {
          req(PatientTimelineSummPlot_react())
          p1 <- PatientTimelineSummPlot_react()
          p1
        }
      })



      output$rendSummaryLinePlots <- renderUI({
        if (isTruthy(input$LinePlotSubCrit)) {
          shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("PatientTimelineLineSummPlot",height = "600px", width = "100%")), type = 6)
        } else {
          shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("PatientTimelineLineSummPlot",height = "300px", width = "100%")), type = 6)
        }

      })

      ## Line Plot --------------------------------------------------------------
      observe({
        req(wkbk_react_sub())
        req(input$LinePlotTable)
        dataTab <- input$LinePlotTable
        Clin_Supp_List <- wkbk_react_sub()
        xChoices <- colnames(Clin_Supp_List[[dataTab]])[-1]
        if (grepl("lab",dataTab, ignore.case = T)) {
          PreSelect <- "LabTest"
        } else {
          PreSelect <- "Select all data"
        }
        updateSelectizeInput(session,"LinePlotSub", choices = c("Select all data",xChoices), server = T, selected = PreSelect)
      })

      #lastCrit <- reactiveVal(NULL)
      observe({
        #output$rendLinePlotSubCrit <- renderUI({

        req(input$LinePlotSub)
        req(input$PatientSelectionTab_rows_selected)
        if (input$LinePlotSub != "Select all data") {
          req(wkbk_react_sub())
          req(input$LinePlotTable)
          Clin_Supp_List <- wkbk_react_sub()
          Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
          Patient_Table_df <- Patient_Table_React()
          Patient <- Patient_Table_df[Patient_Row_Selec,1]
          dataTab <- input$LinePlotTable
          df <- Clin_Supp_List[[dataTab]]
          choices <- sort(unique(df[which(df[,1] == Patient),input$LinePlotSub]))
          #save(list = ls(), file = "lineplot.RData", envir = environment())
          #selectInput("LinePlotSubCrit","Subset criteria:",choices = choices, selected = choices[1])
          updateSelectizeInput(session,"LinePlotSubCrit",choices = choices, selected = choices[1], server = T)
        }

      })
      observe({
        req(input$LinePlotSubCrit)
        input$LinePlotSubCrit
        updateNumericInput(session,"linePlotCutP",value = NA)
      })
      #observe({
      #  req(input$LinePlotSubCrit)
      #  lastCrit(input$LinePlotSubCrit)
      #})
      #observe()

      observe({

        req(wkbk_react_sub())
        req(input$LinePlotTable)
        dataTab <- input$LinePlotTable
        Clin_Supp_List <- wkbk_react_sub()
        Choices <- colnames(Clin_Supp_List[[dataTab]])[-1]
        if (input$LinePlotSub != "Select all data") {
          Choices <- Choices[which(Choices!=input$LinePlotSub)]
        }
        unitColPred <- ifelse(any(grepl("unit",Choices,ignore.case = T)),grep("unit",Choices,ignore.case = T, value = T)[1],NA)
        updateSelectizeInput(session,"LinePlotX", choices = Choices, selected = "AgeAtLabResults")
        updateSelectizeInput(session,"LinePlotY", choices = Choices, selected = "LabResults")
        updateSelectizeInput(session,"LinePunitCol", choices = Choices, selected = unitColPred)

      })

      observe({
        req(PatientLinePlot_df())
        req(input$LinePunitCol)
        df <- PatientLinePlot_df()
        yChoices <- unique(df[,input$LinePunitCol])
        updateSelectizeInput(session,"LinePunitSelect",choices = yChoices, selected = yChoices[1])
      })

      #output$rendLinePlotX <- renderUI({
      #
      #  req(wkbk_react())
      #  req(input$LinePlotTable)
      #  dataTab <- input$LinePlotTable
      #  Clin_Supp_List <- wkbk_react()
      #  xChoices <- colnames(Clin_Supp_List[[dataTab]])[-1]
      #  if (input$LinePlotSub != "Select all data") {
      #    xChoices <- xChoices[which(xChoices!=input$LinePlotSub)]
      #  }
      #  selectInput("LinePlotX","X-Axis", choices = xChoices, selected = "AgeAtLabResults")
      #
      #})

      #output$rendLinePlotY <- renderUI({
      #
      #  req(wkbk_react())
      #  req(input$LinePlotTable)
      #  dataTab <- input$LinePlotTable
      #  Clin_Supp_List <- wkbk_react()
      #  yChoices <- colnames(Clin_Supp_List[[dataTab]])[-1]
      #  if (input$LinePlotSub != "Select all data") {
      #    yChoices <- yChoices[which(yChoices!=input$LinePlotSub)]
      #  }
      #  selectInput("LinePlotY","Y-Axis", choices = yChoices, selected = "LabResults")
      #
      #})


      #output$rendLinePunitCol <- renderUI({
      #
      #  req(wkbk_react())
      #  req(input$LinePlotTable)
      #  dataTab <- input$LinePlotTable
      #  Clin_Supp_List <- wkbk_react()
      #  yChoices <- colnames(Clin_Supp_List[[dataTab]])[-1]
      #  if (input$LinePlotSub != "Select all data") {
      #    yChoices <- yChoices[which(yChoices!=input$LinePlotSub)]
      #  }
      #  unitColPred <- ifelse(any(grepl("unit",yChoices,ignore.case = T)),grep("unit",yChoices,ignore.case = T, value = T)[1],NA)
      #  selectInput("LinePunitCol","Y-Axis Units Column", choices = yChoices, selected = unitColPred)
      #
      #})




      PatientLinePlot_df <- reactive({

        req(wkbk_react_sub())
        req(input$LinePlotTable)
        req(input$LinePlotSub)
        req(input$PatientSelectionTab_rows_selected)
        Clin_Supp_List <- wkbk_react_sub()
        param <- param_data()
        dataTab <- input$LinePlotTable
        tab_sub <- input$LinePlotSub
        tab_crit <- input$LinePlotSubCrit
        xAxis <- input$LinePlotX
        yAxis <- input$LinePlotY

        Patient_Row_Selec <- input$PatientSelectionTab_rows_selected
        Patient_Table_df <- Patient_Table_React()
        Patient <- Patient_Table_df[Patient_Row_Selec,1]

        plot_df <- Clin_Supp_List[[dataTab]]

        plot_df <- plot_df[which(plot_df[,1] == Patient),]

        if (tab_sub != "Select all data") {
          plot_df <- plot_df[which(plot_df[,tab_sub] == tab_crit),]
        }
        plot_df <- plot_df %>%
          relocate(any_of(c(colnames(plot_df)[1],tab_crit,xAxis,yAxis)))

        #save(list = ls(), file = "lineplot.RData", envir = environment())


        plot_df

      })

      output$rendLinePlotTitle <- renderUI({

        dataTab <- input$LinePlotTable
        tab_sub <- input$LinePlotSub
        tab_crit <- input$LinePlotSubCrit
        xAxis <- input$LinePlotX
        yAxis <- input$LinePlotY
        plot_df <- PatientLinePlot_df()
        if (tab_sub != "Select all data") {
          plotTitle <- paste0("Line plot of patient ",unique(plot_df[,1])," ",dataTab," - ",tab_crit,"\ndata featuring ",yAxis," over ", xAxis)
        } else {
          plotTitle <- paste0("Line plot of patient ",unique(plot_df[,1])," ",dataTab,"\ndata featuring ",yAxis," over ", xAxis)
        }
        textInput("LinePlotTitle","Title:",value = plotTitle)

      })
      output$rendLinePlotXTitle <- renderUI({

        textInput("LinePlotXTitle","X-Axis Title:",value = input$LinePlotX)

      })
      output$rendLinePlotYTitle <- renderUI({

        textInput("LinePlotYTitle","Y-Axis Title:",value = input$LinePlotY)

      })

      PatientLinePlot_react <- reactive({

        req(PatientLinePlot_df())
        req(input$LinePlotX)
        req(input$LinePlotY)
        plot_df <- PatientLinePlot_df()
        dataTab <- input$LinePlotTable
        tab_sub <- input$LinePlotSub
        tab_crit <- input$LinePlotSubCrit
        xAxis <- input$LinePlotX
        yAxis <- input$LinePlotY
        userCutP <- input$linePlotCutP
        Xaxis_font <- input$LinePlotXAxisSize
        Yaxis_font <- input$LinePlotYAxisSize
        title_font <- input$LinePlotTitleSize
        #Xaxis_title <- input$LinePlotXTitle
        #Yaxis_title <- input$LinePlotYTitle
        #title_title <- input$LinePlotTitle
        unitCol <- input$LinePunitCol
        unitSel <- input$LinePunitSelect

        LinePlotTheme <- input$LinePlotTheme


        if (isTruthy(unitCol) & isTruthy(unitSel)) {
          plot_df <- plot_df[which(plot_df[,unitCol] == unitSel),]
        }

        unit_col <- grep("unit",colnames(plot_df),ignore.case = T, value = T)[1]
        if (isTruthy(unit_col)) {
          plot_df <- plot_df[,c(colnames(plot_df)[1],xAxis,yAxis,unit_col)]
          units <- paste0(unique(plot_df[,unit_col]), collapse = ", ")
          if (tab_sub != "Select all data") {
            plotTitle <- paste0("Line plot of patient ",unique(plot_df[,1])," ",dataTab," - ",tab_crit," (",units,")","\ndata featuring ",yAxis," over ", xAxis)
          } else {
            plotTitle <- paste0("Line plot of patient ",unique(plot_df[,1])," ",dataTab,"\ndata featuring ",yAxis," over ", xAxis)
          }
          yaxlab <- paste0(tab_crit," (",units,")")
        } else {
          plot_df <- plot_df[,c(colnames(plot_df)[1],xAxis,yAxis)]
          if (tab_sub != "Select all data") {
            plotTitle <- paste0("Line plot of patient ",unique(plot_df[,1])," ",dataTab," - ",tab_crit,"\ndata featuring ",yAxis," over ", xAxis)
          } else {
            plotTitle <- paste0("Line plot of patient ",unique(plot_df[,1])," ",dataTab,"\ndata featuring ",yAxis," over ", xAxis)
          }
          yaxlab <- paste0(tab_crit)
        }

        plot_df[,yAxis] <- as.numeric(plot_df[,yAxis])
        plot_df[,xAxis] <- as.numeric(plot_df[,xAxis])
        p <- ggplot(data=plot_df, aes(x=!!sym(xAxis), y=!!sym(yAxis))) +
          geom_line(aes(group = 1),size = 0.5) +
          geom_point(size = 3) +
          get(LinePlotTheme)() +
          theme(axis.title.x = element_text(size = Xaxis_font, margin = margin(30,0,0,0)),
                axis.text.x = element_text(size = Xaxis_font, angle = 45, vjust = 0.5, hjust=1),
                axis.text.y = element_text(size = Yaxis_font),
                axis.title.y = element_text(size = Yaxis_font),
                plot.title = element_text(size = title_font, margin = margin(0,0,30,0)),
                legend.position = "none") +
          #ggtitle(plotTitle) +
          #xlab(Xaxis_title) +
          ylab(paste0(yaxlab))
        if (isTruthy(userCutP)) {
          p <- p +
            geom_hline(yintercept = userCutP, linetype = "dashed", color = "red")
        }
        #save(list = ls(), file = "Line_Plot_Env.RData", envir = environment())
        p

      })

      #output$PatientLinePlot <- renderPlot({
      #
      #  req(PatientLinePlot_react())
      #  p <- PatientLinePlot_react()
      #  p
      #
      #})

      output$dnldPatientLinePlot <- downloadHandler(
        filename = function() {
          dataTab <- input$LinePlotTable
          paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",dataTab),"_LinePlot_",Sys.Date(),".svg")
        },
        content = function(file) {
          pl <- PatientLinePlot_react()
          ggsave(file,pl,height = input$LinePlotHeight, width = input$LinePlotWidth)
        }
      )

      output$PatientLinePlotTable <- DT::renderDataTable({

        df <- PatientLinePlot_df()
        DT::datatable(df,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 10,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      rownames = F
        )

      })

      # Clustering -------------------------------------------------------------
      ## Sankey ----------------------------------------------------------------

      observeEvent(event_data_tr_clusters_clean(),{
        req(event_data_tr_clusters_clean())
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        sankey_event_choices <- unique(event_data_tr_clusters_clean$EventType)
        sankey_event_select <- ifelse(any(grepl("cluster",sankey_event_choices, ignore.case = T)),
                                      grep("cluster",sankey_event_choices,value = T, ignore.case = T)[1],
                                      sankey_event_choices[1])
        #save(list = ls(), file = "sankeyEvent.RData", envir = environment())
        updateSelectInput(session,"sankeyEvent", choices = sankey_event_choices, selected = sankey_event_select)
      })
      observeEvent(input$sankeyEvent,{
        req(event_data_tr_clusters_clean())
        req(input$sankeyEvent)
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        sankeyEvent <- input$sankeyEvent
        event_data_tr_clusters_clean_sub <- event_data_tr_clusters_clean[which(event_data_tr_clusters_clean$EventType == sankeyEvent),]
        sankey_line_choices <- unique(event_data_tr_clusters_clean_sub$Treatment_Line_Cluster)
        sankey_line_choices_sel <- ifelse((length(sankey_line_choices) > 4),4,length(sankey_line_choices))
        #save(list = ls(), file = "sankeyXaxis.RData", envir = environment())
        updateSelectizeInput(session,"sankeyXaxis", choices = sankey_line_choices, selected = sankey_line_choices[c(1:sankey_line_choices_sel)], server = T)
      })

      sankey_clusters_cast <- reactive({
        req(event_data_tr_clusters_clean())
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        #save(list = ls(), file = "sankey_clusters_cast.RData", envir = environment())
        sankey_event_opts <- unique(event_data_tr_clusters_clean$Event)
        sankey_events_cast_full <- event_data_tr_clusters_clean %>%
          pivot_wider(id_cols = Name,
                      names_from = Treatment_Line_Cluster,
                      values_from = EventSummary)
        sankey_event_opts <- sort(sankey_event_opts)
        sankey_events_cast_full <- sankey_events_cast_full[,c(colnames(sankey_events_cast_full)[1],sankey_event_opts)]
        sankey_events_cast_full
      })

      observe({
        event_data_key <- event_data_key()
        cat_NA <- input$SankeyNAs
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        sankey_event <- input$sankeyEvent
        SankeyXgroups <- input$sankeyXaxis
        sankey_clusters_cast <- sankey_clusters_cast()
        #save(list = ls(), file = "sankeyEvent_df_react_check.RData", envir = environment())
      })

      sankeyEvent_df_react <- reactive({
        req(event_data_tr_clusters_clean())
        req(input$sankeyEvent)
        req(input$sankeyXaxis)
        req(event_data_key())
        req(sankey_clusters_cast())
        event_data_key <- event_data_key()
        cat_NA <- input$SankeyNAs
        event_data_tr_clusters_clean <- event_data_tr_clusters_clean()
        sankey_event <- input$sankeyEvent
        SankeyXgroups <- input$sankeyXaxis
        sankey_clusters_cast <- sankey_clusters_cast()

        sankey_clusters_cast_sub <- sankey_clusters_cast[,c(colnames(sankey_clusters_cast)[1],SankeyXgroups)]
        if (cat_NA) {
          sankey_clusters_cast_sub[is.na(sankey_clusters_cast_sub)] <- "NA"
        }
        sankey_clusters_cast_sub <- sankey_clusters_cast_sub[which(rowSums(!is.na(sankey_clusters_cast_sub)) > 1),]
        #event_data_clusters_sub_wid <- sankey_clusters_cast_sub
        #save(list = ls(), file = "sankeyEvent_df_react.RData", envir = environment())
        sankey_clusters_cast_sub_plot <- do.call(rbind, apply(sankey_clusters_cast_sub, 1, function(x) {
          Name <- x[1]
          x <- na.omit(x[-1])
          data.frame(Name = Name,
                     x = names(x), node = x,
                     next_x = dplyr::lead(names(x)),
                     next_node = dplyr::lead(x), row.names = NULL)
        })) %>%
          mutate(x = factor(x, names(sankey_clusters_cast_sub)[-1]),
                 next_x = factor(next_x, names(sankey_clusters_cast_sub)[-1])) %>%
          as.data.frame()
        sankey_clusters_cast_sub_plot$x <- factor(sankey_clusters_cast_sub_plot$x, levels = SankeyXgroups)
        sankey_clusters_cast_sub_plot$next_x <- factor(sankey_clusters_cast_sub_plot$next_x, levels = SankeyXgroups)
        sankey_clusters_cast_sub_plot_n <- sankey_clusters_cast_sub_plot %>%
          group_by(across(all_of(c("x", "node")))) %>%
          tally() %>%
          as.data.frame()
        sankey_clusters_cast_sub_plot2 <- merge(sankey_clusters_cast_sub_plot,
                                                sankey_clusters_cast_sub_plot_n,
                                                all.x = TRUE)
        sankey_clusters_cast_sub_plot2

      })

      sankey_plot_react <- reactive({
        req(sankeyEvent_df_react())
        event_data_clusters_sub_wid_plot2 <- sankeyEvent_df_react()

        if (sum(is.na(event_data_clusters_sub_wid_plot2$x))!=nrow(event_data_clusters_sub_wid_plot2)) {
          pl <- ggplot(event_data_clusters_sub_wid_plot2, aes(x = x,
                                                              next_x = next_x,
                                                              node = node,
                                                              next_node = next_node,
                                                              fill = factor(node),
                                                              #label = paste0(n,"   ",str_wrap(node,25)))) +
                                                              label = paste0(node, " = ", n))) +
            theme_minimal()
          pl <- pl + geom_sankey(flow.alpha = 0.5,          # This Creates the transparency of your node
                                 node.color = "black",      # This is your node color
                                 show.legend = FALSE)       # This determines if you want your legend to show
          pl <- pl + geom_sankey_label(size = 4,
                                       color = "black",
                                       fill = NA,
                                       hjust = 0,
                                       position = position_nudge(x = 0.05),
                                       #position = position_nudge(x = -0.02),
                                       label.size = NA
          )
          pl <- pl + theme(legend.position = 'none',
                           axis.title = element_blank(),
                           axis.text.y = element_blank(),
                           #axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
                           axis.text.x = element_text(size = 12),
                           axis.ticks = element_blank(),
                           panel.grid.major.y = element_blank(),
                           plot.margin = unit(c(0, 0, 0.5, 0.5),
                                              "inches")) +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
          pl
        }

      })


      output$SankeyPlot <- renderPlot({
        req(sankey_plot_react())
        pl <- sankey_plot_react()
        pl
      })

      output$dnldSankeyTable <- downloadHandler(
        filename = function() {
          dataTab <- input$sankeyEvent
          paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",dataTab),"_Table_",Sys.Date(),".txt")
        },
        content = function(file) {
          df <- sankeyEvent_df_react()
          write.table(df,file, sep = '\t', row.names = F)
        }
      )
      output$dnldSankeyTableFiltered <- downloadHandler(
        filename = function() {
          dataTab <- input$sankeyEvent
          paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",dataTab),"_Table_Filtered_",Sys.Date(),".txt")
        },
        content = function(file) {
          df <- sankeyEvent_df_react()
          rows_filtered <- input$SankeyPlotTable_rows_all
          df_filtered <- df[rows_filtered,]
          write.table(df,file, sep = '\t', row.names = F)
        }
      )
      output$dnldSankeyPlot <- downloadHandler(
        filename = function() {
          dataTab <- input$sankeyEvent
          paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",dataTab),"_Sankey_",Sys.Date(),".svg")
        },
        content = function(file) {
          pl <- sankey_plot_react()
          ggsave(file,pl,height = 10, width = 15)
        }
      )

      output$SankeyPlotTable <- DT::renderDataTable({
        req(sankeyEvent_df_react())
        df <- sankeyEvent_df_react()
        DT::datatable(df,
                      escape = F,
                      class = "display nowrap",
                      extensions = 'ColReorder',
                      filter = 'top',
                      options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                     pageLength = 10,
                                     scrollX = T,
                                     target = "cell",
                                     colReorder = TRUE),
                      rownames = F
        )

      })

      #output$dnldSankeyPlotTable <- downloadHandler(
      #  filename = function() {
      #    dataTab <- input$sankeyDataTable
      #    paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",dataTab),"_Table_",Sys.Date(),".txt")
      #  },
      #  content = function(file) {
      #    dataTab <- input$sankeyDataTable
      #    Clin_Supp_List <- wkbk_react_sub()
      #    df <- Clin_Supp_List[[dataTab]]
      #
      #    write.table(df,file, sep = '\t', row.names = F)
      #  }
      #)

      ## Heatmap ----------------------------------------------------------------
      observe({
        if (input$heatView == "Patients") {
          updateCheckboxGroupInput(session,"HeatClusterRC", selected = c("Rows","Columns"))
        }
      })
      observe({
        req(wkbk_react_sub())
        req(input$heatView)
        wkbk_names <- names(wkbk_react_sub())
        if (input$heatView == "Patients") {
          updateSelectizeInput(session,"HeatAnnoDataTable",choices = c("No Annotation",wkbk_names),
                               options = list(
                                 placeholder = 'Please select an option below',
                                 onInitialize = I('function() { this.setValue(""); }')
                               ))
        }
      })
      observe({
        req(wkbk_react_sub())
        req(input$HeatAnnoDataTable)
        dataTab <- input$HeatAnnoDataTable
        Clin_Supp_List <- wkbk_react_sub()
        col_choices <- colnames(Clin_Supp_List[[dataTab]])[-1]
        updateSelectizeInput(session,"HeatAnnoCols", choices = col_choices, server = T)
      })

      TreatClusterHeatmapdf_EventCount <- reactiveVal(NULL)
      TreatClusterHeatmapdf_react <- reactive({
        req(event_data())
        #req(Patient_List_React())
        req(event_data_tr_clusters_clean())
        req(input$sankeyEvent)
        req(input$heatView)
        event_data <- event_data()
        #pat_list <- Patient_List_React()
        #event_data <- event_data[which(event_data[,1] %in% pat_list),]
        cluster_df <- event_data_tr_clusters_clean()
        EventSum <- input$sankeyEvent
        #EventCat <- gsub("Summary$","",EventSum)
        #EventCat <- trimws(EventCat)
        heat_view <- input$heatView
        HeatFlip <- input$HeatFlip
        event_data_key <- event_data_key()

        # add in event_data_key???

        #save(list = ls(), file = "TreatClusterHeatmapdf_react.RData", envir = environment())

        # code is splitting up pre-existing strings with commas, also leaving white space before word
        # maybe check if remove space before, then collapse the summary with space, and then when you sep with space you get the correct items

        if (EventSum %in% cluster_df$EventType) {
          cluster_df_event <- cluster_df %>%
            filter(EventType == EventSum) %>%
            separate_rows(EventSummary, sep = ", ") %>%
            mutate(Event = EventSummary) %>%
            rename("EventStart_Cluster" = EventStart) %>%
            rename("EventEnd_Cluster" = EventEnd) %>%
            select(Name,Event,Treatment_Line_Cluster,EventStart_Cluster,EventEnd_Cluster)
          cluster_event_df <- cluster_df_event

          if (heat_view == "Clusters") {
            cluster_event_df_cast <- reshape2::dcast(cluster_event_df, Event ~ Treatment_Line_Cluster,
                                                     fun.aggregate = length, value.var = "EventEnd_Cluster")
          } else {
            cluster_event_df_cast <- reshape2::dcast(cluster_event_df, Event ~ Name,
                                                     fun.aggregate = length, value.var = "EventEnd_Cluster")
          }
          #save(list = ls(), file = "heat_df.RData", envir = environment())
          rownames(cluster_event_df_cast) <- cluster_event_df_cast[,1]
          cluster_event_df_cast_mat <- as.matrix(cluster_event_df_cast[,-1])
          rownames(cluster_event_df_cast_mat) <- rownames(cluster_event_df_cast)
          colnames(cluster_event_df_cast_mat) <- colnames(cluster_event_df_cast)[-1]
          columns_ordered <- stringr::str_sort(colnames(cluster_event_df_cast_mat), numeric = T)
          cluster_event_df_cast_mat <- cluster_event_df_cast_mat[,columns_ordered, drop = F]
          #cluster_event_df_cast_mat <- cluster_event_df_cast_mat[,columns_ordered]
          if (HeatFlip) {
            cluster_event_df_cast_mat <- as.matrix(t(cluster_event_df_cast_mat))
          }
          TreatClusterHeatmapdf_EventCount(cluster_event_df_cast_mat)
          if (heat_view == "Patients") {
            cluster_event_df_cast_mat[which(cluster_event_df_cast_mat > 0)] <- 1
          }
          cluster_event_df_cast_mat
        }
      })

      observe({
        req(TreatClusterHeatmapdf_react())
        plot_df <- TreatClusterHeatmapdf_react()
        updateSelectizeInput(session,"HeatXchoices", choices = colnames(plot_df), selected = colnames(plot_df), server = T)
        updateSelectizeInput(session,"HeatYchoices", choices = rownames(plot_df), selected = rownames(plot_df), server = T)
      })


      #output$HeatmapErrorText <- renderText({
      #  req(TreatClusterHeatmapdf_react())
      #  req(input$HeatXchoices)
      #  req(input$HeatYchoices)
      #  plot_df_in <- TreatClusterHeatmapdf_react()
      #  x_vars <- input$HeatXchoices
      #  y_vars <- input$HeatYchoices
      #  plot_df <- as.matrix(plot_df_in[which(rownames(plot_df_in) %in% y_vars),which(colnames(plot_df_in) %in% x_vars),drop = F])
      #  if (isTruthy(plot_df)) {
      #    if (is.null(dim(plot_df))) {
      #      paste0("<font color=\"#FF0000\"><b>Error: </b>Insufficient number of variables to generate heatmap.</font>")
      #    }
      #  }
      #})


      TreatClusterHeatmap_react <- reactive({
        req(TreatClusterHeatmapdf_react())
        req(input$HeatXchoices)
        req(input$HeatYchoices)
        req(input$heatView)
        plot_df_in <- TreatClusterHeatmapdf_react()
        rc_cluster <- input$HeatClusterRC
        x_vars <- input$HeatXchoices
        y_vars <- input$HeatYchoices
        cluster_method <- input$HeatClusterMethod
        heat_cluster_rows <- ifelse("Rows" %in% rc_cluster,TRUE,FALSE)
        heat_cluster_cols <- ifelse("Columns" %in% rc_cluster,TRUE,FALSE)
        capYN <- input$HeatEventCapYN
        capN <- input$HeatEventCapN
        heat_view <- input$heatView
        border_op <- input$border_op
        row_font <- input$heatmapFontRow
        col_font <- input$heatmapFontCol

        color_lo <- input$HeatColorLow
        color_hi <- input$HeatColorHigh
        #save(list = ls(), file = "TreatClusterHeatmap_react.RData", envir = environment())


        plot_df <- as.matrix(plot_df_in[which(rownames(plot_df_in) %in% y_vars),which(colnames(plot_df_in) %in% x_vars),drop = F])
        #plot_df <- plot_df[which(rownames(plot_df) %in% y_vars),which(colnames(plot_df) %in% x_vars)]
        #plot_df <- plot_df[,which(colnames(plot_df) %in% x_vars)]


        if (isTruthy(plot_df)) {
          if (!is.null(dim(plot_df))) {
            if (capYN) {
              if (!is.na(capN)) {
                plot_df[which(plot_df>=capN)] <- capN
              }
            }
            if (all(areColors(c(color_lo, color_hi)))) {
              if (heat_view == "Patients") {
                col_fun = structure(c(color_lo,color_hi), names = c("0", "1"))
                legendParam <- list(title = "Event (Yes-1/No-0)",
                                    title_gp = gpar(fontsize = 12, fontface = 'bold'),
                                    labels_gp = gpar(fontsize = 12))
              } else {
                if (min(plot_df, na.rm = T) == max(plot_df, na.rm = T)) {
                  col_fun = structure(c(color_lo,color_hi), names = c("0", as.character(max(plot_df, na.rm = T))))
                  legendParam <- list(title = "Number of Events",
                                      title_gp = gpar(fontsize = 12, fontface = 'bold'),
                                      labels_gp = gpar(fontsize = 12))
                } else {
                  col_fun = colorRamp2(seq(min(plot_df, na.rm = T), max(plot_df, na.rm = T), length = 2),
                                       colors = c(color_lo,color_hi))
                  legendParam <- list(title = "Number of Events",
                                      title_gp = gpar(fontsize = 12, fontface = 'bold'),
                                      labels_gp = gpar(fontsize = 12))
                }
              }
              if (border_op) {
                cell_border <- gpar(col = "black", lwd = 1)
              } else {
                cell_border <- gpar(col = NA)
              }
              #print(heat_hover_avail)
              #pdf(NULL)
              p <- ComplexHeatmap::Heatmap(plot_df,
                                           #top_annotation = heat_anno,
                                           col = col_fun,
                                           clustering_method_columns = cluster_method, row_names_max_width = unit(10, "cm"),
                                           cluster_rows = heat_cluster_rows, cluster_columns = heat_cluster_cols,
                                           heatmap_legend_param = legendParam,
                                           row_names_gp = gpar(fontsize = row_font), column_names_gp = gpar(fontsize = col_font),
                                           rect_gp = cell_border,
                                           border = FALSE)
              #pdf(NULL)
              draw(p, padding = unit(c(25, 50, 2, 2), "mm"), align_heatmap_legend = "heatmap_top") # unit(c(bottom,left,right,top))
            }
          }
          #if (max(plot_df, na.rm = T) > 20) {
          #  if (!is.null(capYN)) {
          #    if (capYN) {
          #      req(capN)
          #      plot_df[which(plot_df>=capN)] <- capN
          #    }
          #  }
          #}
          #heat_anno <- NULL
          #anno_df <- input$HeatAnnoDataTable
          #if (isTruthy(anno_df) & anno_df != "No Annotation") {
          # anno_cols <- input$HeatAnnoCols
          # if (length(anno_cols) > 0) {
          #   req(wkbk_react())
          #   wkbk <- wkbk_react()
          #   anno_df <- wkbk[[anno_df]][,c(colnames(wkbk[[anno_df]])[1],anno_cols)]
          #   rownames(anno_df) <- anno_df[,1]
          #   anno_df <- anno_df[,-1]
          #   anno_df[,anno_cols] <- as.data.frame(lapply(anno_df[,anno_cols, drop = F], function(x) {
          #     if (!is.numeric(x) | all(suppressWarnings(as.numeric(x))%%1==0)) {
          #       return(factor(x))
          #     } else {
          #       return(x)
          #     }
          #   }))
          #   anno_df <- anno_df[x_vars,]
          #   heat_anno <- ComplexHeatmap::HeatmapAnnotation(df = anno_df,
          #                                                  name = anno_cols,
          #                                                  which = 'col'
          #   )
          # }
          #}

        }


      })

      output$TreatClusterHeatmap <- renderPlot({
        req(TreatClusterHeatmap_react())
        p <- TreatClusterHeatmap_react()
        if (heat_hover_avail) {
          p_int <- InteractiveComplexHeatmap::htPositionsOnDevice(p)
          heatInteractive_react(p_int)
        }
        #pdf(NULL)
        p
      })

      if (heat_hover_avail) {
        heatInteractive_react <- reactiveVal(NULL)

        hover_column <- shiny::reactiveVal(NULL)
        hover_row <- shiny::reactiveVal(NULL)
        shiny::observeEvent(input$heatmap_hover, {
          pos = InteractiveComplexHeatmap::getPositionFromHover(input$heatmap_hover)
          selection = InteractiveComplexHeatmap::selectPosition(TreatClusterHeatmap_react(), pos, mark = FALSE, ht_pos = heatInteractive_react(), verbose = FALSE)
          hover_column(selection$column_label)
          hover_row(selection$row_label)
        })

        output$HeatHoverInfo <- renderUI({
          req(heatInteractive_react())
          req(TreatClusterHeatmapdf_EventCount())
          hover_column <- hover_column()
          hover_row <- hover_row()
          df <- TreatClusterHeatmapdf_EventCount()
          if (isTruthy(hover_column) & isTruthy(hover_row)) {
            if (hover_column %in% colnames(df) & hover_row %in% rownames(df)) {
              value <- df[hover_row,hover_column]
              wellPanel(
                p(HTML(paste0("<b> Row: </b>", hover_row, "<br/>",
                              "<b> Column: </b>", hover_column, "<br/>",
                              "<b> Number of Events: </b>", value, "<br/>",
                              NULL
                ))))
            }
          }
        })
      }

      output$dnldTreatClusterHeatmap <- downloadHandler(
        filename = function() {
          event <- input$sankeyEvent
          heatView <- input$heatView
          paste0(gsub(" ","",event),"_By_",heatView,"_EventCount_Heatmap_",Sys.Date(),".svg")
        },
        content = function(file) {
          svg(filename = file, height = input$heatmapHeight1, width = input$heatmapWidth1)
          ComplexHeatmap::draw(TreatClusterHeatmap_react())
          dev.off()
        }
      )

      # Duration ---------------------------------------------------------------

      ## Boxplot ----------------------------------------------------------------



      #observe({
      #  req(param_data())
      #  req(event_data_key())
      #  param <- param_data()
      #  event_data_key <- event_data_key()
      #  save(list = ls(), file = "durationHeatEventType.RData", envir = environment())
      #
      #  duration_event_tabs <- unique(param[which(!is.na(param[,"Event End Column"])),1])
      #  duration_event_type <- unique(param[which(param[,"Treatment"] == TRUE),"Event Category"])
      #  duration_events <- unique(event_data_key[which(event_data_key$EventTab %in% duration_event_tabs & event_data_key$EventType %in% duration_event_type),"EventType"])
      #
      #  start_terms <- c("medication","drug","treatment","diagnosis")
      #  start_terms_found <- sapply(start_terms,function(x) {
      #    grep(x,duration_events,ignore.case = T)[1]
      #  })
      #  start_terms_found <- start_terms_found[!is.na(start_terms_found)]
      #  duration_events_selected <- ifelse(length(start_terms_found) > 0,
      #                                     duration_events[start_terms_found[[1]]],duration_events[1])
      #  #duration_events_selected <- ifelse(grepl("Medication",duration_events, ignore.case = T),
      #  #                                   grep("Medication",duration_events,value = T, ignore.case = T)[1],
      #  #                                   duration_events[1])
      #  updateSelectizeInput(session,"durationHeatEventType", choices = duration_events,selected = duration_events_selected)
      #})

      observe({
        req(event_data_key())
        #req(event_data())
        req(input$durationHeatEventType)
        #req(input$BoxplotDataTable)
        #event <- event_data()
        req(param_data())
        param <- param_data()
        event_data_key <- event_data_key()
        event_type <- input$durationHeatEventType
        if (event_type == "All Treatment Events") {
          duration_event_tabs <- unique(param[which(!is.na(param[,"Event End Column"])),1])
          event_opts <- unique(event_data_key[which(event_data_key$EventTab %in% duration_event_tabs),"EventType"])
        } else {
          event_opts <- unique(event_data_key[which(event_data_key$EventType == event_type),"EventSpecified"])
        }

        updateSelectizeInput(session,"BoxplotXaxis", choices = event_opts, selected = event_opts[1])

        #event_tab <- input$BoxplotDataTable
        #event_sub <- event[which(event$EventTab == event_tab),]
        #event_type_opts <- unique(event_sub$EventType)
        #if (length(event_type_opts) > 1) {
        #  event_type <- input$BoxPlotEventType
        #} else {
        #  event_type <- event_type_opts
        #}
        #event_sub_type <- event_sub[which(event_sub$EventType == event_type),]
        #event_opts <- gsub(paste0(event_type,": "),"",unique(event_sub_type$Event))
        #event_opts <- event_opts[event_opts != "NA"]
        #event_preSelect <- ifelse(any(grepl("Nivolumab",event_opts, ignore.case = T)),grep("Nivolumab",event_opts,value = T, ignore.case = T)[1],event_opts[1])
        #updateSelectizeInput(session,"BoxplotXaxis", choices = event_opts, selected = event_preSelect)
      })

      EventBoxPlotdf_react <- reactive({

        req(event_data_key())
        req(event_data())
        req(param_data())
        #req(input$BoxplotDataTable)
        req(input$BoxplotXaxis)
        param <- param_data()
        event_data_key <- event_data_key()
        event <- event_data()
        event_type <- input$durationHeatEventType
        event_spec <- input$BoxplotXaxis
        if (event_type == "All Treatment Events") {
          #duration_event_tabs <- unique(param[which(!is.na(param[,"Event End Column"])),1])
          #event_selected <- unique(event_data_key[which(event_data_key$EventTab %in% duration_event_tabs),"EventType"])
          event_selected <- event_spec
        } else {
          event_selected <- event_data_key[which(event_data_key$EventType == event_type & event_data_key$EventSpecified == event_spec),1]
        }
        #event_tab <- input$BoxplotDataTable
        event_sub <- event[which(event$Event == event_selected),]
        #event_type_opts <- unique(event_sub$EventType)
        MaxOrSum <- input$EventDurMaxSum
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        #if (length(event_type_opts) > 1) {
        #  event_type <- input$BoxPlotEventType
        #} else {
        #  event_type <- event_type_opts
        #}
        #event <- input$BoxplotXaxis
        #event_sub_event <- event_sub[which(event_sub$Event == paste0(event_type,": ",event)),]
        #event_sub_event$Event <- gsub(paste0(event_type,": "),"",unique(event_sub_event$Event))
        #event_sub_event_bp <- event_sub_event[,c(1,2,5,6)]
        event_sub_event_bp <- event_sub[,c(1,2,5,6)]
        if (AppTimeUnits != "days") {
          time_duration <- event_sub_event_bp[,4] - event_sub_event_bp[,3]
          time_duration <- convert_time_units(suppressWarnings(as.numeric(time_duration)),AppTimeUnits,"days")
          event_sub_event_bp$Days <- time_duration
        } else {
          event_sub_event_bp$Days <- round((event_sub_event_bp[,4] - event_sub_event_bp[,3]),4)
        }
        #event_sub_event_bp$Days <- round(((event_sub_event_bp[,4] - event_sub_event_bp[,3]) * 365.25),4)
        event_sub_event_bp <- event_sub_event_bp[complete.cases(event_sub_event_bp),]
        event_sub_event_bp$Days <- ifelse(event_sub_event_bp$Days == 0,1,event_sub_event_bp$Days)
        rownames(event_sub_event_bp) <- NULL
        if (MaxOrSum == "sum") {
          event_sub_event_bp <- event_sub_event_bp %>%
            group_by(!!sym(colnames(event_sub_event_bp)[1])) %>%
            mutate(Days = sum(Days)) %>%
            ungroup() %>%
            mutate(Outlier = ifelse(rstatix::is_outlier(Days),TRUE,FALSE))
        } else if (MaxOrSum == "max") {
          event_sub_event_bp <- event_sub_event_bp %>%
            group_by(!!sym(colnames(event_sub_event_bp)[1])) %>%
            mutate(Days = max(Days)) %>%
            ungroup() %>%
            mutate(Outlier = ifelse(rstatix::is_outlier(Days),TRUE,FALSE))
        }
        quart <- quantile(event_sub_event_bp$Days, na.rm = T)
        event_sub_event_bp$Quartile <- cut(event_sub_event_bp$Days,breaks = quart, labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE)
        event_sub_event_bp <- event_sub_event_bp %>%
          select(-c(EventStart,EventEnd)) %>%
          unique() %>%
          as.data.frame()
        event_sub_event_bp
      })


      EventBoxPlot_react <- reactive({
        req(EventBoxPlotdf_react())
        df <- EventBoxPlotdf_react()
        #dataTab <- input$BoxplotDataTable
        event_type <- input$durationHeatEventType
        #event_spec <- input$BoxplotXaxis
        xAxisCol <- input$BoxplotXaxis
        bpFlip <- input$BPflipBP
        dotChoice <- input$BPplotsampledots
        dotSize <- input$BPplotDotSize
        title_font <- input$BPplot1TitleSize              # Title font size
        Xaxis_font <- input$BPplot1XAxisSize              # Axis font size
        Yaxis_font <- input$BPplot1YAxisSize              # Axis font size
        BPplottheme <- input$BPTheme
        VilOrBP <- input$ViolinOrBoxP

        if (nrow(df) > 0) {
          p <- ggplot(df, aes(x = Event, y = Days))
          if (VilOrBP == "Box Plot") {
            p <- p + geom_boxplot(fill = "#5F9EA0")
          }
          if (VilOrBP == "Violin Plot") {
            p <- p + geom_violin(fill = "#5F9EA0") +
              stat_summary(fun=median, geom="crossbar", width=0.5, color="black")
          }
          p <- p +
            get(BPplottheme)() +
            labs(title = paste("Length of ",event_type," Event: \n",xAxisCol," in Days",sep = ""),
                 x = NULL, y = "Days")
          if (dotChoice) {
            p <- p + suppressWarnings(geom_jitter(aes(label = Name),
                                                  width = 0.25, size = dotSize))
          }
          p <- p + theme(axis.text.x = element_text(size = Xaxis_font),
                         axis.title.x = element_text(size = Xaxis_font),
                         axis.text.y = element_text(size = Yaxis_font),
                         axis.title.y = element_text(size = Yaxis_font),
                         plot.title = element_text(size = title_font,margin=margin(0,0,30,0)),
                         legend.position = "none") +
            scale_x_discrete(labels = xAxisCol)
          if (bpFlip) {
            p <- p + coord_flip()
          }
          ply <- ggplotly(p)
          ply <- ply %>%
            config(
              toImageButtonOptions = list(
                format = "svg",
                height = input$BPHeight,
                width = input$BPWidth,
                filename = paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",xAxisCol),"_Boxplot_",Sys.Date(),".txt")
              )
            )
          ply

        }


      })

      output$EventBoxPlot <- renderPlotly({

        req(EventBoxPlot_react())
        p <- EventBoxPlot_react()
        p

      })

      output$EventBoxPlotTable <- DT::renderDataTable({
        req(EventBoxPlotdf_react())
        df <- EventBoxPlotdf_react()
        if (nrow(df) > 0) {
          DT::datatable(df,
                        escape = F,
                        class = "display nowrap",
                        extensions = 'ColReorder',
                        options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                       pageLength = 10,
                                       scrollX = T,
                                       target = "cell",
                                       colReorder = TRUE),
                        rownames = F
          )
        }
      })

      output$dnldEventBoxPlotTable <- downloadHandler(
        filename = function() {
          dataTab <- input$BoxplotDataTable
          xAxis <- input$BoxplotXaxis
          paste0(gsub(" ","_",Project_Name),"_",gsub(" ","_",xAxis),"Duration_BoxplotTable_",Sys.Date(),".txt")
        },
        content = function(file) {
          df <- EventBoxPlotdf_react()
          write.table(df,file, sep = '\t', row.names = F)
        }
      )

      ## Heatmap ------------------------------------------------------

      observe({
        req(param_data())
        req(event_data_key())
        param <- param_data()
        event_key <- event_data_key()
        duration_event_tabs <- unique(param[which(!is.na(param[,"Event End Column"])),1])
        duration_event_type <- unique(param[which(param[,"Treatment"] == TRUE),"Event Category"])
        duration_events <- unique(event_key[which(event_key$EventTab %in% duration_event_tabs & event_key$EventType %in% duration_event_type),"EventType"])
        duration_events_spec <- unique(event_key[which(event_key$EventTab %in% duration_event_tabs & event_key$EventType %in% duration_event_type),"Event"])
        #duration_events <- unique(event_key[which(event_key$EventTab %in% duration_event_tabs),"EventType"])
        if (all(duration_events_spec == duration_events)) {
          updateSelectizeInput(session,"durationHeatEventType", choices = "All Treatment Events",selected = "All Treatment Events")
        } else {
          duration_events_selected <- ifelse(any(grepl("Medication",duration_events, ignore.case = T)),
                                             grep("Medication",duration_events,value = T, ignore.case = T)[1],
                                             duration_events[1])
          updateSelectizeInput(session,"durationHeatEventType", choices = duration_events,selected = duration_events_selected)
        }
      })

      TreatDistHeatmapdf_react <- reactive({
        req(event_data())
        #req(Patient_List_React())
        req(event_data_key())
        #req(input$heatViewDist)
        req(input$durationHeatEventType)
        req(input$EventDurMaxSumHeat)
        req(param_data())
        param <- param_data()
        event_data <- event_data()
        #pat_list <- Patient_List_React()
        event_key <- event_data_key()
        dur_event <- input$durationHeatEventType
        MaxOrSum <- input$EventDurMaxSumHeat
        #dur_view <- input$heatViewDist
        heat_flip <- input$HeatFlipDist
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())

        #save(list = ls(), file = "heat.RData", envir = environment())

        #event_data <- event_data[which(event_data[,1] %in% pat_list),]
        if (dur_event == "All Treatment Events") {
          duration_event_tabs <- unique(param[which(!is.na(param[,"Event End Column"])),1])
          duration_events <- unique(event_key[which(event_key$EventTab %in% duration_event_tabs),"EventType"])
          event_data <- event_data[which(event_data$Event %in% duration_events),]
        } else {
          event_data <- event_data[which(event_data$EventType == dur_event),]
        }
        if (AppTimeUnits != "days") {
          time_duration <- event_data$EventEnd - event_data$EventStart
          time_duration <- convert_time_units(suppressWarnings(as.numeric(time_duration)),AppTimeUnits,"days")
          event_data$EventDuration <- time_duration
        } else {
          event_data$EventDuration <- round((event_data$EventEnd - event_data$EventStart),4)
        }
        #event_data$EventDuration <- round(((event_data$EventEnd - event_data$EventStart) * 365.25),4)
        event_data$EventDuration <- ifelse(event_data$EventDuration == 0,1,event_data$EventDuration)

        if (MaxOrSum == "sum") {
          event_data_dur <- event_data %>%
            group_by(Name,Event) %>%
            mutate(EventDuration = sum(EventDuration)) %>%
            ungroup() %>%
            select(Name,Event,EventDuration) %>%
            unique() %>%
            mutate(Event = gsub(paste0(dur_event,": "),"",Event)) %>%
            pivot_wider(id_cols = Name,
                        names_from = Event,
                        values_from = EventDuration) %>%
            as.data.frame()

        } else if (MaxOrSum == "max") {
          event_data_dur <- event_data %>%
            group_by(Name,Event) %>%
            mutate(EventDuration = max(EventDuration)) %>%
            ungroup() %>%
            select(Name,Event,EventDuration) %>%
            unique() %>%
            mutate(Event = gsub(paste0(dur_event,": "),"",Event)) %>%
            pivot_wider(id_cols = Name,
                        names_from = Event,
                        values_from = EventDuration) %>%
            as.data.frame()
        }

        rownames(event_data_dur) <- event_data_dur[,1]
        event_data_dur <- as.matrix(event_data_dur[,-1,drop = F])
        event_data_dur <- as.matrix(t(event_data_dur))

        if (heat_flip) {
          event_data_dur <- as.matrix(t(event_data_dur))
        }
        event_data_dur
      })

      observe({
        req(TreatDistHeatmapdf_react())
        plot_df <- TreatDistHeatmapdf_react()
        updateSelectizeInput(session,"HeatXchoicesDist", choices = colnames(plot_df), selected = colnames(plot_df), server = T)
        updateSelectizeInput(session,"HeatYchoicesDist", choices = rownames(plot_df), selected = rownames(plot_df), server = T)
      })

      #output$HeatmapErrorText2 <- renderText({
      #  req(TreatDistHeatmapdf_react())
      #  req(input$HeatXchoicesDist)
      #  req(input$HeatYchoicesDist)
      #  plot_df_in <- TreatDistHeatmapdf_react()
      #  x_vars <- input$HeatXchoicesDist
      #  y_vars <- input$HeatYchoicesDist
      #  plot_df <- as.matrix(plot_df_in[which(rownames(plot_df_in) %in% y_vars),which(colnames(plot_df_in) %in% x_vars),drop = F])
      #  if (isTruthy(plot_df)) {
      #    if (is.null(dim(plot_df))) {
      #      paste0("<font color=\"#FF0000\"><b>Error: </b>Insufficient number of variables to generate heatmap.</font>")
      #    }
      #  }
      #})

      TreatDistHeatmap_react <- reactive({
        req(TreatDistHeatmapdf_react())
        req(input$HeatXchoicesDist)
        req(input$HeatYchoicesDist)
        #req(input$heatView)
        plot_df_in <- TreatDistHeatmapdf_react()
        rc_cluster <- input$HeatClusterRCDist
        x_vars <- input$HeatXchoicesDist
        y_vars <- input$HeatYchoicesDist
        cluster_method <- input$HeatClusterMethodDist
        heat_cluster_rows <- ifelse("Rows" %in% rc_cluster,TRUE,FALSE)
        heat_cluster_cols <- ifelse("Columns" %in% rc_cluster,TRUE,FALSE)
        #plot_df <- plot_df[which(rownames(plot_df) %in% y_vars),]
        #plot_df <- plot_df[,which(colnames(plot_df) %in% x_vars)]
        capYN <- input$HeatEventCapYNDist
        capN <- input$HeatEventCapNDist
        #heat_view <- input$heatView
        border_op <- input$border_opDist
        row_font <- input$heatmapDistFontRow
        col_font <- input$heatmapDistFontCol
        #convertNA <- input$HeatNAtoZero

        color_lo <- input$HeatColorLowDist
        color_hi <- input$HeatColorHighDist

        #save(list = ls(), file = "dist_heatmap_inputs.RData", envir = environment())

        plot_df <- as.matrix(plot_df_in[which(rownames(plot_df_in) %in% y_vars),which(colnames(plot_df_in) %in% x_vars),drop = F])

        #plot_df <- plot_df[,which(colnames(plot_df) %in% x_vars)]

        if (isTruthy(plot_df)) {
          if (capYN) {
            if (!is.na(capN)) {
              plot_df[which(plot_df>=capN)] <- capN
            }
          }

          #if (convertNA) {
          plot_df[is.na(plot_df)] <- 0
          #}

          if (all(areColors(c(color_lo, color_hi)))) {

            if (min(plot_df, na.rm = T) == max(plot_df, na.rm = T)) {
              col_fun = structure(c(color_lo,color_hi), names = c("0", as.character(max(plot_df, na.rm = T))))
              legendParam <- list(title = "Event Duration (Days)",
                                  title_gp = gpar(fontsize = 12, fontface = 'bold'),
                                  labels_gp = gpar(fontsize = 12))
            } else {
              col_fun = colorRamp2(c(min(plot_df,na.rm = T),max(plot_df,na.rm = T)), c(color_lo, color_hi))
              legendParam <- list(title = "Event Duration (Days)",
                                  title_gp = gpar(fontsize = 12, fontface = 'bold'),
                                  labels_gp = gpar(fontsize = 12))
            }

            #if (heat_view == "Patients") {
            #col_fun = colorRamp2(c(min(plot_df,na.rm = T),max(plot_df,na.rm = T)), c(color_lo, color_hi))
            #legendParam <- list(title = "Event Duration (Days)",
            #                    title_gp = gpar(fontsize = 12, fontface = 'bold'),
            #                    labels_gp = gpar(fontsize = 12))
            #col_fun = structure(c(color_lo,color_hi), names = c("0", "1"))
            #legendParam <- list(title = "Event (Yes-1/No-0)")
            #} else {
            #  col_fun = colorRamp2(seq(min(plot_df, na.rm = T), max(plot_df, na.rm = T), length = 2),
            #                       colors = c(color_lo,color_hi))
            #  legendParam <- list(title = "Number of Events")
            #}
            if (border_op) {
              cell_border <- gpar(col = "black", lwd = 1)
            } else {
              cell_border <- gpar(col = NA)
            }
            p <- ComplexHeatmap::Heatmap(plot_df,
                                         #top_annotation = heat_anno,
                                         col = col_fun,
                                         clustering_method_columns = cluster_method, row_names_max_width = unit(10, "cm"),
                                         cluster_rows = heat_cluster_rows, cluster_columns = heat_cluster_cols,
                                         #cluster_rows = FALSE, cluster_columns = FALSE,
                                         heatmap_legend_param = legendParam,
                                         row_names_gp = gpar(fontsize = row_font), column_names_gp = gpar(fontsize = col_font),
                                         rect_gp = cell_border,
                                         border = FALSE)
            #pdf(NULL)
            draw(p, padding = unit(c(25, 50, 2, 2), "mm"), align_heatmap_legend = "heatmap_top") # unit(c(bottom,left,right,top))
          }
        }


      })

      output$TreatDistHeatmap <- renderPlot({
        req(TreatDistHeatmap_react())
        p <- TreatDistHeatmap_react()
        if (heat_hover_avail) {
          p_int <- InteractiveComplexHeatmap::htPositionsOnDevice(p)
          heatDistInteractive_react(p_int)
        }
        p
      })

      if (heat_hover_avail) {
        heatDistInteractive_react <- reactiveVal(NULL)

        hoverDist_column <- shiny::reactiveVal(NULL)
        hoverDist_row <- shiny::reactiveVal(NULL)
        shiny::observeEvent(input$heatmapDist_hover, {
          pos = InteractiveComplexHeatmap::getPositionFromHover(input$heatmapDist_hover)
          selection = InteractiveComplexHeatmap::selectPosition(TreatDistHeatmap_react(), pos, mark = FALSE, ht_pos = heatDistInteractive_react(), verbose = FALSE)
          hoverDist_column(selection$column_label)
          hoverDist_row(selection$row_label)
        })

        output$HeatHoverInfoDist <- renderUI({
          req(heatDistInteractive_react())
          req(TreatDistHeatmapdf_react())
          hover_column <- hoverDist_column()
          hover_row <- hoverDist_row()
          df <- TreatDistHeatmapdf_react()
          if (isTruthy(hover_column) & isTruthy(hover_row)) {
            if (hover_column %in% colnames(df) & hover_row %in% rownames(df)) {
              value <- df[hover_row,hover_column]
              wellPanel(
                p(HTML(paste0("<b> Row: </b>", hover_row, "<br/>",
                              "<b> Column: </b>", hover_column, "<br/>",
                              "<b> Event Duration: </b>", value, "<br/>",
                              NULL
                ))))
            }
          }
        })
      }


      output$dnldTreatDistHeatmap <- downloadHandler(
        filename = function() {
          event <- input$durationHeatEventType
          #heatView <- input$heatView
          paste0(gsub(" ","",event),"By_Patients_Duration_Heatmap_",Sys.Date(),".svg")
        },
        content = function(file) {
          svg(filename = file, height = input$heatmapDistHeight1, width = input$heatmapDistWidth1)
          ComplexHeatmap::draw(TreatDistHeatmap_react())
          dev.off()
        }
      )

      ## Swim --------------------------------------------------------------

      output$rendDurationSwimmers <- renderUI({
        req(DurationSwimmers_plot_react())
        req(input$SwimmPlotHeightDist)
        plot_height <- paste0(input$SwimmPlotHeightDist,"px")
        shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("DurationSwimmers",height = plot_height, width = "100%")),type = 6)
      })

      DurationSwimmers_plot_df <- reactive({
        ##req(Patient_List_React())
        #req(event_data_key())
        #req(input$BoxplotDataTable)
        #req(input$BoxplotXaxis)
        #event_data_key <- event_data_key()
        #event_tab <- input$BoxplotDataTable
        #event <- input$BoxplotXaxis
        ##pat_list <- Patient_List_React()
        #event_data <- event_data()
        #event_type <- input$BoxPlotEventType

        req(event_data_key())
        req(event_data())
        req(input$BoxplotXaxis)
        req(param_data())
        param <- param_data()
        event_data_key <- event_data_key()
        event_data <- event_data()
        event_type <- input$durationHeatEventType
        event_spec <- input$BoxplotXaxis

        #save(list = ls(), file = "DurationSwimmers_plot_df.RData", envir = environment())

        if (event_type == "All Treatment Events") {
          #duration_event_tabs <- unique(param[which(!is.na(param[,"Event End Column"])),1])
          #event_selected <- unique(event_data_key[which(event_data_key$EventTab %in% duration_event_tabs),"EventType"])
          event_selected <- event_spec
        } else {
          event_selected <- event_data_key[which(event_data_key$EventType == event_type & event_data_key$EventSpecified == event_spec),1]
        }
        #event_selected <- event_data_key[which(event_data_key$EventType == event_type & event_data_key$EventSpecified == event_spec),1]
        event_sub_event <- event_data[which(event_data$Event == event_selected),]
        #event_sub_event$Event <- gsub(paste0(event_type,": "),"",unique(event_sub_event$Event))

        #event_data <- event_data[which(event_data$Name %in% pat_list),]

        #event_sub <- event_data[which(event_data$EventTab == event_tab),]
        #event_type_opts <- unique(event_sub$EventType)
        #if (length(event_type_opts) > 1) {
        #  event_type <- event_type
        #} else {
        #  event_type <- event_type_opts
        #}
        #full_event_name <- paste0(event_type,": ",event)
        #event_sub_event <- event_sub[which(event_sub$Event == full_event_name),]
        #event_sub_event$Event <- gsub(paste0(event_type,": "),"",unique(event_sub_event$Event))

        Patient_Event_Data <- event_sub_event %>%
          arrange(Name,EventStart) %>%
          select(Name,Event,EventStart,EventEnd) %>%
          as.data.frame()

        Patient_Event_Data_fl <- event_data %>%
          group_by(Name) %>%
          filter(Name %in% unique(Patient_Event_Data$Name)) %>%
          #filter(any(full_event_name %in% Event)) %>%
          mutate(Event_New = case_when(
            EventStart == min(EventStart,na.rm = T) ~ "First Recorded Time Point",
            EventStart == max(EventStart,na.rm = T) ~ "Final Recorded Time Point"
          )) %>%
          ungroup() %>%
          select(Name,Event,Event_New,EventStart,EventEnd) %>%
          filter(!is.na(Event_New)) %>%
          arrange(Name, EventStart) %>%
          group_by(Name, Event_New) %>%
          mutate(Event_Details = paste0(unique(Event), collapse = "\n")) %>%
          ungroup() %>%
          mutate(Event = Event_New) %>%
          select(Name,Event,EventStart,EventEnd,Event_Details) %>%
          unique() %>%
          group_by(Name, Event) %>%
          arrange(EventStart,EventEnd, .by_group = TRUE) %>%
          slice_head(n = 1) %>%
          unique() %>%
          as.data.frame()

        Patient_Event_Data2 <- merge(Patient_Event_Data,Patient_Event_Data_fl,all = T)

        Patient_Event_Data3 <- Patient_Event_Data2 %>%
          arrange(Name,EventStart) %>%
          group_by(Name) %>%
          mutate(EventStart_FromZero = round(EventStart - min(EventStart,na.rm = T),3)) %>%
          mutate(EventEnd_FromZero = round(EventEnd - min(EventStart,na.rm = T),3)) %>%
          mutate(FirstEventTime_FromZero = 0.00) %>%
          mutate(FinalEventTime_FromZero = round(EventEnd[which(Event == "Final Recorded Time Point")] - min(EventStart,na.rm = T),3)) %>%
          ungroup() %>%
          mutate(Event_Details = case_when(
            Event == "First Recorded Time Point" ~ paste0("<b>",Name,"</b>\n",paste0("<b>First Recorded Time Point</b>: ",FirstEventTime_FromZero,"\n"),
                                                          paste0("Age at Event: ",round(EventStart,3),"\n"),
                                                          Event_Details),
            Event == "Final Recorded Time Point" ~ paste0("<b>",Name,"</b>\n",paste0("<b>Final Recorded Time Point</b>: ",FinalEventTime_FromZero,"\n"),
                                                          paste0("Age at Event: ",round(EventEnd,3),"\n"),
                                                          Event_Details),
            !Event %in% c("First Recorded Time Point","Final Recorded Time Point") ~ paste0("<b>",Name,"</b>\n",paste0("<b>",Event,"</b>: ",EventStart_FromZero," - ",EventEnd_FromZero,"\n"),
                                                                                            paste0("Age at Event: ",round(EventStart,3)," - ",round(EventEnd,3),"\n"))
          )) %>%
          select(-EventStart,-EventEnd) %>%
          unique() %>%
          #group_by(Name,EventStart_FromZero) %>%
          #mutate(Event_Details = paste(unique(Event_Details), collapse = "\n")) %>%
          #ungroup() %>%
          as.data.frame()

        Patient_Event_Data3

      })


      observe({
        req(DurationSwimmers_plot_df())
        ply_df <- DurationSwimmers_plot_df()
        if (nrow(ply_df) > 0) {
          pat_choices <- unique(ply_df$Name)
          updateSelectizeInput(session,"swimmerPatSpecDist",choices = pat_choices, selected = NA, server = T)
        }
      })

      observe({
        req(DurationSwimmers_plot_df())
        ply_df <- DurationSwimmers_plot_df()
        swimmerShowAllDist <- input$swimmerShowAllDist
        if (swimmerShowAllDist) {
          updateNumericInput(session,"maxpatviewDist",value = length(unique(ply_df$Name)))
        }
      })

      DurationSwimmers_plot_react <- reactive({
        req(DurationSwimmers_plot_df())
        req(input$swimmerSortDist)
        req(input$maxpatviewDist)
        ply_df <- DurationSwimmers_plot_df()

        swimmSort <- input$swimmerSortDist
        maxpats <- input$maxpatviewDist
        patientsSpec <- input$swimmerPatSpecDist
        swimmerShowAll <- input$swimmerShowAllDist
        #event_tab <- input$BoxplotDataTable
        event_type <- input$durationHeatEventType
        event <- input$BoxplotXaxis


        if (event_type == "All Treatment Events") {
          full_event_name <- event
        } else {
          full_event_name <- paste0(event_type,": ",event)
        }


        #save(list = ls(), file = "DurationSwimmers_plot_react.RData", envir = environment())

        ply_df <- ply_df %>%
          mutate(EventTime = EventEnd_FromZero - EventStart_FromZero) %>%
          group_by(Name) %>%
          mutate(EventTime_Sum = sum(EventTime,na.rm = T)) %>%
          as.data.frame()


        if ((swimmerShowAll & maxpats == length(unique(ply_df$Name))) | !swimmerShowAll) {
          # sort to get correct head of patients
          if (swimmSort == "Duration - Ascending") {
            ply_df <- ply_df[order(ply_df$EventTime_Sum),]
          } else if (swimmSort == "Duration - Descending") {
            ply_df <- ply_df[order(ply_df$EventTime_Sum, decreasing = T),]
          } else if (swimmSort == "Overall Time - Ascending") {
            ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero),]
          } else if (swimmSort == "Overall Time - Descending") {
            ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero, decreasing = T),]
          } else if (swimmSort == "Alphabetical - Ascending") {
            ply_df <- ply_df[order(ply_df$Name, decreasing = T),]
          } else if (swimmSort == "Alphabetical - Descending") {
            ply_df <- ply_df[order(ply_df$Name),]
          }

          if (maxpats >= 1) {
            if (length(patientsSpec) > 0) {
              maxpats <- maxpats-length(patientsSpec)
              get_pats <- c(patientsSpec,head(unique(ply_df$Name),maxpats))
              ply_df <- ply_df[which(ply_df$Name %in% get_pats),]
            } else {
              if (length(unique(ply_df$Name)) > maxpats) {
                get_pats <- head(unique(ply_df$Name),maxpats)
                ply_df <- ply_df[which(ply_df$Name %in% get_pats),]
              }
            }
            # sort to display in correct visual order (needs to be opposite)
            if (swimmSort == "Duration - Ascending") {
              ply_df <- ply_df[order(ply_df$EventTime_Sum, decreasing = T),]
            } else if (swimmSort == "Duration - Descending") {
              ply_df <- ply_df[order(ply_df$EventTime_Sum),]
            } else if (swimmSort == "Overall Time - Ascending") {
              ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero, decreasing = T),]
            } else if (swimmSort == "Overall Time - Descending") {
              ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero),]
            } else if (swimmSort == "Alphabetical - Ascending") {
              ply_df <- ply_df[order(ply_df$Name, decreasing = T),]
            } else if (swimmSort == "Alphabetical - Descending") {
              ply_df <- ply_df[order(ply_df$Name),]
            }

            patientIDs <- unique(ply_df[,1])
            ply_df$Index <- as.numeric(factor(ply_df$Name, levels = patientIDs))
            patients <- unique(ply_df$Index)

            base_lines <- ply_df %>%
              group_by(Name) %>%
              summarize(
                First = min(FirstEventTime_FromZero),
                Final = max(FinalEventTime_FromZero),
                Index = unique(Index),
                .groups = "drop"
              )
            start_events <- ply_df %>%
              filter(Event == "First Recorded Time Point") %>%
              select(x = EventStart_FromZero, y = Index, Event_Details)
            end_events <- ply_df %>%
              filter(Event == "Final Recorded Time Point") %>%
              select(x = EventStart_FromZero, y = Index, Event_Details)
            main_events <- ply_df %>%
              filter(Event == full_event_name)

            plot_ly() %>%
              # Add base duration lines
              add_segments(data = base_lines,
                           x = ~First, xend = ~Final,
                           y = ~Index, yend = ~Index,
                           line = list(color = '#5F9EA0', width = 4),
                           #name = "Observation Window",
                           showlegend = FALSE) %>%
              # Add base start markers with hover text
              add_markers(data = start_events,
                          x = ~x, y = ~y,
                          marker = list(symbol = 'line-ns', size = 10, color = '#5F9EA0'),
                          text = ~Event_Details,
                          #text = ~paste("Start:", Event_Details),
                          hoverinfo = 'text',
                          #name = "Start (Base)",
                          showlegend = FALSE) %>%
              # Add base end markers with hover text
              add_markers(data = end_events,
                          x = ~x, y = ~y,
                          marker = list(symbol = 'line-ns', size = 10, color = '#5F9EA0'),
                          text = ~Event_Details,
                          #text = ~paste("End:", Event_Details),
                          hoverinfo = 'text',
                          #name = "End (Base)",
                          showlegend = FALSE) %>%
              # Add Carboplatin segments
              add_segments(data = main_events,
                           x = ~EventStart_FromZero, xend = ~EventEnd_FromZero,
                           y = ~Index, yend = ~Index,
                           line = list(color = 'tomato', width = 6),
                           text = ~Event_Details,
                           hoverinfo = 'text',
                           name = event) %>%
              # Carboplatin start marker
              add_markers(data = main_events,
                          x = ~EventStart_FromZero, y = ~Index,
                          marker = list(symbol = 'circle', size = 10, color = 'tomato'),
                          text = ~Event_Details,
                          #text = ~paste("Start:", Event_Details),
                          hoverinfo = 'text',
                          #name = paste0("Start - ",event),
                          showlegend = FALSE) %>%
              # Carboplatin end marker
              add_markers(data = main_events,
                          x = ~EventEnd_FromZero, y = ~Index,
                          marker = list(symbol = 'circle', size = 10, color = 'tomato'),
                          text = ~Event_Details,
                          #text = ~paste("End:", Event_Details),
                          hoverinfo = 'text',
                          #name = paste0("End - ",event),
                          showlegend = FALSE) %>%
              # Layout settings
              layout(
                xaxis = list(title = 'Time (Years)', zeroline = FALSE, titlefont = list(size = 20), tickfont = list(size = 15)),
                yaxis = list(title = 'Patients', tickvals = patients, ticktext = patientIDs,
                             titlefont = list(size = 20), tickfont = list(size = 12))
                #xaxis = list(title = "Time (Years)"),
                #title = "Swimmer Plot of Patients",
                #hovermode = "closest"
              ) %>%
              config(
                toImageButtonOptions = list(
                  format = "svg",
                  height = input$DistSwimmerHeight,
                  width = input$DistSwimmerWidth,
                  filename = paste0(event_type,"_",event,"EventDuration_Swimmers",Sys.Date())
                )
              )

          }

        }




      })

      output$DurationSwimmers <- renderPlotly({
        req(DurationSwimmers_plot_react())
        ply <- DurationSwimmers_plot_react()
        ply
      })






      # Time-To-Event ----------------------------------------------------------

      #event_data_key <- reactive({
      #  req(Patient_List_React())
      #  req(paramEvent_data())
      #  req(event_data())
      #  pat_list <- Patient_List_React()
      #  paramEvent_data <- paramEvent_data()
      #  event_data <- event_data()
      #  added_events <- added_events()
      #  sankey_added_events <- sankey_added_events()
      #  events_exp <- unique(paramEvent_data[which(paramEvent_data[,"Column Defined Event"] == TRUE),"Event Name"])
      #  save(list = ls(), file = "event_data_key.RData", envir = environment())
      #  event_data_key <- event_data %>%
      #    filter(Name %in% pat_list) %>%
      #    select(Event,EventType,EventTab,EventColumn) %>%
      #    unique() %>%
      #    mutate(EventLabel = case_when(
      #      Event %in% sankey_added_events ~ EventType,
      #      Event %in% added_events ~ paste0(EventTab,": Cut-Points"),
      #      !is.na(EventTab) & !is.na(EventColumn) ~ paste0(EventTab,": ",EventColumn),
      #      is.na(EventTab) ~ Event
      #    )) %>%
      #    #mutate(EventLabel = ifelse(!is.na(EventTab),paste0(EventTab,": ",EventColumn), Event)) %>%
      #    mutate(EventExpanded = ifelse(EventColumn %in% events_exp | Event %in% sankey_added_events | Event %in% added_events,TRUE,FALSE)) %>%
      #    as.data.frame()
      #  event_data_key$EventSpecified <- str_split_fixed(event_data_key$Event,": ",2)[,-1]
      #  event_data_key$EventSpecified[event_data_key$EventSpecified == ""] <- NA
      #  event_data_key$EventSpecified <- ifelse(event_data_key$Event %in% added_events,event_data_key$Event,event_data_key$EventSpecified)
      #  event_data_key$EventSpecified <- ifelse(event_data_key$Event %in% sankey_added_events,event_data_key$Event,event_data_key$EventSpecified)
      #  rownames(event_data_key) <- NULL
      #  event_data_key
      #})

      observe({
        req(event_data_key())
        event_data_key <- event_data_key()

        tte_choices <- unique(event_data_key$EventLabel)
        tte_choices_summ <- sort(grep("Summary$",tte_choices,value = T))
        tte_choices_spec <- grep("Summary$",tte_choices,value = T,invert = T)
        tte_choices_all <- c(tte_choices_spec,tte_choices_summ)
        #save(list = ls(), file = "tte_choices.RData", envir = environment())

        if (isTruthy(TTE_Start_Event_Col)) {
          tteSelected <- ifelse(any(grepl(TTE_Start_Event_Col,tte_choices,ignore.case = T)),
                                grep(TTE_Start_Event_Col,tte_choices,ignore.case = T, value = T)[1],tte_choices[1])
        } else {
          start_terms <- c("medication","drug","treatment","diagnosis")
          start_terms_found <- sapply(start_terms,function(x) {
            grep(x,tte_choices_all,ignore.case = T)[1]
          })
          start_terms_found <- start_terms_found[!is.na(start_terms_found)]
          tteSelected <- ifelse(length(start_terms_found) > 0,
                                tte_choices_all[start_terms_found[[1]]],tte_choices_all[1])
          #tteSelected <- ifelse(any(grepl("diagnosis",tte_choices,ignore.case = T)),
          #                      grep("diagnosis",tte_choices,ignore.case = T, value = T)[1],tte_choices[1])
        }

        if (isTruthy(TTE_End_Event_Col)) {
          tteEndSelected <- ifelse(any(grepl(TTE_End_Event_Col,tte_choices_all,ignore.case = T)),
                                   grep(TTE_End_Event_Col,tte_choices_all,ignore.case = T, value = T)[1],tte_choices_all[1])
        } else {
          tteEndSelected <- ifelse(any(grepl("Treatment",tte_choices_summ,ignore.case = T)),
                                   grep("Treatment",tte_choices_summ,ignore.case = T, value = T)[1],tte_choices_summ[1])
        }

        updateSelectInput(session,"tteStartCol2",choices = tte_choices_all,selected = tteSelected)
        updateSelectInput(session,"tteStopCol2",choices = tte_choices_all,selected = tteEndSelected)
      })

      ### Start
      strartCrit2_Choices <- reactive({
        req(event_data_key())
        req(input$tteStartCol2)
        event_data_key <- event_data_key()
        startCols_full <- input$tteStartCol2
        startChoicesRows <- rownames(event_data_key[which(event_data_key$EventLabel == startCols_full & !is.na(event_data_key$EventSpecified)),])
        startChoicesNames <- event_data_key[which(event_data_key$EventLabel == startCols_full & !is.na(event_data_key$EventSpecified)),"EventSpecified"]
        names(startChoicesRows) <- startChoicesNames
        startChoices_list <- startChoicesRows[sort(names(startChoicesRows))]
        if (isTruthy(startChoices_list)) {
          startChoices_list
        }
      })

      output$ShowStartCrit <- reactive({
        req(event_data_key())
        req(input$tteStartCol2)
        event_data_key <- event_data_key()
        startCols_full <- input$tteStartCol2
        if (any(event_data_key[which(event_data_key$EventLabel %in% startCols_full),"EventExpanded"])) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "ShowStartCrit", suspendWhenHidden = FALSE)

      output$BiomarkerData <- reactive({
        biomarkerUIinput <- input$InputBiomarkerData
        print(biomarkerUIinput)
        if (isTruthy(biomarkerUIinput)) {
          if (biomarkerUIinput == "Yes") {
            TRUE
          } else {
            FALSE
          }
        } else if (Data_Contains_Longitudinal_Biomarkers) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "BiomarkerData", suspendWhenHidden = FALSE)


      observe({
        req(strartCrit2_Choices())
        startChoices <- strartCrit2_Choices()
        #StartSelect <- startChoices[1]
        if (isTruthy(TTE_Start_Event_Crit)) {
          StartSelect <- startChoices[grep(paste0(TTE_Start_Event_Crit,collapse = "|"),names(startChoices),value = T, ignore.case = T)]
          #StartSelect <- startChoices[TTE_Start_Event_Crit]
        } else {
          StartSelect <- startChoices[1]
        }
        #save(list = ls(), file = "StartCrit2.RData", envir = environment())
        updateSelectInput(session,"StartCrit2",choices = startChoices, selected = StartSelect)
      })


      observeEvent(input$SelectAllStartCrit2,{
        req(strartCrit2_Choices())
        if (input$SelectAllStartCrit2) {
          updateSelectInput(session,"StartCrit2",choices = strartCrit2_Choices(), selected = strartCrit2_Choices())
        } else if (!input$SelectAllStartCrit2) {
          updateSelectInput(session,"StartCrit2",choices = strartCrit2_Choices(), selected = strartCrit2_Choices()[1])
        }
      })

      start_events_react <- reactive({
        req(event_data_key())
        req(input$tteStartCol2)
        event_data_key <- event_data_key()
        StartPoint <- input$tteStartCol2
        StartCrit <- input$StartCrit2

        #save(list = ls(), file = "start_events_react.RData", envir = environment())
        if (!isTruthy(StartCrit)) {
          start_events <- event_data_key[which(event_data_key$EventLabel %in% StartPoint & is.na(event_data_key$EventSpecified)),"Event"]
          #start_events <- event_data_key[which(event_data_key$Event == StartPoint),"Event"]
          #start_events <- event_data_key[which(event_data_key$EventLabel == StartPoint),"Event"]
          #start_events <- StartPoint
        } else {
          if (all(event_data_key[StartCrit,"EventLabel"] %in% StartPoint)) {
            start_events <- event_data_key[StartCrit,"Event"]
          } else {
            start_events <- event_data_key[which(event_data_key$EventLabel %in% StartPoint & is.na(event_data_key$EventSpecified)),"Event"]
            #start_events <- event_data_key[which(event_data_key$Event == StartPoint),"Event"]
            #start_events <- event_data_key[which(event_data_key$EventLabel == StartPoint),"Event"]
            #start_events <- StartPoint
          }
        }
      })


      EventData_wStartPoint <- reactive({
        req(event_data())
        #req(Patient_List_React())
        req(start_events_react())
        Patient_Event_Data <- event_data()
        #pat_list <- Patient_List_React()
        start_events <- start_events_react()
        AndOr <- input$StartAndOr2
        #save(list = ls(), file = "EventData_wStartPoint.RData", envir = environment())

        if (isTruthy(AndOr)) {
          if (AndOr == "'Or' Statement") {
            Patient_Event_Data <- Patient_Event_Data %>%
              #filter(Name %in% pat_list) %>%
              group_by(Name) %>%
              filter(any(Event %in% start_events)) %>%
              mutate(Start_Time_Point = min(EventStart[which(Event %in% start_events)], na.rm = T)) %>%
              mutate(Final_Time_Point = max(c(EventStart,EventEnd), na.rm = T)) %>%
              as.data.frame()
            Patient_Event_Data
          } else {
            Patient_Event_Data <- Patient_Event_Data %>%
              #filter(Name %in% pat_list) %>%
              group_by(Name) %>%
              mutate(StartPoitntFound = Event %in% start_events) %>%
              filter(length(unique(Event[which(StartPoitntFound==TRUE)])) == length(start_events)) %>%
              select(-StartPoitntFound) %>%
              mutate(Start_Time_Point = min(EventStart[which(Event %in% start_events)], na.rm = T)) %>%
              mutate(Final_Time_Point = max(c(EventStart,EventEnd), na.rm = T)) %>%
              as.data.frame()
            Patient_Event_Data
          }
        } else {
          Patient_Event_Data <- Patient_Event_Data %>%
            #filter(Name %in% pat_list) %>%
            group_by(Name) %>%
            filter(any(Event %in% start_events)) %>%
            mutate(Start_Time_Point = min(EventStart[which(Event %in% start_events)], na.rm = T)) %>%
            mutate(Final_Time_Point = max(c(EventStart,EventEnd), na.rm = T)) %>%
            as.data.frame()
          Patient_Event_Data
        }

      })

      ### Stop



      stopCrit2_Choices <- reactive({
        event_data_key <- event_data_key()
        stopCols_full <- input$tteStopCol2

        if (length(stopCols_full) > 1) {
          stopChoices_list <- lapply(stopCols_full,function(x) {
            stopChoicesRows <- rownames(event_data_key[which(event_data_key$EventLabel == x & !is.na(event_data_key$EventSpecified)),])
            stopChoicesNames <- event_data_key[which(event_data_key$EventLabel == x & !is.na(event_data_key$EventSpecified)),"EventSpecified"]
            names(stopChoicesRows) <- stopChoicesNames
            stopChoices <- stopChoicesRows[sort(names(stopChoicesRows))]
            return(stopChoices)
          })
          names(stopChoices_list) <- stopCols_full
          stopChoices_list <- stopChoices_list[lapply(stopChoices_list,length)>0]
          stopChoices_list
        } else {

          stopChoicesRows <- rownames(event_data_key[which(event_data_key$EventLabel %in% stopCols_full & !is.na(event_data_key$EventSpecified)),])
          stopChoicesNames <- event_data_key[which(event_data_key$EventLabel %in% stopCols_full & !is.na(event_data_key$EventSpecified)),"EventSpecified"]
          names(stopChoicesRows) <- stopChoicesNames
          stopChoices_list <- stopChoicesRows[sort(names(stopChoicesRows))]

          stopChoices_list
        }

      })

      output$ShowStopCrit <- reactive({
        req(event_data_key())
        req(input$tteStopCol2)
        event_data_key <- event_data_key()
        stopCols_full <- input$tteStopCol2
        #save(list = ls(), file = "ShowStopCrit.RData", envir = environment())
        if (any(event_data_key[which(event_data_key$EventLabel %in% stopCols_full),"EventExpanded"])) {
          TRUE
        } else {
          FALSE
        }
      })
      outputOptions(output, "ShowStopCrit", suspendWhenHidden = FALSE)

      criteria_selected <- reactiveValues(Criteria = 1)
      observeEvent(input$StopCrit2, {
        criteria_selected$Criteria <- input$StopCrit2
      })
      observe({
        if (all(criteria_selected$Criteria == 1)) {
          preSelect <- TTE_End_Event_Crit
          if (isTruthy(preSelect)) {
            req(stopCrit2_Choices())
            stopChoices <- stopCrit2_Choices()
            StopSelect <- stopChoices[grep(paste0(preSelect,collapse = "|"),names(stopChoices),value = T, ignore.case = T)]
            criteria_selected$Criteria <- StopSelect
          }
        }
      })
      observe({
        req(stopCrit2_Choices())
        stopChoices <- stopCrit2_Choices()
        #StopSelect <- stopChoices[1]
        if (isTruthy(TTE_End_Event_Crit)) {
          StopSelect <- stopChoices[grep(paste0(TTE_End_Event_Crit,collapse = "|"),names(stopChoices),value = T, ignore.case = T)]
        } else {
          StopSelect <- stopChoices[1]
        }
        #save(list = ls(), file = "StopCrit2.RData", envir = environment())
        updateSelectInput(session,"StopCrit2",choices = stopChoices, selected = StopSelect)
      })
      observeEvent(input$tteStopCol2, {
        req(stopCrit2_Choices())
        stopChoices <- stopCrit2_Choices()
        crit_sel <- criteria_selected$Criteria
        stopChoices_vec <- unname(unlist(stopChoices))
        crit_sel <- stopChoices_vec[which(stopChoices_vec %in% crit_sel)]
        updateSelectInput(session = session, inputId = "StopCrit2",choices = stopChoices, selected = crit_sel)
      })

      observeEvent(input$SelectAllStopCrit2,{
        req(stopCrit2_Choices())
        if (input$SelectAllStopCrit2) {
          updateSelectInput(session,"StopCrit2",choices = stopCrit2_Choices(), selected = stopCrit2_Choices())
        } else if (!input$SelectAllStopCrit2) {
          updateSelectInput(session,"StopCrit2",choices = stopCrit2_Choices(), selected = stopCrit2_Choices()[1])
        }
      })


      end_events_react <- reactive({

        req(event_data_key())
        req(input$tteStopCol2)
        event_data_key <- event_data_key()
        StopPoint <- input$tteStopCol2
        StopCrit <- input$StopCrit2
        #save(list = ls(), file = "end_events_react.RData", envir = environment())

        #end_events_df <- event_data_key[which(event_data_key$EventLabel == StopPoint),]

        if (!isTruthy(StopCrit)) {
          end_events <- event_data_key[which(event_data_key$EventLabel %in% StopPoint & is.na(event_data_key$EventSpecified)),"Event"]
          #end_events <- event_data_key[which(event_data_key$EventLabel == StopPoint),"Event"]
          #end_events <- event_data_key[which(event_data_key$Event == StopPoint),"Event"]
          #end_events <- StopPoint
        } else {
          if (all(event_data_key[StopCrit,"EventLabel"] %in% StopPoint)) {
            end_events_spec <- event_data_key[StopCrit,"Event"]
            end_events_Nspec <- event_data_key[which(event_data_key$EventLabel %in% StopPoint & is.na(event_data_key$EventSpecified)),"Event"]
            end_events <- c(end_events_Nspec,end_events_spec)
          } else {
            end_events <- event_data_key[which(event_data_key$EventLabel %in% StopPoint & is.na(event_data_key$EventSpecified)),"Event"]
            #end_events <- event_data_key[which(event_data_key$EventLabel == StopPoint),"Event"]
            #end_events <- event_data_key[which(event_data_key$Event == StopPoint),"Event"]
            #end_events <- StopPoint
          }
        }


      })


      observe({
        req(wkbk_react_anno_sub())
        #req(wkbk_react())
        req(input$KPstrataDataTable)
        dataTab <- input$KPstrataDataTable
        #Clin_Supp_List <- wkbk_react()
        Clin_Supp_List <- wkbk_react_anno_sub()
        if (dataTab == "ShinyEvents Treatment Clusters") {
          req(sankey_clusters_cast())
          cluster_df <- sankey_clusters_cast()
          col_choices <- colnames(cluster_df)[-1]
        } else {
          col_choices <- colnames(Clin_Supp_List[[dataTab]])[-1]
        }
        updateSelectizeInput(session,"KPstrataCol", choices = col_choices, server = T)
      })
      observe({
        req(wkbk_react_anno_sub())
        req(cohort_TTE_table())
        tte_df <- cohort_TTE_table()
        dataTab <- input$KPstrataDataTable
        xAxisCol <- input$KPstrataCol
        #Clin_Supp_List <- wkbk_react()
        Clin_Supp_List <- wkbk_react_anno_sub()
        if (dataTab == "ShinyEvents Treatment Clusters") {
          req(sankey_clusters_cast())
          df <- sankey_clusters_cast()
          if (isTruthy(xAxisCol)) {
            if (all(xAxisCol %in% colnames(df))) {
              df <- df[,c(colnames(df)[1],xAxisCol)]
              df <- df[which(df[,1] %in% tte_df[,1]),]
              df <- df[complete.cases(df),]
              groupChoices <- unique(df[,xAxisCol])
              groupChoices <- groupChoices[!is.na(groupChoices)]
              updateSelectizeInput(session,"KPstrataColGroups",choices = groupChoices,
                                   selected = groupChoices[1], server = T)
            }
          }
        } else {
          if (isTruthy(xAxisCol)) {
            df <- Clin_Supp_List[[dataTab]]
            if (all(xAxisCol %in% colnames(df))) {
              df <- df[,c(colnames(df)[1],xAxisCol)]
              df <- df[which(df[,1] %in% tte_df[,1]),]
              df <- df[complete.cases(df),]
              groupChoices <- unique(df[,xAxisCol])
              groupChoices <- groupChoices[!is.na(groupChoices)]
              updateSelectizeInput(session,"KPstrataColGroups",choices = groupChoices,
                                   selected = groupChoices[1], server = T)
            }
          }
        }



      })

      cohort_EventSumm_df <- reactive({

        req(EventData_wStartPoint())
        req(start_events_react())
        req(end_events_react())

        Patient_Event_Data <- EventData_wStartPoint()
        start_events <- start_events_react()
        stop_events <- end_events_react()
        AndOrstop <- input$StopAndOr2
        #save(list = ls(), file = "cohort_EventSumm_df.RData", envir = environment())

        if (nrow(Patient_Event_Data) > 0) {
          if (isTruthy(AndOrstop)) {
            if (AndOrstop == "'Or' Statement") {
              Patient_Event_Data <- Patient_Event_Data %>%
                group_by(Name) %>%
                mutate(End_Time_Point = min((EventStart[which(Event %in% stop_events)][(EventStart[which(Event %in% stop_events)]) > unique(Start_Time_Point)]), na.rm = T)) %>%
                as.data.frame()
            } else {
              Patient_Event_Data <- Patient_Event_Data %>%
                group_by(Name) %>%
                mutate(End_Time_Point = min((EventStart[which(all(stop_events %in% Event))][(EventStart[which(all(stop_events %in% Event))]) > unique(Start_Time_Point)]), na.rm = T)) %>%
                as.data.frame()
            }
          } else {
            Patient_Event_Data <- Patient_Event_Data %>%
              group_by(Name) %>%
              mutate(End_Time_Point = min((EventStart[which(Event %in% stop_events)][(EventStart[which(Event %in% stop_events)]) > unique(Start_Time_Point)]), na.rm = T)) %>%
              as.data.frame()
          }
          if (nrow(Patient_Event_Data) > 0) {
            Patient_Event_Data
          }
        }

      })


      output$rendTTEexplHeader <- renderUI({
        req(EventData_wStartPoint())
        h4("Time-To-Event Description")
      })
      output$rendTTEexpl <- renderUI({
        req(EventData_wStartPoint())
        start_events <- start_events_react()
        stop_events <- end_events_react()
        AndOrstart <- input$StartAndOr2
        AndOrstop <- input$StopAndOr2
        Patient_Event_Data <- EventData_wStartPoint()

        if (length(start_events) > 1) {
          start_events_text <- paste0("events: <b>",paste0(start_events,collapse = ", "),"</b>")
          if (AndOrstart == "'And' Statement") {
            AndOrstart_text <- paste0(" all of ")
          } else {
            AndOrstart_text <- paste0(" at least one of ")
          }
        } else {
          start_events_text <- paste0("event: <b>",start_events,"</b>")
          AndOrstart_text <- paste0(" ")
        }
        if (length(stop_events) > 1) {
          stop_events_text <- paste0("these progression events: <b>",paste0(stop_events,collapse = ", "),"</b>")
          if (AndOrstop == "'And' Statement") {
            AndOrstop_text <- paste0(" of all of ")
          } else {
            AndOrstop_text <- paste0(" at least one of ")
          }
        } else {
          stop_events_text <- paste0("this progression event: <b>",stop_events,"</b>")
          AndOrstop_text <- paste0(" ")
        }

        p(HTML("A total of <b>",length(unique(Patient_Event_Data[,1]))," Patients</b> were identified that had event information associated with",
               AndOrstart_text,"the following ",start_events_text,".<br>",
               "A real-world end point was calculated with the occurence of",AndOrstop_text,stop_events_text,"."))
      })

      ## KP -------------------------------------------------------------------

      cohort_TTE_table_wInfo <- reactive({
        req(cohort_EventSumm_df())
        Patient_Event_Data_sub_ETC3 <- cohort_EventSumm_df()
        req(start_events_react())
        req(end_events_react())

        start_events <- start_events_react()
        stop_events <- end_events_react()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())

        #save(list = ls(), file = "cohort_TTE_table_wInfo.RData", envir = environment())

        # need to tell app if time data is starting from 0 or min patient time

        Patient_Event_Data_sub_ETC4 <- Patient_Event_Data_sub_ETC3
        Patient_event_info <- Patient_Event_Data_sub_ETC4 %>%
          filter(Event %in% c(start_events,stop_events)) %>%
          select(Name,Event,Start_Time_Point) %>%
          unique() %>%
          group_by(Name) %>%
          mutate(Start_Event = paste0(unique(Event[Event %in% start_events]), collapse = ", ")) %>%
          mutate(Progression_Event = ifelse(any(Event %in% stop_events),paste0(unique(Event[Event %in% stop_events]), collapse = ", "),NA)) %>%
          select(Name,Start_Event,Progression_Event,Start_Time_Point) %>%
          unique() %>%
          as.data.frame()
        Patient_Event_Data_sub_surv <- Patient_Event_Data_sub_ETC4 %>%
          select(Name,Start_Time_Point,End_Time_Point,Final_Time_Point) %>%
          unique() %>%
          as.data.frame()
        Patient_Event_Data_sub_surv <- merge(Patient_event_info,Patient_Event_Data_sub_surv, all = T)
        Patient_Event_Data_sub_surv$status <- ifelse(is.infinite(Patient_Event_Data_sub_surv$End_Time_Point),0,1)
        Patient_Event_Data_sub_surv[is.infinite(Patient_Event_Data_sub_surv$End_Time_Point),c("Progression_Event","End_Time_Point")] <- NA


        #Patient_Event_Data_sub_surv[is.infinite(Patient_Event_Data_sub_surv$End_Time_Point),"End_Time_Point"] <- Patient_Event_Data_sub_surv[is.infinite(Patient_Event_Data_sub_surv$End_Time_Point),"Final_Time_Point"]


        Patient_Event_Data_sub_surv$time <- ifelse(is.na(Patient_Event_Data_sub_surv$End_Time_Point),
                                                   (Patient_Event_Data_sub_surv$Final_Time_Point-Patient_Event_Data_sub_surv$Start_Time_Point),
                                                   (Patient_Event_Data_sub_surv$End_Time_Point-Patient_Event_Data_sub_surv$Start_Time_Point))

        Patient_Event_Data_sub_surv$time <- convert_time_units(Patient_Event_Data_sub_surv$time,AppTimeUnits,"days")

        #Patient_Event_Data_sub_surv$time <- ifelse(is.na(Patient_Event_Data_sub_surv$End_Time_Point),
        #                                           (Patient_Event_Data_sub_surv$Final_Time_Point-Patient_Event_Data_sub_surv$Start_Time_Point)*365.25,
        #                                           (Patient_Event_Data_sub_surv$End_Time_Point-Patient_Event_Data_sub_surv$Start_Time_Point)*365.25)

        #Patient_Event_Data_sub_surv$time <- (Patient_Event_Data_sub_surv$End_Time_Point-Patient_Event_Data_sub_surv$Start_Time_Point)*365.25
        rownames(Patient_Event_Data_sub_surv) <- Patient_Event_Data_sub_surv[,1]
        Patient_Event_Data_sub_surv

      })

      cohort_TTE_table <- reactive({
        req(cohort_TTE_table_wInfo())
        plot_df <- cohort_TTE_table_wInfo()
        plot_df <- plot_df %>%
          select(-any_of(c("Start_Event","Progression_Event","Start_Time_Point","End_Time_Point","Final_Time_Point"))) %>%
          as.data.frame()
        plot_df
      })

      observe({
        req(input$KPstrataCol)
        strata_col <- input$KPstrataCol
        updateTextInput(session,"KPotherGroupName", value = paste0(strata_col,"_Other"))
      })

      cohort_TTE_table_strata <- reactive({

        req(wkbk_react_anno_sub())
        req(cohort_TTE_table())
        plot_df <- cohort_TTE_table()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        kp_dt <- input$KPstrataDataTable
        strata_col <- input$KPstrataCol
        strata_groups <- input$KPstrataColGroups
        #Clin_Supp_List <- wkbk_react()
        Clin_Supp_List <- wkbk_react_anno_sub()
        #save(list = ls(), file = "cohort_TTE_table_strata.RData", envir = environment())
        if (isTruthy(input$KPstrataDataTable)) {
          if (isTruthy(strata_col)) {
            if (kp_dt == "ShinyEvents Treatment Clusters") {
              req(sankey_clusters_cast())
              df <- sankey_clusters_cast()
            } else {
              if (kp_dt %in% names(Clin_Supp_List)) {
                df <- Clin_Supp_List[[kp_dt]]
              }
            }
            if (strata_col %in% colnames(df)) {
              df_sub <- df[,c(colnames(df)[1],strata_col)]
              colnames(df_sub)[1] <- colnames(plot_df)[1]
              if (input$KPshowOtherGroup) {
                req(input$KPotherGroupName)
                df_sub[which(!df_sub[,2] %in% strata_groups),strata_col] <- input$KPotherGroupName
                df_sub_g <- df_sub
              } else {
                df_sub_g <- df_sub[which(df_sub[,2] %in% strata_groups),]
              }
              df_sub_g <- unique(df_sub_g)
              plot_df <- merge(df_sub_g,plot_df,all.y = T)
            }
          }
          #plot_df$time <- convert_time_units(plot_df$time,AppTimeUnits,"days")
          plot_df
        } else {
          #plot_df$time <- convert_time_units(plot_df$time,AppTimeUnits,"days")
          plot_df
        }
      })


      KPplotHRtab_react <- reactive({
        req(cohort_TTE_table_strata())
        plot_df <- cohort_TTE_table_strata()
        strata_col <- input$KPstrataCol
        if (ncol(plot_df) == 4) {
          if (length(plot_df[,strata_col][which(is.na(plot_df[,strata_col]))]) < nrow(plot_df)) {
            if (length(unique(plot_df[,strata_col])) > 1) {
              strata_col <- sprintf(ifelse(grepl(" ", strata_col), "`%s`", "%s"), strata_col)
              form <- as.formula(paste0("Surv(time,status) ~ ",strata_col))
              tab <- coxph(as.formula(paste0("Surv(time,status) ~ ",strata_col)),data = plot_df)
              tab
            }
          }
        }
      })
      output$KPplotHRtab <- renderTable({

        req(KPplotHRtab_react())
        tab <- KPplotHRtab_react()
        strata_col <- input$KPstrataCol
        if (isTruthy(strata_col)) {
          if (!is.null(strata_col)) {
            tab <- tab %>%
              gtsummary::tbl_regression(exp = TRUE,
                                        label = list(strata_col ~ strata_col)) %>%
              as_gt()
          } else {
            tab <- tab %>%
              gtsummary::tbl_regression(exp = TRUE) %>%
              as_gt()
          }
          tab_df <- as.data.frame(tab)
          tab_df <- tab_df %>%
            dplyr::select(label,estimate,ci,p.value)
          colnames(tab_df) <- c("Characteristic","Hazard Ratio","95% Confidence Interval","P.Value")
          tab_df <- sapply(tab_df,function(x) { gsub("<br />", "", x) })
          tab_df
          tab_df
        }
      })

      output$KPplotSummary <- renderPrint({
        req(KPplotHRtab_react())
        CoxPHobj <- KPplotHRtab_react()
        out <- capture.output(summary(CoxPHobj))
        xph <- capture.output(cox.zph(CoxPHobj))
        con_line <- grep("^Concordance=",out,value = T)
        lik_line <- grep("^Likelihood ratio test=",out,value = T)
        wal_line <- grep("^Wald test",out,value = T)
        sco_line <- grep("^Score ",out,value = T)
        text <- paste("CoxH Summary:",con_line,lik_line,wal_line,sco_line,"","Proportional Hazards assumption:",xph[1],xph[2],xph[3],sep = "\n")
        cat(text)
      })

      observe({
        AppTimeUnits <- GlobalAppTimeUnit_react()
        if (AppTimeUnits == "Months") {
          updateRadioButtons(session,"SurvYearOrMonth", selected = "Months")
        } else if (AppTimeUnits == "Years") {
          updateRadioButtons(session,"SurvYearOrMonth", selected = "Years")
        } else if (AppTimeUnits == "Days") {
          updateRadioButtons(session,"SurvYearOrMonth", selected = "Days")
        }


      })

      observe({
        req(input$SurvYearOrMonth)
        if (input$SurvYearOrMonth == "Months") {
          updateNumericInput(session,"SurvXaxisBreaks",label = "Survival X-Axis Breaks (Months):", value = 12, step = 1)
        } else if (input$SurvYearOrMonth == "Years") {
          updateNumericInput(session,"SurvXaxisBreaks",label = "Survival X-Axis Breaks (Years):", value = 1, step = 1)
        } else if (input$SurvYearOrMonth == "Days") {
          updateNumericInput(session,"SurvXaxisBreaks",label = "Survival X-Axis Breaks (Days):", value = 90, step = 15)
        }
      })

      cohort_TTE_table_KP_react <- reactive({
        req(cohort_TTE_table_strata())
        plot_df <- cohort_TTE_table_strata()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        strata_col <- input$KPstrataCol
        showMedSurv <- input$ShowMedSurvLine
        dOm <- input$SurvYearOrMonth
        xBreaks <- input$SurvXaxisBreaks
        SurvXaxis <- input$SurvXaxis
        ShowConfInt <- input$ShowConfInt


        #save(list = ls(), file = "cohort_TTE_table_KP_react.RData", envir = environment())


        if (nrow(plot_df) > 1) {
          strata_col <- input$KPstrataCol
          showMedSurv <- input$ShowMedSurvLine
          if (showMedSurv == T) {
            showMedSurv <- "hv"
          }
          else if (showMedSurv == F) {
            showMedSurv <- "none"
          }
          dOm <- input$SurvYearOrMonth

          scale_vec <- c("Days" = 1,"Years" = 365.25,"Months" = 30.4375)

          #scale <- ifelse(substr(AppTimeUnits,1,1)!=tolower((substr(dOm,1,1))),
          #                (paste0(substr(AppTimeUnits,1,1),"_",tolower((substr(dOm,1,1))))),
          #                scale_vec[[dOm]])

          scale <- scale_vec[[dOm]]
          xlab <- dOm
          xBreaks <- input$SurvXaxisBreaks

          xBreaks <- xBreaks*scale_vec[[dOm]]

          #if (dOm == "Years") {
          #  scale <- "d_y"
          #  xlab <- "Years"
          #} else {
          #  scale <- "d_m"
          #  xlab <- "Months"
          #}
          #if (input$SurvYearOrMonth == "Years") {
          #  xBreaks <- input$SurvXaxisBreaks * 365.25
          #} else {
          #  xBreaks <- input$SurvXaxisBreaks * 30.4375
          #}
          if (max(plot_df$time) < xBreaks) {
            xBreaks <- NULL
          }
          if (isTruthy(input$SurvXaxis)) {
            if (input$SurvYearOrMonth == "Years") {
              xaxlim <- input$SurvXaxis * 365.25
            } else if (input$SurvYearOrMonth == "Months") {
              xaxlim <- input$SurvXaxis * 30.4375
            } else {
              xaxlim <- input$SurvXaxis
            }
          } else {
            xaxlim <- NULL
          }
          fit <- NULL
          #save(list = ls(), file = "cohort_TTE_table_KP_react.RData", envir = environment())
          if (ncol(plot_df) == 4) {
            if (length(plot_df[,strata_col][which(is.na(plot_df[,strata_col]))]) < nrow(plot_df)) {
              colnames(plot_df)[which(colnames(plot_df) == strata_col)] <- "Feature"
              form <- paste0("Surv(time,status) ~ Feature")
              fit <- eval(substitute(survfit(as.formula(form),data = plot_df, type="kaplan-meier")))
              #names(fit[["strata"]]) <- gsub("^Feature=",paste0(strata_col,"="),names(fit[["strata"]]))
              if (isTruthy(names(fit[["strata"]]))) {
                names(fit[["strata"]]) <- gsub("^Feature="," ",names(fit[["strata"]]))
                names(fit[["strata"]]) <- gsub("_"," ",names(fit[["strata"]]))
                names(fit[["strata"]]) <- str_wrap(names(fit[["strata"]]),width = 25, whitespace_only = FALSE)
              }
              #colnames(plot_df)[which(colnames(plot_df) == strata_col)] <- sprintf(ifelse(grepl(" ", strata_col), "`%s`", "%s"), strata_col)
              #strata_col <- sprintf(ifelse(grepl(" ", strata_col), "`%s`", "%s"), strata_col)
              #form <- as.formula(paste0("Surv(time,status) ~ ",strata_col))
              #fit <- eval(substitute(survfit(form,data = plot_df, type="kaplan-meier")))
              pvalSel <- input$ShowPval
              legpos <- input$SurvLegendPos
            }
          } else {
            fit <- survfit(Surv(time,status) ~ 1, data = plot_df, type = "kaplan-meier")
            pvalSel <- FALSE
            legpos <- "none"
          }
          if (isTruthy(fit)) {
            ggsurv <- survminer::ggsurvplot(fit, data = plot_df, risk.table = TRUE,
                                            #title = title,
                                            xscale = scale,
                                            break.time.by=xBreaks,
                                            xlab = xlab,
                                            ylab = "Event Free Survival Probability",
                                            submain = "Based on Kaplan-Meier estimates",
                                            caption = "created with survminer",
                                            pval = pvalSel,
                                            #conf.int <- FALSE,
                                            conf.int = ShowConfInt,
                                            ggtheme = theme_bw(),
                                            font.title = c(16, "bold"),
                                            font.submain = c(12, "italic"),
                                            font.caption = c(12, "plain"),
                                            font.x = c(14, "plain"),
                                            font.y = c(14, "plain"),
                                            font.tickslab = c(12, "plain"),
                                            legend = legpos,
                                            risk.table.height = 0.30,
                                            surv.median.line = showMedSurv
            )
            if (showMedSurv != "none") {
              MedSurvItem <- ggsurv[["plot"]][["layers"]][length(ggsurv[["plot"]][["layers"]])]
              MedSurvItem_df <- MedSurvItem[[1]][["data"]]
              MedSurvItem_df <- MedSurvItem_df[order(MedSurvItem_df[,1]),]
              MedSurvItem_df <- MedSurvItem_df %>%
                mutate(label = paste(round(MedSurvItem_df[,1]),"Days"))
              rownames(MedSurvItem_df) <- 1:nrow(MedSurvItem_df)
              if (nrow(MedSurvItem_df) >= 1) {
                ggsurv$plot <- ggsurv$plot +
                  geom_label_repel(data = MedSurvItem_df, aes(x = x1, y = y1, label = label, size = 4), label.size = NA, show.legend = FALSE)
              }
            }
            if (!is.null(xaxlim)) {
              ggsurv$plot$coordinates$limits$x <- c(0,xaxlim)
              ggsurv$table$coordinates$limits$x <- c(0,xaxlim)
            }
            ggsurv
          }
        }

      })

      output$KPplot <- renderPlot({
        p <- cohort_TTE_table_KP_react()
        p
      })

      KPplotTable_out <- reactive({
        req(cohort_TTE_table_wInfo())
        plot_df <- cohort_TTE_table_wInfo()
        plot_df_strat <- cohort_TTE_table_strata()
        AppTimeUnits <- tolower(GlobalAppTimeUnit_react())
        #save(list = ls(), file = "KPplotTable_out.RData", envir = environment())
        if (isTruthy(plot_df_strat)) {
          strata_col <- input$KPstrataCol
          plot_df <- merge(plot_df,plot_df_strat,all = T)
          plot_df <- plot_df %>%
            relocate(any_of(c("Name","status","time","Start_Event","Progression_Event",strata_col,
                              "Start_Time_Point","End_Time_Point","Final_Time_Point"))) %>%
            as.data.frame()
          plot_df$time <- convert_time_units(plot_df$time,"days",AppTimeUnits)
          plot_df
        } else {
          plot_df <- plot_df %>%
            relocate(any_of(c("Name","status","time","Start_Event","Progression_Event",strata_col,
                              "Start_Time_Point","End_Time_Point","Final_Time_Point"))) %>%
            as.data.frame()
          plot_df$time <- convert_time_units(plot_df$time,"days",AppTimeUnits)
          plot_df
        }
      })

      output$KPplotTable <- DT::renderDataTable({
        req(KPplotTable_out())
        plot_df <- KPplotTable_out()
        if (nrow(plot_df) > 0) {
          DT::datatable(plot_df,
                        escape = F,
                        class = "display nowrap",
                        extensions = 'ColReorder',
                        options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                       pageLength = 10,
                                       scrollX = T,
                                       target = "cell",
                                       colReorder = TRUE),
                        rownames = F) %>%
            formatRound(columns = 3, digits = 4)
        }
      })

      output$dnldKPplotTable <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_",Project_Name),"_KP_Table_",Sys.Date(),".txt")
        },
        content = function(file) {
          req(KPplotTable_out())
          plot_df <- KPplotTable_out()
          write.table(plot_df,file, sep = '\t', row.names = F)
        }
      )

      output$dnldKPplot <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_",Project_Name),"_KP_Plot_",Sys.Date(),".svg")
        },
        content = function(file) {
          p <- cohort_TTE_table_KP_react()
          ggplot2::ggsave(filename = file, plot = p$plot,
                          width = input$PlotDnldWidth, height = input$PlotDnldHight,
                          units = input$PlotDnldUnits)
        }
      )


      ## Swim --------------------------------------------------------------


      swimmer_plot_df <- reactive({

        req(cohort_EventSumm_df())
        Patient_Event_Data <- cohort_EventSumm_df()

        req(start_events_react())
        req(end_events_react())
        start_events <- start_events_react()
        stop_events <- end_events_react()


        if (nrow(Patient_Event_Data) > 0) {

          Patient_Event_Data2 <- Patient_Event_Data %>%
            group_by(Name) %>%
            mutate(swimmer_event = case_when(
              Event %in% start_events & EventStart == Start_Time_Point ~ "Start Event",
              EventStart == min(EventStart,na.rm = T) ~ "First Recorded Time Point",
              Event %in% stop_events & EventStart == End_Time_Point ~ "Progression Event",
              EventEnd == Final_Time_Point ~ "Final Recorded Time Point"
            )) %>%
            relocate(swimmer_event, .after = Name) %>%
            ungroup()

          event_types <- c("First Recorded Time Point", "Start Event", "Progression Event", "Final Recorded Time Point")

          Patient_Event_Data3 <- Patient_Event_Data2 %>%
            filter(!is.na(swimmer_event)) %>%
            arrange(Name,EventStart) %>%
            group_by(Name, swimmer_event) %>%
            mutate(Event_Details = paste0(unique(Event), collapse = "\n")) %>%
            select(Name, swimmer_event,Event_Details,EventStart,Start_Time_Point,End_Time_Point,Final_Time_Point) %>%
            unique() %>%
            mutate(EventTime = case_when(
              swimmer_event == "First Recorded Time Point" ~ min(unique(EventStart,na.rm = T)),
              swimmer_event == "Start Event" ~ min(unique(Start_Time_Point,na.rm = T)),
              swimmer_event == "Progression Event" ~ min(unique(End_Time_Point,na.rm = T)),
              swimmer_event == "Final Recorded Time Point" ~ min(unique(Final_Time_Point,na.rm = T))
            )) %>%
            ungroup() %>%
            group_by(Name) %>%
            mutate(EventTime_FromZero = EventTime - min(EventTime,na.rm = T)) %>%
            select(Name, swimmer_event,Event_Details,EventTime,EventTime_FromZero) %>%
            complete(swimmer_event = event_types, fill = list()) %>%
            mutate(swimmer_event = factor(swimmer_event, levels = event_types)) %>%
            arrange(Name, swimmer_event) %>%
            unique() %>%
            as.data.frame()
          NonProg_Pats <- Patient_Event_Data3[which(Patient_Event_Data3$swimmer_event == "Progression Event" & is.na(Patient_Event_Data3$Event_Details)),1]


          Patient_Event_Data3 <- Patient_Event_Data3 %>%
            group_by(Name) %>%
            fill(Event_Details,EventTime,EventTime_FromZero, .direction = "up") %>%
            ungroup() %>%
            mutate(across(c(Event_Details,EventTime,EventTime_FromZero), ~ ifelse(swimmer_event == "Progression Event" & Name %in% NonProg_Pats,NA,.))) %>%
            mutate(Event_Details = paste0(paste0("<b>",swimmer_event,"</b> - ",round(EventTime_FromZero,3),"\n"),
                                          paste0("Age - ",round(EventTime,3),"\n"),
                                          Event_Details,sep = "\n")) %>%
            as.data.frame() %>%
            group_by(Name,EventTime,EventTime_FromZero) %>%
            mutate(Event_Details = paste0("<b>",Name,"</b>\n",paste(unique(Event_Details), collapse = "\n"))) %>%
            ungroup() %>%
            as.data.frame()

          Patient_Event_Data3[which(Patient_Event_Data3[,1] %in% NonProg_Pats & Patient_Event_Data3$swimmer_event == "Progression Event"),c(3,4,5)] <- NA

          Patient_Event_Data3_cast <- Patient_Event_Data3 %>%
            mutate(row_index = row_number()) %>%
            pivot_wider(
              id_cols = c(Name,row_index,Event_Details),
              names_from = swimmer_event,
              values_from = EventTime_FromZero
            ) %>%
            select(-row_index,-Event_Details) %>%
            group_by(Name) %>%
            summarize_all(~ max(., na.rm = TRUE)) %>%
            as.data.frame()
          Patient_Event_Data3_cast[] <- Map(function(x) replace(x, is.infinite(x), NA), Patient_Event_Data3_cast)

          Patient_Event_Data3_cast$Index <- as.numeric(factor(Patient_Event_Data3_cast[,1]))

          Patient_Event_Data3_cast_det <- merge(Patient_Event_Data3_cast,Patient_Event_Data3[,-c(4,5)], all = T)
          colnames(Patient_Event_Data3_cast_det)[which(colnames(Patient_Event_Data3_cast_det) == "swimmer_event")] <- "Event"

          if (ncol(Patient_Event_Data3_cast_det) == 8) {
            ply_df <- Patient_Event_Data3_cast_det
            colnames(ply_df) <- c("Name","First_Time_Point","Start_Time_Point","Prog_Time_Point","Final_Time_Point","Index","Event","Event_Details")
            ply_df
          }
        }
      })

      observe({
        req(swimmer_plot_df())
        ply_df <- swimmer_plot_df()
        if (nrow(ply_df) > 0) {
          pat_choices <- unique(ply_df$Name)
          updateSelectizeInput(session,"swimmerPatSpec",choices = pat_choices, selected = NA, server = T)
        }
      })


      observe({
        req(swimmer_plot_df())
        ply_df <- swimmer_plot_df()
        swimmerShowAll <- input$swimmerShowAll
        if (swimmerShowAll) {
          updateNumericInput(session,"maxpatview",value = length(unique(ply_df$Name)))
        }
      })

      swimmer_plot_react <- reactive({
        req(swimmer_plot_df())
        req(input$swimmerSort)
        ply_df <- swimmer_plot_df()

        swimmSort <- input$swimmerSort
        maxpats <- input$maxpatview
        patientsSpec <- input$swimmerPatSpec
        swimmerShowAll <- input$swimmerShowAll

        if ((swimmerShowAll & maxpats == length(unique(ply_df$Name))) | !swimmerShowAll) {
          # sort to get correct head of patients
          if (swimmSort == "Time - Ascending") {
            ply_df <- ply_df[order(ply_df$Final_Time_Point),]
          } else if (swimmSort == "Time - Descending") {
            ply_df <- ply_df[order(ply_df$Final_Time_Point, decreasing = T),]
          } else if (swimmSort == "Alphabetical - Ascending") {
            ply_df <- ply_df[order(ply_df$Name, decreasing = T),]
          } else if (swimmSort == "Alphabetical - Descending") {
            ply_df <- ply_df[order(ply_df$Name),]
          }

          if (maxpats >= 1) {
            if (length(patientsSpec) > 0) {
              maxpats <- maxpats-length(patientsSpec)
              get_pats <- c(patientsSpec,head(unique(ply_df$Name),maxpats))
              ply_df <- ply_df[which(ply_df$Name %in% get_pats),]
            } else {
              if (length(unique(ply_df$Name)) > maxpats) {
                get_pats <- head(unique(ply_df$Name),maxpats)
                ply_df <- ply_df[which(ply_df$Name %in% get_pats),]
              }
            }
            # sort to display in correct visual order (needs to be opposite)
            if (swimmSort == "Time - Ascending") {
              ply_df <- ply_df[order(ply_df$Final_Time_Point, decreasing = T),]
            } else if (swimmSort == "Time - Descending") {
              ply_df <- ply_df[order(ply_df$Final_Time_Point),]
            } else if (swimmSort == "Alphabetical - Ascending") {
              ply_df <- ply_df[order(ply_df$Name, decreasing = T),]
            } else if (swimmSort == "Alphabetical - Descending") {
              ply_df <- ply_df[order(ply_df$Name),]
            }
            patientIDs <- unique(ply_df[,1])
            ply_df$Index <- as.numeric(factor(ply_df$Name, levels = patientIDs))
            patients <- unique(ply_df$Index)
            showlegend1 <- TRUE
            showlegend2 <- TRUE
            showlegend3 <- TRUE
            p <- plot_ly()
            # Loop through patients
            for (patient in patients) {
              df_patient <- ply_df[ply_df$Index == patient, ]

              # Line segment: First_Time_Point to Start_Time_Point
              p <- add_trace(p,
                             x = c(df_patient$First_Time_Point[1], df_patient$Start_Time_Point[1]),
                             y = c(patient, patient),
                             type = 'scatter',
                             mode = 'lines',
                             line = list(color = '#5F9EA0', width = 4),
                             showlegend = FALSE)

              # Line segment: Start_Time_Point to Final_Time_Point
              if (!is.na(df_patient$Prog_Time_Point[1])) {
                p <- add_trace(p,
                               x = c(df_patient$Start_Time_Point[1], df_patient$Prog_Time_Point[1]),
                               y = c(patient, patient),
                               type = 'scatter',
                               mode = 'lines',
                               line = list(color = 'tomato', width = 4),
                               showlegend = FALSE)
                p <- add_trace(p,
                               x = c(df_patient$Prog_Time_Point[1], df_patient$Final_Time_Point[1]),
                               y = c(patient, patient),
                               type = 'scatter',
                               mode = 'lines',
                               line = list(color = '#5F9EA0', width = 4),
                               showlegend = FALSE)
              } else {
                p <- add_trace(p,
                               x = c(df_patient$Start_Time_Point[1], df_patient$Final_Time_Point[1]),
                               y = c(patient, patient),
                               type = 'scatter',
                               mode = 'lines',
                               line = list(color = 'tomato', width = 4),
                               showlegend = FALSE)
              }

              # First Time Point marker (line)
              p <- add_trace(p,
                             x = df_patient$First_Time_Point[1],
                             y = patient,
                             type = 'scatter',
                             mode = 'markers',
                             marker = list(symbol = 'line-ns', size = 15, color = "#5F9EA0"),
                             text = df_patient$Event_Details[df_patient$Event == "First Recorded Time Point"],
                             hoverinfo = 'text',
                             showlegend = FALSE)

              # Start_Time_Point marker (Open Circle)
              p <- add_trace(p,
                             x = df_patient$Start_Time_Point[1],
                             y = patient,
                             type = 'scatter',
                             mode = 'markers',
                             marker = list(symbol = 'circle', size = 15, color = "#228B22"),
                             text = df_patient$Event_Details[df_patient$Event == "Start Event"],
                             hoverinfo = 'text',
                             #showlegend = TRUE,
                             showlegend = (function(){
                               if(showlegend1){
                                 showlegend1 <<- FALSE
                                 return(TRUE)
                               } else {
                                 return(FALSE)
                               }
                             })(),
                             name = "Start Event")

              # Progression Event marker (X symbol)
              if (!is.na(df_patient$Prog_Time_Point[1])) {
                p <- add_trace(p,
                               x = df_patient$Prog_Time_Point[1],
                               y = patient,
                               type = 'scatter',
                               mode = 'markers',
                               marker = list(symbol = 'x', size = 15, color = "#8B1A1A"),
                               text = df_patient$Event_Details[df_patient$Event == "Progression Event"],
                               hoverinfo = 'text',
                               #showlegend = TRUE,
                               showlegend = (function(){
                                 if(showlegend2){
                                   showlegend2 <<- FALSE
                                   return(TRUE)
                                 } else {
                                   return(FALSE)
                                 }
                               })(),
                               name = "Progression Event")

                # Final Time Point marker (line)
                p <- add_trace(p,
                               x = df_patient$Final_Time_Point[1],
                               y = patient,
                               type = 'scatter',
                               mode = 'markers',
                               marker = list(symbol = 'line-ns', size = 15, color = "#5F9EA0"),
                               text = df_patient$Event_Details[df_patient$Event == "Final Recorded Time Point"],
                               hoverinfo = 'text',
                               showlegend = FALSE)
              } else {
                # If no progression event, Final_Time_Point gets a triangle-right marker
                p <- add_trace(p,
                               x = df_patient$Final_Time_Point[1],
                               y = patient,
                               type = 'scatter',
                               mode = 'markers',
                               marker = list(symbol = 'triangle-right', size = 20, color = 'tomato'),
                               text = df_patient$Event_Details[df_patient$Event == "Final Recorded Time Point"],
                               hoverinfo = 'text',
                               #showlegend = TRUE,
                               showlegend = (function(){
                                 if(showlegend3){
                                   showlegend3 <<- FALSE
                                   return(TRUE)
                                 } else {
                                   return(FALSE)
                                 }
                               })(),
                               name = "Continued Response")
              }
            }

            # Customize layout
            p_out <- layout(p,
                            xaxis = list(title = 'Time (Years)', zeroline = FALSE, titlefont = list(size = 20), tickfont = list(size = 15)),
                            yaxis = list(title = 'Patients', tickvals = patients, ticktext = patientIDs,
                                         titlefont = list(size = 20), tickfont = list(size = 15)),
                            showlegend = TRUE) %>%
              config(
                toImageButtonOptions = list(
                  format = "svg",
                  height = input$TTESwimmerHeight,
                  width = input$TTESwimmerWidth,
                  filename = paste0("Time_To_Event_Swimmers_",Sys.Date())
                )
              )

            p_out

          }
        }




      })

      output$swimmer_plot <- renderPlotly({
        req(swimmer_plot_react())
        p <- swimmer_plot_react()
        p
      })

      swimmer_plotTable_out <- reactive({
        req(cohort_TTE_table_wInfo())
        req(swimmer_plot_df())
        plot_df <- cohort_TTE_table_wInfo()
        plot_df_strat <- cohort_TTE_table_strata()
        swimm_df <- swimmer_plot_df()
        strata_col <- input$KPstrataCol
        #save(list = ls(), file = "swimm_df_out.RData", envir = environment())
        plot_df <- plot_df[,c("Name","Start_Event","Progression_Event")]
        swimm_df <- unique(swimm_df[,c(1:5)])
        plot_df <- merge(plot_df,plot_df_strat,all = T)
        plot_df <- merge(plot_df,swimm_df, all = T)

        plot_df <- plot_df %>%
          #relocate(any_of(c("Name","status","time","Start_Event","Progression_Event",strata_col,
          #                  "Start_Time_Point","End_Time_Point","Final_Time_Point"))) %>%
          select(any_of(c("Name","Start_Event","Progression_Event",strata_col,
                          "First_Time_Point","Start_Time_Point","Prog_Time_Point","Final_Time_Point"))) %>%
          as.data.frame()
        colnames(plot_df) <- gsub("_"," ",colnames(plot_df))
        plot_df
      })

      output$swimmer_plotTable <- DT::renderDataTable({
        req(swimmer_plotTable_out())
        plot_df <- swimmer_plotTable_out()
        num_cols <- sapply(colnames(plot_df),function(x){
          is.numeric(plot_df[,x])
        })
        if (nrow(plot_df) > 0) {
          DT::datatable(plot_df,
                        escape = F,
                        class = "display nowrap",
                        extensions = 'ColReorder',
                        options = list(lengthMenu = c(5, 10, 20, 100, 1000),
                                       pageLength = 10,
                                       scrollX = T,
                                       target = "cell",
                                       colReorder = TRUE),
                        rownames = F) %>%
            formatRound(columns = names(num_cols[which(num_cols==TRUE)]), digits = 3)
        }
      })

      output$dnldswimmer_plotTable <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_",Project_Name),"_KP_Table_",Sys.Date(),".txt")
        },
        content = function(file) {
          req(swimmer_plotTable_out())
          plot_df <- swimmer_plotTable_out()
          write.table(plot_df,file, sep = '\t', row.names = F)
        }
      )





      ## Power -----------------------------------------------------------------

      powerCTAnalysis_steps_react <- reactive({
        #paste("Calculating...")
        SampleSizeA_init = input$SampleSizeA_Init_4;
        SampleSizeA_step = input$SampleSizeAStep_4;
        SampleSizeA_max = input$SampleSizeAMax_4;
        SampleSizeB = input$SampleSizeB_4;

        failureRateA = input$FrequencyA_4;
        failureRateB = input$FrequencyB_4;

        hazard_ratio1 = input$HazardRatio1_4;
        hazard_ratio2 = input$HazardRatio2_4;
        hazard_ratio3 = input$HazardRatio3_4;
        sig_level = input$SigLevel_4;

        sample_sizes = seq(from = SampleSizeA_init, to = SampleSizeA_max, by = SampleSizeA_step);
        power_levels = sample_sizes;
        final_M = vector();
        i = 1;
        for (SampleSizeA in seq(from = SampleSizeA_init, to = SampleSizeA_max, by = SampleSizeA_step)) {
          power_levels[i] = powerCT.default(SampleSizeA, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio1, alpha = sig_level);
          i = i + 1;
        }
        final_M = rbind(final_M, cbind(power_levels, sample_sizes, hazard_ratio1));
        i = 1;
        for (SampleSizeA in seq(from = SampleSizeA_init, to = SampleSizeA_max, by = SampleSizeA_step)) {
          power_levels[i] = powerCT.default(SampleSizeA, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio2, alpha = sig_level);
          i = i + 1;
        }
        final_M = rbind(final_M, cbind(power_levels, sample_sizes, hazard_ratio2));
        i = 1;
        for (SampleSizeA in seq(from = SampleSizeA_init, to = SampleSizeA_max, by = SampleSizeA_step)) {
          power_levels[i] = powerCT.default(SampleSizeA, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio3, alpha = sig_level);
          i = i + 1;
        }
        final_M = rbind(final_M, cbind(power_levels, sample_sizes, hazard_ratio3));

        final_M = data.frame(final_M)
        colnames(final_M) = c("power_levels", "Group_A_size", "HazardRatio");
        final_M$HazardRatio = as.factor(as.character(final_M$HazardRatio))
        p = ggplot(data=final_M, aes(x=power_levels, y=Group_A_size, group=HazardRatio, color=HazardRatio)) + geom_line() + geom_vline(xintercept=0.8, linetype="dashed", color="red", size=1) + theme(text = element_text(size = 25)) ;
        p;

      })

      output$powerCTAnalysis_steps <- renderPlot({

        p <- powerCTAnalysis_steps_react()
        p

      })

      output$powerCTAnalysis_report <- renderText({
        SampleSizeA_init = input$SampleSizeA_Init_4;
        SampleSizeA_max = input$SampleSizeAMax_4;
        SampleSizeB = input$SampleSizeB_4;

        failureRateA = input$FrequencyA_4;
        failureRateB = input$FrequencyB_4;

        hazard_ratio1 = input$HazardRatio1_4;
        hazard_ratio2 = input$HazardRatio2_4;
        hazard_ratio3 = input$HazardRatio3_4;

        sig_level = input$SigLevel_4;

        power1 = powerCT.default(SampleSizeA_init, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio1, alpha = sig_level);
        power2 = powerCT.default(SampleSizeA_init, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio2, alpha = sig_level);
        power3 = powerCT.default(SampleSizeA_init, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio3, alpha = sig_level);
        text = "";
        text = c(text, paste("Group A N = ", SampleSizeA_init, "\n"))
        text = c(text, paste("Grp A FailureRate = ", failureRateA, "\n"))
        text = c(text, paste("Group B N = ", SampleSizeB, "\n"))
        text = c(text, paste("Grp B FailureRate = ", failureRateB, "\n"))
        text = c(text, paste(" Hazard_Ratio 1 = ", hazard_ratio1, "\n"))
        text = c(text, paste("   Estimated power_level 1: ", power1, "\n"))
        text = c(text, paste(" Hazard_Ratio 2 = ", hazard_ratio2, "\n"))
        text = c(text, paste("   Estimated power_level 2: ", power2, "\n"))
        text = c(text, paste(" Hazard_Ratio 3 = ", hazard_ratio3, "\n"))
        text = c(text, paste("   Estimated power_level 3: ", power3, "\n\n"))

        power1 = powerCT.default(SampleSizeA_max, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio1, alpha = sig_level);
        power2 = powerCT.default(SampleSizeA_max, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio2, alpha = sig_level);
        power3 = powerCT.default(SampleSizeA_max, SampleSizeB, pE = failureRateA, pC = failureRateB, RR = hazard_ratio3, alpha = sig_level);
        text = c(text, paste("Group A N = ", SampleSizeA_max, "\n"))
        text = c(text, paste("Grp A FailureRate = ", failureRateA, "\n"))
        text = c(text, paste("Group B N = ", SampleSizeB, "\n"))
        text = c(text, paste("Grp B FailureRate = ", failureRateB, "\n"))
        text = c(text, paste(" Hazard_Ratio 1 = ", hazard_ratio1, "\n"))
        text = c(text, paste("   Estimated power_level 1: ", power1, "\n"))
        text = c(text, paste(" Hazard_Ratio 2 = ", hazard_ratio2, "\n"))
        text = c(text, paste("   Estimated power_level 2: ", power2, "\n"))
        text = c(text, paste(" Hazard_Ratio 3 = ", hazard_ratio3, "\n"))
        text = c(text, paste("   Estimated power_level 3: ", power3, "\n"))

        text;
      })

      output$dnldpowerCTAnalysis <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_",Project_Name),"_Power_Plot_",Sys.Date(),".svg")
        },
        content = function(file) {
          p <- powerCTAnalysis_steps_react()
          ggplot2::ggsave(filename = file, plot = p,
                          width = input$PowPlotDnldWidth, height = input$PowPlotDnldHight,
                          units = input$PowPlotDnldUnits)
        }
      )

      # Cohort Summary ---------------------------------------------------------

      #shiny::observe({
      #
      #  Clin_Supp_Cols_List_mod <- setNames(lapply(names(Clin_Supp_Cols_List_react()), function(x){
      #    setNames(paste0(x, "_", Clin_Supp_Cols_List_react()[[x]]), Clin_Supp_Cols_List_react()[[x]])
      #  }), names(Clin_Supp_Cols_List_react()))
      #
      #  updateSelectizeInput(session = session, inputId = "SubsetPatientDataSumm",
      #                       choices = Clin_Supp_Cols_List_mod,
      #                       selected = Clin_Supp_Cols_List_mod[1],
      #                       options = list(
      #                         placeholder = 'Please select an option below',
      #                         onInitialize = I('function() { this.setValue(""); }')
      #                       ),
      #                       server = T
      #  )
      #
      #})

      ## Render patient subset criteria
      #output$rendPatientSubsetCriteriaSumm <- renderUI({
      #
      #  req(input$SubsetPatientDataSumm)
      #  SubsetColSelec <- input$SubsetPatientDataSumm
      #  SubsetCol_dfName <- strsplit(SubsetColSelec,"_")[[1]][1]
      #  SubsetCol_Name <- paste(strsplit(SubsetColSelec,"_")[[1]][-1], collapse = "_")
      #  SubsetCol_Opts <- unique(wkbk_react_sub()[[SubsetCol_dfName]][,SubsetCol_Name])
      #  selectInput("PatientSubsetCriteriaSumm","Patient Criteria:", choices = SubsetCol_Opts)
      #
      #})

      ## Generate patient list based on subset criteria
      #Patient_List_React_summ <- reactive({
      #
      #  if (!is.null(input$SubsetPatientDataSumm)) {
      #    SubsetColSelec <- input$SubsetPatientDataSumm
      #    Patient_Annotation <- pat_react()
      #    if (SubsetColSelec == "") {
      #      PatientsSelected <- Patient_Annotation[,1]
      #      PatientsSelected
      #    } else {
      #      SubsetCritSelec <- input$PatientSubsetCriteriaSumm
      #      SubsetCol_dfName <- strsplit(SubsetColSelec,"_")[[1]][1]
      #      SubsetCol_Name <- paste(strsplit(SubsetColSelec,"_")[[1]][-1], collapse = "_")
      #      SubsetCol_Opts <- unique(wkbk_react_sub()[[SubsetCol_dfName]][,SubsetCol_Name])
      #      PatientsSelected <- wkbk_react_sub()[[SubsetCol_dfName]][which(wkbk_react_sub()[[SubsetCol_dfName]][,SubsetCol_Name] == SubsetCritSelec),1]
      #      PatientsSelected
      #    }
      #  }
      #
      #})

      observe({
        req(CohortEventSumm_Table())
        df <- CohortEventSumm_Table()
        updateSelectizeInput(session,"SummaryOptions",choices = unique(df[,2]), selected = unique(df[,2]), server = T)
      })

      CohortEventSumm_Table <- reactive({
        req(event_data())
        #req(Patient_List_React_summ())
        #pat_list <- Patient_List_React_summ()
        event_data <- event_data()
        event_data <- event_data[which(is.na(event_data$EventTab)),]
        event_data_summ <- event_data[grepl(" Cluster$",event_data[,2]),]
        #event_data_summ <- event_data[grepl(" Summary$",event_data[,3]),]
        #event_data_summ <- event_data_summ[which(event_data_summ[,1] %in% pat_list),]
        event_data_summ

      })

      ## Render Patient Selection Table
      output$CohortEventSummaryTable <- DT::renderDataTable({

        req(CohortEventSumm_Table())
        req(input$SummaryOptions)
        Patient_Table_df <- CohortEventSumm_Table()
        Events_Chose <- input$SummaryOptions
        Patient_Table_df <- Patient_Table_df[which(Patient_Table_df[,2] %in% Events_Chose),]
        DT::datatable(Patient_Table_df,
                      extensions = 'Scroller',
                      options = list(lengthMenu = c(5,10, 20, 100, 1000),
                                     pageLength = 10,
                                     scrollX = T),
                      selection = list(mode = 'single', selected = 1),
                      rownames = F
        )

      })

      output$dlndCohortEventSummaryTable <- downloadHandler(
        filename = function() {
          paste0(gsub(" ","_",Project_Name),"_Event_Summary_",Sys.Date(),".txt")
        },
        content = function(file) {
          Patient_Table_df_in <- CohortEventSumm_Table()
          Patient_Table_df <- CohortEventSumm_Table()
          Events_Chose <- input$SummaryOptions
          Patient_Table_df <- Patient_Table_df[which(Patient_Table_df[,2] %in% Events_Chose),]
          write.table(Patient_Table_df,file, sep = '\t', row.names = F)
        }
      )

      ## Swim --------------------------------------------------------------

      output$rendsumm_swimmer_plot <- renderUI({
        req(summ_swimmer_plot_react())
        req(input$SwimmPlotHeight)
        plot_height <- paste0(input$SwimmPlotHeight,"px")
        shinycssloaders::withSpinner(shinyjqui::jqui_resizable(plotlyOutput("summ_swimmer_plot",height = plot_height, width = "100%")),type = 6)
      })

      summ_swimmer_plot_df <- reactive({
        req(event_data_key())
        #req(summary_cluster_window())
        req(input$SummaryOptions)
        event_data_key <- event_data_key()
        #pat_list <- Patient_List_React_summ()
        Patient_Event_Data <- event_data()
        #SummLein_in <- summary_cluster_window()
        events_to_plot <- input$SummaryOptions

        #Patient_Event_Data <- Patient_Event_Data[which(Patient_Event_Data$Name %in% pat_list),]
        Patient_Event_Data_summ <- Patient_Event_Data[which(Patient_Event_Data$Event %in% events_to_plot),]

        summ_text_cols <- grep("Summary$",colnames(Patient_Event_Data_summ),value = T)

        if (length(summ_text_cols) > 0) {
          Patient_Event_Data_summ <- Patient_Event_Data_summ %>%
            mutate(Event_Details = coalesce(!!!syms(summ_text_cols))) %>%
            select(-any_of(summ_text_cols))
        } else {
          Patient_Event_Data_summ <- Patient_Event_Data_summ %>%
            mutate(Event_Details = NA)
        }

        Patient_Event_Data_summ2 <- Patient_Event_Data_summ %>%
          arrange(Name,EventStart) %>%
          select(-c(EventType,EventTab,EventColumn)) %>%
          as.data.frame()

        Patient_Event_Data_fl <- Patient_Event_Data %>%
          group_by(Name) %>%
          mutate(Event_New = case_when(
            EventStart == min(EventStart,na.rm = T) ~ "First Recorded Time Point",
            EventStart == max(EventStart,na.rm = T) ~ "Final Recorded Time Point"
          )) %>%
          ungroup() %>%
          select(Name,Event,Event_New,EventStart,EventEnd) %>%
          filter(!is.na(Event_New)) %>%
          arrange(Name, EventStart) %>%
          group_by(Name, Event_New) %>%
          mutate(Event_Details = paste0(unique(Event), collapse = "\n")) %>%
          ungroup() %>%
          mutate(Event = Event_New) %>%
          select(Name,Event,EventStart,EventEnd,Event_Details) %>%
          unique() %>%
          as.data.frame()

        Patient_Event_Data_summ3 <- merge(Patient_Event_Data_summ2,Patient_Event_Data_fl,all = T)

        #save(list = ls(), file = "Big_Genie_Fail.RData", envir = environment())

        Patient_Event_Data_summ3 <- Patient_Event_Data_summ3 %>%
          arrange(Name,EventStart) %>%
          group_by(Name) %>%
          mutate(EventTime_FromZero = EventStart - min(EventStart,na.rm = T)) %>%
          mutate(FirstEventTime_FromZero = 0.00) %>%
          mutate(FinalEventTime_FromZero = max(EventEnd[which(Event == "Final Recorded Time Point")], na.rm = T) - min(EventStart,na.rm = T)) %>%
          ungroup() %>%
          mutate(Event_Details = paste0(paste0("<b>",Event,"</b> - ",round(EventTime_FromZero,3),"\n"),
                                        paste0("Age at Event Start - ",round(EventStart,3),"\n"),
                                        Event_Details,sep = "\n")) %>%
          select(-EventStart,-EventEnd) %>%
          unique() %>%
          group_by(Name,EventTime_FromZero) %>%
          mutate(Event_Details = paste0("<b>",Name,"</b>\n",paste(unique(Event_Details), collapse = "\n"))) %>%
          ungroup() %>%
          as.data.frame()

        Patient_Event_Data_summ3$Index <- as.numeric(factor(Patient_Event_Data_summ3$Name))

        ply_df <- Patient_Event_Data_summ3
        ply_df
      })


      observe({
        req(summ_swimmer_plot_df())
        ply_df <- summ_swimmer_plot_df()
        if (nrow(ply_df) > 0) {
          pat_choices <- unique(ply_df$Name)
          updateSelectizeInput(session,"swimmerPatSpecSumm",choices = pat_choices, selected = NA, server = T)
        }
      })

      observe({
        req(summ_swimmer_plot_df())
        ply_df <- summ_swimmer_plot_df()
        swimmerShowAllSumm <- input$swimmerShowAllSumm
        if (swimmerShowAllSumm) {
          updateNumericInput(session,"maxpatviewSumm",value = length(unique(ply_df$Name)))
        }
      })

      summ_swimmer_plot_react <- reactive({
        req(summ_swimmer_plot_df())
        req(input$swimmerSortSumm)
        req(input$maxpatviewSumm)
        ply_df <- summ_swimmer_plot_df()

        swimmSort <- input$swimmerSortSumm
        maxpats <- input$maxpatviewSumm
        patientsSpec <- input$swimmerPatSpecSumm
        swimmerShowAllSumm <- input$swimmerShowAllSumm


        if ((swimmerShowAllSumm & maxpats == length(unique(ply_df$Name))) | !swimmerShowAllSumm) {
          # sort to get correct head of patients
          if (swimmSort == "Time - Ascending") {
            ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero),]
          } else if (swimmSort == "Time - Descending") {
            ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero, decreasing = T),]
          } else if (swimmSort == "Alphabetical - Ascending") {
            ply_df <- ply_df[order(ply_df$Name, decreasing = T),]
          } else if (swimmSort == "Alphabetical - Descending") {
            ply_df <- ply_df[order(ply_df$Name),]
          }

          if (maxpats >= 1) {
            if (length(patientsSpec) > 0) {
              maxpats <- maxpats-length(patientsSpec)
              get_pats <- c(patientsSpec,head(unique(ply_df$Name),maxpats))
              ply_df <- ply_df[which(ply_df$Name %in% get_pats),]
            } else {
              if (length(unique(ply_df$Name)) > maxpats) {
                get_pats <- head(unique(ply_df$Name),maxpats)
                ply_df <- ply_df[which(ply_df$Name %in% get_pats),]
              }
            }
            # sort to display in correct visual order (needs to be opposite)
            if (swimmSort == "Time - Ascending") {
              ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero, decreasing = T),]
            } else if (swimmSort == "Time - Descending") {
              ply_df <- ply_df[order(ply_df$FinalEventTime_FromZero),]
            } else if (swimmSort == "Alphabetical - Ascending") {
              ply_df <- ply_df[order(ply_df$Name, decreasing = T),]
            } else if (swimmSort == "Alphabetical - Descending") {
              ply_df <- ply_df[order(ply_df$Name),]
            }
            patientIDs <- unique(ply_df[,1])
            ply_df$Index <- as.numeric(factor(ply_df$Name, levels = patientIDs))
            patients <- unique(ply_df$Index)


            event_types <- unique(ply_df$Event)
            event_types <- event_types[which(!event_types %in% c("First Recorded Time Point","Final Recorded Time Point"))]

            symbol_palette_base <- c("circle", "square", "diamond", "x", "triangle-up", "star", "cross", "triangle-down", "hourglass", "diamond-wide","star-triangle-up","diamond-tall")
            if (length(event_types) > length(symbol_palette_base)) {
              #library(listviewer)
              vals <- schema(F)$traces$scatter$attributes$marker$symbol$values
              symbol_palette <- sample(vals,length(event_types))
            } else {
              symbol_palette <- symbol_palette_base[c(1:length(event_types))]
            }
            names(symbol_palette) <- event_types

            qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
            col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
            color_palette <- sample(col_vector, length(event_types))
            names(color_palette) <- event_types

            req(length(event_types)>0)
            showLegend_list <- lapply(rep(TRUE,length(event_types)),function(x){x})
            names(showLegend_list) <- paste0("showLegend",seq(1:length(event_types)))
            p <- plot_ly()
            for (patient in patients) {
              df_patient <- ply_df[ply_df$Index == patient, ]

              p <- p %>%
                add_segments(
                  x = df_patient$FirstEventTime_FromZero[1], xend = df_patient$FinalEventTime_FromZero[1],
                  y = df_patient$Index[1], yend = df_patient$Index[1],
                  line = list(color = "black", width = 2),
                  name = patient,
                  showlegend = FALSE
                )
              p <- add_trace(p,
                             x = 0,
                             y = patient,
                             type = 'scatter',
                             mode = 'markers',
                             marker = list(symbol = 'line-ns', size = 10, color = "black"),
                             text = df_patient$Event_Details[df_patient$Event == "First Recorded Time Point"],
                             hoverinfo = 'text',
                             showlegend = FALSE)
              p <- add_trace(p,
                             x = df_patient$FinalEventTime_FromZero[1],
                             y = patient,
                             type = 'scatter',
                             mode = 'markers',
                             marker = list(symbol = 'line-ns', size = 10, color = "black"),
                             text = df_patient$Event_Details[df_patient$Event == "Final Recorded Time Point"],
                             hoverinfo = 'text',
                             showlegend = FALSE)

              for (event_seq in seq_along(event_types)) {
                event <- event_types[event_seq]
                if (event %in% unique(df_patient$Event)) {
                  p <- add_trace(p,
                                 x = df_patient[which(df_patient$Event == event),"EventTime_FromZero"],
                                 y = patient,
                                 type = 'scatter',
                                 mode = 'markers',
                                 marker = list(symbol = symbol_palette[event], size = 10, color = color_palette[event]),
                                 text = df_patient$Event_Details[df_patient$Event == event],
                                 hoverinfo = 'text',
                                 showlegend = (function(){
                                   if(showLegend_list[[event_seq]]){
                                     showLegend_list[[event_seq]] <<- FALSE
                                     return(TRUE)
                                   } else {
                                     return(FALSE)
                                   }
                                 })(),
                                 name = event)
                }
              }

            }

            p_out <- layout(p,
                            xaxis = list(title = 'Time (Years)', zeroline = FALSE, titlefont = list(size = 20), tickfont = list(size = 15)),
                            yaxis = list(title = 'Patients', tickvals = patients, ticktext = patientIDs,
                                         titlefont = list(size = 20), tickfont = list(size = 12)),
                            legend = list(title = list(text = "Event Type")),
                            showlegend = TRUE) %>%
              config(
                toImageButtonOptions = list(
                  format = "svg",
                  height = input$SummSwimmerHeight,
                  width = input$SummSwimmerWidth,
                  filename = paste0("Event_Summary_Swimmers_",Sys.Date())
                )
              )
            p_out

          }

        }




      })

      output$summ_swimmer_plot <- renderPlotly({
        req(summ_swimmer_plot_react())
        ply <- summ_swimmer_plot_react()
        ply
      })

      ##  Mol Anno --------------------------------------------------------

      observe({
        req(event_data())
        event_data <- event_data()
        table_names <- unique(event_data[,"EventTab"])[which(!is.na(unique(event_data[,"EventTab"])))]
        table_select_ref <- ifelse(any(grepl("molecular",table_names,ignore.case = T)),grep("molecular",table_names,ignore.case = T, value = T)[1],
                                   table_names[1])
        table_names <- c(table_names,"ShinyEvents Treatment Clusters")
        updateSelectizeInput(session,"RefDataTable", choices = table_names, selected = table_select_ref, server = T)
        updateSelectizeInput(session,"EOIDataTable", choices = table_names, selected = table_names[which(table_names!=table_select_ref)][1], server = T)
      })

      observe({
        req(wkbk_react_sub())
        req(event_data())
        req(input$RefDataTable)
        wkbk <- wkbk_react_sub()
        event_data <- event_data()
        reftable <- input$RefDataTable

        if (reftable == "ShinyEvents Treatment Clusters") {
          req(sankey_clusters_cast())
          df <- sankey_clusters_cast()
          ref_events <- unique(event_data[which(event_data[,"EventType"] %in% colnames(df)),"Event"])
        } else {
          RefDataTableCols <- colnames(wkbk[[reftable]])
          ref_events <- unique(event_data[which(event_data[,"EventTab"] == reftable),"Event"])
        }

        updateSelectizeInput(session,"RefEvent", choices = ref_events, selected = ref_events, server = T)
      })

      observe({
        req(wkbk_react_sub())
        req(event_data())
        req(input$EOIDataTable)
        wkbk <- wkbk_react_sub()
        event_data <- event_data()
        eoitable <- input$EOIDataTable
        reset <- input$SaveAnnotation

        if (eoitable == "ShinyEvents Treatment Clusters") {
          req(sankey_clusters_cast())
          df <- sankey_clusters_cast()
          eoi_events <- unique(event_data[which(event_data[,"EventType"] %in% colnames(df)),"Event"])
        } else {
          eoiDataTableCols <- colnames(wkbk[[eoitable]])
          eoi_events <- unique(event_data[which(event_data[,"EventTab"] == eoitable),"Event"])
        }
        updateSelectizeInput(session,"EOIEvent", choices = eoi_events, selected = NULL, server = T)
      })

      MolecularAnnoTable_reactiveVal <- reactiveVal()
      MolecularAnnoTable_react <- reactive({
        req(wkbk_react_sub())
        req(event_data())
        req(input$RefDataTable)
        req(input$RefEvent)
        req(input$EOIDataTable)
        wkbk <- wkbk_react_sub()
        event_data <- event_data()
        RefDataTable <- input$RefDataTable
        RefEvent <- input$RefEvent
        EOIDataTable <- input$EOIDataTable
        EOIEvent <- input$EOIEvent
        newcolname <- input$NewColName
        andor <- input$EOIEventAndOr
        sankey_clusters_cast <- sankey_clusters_cast()
        reactive_val_df <- MolecularAnnoTable_reactiveVal()
        newcolname <- ifelse(isTruthy(newcolname),newcolname,"ReferenceEvent_RelativeTo_NewEvent")
        newcolname2 <- paste0(newcolname,"_TimeDiff")
        newcolname3 <- paste0(newcolname,"_EventPresent")
        #save(list = ls(), file = "eoi_anno.RData", envir = environment())
        if (!isTruthy(reactive_val_df)) {
          ref_df <- wkbk[[RefDataTable]]
        } else {
          ref_df <- reactive_val_df
        }

        if (EOIDataTable == "ShinyEvents Treatment Clusters") {
          req(sankey_clusters_cast())
          eoi_df <- sankey_clusters_cast
        } else {
          eoi_df <- wkbk[[EOIDataTable]]
        }
        if (isTruthy(EOIEvent)) {
          ref_event <- event_data[which(event_data$Event %in% RefEvent),]
          ref_event <- ref_event %>%
            group_by(Name) %>%
            mutate(First_RefEventStart = min(EventStart, na.rm = T)) %>%
            select(Name,First_RefEventStart) %>%
            unique() %>%
            as.data.frame()
          if (andor == "'And' Statement") {
            eoi_event <- event_data %>%
              group_by(Name) %>%
              filter(all(EOIEvent %in% Event)) %>%
              filter(Event %in% EOIEvent) %>%
              as.data.frame()

          } else {
            eoi_event <- event_data[which(event_data$Event %in% EOIEvent),]
          }
          eoi_event2 <- eoi_event %>%
            group_by(Name) %>%
            slice_min(order_by = data.frame(EventStart,EventEnd)) %>%
            select(Name,EventStart,EventEnd)%>%
            unique() %>%
            as.data.frame()

          ref_eoi <- merge(ref_event,eoi_event2,all = T)
          ref_eoi2 <- ref_eoi %>%
            mutate(!!sym(newcolname) := case_when(
              First_RefEventStart >= EventStart & First_RefEventStart <= EventEnd ~ "During",
              First_RefEventStart < EventStart & First_RefEventStart < EventEnd ~ "Before",
              First_RefEventStart > EventStart & First_RefEventStart > EventEnd ~ "After"
            )) %>%
            mutate(!!sym(newcolname2) := case_when(
              !!sym(newcolname) == "During" ~ 0,
              !!sym(newcolname) == "Before" ~ round(First_RefEventStart-EventStart,4),
              !!sym(newcolname) == "After" ~ round(First_RefEventStart-EventEnd,4)
            )) %>%
            mutate(!!sym(newcolname3) := case_when(
              !!sym(newcolname) %in% c("Before","During","After") ~ as.character(TRUE),
              is.na(!!sym(newcolname)) ~ as.character(FALSE)
            ))
          colnames(ref_eoi2)[1] <- colnames(ref_df)[1]
          ref_df_eoi <- merge(ref_eoi2[,c(colnames(ref_eoi2)[1],newcolname,newcolname2,newcolname3)],ref_df, all.y = T)
          ref_df_eoi
        } else {
          ref_df
        }
      })

      observeEvent(input$SaveAnnotation, {
        req(wkbk_react_sub())
        req(input$RefDataTable)
        wkbk <- wkbk_react_sub()
        RefDataTable <- input$RefDataTable
        ref_df <- wkbk[[RefDataTable]]
        new_df <- MolecularAnnoTable_react()
        new_cols <- setdiff(colnames(new_df),colnames(ref_df))
        new_df <- new_df %>%
          relocate(any_of(new_cols), .after = !!sym(colnames(new_df)[1]))
        updateTextInput(session,"NewColName",value = "", placeholder = "Molecular Anno Before_After")
        MolecularAnnoTable_reactiveVal(new_df)
      })

      observeEvent(input$SaveAnnotation, {
        req(wkbk_react_sub())
        req(input$RefDataTable)
        wkbk <- wkbk_react_sub()
        RefDataTable <- input$RefDataTable

        if (!isTruthy(MolecularAnnoTable_reactiveVal())) {
          new_df <- wkbk[[RefDataTable]]
        } else {
          new_df <- MolecularAnnoTable_reactiveVal()
        }

        wkbk[[RefDataTable]] <- new_df
        wkbk_react_anno(wkbk)

      })

      output$MolecularAnnoTable <- DT::renderDataTable({
        req(MolecularAnnoTable_react())
        df <- MolecularAnnoTable_react()
        df[is.na(df)] <- "NA"
        DT::datatable(df,
                      extensions = 'Scroller',
                      options = list(lengthMenu = c(5,10,25,50,100, 1000),
                                     pageLength = 10,
                                     scrollX = T),
                      rownames = F
        )
      })

      output$dlndMolecularAnnoTable <- downloadHandler(
        filename = function() {
          ref_table <- input$RefDataTable
          paste0(gsub(" ","_",Project_Name),"_",ref_table,"_MolAnno_",Sys.Date(),".txt")
        },
        content = function(file) {
          df <- MolecularAnnoTable_react()
          write.table(df,file, sep = '\t', row.names = F)
        }
      )

      ## Mol Breakdown ----------------------------------------------------------


      output$rendMolecularBreakdownTabs <- renderUI({
        feats <- input$FilterTabCountFeatures
        ref_feat <- input$FilterTabRefFeature
        if (!isTruthy(feats)) {
          fluidRow(
            column(12,
                   div(DT::dataTableOutput("MolecularCountTable"), style = "font-size:14px"),
                   p(),
                   downloadButton("dlndMolecularCountTable", "Download Table")
            )
          )
        } else {
          if (isTruthy(ref_feat)) {
            fluidRow(
              column(7,
                     div(DT::dataTableOutput("MolecularCountTable"), style = "font-size:14px"),
                     p(),
                     downloadButton("dlndMolecularCountTable", "Download Table")
              ),
              column(5,
                     uiOutput("rendBreakdownHeader"),
                     uiOutput("rendBreakdownSubHeader"),
                     div(tableOutput("MolecularBreakdownOutput"),style = "font-size:14px"),
                     p(),
                     uiOutput("renddnldMolecularBreakdownOutput")
              )
            )
          } else {
            fluidRow(
              column(9,
                     div(DT::dataTableOutput("MolecularCountTable"), style = "font-size:14px"),
                     p(),
                     downloadButton("dlndMolecularCountTable", "Download Table")
              ),
              column(3,
                     uiOutput("rendBreakdownHeader"),
                     uiOutput("rendBreakdownSubHeader"),
                     div(tableOutput("MolecularBreakdownOutput"),style = "font-size:14px"),
                     p(),
                     uiOutput("renddnldMolecularBreakdownOutput")
              )
            )
          }
        }
      })

      observe({
        req(param_data())
        param_data <- param_data()
        table_names <- unique(param_data[,1])
        #req(event_data())
        #event_data <- event_data()
        #table_names <- unique(event_data[,"EventTab"])[which(!is.na(unique(event_data[,"EventTab"])))]
        table_select_ref <- ifelse(any(grepl("molecular",table_names,ignore.case = T)),grep("molecular",table_names,ignore.case = T, value = T)[1],
                                   table_names[1])
        updateSelectizeInput(session,"TableToFilter", choices = table_names, selected = table_select_ref, server = T)
        updateSelectizeInput(session,"TableToFilterMain", choices = table_names, selected = 1, server = T,
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             ))
      })


      TableFilterInput_df <- reactive({
        req(input$TableToFilter)
        req(wkbk_react_anno_sub())
        df_name <- input$TableToFilter
        wkbk <- wkbk_react_anno_sub()
        df <- wkbk[[df_name]]
        df_m <- cbind(data.frame(Row_Index = rownames(df)),
                      df)
        df_m2 <- reshape2::melt(df_m,id.vars = "Row_Index")
        df_m2 <- df_m2[,-1]
        df_m2[is.na(df_m2)] <- "NA"
        df_m2_levels <- unique(df_m2[,1])
        df_m3 <- df_m2 %>%
          group_by(variable) %>%
          mutate(variable_N = paste0(variable," (",length(unique(value)),")")) %>%
          mutate(var_val_id = paste0(variable,"_",value)) %>%
          unique() %>%
          arrange(value, .by_group = TRUE) %>%
          as.data.frame()
        df_m3
      })

      output$rendTableFilterInput <- renderUI({
        req(TableFilterInput_df())
        df_m3 <- TableFilterInput_df()
        tree_df_m3 <- create_tree(df_m3, levels = c("variable_N","value"), levels_id = c("variable","var_val_id"))
        treeInput("TableFilterInput",NULL, choices = tree_df_m3, returnValue = "all", closeDepth = 0)
      })

      TableFilterMainInput_df <- reactive({
        req(input$TableToFilterMain)
        req(wkbk_raw_react())
        #req(wkbk_react_anno())
        df_name <- input$TableToFilterMain
        wkbk <- wkbk_raw_react()
        df <- wkbk[[df_name]]
        df_m <- cbind(data.frame(Row_Index = rownames(df)),
                      df)
        df_m2 <- reshape2::melt(df_m,id.vars = "Row_Index")
        df_m2 <- df_m2[,-1]
        df_m2[is.na(df_m2)] <- "NA"
        df_m2_levels <- unique(df_m2[,1])
        df_m3 <- df_m2 %>%
          group_by(variable) %>%
          mutate(variable_N = paste0(variable," (",length(unique(value)),")")) %>%
          mutate(var_val_id = paste0(variable,"_",value)) %>%
          unique() %>%
          arrange(value, .by_group = TRUE) %>%
          as.data.frame()
        df_m3
      })
      output$rendTableFilterMainInput <- renderUI({
        req(TableFilterMainInput_df())
        df_m3 <- TableFilterMainInput_df()
        tree_df_m3 <- create_tree(df_m3, levels = c("variable_N","value"), levels_id = c("variable","var_val_id"))
        treeInput("TableFilterMainInput",NULL, choices = tree_df_m3, returnValue = "all", closeDepth = 0)
      })

      observe({
        req(input$TableToFilter)
        req(wkbk_raw_react())
        #req(wkbk_react_anno())
        df_name <- input$TableToFilter
        wkbk <- wkbk_raw_react()
        df <- wkbk[[df_name]]
        updateSelectizeInput(session,"FilterTableIDcol", choices = colnames(df), selected = colnames(df)[1], server = T)
        updateSelectizeInput(session,"FilterTabRefFeature", choices = c("No Reference",colnames(df)), selected = 1,
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             ), server = T)
        updateSelectizeInput(session,"FilterTabCountFeatures", choices = colnames(df), selected = NULL,
                             options = list(
                               placeholder = 'Please select an option below',
                               onInitialize = I('function() { this.setValue(""); }')
                             ), server = T)
      })
      observe({
        req(wkbk_react_anno_sub())
        req(input$TableToFilter)
        df_name <- input$TableToFilter
        wkbk <- wkbk_react_anno_sub()
        df <- wkbk[[df_name]]
        ref_feat <- input$FilterTabRefFeature
        if (isTruthy(ref_feat)) {
          if (ref_feat != "No Reference") {
            OR_opts <- unique(df[,ref_feat])
            updateSelectizeInput(session,"OddsRatioColumn", choices = OR_opts, selected = 1,
                                 options = list(
                                   placeholder = 'Please select an option below',
                                   onInitialize = I('function() { this.setValue(""); }')
                                 ), server = T)
            #}
          }
        }
      })

      breakdown_table <- reactive({
        wkbk <- wkbk_react_anno_sub()
        df_name <- input$TableToFilter
        id_col <- input$FilterTableIDcol
        df <- wkbk[[df_name]]
        filters <- input$TableFilterInput
        #save(list = ls(), file = "breakdown_table.RData", envir = environment())

        if (isTruthy(filters)) {
          req(TableFilterInput_df())
          tree_df <- TableFilterInput_df()
          # Getting info from filters selected
          filter_keys <- lapply(filters, function(x) {
            if (!x$text[[1]] %in% unique(tree_df$variable_N)) {
              id <- x[["id"]][[1]]
              val <- x[["text"]][[1]]
              if (val == "NA") {
                val <- NA
              }
              col <- gsub(paste0("_",val,"$"),"",id)
              return(c(col = col,val = val))
            }
          })
          # Remove NULL elements from list
          filter_keys <- Filter(Negate(is.null),filter_keys)
          # Move column name to element name in list
          filter_keys2 <- setNames(lapply(filter_keys, `[[`, "val"), sapply(filter_keys, `[[`, "col"))
          # Collapse filters from the same column
          filter_keys3 <- lapply(split(filter_keys2, names(filter_keys2)), unlist)
          # Get rows that satisfy each filter
          filter_keys4 <- lapply(names(filter_keys3), function(x) {
            filter_vals <- filter_keys3[[x]]
            keys <- which(df[,x] %in% filter_vals)
            #keys <- rownames(df[which(df[,x] %in% filter_vals),])
            return(keys)
          })
          # Intersect the filter rows to get rows that match all filters
          filter_keys5 <- Reduce(intersect,filter_keys4)
          # Subset out rows from df
          df <- df[filter_keys5,]
          df
        } else {
          df
        }
      })
      output$MolecularCountTable <- DT::renderDataTable({
        req(breakdown_table())
        df <- breakdown_table()
        DT::datatable(df,
                      extensions = 'Scroller',
                      options = list(lengthMenu = c(5,10,25,50,100, 1000),
                                     pageLength = 10,
                                     scrollX = T),
                      rownames = F
        )

      })
      output$dlndMolecularCountTable <- downloadHandler(
        filename = function() {
          df_name <- input$TableToFilter
          paste0(gsub(" ","_",Project_Name),"_",df_name,"_Filtered_",Sys.Date(),".txt")
        },
        content = function(file) {
          df <- breakdown_table()
          write.table(df,file, sep = '\t', row.names = F)
        }
      )

      breakdown_counting_df <- reactive({
        req(breakdown_table())
        req(input$FilterTableIDcol)
        req(input$FilterTabCountFeatures)
        df <- breakdown_table()
        id_col <- input$FilterTableIDcol
        feats <- input$FilterTabCountFeatures
        ref_feat <- input$FilterTabRefFeature
        or_col <- input$OddsRatioColumn
        if (!isTruthy(ref_feat) | ref_feat == "No Reference") {
          ref_feat <- NULL
        } else {
          feats <- feats[which(feats!=ref_feat)]
        }
        df <- df[which(!is.na(df[,id_col])),]
        feats_tab <- feat_breakdown(df,id_col,ref_feat,feats, shiny = T)

        #save(list = ls(), file = "OR_Err.RData", envir = environment())

        if (isTruthy(or_col)) {
          #or_col <- paste0(id_col," ",or_col)
          or_col <- paste0(ref_feat," ",or_col)
          feats_tab <- perform_fishers_test(feats_tab,or_col)
          if (input$ORtest) {
            feats_tab[,grep(" FishersTest Pvalue$",colnames(feats_tab))] <- ifelse(feats_tab[,grep(" OddsRatio$",colnames(feats_tab))] < 1,
                                                                                   1,feats_tab[,grep(" FishersTest Pvalue$",colnames(feats_tab))])
          }
        }

        feats_tab
      })
      output$rendBreakdownHeader <- renderUI({
        req(breakdown_counting_df())
        h3("Feature Break-Down")
      })
      output$rendBreakdownSubHeader <- renderUI({
        req(breakdown_counting_df())
        id_col <- input$FilterTableIDcol
        ref_feat <- input$FilterTabRefFeature
        if (!is.null(ref_feat) | ref_feat == "No Reference") {
          p(HTML(paste0("Count of unique <b>",id_col,"</b> identifiers by the reference feature: <b>",ref_feat,"</b> across selected features")))
        } else {
          p(HTML(paste0("Count of unique <b>",id_col,"</b> identifiers across selected features")))
        }
      })
      output$MolecularBreakdownOutput <- renderTable({
        req(breakdown_counting_df())
        df <- breakdown_counting_df()
        df
      },sanitize.text.function=function(x){x})
      output$renddnldMolecularBreakdownOutput <- renderUI({
        req(breakdown_counting_df())
        downloadButton("dnldMolecularBreakdownOutput")
      })
      output$dnldMolecularBreakdownOutput <- downloadHandler(
        filename = function() {
          df_name <- input$TableToFilter
          paste0(gsub(" ","_",Project_Name),"_",df_name,"_FeatureBreakDown_",Sys.Date(),".txt")
        },
        content = function(file) {
          df <- breakdown_counting_df()
          df[,1] <- gsub("<strong>|<\\/strong>","",df[,1])
          df[,2] <- suppressWarnings(as.numeric(df[,2]))
          write.table(df,file, sep = '\t', row.names = F)
        }
      )

      switch_main_tab <- function(tab_id, tab_selected){
        shiny::updateTabsetPanel(session = session, inputId = tab_id, selected = tab_selected)
      }
      switch_main_navbar <- function(navbar_id, navbar_selected){
        shiny::updateNavbarPage(session = session, inputId = navbar_id, selected = navbar_selected)
      }

      Homepage_server("ShinyEvents1", homepage_tutorial_text_list, switch_main_tab, switch_main_navbar)
      Tutorial_server("ShinyEvents1", homepage_tutorial_text_list)

      observe({
        print("this is stupid")
        updateNavbarPage(session = session, inputId = "shinyevents_tabs", selected = "patient_visual_analytics")
        updateTabsetPanel(session = session, inputId = "PatientMainPanel", selected = "2")
      })%>% bindEvent(input$tabselect_test)










    }



  })



}




# Run the application
shinyApp(ui = ui, server = server)



















