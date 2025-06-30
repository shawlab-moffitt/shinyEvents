Tutorial_UI <- function(id){
  bslib::nav_panel("Tutorial", 
                    bslib::page_fillable(
                      p(
                        bslib::layout_sidebar(
                          sidebar = bslib::accordion(
                            bslib::accordion_panel("Pre Processing",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "change_project_name"), "Change Project Name")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "load_data_files"), "Load Data Files")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "load_example_data"), "Load Example Data")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "set_up_parameter_data"), "Set up Parameter Data")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "set_up_event_data"), "Set up Event Data")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "file_type_overview"), "Overview of Input File Types"))
                                                   )
                            ),
                            bslib::accordion_panel("Example Use Cases",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "sample_qa"), "Sample Quality Assurance")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "adr_detection"), "Adverse effects detection")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "regimen_efficacy"), "Drug Regimen Efficacy")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "drug_repurposing"), "Identification of Agents for Drug Repurposing")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "disease_progression_analysis"), "Disease Progression Analysis")),
                                                     
                                                   )
                            ),
                            bslib::accordion_panel("Patient Visual Analytics",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "clinical_course_summary"), "Clinical Course Summary")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "clinical_course_detailed"), "Clinical Course Detailed"))
                                                   )
                            ),
                            bslib::accordion_panel("Treatment Associated Analytics",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "treatment_cluster_sankey"), "Treatment Clustering by Sankey")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "treatment_cluster_heatmap"), "Treatment Clustering by Heatmap")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "event_duration_swimmers"), "Event Duration by Swimmers Plot")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "event_duration_boxplot"), "Event Duration by Boxplot")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "event_duration_heatmap"), "Event Duration by Heatmap"))
                                                   )
                            ),
                            
                            bslib::accordion_panel("Time To Event Analytics",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "tte_kaplan"), "Time To Event By Kaplan")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "tte_swimmers"), "Time To Event By Swimmers"))
                                                     
                                                   )
                            ),
                            bslib::accordion_panel("Cohort Overview",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "cohort_overview_summary"), "Cohort Overview Event Summary")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "cohort_overview_swimmers"), "Cohort Overview Swimmers")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "cohort_overview_ma"), "Cohort Overview Molecular Annotation")),
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "cohort_overview_mb"), "Cohort Overview Molecular Breakdown"))
                                                     
                                                   )
                            ),
                            bslib::accordion_panel("Figure Adjustment",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "figure_settings"), "Figure Settings"))
                                                   )
                            ),
                            bslib::accordion_panel("Data Export",
                                                   shiny::fluidRow(
                                                     shiny::column(12,
                                                                   actionButton(NS(id, "data_export"), "Data Export"))
                                                   )
                            )
                            
                          ),
                          bslib::layout_column_wrap(
                            bslib::layout_columns(
                              bslib::card(bslib::card_header("Tutorial Video"),
                                          bslib::card_body(uiOutput(NS(id, "video"), fill = TRUE)),
                                          full_screen = TRUE
                              ),
                              bslib::card(bslib::card_header("Helpful Information"),
                                          bslib::card_body(shiny::htmlOutput(NS(id, "tutorial_text"))),
                                          full_screen = TRUE
                              ),
                              col_widths = c(8,4)
                            ),
                            height = "850px"
                            #max_height = "850px"
                          )
                          
                      )
                      )
                    ),
                    value = "tutorial"
    )
}