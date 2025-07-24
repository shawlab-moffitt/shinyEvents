Homepage_UI <- function(id){
  bslib::nav_panel("About",
                   p(
                     bslib::page_fillable(
                       bslib::layout_column_wrap(
                         height = "850px",
                         bslib::layout_columns(
                           bslib::card(bslib::card_header("Introduction"),
                                       bslib::card_body(shiny::htmlOutput(NS(id, "homepage_text")))#,
                                       #bslib::card_header("Disclaimer",shiny::icon("triangle-exclamation")),
                                       #bslib::card_body(card_title("Disclaimer",shiny::icon("triangle-exclamation")),
                                       #                 shiny::htmlOutput(NS(id, "disclaimer_text")))
                           ),
                           bslib::card(bslib::card_header("ShinyEvents Workflow"),
                                       bslib::card_body(shiny::uiOutput(NS(id, "homepage")))
                           ),
                           bslib::card(bslib::card_header("ShinyEvents Links"),
                                       bslib::card_body(
                                         bslib::layout_column_wrap(
                                           width = 1,
                                           fillable = FALSE,
                                           bslib::card(bslib::card_header(shiny::HTML("ShinyEvents App")),
                                           #bslib::card(bslib::card_header(shiny::HTML("ShinyEvents<br/>Shiny App")),
                                                       bslib::card_body(
                                                         shiny::tags$a(shiny::imageOutput(NS(id, "shiny_app"), width = 180, height = 200), href="https://shawlab-moffitt.shinyapps.io/ShinyEvents/", target = "_blank",
                                                                       style="display: block; margin-left: auto; margin-right: auto;"))#,
                                                       #shiny::actionButton(NS(id, "get_started"), "Get Started"),
                                                       #shiny::actionButton(NS(id, "need_tutorial"), "Need Tutorial?"),
                                                       #shiny::actionButton(NS(id, "need_Vignette"), "Vignette")
                                           ),
                                           bslib::card(bslib::card_header(shiny::HTML("ShinyEvents Github")),
                                           #bslib::card(bslib::card_header(shiny::HTML("ShinyEvents<br/>Shiny Github")),
                                                       bslib::card_body(shiny::tags$a(shiny::imageOutput(NS(id, "shiny_github"), width = 180, height = 200), href="https://github.com/shawlab-moffitt/shinyEvents", target = "_blank",
                                                                                      style="display: block; margin-left: auto; margin-right: auto;"))
                                           )#,
                                           #bslib::card(bslib::card_header(shiny::HTML("ShinyEvents<br/>Function Github")),
                                           #            bslib::card_body(shiny::tags$a(shiny::imageOutput(NS(id, "function_github"), width = 180, height = 200), href="https://github.com/shawlab-moffitt/ShinyEvents", target = "_blank"))
                                           #),
                                           #bslib::card(bslib::card_header(shiny::HTML("Journal<br/>Link")),
                                           #            tags$script("Shiny.addCustomMessageHandler('txt', function (txt) {navigator.clipboard.writeText(txt);});"),
                                           #            bslib::card_body(shiny::tags$a(shiny::imageOutput(NS(id, "journal"), width = 180, height = 200), href="https://academic.oup.com/bioinformatics", target = "_blank")),
                                           #            shiny::actionButton(NS(id, "citation"), "Cite"),
                                           #),
                                           #bslib::card(bslib::card_header(shiny::HTML("MergeQC<br/>Shiny App")),
                                           #            bslib::card_body(shiny::tags$a(shiny::imageOutput(NS(id, "shiny_mergeqc"), width = 180, height = 200), href="https://shawlab-moffitt.shinyapps.io/mergeqc/", target = "_blank"))
                                           #),
                                           #bslib::card(bslib::card_header(shiny::HTML("MergeQC<br/> Github")),
                                           #            bslib::card_body(shiny::tags$a(shiny::imageOutput(NS(id, "mergeqc_github"), width = 180, height = 200), href="https://github.com/shawlab-moffitt/mergeQC/tree/main", target = "_blank"))
                                           #)
                                         )
                                       )
                           ),
                           col_widths = c(4,6,2)
                         )
                       )
                       
                   )
                  )
  )
}