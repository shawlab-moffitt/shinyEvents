Tutorial_server <- function(id, homepage_texts){
  shiny::moduleServer(id, 
                      function(input, output, session){
                        
                        homepage_tutorial_text_list <- homepage_texts
                        
                        #preprocessing
                        observeEvent(input$change_project_name, {
                          output$video <- renderUI({
                            HTML('<iframe  width="100%" height="100%" src="//www.youtube.com/embed/_291toyzL7o?playlist=_291toyzL7o&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                          })
                          #output$tutorial_text <- renderText(homepage_tutorial_text_list[["change_project_name"]]$text)
                        })
                        observeEvent(input$load_data_files, {
                          output$video <- renderUI({
                            HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/GrDZMAB8_bw?playlist=GrDZMAB8_bw&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                          })
                          #output$tutorial_text <- renderText(homepage_tutorial_text_list[["load_data_files"]]$text)
                        })
                        observeEvent(input$load_example_data, {
                          output$video <- renderUI({
                            HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/tAdr36D0VvQ?playlist=tAdr36D0VvQ&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                          })
                          #output$tutorial_text <- renderText(homepage_tutorial_text_list[["load_example_data"]]$text)
                        })
                        observeEvent(input$load_app_overview, {
                          output$video <- renderUI({
                            HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/CpDyhlVKPDE?playlist=CpDyhlVKPDE&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                          })
                          #output$tutorial_text <- renderText(homepage_tutorial_text_list[["load_example_data"]]$text)
                        })
                        observeEvent(input$load_cohort_overview, {
                          output$video <- renderUI({
                            HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/Y1vg6k60PaQ?playlist=Y1vg6k60PaQ&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                          })
                          #output$tutorial_text <- renderText(homepage_tutorial_text_list[["load_example_data"]]$text)
                        })
                        observeEvent(input$input_data_formatting, {
                          output$video <- renderUI({
                            div(class = "html-embed", includeHTML("www/ShinyEvents_GettingStarted_body.html"))
                            #includeHTML("www/ShinyEvents_GettingStarted_body.html")
                          })
                        })
                        observeEvent(input$need_Vignette, {
                          output$video <- renderUI({
                            div(class = "html-embed", includeHTML("www/ShinyEvents_GettingStarted_body.html"))
                            #includeHTML("www/ShinyEvents_GettingStarted_body.html")
                          })
                        }, ignoreInit = TRUE)
                        observeEvent(input$TimeToEvent_Tutorial, {
                          output$video <- renderUI({
                            HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/k1HcDm12ZzM?playlist=k1HcDm12ZzM&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                          })
                          #output$tutorial_text <- renderText(homepage_tutorial_text_list[["load_example_data"]]$text)
                        })
                        #observeEvent(input$set_up_parameter_data, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/24ZZKPyzVas?playlist=24ZZKPyzVas&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["set_up_parameter_data"]]$text)
                        #})
                        #observeEvent(input$set_up_event_data, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/UToZcttTFzo?playlist=UToZcttTFzo&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["set_up_event_data"]]$text)
                        #})
                        #observeEvent(input$file_type_overview, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/Awt3Ei-GX_U?playlist=Awt3Ei-GX_U&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["file_type_overview"]]$text)
                        #})
                        
                        
                        #example_use_cases
                        #observeEvent(input$sample_qa, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["sample_qa"]]$text)
                        #})
                        #observeEvent(input$adr_detection, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["adr_detection"]]$text)
                        #})
                        #observeEvent(input$regimen_efficacy, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["regimen_efficacy"]]$text)
                        #})
                        #observeEvent(input$drug_repurposing, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["drug_repurposing"]]$text)
                        #})
                        #observeEvent(input$disease_progression_analysis, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["disease_progression_analysis"]]$text)
                        #})
                        #
                        ##Patient Visual Analytics
                        #observeEvent(input$disease_progression_analysis, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["disease_progression_analysis"]]$text)
                        #})
                        #observeEvent(input$disease_progression_analysis, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["disease_progression_analysis"]]$text)
                        #})
                        #
                        ##Treatment Associated Analytics
                        #observeEvent(input$treatment_cluster_sankey, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["treatment_cluster_sankey"]]$text)
                        #})
                        #observeEvent(input$treatment_cluster_heatmap, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["treatment_cluster_heatmap"]]$text)
                        #})
                        #observeEvent(input$event_duration_swimmers, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["event_duration_swimmers"]]$text)
                        #})
                        #observeEvent(input$event_duration_boxplot, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["event_duration_boxplot"]]$text)
                        #})
                        #observeEvent(input$event_duration_heatmap, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["event_duration_heatmap"]]$text)
                        #})
                        #
                        ##Time To Event Analytics
                        #observeEvent(input$tte_kaplan, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["tte_kaplan"]]$text)
                        #})
                        #observeEvent(input$tte_swimmers, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["tte_swimmers"]]$text)
                        #})
                        #
                        ##Cohort Overview
                        #observeEvent(input$cohort_overview_summary, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["cohort_overview_summary"]]$text)
                        #})
                        #observeEvent(input$cohort_overview_swimmers, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["cohort_overview_swimmers"]]$text)
                        #})
                        #observeEvent(input$cohort_overview_ma, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["cohort_overview_ma"]]$text)
                        #})
                        #observeEvent(input$cohort_overview_mb, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/ZXtPsu95bdI?playlist=ZXtPsu95bdI&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["cohort_overview_mb"]]$text)
                        #})
                        #
                        #
                        ## Tutorial Figure Settings
                        #observeEvent(input$figure_settings, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/1j5YBZNJGTc?playlist=1j5YBZNJGTc&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["figure_settings"]]$text)
                        #})
                        #
                        ## Tutorial Data Export
                        #observeEvent(input$data_export, {
                        #  output$video <- renderUI({
                        #    HTML('<iframe width="100%" height="100%" src="//www.youtube.com/embed/BTZWA8m4xBg?playlist=BTZWA8m4xBg&&loop=1;rel=0&autoplay=1&mute=1&controls=0&showinfo=0" frameborder="0" allowfullscreen></iframe>')
                        #  })
                        #  output$tutorial_text <- renderText(homepage_tutorial_text_list[["data_export"]]$text)
                        #})
                      })
}