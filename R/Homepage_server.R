Homepage_server <- function(id, homepage_texts, switch_main_tab, switch_main_navbar){
  shiny::moduleServer(id, function(input, output, session){
    
    homepage_tutorial_text_list <- homepage_texts
    
    output$homepage_text <- renderText(homepage_tutorial_text_list[["Homepage"]]$text)
    # Homepage homepage
    output$homepage <- renderUI({
      shiny::fluidPage(
        shiny::tags$style(HTML(sprintf("
      #%s-image-container {
        position: relative;
        width: 800px;
        height: auto;
      }
      #%s-floor-img {
        position: relative;
        width: 100%%;
        height: auto;
        display: block;
      }
      #%s-click-zone1 {
        position: absolute;
        background-image: url('patient_timeline_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone2 {
        position: absolute;
        background-image: url('treatment_cluster_sankey_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone3 {
        position: absolute;
        background-image: url('kaplan_meier_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone4 {
        position: absolute;
        background-image: url('event_summary_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone5 {
        position: absolute;
        background-image: url('patient_event_summary_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone6 {
        position: absolute;
        background-image: url('treatment_cluster_heatmap_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone7 {
        position: absolute;
        background-image: url('T2E_swimmers_plot_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone8 {
        position: absolute;
        background-image: url('all_event_swimmers_plot_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone9 {
        position: absolute;
        background-image: url('treatment_duration_swimmers_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone10 {
        position: absolute;
        background-image: url('annotation_and_breakdown_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      #%s-click-zone11 {
        position: absolute;
        background-image: url('treatment_duration_boxplot_resize.png');
        background-color: rgba(0, 0, 0, 0);
        cursor: pointer;
        transition: transform 0.2s ease-in-out;
        z-index: 10;
      }
      .%s-click-zone {
      opacity: 0.2;
      }
      .%s-click-zone:hover {
        background-color: rgba(255, 0, 0, .4);
        transform: scale(1.4);
        opacity: 1;
        z-index: 11;
      }", id, id, id, id, id, id, id, id, id, id, id, id, id, id, id))),
      shiny::tags$div(
        id = shiny::NS(id, "image-container"),
        shiny::tags$img(id = shiny::NS(id, "floor-img"), src = "ShinyEvents_Figure_3_JD_V2_20250603.png"),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone1"),
          style = "top: 51px; left: 60px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'patient_timeline', {priority: 'event'})", id),
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone2"),
          style = "top: 51px; left: 240px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'treatment_cluster_sankey', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone3"),
          style = "top: 51px; left: 420px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'kaplan_meier', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone4"),
          style = "top: 51px; left: 600px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'event_summary', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone5"),
          style = "top: 154px; left: 60px; width: 147px; height: 34px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'patient_event_summary', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone6"),
          style = "top: 154px; left: 240px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'treatment_cluster_heatmap', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone7"),
          style = "top: 154px; left: 420px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 't2e_swimmers', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone8"),
          style = "top: 154px; left: 600px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'all_event_swimmers', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone9"),
          style = "top: 264px; left: 240px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'treatment_duration_swimmers', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone10"),
          style = "top: 264px; left: 600px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'annotation_breakdown', {priority: 'event'})", id)
        ),
        shiny::tags$div(
          class = NS(id, "click-zone"),
          id = NS(id, "click-zone11"),
          style = "top: 370px; left: 240px; width: 147px; height: 80px;",
          onclick = sprintf("Shiny.setInputValue('%s-room_click', 'treatment_duration_boxplot', {priority: 'event'})", id)
        )
      )
      )
      
      #list(src = "ShinyEvents_Homepage/image_files/ShinyEvents_Figure_3_JD_V2_20250603.png",
      #     contentType = 'image/png',
      #     width = 800,
      #     height = 480)
    })
    
    
    
    shiny::observe({
      if(input$room_click == "patient_timeline"){
        switch_main_navbar("shinyevents_tabs", "patient_visual_analytics")
        switch_main_tab("PatientMainPanel", "1")
      }else if(input$room_click == "patient_event_summary"){
        switch_main_navbar("shinyevents_tabs", "patient_visual_analytics")
        switch_main_tab("PatientMainPanel", "2")
      }else if(input$room_click == "treatment_cluster_sankey"){
        switch_main_navbar("shinyevents_tabs", "treatment_associated_analytics")
        switch_main_tab("treatAnalytics", "1")
        switch_main_tab("treatAnalyticsCls", "1")
      }else if(input$room_click == "treatment_cluster_heatmap"){
        switch_main_navbar("shinyevents_tabs", "treatment_associated_analytics")
        switch_main_tab("treatAnalytics", "1")
        switch_main_tab("treatAnalyticsCls", "2")
      }else if(input$room_click == "treatment_duration_swimmers"){
        switch_main_navbar("shinyevents_tabs", "treatment_associated_analytics")
        switch_main_tab("treatAnalytics", "2")
        switch_main_tab("treatAnalyticsDur", "3")
      }else if(input$room_click == "treatment_duration_boxplot"){
        switch_main_navbar("shinyevents_tabs", "treatment_associated_analytics")
        switch_main_tab("treatAnalytics", "2")
        switch_main_tab("treatAnalyticsDur", "1")
      }else if(input$room_click == "kaplan_meier"){
        switch_main_navbar("shinyevents_tabs", "time_to_event_analysis")
        switch_main_tab("ttetabs", "2")
      }else if(input$room_click == "t2e_swimmers"){
        switch_main_navbar("shinyevents_tabs", "time_to_event_analysis")
        switch_main_tab("ttetabs", "1")
      }else if(input$room_click == "event_summary"){
        switch_main_navbar("shinyevents_tabs", "cohort_overview")
        switch_main_tab("SummaryMain", "1")
      }else if(input$room_click == "all_event_swimmers"){
        switch_main_navbar("shinyevents_tabs", "cohort_overview")
        switch_main_tab("SummaryMain", "4")
      }else if(input$room_click == "annotation_breakdown"){
        switch_main_navbar("shinyevents_tabs", "cohort_overview")
        switch_main_tab("SummaryMain", "2")
      }
    }) %>% shiny::bindEvent(input$room_click)
    
    # Homepage links
    output$shiny_app <- renderImage({
      list(src = "ShinyEvents_Homepage/image_files/ShinyEvents_Logo2.png",
      #list(src = "ShinyEvents_Homepage/image_files/GitHub_Logo.png",
           contentType = 'image/png',
           width = 180,
           height = 180)
    }, deleteFile = FALSE)
    
    output$shiny_github <- renderImage({
      list(src = "ShinyEvents_Homepage/image_files/github_logo3.png",
      #list(src = "ShinyEvents_Homepage/image_files/ShinyEvents_Logo.png",
      #list(src = "ShinyEvents_Homepage/image_files/ShinyEvents_Icon_MC.png",
           contentType = 'image/png',
           width = 180,
           height = 180)
    }, deleteFile = FALSE)
    
    #output$function_github <- renderImage({
    #  list(src = "ShinyEvents_Homepage/image_files/ShinyEvents_Icon_BL.png",
    #       contentType = 'image/png',
    #       width = 180,
    #       height = 180)
    #}, deleteFile = FALSE)
    
    #output$shiny_mergeqc <- renderImage({
    #  list(src = "ShinyEvents_Homepage/image_files/Mergeqc_app.png",
    #       contentType = 'image/png',
    #       width = 180,
    #       height = 180)
    #}, deleteFile = FALSE)
    
    #output$mergeqc_github <- renderImage({
    #  list(src = "ShinyEvents_Homepage/image_files/Mergeqc_github.png",
    #       contentType = 'image/png',
    #       width = 180,
    #       height = 180)
    #}, deleteFile = FALSE)
    
    #output$journal <- renderImage({
    #  list(src = "ShinyEvents_Homepage/image_files/bioinfo_40_7cover.jpeg",
    #       contentType = 'image/jpeg',
    #       width = 180,
    #       height = 200)
    #}, deleteFile = FALSE)
    
    observeEvent(input$citation, {
      text <- paste0("Test Citation Text")
      session$sendCustomMessage("txt", text)
    })
    
    observeEvent(input$get_started, {
      updateTabsetPanel(session, "navbar_id", selected = "step_1")
    })
    
    observeEvent(input$need_tutorial, {
      updateTabsetPanel(session, "navbar_id", selected = "tutorial")
    })
  })
  
}