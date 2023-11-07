
library(gtx)
library(implyr)
library(shinydashboardPlus)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(DT)
library(shiny)
library(bslib)
library(data.table)
library(glue)
library(tidyverse)
library(readxl)
library(ggplot2)
library(gghighlight)
library(ggtext)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(cluster)
library(ggdendro)

#Source functions
source("server_fxns.R")
source("ui_fxns.R")
source("general_fxns.R")

#Database connection
dsn = "IPSCC"
db_init = "gene_gwas_hg38_use"
db ="gene_gwas_hg38_shared"
conn <- get_connection(dsn = dsn, db = db_init, cache = F)


#Options for Spinner
options(shiny.reactlog=TRUE)
options(spinner.color="#FFA500", spinner.color.background="#ffffff", spinner.size=1)

# The width of the side menu (header + sidebar) in pixels
sidebar_width <- 350
# ui ---------------------------------------------------------------------------

# title ----
s.header <- dashboardHeader(title = "Heritability & Genetic Correlation", titleWidth = 350)

# sidebar ----
s.sidebar <- dashboardSidebar(width = sidebar_width,
                              sidebarMenu(id = "sidebarid",
                                          menuItem("Tutorials", icon = icon("book", lib = "glyphicon"), tabName = "tutorial_tab"),
                                          menuItem("Heritability & Correlation", icon=icon("dna"), tabName = "heritability_tab"),
                                                   conditionalPanel(
                                                     'input.sidebarid == "heritability_tab"',
                                                     selectInput("h2_select_class",label="Choose a classification",c("Disease"="disease","MeSH"="mesh","MeSH label"="mesh_label"),selected='disease'),
                                                     selectizeInput(inputId="h2_select_class_cate",label = "Select a category", choices = NULL ),
                                                     sliderInput("h2_select_zh2_thres","Select a Z h2 threshold",min=0,max=10,value=1.5,step = 0.5),
                                                     actionButton("h2_select_run", "RUN")
                                           ),
                                          hr(),
                                          actionButton("show_guide", "User Guide"),
                                          hr(),
                                          img(src='gsk_logo_2022.png', align = "center", width='30%'),
                                          helpText(HTML("&ensp;This site is hosted by <a target=\"_blank\" href = \"https://www.gsk.com/\">&nbsp;GSK</a>")),
                                          helpText(HTML("&ensp;Contacts for Dashboard or Scientific help:<br><a target=\"_blank\" href = \"mailto:kys21207@gsk.com\"><strong>&ensp;Kijoung Song</strong></a><br><a target=\"_blank\" href = \"mailto:padhraig.j.gormley@gsk.com\">&ensp;Padhraig Gormley</a><br>"))
                              )
)

# body ----
tab0 <- tabItem(tabName = "tutorial_tab", fluidPage(
      tags$iframe(style="height:600px; width:100%", src="tutorial.pdf")
))
tab1 <- tabItem(tabName = "heritability_tab", fluidPage(
  tagList(
    fluidRow(
      box(width=12,
          title = "Heritability (h2) & Genetic correlation (rg): Selecting traits with MeSH terms or disease category",
          p(HTML("More information: <a target=\"_blank\" href = \"https://github.com/bulik/ldsc/wiki/Heritability-and-Genetic-Correlation/\">&nbsp;ldsc Wiki</a>"))
      )
    ),
    fluidRow(
      box(width=12, status = "warning", solidHeader = T, collapsible = T, title = "h2 table in Analysis1",
          DTOutput("h2_table"), style = "overflow-x: scroll")
    ),
    output_rg_ui("h2_Dist_Ratio_Plot"),
    output_rg_ui("rg_table"),
    output_rg_ui("rg_Dist_p2_Ratio_Plot"),
    output_rg_ui("rg_Volcano_Plot")
  )
))

s.body <- dashboardBody(
  useShinyjs(),
  tabItems(tab0,tab1)
)

ui <- dashboardPage(
  s.header, 
  s.sidebar, 
  s.body, 
  skin = "red",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
  )
)


# server -----------------------------------------------------------------------

server <- function(input, output, session) {

  plot_table_reset <- reactiveVal(FALSE)  
  correlation_button_clicked <- reactiveVal(FALSE)
  selected_row_data <- reactiveVal(NULL)
  selected_row_data_scale <- reactiveVal(NULL)
  
  # Create a reactive value to store choices
  attribute_modal_sub_choices <- reactiveVal(NULL)
  rv_dta_rg <- reactiveVal()
  
  # update selectizeinput for h2_select_class_cate 
  observeEvent(input$h2_select_class,{

      req(input$h2_select_class)
      result <- query_h2_groups(input$h2_select_class,dsn = dsn,db = db)
      groups <- result[["group"]]
      updateSelectizeInput(session,inputId="h2_select_class_cate",choices = sort(groups), server=TRUE)
   }) 

  allinputIds_h2_1 <- reactive(c("h2_select_class","h2_select_class_cate","h2_select_zh2_thres","h2_select_run"))
  
 changingInputValues_h2_1 <- reactive({

    checkIds <- c("h2_select_class","h2_select_class_cate","h2_select_zh2_thres")
    lapply(checkIds, function(x) input[[x]])
  })
  
  observeEvent(input$h2_select_run, {
    plot_table_reset(FALSE) # toggles the value
    selected_row_data(NULL)
    correlation_button_clicked(FALSE)
    rgtableServer("rg_table", plot_table_reset,correlation_button_clicked,rv_dta_rg)
    rgDist_p2RatioPlotServer("rg_Dist_p2_Ratio_Plot", plot_table_reset, correlation_button_clicked, rv_dta_rg)
    rgVolcanoPlotServer("rg_Volcano_Plot", plot_table_reset, selected_row_data, correlation_button_clicked, rv_dta_rg)
    
    if(isTRUE(input$h2_select_class_cate)){
      lapply(allinputIds_h2_1(), shinyjs::disable)
    }
  })
  
  observeEvent(changingInputValues_h2_1(), shinyjs::enable("h2_select_run"))
  
  dta_h2_1 <- eventReactive(input$h2_select_run, {
    req(input$h2_select_class,input$h2_select_class_cate,input$h2_select_zh2_thres)
    dta <- query_h2_selected(input$h2_select_class,input$h2_select_class_cate,input$h2_select_zh2_thres,dsn = dsn,db = db) 
    return(dta)
  })
  
  observe({
    # When a new row is selected
     if (!is.null(selected_row_data())) {
      plot_table_reset(FALSE)
      
      # Reset correlation button clicked flag
      correlation_button_clicked(FALSE)
      
      # Reset the data for additional table
      rv_dta_rg(NULL)
    }
  })
  
    
 observeEvent(input$h2_table_rows_selected, {

   correlation_button_clicked(FALSE)
    if (!is.null(input$h2_table_rows_selected)) {
      selected_row_data(dta_h2_1()[input$h2_table_rows_selected,])
    }
  })

 observe({

   if(!is.null(input$h2_table_rows_selected)){
     h2DistRatioPlotServer("h2_Dist_Ratio_Plot", plot_table_reset, selected_row_data, dta_h2_1)
   }
 })

 
  observeEvent(input$attribute_modal, {
    #print("attribute_modal changed!")
    # Update the choices for attribute_modal_sub when attribute_modal changes
    if(!is.null(input$attribute_modal)) {
      if(input$attribute_modal=="all"){
        groups_data <- c(All = "All")         
      }else{
         groups_data <- query_rg_groups(input$attribute_modal,dsn = dsn,db = db)$group 
      }
      attribute_modal_sub_choices(sort(groups_data))
    # Update the select inputs
#        updateSelectInput(session, "attribute", selected = input$attribute_modal)
#      print(attribute_modal_sub_choices())
        updateSelectizeInput(session, "attribute_modal_sub", choices = attribute_modal_sub_choices(), server=FALSE)
    }    
  })
 
  observeEvent(input$correlation_button, {
    clicked_button_index <- as.numeric(gsub("correlation-btn-", "", input$correlation_button))
    
    # Update the selected row data
    selected_row_data(dta_h2_1()[clicked_button_index, ])
    
    # Set correlation_button_clicked to TRUE
    correlation_button_clicked(TRUE)
    showModal(modalDialog(
      title = "Choose a Classification for rg",
      selectInput("attribute_modal", "Choose a classification for Analysis2", choices = c("Disease"="disease","MeSH"="mesh","MeSH label"="mesh_label","All"="all"), selected = "disease"),
      # Use the current choices from reactiveVal for attribute_modal_sub
      selectizeInput(inputId="attribute_modal_sub", label = "Select a category", choices = attribute_modal_sub_choices()), 
  #    numericInput('rg_pvalue_thres','Select a p-value threshold for rg',min=0,max=1,value=0.05,step=0.001),
      easyClose = TRUE,
      footer = actionButton("submit_modal", "Submit")
    ))
  })

  observeEvent(input$liability_button, {
    clicked_button_index <- as.numeric(gsub("liability-btn-", "", input$liability_button))
    
    # Update the selected row data
    selected_row_data_scale(dta_h2_1()[clicked_button_index, ])
    # Set correlation_button_clicked to TRUE
    showModal(modalDialog(
      title = "Convert h2 from the observed scale to the liability scale",
      numericInput('pop_prevalence','Choose Pop. prevalence',min=0,max=1,value=0.05,step=0.001),
      actionButton("calculate", "Calculate liability"),
      br(),
      br(),
      fluidRow(
      box(status = "warning", solidHeader = T, collapsible = T, title = "h2 liablity",
          textOutput("result"))
      ),
      footer = tagList(
        modalButton("Close")
      )
      ))
  })
  
  calc_result <- reactive({
    req(input$calculate)  # ensure the button was pressed
    K=input$pop_prevalence
    P=selected_row_data_scale()$p
    h2=selected_row_data_scale()$h2
    zv <- dnorm(qnorm(K))
    
    h2_liab <- h2 * K^2 * (1 - K)^2 / P / (1-P) / zv^2
    h2_liab 
    
  })
  
  output$result <- renderText({
    calc_result()
  })
  
  observeEvent(input$submit_modal, {
    # Capture input values
    attribute_modal_value <- input$attribute_modal
    attribute_modal_sub_value <- input$attribute_modal_sub
 #   rg_pvalue_thres_value <- input$rg_pvalue_thres
    
    # Close the modal
    removeModal()
    
    #current_data <- ifelse(is.null(selected_row_data()), dta_h2_1()[1,], selected_row_data())
    
    # Use the captured values to run the query
    result <- query_rg_selected_class(attribute_modal_value, 
                                      selected_row_data()$analysis,
                                      attribute_modal_sub_value, 
                                      dsn = dsn,
                                      db = db)
    # Store the result in the reactive value
    rv_dta_rg(result)

    
    rgtableServer("rg_table", plot_table_reset,correlation_button_clicked,rv_dta_rg)
    rgDist_p2RatioPlotServer("rg_Dist_p2_Ratio_Plot", plot_table_reset, correlation_button_clicked, rv_dta_rg)
    rgVolcanoPlotServer("rg_Volcano_Plot", plot_table_reset, selected_row_data, correlation_button_clicked, rv_dta_rg)
    
  })

  output$h2_table <- renderDT({
    req(dta_h2_1())
    
    if(nrow(dta_h2_1()) == 0){
      return(data.frame(Notice = "No data available for the selected criteria."))
    }
    
    # Add Correlation buttons to data
    dta_for_table <- dta_h2_1() 
      
    dta_for_table$Correlation <- sapply(1:nrow(dta_for_table), function(i) {
      paste0('<button id="correlation-btn-', i, '">Correlation</button>')
    })
    dta_for_table$liability <- sapply(1:nrow(dta_for_table), function(i) {
      if(!is.na(dta_for_table$ncase[i])){
        paste0('<button id="liability-btn-', i, '">liability</button>')
      }else{
        paste0('NA')
      }
    })
    dta_for_table <- dta_for_table %>% select(-n,-ratio_th,-ncase,-ncohort,-p)
    
    datatable(dta_for_table, 
              escape = FALSE, 
              filter='top',
              selection = list(target = 'row', mode="single"),
              options = list(pageLength = 5),
              callback = JS("
                    table.on('click.dt', 'button[id^=correlation]', function() {
                              var btnId = $(this).attr('id');
                              Shiny.setInputValue('correlation_button', btnId, {priority: 'event'});
                    });
                    table.on('click.dt', 'button[id^=liability]', function() {
                              var btnId = $(this).attr('id');
                              Shiny.setInputValue('liability_button', btnId, {priority: 'event'});
                    });
             ")
    )
  })

  observeEvent(input$show_guide, {
        guideModalServer("guide_id")
  })
}
# shiny app --------------------------------------------------------------------

shinyApp(ui, server)