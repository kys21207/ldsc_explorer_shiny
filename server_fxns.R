#server_fxns.R
rgtableServer <- function(id, plot_table_reset,correlation_button_clicked,data=reactive({NULL})) {
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns    
    output$ui_output <- renderUI({
      if (!plot_table_reset() && !correlation_button_clicked()) {
        return(NULL)  # Return nothing if plot_table_reset is TRUE
      }else{
        fluidRow(
          box(width=12, status = "warning", solidHeader = T, collapsible = T, title = "rg table for Analysis2 with a selected trait",
            DTOutput(ns("rg_table")), style = "overflow-x: scroll")
        )
      }
    })
    output$rg_table <- renderDT({
  # rv_dta_rg   
      req(data())
      if(nrow(data()) > 0){
        datatable(data(), 
                  escape = FALSE, 
#                  selection = list(target = 'row', mode="single"),
                  options = list(pageLength = 5))
      } else {
        return(data.frame(Notice = "No data available for the selected criteria."))
      }
    })  
  })
}


rgDist_p2RatioPlotServer <- function(id, plot_table_reset, correlation_button_clicked, rv_dta_rg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns    
    output$ui_output <- renderUI({
      if (!plot_table_reset() && !correlation_button_clicked()) {
        return(NULL)  # Return nothing if plot_table_reset is TRUE
      }else{
      fluidRow(
        box(width=12, status = "warning", solidHeader = T, collapsible = T, title = "Plot Distributions of rg and ratio for Analysis2",
            splitLayout(cellWidths = c("50%", "50%"), plotOutput(ns("rg_dist_plot"),height="600px"), plotOutput(ns("p2_ratio_plot"),height="600px")))
      )
      }
    })

    output$rg_dist_plot <- renderPlot({
      req(rv_dta_rg())
      if(nrow(rv_dta_rg()) > 0 ){
 #       data_to_plot <- selected_row_data2()
        dta <- rv_dta_rg()
        ggplot(dta, aes(x=rg)) + 
          geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                         binwidth=.02,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#FF6666") +
 #         geom_vline(xintercept = data_to_plot$rg,linetype = "dashed",color="blue") +
          theme_bw()+ # Overlay with transparent density plot
          theme(axis.title=element_text(size=20),
                axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20))
      }else{
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No data available for the selected criteria.", 
                          hjust = 0.5, vjust = 0.5))
      }
    })
    output$p2_ratio_plot <- renderPlot({
      req(rv_dta_rg())
      if(nrow(rv_dta_rg()) > 0 ){
    #    data_to_plot <- selected_row_data2()
        dta <- rv_dta_rg()
        ggplot(dta, aes(x=p2_ratio)) + 
          geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                         binwidth=.02,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#FF6666") +
          geom_vline(xintercept = dta$ratio_th[1],linetype = "dashed",color="red") +
#          geom_vline(xintercept = data_to_plot$p2_ratio,linetype = "dashed",color="blue") +
          theme_bw()+ # Overlay with transparent density plot
          theme(axis.title=element_text(size=20),
                axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20)
          )
      }else{
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No data available for the selected criteria.", 
                          hjust = 0.5, vjust = 0.5))
      }
    })
    
  })
}

h2DistRatioPlotServer <- function(id, plot_table_reset, selected_row_data, dta_h2_1=reactive({NULL})) {
  stopifnot(is.reactive(dta_h2_1))
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$ui_output <- renderUI({
      if (!plot_table_reset() && is.null(selected_row_data())) {
        return(NULL)  # Return nothing if plot_table_reset is TRUE
      }else{
      
      fluidRow(
        box(width=12, status = "warning", solidHeader = T, collapsible = T, title = "Plot Distributions of h2 and ratio for Analysis1",
            splitLayout(cellWidths = c("50%", "50%"), plotOutput(ns("h2_dist_plot"),height="600px"), plotOutput(ns("h2_ratio_plot"),height="600px")))
      )
      }
    })
    output$h2_dist_plot <- renderPlot({
      req(selected_row_data())
      if(nrow(selected_row_data()) > 0 ){
        data_to_plot <- selected_row_data()
        
        dta <- dta_h2_1()
        ggplot(dta, aes(x=h2)) + 
          geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                         binwidth=.02,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#FF6666") +
          geom_vline(xintercept = data_to_plot$h2,linetype = "dashed",color="blue") +
          theme_bw()+ # Overlay with transparent density plot
          theme(axis.title=element_text(size=20),
                axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20))
      }else{
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No data available for the selected criteria.", 
                          hjust = 0.5, vjust = 0.5))
      }
    })
    output$h2_ratio_plot <- renderPlot({
      req(selected_row_data())
      if(nrow(selected_row_data()) > 0 ){
        data_to_plot <- selected_row_data()
        
        dta <- dta_h2_1()
        ggplot(dta, aes(x=ratio)) + 
          geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                         binwidth=.02,
                         colour="black", fill="white") +
          geom_density(alpha=.2, fill="#FF6666") +
          geom_vline(xintercept = dta$ratio_th[1],linetype = "dashed",color="red") +
          geom_vline(xintercept = data_to_plot$ratio,linetype = "dashed",color="blue") +
          theme_bw()+ # Overlay with transparent density plot
          theme(axis.title=element_text(size=20),
                axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20)
          )
      }else{
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No data available for the selected criteria.", 
                          hjust = 0.5, vjust = 0.5))
      }
    })
  })
}

rgVolcanoPlotServer <- function(id, plot_table_reset, selected_row_data,correlation_button_clicked, rv_dta_rg) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns 
    output$ui_output <- renderUI({
      if (!plot_table_reset() && !correlation_button_clicked()) {
        return(NULL)  # Return nothing if plot_table_reset is TRUE
      }else{
      fluidRow(
        box(width=10, status = "warning", solidHeader = T, collapsible = T, title = "Volcano Plot for Analysis2 with a selected trait",
            plotOutput(ns("rg_volcano_plot"),height="800px"))      )
      }
    })
    
    output$rg_volcano_plot <- renderPlot({
      req(rv_dta_rg())
       if(nrow(rv_dta_rg()) > 0){
        data_rg <- rv_dta_rg()
        # add a column of NAs
        data_rg$Sig_phenotype <- "NOT"
        data_rg$Sig_phenotype[ data_rg$rg > 0 & data_rg$p < (0.05/selected_row_data()$n)] <- "Positive"
        data_rg$Sig_phenotype[ data_rg$rg < 0 & data_rg$p < (0.05/selected_row_data()$n)] <- "Negative"
        
        data_rg$delabel <- NA
        data_rg$delabel[data_rg$Sig_phenotype != "NOT"] <- data_rg$p2_phenotype[data_rg$Sig_phenotype != "NOT"]
        
        p <- ggplot(data=data_rg, aes(x=rg, y=-log10(p), col=Sig_phenotype, label=delabel)) + geom_point() + 
          theme_minimal() +
          geom_text(size=4,hjust = 0, nudge_x = 0.02) +
          geom_hline(yintercept = -log10(0.05/selected_row_data()$n),linetype = "dashed",color="orange") 
          
        mycolors <- c("blue", "red", "black")
        names(mycolors) <- c("Negative", "Positive", "NOT")
        p3 <- p + scale_colour_manual(values = mycolors)+
          theme(axis.title=element_text(size=20),
                axis.text.x=element_text(size=20),
                axis.text.y=element_text(size=20),
                legend.title = element_text(size = 18),
                legend.text = element_text(size = 16))
        p3
      } else {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No data available for the selected criteria.", 
                          hjust = 0.5, vjust = 0.5))
      }
    })
  })
}

guideModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
      showModal(modalDialog(
        title = "User Guide",
        tags$ul(
          tags$li(tags$strong("Navigation:"),
                  tags$ul(
                    tags$li(tags$strong("Tutorial:"), "To understand how to generate h2 & rg and get help interpreting them, navigate to the \"Tutorials\" tab where a document is embedded."),
                    tags$li(tags$strong("Heritability & Correlation:"), "Under this tab, you can explore the heritability and genetic correlation of a selected trait.")
                  )
          ),
          tags$li(tags$strong("Heritability (h2) Selection:"),
                  tags$ul(
                    tags$li(tags$strong("Select Classification:"), "Choose the trait classification from the dropdown menu. Options include 'Disease', 'MeSH', and 'MeSH label'."),
                    tags$li(tags$strong("Select Category:"), "Based on the classification selected, pick a specific category from the dropdown."),
                    tags$li(tags$strong("Select Z h2 Threshold:"), "Adjust the slider to set the Z h2 threshold. (default=1.5) "),
                    tags$li(tags$strong("Select Ratio Threshold:"), "Adjust the slider to define the desired ratio threshold. (default=0.2, but adjust to 0.45 for quantitative trait)"),
                    tags$li(tags$strong("Click on the RUN button"), "to apply your selection.")
                  )
          ),
          tags$li(tags$strong("Viewing Results:"),
                  tags$ul(
                    tags$li(tags$strong("Heritability Bar Plot:"), "After selecting criteria, you'll be presented with two bar & density plots visualizing the selected data based on the heritability & ratio estimates. The red vertical line indicates the desired ratio threshold. And the blue vertical line indicates the h2 & ratio for the selected trait. "),
                    tags$li(tags$strong("Heritability Data Table:"), "Below the plots, a table displays detailed heritability data for the selected trait vs the selected category.")
                  )
          ),
          tags$li(tags$strong("Genetic Correlation:"),
                  tags$ul(
                    tags$li(tags$strong("In the heritability table, click on the Correlation button"), "corresponding to a specific trait. A modal dialog will pop up."),
                    tags$li(tags$strong("In this dialog:"),
                            tags$ul(
                              tags$li(tags$strong("Choose a Classification:"), "Select a desired classification from the dropdown. Options include 'Disease', 'MeSH', 'MeSH label' and 'All'."),
                              tags$li(tags$strong("Select a Category:"), "Depending on the chosen classification, select a specific category from the dropdown."),
                              tags$li(tags$strong("Select a p-value Threshold:"), "Adjust the slider to set a p-value threshold for genetic correlation."),
                              tags$li(tags$strong("Click on Submit"), "to view the genetic correlation data.")
                            )
                    )
                  )
          ),
          tags$li(tags$strong("Viewing Results:"),
                  tags$ul(
                    tags$li(tags$strong("Correlation volcano Plot:"), "After selecting criteria, you'll be presented with a volcano plot visualizing the selected trait vs the selected category or all other traits based on the genetic correlation estimates."),
                    tags$li(tags$strong("Correlation Data Table:"), "Below the plot, a table displays detailed genetic correlation data for the selected trait vs the selected category or all other traits.")
                  )
          ),
          tags$li(tags$strong("Downloading Results:"),
                  tags$ul(
                    tags$li(tags$strong("To save the heritability plot or table for your records,"), "click the corresponding Download buttons (if implemented).")
                  )
          )
        ),
        easyClose = TRUE
      ))
  })
}

#' Server function to update the single selectize inputs and return the selected values from it (max_level = 1)
#' @param id Namespace identifier for the module
#' @param data Reactive vector input of choices to be displayed
#' @param selected Reactive vector of selected choice
#' @param server Boolean input for server side processing 
single_selectize_server <- function(id, data, selected = reactive({NULL}), server = TRUE){
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(selected))
  moduleServer(id, function(input, output, session){
    # Note these are reactive based on the data input at this level (previosuly done at level above)
    observeEvent(data(), {
      if(isTruthy(data())){
        if(!is.null(selected())){
          updateSelectizeInput(session,"selectize_input",choices = data(), selected = selected(), server = server)
        } else {
          updateSelectizeInput(session,"selectize_input",choices = data(), server = server)
        }
      }
    })
    # Return values only when truthy
    return(
      reactive({
        if(isTruthy(input$selectize_input)){
          return(input$selectize_input)
        }
      })
    )
  })
}

#' Server function to monitor the value or status of a user input like a button or slider (max_level = 1)
#' @param id Namespace identifier for the module
#' @param disable (boolean) Can the input be disabled
#' @param condition (reactive boolean) Condition for disabling the input
monitor_input <- function(id,disable = TRUE, condition = reactive({NULL})){
  stopifnot(is.reactive(condition))
  moduleServer(id, function(input, output, session){
    observe({
      if(disable){
        if(condition()){
          shinyjs::enable("user_input")
        } else {
          shinyjs::disable("user_input")
        }
      }
    })
    return(reactive(input$user_input))
  })
}

#' Update the radio buttons which determine the result that is displayed to the user
#' @param id Namespace identifier for the module
radio_button_server <- function(id){
  moduleServer(id, function(input, output, session){
    updateRadioButtons(session, "buttons", choices = c("none","plot","table"), selected = "none")
    return(reactive({input$buttons}))
  })
}

#' Reactive UI element which displays a particular result to the user, depending
#' on the reactive value passed to radio_input
#' @param id Namespace identifier for the module
#' @param radio_input Reactive value which determines the result displayed to the user
ui_output_server <- function(id, radio_input = NULL){
  stopifnot(is.reactive(radio_input))
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Update when the radio is changed
    observeEvent(radio_input(),{
      button_choice <- radio_input()
      if(button_choice == "none"){
        output$ui_output <- renderUI({fluidRow(renderText({paste0("No result output has been selected.")}))})
      }
      if(button_choice == "plot"){
        output$ui_output <- renderUI({plot_with_selection_ui(id = ns("selection_plot"), label1 = "Select X variable", label2 = "Select Y variable", box_title = "Plot")})
        plot_with_selection_server(id = "selection_plot")
      }
      if(button_choice == "table"){
        output$ui_output <- renderUI({table_with_selection_ui(ns("selection_table"), box_title = "Table")})
        table_with_selection_server("selection_table")
      }
    }, ignoreInit = T)
  })
}

#' Generate a heat plot and table based on the data selected by user
#' @param id Namespace identifier for the module
#' @param dta Reactive dataframe of data to be displayed
heatmap_table_output_server <- function(id,dta=reactive({NULL})){
  stopifnot(is.reactive(dta))
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$ui_output <- renderUI({
      fluidRow(
        box(width=12, height = "100%", status = "warning", solidHeader = T, collapsible = T, title = "Table & Plot",
            withSpinner(plotOutput(ns("plot1"), height="600px"),type=1),
  #          withSpinner(plotOutput(ns("plot2"), height="800px"),type=1)
            withSpinner(plotOutput(ns("plot3"), height="800px"),type=1)),
            DTOutput(ns("table"))
     )
    })

    output$table = renderDT({
      downloadable_table(dta())
    })
    
    output$plot1 = renderPlot({
      gen_heatmap_rg_all(dta())
    })
    
#    output$plot2 = renderPlot({
#      gen_heatmap_rg_cluster(dta())
#    })
    output$plot3 = renderPlot({
      gen_dendrogram_rg_cluster(dta())
    })
    
    
  })
}

#' Generate a heat plot and table based on the data selected by user
#' @param id Namespace identifier for the module
#' @param dta Reactive dataframe of data to be displayed
heatmap_table1_output_server <- function(id,dta=reactive({NULL})){
  stopifnot(is.reactive(dta))
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$ui_output <- renderUI({
      fluidRow(
        box(width=12, status = "warning", solidHeader = T, collapsible = T, title = "Table & Plot",
            withSpinner(plotOutput(ns("plot1")),type=1)),
        DTOutput(ns("table"))
      )
    })
    
    output$table = renderDT({
      downloadable_table(dta())
    })
    
    output$plot1 = renderPlot({
      gen_heatmap_aver_rg_all(dta())
    })
  
  })
}

#' Generate a bar plot and table based on the data selected by user
#' @param id Namespace identifier for the module
#' @param dta Reactive dataframe of data to be displayed
table_histo_output_server <- function(id,dta=reactive({NULL})){
  stopifnot(is.reactive(dta))
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    output$ui_output <- renderUI({
      fluidRow(
      box(width=12, status = "warning", solidHeader = T, collapsible = T, title = "Table & Plot",
#        withSpinner(plotOutput(ns("plot")),type=1)),
        DTOutput(ns("table"))),
        splitLayout(cellWidths = c("50%", "50%"), plotOutput(ns("plotgraph1")), plotOutput(ns("plotgraph2")))
      )             
    })
    
    output$table = renderDT({
      downloadable_table(dta())
    })
    
    output$plot = renderPlot({
      gen_barplot_h2(dta())
    })
  })
}
#' Combination of the server processing for the radio button choice and the 
#' respective UI output that is shown to the user.
#' @param id Namespace identifier for the module
radio_with_ui_server <- function(id){
  moduleServer(id, function(input, output, session){
    button_choice <- radio_button_server(id = "radio_buttons")
    ui_output_server(id = "ui_output", radio_input = button_choice)
  })
}

#' Update the selection boxes which are used to select the X- and Y-axis variables
#' that are displayed on the plots and within tables
#' @param id Namespace identifier for the module
selection_server <- function(id){
  moduleServer(id, function(input, output, session) {
    #Selection boxes
    data <- mtcars
    updateSelectizeInput(session, "selection1", choices = names(data), server = TRUE, selected = names(data)[1])
    updateSelectizeInput(session, "selection2", choices = names(data), server = TRUE, selected = names(data)[2])
    
    #Read selection box choice
    return(list(
      xvar = reactive({input$selection1}),
      yvar = reactive({input$selection2})
    ))
  })
}


#' Generate a plot based on the X- and Y-axis variables selected by the user
#' @param id Namespace identifier for the module
#' @param selection_xvar Reactive value for the selected X-axis variable
#' @param selection_yvar Reactive value for the selected Y-axis variable
plot_server <- function(id, selection_xvar, selection_yvar){
  stopifnot(is.reactive(selection_xvar))
  moduleServer(id, function(input, output, session) {
    #Read data
    data <- mtcars
    
    #Plot
    selected <- reactive({
      list(selection_xvar(),
           selection_yvar())
    })
    observeEvent(selected(),{
      xvar <- selection_xvar()
      yvar <- selection_yvar()   
      if(isTruthy(xvar)){
        plot <- ggplot(data = data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
          geom_point() +
          theme_bw()
        output$plot <- renderPlot({plot})
      }
    })
  })
}

#' Combine the server processing for the variable selection boxes with the plot output
#' @param id Namespace identifier for the module
plot_with_selection_server <- function(id){
  moduleServer(id, function(input, output, session) {
    selection <- selection_server(id = "selection_output")
    plot_server(id = "plot_output", selection = selection$xvar,selection_yvar = selection$yvar)
  })
}

#' Render the tabular output for data that is passed to the function by \code{data}
#' @param id Namespace identifier for the module
#' @param data Reactive dataframe of data to be displayed
table_output_server <- function(id, data){
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {
    # Output the datatable
    output$table <- renderDT({downloadable_table(data())})
  })
}


#' Select columns from a dataframe based on variables chosen by the user
#' @param id Namespace identifier for the module
#' @param selection_xvar Reactive value for the selected X-axis variable
#' @param selection_yvar Reactive value for the selected Y-axis variable
manip_data <- function(selection_xvar,selection_yvar){
  stopifnot(is.reactive(selection_xvar))
  stopifnot(is.reactive(selection_yvar))
  # Take the input selection variables and manipulate the data
  return(reactive({
    data <- mtcars
    xvar <- selection_xvar()
    yvar <- selection_yvar()   
    return(data %>% select(any_of(c(xvar,yvar))))
  }))
}

#' Combine the server processing to display a table of selected columns from 
#' variable selection through to results table output
#' @param id Namespace identifier for the module
table_with_selection_server <- function(id){
  moduleServer(id, function(input, output, session) {
    selection <- selection_server(id = "selection_output")
    data_use <- manip_data(selection_xvar = selection$xvar ,selection_yvar = selection$yvar) 
    table_output_server(id = "table_output",data = data_use)
  })
}

#' Server function to monitor the value or status of a user input like a button or slider (max_level = 1)
#' @param id Namespace identifier for the module
#' @param disable (boolean) Can the input be disabled
#' @param condition (reactive boolean) Condition for disabling the input
monitor_input <- function(id,disable = TRUE, condition = reactive({NULL})){
  stopifnot(is.reactive(condition))
  moduleServer(id, function(input, output, session){
    observe({
      if(disable){
        if(condition()){
          shinyjs::enable("user_input")
        } else {
          shinyjs::disable("user_input")
        }
      }
    })
    return(reactive(input$user_input))
  })
}
