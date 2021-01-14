library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)

source('linearRegression.R')
example_file <- read.csv('stability_stats.csv')

ui = tagList(
    navbarPage(
        theme = shinytheme("journal"),  
        "Regression for Stability",
        tabPanel("",
                 sidebarPanel(
                     downloadButton("downloadData", "Download Stats Template"),
                     br(),
                     br(),
                     downloadButton("downloadExample", "Download Stats Example"),
                     br(),
                     br(),
                     fileInput('target_upload', 'Choose file to upload',
                               accept = c(
                                   'text/csv',
                                   'text/comma-separated-values',
                                   '.csv'
                               )),
                     selectInput('CI', 'Confidence Interval', choices=c(0.85, 0.90, 0.95, 0.99), selected = 0.95),
                     textInput('threshold', '% of 4C Reference MFI Threshold', value=75),
                     checkboxGroupInput('conc_avgs', 'Regression Concentrations', choices = c(
                         '50 ng/test' = 2, 
                         '100 ng/test' = 3,
                         '150 ng/test' = 4,
                         '200 ng/test' = 5,
                         '250 ng/test' = 6,
                         '500 ng/test' = 7,
                         '1000 ng/test' = 8
                     ),
                     selected = c(
                         '50 ng/test' = 2, 
                         '100 ng/test' = 3,
                         '150 ng/test' = 4,
                         '200 ng/test' = 5,
                         '250 ng/test' = 6,
                         '500 ng/test' = 7,
                         '1000 ng/test' = 8
                     )),
                     radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                     downloadButton("report", "Generate report", class = "btn-primary")
                 ),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Stats Table",
                                  br(),
                                  DT::dataTableOutput("sample_table")
                                  
                                  
                         ),
                         tabPanel("Regression & Shelf-Life", 
                                  wellPanel(h4(p(strong("Regression Analysis for Stability"))), 
                                            plotOutput("plot1", height = 350,
                                                       click = "plot1_click",
                                                       brush = brushOpts(
                                                           id = "plot1_brush"
                                                       )
                                            ),
                                            actionButton("exclude_reset", "Reset")
                                  ),
                                  wellPanel(h4(p(strong("Predicted Shelf-Life"))), textOutput('shelf_life_output'),
                                            h4(p(strong("Predicted Shelf-Life w/ 95% Confidence"))), textOutput('shelf_life_lower_output')

                                            )
                                  )
                         
                     )
                 )
        )

    )
)
server = function(input, output) {
    
    template_data <- data.frame('Time'=c(0:7), 
                                'Conc_50_ng'=rep(NA, 8), 
                                'Conc_100_ng'=rep(NA, 8),
                                'Conc_150_ng'=rep(NA, 8),
                                'Conc_200_ng'=rep(NA, 8),
                                'Conc_250_ng'=rep(NA, 8),
                                'Conc_500_ng'=rep(NA, 8),
                                'Conc_1000_ng'=rep(NA, 8),
                                row.names=NULL
                                )
    
    example_data <- data.frame(read.csv('stability_stats.csv'))
    
    # Download Template Stats File
    output$downloadData <- downloadHandler(
        filename = 'stability_stats_template.csv',
        content = function(file) {
            write.csv(template_data, file, row.names=FALSE)
        }
    )
    
    # Download Example Stats File
    output$downloadExample <- downloadHandler(
        filename = 'stability_stats_example.csv',
        content = function(file) {
            write.csv(example_file, file, row.names=FALSE)
        }
    )
    
    df_products_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile)){ 
            df_unrounded <- read.csv('stability_stats.csv', header = TRUE, sep = ',')
            df <- cbind('Time'=df_unrounded[[1]], round(df_unrounded[2:ncol(df_unrounded)]))
            return(df)
        }
        df_unrounded <- read.csv(inFile$datapath, header = TRUE, sep = ',')
        df <- cbind('Time'=df_unrounded[[1]], round(df_unrounded[2:ncol(df_unrounded)]))
        return(df)
    })
    
    # Reactive User Inputs
    confidenceInterval <- reactive({input$CI})
    threshold_y <- reactive({input$threshold})
    
    # Concentrations to average
    conc_avgs_list <- reactive({ unlist(strsplit(input$conc_avgs, " ")) })
    output$conc_avgs_list_output <- renderText({ length(conc_avgs_list()) })
    
    # Run linearRegression.R file
    full_data_table <- reactive({
        if(is.null(input$target_upload)){
            return(conc_to_exclude(df_products_upload(), conc_avgs_list()))
        }
        
        return(conc_to_exclude(df_products_upload(), conc_avgs_list()))
    })
    
    

    melted_data_table <- reactive({meltedDataTable(full_data_table())})
    output$melt_out <- renderPrint({melted_data_table()})
    regression_data_table <- reactive({regressionDataTable(full_data_table())})
    confidence_intervals <- reactive({ reg_conf_intervals(keep()$Time, keep()$value, as.numeric(confidenceInterval()), as.numeric(threshold_y())) })
    
    labels <- reactive({ labels_column(input$target_upload) })
    
    ###############################################################################
    # For storing which rows have been excluded
    vals <- reactiveValues(
        keeprows = rep(TRUE, 64)
        
    )
    observeEvent(input$target_upload, {
        vals$keeprows <- rep(TRUE, nrow(melted_data_table()))
        
    })
    
    output$plot1 <- renderPlot({
        # Plot the kept and excluded points as two separate data sets
        keep    <- melted_data_table()[ vals$keeprows, , drop = FALSE] 
        exclude <- melted_data_table()[!vals$keeprows, , drop = FALSE] 
        output$text1 <- renderPrint(keep)
        keep <- keep[complete.cases(keep),]
        print(keep)
        ggplot(keep, aes(x=Time, y=value, color=Concentrations)) + 
            geom_smooth(data=keep, aes(x=Time, y=value), formula = y ~ x, method="lm", col = "red", level=as.numeric(confidenceInterval())) +
            geom_point(size=10) + 
            geom_point(data = exclude, size = 10, shape = 21, fill = NA, color = 'black') +
            labs(x = "Time (years)",
                          y = "% of 4C Reference MFI") +
                     theme_minimal() +
                     scale_color_brewer(palette = 'Reds', na.translate = F,
                                        labels = unique(keep$Labels)
                                        # labels = c("30 ng/test", '60 ng/test', '125 ng/test', '250 ng/test', '500 ng/test', '1000 ng/test', '2000 ng/test')
                     )
        
        

        
    })
    keep    <- reactive({ melted_data_table()[ vals$keeprows, , drop = FALSE] })
    output$text1 <- renderPrint(keep())
    
    # Toggle points that are clicked
    observeEvent(input$plot1_click, {
        res <- nearPoints(melted_data_table(), input$plot1_click, allRows = TRUE) 
        output$text2 <- renderPrint(vals$keeprows)
        vals$keeprows <- xor(vals$keeprows, res$selected_) 
        output$text3 <- renderPrint(xor(vals$keeprows, res$selected_))
    })
    
    # Toggle points that are brushed, when button is clicked
    observeEvent(input$exclude_toggle, {
        res <- brushedPoints(melted_data_table(), input$plot1_brush, allRows = TRUE)
        
        vals$keeprows <- xor(vals$keeprows, res$selected_)
    })
    
    # Reset all points
    observeEvent(input$exclude_reset, {
        vals$keeprows <- rep(TRUE, 64)
    })
    
    
    
    # When file uploaded, create plot
    df_full <- eventReactive(input$target_upload,{
        plot <- createPlot(melted_data_table(), regression_data_table(), as.numeric(confidenceInterval()))
        if (is.null(input$target_upload)) {
            return (example_data)
        }
        return (full_data_table)
    })
    
    
    # When file uploaded, output shelf-life from linearRegression.R file
    output$shelf_life_output <- renderText({
        SL <- paste(summarizeData(keep(), as.numeric(threshold_y())), ' years')
    })
    
    output$shelf_life_lower_output <- renderText({
        SL <- paste(confidence_intervals(), ' years')
    })
    
    output$sample_table <- DT::renderDataTable({
        df <- full_data_table()
        
        DT::datatable(df, options=list(pageLength=8)) %>%
            formatRound(columns=c(1), 1) %>%
            formatRound(columns=c(2:ncol(df)), 0)
    })
    

    
    ################## R Markdown Report ######################
    output$report <- downloadHandler(
        
        # For PDF output, change this to "report.pdf"
        filename = "report.pdf",
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            # Set up parameters to pass to Rmd document
            params <- list(CI = input$CI,
                           threshold = input$threshold,
                           fullDT = full_data_table(),
                           meltedDT = melted_data_table(),
                           shelf_life = summarizeData(melted_data_table(), as.numeric(input$threshold)),
                           shelf_life_lower = confidence_intervals()
            )
            
            rmarkdown::render(tempReport, output_format = "pdf_document", output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
            
        }
    )
}
shinyApp(ui=ui, server=server)