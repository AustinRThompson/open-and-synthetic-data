library(shiny)
library(dplyr)
library(synthpop)
library(osfr)

# Define UI for the app
ui <- fluidPage(
    
    # Title with 'synthpop' italicized
    titlePanel(
        HTML("<h1>Synthetic Data Generation with the <i>synthpop</i> package</h1>")
    ),
    
    # Define tab structure
    tabsetPanel(
        # First tab: Instructions
        tabPanel(
            title = "Instructions",
            h2("Instructions"),
            p("This website provides a simple user interface to generate synthetic data with the synthpop package in R (Nowok et al. 2018)."),
            p("The following steps are required:"),
            p("1. Set a seed for reproducibility. If you change this seed, it will generate a synthetic dataset with slightly different characteristics."),
            p("2. Upload your dataset in CSV file format."),
            p("3. Select the variables you'd like to keep for synthetic data generation."),
            p("4. Select the data type for each variable. If variables were characters and coded as numeric, then the synthetic dataset will not generate."),
            p("5. Generate synthetic data."),
            p("6. Compare synthetic and original data."),
            p("7. Download the synthetic data."),
            h3("Download Example Data File"),
            p("Data must be in long format. Below is an example of this format from Curtis et al. (2023)."),
            actionButton("download_example", "Download Example Data")
        ),
        
        # Second tab: Set Seed & Upload Data
        tabPanel(
            title = "Set Seed & Upload Data",
            h3("1. Specify seed number"),
            numericInput("seed", NULL, value = 1234, min = 1),
            h3("2. Upload CSV file"),
            fileInput("file1", NULL, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
        ),
        
        # Third tab: Select Variables & Data Type
        tabPanel(
            title = "Select Variables & Data Type",
            h3("3. Select variables for data synthesis"),
            p("Select the variables you'd like to keep for synthetic data generation."),
            p("This must include a study/participant ID. Note that synthetic data will only be generated for the variables selected."),
            uiOutput("var_select"),
            h3("4. Select the data type for each variable"),
            p("Note that if character variables are coded as numeric, then synthetic data will not be generated."),
            uiOutput("var_type")
        ),
        
        # Fourth tab: Data Preview
        tabPanel(
            title = "Data Preview",
            div(style = "text-align: center; font-size: 16px;",
                h3("Original Data Preview"),
                p("If values are shown as NA, this may indicate that the incorrect variable type was selected under the 'Select Variables & Data Type' tab."),
                tableOutput("data_preview")
            ),
            h3("5. Generate synthetic data"),
            actionButton("generate", "Generate Synthetic Data"),
            div(style = "text-align: center; font-size: 16px;",
                h3("Synthetic Data Preview"),
                tableOutput("synthetic_data_preview")
            )
        ),
        
        # Fifth tab: Compare Datasets
        tabPanel(
            title = "Compare Datasets",
            h3("6. Comparison of synthetic and original dataset"),
            verbatimTextOutput("comparison_output"),  # Utility output from compare()
            uiOutput("var_select_visualization"),
            actionButton("compare_button", "Compare Selected Variables"),
            uiOutput("comparison_plots")
        ),
        
        # Sixth tab: Download Synthetic Dataset
        tabPanel(
            title = "Download Synthetic Dataset",
            h3("7. Download synthetic dataset"),
            downloadButton("download_data", "Download CSV")
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive expression to read the uploaded CSV file
    uploaded_data <- reactive({
        req(input$file1)
        read.csv(input$file1$datapath)
    })
    
    # Dynamic UI for selecting variables to keep in dataset
    output$var_select <- renderUI({
        req(uploaded_data())
        checkboxGroupInput("selected_vars", NULL, choices = names(uploaded_data()), selected = names(uploaded_data()))
    })
    
    # Dynamic UI for selecting variable types (numeric or factor)
    output$var_type <- renderUI({
        req(input$selected_vars)
        lapply(input$selected_vars, function(var) {
            selectInput(paste0("type_", var), label = paste("Select Type for", var), choices = c("numeric", "factor"), selected = "numeric")
        })
    })
    
    # Reactive to modify dataset based on user selections
    modified_data <- reactive({
        req(input$selected_vars)
        data <- uploaded_data()[, input$selected_vars, drop = FALSE]
        for (var in input$selected_vars) {
            type <- input[[paste0("type_", var)]]
            if (type == "numeric") {
                data[[var]] <- as.numeric(data[[var]])
            } else if (type == "factor") {
                data[[var]] <- as.factor(data[[var]])
            }
        }
        return(data)
    })
    
    # Output: Display the modified dataset
    output$data_preview <- renderTable({
        req(modified_data())
        head(modified_data())
    })
    
    # Reactive expression to generate synthetic data when button is clicked
    synthetic_data <- eventReactive(input$generate, {
        req(modified_data())
        req(input$seed)
        syn(modified_data(), method = "ctree", m = 1, seed = input$seed)$syn
    })
    
    # Output: Display the synthetic data
    output$synthetic_data_preview <- renderTable({
        req(synthetic_data())
        head(synthetic_data())
    })
    
    # Output: Comparison of synthetic and original datasets
    output$comparison_output <- renderPrint({
        req(synthetic_data())
        req(modified_data())
        compare(synthetic_data(), modified_data(), plot = FALSE)
    })
    
    # Dynamic UI for selecting variables to visualize
    output$var_select_visualization <- renderUI({
        req(input$selected_vars)
        checkboxGroupInput("visualize_vars", "Select Variables to Visualize:", choices = input$selected_vars, selected = input$selected_vars[1])
    })
    
    # Render comparison plots for selected variables
    observeEvent(input$compare_button, {
        req(synthetic_data())
        req(modified_data())
        selected_vars <- input$visualize_vars
        output$comparison_plots <- renderUI({
            plot_output_list <- lapply(selected_vars, function(var) {
                plotOutput(outputId = paste0("plot_", var))
            })
            do.call(tagList, plot_output_list)
        })
        lapply(selected_vars, function(var) {
            output[[paste0("plot_", var)]] <- renderPlot({
                original_subset <- modified_data()[, var, drop = FALSE]
                synthetic_subset <- synthetic_data()[, var, drop = FALSE]
                compare(synthetic_subset, original_subset, plot = TRUE)
            })
        })
    })
    
    # Download handler for synthetic dataset
    output$download_data <- downloadHandler(
        filename = function() {
            paste("synthetic_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(synthetic_data())
            write.csv(synthetic_data(), file, row.names = FALSE)
        }
    )
    
    # Action to download example data file from OSF
    observeEvent(input$download_example, {
        osfr::osf_retrieve_file("anmz4") |>
            osfr::osf_download(conflicts = "overwrite")
    })
}

# Run the app
shinyApp(ui = ui, server = server)
