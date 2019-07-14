library(shinydashboard)
library(DT)

concrete <-read.csv("concrete.csv")

ui <- dashboardPage(
    dashboardHeader(title = "AI Concrete",
                    
                    dropdownMenu(type = "messages",
                                 messageItem(
                                     from = "Hey there!",
                                     message = "AI Concrete welcomed you.",
                                     icon=icon("user")
                                 ),
                                 
                                 messageItem(
                                     from = "Support",
                                     message = "Contact us at aiconcrete@utp.edu.my.",
                                     icon = icon("life-ring"),
                                     time = "2019-07-13"
                                 )
                    ),#dropdown message
                    
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                     text = "468 person visited us today!",
                                     icon("users")
                                 ),
                                 notificationItem(
                                     text = "5687 people liked this project",
                                     icon("thumbs-up")
                                 )
                    )#dropdown Notification
                
                    
                    ),#end of dropdown menu
    
    dashboardSidebar(
        sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                          label = "Search..."),
        sidebarMenu(
            menuItem("Home Page", tabName = "homepage", icon = icon("home")),
            menuItem("Summary of Data", tabName = "summary", icon = icon("chart-bar")),
            menuItem("Variable Correlation", tabName = "variable", icon = icon("chart-line")),
            menuItem("ANN Model", tabName = "annModel", icon = icon("project-diagram")),
            menuItem("Prediction", tabName = "prediction", icon = icon("cloudversify"))
        )
        
        
    ),#end of dashboard sidebar
    
    dashboardBody(
        tabItems(
            # Home page
            tabItem(tabName = "homepage"
                   
                    
            ),
            
            # Summary
            tabItem(tabName = "summary",
                    h2("Raw Data"),
                    dataTableOutput("dataTable"),
                    downloadButton("downloadCsv", "Download as CSV")
            ),
            
            #--------------------------------------Variable Correlation tab-------------------------------------#
            tabItem(tabName = "variable",
                    fluidRow(
                        column(4,
                            box(width=12,
                            #--------------------Checkbox-----------------------------#
                            checkboxGroupInput("var", 
                                               h4("Variables"), 
                                               choices = list("Cement" = "Cement", 
                                                              "Slag" = "Slag", 
                                                              "Ash" = "Ash",
                                                              "Water" = "Water",
                                                              "Superplastic" = "Superplastic",
                                                              "Coarseagg" = "Coarseagg",
                                                              "Fineagg" = "Fineagg",
                                                              "Age" = "Age"),
                                               ),
                            #----------------------Selected Var-----------------------#
                            h4("You have selected:"),
                            htmlOutput("selected_var"),
                            #----------------------Range of Data----------------------#
                            sliderInput(inputId = "dataRange",
                                        label = "Range of Data:",
                                        min = 1,
                                        max = 1030,
                                        value = 500)
                            )#end of box
                        
                        ),#end of column
                        
                        column(8,
                               plotOutput("distPlot")
                        )#end of column
                        
                    )#end of fluid row
            ),
            
            #-------------------------------------------ANN Model tab------------------------------------------#
            tabItem(tabName = "annModel",
                    h2("ANN Model here")
            ),
            
            #-------------------------------------------Prediction tab-----------------------------------------#
            tabItem(tabName = "prediction",
                    h2("Prediction here")
                    
            )
            

        )#end of tab items
    )#end of dashboard body
)

server <- function(input, output,session) {
    #download the raw data
    output$downloadCsv <- downloadHandler(
        filename = "concrete.csv",
        content = function(file) {
            write.csv(concrete, file)
        },
        contentType = "text/csv"
    )#end of download
    
    myCSV <- reactiveFileReader(100, session, 'concrete.csv', read.csv)#get data from concrete.csv
    
    output$dataTable <- renderDT(
        myCSV(), # reactive data
        class = "display nowrap compact", # style
        filter = "top", # location of column filters
        options = list(  # options
            scrollX = TRUE # allow user to scroll wide tables horizontally
        )
    )#end of output$datatable
    
    output$selected_var <- renderText({
        HTML(paste(input$var,collapse = "<br/>"))
    })
    
    output$distPlot <- renderPlot({
        
        x    <- faithful$waiting
        dataRange <- seq(min(x), max(x), length.out = input$dataRange + 1)
        
        hist(x, breaks = dataRange, col = "#75AADB", border = "white",
             xlab = "X",
             main = "Histogram of BlaBlaBla")
        
    })

}#end of server

shinyApp(ui, server)