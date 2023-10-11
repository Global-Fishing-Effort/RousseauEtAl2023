
# library(shiny)

#### step 1 - create empty framework: ui, server, shinyApp ----

# # Define UI for miles per gallon app ----
# 
# # The three functions headerPanel, sidebarPanel, and mainPanel define the 
# # various regions of the user-interface. The application will be called “Miles Per Gallon” 
# # so we specify that as the title when we create the header panel. The other panels are empty for now.
# 
# # We want to provide a way to select which variable to plot MPG against 
# # as well as provide an option to include or exclude outliers from the plot. 
# # To do this we’ll add two elements to the sidebar, a selectInput to specify 
# # the variable and a checkboxInput to control display of outliers. 
# # Our user-interface definition looks like this after adding these elements:
# 
# # ui <- pageWithSidebar(
# #   
# #   # App title ----
# #   headerPanel("Miles Per Gallon"),
# #   
# #   # Sidebar panel for inputs ----
# #   sidebarPanel(
# #     
# #     # Step 2 fill in input and output in sidebar panel -----
# #     
# #     # Input: Selector for variable to plot against mpg ----
# #     selectInput("variable", "Variable:", c("Cylinders" = "cyl","Transmission" = "am","Gears" = "gear")),
# #     
# #     # Input: Checkbox for whether outliers should be included ----
# #     checkboxInput("outliers", "Show outliers", TRUE)
# #     
# #   ),
# #   
# #   # Main panel for displaying outputs ----
# #   mainPanel()
# # )
# 
# ### step 4 udate the ui to consider outputs 
# 
# # Define UI for miles per gallon app ----
# ui <- fluidPage( ## changed 
#   
#   # App title ----
#   titlePanel("Miles Per Gallon"), ## changed 
#   
#   # Sidebar layout with input and output definitions ----
#   sidebarLayout( ## changed 
#     
#     # Sidebar panel for inputs ----
#     sidebarPanel(
#       
#       # Input: Selector for variable to plot against mpg ----
#       selectInput("variable", "Variable:",c("Cylinders" = "cyl","Transmission" = "am","Gears" = "gear")),
#       
#       # Input: Checkbox for whether outliers should be included ----
#       checkboxInput("outliers", "Show outliers", TRUE)
#       
#     ),
#     
#     # Main panel for displaying outputs ----
#     mainPanel( ## changed and the most imporant change 
#       
#       # Output: Formatted text for caption ----
#       h3(textOutput("caption")),
#       
#       # Output: Plot of the requested variable against mpg ----
#       plotOutput("mpgPlot")
#       
#     )
#   )
# )
# 
# ### Step 3 - fill in the server function -----
# # # Define server logic to plot various variables against mpg ----
# # server <- function(input, output) {
# # 
# # }
# 
# # Data pre-processing ----
# # Tweak the "am" variable to have nicer factor labels -- since this
# # doesn't rely on any user inputs, we can do this once at startup
# # and then use the value throughout the lifetime of the app
# mpgData <- mtcars
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
# 
# # Define server logic to plot various variables against mpg ----
# server <- function(input, output) {
#   
#   # Compute the formula text ----
#   # This is in a reactive expression since it is shared by the
#   # output$caption and output$mpgPlot functions
#   formulaText <- reactive({
#     paste("mpg ~", input$variable)
#   })
#   
#   # Return the formula text for printing as a caption ----
#   output$caption <- renderText({
#     formulaText()
#   })
#   
#   # Generate a plot of the requested variable against mpg ----
#   # and only exclude outliers if requested
#   output$mpgPlot <- renderPlot({
#     boxplot(as.formula(formulaText()),
#             data = mpgData,
#             outline = input$outliers,
#             col = "#007bc2", pch = 19)
#   })
#   
# }
# 
# 
# # Finally, we need the shinyApp function that uses the ui object and the server function we defined to build a Shiny app object.
# # 
# # Putting it altogether, our app.R script looks like this:
# 
# shinyApp(ui, server)

## tentative 2 - changing Julia's code only ---- 

rm(list = ls())

library(shiny)
library(plotly)
library(DT)
library(tidyverse)
library(patchwork)
library(viridis)

effort<-read_csv(file="https://data.imas.utas.edu.au/attachments/1241a51d-c8c2-4432-aa68-3d2bae142794/CapacityCountryLevel_Detailed.csv")[,-1]

# convert character to factor for key drop-down variables

effort$Country<-as.factor(effort$Country)
effort$Sector<-as.factor(effort$Sector)
effort$Gear<-as.factor(effort$Gear)

rel_effort_error<-effort %>% 
  group_by(Region, Country, Sector, Gear, Length_Category) %>%
  summarise(total_errNV = sum(NVerr), 
            total_errGT = sum(GTerr),
            total_errP = sum(Perr),
            mean_errNV = mean(NVerr), 
            mean_errGT = mean(GTerr),
            mean_errP = mean(Perr))


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Explore Mean Error"),
      selectInput(inputId = "Sector", label = "Sector",
                  choices = levels(rel_effort_error$Sector),
                  selected = "I"),
      # selectInput(inputId = "Country", label = "Country",
      #             choices = levels(effort$Country),
      #             selected = "GBR"),
      # selectInput(inputId = "Sector", "Sector",
      #             choices = levels(effort$Sector),
      #             selected = "I"),
      # selectInput(inputId = "Gear", "Gear",
      #             choices = levels(effort$Gear),
      #             selected = "Trawl_Bottom"),
      downloadButton(outputId = "download_data", label = "Download"),
    ),
    mainPanel(
      plotlyOutput(outputId = "plot"), br(),
      em("Mean Relative Error across all years"),
      br(), br(), br(),
      DT::dataTableOutput(outputId = "table")
    )
  )
)


server <- function(input, output) {
  # filtered_data <- reactive({
  #   subset(effort,Country %in% input$Country & Sector %in% input$Sector & Gear %in% input$Gear)})
  # 
  filtered_data <- reactive({
    subset(rel_effort_error, Sector %in% input$Sector)})
  
  output$plot <- renderPlotly({
    ggplotly({
      p1 <- ggplot(filtered_data(), aes_string(x="Gear", y="Country", fill = "mean_errNV")) +
        geom_tile(color="white", size=0.1)+
        theme(axis.text.x = element_text(angle = 90, 
                                         vjust = 0.5, 
                                         hjust=1,
                                         size=6),
              axis.text.y = element_text(size=3),
              strip.text.y = element_blank()) +  #remove facet bar on y
        scale_fill_viridis(name="Mean Relative Error") +
        ggtitle("Mean error for Number of Vessels (1-  predicted/observed)")
        # facet_wrap(~Region)
        # facet_grid(Region~Length_Category)
        
      
      
      # p1 <- ggplot(filtered_data(), aes_string(x="Year", y="NV",colour="MethodNV")) +
      #   geom_point(alpha=0.5) +
      #   facet_wrap(~Length_Category,nrow =1)
      # theme_minimal() +
      #   ylab("Number of vessels")
      # 
      # 
      # p2 <-ggplot(filtered_data(), aes_string(x="Year", y="GT",colour="MethodGT")) +
      #   geom_point(alpha=0.5) +
      #   facet_wrap(~Length_Category,nrow =1)
      # theme_minimal() +
      #   ylab("Gross Tonnage")
      # 
      # 
      # p3 <-ggplot(filtered_data(), aes_string(x="Year", y="P",colour="MethodP")) +
      #   geom_point(alpha=0.5) +
      #   facet_wrap(~Length_Category,nrow =1)
      # theme_minimal() +
      #   ylab("Power")
      # 
      # subplot(p1,p2,p3,nrows=3,shareX=F,shareY=F,margin=0.1)
      
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)









