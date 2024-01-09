
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
        # facet_grid(rows = vars(Region),
        #            cols = vars(Length_Category), 
        #            scales = "free", 
        #            space="free_y") 
      
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
