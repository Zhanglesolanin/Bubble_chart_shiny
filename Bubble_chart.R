library(shiny)
library(ggplot2)
library(shinythemes)
library(enrichplot)
library(stringr)

df<-read.csv("./GOE.csv",header = T)
colnames(df)=c("Cluster","ID","Description","GeneRatio","BgRatio","pvalue","p.adjust","qvalue","geneID","Count")
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Bubble 2018/04/08 Zhangle"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Chart"',
        checkboxGroupInput("show_vars", "Columns in Result integration to show:",
                           names(df), selected = c("Cluster","ID","GeneRatio","BgRatio","pvalue","p.adjust","qvalue","Count"))
      ),
      conditionalPanel(
        'input.dataset === "Plot"',
        helpText(),
        selectInput('description', 'Description', str_sort(df$Description), multiple=TRUE, selectize=TRUE),
        radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                     inline = TRUE),
        downloadButton('downloadReport')
      )
    ),
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  id = 'dataset',
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel('Chart',        DT::dataTableOutput('chart'))
      )
      
    )
  )
)


server <- function(input, output) {
  choiceData <- reactive({
    subset(df, Description %in% input$description)
  })
  output$plot <- renderPlot({
    Description <- Percentage <- Count <- Cluster <- GeneRatio <- p.adjust <- pvalue <- NULL
    p <- ggplot(choiceData(),aes_(x = ~Description, y = ~Cluster, size = ~pvalue))
    p <- p +
      geom_point() +
      aes_string(color="p.adjust") +theme_bw()+theme(axis.text.x = element_text(size=8,face = "bold",angle=30, hjust=1, vjust=1))+
      scale_color_gradientn(guide=guide_colorbar(reverse=TRUE), colors = enrichplot:::sig_palette)
    p
  })
  output$chart <- DT::renderDataTable({
    DT::datatable(df[, input$show_vars, drop = FALSE], options = list(orderClasses = TRUE,lengthMenu = c(5, 10, 30, 50), pageLength = 50))
  })
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Bubble_report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('./report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, './report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('./report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)