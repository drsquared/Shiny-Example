library(shiny)
library(ggplot2)
library(fmsb)

mvdf <- data.frame(matrix(nrow = 10, ncol = 15))
names(mvdf) <- c("Group", "Type", "Store", paste("m", 1:12, sep = ""))
mvdf$Group <- rep("Group1", 10)
mvdf$Type <- c(rep("Type1", 5), rep("Type2", 5))
mvdf$Store <- c(paste("Grp1Store", 1:5, sep = ""), paste("Grp2Store", 1:5, sep = ""))
mvdf[1, 4:15] <- c(1, 1, 1.2, 1.1, 1, 1, 1, 1.2, 1.2, 1.1, 1.2, 1.3)
mvdf[2, 4:15] <- c(1.1, 1, 1.2, 1.3, 1.3, 1.1, 1.3, 1.4, 1.5, 1.5, 1.4, 1.3)
mvdf[3, 4:15] <- c(1.8, 1.5, 1.9, 1.8, 2, 1.9, 2, 2.2, 2.2, 1.8, 1.8, 2.3)
mvdf[4, 4:15] <- c(1.3, 1, 1.2, 5.1, 6, 6, 4, 2.2, 1.2, 1.3, 1.1, 1.3)
mvdf[5, 4:15] <- c(5, 5.1, 5.2, 5.1, 6, 5, 5, 5.2, 5.2, 5.1, 5.2, 5.3)
mvdf[6, 4:15] <- c(1, 1, 1.2, 1.1, 1, 1, 1, 1.2, 1.2, 1.1, 1.2, 1.3)
mvdf[7, 4:15] <- c(1.1, 1, 1.2, 1.3, 1.3, 1.1, 1.3, 1.4, 1.5, 1.5, 1.4, 1.3)
mvdf[8, 4:15] <- c(1.8, 1.5, 1.9, 1.8, 2, 1.9, 2, 2.2, 2.2, 1.8, 1.8, 2.3)
mvdf[9, 4:15] <- c(1.3, 1, 1.2, 5.1, 6, 6, 4, 2.2, 1.2, 1.3, 1.1, 1.3)
mvdf[10, 4:15] <- c(5, 5.1, 5.2, 5.1, 6, 5, 5, 5.2, 5.2, 5.1, 5.2, 5.3)

GrpList <- unique(mvdf$Group)
exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20)
)
# Prepare data
max_min <- data.frame(
  Biology = c(20, 0), Physics = c(20, 0), Maths = c(20, 0),
  Sport = c(20, 0), English = c(20, 0), Geography = c(20, 0),
  Art = c(20, 0), Programming = c(20, 0), Music = c(20, 0)
)
rownames(max_min) <- c("Max", "Min")

# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
# student1_data <- df[c("Max", "Min", "Student.1"), ]

u<- shinyUI(fluidPage(
  titlePanel("Dominant Merchant Identification"),
  sidebarLayout(position = "left",
                sidebarPanel("Filter Panel",
                             checkboxInput("do2", "Make 2 plots", value = T),
                             selectInput("region", "Selected Region:",
                                         unique(mvdf$Group)
                             ),
                             selectInput("type", "Selected Store-type:",
                                         choices = unique(mvdf$Type),
                                         # c("Type1",
                                         #   "Type2"
                                         # )
                             ),
                             selectInput("store", "Selected Store Name:",
                                         choices = unique(mvdf$Store)
                                         # c("Name1" = "cyl",
                                         #   "Name2" = "am"
                                         # )
                             ),
                             actionButton(inputId = "click",
                                          label = "Update Evaluation")
                ),
                mainPanel(#"Vissualisation panel",
                          fluidRow(
                            verticalLayout(splitLayout(
                              cellWidths = c("50%", "50%"),
                              plotOutput("plotgraph1"),
                              plotOutput("plotgraph2")
                              ),
                              splitLayout(cellWidths = c("50%", "50%"),
                                          plotOutput("plotgraph3"),
                                          plotOutput("plotgraph4")
                              )
                            )
                          )
                )
  )
)
)
s <- shinyServer(function(input, output, session){
  observeEvent(input$region, {
    updateSelectInput(session,
                      "type",
                      choices = unique(mvdf[which(mvdf$Group==input$region),
                                            "Type"])
    )
  })
  observeEvent(input$type, {
    updateSelectInput(session,
                      "store",
                      choices = unique(mvdf[which(mvdf$Type==input$type &
                                                    mvdf$Group == input$region),
                                            "Store"])
    )
  })
  
  student1_data <- df[c("Max", "Min", "Student.1"), ]
  
  set.seed(1234)
  pt3 <- qplot(rnorm(600),fill=I("blue"),binwidth=0.2,main="plotgraph3")
  pt2 <- reactive({
    # input$do2
    if (input$do2){
      return(qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2"))
    } else {
      return(NULL)
    }
  })
  output$plotgraph1 <-  renderPlot(
    radarchart(
      student1_data, axistype = 1,
      pcol = "#00AFBB", pfcol = scales::alpha("#00AFBB", 0.5),
      plwd = 2, plty = 1,
      cglcol = "grey", cglty = 1, cglwd = 0.8,
      axislabcol = "grey", 
      vlcex = 0.9,
      vlabels = colnames(student1_data),
      caxislabels = c(0, 5, 10, 15, 20),
      calcex = 0.01,
      axis = NULL,
      aes = NULL,
      title = NULL
    )
  )
  output$plotgraph2 = renderPlot({pt2()})
  output$plotgraph3 = renderPlot(
    plot(df$Maths)
  )
  output$plotgraph4 = renderPlot({pt3})
})

shinyApp(u,s)