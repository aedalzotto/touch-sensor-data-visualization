#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

touch <- read.csv2("~/touch_events.csv", header=T, sep = ",")
colnames(touch)[3] <- "type"
colnames(touch)[1] <- "date"
swipe_left  <- filter(touch, type == 'b')
swipe_right <- filter(touch, type == 'r')
double_tap  <- filter(touch, type == 'k')
time_idx = seq(0,990, 10)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Eventos de um Sensor Touch"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("type",
                        "Tipo de evento:",
                        choices = c("Deslizar para a esquerda",
                                    "Deslizar para a direita",
                                    "Toque duplo"),
                        selected = "Deslizar para a esquerda"
            ),
            conditionalPanel(condition ='input.type == "Deslizar para a esquerda"',
                             selectInput("eventL",
                                         "Evento:",
                                         choices = swipe_left$date,
                                         selected = swipe_left$date[1])
                             
            ),
            conditionalPanel(condition ='input.type == "Deslizar para a direita"',
                             selectInput("eventR",
                                         "Evento:",
                                         choices = swipe_right$date,
                                         selected = swipe_right$date[1])
                             
            ),
            conditionalPanel(condition ='input.type == "Toque duplo"',
                             selectInput("eventT",
                                         "Evento:",
                                         choices = double_tap$date,
                                         selected = double_tap$date[1])
                             
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Sensor 1",
                         plotOutput("sensor1")
                ),
                tabPanel("Sensor 2",
                         plotOutput("sensor2")
                ),
                tabPanel("Sensor 3",
                         plotOutput("sensor3")
                ),
                tabPanel("Sensor 4",
                         plotOutput("sensor4")
                ),
                tabPanel("Sensor 5",
                         plotOutput("sensor5")
                ),
                tabPanel("Sensor 6",
                         plotOutput("sensor6")
                ),
                tabPanel("Sensor 7",
                         plotOutput("sensor7")
                ),
                tabPanel("Sensor 8",
                         plotOutput("sensor8")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    data_sensors <- reactive({
        if(input$type == "Deslizar para a esquerda"){
            events <- filter(touch, date == input$eventL)       
        } else if(input$type == "Deslizar para a direita"){
            events <- filter(touch, date == input$eventR)
        } else {
            events <- filter(touch, date == input$eventT)
        }

        data <- events$event
        
        # Remove "[" da string
        data_str <- gsub("[[]", "", as.character(data))
        # Remove espaço em branco da string
        data_str <- gsub("[ ]", "", data_str)
        
        # Separa pelo "]", cada linha é o instante de tempo, cada coluna é um sensor
        data_split <- unlist(strsplit(data_str, "]"))[-101]
        # Remove primeiro caractere (vírgula) de todas linhas menos da primeira
        data_split[2:100] <- substring(data_split[-1],2)
        
        # Monta a matriz
        data_mat <- unlist(strsplit(data_split, ","))
        data_sensors <- matrix(data_mat, nrow=8)
    })
    
    output$sensor1 <- renderPlot({
        plot(time_idx, data_sensors()[1,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })
    output$sensor2 <- renderPlot({
        plot(time_idx, data_sensors()[2,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  
    output$sensor3 <- renderPlot({
        plot(time_idx, data_sensors()[3,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  
    output$sensor4 <- renderPlot({
        plot(time_idx, data_sensors()[4,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  
    output$sensor5 <- renderPlot({
        plot(time_idx, data_sensors()[5,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  
    output$sensor6 <- renderPlot({
        plot(time_idx, data_sensors()[6,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  
    output$sensor7 <- renderPlot({
        plot(time_idx, data_sensors()[7,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  
    output$sensor8 <- renderPlot({
        plot(time_idx, data_sensors()[8,], type="l", xlab="Tempo (ms)", ylab="Valor")
    })  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
