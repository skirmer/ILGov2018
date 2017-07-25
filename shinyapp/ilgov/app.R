# Chi Hack Night Governor Campaign Data Project #
# S. Kirmer #
# July 2017 #

# Set up location
# setwd("ILGov2018/shinyapp/ilgov/")
# Put in the token data from shinyapps.io profile
#rsconnect::setAccountInfo()

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(rsconnect)
library(ggvis)
library(plotly)

#### LOAD DATA SOURCE ####
#df <- read.csv("https://query.data.world/s/ebgm9pltjmv22p1w9lmkdx46e",header=T, stringsAsFactors=FALSE)

#### DATA CLEANING AND FILTERING HAPPENS HERE #### 



#### COLUMN NAMES NEED TO BE READY TO DISPLAY ####
# df2 <- unique(df[,c("Year","LOBBYIST_NAME", "CONTRIBUTION_DATE","RECIPIENT","recipient_surname","AMOUNT", "EMPLOYER_NAME")])
# colnames(df2) <- c("Year", "Lobbyist", "Contribution Date", "Receiving Organization","Alderman", "Amount", "Lobbying Firm")




server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
  # 
  # output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
  #                                                   {
  #                                                     data <- df2
  #                                                     
  #                                                     output$barplot <- renderPlotly({
  #                                                       data2 <- summarize(group_by(data, Alderman, Year),
  #                                                                          aggregate = sum(Amount, na.rm=T),
  #                                                                          annual_amt = sum(Amount, na.rm=T))
  #                                                       
  #                                                       # Render a barplot
  #                                                       plot1 <- ggplot(data2, aes(x=Alderman, y=aggregate, fill=factor(Year), label=annual_amt))+
  #                                                         theme_bw()+
  #                                                         theme(axis.text.x = element_text(angle = 45, vjust=.5),
  #                                                               legend.position = "none")+
  #                                                         geom_bar(stat="identity")+
  #                                                         labs(title = "Donations to Aldermen", x="Alderman", y="Amount of Donations") 
  #                                                       
  #                                                       
  #                                                       #plot1
  #                                                       gp <- ggplotly(plot1, tooltip = c("Alderman","factor(Year)", "annual_amt"))
  #                                                       #gp
  #                                                       gp %>% layout(margin = list(l=90, r=60, t=60, b=90))
  #                                                       
  #                                                     })
  #                                                     
  #                                                     if(input$Industry != "All"){
  #                                                       data <- dplyr::filter(data, `Lobbying Firm` == input$Industry)
  #                                                     }
  #                                                     
  #                                                     if(input$Alderman != "All") {
  #                                                       data <- dplyr::filter(data, Alderman == input$Alderman) 
  #                                                     }
  #                                                     
  #                                                     if(input$Year != "All") {
  #                                                       data <- dplyr::filter(data, Year == input$Year) 
  #                                                     }
  #                                                     
  #                                                     if(input$Funder != "All") {
  #                                                       data <- dplyr::filter(data, Lobbyist == input$Funder) 
  #                                                     }
  #                                                     
  #                                                     data}))
}




ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("PLACEHOLDER"),
  
 "This is the future home of the Illinois governor's race financial data visualizations project. 
 Find us on ", a("GitHub", 
                href= "https://github.com/skirmer/ILGov2018"), " or ",  
 a("data.world", 
href="https://data.world/lilianhj/chicago-lobbyists"), " if you want to help out!",  br(),br())
  
  # fluidRow(
  #   
  #   column(3,
  #          selectizeInput("Industry",
  #                         "Lobbying Firm:",
  #                         c(Choose='', "All", sort(trimws(unique(as.character(df2$`Lobbying Firm`))))), 
  #                         multiple=TRUE, selected = "All")),
  #   column(3,
  #          selectInput("Funder",
  #                      "Lobbyist:",
  #                      c("All",  sort(trimws(unique(as.character(df2$Lobbyist))))))),
  #   column(3,
  #          selectInput("Alderman",
  #                      "Alderman:",
  #                      c("All", sort(trimws(unique(as.character(df2$Alderman))))))),
  #   column(2,
  #          selectInput("Year",
  #                      "Year:",
  #                      c("All", sort(trimws(unique(as.character(df2$Year)))))))
  # )
  # ,
  # fluidRow(
  #   plotlyOutput("barplot", height = 500)  
  # )
  # ,
  # fluidRow(
  #   DT::dataTableOutput("table")
  # )
  

shinyApp(ui = ui, server = server)

#### WHEN IT'S READY... ####

# shiny::runApp()