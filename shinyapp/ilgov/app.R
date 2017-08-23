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
library(tm)
library(lubridate)


#### LOAD DATA SOURCE ####
df <- read.csv("https://query.data.world/s/62mtjijsocj6llwcy33he3r6e", header=TRUE, stringsAsFactors=FALSE)

#### DATA CLEANING AND FILTERING HAPPENS HERE #### 
#df <- dplyr::filter(df, nchar(first_name) > 1)

df$received_date <- as.Date(df$received_date)
df$year <- lubridate::year(df$received_date)

# Prepare to display
df2 <- unique(df[,c("candidate_name","last_name","first_name","received_date","year","amount"
                    , "aggregate_amount", "occupation", "employer", "city", "state", "id","committee_id", "filed_doc_id")])


colnames(df2) <- c("Candidate Name","Donor Last Name","Donor First Name","Date of Donation", "Year of Donation"
                   ,"Amount of Donation", "Amount per Filing", "Donor Occupation", "Donor Employer"
                   , "Donor City", "Donor State", "Record ID","Committee ID", "Filing ID")

df2 <- filter(df2, df2$`Year of Donation` >= 2015)

server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
 output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
                                                    {
                                                      data <- df2
                                                      
                                                      output$barplot <- renderPlotly({
                                                        
                                                        data2 <- data_filter() %>%
                                                          group_by(`Candidate Name`) %>%
                                                          summarize(
                                                            donations = n()
                                                            , amount = sum(`Amount of Donation`)
                                                          )
                                                        
                                                        plot1 <- ggplot(data2, aes(x=`Candidate Name`, y=`amount`
                                                                                         , fill=factor(`amount`)
                                                                                         , label=`amount`))+
                                                        theme_bw()+
                                                        theme(axis.text.x = element_text(angle = 45, vjust=.5),
                                                              legend.position = "none")+
                                                        geom_bar(stat="identity")+
                                                        labs(title = "Donations to Candidates", x="Candidate Name", y="Amount Given")
                                                      
            
                                                      #plot1
                                                      gp <- ggplotly(plot1, width = 1000, height = 600)
                                                      # #gp
                                                      gp %>% layout(margin = list(l=90, r=60, t=60, b=90))
                                                      
                                                      })
                                                      
                                                      data_filter <- reactive({
                                                        if(input$Candidate != "All"){
                                                          data <- dplyr::filter(data, `Candidate Name` == input$Candidate)
                                                        }
                                                        
                                                        if(input$Date != "All"){
                                                        data <- dplyr::filter(data, `Date of Donation` == input$Date)
                                                      }
                                                        
                                                      if(input$Year != "All"){
                                                          data <- dplyr::filter(data, `Year of Donation` == input$Year)
                                                      }
                                                      
                                                      if(input$Employer != "All"){
                                                        data <- dplyr::filter(data, `Donor Employer` %in% input$Employer)
                                                      }
                                                        
                                                      if(input$Surname != "All"){
                                                        data <- dplyr::filter(data, `Donor Last Name` %in% input$Surname)
                                                      }
                                                      
                                                      if(input$City != "All"){
                                                        data <- dplyr::filter(data, `Donor City` == input$City)
                                                      }
                                                      
                                                      if(input$State != "All"){
                                                        data <- dplyr::filter(data, `Donor State` == input$State)
                                                      }
                                                      data
                                                      })
                                                      
                                                      data_filter()
                                                      }))
  
  output$intro <- renderText("Welcome! To start exploring the data, click over to the plot or table tabs.
                             Quick Reference:
                             Donor employer does not necessarily indicate that the company or 
                             employing organization endorses or otherwise supports the donation or candidate."
                             )
}


ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("Donations to Candidates for Illinois Governor 2018"),
  
  "This is the home of the Illinois governor's race financial data visualizations project. 
  Find us on ", a("GitHub", 
                  href= "https://github.com/skirmer/ILGov2018"), " or ",  
  a("data.world", 
    href="https://data.world/lilianhj/ilgov-2018"), " if you want to help out!",  br(),br(),
    
  fluidRow(
    
    column(2,
           selectizeInput("Candidate",
                          "Candidate Name:",
                          c(Choose = '', "All", sort(trimws(unique(as.character(df2$`Candidate Name`)))))
                          , multiple = TRUE, selected = "All"))
    ,column(2,
           selectizeInput("Date",
                          "Date of Donation:",
                          c("All", sort(trimws(unique(as.character(df2$`Date of Donation`)))))))
    , column(2,
             selectizeInput("Year",
                            "Year of Donation:",
                            c("All", sort(trimws(unique(as.character(df2$`Year of Donation`)))))))
    
    , column(3,
             selectizeInput("Employer",
                            "Donor Employer:",
                            c(Choose = '', "All", sort(trimws(unique(as.character(df2$`Donor Employer`)))))
                            , multiple = TRUE, selected = "All"))

    , column(3,
             selectizeInput("Surname",
                            "Donor Last Name:",
                            c(Choose = '', "All", sort(trimws(unique(as.character(df2$`Donor Last Name`)))))
                            , multiple = TRUE, selected = "All"))
     , column(2,
             selectizeInput("City",
                            "Donor City:",
                            c("All", sort(trimws(unique(as.character(df2$`Donor City`)))))))
    , column(2,
             selectizeInput("State",
                            "Donor State:",
                            c("All", sort(trimws(unique(as.character(df2$`Donor State`)))))))
  )
  
  ,tabsetPanel(
        tabPanel("Summary", HTML("<BR/>
                                  Welcome! To start exploring the data, click over to the table or plot tabs, and select filters to change the data presented. <BR/>
                                  <BR/>
                                  Reference Notes: <BR/>
                                  <ul>
                                  <li>Donor employer does not necessarily indicate that the company or employing 
                                 organization endorses or otherwise supports the donation or candidate.
                                 </li>
                                <li> For a full data dictionary, please visit <a href=https://data.world/lilianhj/ilgov-2018/workspace/data-dictionary>our data.world page</a>.
                                 </ul>
                                 Please note, this website is a continuous work in progress and we love feedback and ideas. Please visit us at the data.world and github sites linked at the top of this page to help!")), 
        tabPanel("Table", DT::dataTableOutput("table")),
        tabPanel("Plot", plotlyOutput("barplot"))
))
  

shinyApp(ui = ui, server = server)

#### WHEN IT'S READY... ####

# shiny::runApp()