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
library(magrittr)


#### LOAD DATA SOURCE ####
df <- read.csv("https://query.data.world/s/62mtjijsocj6llwcy33he3r6e", header=TRUE, stringsAsFactors=FALSE)

#### DATA CLEANING AND FILTERING HAPPENS HERE #### 
#df <- dplyr::filter(df, nchar(first_name) > 1)

df$received_date <- as.Date(df$received_date)
df$year <- lubridate::year(df$received_date)
df$corporation_name <- ifelse(nchar(df$first_name) == 0, df$last_name, NA)
df$last_name <- ifelse(is.na(df$corporation_name), df$last_name, NA)
# Prepare to display
df2 <- unique(df[,c("candidate_name","last_name","first_name","corporation_name","received_date","year","amount"
                    , "aggregate_amount", "occupation", "city", "state", "id","committee_id", "filed_doc_id")])


colnames(df2) <- c("Candidate Name","Donor Last Name","Donor First Name","Organization","Date of Donation", "Year of Donation"
                   ,"Amount of Donation", "Amount per Filing", "Donor Occupation"
                   , "Donor City", "Donor State", "Record ID","Committee ID", "Filing ID")

df2 <- filter(df2, df2$`Year of Donation` >= 2015)

# ###### Organization Filtering (modified RS)
org = data.frame(df2$Organization)
length(unique(df2$Organization))
LLC = filter(df2,  grepl("LLC", Organization))
NASDAQ_list = read.csv(file = "nasdaq.csv", stringsAsFactors = FALSE)
NYSE_list = read.csv(file = "nyse.csv", stringsAsFactors = FALSE)
nyse_sector_by_name = data.frame(NYSE_list$Name, NYSE_list$industry)

nasdaq_sector_by_name = data.frame(NASDAQ_list$Name, NASDAQ_list$industry)
colnames(nasdaq_sector_by_name) = c("Organization", "Industry")
colnames(org) = c("Organization")
colnames(nyse_sector_by_name) = c("Organization", "Industry")

filter(df2, grepl("Monsanto", Organization))
filter(nyse_sector_by_name, grepl("Monsanto", Organization))
a = join(org, nasdaq_sector_by_name)
b= join(org,nyse_sector_by_name )
unique(a$Industry)
unique(b$Industry)
filter(b, grepl("Agricultural Chemicals", Industry))
filter(a, grepl("Major Pharmaceuticals", Industry))


PAC_list = filter(df2, grepl("Political", Organization))
law_list = filter(df2, grepl("Law", Organization))

###Filter for NASDAQ and NYSE 11+27
industry = vector(length = nrow(org))
listing = vector(length = nrow(org))
corp_idenfitied = 0
corporations_identified = vector(length = nrow(org))
 for(org_index in seq(1:nrow(org))){
   ## Check if NASDAQ
   if(is.na(org$Organization[org_index]) == FALSE){
   matching_firms = filter(nasdaq_sector_by_name, grepl(org$Organization[org_index], Organization))
   
   matching_industries =  select(matching_firms,Industry)
   if(nrow(matching_industries) >0 ){
   corp_idenfitied = corp_idenfitied + 1
   firm_name = matching_firms$Organization
   corporations_identified[org_index] = as.String(firm_name[1])
   industry[org_index]  = matching_industries[1]
   listing[org_index] = "NASDAQ"
   
   }else{
     #Check NYSE
     matching_firms = filter(nyse_sector_by_name, grepl(org$Organization[org_index], Organization))
     
     matching_industries =  select(matching_firms,Industry)
     if(nrow(matching_industries) >0 ){
       corp_idenfitied = corp_idenfitied + 1
       industry[org_index]  = matching_industries[1]
       firm_name = matching_firms$Organization
       corporations_identified[org_index] = as.String(firm_name[1])
       listing[org_index] = "NYSE"
       
     }else{
       #CheckPAC
       if(as.character(org$Organization[org_index]) %in% PAC_list$Organization) {
      
         corp_idenfitied = corp_idenfitied + 1
         industry[org_index]  = "Political"
         firm_name = org$Organization[org_index]
         corporations_identified[org_index] = as.String(firm_name[1])
         listing[org_index] = "PAC"
         
       }else{
         ## Check for law offices
         if(as.character(org$Organization[org_index]) %in% law_list$Organization) {
           
           corp_idenfitied = corp_idenfitied + 1
           industry[org_index]  = "Law"
           firm_name = org$Organization[org_index]
           corporations_identified[org_index] = as.String(firm_name[1])
           listing[org_index] = "LAW"
           
         }else{
           industry[org_index]  = NA
           listing[org_index] = "Unknown"
         }
       }
     }
   }
   
   }else{
     industry[org_index] = NA
     listing[org_index] = "Person"
   }
 }

industry_data = data.frame(t(t(industry)), 
                           t(t(listing)), 
                           t(t(corporations_identified)))
colnames(industry_data) = c("industry", "listing", "corporations_identified")
save(industry_data, file = "industry_classifications.csv")


# 
# ######
# server <- function(input, output) {
#   #the server - literally what data is going into the plot/viz?
#  output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
#                                                     {
#                                                       data <- df2
#                                                       
#                                                       output$barplot <- renderPlotly({
#                                                         
#                                                         data2 <- data_filter() %>%
#                                                           group_by(`Candidate Name`) %>%
#                                                           summarize(
#                                                             donations = n()
#                                                             , amount = sum(`Amount of Donation`)
#                                                           )
#                                                         
#                                                         plot1 <- ggplot(data2, aes(x=`Candidate Name`, y=`amount`
#                                                                                          , fill=factor(`amount`)
#                                                                                          , label=`amount`))+
#                                                         theme_bw()+
#                                                         theme(axis.text.x = element_text(angle = 45, vjust=.5),
#                                                               legend.position = "none")+
#                                                         geom_bar(stat="identity")+
#                                                         labs(title = "Donations to Candidates", x="Candidate Name", y="Amount Given")
#                                                       
#             
#                                                       #plot1
#                                                       gp <- ggplotly(plot1, width = 1000, height = 600)
#                                                       # #gp
#                                                       gp %>% layout(margin = list(l=90, r=60, t=60, b=90))
#                                                       
#                                                       })
#                                                       
#                                                       data_filter <- reactive({
#                                                         if(input$Candidate != "All"){
#                                                           data <- dplyr::filter(data, `Candidate Name` == input$Candidate)
#                                                         }
#                                                         
#                                                         if(input$Date != "All"){
#                                                         data <- dplyr::filter(data, `Date of Donation` == input$Date)
#                                                       }
#                                                         
#                                                       if(input$Year != "All"){
#                                                           data <- dplyr::filter(data, `Year of Donation` == input$Year)
#                                                       }
#                                                       
#                                                       if(input$Corporation != "All"){
#                                                         data <- dplyr::filter(data, `Organization` %in% input$Organization)
#                                                       }
#                                                         
#                                                       if(input$Surname != "All"){
#                                                         data <- dplyr::filter(data, `Donor Last Name` %in% input$Surname)
#                                                       }
#                                                       
#                                                       if(input$City != "All"){
#                                                         data <- dplyr::filter(data, `Donor City` == input$City)
#                                                       }
#                                                       
#                                                       if(input$State != "All"){
#                                                         data <- dplyr::filter(data, `Donor State` == input$State)
#                                                       }
#                                                       data
#                                                       })
#                                                       
#                                                       data_filter()
#                                                       }))
#   
#   output$intro <- renderText("Welcome! To start exploring the data, click over to the plot or table tabs.
#                              Quick Reference:
#                              Corporation indicates that no individual is listed for that donation, but instead a corporate entity gave the funds."
#                              )
# }
# 
# 
# ui <- fluidPage(
#   theme = shinytheme('lumen'),
#   titlePanel("Donations to Candidates for Illinois Governor 2018"),
#   
#   "This is the home of the Illinois governor's race financial data visualizations project. 
#   Find us on ", a("GitHub", 
#                   href= "https://github.com/skirmer/ILGov2018"), " or ",  
#   a("data.world", 
#     href="https://data.world/lilianhj/ilgov-2018"), " if you want to help out!",  br(),br(),
#     
#   fluidRow()
#     
#     column(2,
#            selectizeInput("Candidate",
#                           "Candidate Name:",
#                           c(Choose = '', "All", sort(trimws(unique(as.character(df2$`Candidate Name`)))))
#                           , multiple = TRUE, selected = "All"))
#     ,column(2,
#            selectizeInput("Date",
#                           "Date of Donation:",
#                           c("All", sort(trimws(unique(as.character(df2$`Date of Donation`)))))))
#     , column(2,
#              selectizeInput("Year",
#                             "Year of Donation:",
#                             c("All", sort(trimws(unique(as.character(df2$`Year of Donation`)))))))
#     
#     , column(3,
#              selectizeInput("Organization",
#                             "Organization:",
#                             c(Choose = '', "All", sort(trimws(unique(as.character(df2$`Organization`)))))
#                             , multiple = TRUE, selected = "All"))
# 
#     , column(3,
#              selectizeInput("Surname",
#                             "Donor Last Name:",
#                             c(Choose = '', "All", sort(trimws(unique(as.character(df2$`Donor Last Name`)))))
#                             , multiple = TRUE, selected = "All"))
#      , column(2,
#              selectizeInput("City",
#                             "Donor City:",
#                             c("All", sort(trimws(unique(as.character(df2$`Donor City`)))))))
#     , column(2,
#              selectizeInput("State",
#                             "Donor State:",
#                             c("All", sort(trimws(unique(as.character(df2$`Donor State`)))))))
#   )
#   
#   ,tabsetPanel(
#         tabPanel("Summary", HTML("<BR/>
#                                   Welcome! To start exploring the data, click over to the table or plot tabs, and select filters to change the data presented. <BR/>
#                                   <BR/>
#                                   Reference Notes: <BR/>
#                                   <ul>
#                                   <li>Organization indicates that a corporation, union, or other entity gave the funds, but no individual's name was associated.
#                                  </li>
#                                 <li> For a full data dictionary, please visit <a href=https://data.world/lilianhj/ilgov-2018/workspace/data-dictionary>our data.world page</a>.
#                                  </ul>
# <BR/>
# Candidate Platforms:
# <ul>
# <li> Bruce Rauner (incumbent): <a href=https://www.brucerauner.com/issues/>https://www.brucerauner.com/issues/</a></li>
# <li> JB Pritzker: <a href=https://www.jbpritzker.com/vision/>https://www.jbpritzker.com/vision/</a></li>
# <li> Ameya Pawar: <a href=https://www.pawar2018.com/issues/>https://www.pawar2018.com/issues/</a></li>
# <li> Daniel Biss: <a href=https://www.danielbiss.com/the-issues/>https://www.danielbiss.com/the-issues/</a></li>
# <li> Robert Daiber: <a href=http://bobdaiber.com/issues#Issues>http://bobdaiber.com/issues#Issues</a> </li>
# <li> Scott Drury: <a href=https://drive.google.com/file/d/0B2iD02dMsK-FUXZxM1FTMmJ1bkhpYTM4OFNQSlM3VmR0Y19v/view>https://drive.google.com/file/d/0B2iD02dMsK-FUXZxM1FTMmJ1bkhpYTM4OFNQSlM3VmR0Y19v/view</a> </li>
# <li> Chris Kennedy: <a href=https://kennedyforillinois.com/about/>https://kennedyforillinois.com/about/</a></li>
# </ul>
# <BR/>
# <BR/>
# 
#                                  Please note, this website is a continuous work in progress and we love feedback and ideas. Please visit us at the data.world and github sites linked at the top of this page to help!")), 
#         tabPanel("Table", DT::dataTableOutput("table")),
#         tabPanel("Plot", plotlyOutput("barplot"))
# ))
#   
# 
# shinyApp(ui = ui, server = server)
# 
# #### WHEN IT'S READY... ####
# 
#  shiny::runApp()