# Chi Hack Night Governor Campaign Data Project #
# S. Kirmer #
# July 2017 #

# Set up location
 setwd("ILGov2018/")
# Put in the token data from shinyapps.io profile
 setAccountInfo()

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinythemes)
library(rsconnect)
library(ggvis)
library(plotly)

df <- read.csv("https://query.data.world/s/ebgm9pltjmv22p1w9lmkdx46e",header=T, stringsAsFactors=FALSE)
df <- dplyr::filter(df, !is.na(recipient_surname))
df$LOBBYIST_NAME <- paste0(df$LOBBYIST_LAST_NAME, ", ", df$LOBBYIST_FIRST_NAME)

# Clean lobbying company names #
#Save the old name
df$EMP_NAME_ORIG <- df$EMPLOYER_NAME

# text pre-processing of EMPLOYER_NAME -- these correct for common entry mistakes
df$EMPLOYER_NAME <- trimws(df$EMPLOYER_NAME, which = "both") #Might want to run this at repeated points
df$EMPLOYER_NAME <- toupper(df$EMPLOYER_NAME) #make all characters upper case
df$EMPLOYER_NAME <- gsub(".COM$", "", df$EMPLOYER_NAME) # drop .com if it appears at the end
df$EMPLOYER_NAME <- removePunctuation(df$EMPLOYER_NAME)
#Fix a spelling screwup or two
df$EMPLOYER_NAME <- gsub("PC$", "", df$EMPLOYER_NAME) # drop PC if it appears at the end
df$EMPLOYER_NAME <- gsub("LTD$", "", df$EMPLOYER_NAME) # drop LTD if it appears at the end
df$EMPLOYER_NAME <- gsub(" HACIA", "", df$EMPLOYER_NAME) 
df$EMPLOYER_NAME <- gsub(" ASSOC$", " ASSOCIATION", df$EMPLOYER_NAME) 
df$EMPLOYER_NAME <- gsub(" BUNEY", " BURNEY", df$EMPLOYER_NAME) 

df$EMPLOYER_NAME <- gsub("AND ITS AFFILIATES|AN ILLINOIS CORPORATION| LLC| INC| LLP| CORPORATE| CORPORATION| CORP", "", df$EMPLOYER_NAME)

df$EMPLOYER_NAME <- trimws(df$EMPLOYER_NAME, which = "both") #Might want to run this at repeated points
# Remove multiple spaces
df$EMPLOYER_NAME <- gsub("  ", " ", df$EMPLOYER_NAME)

# Prepare to display
df2 <- unique(df[,c("Year","LOBBYIST_NAME", "CONTRIBUTION_DATE","RECIPIENT","recipient_surname","AMOUNT", "EMPLOYER_NAME")])
colnames(df2) <- c("Year", "Lobbyist", "Contribution Date", "Receiving Organization","Alderman", "Amount", "Lobbying Firm")






server <- function(input, output) {
  #the server - literally what data is going into the plot/viz?
  
  output$table <- DT::renderDataTable(DT::datatable(rownames = FALSE,
                                                    {
                                                      data <- df2
                                                      
                                                      output$barplot <- renderPlotly({
                                                        data2 <- summarize(group_by(data, Alderman, Year),
                                                                           aggregate = sum(Amount, na.rm=T),
                                                                           annual_amt = sum(Amount, na.rm=T))
                                                        
                                                        # Render a barplot
                                                        plot1 <- ggplot(data2, aes(x=Alderman, y=aggregate, fill=factor(Year), label=annual_amt))+
                                                          theme_bw()+
                                                          theme(axis.text.x = element_text(angle = 45, vjust=.5),
                                                                legend.position = "none")+
                                                          geom_bar(stat="identity")+
                                                          labs(title = "Donations to Aldermen", x="Alderman", y="Amount of Donations") 
                                                        
                                                        
                                                        #plot1
                                                        gp <- ggplotly(plot1, tooltip = c("Alderman","factor(Year)", "annual_amt"))
                                                        #gp
                                                        gp %>% layout(margin = list(l=90, r=60, t=60, b=90))
                                                        
                                                      })
                                                      
                                                      if(input$Industry != "All"){
                                                        data <- dplyr::filter(data, `Lobbying Firm` == input$Industry)
                                                      }
                                                      
                                                      if(input$Alderman != "All") {
                                                        data <- dplyr::filter(data, Alderman == input$Alderman) 
                                                      }
                                                      
                                                      if(input$Year != "All") {
                                                        data <- dplyr::filter(data, Year == input$Year) 
                                                      }
                                                      
                                                      if(input$Funder != "All") {
                                                        data <- dplyr::filter(data, Lobbyist == input$Funder) 
                                                      }
                                                      
                                                      data}))
}




ui <- fluidPage(
  theme = shinytheme('lumen'),
  titlePanel("Exchange Between Aldermen and Lobbyists"),
  
  "This page is part of the Data4Democracy Chicago Lobbying project, and is a collaborative work in progress. 
  All data comes from the City of Chicago Data Portal. Begin typing in the Lobbying Firm box to browse the companies- you can select more than one.",
  br(),br(),
  
  "If you'd like to learn more, join us at ", a("data.world/lilianhj/chicago-lobbyists!", 
                                                href="https://data.world/lilianhj/chicago-lobbyists"),
  "If you like this app, you might also enjoy the sister apps ", a("Aldermanic Voting Records",
                                                                   href="https://skirmer.shinyapps.io/chivotes/"),
  "and ", a("Lobbyist Connections Network.",
            href="https://nathanielwroblewski.github.io/data-for-democracy-2017/"),
  br(),br(),
  "Code for this app is available on ", a("GitHub.", 
                                          href= "https://github.com/skirmer/shinyapp/tree/master/chilobby"), br(),br(),
  
  fluidRow(
    
    column(3,
           selectizeInput("Industry",
                          "Lobbying Firm:",
                          c(Choose='', "All", sort(trimws(unique(as.character(df2$`Lobbying Firm`))))), 
                          multiple=TRUE, selected = "All")),
    column(3,
           selectInput("Funder",
                       "Lobbyist:",
                       c("All",  sort(trimws(unique(as.character(df2$Lobbyist))))))),
    column(3,
           selectInput("Alderman",
                       "Alderman:",
                       c("All", sort(trimws(unique(as.character(df2$Alderman))))))),
    column(2,
           selectInput("Year",
                       "Year:",
                       c("All", sort(trimws(unique(as.character(df2$Year)))))))
  )
  ,
  fluidRow(
    plotlyOutput("barplot", height = 500)  
  )
  ,
  fluidRow(
    DT::dataTableOutput("table")
  )
  
)

shinyApp(ui = ui, server = server)