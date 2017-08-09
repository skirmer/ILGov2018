rm(list = ls())

library(httr)
library(data.table)
library(jsonlite)

# Read in candidate information from JSON file
candidatesList <- jsonlite::read_json('candidates.json')

# Function to generate Illinois Sunshine API query based on candidateId
API_ENDPOINT <- "http://www.illinoissunshine.org/api/receipts/"
GenerateQuery <- function(candidateId) {
    queryURL <- paste0(
        API_ENDPOINT
        , '?'
        , 'committee_id='
        , candidateId
        , '&datatype=csv'
    )
    return(queryURL)
}

# Initialize list to collect DTs
candidateDTList <- list()

# Loop over all candidates, make request, create data.table, and add to list
for (ind in seq_along(candidatesList)) {
    
    candidateRecord <- candidatesList[[ind]]
    
    # Generate Query URL and make HTTP GET request
    response <- httr::GET(GenerateQuery(candidateRecord[['id']]))
    responseContent <- httr::content(response, "text")
    
    # Pass response to data.table if non-empty
    if (nchar(responseContent) > 0) {
        candidateDT <- data.table::fread(responseContent)
        
        # Log statement
        print(paste(
            "Queried"
            , as.character(nrow(candidateDT))
            , "for candidate"
            , candidateRecord[["candidate_name"]]
        ))
        
        # Add column for candidate name
        candidateDT[, candidate_name := candidateRecord[["candidate_name"]]]
        
        # Add DT to list
        candidateDTList[[ind]] <- candidateDT
    }
    
} 

# Rowbind all data.tables into one combined data.table
combinedDT <- data.table::rbindlist(candidateDTList)

# Write to .csv
write.table(x = combinedDT
            , file = "all_candidates_receipts.csv"
            , quote = TRUE
            , sep = ","
            , row.names = FALSE
            , col.names = TRUE
)