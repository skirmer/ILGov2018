rm(list = ls())

library(httr)
library(data.table)
library(jsonlite)

# Read in candidate information from JSON file
candidatesList <- jsonlite::read_json('candidates.json')

# Function to generate Illinois Sunshine API query based on candidateId
API_ENDPOINT <- "http://www.illinoissunshine.org/api/receipts/"
PAGE_SIZE <- 1000
QUERY_LIMIT <- 100000
GenerateQuery <- function(candidateId, offset) {
    queryURL <- paste0(
        API_ENDPOINT
        , '?'
        , 'committee_id='
        , candidateId
        , '&datatype=csv'
        , '&limit='
        , as.character(PAGE_SIZE)
        , '&offset='
        , as.character(offset)
    )
    return(queryURL)
}

# Initialize list to collect DTs
allCandidateDTList <- list()

# Loop over all candidates, make request, create data.table, and add to list
for (candInd in seq_along(candidatesList)) {
    
    candidateRecord <- candidatesList[[candInd]]
    
    # Generate Query URL and make HTTP GET request
    response <- httr::GET(GenerateQuery(candidateRecord[['id']], offset = 0))
    responseContent <- httr::content(response, "text")
    
    # If response is non-empty, save to a data.table and query again
    thisCandidateDTList <- list()
    offset <- 0
    while (nchar(responseContent) > 0) {
        resultDT <- data.table::fread(responseContent)
        
        # Add column for candidate name
        resultDT[, candidate_name := candidateRecord[["candidate_name"]]]
        
        # Add page to this candidate's list
        thisCandidateDTList[[1 + offset/PAGE_SIZE]] <- resultDT
        
        # Increment offset by page size and requery
        offset <- offset + PAGE_SIZE
        response <- httr::GET(GenerateQuery(candidateRecord[['id']], offset = offset))
        responseContent <- httr::content(response, "text")
        
        # If loop is not ending, break it
        if (offset > QUERY_LIMIT) {
            print(paste("Hit", QUERY_LIMIT, "limit."))
            break
        }
    }
    
    # If we got some non-empty response, combine pages and add to all candidates list
    if (length(thisCandidateDTList) > 0) {
        
        thisCandidateDT <- data.table::rbindlist(thisCandidateDTList)
        
        # Log statement
        print(paste(
            "Queried"
            , as.character(nrow(thisCandidateDT))
            , "total rows for candidate"
            , candidateRecord[["candidate_name"]]
        ))
        
        allCandidateDTList[[candInd]] <- thisCandidateDT
    } else {
        # Log statement
        print(paste(
            "Queried"
            , 0
            , "total rows for candidate"
            , candidateRecord[["candidate_name"]]
        ))
    }
    
} 

# Rowbind all data.tables into one combined data.table
combinedDT <- data.table::rbindlist(allCandidateDTList)

# Log statement
print(paste("Queried", nrow(combinedDT), "total rows for all candidates."))

# Write to .csv
write.table(x = combinedDT
            , file = "all_candidates_receipts.csv"
            , quote = TRUE
            , sep = ","
            , row.names = FALSE
            , col.names = TRUE
)