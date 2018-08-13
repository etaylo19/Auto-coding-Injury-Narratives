# This open source product is licensed under the MIT license. August 13, 2018
# For more information, use this link:  https://github.com/etaylo19/Auto-coding-Injury-Narratives/blob/master/LICENSE
#
#
# June 20, 2018
# Edward L. Taylor -- The University of Tennessee, Knoxville
# This shinydashboard application:
#  accepts injury text narratives as user input,
#  assigns a two-digit OIICS event code to each narrative,
#  allows the user to download a csv file of the results, 
#  and provides a summary report of the results.
# Rev 2; added googlesheets and download of betas & WordsToRemove in server
# June 7; added module to Perform Autocoding'
# Rev 4; I am going to do some wild experimentation to perfect autocoding display
# Rev 5; More experimentation with downloading module; wrap all previous code with reactive 
# Rev 6; More reactivity and remove download tab
# Rev 7; Do autocoding outside of reactivity function
# Rev 8; Delete Report tab





# load the needed packages

library(shinydashboard)
library(shiny)
library(googlesheets)
library(tm)
library(SnowballC)

# retrieve a file using googlesheets package; the user will be blind to this action
# 'betas' is the registered name of the sheet in googlesheets
betas <- gs_url("https://docs.google.com/spreadsheets/d/1KYquB9_41RVVmM4VkTEBl0zEwPlw-hzdVsquXyoflfM/")
betas_data <- gs_read(betas)    # note the name of the file is 'betas_data'; change in the narratives code

WordsToRemove <- gs_url("https://docs.google.com/spreadsheets/d/1EsUnEqyz8w3Ap-TXHu6aMx0wsfsdvFDkgFjdZ4599CU/")
words_data <- WordsToRemove



## app.R ##

ui <- dashboardPage(
    dashboardHeader(title = "OIICS Event Codes"),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Getting Started", tabName = "introduction", icon = icon("info")),
        menuItem("Upload Narratives", tabName = "upload", icon = icon("upload")),
        menuItem("Perform Autocoding", tabName = "autocode", icon = icon("spinner")),
        menuItem("Download Results", tabName = "download", icon = icon("download"))
        # menuItem("Get a Report", tabName = "report", icon = icon("file"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "introduction",
                    h2("The University of Tennessee, Knoxville"),
                    h2("OIICS Autocoding Dashboard"),
                    p("This dashboard allows the user to autocode injury narratives using a remote web server. No software
                        installation is required."),
                    p("The coding is based upon the Occupational Injury and Illness
                        Classification System (OIICS) v 2.01 Event Codes."),
                    h2(""),
                    
                    tags$a(href = "https://wwwn.cdc.gov/wisards/oiics/Trees/MultiTree.aspx?TreeType=Event",
                           "For an overview of the OIICS Event Code Tree web page, click this link."),                   
                    h2(""),
                    
                    p("The coding is done by logistic regression and its accuracy of the coding depends on several factors."), 
                    tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4915551/",
                           "For more explanation on the coding methodology by Bertke et al., click this link."),
                    h2(""),
                    
                    p(""),
                    p("Use the tabs at left for the following process:"),
                    tags$ul(
                        tags$li("'Upload Narratives' - Upload a file containing injury narratives to be coded"),
                        tags$li("'Perform Autocoding' - Assign each a two-digit OIICS Event Code and a score"),
                        tags$li("'Download Results' - Download the results in a csv file")
                    ),

                    h3("To begin, select the 'Upload Narratives' tab.")
            ),
            
            # Second tab content "Upload Narratives"
            tabItem(tabName = "upload",
                    
                    h3("Step 1. Upload an injury narrative text file from your computer."),
                    p("Follow these instructions to upload a file."),
                    tags$ol(type = 'a',
                            tags$li("Check the header box if your file contains headers (a header row is optional)."),
                            tags$li("Choose the separator (i.e. comma, semicolon, or tab) used as a delimiter within your file."),
                            tags$li("Choose the type of quoting characters your file contains. If unknown, use the default 'Double Quotes'."),
                            tags$li("Once these three selections are made, use the 'Browse' button to select your file to upload.")
                    ),

                    h4("______________________________________________________________________________"),
                    h4("NOTES:"),
                    tags$ul(
                        tags$li("The file must have ONLY TWO columns. Place unique identifiers in column 1 
                            and text narratives in column 2."),
                        tags$li("File upload is limited to 5MB (i.e. approximately 50,000 narratives of 100 characters each.")
                    ),
                    h4("______________________________________________________________________________"),
                    h3("When you observe the 'Upload complete' message, select the 'Perform Autocoding' tab."),

                    
                    box(width=10,
                        fluidPage(
                            
                            # App title ----
                            titlePanel("Uploading File"),
                            
                            # Sidebar layout with input and output definitions ----
                            sidebarLayout(
                                
                                # Sidebar panel for inputs ----
                                sidebarPanel(
                                    
                                    # Input: Select a file ----
                                    fileInput("file1", "Choose Input File",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    
                                    # Horizontal line ----
                                    tags$hr(),
                                    
                                    # Input: Checkbox if file has header ----
                                    checkboxInput("header", "Header", TRUE),
                                    
                                    # Input: Select separator ----
                                    radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ","),
                                    
                                    # Input: Select quotes ----
                                    radioButtons("quote", "Quote",
                                                 choices = c(None = "",
                                                             "Double Quote" = '"',
                                                             "Single Quote" = "'"),
                                                 selected = '"'),
                                    
                                    # Horizontal line ----
                                    tags$hr()
                                    
                                    
                                ),     #end sidebar panel
                                
                                # Main panel for displaying outputs ----
                                mainPanel(
                                    
                                    # Output: Data file ----
                                    tableOutput("mytable")
                                    
                                ) # end main panel
                                
                            )   # end sidebar layout
                        )    # end fluid page
                        
                    )   # end box
                    
            ),
            
            # Third tab content "Perform Autocoding"
            tabItem(tabName = "autocode",
                    tableOutput('dat'),
                    h2("Step 2. 'Perform Autocoding' of the uploaded file."),
                    h4("______________________________________________________________________________"),
                    h4("NOTE:  The autocoding process can be lengthy depending on the number of narratives in your file."),
                    h4("After selecting the 'Begin Coding' button below, expect an initial processing delay from one to two 
                    minutes before a progress indicator becomes visible. Please be patient"),
                    h4(""),
                    h4("After the initial processing delay, an incremental indicator will scroll until the autocoding process is complete. Allow
                       approximately ten minutes for 50,000 narratives."), 
                    h4("______________________________________________________________________________"),
                    h2(""),
                    
                    actionButton('goAutocode', 'Begin Coding'),
                    h2(""),
                    

                    h4("When scrolling ceases, the first few results will be displayed above. These results include a score."),
                    p(""),
                    p("(These scores range from 0 to 1 and are probability estimates that the assigned codes are correct.)"),
                    p(""),
                    h3("After results display, select 'Download Results' tab.")
            ),
            
            # Fourth tab content "Download Results"
            tabItem(tabName = "download",
                    h2("Step 3. 'Download Results' to your computer."),

                    # Button
                    downloadButton("downloadData", "Download")
                    
                    #h3("To finish, select 'Get a Report' tab.")
            )

            ### For future use
            # # Fifth tab content "Get a Report"
            # tabItem(tabName = "report", "Get Report")

            )
        )
    )








# Define server logic to read selected file ----
server <- function(input, output) {
    
##########################################################################################
    ## This is the first reactive function to enable input of the data
    
    inputData <- reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)    # Just a check to see if input is 'truthy'
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                unknowns <- read.csv(input$file1$datapath,
                                     header = input$header,
                                     sep = input$sep,
                                     quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        
        return(unknowns)  
        
        
    })      # End of reactive upload operation 
###################################################################################################
    

    
####################################################################################################
    ## This is the second reactive function to enable caching of the data output file

    dataFile <- reactive({
                
        # Two data files have already been retrieved from googlesheets; names are 'betas_data'
        #    and 'words_data'
        # The narratives data file has been input by user in the 'Uploading Files' tab; its name is 'unknowns'
        # It is cached in the inputData() reactive
        # The data file should have an id in column 1 and narrative in column 2
        #   with the first row being column names

            
        one_word <- inputData() # this is calling a reactive fn inside a reactive fn
        
        # This is the list of 3000+ key words and their beta values for logistic regression (supplied by Bertke, et al.)
        betas <- betas_data
        beta_wts <- betas[ , 3:37]  # there are 35 possible two-digit OIICS categories in cols 3 to 37
        
        # This is a list of useless narrative words to be removed
        words_to_remove <- words_data
        
        ########################################################################################
        # Clean up the narrative data 
        
        # Keep column of id numbers separate from narratives
        keep_ids <- one_word[ , 1]
        
        # Make an R list of narratives only
        one_word_list <- list(one_word[ , 2])
        
        # Remove non-UTF8 characters from list of narratives
        one_word_list <- sapply(one_word_list, function(x) iconv(x, "latin1", "ASCII", sub=""))
        
        # Ensure all words are in upper case
        one_word_list <- toupper(one_word_list)
        
        # Remove useless words, puctuation, and numbers using R package tm
        one_word_list <- sapply(one_word_list, function (x) tm::removeWords(x ,toupper(words_to_remove)))
        one_word_list <- sapply(one_word_list, function (x) tm::removePunctuation(x))
        one_word_list <- sapply(one_word_list, function (x) tm::removeNumbers(x))
        
        # Remove leading or trailing whitespace from strings
        one_word_list <- sapply(one_word_list, function (x) gsub("^\\s+|\\s+$", "", x))
        
        # Remove interior whitespace
        one_word_list <- sapply(one_word_list, function (x) gsub('\\s+', ' ', x))
        
        ########################################################################################
        # Now compose two R lists. The first list is individual words. The second is two-word sequences.
        
        # Split the narrative strings into individual word strings
        one_word_list <- sapply(one_word_list, function (x) strsplit(x, ' '))
        
        # Correct some common misspellings
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='FELLED', 'FELL', x))
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='FELF', 'FELL', x))
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='SLIPED', 'SLIPPED', x))
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='TRIPED', 'TRIPPED', x))
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='BCK', 'BACK', x))
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='SHOLDER', 'SHOULDER', x))
        one_word_list <- sapply(one_word_list, function (x) ifelse(x=='SHOULDERS', 'SHOULDER', x))
        
        
        # Using the function below assembles two word sequences from the one word list
        # CAUTION: for this function to work, all list items shall have length two or greater (for two-word strings)
        two_word_func <- function(x) {
            two_word <- c(rep( 0,  (ifelse(length(x) > 1, length(x), 2) ) - 1 ))
            for (j in 1: (ifelse(length(x) > 1, length(x), 2) ) - 1 ) {        # ensure length(x) > 1
                two_word[j] <- paste(x[j], x[j+1] )
            }
            return(two_word)  
        }
        
        
        # Create an R list of two-word sequences using two-word function above
        two_word_list <- sapply(one_word_list, two_word_func)
        
        ########################################################################################
        # Perform some operations using the word list tables and beta weights 
        
        # For each narrative, create a vector of final words by combining the corresponding elements of one_word_list and two_word_list 
        # For example, the code for the final word vector of the initial elements of the two lists is:
        #  vector <- unname(unlist ( c( one_word_list[1], two_word_list[1] ) ) )
        
        # Loop through all narratives one by one, find the vector of weights corresponding to the final word vector ...
        #... for each  two-digit OIICS category 
        
        # Initialize two vectors to store results
        score <- c( rep(0,  nrow(one_word) ) )
        event_code <- c( rep(0,  nrow(one_word) ) )
        
        # Create 0-row data frame which will be used to store data
        dat <- data.frame(matrix( nrow = nrow(one_word), ncol=2))
        colnames(dat) <- c("event_code", "score")
                    
                    
                    # Use a loop to obtain and store the results for each narrative
        for (i in 1: nrow(one_word) ){
            vector <- unname(unlist ( c( one_word_list[i], two_word_list[i] ) ) )       # create and sort vector of one-word ...
            vector <- sort(vector)                                                              # ... and two-word sequences
            indices <- match(vector, betas$w)       # find indices of the narrative words occuring within the beta file of key words
            indices <- indices[!is.na(indices)]     # drop all indices for beta words NOT in the (one & two word) vector just created  
            final_words <- as.character(betas$w[indices])       # create character vector of narrative words (not required for processing)
            col_totals <- colSums(betas[c(1 , indices) , 3:37] )         # get totals for column weights (incl. intercept in row 1) corresponding to each OIICS code
            max(col_totals)                         # find the maximum column total (i.e. most likely OIICS code based on words in vector)
            event_code[i] <- names(beta_wts[ which( col_totals == max( col_totals) ) ])    # assign and record the OIICS code to this narrative
            score[i] <- round( 1/ (1 + exp( -(max(col_totals)  ) ) ) , 3)        # use logistic regression to establish and assign the ...
            # ...probability of OIICS code
            dat[i,] <- c(event_code[i], score[i])
                        
     #######################################################################################################
                # Begin a loop to track progress of autocoding. Is there a better method?
                withProgress(message = 'Processing Input File', value = 0, {
                    # Increment the progress bar, and update the detail text.
                    incProgress(1/nrow(one_word), detail = paste("Processing row", i))
                } )             # End of the progress loop
     #######################################################################################################
                        
        }           # End of 'for loop'
                    
        # Bind the narratives together with the results in a single dataframe
            
        dat <- cbind(dat, inputData() )      # This stores 'dataFile' as a reactive to use later for 
                                                    #  renderTable and downloadHandler functions
            

    })     # End of reactive function 'dataFile' (note: returns file 'dat')
            
#######################################################################################################                   
    # Autocode tab starts here
        
    observeEvent(input$goAutocode,  {
            
    # Call reactive function output to renderTable w/ a few lines of autocoded dataFile (i.e. file 'dat')        
    output$dat <- renderTable({head(dataFile(), 4) })   # call reactive function output here
            
    }) # End of observeEvent for autocode
    
####################################################################################################
    # Download tab starts here
    
        # Call reactive function output to download autocoded dataFile output (i.e. file 'dat')

    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(dataFile(), file , row.names=FALSE)
            }
    )              


####################################################################################################
    # Report tab starts here    
      
}           # End of server function

# Create Shiny app ----
shinyApp(ui, server)
