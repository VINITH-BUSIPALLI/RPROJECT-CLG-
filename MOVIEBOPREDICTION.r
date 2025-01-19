library(shiny)
library(shinythemes)
library(httr)
library(jsonlite)
library(stringr)
library(scales)

# API Configuration
API_KEY <- "6a9f7f8989bcb2e40ea4c1694e31280d"
BASE_URL <- "https://api.themoviedb.org/3"

# Custom Predictive Model Placeholder
predict_box_office <- function(release_date) {
  days_until_release <- as.numeric(as.Date(release_date) - Sys.Date())
  if (days_until_release > 0) {
    return(200000000 + (days_until_release * 1000000))  # Example prediction logic
  } else {
    return(0)  # Not applicable for already released movies
  }
}

# Custom Theme and Styling Function
create_custom_theme <- function() {
  tags$style(HTML("
    body {
      background-color: #FFF176;  /* Soft Yellow Background */
      font-family: 'Arial', sans-serif;
    }
    .input-container {
      text-align: center;
      margin-bottom: 20px;
    }
    .movie-details-container {
      margin-top: 20px;
    }
    .financial-section {
      margin-top: 20px;
      border: 1px solid #6A1B9A;
      padding: 15px;
      border-radius: 5px;
      background-color: #E1BEE7; /* Light Purple Background */
    }
    .movie-poster {
      margin-right: 20px; /* Add margin to the right of the poster */
      max-width: 100%; /* Ensure responsive image */
    }
    .details-column {
      padding-left: 20px; /* Add padding to the left of the details column */
      text-align: left; /* Align text to the left */
    }
  "))
}

# UI Definition
ui <- fluidPage(
  create_custom_theme(),
  
  titlePanel(
    div(
      style = "color: #6A1B9A; text-align: center;", 
      "MOVIE RATING AND BOX OFFICE ANALYSIS"
    )
  ),
  
  div(
    class = "container centered",
    div(
      class = "input-container",
      textInput(
        "movie_name", 
        label = NULL, 
        placeholder = "Enter Movie Name ex: avatar",
        width = "300px"
      ),
      actionButton(
        "search_movie", 
        "Search Movies", 
        class = "btn btn-custom"
      ),
      uiOutput("movie_select")  # Dropdown for movie selection
    ),
    
    uiOutput("movie_details_container")
  )
)

# Server Logic
server <- function(input, output, session) {
  movie_data <- reactiveVal(NULL)
  error_message <- reactiveVal(NULL)
  
  observeEvent(input$search_movie, {
    movie_data(NULL)
    error_message(NULL)
    
    movie_name <- str_trim(input$movie_name)
    if (nchar(movie_name) < 2) {
      error_message("Please enter a valid movie name")
      return()
    }
    
    search_url <- paste0(
      BASE_URL, 
      "/search/movie?api_key=", API_KEY, 
      "&query=", URLencode(movie_name)
    )
    
    tryCatch({
      response <- GET(search_url)
      content <- fromJSON(content(response, "text", encoding = "UTF-8"))
      
      if (length(content$results) == 0) {
        error_message("No movies found matching your search.")
        output$movie_select <- renderUI({ NULL })  # Clear dropdown
        return()
      }
      
      # Create a list of movie titles and IDs for the dropdown
      movie_choices <- setNames(content$results$id, content$results$title)
      output$movie_select <- renderUI({
        selectInput("selected_movie", "Select a Movie:", choices = movie_choices, selected = NULL)
      })
      
    }, 
    error = function(e) {
      error_message(paste("An unexpected error occurred:", e$message))
    })
  })
  
  # Observe changes to the selected movie
  observeEvent(input$selected_movie, {
    req(input$selected_movie)  # Ensure that a movie is selected
    
    details_url <- paste0(
      BASE_URL, 
      "/movie/", input$selected_movie, 
      "?api_key=", API_KEY
    )
    
    details_response <- GET(details_url)
    full_details <- fromJSON(content(details_response, "text", encoding = "UTF-8"))
    
    # Get details of the selected movie
    combined_movie_data <- list(
      title = full_details$title,
      overview = full_details$overview,
      release_date = full_details$release_date,
      poster_path = paste0("https://image.tmdb.org/t/p/w500", full_details$poster_path),
      vote_average = full_details$vote_average,
      budget = full_details$budget %||% 0,
      revenue = full_details$revenue %||% 0,
      genres = paste(full_details$genres$name, collapse = ", "),
      runtime = full_details$runtime
    )
    
    # Predict Box Office Collection
    predicted_collection <- predict_box_office(full_details$release_date)
    combined_movie_data$predicted_collection <- predicted_collection
    
    movie_data(combined_movie_data)
  })
  
  output$movie_details_container <- renderUI({
    req(movie_data())
    
    data <- movie_data()
    
    div(
      class = "movie-details-container",
      fluidRow(
        column(4, 
               tags$img(
                 src = data$poster_path, 
                 class = "img-fluid movie-poster",
                 alt = paste("Poster for", data$title)
               )
        ),
        column(8,
               div(class = "details-column",  # Added a class for padding
                   h2(style = "color: #FFD54F;", data$title),
                   p(strong("Release Date:"), data$release_date),
                   p(strong("Genres:"), data$genres),
                   p(strong("Runtime:"), paste(data$runtime, "minutes")),
                   p(strong("Overview:"), data$overview)
               )
        )
      ),
      
      div(
        class = "financial-section",
        h3("Financial Details"),
        p(strong("Budget:"), dollar(data$budget)),
        p(strong("Revenue:"), dollar(data$revenue)),
        p(strong("Predicted Box Office Collection:"), dollar(data$predicted_collection)),
        p(strong("Profit:"), dollar(data$revenue - data$budget)),
        p(strong("Vote Average:"), sprintf("%.1f/10", data$vote_average))
      )
    )
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
