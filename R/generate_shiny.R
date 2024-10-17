#' Generate a Shiny application based on user description
#'
#' This function sends a user description of a Shiny application to OpenAI and saves the generated code to a specified path.
#' @param app_description A description of the type of Shiny application to generate
#' @param path A path to the directory where the application will be saved
#' @return NULL
#' @export
generate_shiny <- function(app_description, path) {
  # Get OpenAI API key from environment
  OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
  
  # Construct the prompt for the LLM
  prompt <- paste(
    "Generate R code for a Shiny application based on the following description:",
    app_description
  )
  
  # Send the POST request to OpenAI API
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(
      model = "gpt-3.5-turbo-0125",
      messages = list(
        list(role = "system", content = "You are a helpful assistant that generates R code for Shiny applications."),
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  # Check for successful response
  if (httr::status_code(response) == 200) {
    content <- httr::content(response)
    app_code <- content$choices[[1]]$message$content
    
    # Create the directory if it doesn't exist
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    
    # Write the app code to App.R in the specified path
    app_file <- file.path(path, "App.R")
    writeLines(app_code, app_file)
    
    message("Shiny application saved to ", app_file)
  } else {
    stop("Failed to retrieve a response from the API: HTTP status code ", httr::status_code(response))
  }
}