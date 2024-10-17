#' Analyze data using OpenAI API
#'
#' This function takes a dataframe and requests a summary from an LLM.
#' @param df A dataframe to be summarized
#' @return A character string containing the summary of the dataframe
#' @export
analyze_data <- function(df) {
  # Get OpenAI API key from environment
  OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")

  # Convert the dataframe to a string representation
  df_string <- capture.output(print(df))  # Converts df to a character vector
  df_string <- paste(df_string, collapse = "\n")  # Collapse into a single string

  # Construct the prompt for analysis
  prompt <- paste(
    "Please summarize the following dataset:",
    df_string
  )

  # Send the POST request to OpenAI API
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(
      model = "gpt-3.5-turbo-0125",
      messages = list(
        list(role = "system", content = "You are a helpful assistant that summarizes datasets."),
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE),
    encode = "json"
  )

  # Check for successful response
  if (httr::status_code(response) == 200) {
    content <- httr::content(response)
    summary_text <- content$choices[[1]]$message$content
    return(summary_text)
  } else {
    stop("Failed to retrieve a response from the API: HTTP status code ", httr::status_code(response))
  }
}