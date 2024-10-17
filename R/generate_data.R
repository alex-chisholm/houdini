#' Preprocess CSV string into a tibble
#'
#' @param csv_string A string containing CSV data
#' @return A tibble dataframe with the processed CSV data
preprocess_csv <- function(csv_string) {
  # Regular expression to extract CSV portion
  csv_pattern <- "(?s)(.+?\\n(?:[^,\n]+(?:,[^,\n]+)*\n){2,})"
  csv_match <- stringr::str_extract(csv_string, csv_pattern)
  
  if (is.na(csv_match)) {
    stop("No valid CSV data found in the response.")
  }
  
  # Split lines and remove empty lines
  lines <- stringr::str_split(csv_match, "\n")[[1]]
  lines <- lines[lines != ""]
  
  # Split header and determine the number of columns
  header <- stringr::str_split(lines[1], ",")[[1]]
  num_cols <- length(header)
  
  # Ensure consistent number of columns for each row
  processed_lines <- sapply(lines[-1], function(line) {  # Skip header
    cols <- stringr::str_split(line, ",")[[1]]
    if (length(cols) < num_cols) {
      cols <- c(cols, rep("", num_cols - length(cols)))
    } else if (length(cols) > num_cols) {
      cols <- cols[1:num_cols]
    }
    cols
  })
  
  # Return a tibble
  tibble::tibble(!!!setNames(as.list(as.data.frame(t(processed_lines))), header))
}

#' Generate synthetic dataset using OpenAI API
#'
#' This function sends a dataset description to an LLM and returns a tibble dataframe.
#' @param dataset_description A description of the dataset to be generated
#' @return A tibble dataframe with the generated dataset
#' @export
generate_data <- function(dataset_description) {
  # Get OpenAI API key from environment
  OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
  
  # Construct the prompt
  prompt <- paste(
    "Generate a fake dataset with at least two variables as a CSV string based on this description:",
    dataset_description,
    "Include a header row. Limit to 25 rows of data. Ensure all rows have the same number of columns. Do not include any additional text or explanations."
  )
  
  # Send the POST request to OpenAI API
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(
      model = "gpt-3.5-turbo-0125",
      messages = list(
        list(role = "system", content = "You are a helpful assistant that generates fake datasets."),
        list(role = "user", content = prompt)
      )
    ), auto_unbox = TRUE),
    encode = "json"
  )
  
  # Check for successful response
  if (httr::status_code(response) == 200) {
    content <- httr::content(response)
    
    # Extract the CSV data from the response
    csv_string <- content$choices[[1]]$message$content
    
    # Process the CSV data into a tibble
    df <- preprocess_csv(csv_string) %>%
      janitor::clean_names() %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(ifelse(!is.na(as.numeric(.)), as.numeric(.), as.character(.)))))
    
    return(df)
  } else {
    stop("Failed to retrieve a response from the API: HTTP status code ", httr::status_code(response))
  }
}

# Example usage
generate_data("planets")