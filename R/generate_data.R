library(tidyverse)
library(janitor)
library(httr)
library(jsonlite)

preprocess_csv <- function(csv_string) {
  # Extract only the CSV part
  csv_pattern <- "(?s)(.+?\\n(?:[^,\n]+(?:,[^,\n]+)*\n){2,})"
  csv_match <- str_extract(csv_string, csv_pattern)
  
  if (is.na(csv_match)) {
    stop("No valid CSV data found in the response")
  }
  
  lines <- str_split(csv_match, "\n")[[1]]
  lines <- lines[lines != ""]  # Remove empty lines
  
  # Get the number of columns from the header
  header <- str_split(lines[1], ",")[[1]]
  num_cols <- length(header)
  
  # Ensure all rows have the same number of columns
  processed_lines <- sapply(lines[-1], function(line) {  # Skip header
    cols <- str_split(line, ",")[[1]]
    if (length(cols) < num_cols) {
      cols <- c(cols, rep("", num_cols - length(cols)))
    } else if (length(cols) > num_cols) {
      cols <- cols[1:num_cols]
    }
    cols
  })
  
  # Create a tibble
  tibble(!!!setNames(as.list(as.data.frame(t(processed_lines))), header))
}

#' Generate data function
#'
#' This function passes a dataset description to an LLM and returns a dataset in tibble dataframe form
#'
#' @return A tibble dataframe
#' @export
generate_data <- function(dataset_description) {
  OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
  prompt <- paste("Generate a fake dataset with at least two variables as a CSV string based on this description:",
  dataset_description, "Include a header row. Limit to 25 rows of data. Ensure all rows have the same number of columns. Do not include any additional text or explanations.")

  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", OPENAI_API_KEY)),
    content_type_json(),
    body = toJSON(list(
    model = "gpt-3.5-turbo-0125",
    messages = list(
    list(role = "system", content = "You are a helpful assistant that generates fake datasets."),
    list(role = "user", content = prompt)
    )
    ), auto_unbox = TRUE),
    encode = "json"
    )
  
    if (status_code(response) == 200) {
      content <- content(response)
      csv_string <- content$choices[[1]]$message$content

      df <- preprocess_csv(csv_string) %>% clean_names() %>% 
          mutate(across(everything(), ~ ifelse(suppressWarnings(!is.na(as.numeric(.))), as.numeric(.), as.character(.))))
        return(df)
      }
  break
}