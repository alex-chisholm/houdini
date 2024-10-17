# Houdini

**Houdini** is an R package designed to generate synthetic datasets using the OpenAI API and provide data analysis capabilities. It also includes functionality to generate Shiny applications based on user descriptions.

## Installation

You can install the development version of **Houdini** from GitHub using the following commands:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install Houdini
devtools::install_github("yourusername/Houdini")
```
## API Key 

Each function relies on an API call to OpenAI. Your key must be stored as an enviornment variable named `OPENAI_API_KEY`.