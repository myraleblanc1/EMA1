#Scoring rename function
#DO NOT EDIT####################################################################
rename_qualfunc <- function(file_path, ques_tibble) {
  library(tidyverse)
  
  rawdata <- read_csv(file_path)
  colnames_raw <- names(rawdata)
  
  # Normalize function
  normalize_text <- function(x) {
    x |>
      str_replace_all("“|”", "\"") |>
      str_replace_all("‘|’", "'") |>
      str_replace_all("[[:punct:]]", ".") |>     # turn all punctuation into dots
      str_replace_all("\\s+", ".") |>            # turn all spaces into dots
      tolower()
  }
  
  
  # Normalize column names and item texts
  colname_df <- tibble(
    original = colnames_raw,
    cleaned = normalize_text(colnames_raw)
  )
  
  ques_tibble_cleaned <- ques_tibble |>
    mutate(cleaned = normalize_text(text))
  
  # Join to get final names
  rename_lookup <- left_join(colname_df, ques_tibble_cleaned, by = "cleaned") |>
    mutate(final_name = coalesce(item, original))
  
  # Remove duplicates
  matched_lookup <- rename_lookup |> filter(!duplicated(original))
  
  # Apply renaming
  for (i in seq_len(nrow(matched_lookup))) {
    col <- matched_lookup$original[i]
    new_col <- matched_lookup$final_name[i]
    if (col != new_col && col %in% names(rawdata)) {
      names(rawdata)[names(rawdata) == col] <- new_col
    }
  }
  
  return(rawdata)
}

