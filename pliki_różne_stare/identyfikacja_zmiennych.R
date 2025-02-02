# unikalne w zmiennych kategorycznych tekstowych
unique_values_list <- lapply(HR[, sapply(HR, is.character)], unique)

unique_values_list_padded <- lapply(unique_values_list, function(x) {
  length(x) <- max(sapply(unique_values_list, length))
  return(x)
})

unique_values_df <- as.data.frame(unique_values_list_padded)

colnames(unique_values_df) <- names(unique_values_list)

unique_values_df

# unikalne w zmiennych kategorycznych numerycznych
get_limited_numeric_values <- function(df, threshold = 10) {
  numeric_cols <- names(df)[sapply(df, is.numeric) & sapply(df, function(x) n_distinct(x) < threshold)]
  
  if (length(numeric_cols) == 0) return(data.frame())
  
  unique_values_list <- lapply(df[numeric_cols], function(x) sort(unique(x)))
  
  max_length <- max(sapply(unique_values_list, length))
  unique_values_df <- as.data.frame(lapply(unique_values_list, `length<-`, max_length))
  unique_values_df <- unique_values_df[rowSums(!is.na(unique_values_df)) > 0, ]
  
  return(unique_values_df)
}

# kolumny z unikalnymi wartosciami numerycznymi (identyfikacja zmiennej identyfikujacej pracownika)
get_limited_numeric_values(HR)

unique_columns <- function(df) {
  col_names <- names(df)
  unique_cols <- col_names[sapply(df, function(x) {
    is.vector(x) && !any(duplicated(x)) && (!is.numeric(x) || all(x == round(x)))
  })]
  return(unique_cols)
}

unique_columns(HR)