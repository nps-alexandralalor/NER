# QAQC function
qaqc <- function(data, query, query_message, values_check) {
  
  # Check if there is data to QAQC
  if(nrow(data) == 0) {
    # If there is no data, default to "No Error" data frame
    errors <- errors_blank %>%
      mutate(SavedQuery = query,
             Error = "No Error")
    # If there is data, perform error check
  } else {
    
    # Use relevant dataset to look for errors
    errors <- data %>%  
      # Add columns relevant to error checking
      # Populate the "SavedQuery" column with the query name
      mutate("SavedQuery" = query, 
             # Populate the "Error" column with the query message and relevant data
             "Error" = paste(query_message, "=", values_data),
             # Create a blank "Fixed" column
             "Fixed" = "",
             # Create a blank "Explanation" column
             "Explanation" = "",
             # Create a blank "Queryers" column
             "Queryers" = "") %>%   
      # Filter for data which is "false" (data does not match valid conditions)
      # NA values are treated as "false" (missing data is not valid)
      filter(!(values_check %>% replace_na(FALSE))) %>%   
      # Select relevant columns to view errors
      select(colnames(errors_blank))
    
    # Next, check if there are duplicate errors
    errors_temp <- errors %>% 
      get_dupes(errors_group)
    # Merge duplicate errors
    errors <- unique(merge(errors, errors_temp, all = T))
    # If duplicate errors exist, add number of duplicates to error message
    errors <- errors %>% 
      mutate(Error = ifelse(is.na(dupe_count), Error, paste0("(x", dupe_count, ") ", Error))) %>% 
      select(!"dupe_count")
  }
  
  # Check if there are no errors
  if (nrow(errors) == 0) {  
    # If there are no errors, default to "No Error" data frame
    errors <- errors_blank %>%
      mutate(SavedQuery = query,
             Error = "No Error")
    # If there are errors, keep error log you just created
  } else {   
    errors <- errors
  }
}