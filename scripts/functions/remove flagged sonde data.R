### function to remove flagged data
# input is the cleaned dataframe

# function to remove the flagged data
remove.flagged.data <- function(df) {
  for (i in 1:nrow(df)) {
    # split the flag column into individual column names for each row
    flagged_columns <- str_split(df$flag[i], "; ")[[1]]
    
    # replace the values in the flagged columns with NA for the current row
    if(!is.na(df$flag[i])){
      df[i, flagged_columns] <- NA
    }
  }
  return(df)
  
}
