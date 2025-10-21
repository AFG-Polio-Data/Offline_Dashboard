all_data_list <- map(all_data_list, function(x) {
  if (is.data.frame(x)) {
    return(ungroup(x))  # Apply ungroup() if it's a dataframe
  } else {
    return(x)  # Otherwise, leave it unchanged
  }
})

ungroup_all_levels <- function(x) {
  if (is.data.frame(x)) {
    return(ungroup(x)) # Ungroup data frames and return them as data frames
  } else if (is.list(x)) {
    return(lapply(x, ungroup_all_levels)) # Use lapply to maintain structure
  } else {
    return(x) # Leave other elements unchanged
  }
}

# Apply to all_data_list
all_data_list <- map(all_data_list, ungroup_all_levels)
