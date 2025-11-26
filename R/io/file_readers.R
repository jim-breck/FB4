########################################################################
### File Reading Utilities
########################################################################
#
# This module provides standardized functions for reading input files
# with consistent error handling.
#
########################################################################

read_csv_with_check <- function(file_path, file_description = "file") {
  ### Read a CSV file with existence check and error handling
  ###
  ### Parameters:
  ###   file_path: Path to the CSV file
  ###   file_description: Description of file for error messages
  ###
  ### Returns: Data frame from CSV file
  ### Exits: If file does not exist

  if(file.exists(file_path)) {
    return(read.csv(file_path, stringsAsFactors = FALSE))
  } else {
    print(paste("Cannot find", file_description, ":", file_path))
    exit()
  }
}

read_csv_with_header <- function(file_path, file_description = "file") {
  ### Read a CSV file with header=TRUE and existence check
  ###
  ### Parameters:
  ###   file_path: Path to the CSV file
  ###   file_description: Description of file for error messages
  ###
  ### Returns: Data frame from CSV file with headers
  ### Exits: If file does not exist

  if(file.exists(file_path)) {
    return(read.csv(file_path, head=TRUE, stringsAsFactors = FALSE))
  } else {
    print(paste("Cannot find", file_description, ":", file_path))
    exit()
  }
}

check_file_exists <- function(file_path, file_description = "file") {
  ### Check if a file exists and print error if not
  ###
  ### Parameters:
  ###   file_path: Path to check
  ###   file_description: Description of file for error messages
  ###
  ### Returns: TRUE if file exists, exits otherwise

  if(file.exists(file_path)) {
    return(TRUE)
  } else {
    print(paste("Cannot find", file_description, ":", file_path))
    exit()
  }
}
