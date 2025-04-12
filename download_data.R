# Getting Stock_Data

# Load or get sp500_symbols
if (!file.exists("symbols.csv")) {
  sp500 <- tq_index("SP500")
  sp500_symbols <- sp500$symbol
  write.csv(sp500_symbols, "symbols.csv", row.names = FALSE)
} else {
  sp500_symbols <- read.csv("symbols.csv", stringsAsFactors = FALSE)$x
  print("Symbols already exist")
  sp500 <- NULL  # In case not loaded
}

# Initialize or load existing data
if (file.exists("Data.RData")) {
  load("Data.RData")
  
  # Get last date from existing data
  last_date <- max(sp500_data$date, na.rm = TRUE)
  
  # Check if update is needed
  if (last_date < Sys.Date()) {
    print(last_date)
    print(Sys.Date())
    # Fetch new data starting from the day after the last recorded date
    new_data <- tq_get(
      sp500_symbols,
      from = as.Date(last_date) + 1,
      to = Sys.Date()
    )
    
    # Combine old and new data only if new_data is not empty
    if (nrow(new_data) > 0) {
      sp500_data <- bind_rows(sp500_data, new_data) %>%
        distinct(symbol, date, .keep_all = TRUE)
      print("Data updated successfully.")
    } else {
      print("Data is already up to date.")
    }
  } else {
    print("Data is already up to date.")
  }
  
} else {
  # First time: get full data (past 5 years)
  sp500_data <- tq_get(
    sp500_symbols,
    from = Sys.Date() - 5 * 365,
    to = Sys.Date()
  )
}

# Optionally remove the old data file (not necessary since save() overwrites)
if(file.exists("Data.RData")){
  if(file.remove("Data.RData")){
    print("Old file removed successfully")
  } else {
    print("File could not be removed")
  }
}

sp500_data <- sp500_data %>%
  distinct(symbol, date, .keep_all = TRUE)

# Save everything using a conventional R data file extension
save(sp500_data, file = "Data.RData")

# Clearing the Variables
rm(list = ls())
cat("\014")