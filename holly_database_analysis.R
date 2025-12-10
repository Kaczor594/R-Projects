# Holly Bronze Database - R Analysis Script
# ===========================================
# R version of the SQLite database analysis for municipal data

# Load required libraries
library(DBI)        # Database interface
library(RSQLite)    # SQLite driver for R
library(dplyr)      # Data manipulation
library(dbplyr)     # Database backend for dplyr
library(glue)       # String interpolation
library(knitr)      # Pretty table printing

cat("📦 Loading required R packages...\n")

# Check if packages are installed, install if missing
required_packages <- c("DBI", "RSQLite", "dplyr", "dbplyr", "glue", "knitr")
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
  
  # Load newly installed packages
  lapply(missing_packages, library, character.only = TRUE)
}

cat("✅ All packages loaded successfully!\n\n")

# ===========================================
# Database Connection Functions
# ===========================================

# Database path
DB_PATH <- "/Users/isaackaczor/VS-Code/R-Projects/holly_bronze.db"

# Function to create database connection
get_connection <- function() {
  if (!file.exists(DB_PATH)) {
    stop(paste("Database", DB_PATH, "not found!"))
  }
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

# Function to execute SQL queries and return data frame
query <- function(sql) {
  conn <- get_connection()
  on.exit(dbDisconnect(conn))  # Ensure connection is closed
  
  result <- dbGetQuery(conn, sql)
  return(as.data.frame(result))
}

# Function to execute SQL statements (INSERT/UPDATE/DELETE)
execute_sql <- function(sql) {
  conn <- get_connection()
  on.exit(dbDisconnect(conn))
  
  result <- dbExecute(conn, sql)
  return(result)
}

# Test connection
cat("🔌 Testing database connection...\n")
tryCatch({
  test_query <- "SELECT name FROM sqlite_master WHERE type='table' LIMIT 1"
  result <- query(test_query)
  
  # Get table count
  table_count <- query("SELECT COUNT(*) as count FROM sqlite_master WHERE type='table'")
  
  cat("✅ Successfully connected to", DB_PATH, "\n")
  cat("✅ Database contains", table_count$count, "tables\n\n")
}, error = function(e) {
  cat("❌ Connection failed:", e$message, "\n")
})

# ===========================================
# Database Exploration Functions
# ===========================================

# Function to list all tables
list_tables <- function() {
  sql <- "SELECT name as table_name FROM sqlite_master WHERE type='table' ORDER BY name"
  tables <- query(sql)
  
  cat("📋 Total tables:", nrow(tables), "\n\n")
  cat("All tables:\n")
  for (i in 1:nrow(tables)) {
    cat(sprintf("%2d. %s\n", i, tables$table_name[i]))
  }
  
  return(tables)
}

# Function to describe a table structure
describe_table <- function(table_name) {
  cat("\n=== Table:", table_name, "===\n")
  
  # Get column information
  columns_sql <- glue("PRAGMA table_info(`{table_name}`);")
  columns <- query(columns_sql)
  
  # Get row count
  count_sql <- glue("SELECT COUNT(*) as row_count FROM `{table_name}`;")
  count <- query(count_sql)
  
  cat("Rows:", format(count$row_count, big.mark = ","), "\n")
  cat("Columns:", nrow(columns), "\n\n")
  cat("Column Details:\n")
  
  # Print column info nicely
  print(columns[, c("name", "type", "notnull", "pk")])
  
  return(columns)
}

# ===========================================
# Leading Zero Padding Functions (R Version)
# ===========================================

# Function to find tables with department_number/gl_number
find_padding_tables <- function() {
  cat("🔍 FINDING TABLES WITH DEPARTMENT/GL NUMBERS...\n")
  cat("="*50, "\n")
  
  # Known tables to check
  known_tables <- c(
    'as_of_6_30_24', 'as-of-6-30-24',
    'as_of_6_30_25', 'as-of-6-30-25', 
    'general_25_26', 'general-25-26',
    'as_of_6_30_23', 'as-of-6-30-23'
  )
  
  found_tables <- list()
  
  for (table_name in known_tables) {
    tryCatch({
      # Check if table exists
      table_check <- query(glue("SELECT name FROM sqlite_master WHERE name = '{table_name}' LIMIT 1"))
      
      if (nrow(table_check) > 0) {
        actual_table <- table_check$name[1]
        cat("\n📋 Found:", actual_table, "\n")
        
        # Check columns
        cols <- query(glue("PRAGMA table_info(`{actual_table}`)"))
        col_names <- tolower(cols$name)
        
        dept_col <- cols$name[grep("department", col_names)][1]
        gl_col <- cols$name[grep("gl_number", col_names)][1]
        
        if (!is.na(dept_col) || !is.na(gl_col)) {
          found_tables[[actual_table]] <- list(
            table = actual_table,
            dept_col = if(!is.na(dept_col)) dept_col else NULL,
            gl_col = if(!is.na(gl_col)) gl_col else NULL
          )
          
          cat("   ✅ Dept column:", if(!is.na(dept_col)) dept_col else "None", "\n")
          cat("   ✅ GL column:", if(!is.na(gl_col)) gl_col else "None", "\n")
        }
      }
    }, error = function(e) {
      cat("   ❌ Error with", table_name, ":", substr(e$message, 1, 50), "...\n")
    })
  }
  
  return(found_tables)
}

# Function to create padded views
create_padded_views <- function() {
  cat("🏗️ CREATING PADDED VIEWS...\n")
  cat("="*40, "\n")
  
  found_tables <- find_padding_tables()
  success_count <- 0
  
  for (table_info in found_tables) {
    tryCatch({
      table_name <- table_info$table
      dept_col <- table_info$dept_col
      gl_col <- table_info$gl_col
      
      view_name <- paste0(table_name, "_PADDED")
      
      # Build SELECT statement
      select_parts <- c("*")  # Keep all original columns
      
      if (!is.null(dept_col)) {
        select_parts <- c(select_parts, 
                         glue("PRINTF('%03d', CAST(`{dept_col}` AS INTEGER)) as {dept_col}_padded"))
      }
      
      if (!is.null(gl_col)) {
        select_parts <- c(select_parts,
                         glue("PRINTF('%04d', CAST(`{gl_col}` AS INTEGER)) as {gl_col}_padded"))
      }
      
      # Create view SQL
      view_sql <- glue("
        CREATE OR REPLACE VIEW `{view_name}` AS
        SELECT {paste(select_parts, collapse = ',\n       ')}
        FROM `{table_name}`;
      ")
      
      # Execute view creation
      execute_sql(view_sql)
      cat("   ✅ Created:", view_name, "\n")
      
      # Test the view with sample data
      if (!is.null(dept_col) && !is.null(gl_col)) {
        test_sql <- glue("
          SELECT {dept_col}, {dept_col}_padded, {gl_col}, {gl_col}_padded 
          FROM `{view_name}` 
          LIMIT 3
        ")
        
        sample_data <- query(test_sql)
        cat("   📊 Sample from", view_name, ":\n")
        print(sample_data)
        cat("\n")
      }
      
      success_count <- success_count + 1
      
    }, error = function(e) {
      cat("   ❌ Failed to create view for", table_info$table, ":", e$message, "\n")
    })
  }
  
  cat("🎉 COMPLETED! Created", success_count, "padded views.\n\n")
  
  if (success_count > 0) {
    cat("🎯 NOW YOU CAN USE:\n")
    cat("   • query('SELECT * FROM as_of_6_30_24_PADDED LIMIT 5')\n")
    cat("   • All department numbers: 001, 002, 003...\n")
    cat("   • All GL numbers: 0001, 0012, 0123...\n")
  }
  
  return(success_count)
}

# ===========================================
# Data Analysis Helper Functions
# ===========================================

# Function for quick SQL execution with pretty output
sql <- function(query_string) {
  result <- query(query_string)
  
  # Print nicely formatted result
  if (nrow(result) > 0) {
    print(knitr::kable(result, format = "pipe"))
  } else {
    cat("No results returned.\n")
  }
  
  return(result)
}

# Function to show table sample
show_sample <- function(table_name, n = 5) {
  cat("📊 Sample from", table_name, "(first", n, "rows):\n")
  sample_sql <- glue("SELECT * FROM `{table_name}` LIMIT {n}")
  result <- query(sample_sql)
  print(knitr::kable(result, format = "pipe"))
  return(result)
}

# ===========================================
# Example Analysis Functions
# ===========================================

# Road improvements analysis
analyze_roads <- function() {
  cat("🛣️ ROAD IMPROVEMENTS ANALYSIS\n")
  cat("="*40, "\n")
  
  road_query <- "
    SELECT 
      segment_name,
      proposed_treatment,
      estimated_cost,
      current_rating,
      surface_subtype
    FROM critical_road_improvements_1 
    ORDER BY estimated_cost DESC 
    LIMIT 10
  "
  
  cat("Top 10 most expensive road improvements:\n")
  result <- query(road_query)
  print(knitr::kable(result, format = "pipe"))
  
  return(result)
}

# Budget analysis
analyze_budget <- function() {
  cat("💰 BUDGET ANALYSIS (2025-26)\n")
  cat("="*30, "\n")
  
  budget_query <- "
    SELECT 
      department_name,
      gl_number,
      account_description,
      `202526_recommended_budget` as recommended_budget
    FROM `general_25-26`
    WHERE `202526_recommended_budget` > 100000
    ORDER BY `202526_recommended_budget` DESC
    LIMIT 15
  "
  
  cat("Largest budget items (>$100k):\n")
  result <- query(budget_query)
  print(knitr::kable(result, format = "pipe"))
  
  return(result)
}

# ===========================================
# Main Execution
# ===========================================

cat("🚀 HOLLY BRONZE DATABASE - R ANALYSIS READY!\n")
cat("="*50, "\n")
cat("Available functions:\n")
cat("• list_tables()                    - Show all database tables\n")
cat("• describe_table('table_name')     - Show table structure\n")
cat("• show_sample('table_name')        - Show sample data\n")
cat("• sql('SELECT * FROM table')       - Execute SQL with pretty output\n")
cat("• create_padded_views()           - Fix leading zero padding\n")
cat("• analyze_roads()                 - Road improvements analysis\n")
cat("• analyze_budget()                - Budget analysis\n")
cat("\nQuick start:\n")
cat("1. list_tables()           # See what's available\n")
cat("2. create_padded_views()   # Fix department/GL number padding\n")
cat("3. analyze_roads()         # Example road analysis\n\n")

# Auto-run the padding fix
cat("🔧 AUTO-RUNNING PADDING FIX...\n")
create_padded_views()

cat("✅ R analysis environment ready! Happy analyzing! 📊\n")
