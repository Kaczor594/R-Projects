# Holly Bronze Database - Excel to SQLite Importer
# ================================================
# Import Excel workbook sheets to SQLite with proper column types

library(readxl)
library(DBI)
library(RSQLite)
library(dplyr)
library(glue)

# Configuration
EXCEL_PATH <- "/Users/isaackaczor/VS-Code/R-Projects/holly_bronze.xlsx"  # Adjust path
DB_PATH <- "/Users/isaackaczor/VS-Code/R-Projects/holly_bronze.db"

# ===========================================
# Column Type Definitions
# ===========================================

# Define which columns should be TEXT (to preserve leading zeros)
FORCE_TEXT_COLUMNS <- c(
  "department_number",
  "dept_number", 
  "gl_number",
  "fund_number",
  "account_number",
  "object_code",
  "cost_center",
  "project_code"
)

# Define which columns should be REAL (decimal numbers)
FORCE_REAL_COLUMNS <- c(
  "amount",
  "balance",
  "budget",
  "actual",
  "estimated_cost",
  "202425_amended_budget",
  "202526_recommended_budget"
)

# ===========================================
# Import Functions
# ===========================================

# Function to determine SQL column type
get_sql_type <- function(col_name, sample_data) {
  col_name_lower <- tolower(col_name)
  
  # Force specific columns to TEXT (preserves leading zeros)
  if (any(sapply(FORCE_TEXT_COLUMNS, function(x) grepl(x, col_name_lower)))) {
    return("TEXT")
  }
  
  # Force specific columns to REAL (decimal numbers)  
  if (any(sapply(FORCE_REAL_COLUMNS, function(x) grepl(x, col_name_lower)))) {
    return("REAL")
  }
  
  # Auto-detect for other columns
  if (is.numeric(sample_data) && all(sample_data == as.integer(sample_data), na.rm = TRUE)) {
    return("INTEGER")
  } else if (is.numeric(sample_data)) {
    return("REAL")
  } else {
    return("TEXT")
  }
}

# Function to clean column names for SQLite
clean_column_name <- function(name) {
  # Replace spaces and special characters with underscores
  cleaned <- gsub("[^A-Za-z0-9_]", "_", name)
  # Remove multiple consecutive underscores
  cleaned <- gsub("_+", "_", cleaned)
  # Remove leading/trailing underscores
  cleaned <- gsub("^_|_$", "", cleaned)
  return(cleaned)
}

# Function to build CREATE TABLE SQL
build_create_table_sql <- function(data, table_name) {
  
  columns <- names(data)
  column_defs <- character(length(columns))
  
  cat("📋 Column types for", table_name, ":\n")
  
  for (i in seq_along(columns)) {
    original_name <- columns[i]
    clean_name <- clean_column_name(original_name)
    
    # Get sample data for type detection
    sample_data <- data[[original_name]][!is.na(data[[original_name]])]
    
    # Determine SQL type
    sql_type <- get_sql_type(original_name, sample_data)
    
    column_defs[i] <- glue("`{clean_name}` {sql_type}")
    
    cat(sprintf("   %-30s -> %-30s (%s)\n", original_name, clean_name, sql_type))
  }
  
  cat("\n")
  
  return(glue("CREATE TABLE IF NOT EXISTS `{table_name}` (\n  {paste(column_defs, collapse = ',\n  ')}\n);"))
}

# Function to import single Excel sheet
import_excel_sheet <- function(excel_path, sheet_name, table_name = NULL, db_path) {
  
  if (is.null(table_name)) {
    table_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
  }
  
  cat("📄 Importing sheet:", sheet_name, "->", table_name, "\n")
  
  # Read Excel sheet
  tryCatch({
    # Read all columns as text first to preserve formatting
    data <- readxl::read_excel(
      path = excel_path,
      sheet = sheet_name,
      col_types = "text",
      .name_repair = "universal"
    )
    
    # Clean column names
    original_names <- names(data)
    names(data) <- sapply(original_names, clean_column_name)
    
    # Connect to SQLite
    conn <- dbConnect(RSQLite::SQLite(), db_path)
    on.exit(dbDisconnect(conn))
    
    # Drop table if it exists (for fresh import)
    dbExecute(conn, glue("DROP TABLE IF EXISTS `{table_name}`;"))
    
    # Create table with proper types
    create_sql <- build_create_table_sql(data, table_name)
    dbExecute(conn, create_sql)
    
    # Convert columns back to proper R types for insertion
    for (col_name in names(data)) {
      original_col <- original_names[which(sapply(original_names, clean_column_name) == col_name)[1]]
      sql_type <- get_sql_type(original_col, data[[col_name]])
      
      if (sql_type == "INTEGER") {
        data[[col_name]] <- as.integer(data[[col_name]])
      } else if (sql_type == "REAL") {
        data[[col_name]] <- as.numeric(data[[col_name]])
      }
      # Keep as character/TEXT for everything else
    }
    
    # Insert data
    dbWriteTable(conn, table_name, data, append = TRUE, row.names = FALSE)
    
    cat("✅ Successfully imported", nrow(data), "rows,", ncol(data), "columns\n\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error importing", sheet_name, ":", e$message, "\n\n")
    return(FALSE)
  })
}

# Function to import entire Excel workbook
import_entire_workbook <- function(excel_path, db_path) {
  
  cat("📊 IMPORTING ENTIRE WORKBOOK\n")
  cat("="*50, "\n")
  cat("Excel file:", excel_path, "\n")
  cat("Database:", db_path, "\n\n")
  
  # Get all sheet names
  sheet_names <- readxl::excel_sheets(excel_path)
  cat("Found", length(sheet_names), "sheets:\n")
  for (i in seq_along(sheet_names)) {
    cat(sprintf("  %2d. %s\n", i, sheet_names[i]))
  }
  cat("\n")
  
  # Import each sheet
  success_count <- 0
  for (sheet_name in sheet_names) {
    if (import_excel_sheet(excel_path, sheet_name, NULL, db_path)) {
      success_count <- success_count + 1
    }
  }
  
  cat("🎉 IMPORT COMPLETE!\n")
  cat("✅ Successfully imported", success_count, "/", length(sheet_names), "sheets\n")
  
  # Verify database
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  
  tables <- dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table'")
  cat("✅ Database now contains", nrow(tables), "tables\n\n")
  
  return(success_count)
}

# ===========================================
# Data Verification Functions
# ===========================================

# Function to check leading zeros are preserved
verify_padding <- function(db_path) {
  cat("🔍 VERIFYING LEADING ZERO PRESERVATION\n")
  cat("="*40, "\n")
  
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  
  # Check each table for department/GL number columns
  tables <- dbGetQuery(conn, "SELECT name FROM sqlite_master WHERE type='table'")
  
  for (table_name in tables$name) {
    tryCatch({
      # Get column info
      columns <- dbGetQuery(conn, glue("PRAGMA table_info(`{table_name}`)"))
      
      # Check for our target columns
      dept_cols <- columns$name[grepl("department", tolower(columns$name))]
      gl_cols <- columns$name[grepl("gl_number", tolower(columns$name))]
      
      if (length(dept_cols) > 0 || length(gl_cols) > 0) {
        cat("\n📋 Table:", table_name, "\n")
        
        if (length(dept_cols) > 0) {
          for (col in dept_cols) {
            sample <- dbGetQuery(conn, glue("SELECT `{col}` FROM `{table_name}` WHERE `{col}` IS NOT NULL LIMIT 5"))
            if (length(sample[[1]]) > 0) {
              sample_subset <- sample[[1]][seq_len(min(3, length(sample[[1]])))]
              cat("   Department column '", col, "': ", paste(sample_subset, collapse = ", "), "\n")
            }
          }
        }
        
        if (length(gl_cols) > 0) {
          for (col in gl_cols) {
            sample <- dbGetQuery(conn, glue("SELECT `{col}` FROM `{table_name}` WHERE `{col}` IS NOT NULL LIMIT 5"))
            if (length(sample[[1]]) > 0) {
              sample_subset <- sample[[1]][seq_len(min(3, length(sample[[1]])))]
              cat("   GL column '", col, "': ", paste(sample_subset, collapse = ", "), "\n")
            }
          }
        }
      }
    }, error = function(e) {
      cat("❌ Error checking", table_name, "\n")
    })
  }
}

# ===========================================
# Usage Examples
# ===========================================

# Example 1: Import specific sheet
# import_excel_sheet(EXCEL_PATH, "as_of_6_30_24", "as_of_6_30_24", DB_PATH)

# Example 2: Import entire workbook
# import_entire_workbook(EXCEL_PATH, DB_PATH)

# Example 3: Verify results
# verify_padding(DB_PATH)

cat("🚀 HOLLY BRONZE EXCEL IMPORTER READY!\n")
cat("="*50, "\n")
cat("Available functions:\n")
cat("• import_excel_sheet(excel_path, sheet_name, table_name, db_path)\n")
cat("• import_entire_workbook(excel_path, db_path)\n")
cat("• verify_padding(db_path)\n")
cat("\nTo use:\n")
cat("1. Update EXCEL_PATH to point to your Excel file\n")
cat("2. Run: import_entire_workbook(EXCEL_PATH, DB_PATH)\n")
cat("3. Run: verify_padding(DB_PATH)\n")
cat("\n⚠️  This will recreate your database with proper column types!\n")
