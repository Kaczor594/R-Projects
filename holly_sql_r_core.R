library(DBI)        # Database interface
library(RSQLite)    # SQLite driver for R
library(dplyr)      # Data manipulation
library(dbplyr)     # Database backend for dplyr
library(glue)       # String interpolation
library(knitr)      # Pretty table printing
library(readxl)     # Read Excel files

excel_path <- "/Users/isaackaczor/Downloads/Holly_data_bronze.xlsx"
DB_PATH <- "/Users/isaackaczor/VS-Code/R-Projects/holly_bronze_r.db"

# Helper function to build CREATE TABLE SQL
build_create_table_sql <- function(data, table_name, col_types) {
  
  columns <- names(data)
  column_defs <- character(length(columns))
  
  for (i in seq_along(columns)) {
    col_name <- columns[i]
    
    # Determine SQL type
    if (col_name %in% names(col_types)) {
      sql_type <- switch(col_types[[col_name]],
                        "text" = "TEXT",
                        "integer" = "INTEGER", 
                        "real" = "REAL",
                        "TEXT")  # Default to TEXT
    } else {
      # Auto-detect type, but prefer TEXT for safety
      sql_type <- "TEXT"
    }
    
    column_defs[i] <- glue("`{col_name}` {sql_type}")
  }
  
  glue("CREATE TABLE IF NOT EXISTS `{table_name}` (\n  {paste(column_defs, collapse = ',\n  ')}\n);")
}

# Function to import Excel with controlled column types
import_excel_to_sqlite <- function(excel_file, sheet_name, table_name, db_path) {
  
  # Define column types for your specific needs
  col_types <- list(
    department_number = "text",  # Force to text to preserve leading zeros
    gl_number = "text",        # Force to text to preserve leading zeros
    fund_number = "text",       # Force to text to preserve leading zeros
    account_number = "text"     # Force to text to preserve leading zeros
  )
  
  # Read Excel with specified column types
  data <- readxl::read_excel(
    path = excel_file,
    sheet = sheet_name,
    col_types = "text",  # Read everything as text initially
    .name_repair = "universal"  # Fix column names
  )
  
  # Connect to SQLite
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn))
  
  # Create table with proper column types
  create_table_sql <- build_create_table_sql(data, table_name, col_types)
  dbExecute(conn, create_table_sql)
  
  # Insert data
  dbWriteTable(conn, table_name, data, append = TRUE, row.names = FALSE)
  
  cat("✅ Imported", nrow(data), "rows to", table_name, "\n")
}

# Function to import ALL sheets from Excel workbook
import_all_sheets <- function(excel_file, db_path) {
  cat("📊 IMPORTING ALL SHEETS FROM:", excel_file, "\n")
  cat("🎯 Target database:", db_path, "\n\n")
  
  # Get all sheet names
  sheet_names <- readxl::excel_sheets(excel_file)
  cat("📋 Found", length(sheet_names), "sheets:\n")
  for (i in seq_along(sheet_names)) {
    cat(sprintf("  %2d. %s\n", i, sheet_names[i]))
  }
  cat("\n")
  
  # Import each sheet
  success_count <- 0
  for (sheet_name in sheet_names) {
    tryCatch({
      # Clean sheet name to make valid table name
      table_name <- gsub("[^A-Za-z0-9_]", "_", sheet_name)
      table_name <- gsub("_+", "_", table_name)  # Remove multiple underscores
      table_name <- gsub("^_|_$", "", table_name)  # Remove leading/trailing underscores
      
      cat("📄 Importing:", sheet_name, "->", table_name, "\n")
      
      # Import the sheet
      import_excel_to_sqlite(excel_file, sheet_name, table_name, db_path)
      success_count <- success_count + 1
      
    }, error = function(e) {
      cat("❌ Error importing", sheet_name, ":", e$message, "\n")
    })
  }
  
  cat("\n🎉 IMPORT COMPLETE!\n")
  cat("✅ Successfully imported", success_count, "/", length(sheet_names), "sheets\n\n")
  
  return(success_count)
}

# ===========================================
# Excel Import Complete - Ready to Use!
# ==========================================


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
  for (i in seq_len(nrow(tables))) {
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

# ===========================================
# Main Execution
# ===========================================

# Uncomment the line below to import ALL sheets from Excel:
# Force fresh import - delete existing database
if (file.exists(DB_PATH)) {
  file.remove(DB_PATH)
  cat("🗑️  Deleted existing database to force fresh import\n")
}

# Now import fresh data
import_all_sheets(excel_path, DB_PATH)


query("PRAGMA table_info(budget_final);")
query("SELECT * FROM as_of_6_30_21 LIMIT 5")

#table_name <- "as_of_6_30_24"

# Step 1: Create the temp table (use execute_sql for CREATE statements)
execute_sql("UPDATE general_25_26 SET gl_number = '101-205-000.000' WHERE gl_number = '205.101.000.000';")
execute_sql("UPDATE as_of_6_30_21 SET gl_number = '101-250-818.005' WHERE gl_number = '101-250-490.005';")
execute_sql("UPDATE as_of_6_30_23 SET gl_number = '101-250-818.005' WHERE gl_number = '101-250-490.005';")
execute_sql("UPDATE as_of_6_30_24 SET gl_number = '818.005' WHERE fund_number = '101' AND department_number = '250' AND gl_number = '490.005';")
execute_sql("UPDATE as_of_6_30_24 SET department_number = '250' WHERE fund_number = '101' AND department_number = '250' AND gl_number = '490.005';")
execute_sql("UPDATE as_of_6_30_24 SET gl_number = '101' WHERE fund_number = '101' AND department_number = '250' AND gl_number = '490.005';")
execute_sql("UPDATE as_of_6_30_25 SET gl_number = '818.005' WHERE fund_number = '101' AND department_number = '250' AND gl_number = '490.005';")
execute_sql("UPDATE as_of_6_30_25 SET department_number = '250' WHERE fund_number = '101' AND department_number = '250' AND gl_number = '490.005';")
execute_sql("UPDATE as_of_6_30_25 SET gl_number = '101' WHERE fund_number = '101' AND department_number = '250' AND gl_number = '490.005';")
execute_sql("UPDATE general_25_26 SET gl_number = '101-250-818.005' WHERE gl_number = '101-250-490.005';")

execute_sql("DROP TABLE IF EXISTS six_budget;")
execute_sql("CREATE TABLE six_budget 
AS 
SELECT COALESCE(general.department_name, major_street.department_name, local_street.department_name, lake_improvement.department_name, refuse.department_name, sewer.department_name, water.department_name) as department_name,
                COALESCE(general.gl_number, major_street.gl_number, local_street.gl_number, lake_improvement.gl_number, refuse.gl_number, sewer.gl_number, water.gl_number) as gl_number,
                COALESCE(general.description, major_street.description, local_street.description, lake_improvement.description, refuse.description, sewer.description, water.description) as description,
                COALESCE(general.`..2025_26_recommended_budget`, major_street.`..2025_26_recommended_budget`, local_street.`..2025_26_recommended_budget`, lake_improvement.`..2025_26_recommended_budget`, refuse.`..2025_26_recommended_budget`, sewer.`..2025_26_recommended_budget`, water.`..2025_26_recommended_budget`) as amount,
                COALESCE(general.fund_name, major_street.fund_name, local_street.fund_name, lake_improvement.fund_name, refuse.fund_name, sewer.fund_name, water.fund_name) as budget_fund_name
FROM (SELECT *, 'General Fund' as fund_name FROM general_25_26) general
FULL OUTER JOIN (SELECT *, 'Major Street Fund' as fund_name FROM major_street_25_26) major_street ON general.gl_number = major_street.gl_number
FULL OUTER JOIN (SELECT *, 'Local Street Fund' as fund_name FROM local_street_25_26) local_street ON general.gl_number = local_street.gl_number
FULL OUTER JOIN (SELECT *, 'Lake Improvement Fund' as fund_name FROM lake_improvement_25_26) lake_improvement ON general.gl_number = lake_improvement.gl_number
FULL OUTER JOIN (SELECT *, 'Refuse Fund' as fund_name FROM refuse_25_26) refuse ON general.gl_number = refuse.gl_number
FULL OUTER JOIN (SELECT *, 'Sewer Fund' as fund_name FROM sewer_25_26) sewer ON general.gl_number = sewer.gl_number
FULL OUTER JOIN (SELECT *, 'Water Fund' as fund_name FROM water_25_26) water ON general.gl_number = water.gl_number")
execute_sql("DROP TABLE IF EXISTS four;")
execute_sql("CREATE TABLE four 
AS
SELECT *, 
fund_number || '-' || department_number || '-' || gl_number as gl_full
FROM as_of_6_30_24;")
execute_sql("DROP TABLE IF EXISTS five;")
execute_sql("CREATE TABLE five 
AS
SELECT *, 
fund_number || '-' || department_number || '-' || gl_number as gl_full
FROM as_of_6_30_25;")

execute_sql("DROP TABLE IF EXISTS budget_data;")
execute_sql("CREATE TABLE budget_data
AS
WITH unified as (SELECT --COALESCE(one.fund_name, two.fund_name, three.fund_name, four.fund_name, five.fund_name, six.budget_fund_name) as fund_name,
                --COALESCE(one.department_name, two.department_name, three.department_name, four.department_name, five.department_name, six.department_name) as department_name,
                COALESCE(one.gl_number, two.gl_number, three.gl_number, four.gl_full, five.gl_full, six.gl_number) as gl_number,
                --COALESCE(one.description, two.description, three.description, four.description, five.description, six.description) as description,
                sum(COALESCE(CAST(one.as_of_06_30_2021 AS REAL), 0)) as amount_2021,
                sum(COALESCE(CAST(two.as_of_06_30_2022 AS REAL), 0)) as amount_2022,
                sum(COALESCE(CAST(three.balance_as_of_06_30_2023 AS REAL), 0)) as amount_2023,
                sum(COALESCE(CAST(four.year_to_date_thru_06_30_24 AS REAL), 0)) as amount_2024,
                sum(COALESCE(CAST(five.year_to_date_thru_06_30_25 AS REAL), 0)) as amount_2025,
                sum(COALESCE(CAST(six.amount AS REAL), 0)) as amount_2026
FROM as_of_6_30_21 one
FULL OUTER JOIN as_of_6_30_22 two ON one.gl_number = two.gl_number
FULL OUTER JOIN as_of_6_30_23 three ON one.gl_number = three.gl_number
FULL OUTER JOIN four ON one.gl_number = four.gl_full
FULL OUTER JOIN five ON one.gl_number = five.gl_full
FULL OUTER JOIN six_budget six ON one.gl_number = six.gl_number
GROUP BY 1
ORDER BY gl_number ASC),
unified_names as (SELECT unified.*,
COALESCE(one.fund_name, two.fund_name, three.fund_name, four.fund_name, five.fund_name, six.budget_fund_name) as fund_name,
COALESCE(one.department_name, two.department_name, three.department_name, four.department_name, five.department_name, six.department_name) as department_name,
COALESCE(one.description, two.description, three.description, four.description, five.description, six.description) as description
FROM unified
LEFT JOIN as_of_6_30_21 one ON unified.gl_number = one.gl_number
LEFT JOIN as_of_6_30_22 two ON unified.gl_number = two.gl_number
LEFT JOIN as_of_6_30_23 three ON unified.gl_number = three.gl_number
LEFT JOIN four ON unified.gl_number = four.gl_full
LEFT JOIN five ON unified.gl_number = five.gl_full
LEFT JOIN six_budget six ON unified.gl_number = six.gl_number)
SELECT *,
SUBSTR(gl_number, 1, 3) as fund_number,
substr(gl_number, 5, 3) as department_number,
substr(gl_number, 9, 7) as object_number,
case when CAST(substr(gl_number, 9, 1) AS INTEGER) < 7 then 'Revenue' else 'Expenditure' end as account_type
FROM unified_names WHERE (amount_2021 + amount_2022 + amount_2023 + amount_2024 + amount_2025 + amount_2026) != 0;")
execute_sql("DROP TABLE IF EXISTS budget_final;")
execute_sql("CREATE TABLE budget_final AS 
SELECT account_type,
fund_number,
department_number,
object_number,
gl_number,
description,
amount_2021,
amount_2022,
amount_2023,
amount_2024,
amount_2025,
amount_2026
FROM budget_data;")

# Step 2: Diagnostic - Check for mixed data types
#query("SELECT DISTINCT year_to_date_thru_06_30_25, typeof(year_to_date_thru_06_30_25) as data_type FROM as_of_6_30_25 WHERE year_to_date_thru_06_30_25 IS NOT NULL LIMIT 10")

# Step 3: Query the temp table (use query for SELECT statements)
query("SELECT *
FROM budget_data
WHERE gl_number == '101-000-439.000';")

query("SELECT gl_full, gl_number, year_to_date_thru_06_30_25
FROM five
WHERE cast(year_to_date_thru_06_30_25 as real) <  2700
AND CAST(substr(gl_full, 9, 1) AS INTEGER) < 7
ORDER BY cast(year_to_date_thru_06_30_25 as real) DESC;")

query("WITH revs as (SELECT sum(year_to_date_thru_06_30_25) as revenue
FROM five
WHERE CAST(substr(gl_full, 9, 1) AS INTEGER) < 7),
exps as (SELECT sum(year_to_date_thru_06_30_25) as expenditure
FROM five
WHERE CAST(substr(gl_full, 9, 1) AS INTEGER) >= 7)
SELECT revs.revenue, exps.expenditure, (revs.revenue - exps.expenditure) as net_budget
FROM revs, exps;")


# ===========================================
# Export Functions
# ===========================================

# Function to export table to Excel
export_to_excel <- function(table_name, output_file = NULL) {
  
  # Install/load required packages
  if (!require(openxlsx, quietly = TRUE)) {
    install.packages("openxlsx")
    library(openxlsx)
  }
  
  # Default output filename
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- glue("holly_{table_name}_{timestamp}.xlsx")
  }
  
  cat("📊 Exporting", table_name, "to Excel...\n")
  
  # Get all data from the table
  data <- query(glue("SELECT * FROM {table_name}"))
  
  # Create Excel workbook
  wb <- createWorkbook()
  addWorksheet(wb, table_name)
  
  # Write data with formatting
  writeData(wb, sheet = 1, data, startRow = 1, startCol = 1, headerStyle = createStyle(
    fontSize = 12,
    fontColour = "white",
    halign = "center",
    fgFill = "#4472C4",
    border = "TopBottomLeftRight",
    textDecoration = "bold"
  ))
  
  # Auto-size columns
  setColWidths(wb, sheet = 1, cols = 1:ncol(data), widths = "auto")
  
  # Add borders to data
  addStyle(wb, sheet = 1, 
           style = createStyle(border = "TopBottomLeftRight"),
           rows = 1:(nrow(data) + 1), cols = 1:ncol(data), gridExpand = TRUE)
  
  # Save the file
  output_path <- file.path(getwd(), output_file)
  saveWorkbook(wb, output_path, overwrite = TRUE)
  
  cat("✅ Excel file saved:", output_path, "\n")
  cat("📋 Exported", nrow(data), "rows and", ncol(data), "columns\n\n")
  
  return(output_path)
}

# Function to export to CSV (simple alternative)
export_to_csv <- function(table_name, output_file = NULL) {
  
  # Default output filename
  if (is.null(output_file)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- glue("holly_{table_name}_{timestamp}.csv")
  }
  
  cat("📊 Exporting", table_name, "to CSV...\n")
  
  # Get all data from the table
  data <- query(glue("SELECT * FROM {table_name}"))
  
  # Save as CSV
  output_path <- file.path(getwd(), output_file)
  write.csv(data, output_path, row.names = FALSE)
  
  cat("✅ CSV file saved:", output_path, "\n")
  cat("📋 Exported", nrow(data), "rows and", ncol(data), "columns\n\n")
  
  return(output_path)
}

# Function to create comprehensive Excel report
#create_budget_report <- function(output_file = "holly_budget_report.xlsx") {
#  
#  if (!require(openxlsx, quietly = TRUE)) {
#    install.packages("openxlsx")
#    library(openxlsx)
#  }
#  
#  cat("📊 Creating comprehensive budget report...\n")
#  
#  # Create workbook
#  wb <- createWorkbook()
#  
#  # Sheet 1: Full unified data
#  cat("   • Adding unified data sheet...\n")
#  detail_data <- query("SELECT * FROM detail")
#  addWorksheet(wb, "Unified_Data")
#  writeData(wb, "Unified_Data", detail_data, headerStyle = createStyle(
#    fontSize = 12, fontColour = "white", halign = "center", 
#    fgFill = "#4472C4", border = "TopBottomLeftRight", textDecoration = "bold"))
#  setColWidths(wb, "Unified_Data", cols = 1:ncol(detail_data), widths = "auto")
#  
#  # Sheet 2: Summary by Fund
#  cat("   • Adding fund summary...\n")
#  fund_summary <- query("
#    SELECT fund_name, 
#           COUNT(*) as line_items,
#           ROUND(SUM(amount_2024), 2) as total_2024,
#           ROUND(SUM(amount_2025), 2) as total_2025,
#           ROUND(SUM(amount_2025) - SUM(amount_2024), 2) as change_24_to_25
#    FROM detail 
#    WHERE fund_name IS NOT NULL 
#    GROUP BY fund_name
#    ORDER BY total_2025 DESC")
#  addWorksheet(wb, "Fund_Summary")
#  writeData(wb, "Fund_Summary", fund_summary, headerStyle = createStyle(
#    fontSize = 12, fontColour = "white", halign = "center",
#    fgFill = "#70AD47", border = "TopBottomLeftRight", textDecoration = "bold"))
#  setColWidths(wb, "Fund_Summary", cols = 1:ncol(fund_summary), widths = "auto")
#  
#  # Sheet 3: Summary by Department
#  cat("   • Adding department summary...\n")
#  dept_summary <- query("
#    SELECT department_name,
#           COUNT(*) as line_items,
#           ROUND(SUM(amount_2024), 2) as total_2024,
#           ROUND(SUM(amount_2025), 2) as total_2025,
#           ROUND(SUM(amount_2025) - SUM(amount_2024), 2) as change_24_to_25
#    FROM detail 
#    WHERE department_name IS NOT NULL 
#    GROUP BY department_name
#    ORDER BY total_2025 DESC")
#  addWorksheet(wb, "Department_Summary")
#  writeData(wb, "Department_Summary", dept_summary, headerStyle = createStyle(
#    fontSize = 12, fontColour = "white", halign = "center",
#    fgFill = "#FFC000", border = "TopBottomLeftRight", textDecoration = "bold"))
#  setColWidths(wb, "Department_Summary", cols = 1:ncol(dept_summary), widths = "auto")
#  
#  # Sheet 4: 5-Year Trends (Top 20 GL accounts)
#  cat("   • Adding trend analysis...\n")
#  trends <- query("
#    SELECT gl_number, description, fund_name,
#           amount_2021, amount_2022, amount_2023, amount_2024, amount_2025
#    FROM detail 
#    WHERE amount_2025 > 0
#    ORDER BY amount_2025 DESC 
#    LIMIT 20")
#  addWorksheet(wb, "Top_20_Trends")
#  writeData(wb, "Top_20_Trends", trends, headerStyle = createStyle(
#    fontSize = 12, fontColour = "white", halign = "center",
#    fgFill = "#E74C3C", border = "TopBottomLeftRight", textDecoration = "bold"))
#  setColWidths(wb, "Top_20_Trends", cols = 1:ncol(trends), widths = "auto")
#  
#  # Save the workbook
#  output_path <- file.path(getwd(), output_file)
#  saveWorkbook(wb, output_path, overwrite = TRUE)
#  
#  cat("✅ Comprehensive report saved:", output_path, "\n")
#  cat("📊 Report contains", length(names(wb)), "worksheets\n\n")
#  
#  return(output_path)
#}


# Export options - choose one:

# Option 1: Simple Excel export of budget table  
export_to_excel("budget_final", "holly_budget_5.xlsx")


