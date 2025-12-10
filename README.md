# Holly Budget Analysis Project

This project analyzes budget data by importing Excel files into SQLite and performing various financial analyses.

## Setup Instructions

### Prerequisites
- R (version 4.0 or higher)
- Required R packages (will be installed automatically when you run the script)

### Installation Steps

1. **Clone this repository**
   ```bash
   git clone [your-repo-url]
   cd R-Projects
   ```

2. **Install required R packages**
   The script will automatically install missing packages, but you can also install them manually:
   ```r
   install.packages(c("DBI", "RSQLite", "dplyr", "dbplyr", "glue", "knitr", "readxl", "openxlsx"))
   ```

3. **Add your data file**
   - Create a `data` directory in the project root
   - Place your Excel file (`Holly_data_bronze.xlsx`) in the `data` directory
   
4. **Run the analysis**
   ```r
   source("holly_sql_r_core")
   ```

## File Structure
```
R-Projects/
├── holly_sql_r_core          # Main analysis script
├── data/                     # Data directory (create this)
│   └── Holly_data_bronze.xlsx # Your Excel file (add this)
├── holly_bronze_r.db         # Generated SQLite database
└── README.md                 # This file
```

## Key Functions

- `import_all_sheets()` - Imports all Excel sheets to SQLite
- `query()` - Execute SELECT queries
- `execute_sql()` - Execute DDL/DML statements
- `export_to_excel()` - Export results to Excel
- `list_tables()` - Show all database tables
- `describe_table()` - Show table structure

## Usage Examples

```r
# List all tables in the database
list_tables()

# Query budget data
result <- query("SELECT * FROM budget_final WHERE fund_number = '101' LIMIT 10")

# Export results to Excel
export_to_excel("budget_final", "my_analysis.xlsx")
```

## Collaboration Notes

- The script uses relative paths for better collaboration
- Large files (Excel, database) are gitignored but can be shared via cloud storage
- All collaborators should place the Excel file in the `data/` directory
- The database will be recreated automatically when the script runs

## Troubleshooting

1. **"File not found" errors**: Make sure `Holly_data_bronze.xlsx` is in the `data/` directory
2. **Package installation issues**: Run `install.packages()` manually for any missing packages
3. **Database connection errors**: The script will recreate the database automatically
