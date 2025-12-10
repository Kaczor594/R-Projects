"""
Holly Bronze Database SQL Query Interface
========================================
A clean interface for querying and manipulating data in holly_bronze.db

Available tables:
- as_of_6-30-21 through as_of_6-30-25
- capital_assets
- critical_road_improvements_1, critical_road_improvements_2
- debt_schedule tables (2015_go_bond, cap_imp_2021, etc.)
- general_25-26, local_street_25-26, major_street_25-26
- refuse_25-26, sewer_25-26, water_25-26
- rate_schedule
- rowe_cip tables (funding_expenditures, projects_status, summary_of_projects)
- history_register_data, lake_improvement_25-26
- water_pumped tables (2020-2021 through 2024-2025)
"""

import sqlite3
import pandas as pd
from pathlib import Path

# Database connection
DB_PATH = "holly_bronze.db"

def get_connection():
    """Get database connection."""
    if not Path(DB_PATH).exists():
        raise FileNotFoundError(f"Database {DB_PATH} not found. Make sure it exists in the current directory.")
    return sqlite3.connect(DB_PATH)

def query(sql, params=None):
    """
    Execute a SQL query and return results as a pandas DataFrame.
    
    Args:
        sql (str): SQL query string
        params (tuple, optional): Parameters for parameterized queries
    
    Returns:
        pd.DataFrame: Query results
    """
    with get_connection() as conn:
        return pd.read_sql(sql, conn, params=params or ())

def execute(sql, params=None):
    """
    Execute a SQL statement (INSERT, UPDATE, DELETE, etc.) without returning results.
    
    Args:
        sql (str): SQL statement
        params (tuple, optional): Parameters for parameterized queries
    
    Returns:
        int: Number of affected rows
    """
    with get_connection() as conn:
        cursor = conn.cursor()
        cursor.execute(sql, params or ())
        conn.commit()
        return cursor.rowcount

def show_tables():
    """List all tables in the database."""
    sql = "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name;"
    return query(sql)

def describe_table(table_name):
    """Show the structure of a specific table."""
    sql = f"PRAGMA table_info({table_name});"
    return query(sql)

def table_summary(table_name):
    """Get basic statistics about a table."""
    info = describe_table(table_name)
    count_sql = f"SELECT COUNT(*) as row_count FROM `{table_name}`"
    count = query(count_sql)
    
    print(f"\n=== Table: {table_name} ===")
    print(f"Rows: {count.iloc[0]['row_count']}")
    print(f"Columns: {len(info)}")
    print("\nColumn Details:")
    print(info[['name', 'type', 'notnull', 'pk']].to_string(index=False))
    
    return info

# ============================================================================
# SAMPLE QUERIES - Modify these or add your own
# ============================================================================

def sample_queries():
    """Example queries to get you started."""
    
    print("=== SAMPLE QUERIES ===\n")
    
    # 1. Critical Road Improvements Overview
    print("1. Critical Road Improvements - Top 10 Most Expensive:")
    q1 = """
    SELECT segment_name, proposed_treatment, estimated_cost, current_rating
    FROM critical_road_improvements_1 
    ORDER BY estimated_cost DESC 
    LIMIT 10;
    """
    print(query(q1))
    print("\n" + "="*50 + "\n")
    
    # 2. Water Pumped Analysis (latest available)
    print("2. Recent Water Pumped Data (2024-2025):")
    try:
        q2 = """
        SELECT * 
        FROM `water_pumped_2024-2025` 
        LIMIT 10;
        """
        print(query(q2))
    except Exception as e:
        print(f"Could not query water_pumped_2024-2025: {e}")
    print("\n" + "="*50 + "\n")
    
    # 3. Budget Summary (if general budget data is available)
    print("3. General Budget Overview (2025-26):")
    try:
        q3 = """
        SELECT * 
        FROM `general_25-26` 
        LIMIT 10;
        """
        print(query(q3))
    except Exception as e:
        print(f"Could not query general_25-26: {e}")

# ============================================================================
# CUSTOM QUERY FUNCTIONS - Add your specific analyses here
# ============================================================================

def road_improvement_analysis():
    """Analyze road improvement costs and priorities."""
    sql = """
    SELECT 
        current_rating,
        COUNT(*) as segment_count,
        AVG(estimated_cost) as avg_cost,
        SUM(estimated_cost) as total_cost,
        MIN(estimated_cost) as min_cost,
        MAX(estimated_cost) as max_cost
    FROM critical_road_improvements_1 
    WHERE estimated_cost IS NOT NULL
    GROUP BY current_rating
    ORDER BY current_rating;
    """
    return query(sql)

def water_usage_trends():
    """Compare water usage across available years."""
    # This is a template - adjust based on actual column names in water_pumped tables
    tables = ['water_pumped_2020-2021', 'water_pumped_2021-2022', 
              'water_pumped_2022-2023', 'water_pumped_2023-2024', 'water_pumped_2024-2025']
    
    results = {}
    for table in tables:
        try:
            sql = f"SELECT COUNT(*) as records FROM `{table}`"
            results[table] = query(sql)
        except Exception as e:
            print(f"Could not query {table}: {e}")
    
    return results

def budget_comparison():
    """Compare budget data across years (as_of tables)."""
    tables = ['as_of_6-30-21', 'as_of_6-30-22', 'as_of_6-30-23', 'as_of_6-30-24', 'as_of_6-30-25']
    
    for table in tables:
        try:
            print(f"\n=== {table} ===")
            # Get first few rows to understand structure
            sql = f"SELECT * FROM `{table}` LIMIT 5"
            result = query(sql)
            print(result)
        except Exception as e:
            print(f"Could not query {table}: {e}")

# ============================================================================
# MAIN EXECUTION
# ============================================================================

if __name__ == "__main__":
    print("Holly Bronze Database Query Interface")
    print("=====================================\n")
    
    # Show available tables
    print("Available tables:")
    tables = show_tables()
    for table in tables['name']:
        print(f"  - {table}")
    
    print(f"\nTotal tables: {len(tables)}")
    print("\n" + "="*50)
    
    # Run sample queries
    sample_queries()
    
    print("\n" + "="*50)
    print("CUSTOM ANALYSIS FUNCTIONS:")
    print("- road_improvement_analysis()")
    print("- water_usage_trends()")
    print("- budget_comparison()")
    print("- table_summary('table_name')")
    print("- describe_table('table_name')")
    
    print("\nTo run custom queries, use:")
    print("result = query('SELECT * FROM table_name LIMIT 10')")
