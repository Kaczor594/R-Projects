# Holly Budget Analysis - Setup Instructions

## Quick Start for Collaborators

1. **Get the code**: Clone the GitHub repository
2. **Get the data**: Download `Holly_data_bronze.xlsx` from Google Drive (link provided separately)
3. **Setup**: Place the Excel file in a `data/` folder in the project directory
4. **Run**: Execute `source("holly_sql_r_core")` in R

## File Sharing Strategy

### Code (GitHub)
- All R scripts
- Documentation
- Configuration files
- Small reference files

### Data Files (Google Drive)
- `Holly_data_bronze.xlsx` (source data)
- Generated Excel reports
- Large output files

### Generated Files (Local)
- `holly_bronze_r.db` (recreated automatically)
- Temporary analysis files

## Collaboration Workflow

1. **Code changes**: Use GitHub for version control
2. **Data updates**: Share via Google Drive, notify team in GitHub issues
3. **Results sharing**: Export to Excel and share via Google Drive
4. **Discussions**: Use GitHub issues or discussions

## Benefits of This Approach

✅ **Version Control**: Track all code changes  
✅ **Reproducibility**: Anyone can recreate the analysis  
✅ **Flexibility**: Easy to modify for different data sources  
✅ **Documentation**: Clear instructions for new collaborators  
✅ **Scalability**: Can handle larger datasets and more complex analyses  
