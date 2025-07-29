# Google Sheets Setup for Visitor Logs (FREE)

This guide explains how to use Google Sheets to store visitor logs instead of local CSV files. This is completely **FREE** and only requires a Google account.

## Prerequisites

1. **Google Account**: Any Gmail or Google account
2. **R Package**: Install `googlesheets4` package
3. **Google Sheet**: Create a new Google Sheet

## Setup Steps

### 1. Install Required R Package

```r
install.packages("googlesheets4")
```

### 2. Create Google Sheet

1. Go to [Google Sheets](https://sheets.google.com)
2. Click "Blank" to create a new sheet
3. Rename the sheet to something like "Visitor Analytics"
4. Copy the **Sheet ID** from the URL:
   ```
   https://docs.google.com/spreadsheets/d/[SHEET_ID]/edit#gid=0
   ```

### 3. Set Environment Variable

Set the Google Sheets ID as an environment variable:

**For Local Development:**
```bash
export GOOGLE_SHEETS_ID="your_sheet_id_here"
```

**For R Sessions:**
```r
Sys.setenv(GOOGLE_SHEETS_ID = "your_sheet_id_here")
```

**For Production:**
Add to your server environment variables or `.Renviron` file:
```
GOOGLE_SHEETS_ID=your_sheet_id_here
```

### 4. Authentication Options

#### Option A: Interactive Authentication (Local Development)
- First time running locally, it will open browser for Google authentication
- Credentials are cached automatically
- Perfect for development and testing

#### Option B: Service Account (Production Servers)
1. Go to [Google Cloud Console](https://console.cloud.google.com)
2. Create a new project or select existing
3. Enable Google Sheets API
4. Create Service Account:
   - IAM & Admin → Service Accounts → Create Service Account
   - Download JSON key file
5. Share your Google Sheet with the service account email
6. Set environment variable:
   ```bash
   export GOOGLE_APPLICATION_CREDENTIALS="/path/to/service-account.json"
   ```

### 5. Test Configuration

```r
library(googlesheets4)

# Test authentication (interactive)
gs4_auth()

# Test sheet access
sheet_id <- "your_sheet_id_here"
gs4_get(sheet_id)

# Test writing data
test_data <- data.frame(
  timestamp = Sys.time(),
  test = "Hello World"
)
sheet_write(test_data, ss = sheet_id, sheet = "visitor_logs")
```

## Usage

### Manual Sheet Setup (Optional)
If you want to pre-create the sheet structure:

1. Create a new tab named `visitor_logs`
2. Add headers in row 1:
   ```
   timestamp | ip_address | country | region | city | timezone | user_agent | session_id
   ```

The application will automatically create this structure if it doesn't exist.

### Sharing and Permissions

**For Service Account:**
1. Share your Google Sheet with the service account email
2. Give "Editor" permissions

**For Personal Use:**
- Keep sheet private to your Google account
- Share with specific users if needed

## Benefits

✅ **Completely FREE** - No charges ever  
✅ **15GB Storage** - Part of Google Drive quota  
✅ **Web Accessible** - View data in browser  
✅ **Real-time Updates** - See visitor logs live  
✅ **Collaborative** - Share with team members  
✅ **Backup Included** - Google's reliability  
✅ **Export Options** - Download as CSV, Excel, etc.  
✅ **No Server Setup** - Works immediately  

## Limitations

- **Rate Limits**: 300 requests per minute per project
- **Sheet Size**: 10 million cells per sheet (plenty for visitor logs)
- **Concurrent Users**: 100 simultaneous connections

For typical visitor logs, these limits are never reached.

## Fallback Behavior

The application includes automatic fallback:
- **Primary**: Uses Google Sheets if configured and accessible
- **Fallback**: Uses local CSV file if Sheets unavailable
- **Graceful**: No application downtime if Sheets fails

## Troubleshooting

### Common Issues:

1. **Sheet ID Not Found**
   ```r
   # Check if sheet ID is correct
   gs4_get("your_sheet_id_here")
   ```

2. **Permission Denied**
   - Ensure sheet is shared with service account email
   - Check service account has "Editor" permissions

3. **Authentication Failed**
   ```r
   # Clear cached credentials and re-authenticate
   gs4_deauth()
   gs4_auth()
   ```

4. **Package Installation Issues**
   ```r
   # Install with dependencies
   install.packages("googlesheets4", dependencies = TRUE)
   
   # Or try development version
   remotes::install_github("tidyverse/googlesheets4")
   ```

### Debug Mode:
```r
# Enable verbose logging
options(gargle_verbosity = "debug")
```

## Security Considerations

1. **Sheet Visibility**: Keep sheets private or share selectively
2. **Service Account Keys**: Never commit to version control
3. **IP Logging**: Consider privacy implications of logging visitor IPs
4. **Data Retention**: Implement data cleanup policies if needed

## Example Environment Setup

Create a `.Renviron` file in your project:
```
GOOGLE_SHEETS_ID=1abc123def456ghi789jkl0mn_example_sheet_id
```

Or set in your R session:
```r
# At the top of your app.R or in global environment
Sys.setenv(GOOGLE_SHEETS_ID = "your_actual_sheet_id_here")
```

## Cost: $0.00 Forever! 💰

Google Sheets is completely free for personal and small business use, making it the perfect solution for visitor analytics.