# Bee Abundance Analysis App

This Shiny application visualizes the results of Generalized Additive Mixed Models (GAMMs) analyzing bee abundance data.

## Deployment to Posit Connect

### Prerequisites
- A Posit Connect account
- rsconnect R package installed

### Deployment Steps

1. Install the rsconnect package if not already installed:
   ```r
   install.packages("rsconnect")
   ```

2. Set up your Posit Connect account credentials:
   ```r
   rsconnect::setAccountInfo(name="your-account-name",
                          token="your-token",
                          secret="your-secret")
   ```

3. Deploy the app from this directory:
   ```r
   rsconnect::deployApp(appDir = "path/to/bee_abundance_app_deploy",
                      appName = "Bee-Abundance-Analysis",
                      appTitle = "Bee Abundance Analysis")
   ```

## Files Included
- `app.R`: The main Shiny application
- `publication_models.RData`: Pre-fitted GAM models
- `Final_SSO_ESA_Data.xlsx`: Data file for the analysis
- `requirements.txt`: List of R package dependencies

## Application Features
- Interactive visualization of bee abundance patterns
- Model summaries and diagnostics
- Environmental effects analysis
- Seasonal pattern visualization
