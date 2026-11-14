# Stock Analysis Studio

An interactive stock market analysis and financial modelling platform built using **R and Shiny**. Stock Analysis Studio combines market data, statistical analysis, technical indicators, forecasting techniques, and valuation models to help users explore stocks and evaluate companies.

## Features

- **Stock Market Overview:** Search companies and analyse historical stock prices using Yahoo Finance data.
- **Technical Analysis:** Explore technical indicators and price trends.
- **Risk Analysis:** Evaluate volatility, returns, and statistical measures.
- **Strategy Backtesting:** Test trading strategies against historical market data.
- **Peer Comparison:** Compare companies based on market and financial performance.
- **Stock Forecasting:** Generate ARIMA forecasts and Monte Carlo simulations.
- **Financial Analysis:** Import and edit financial statements, ratios, and company data.
- **DCF Valuation:** Estimate intrinsic company value using discounted cash flow analysis.
- **Financial Forecasting:** Build operating forecasts and evaluate future performance.
- **Stock Screener:** Explore stocks based on selected financial criteria.
- **Portfolio Tracking:** Monitor and manage stock portfolios.
- **Reports and Exports:** Export analysis and financial data for further use.

## Technology Stack

- **Language:** R
- **Framework:** Shiny
- **Market Data:** Yahoo Finance
- **Financial Analysis:** quantmod, TTR
- **Forecasting:** forecast
- **Statistical Analysis:** tseries, FinTS
- **Data Processing:** dplyr, xts, zoo
- **Visualisation:** ggplot2
- **Tables:** DT
- **Export:** writexl

## Installation and Setup

### 1. Clone the repository

```bash
git clone https://github.com/your-username/stock-analysis-studio.git
```

### 2. Open the project in RStudio

Navigate to the project folder and open the R application file.

### 3. Install required packages

```r
packages <- c(
  "shiny", "bslib", "quantmod", "TTR",
  "xts", "zoo", "dplyr", "ggplot2",
  "DT", "httr", "jsonlite", "forecast",
  "tseries", "FinTS", "writexl"
)

install.packages(packages)
```

### 4. Run the application

```r
shiny::runApp()
```

## Project Objective

The objective of Stock Analysis Studio is to bring multiple financial analysis techniques into one interactive platform. It combines stock market research, financial statement analysis, forecasting, and valuation methods to make financial analysis more accessible and structured.

## Disclaimer

This project is developed for educational and research purposes only. Market data is sourced through Yahoo Finance, and availability or accuracy may vary. Forecasts and valuation outputs are estimates, not guaranteed results. This platform does not provide financial or investment advice.

## Author

**Pranav Arekar**

Developed as a project combining financial analysis, business analytics, and R programming.
