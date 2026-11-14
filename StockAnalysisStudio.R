# ============================================================
# STOCK ANALYSIS STUDIO - R SHINY (dark theme, single file)
# Save as: app.R   |   Run in RStudio: open app.R -> Run App
#
# Includes: company/ticker search, overview, technicals, risk and
# statistical tests, backtest, peers, ARIMA + Monte Carlo, editable
# financials and ratios, DCF, operating forecast, screener, portfolio,
# and report/data exports.
#
# Yahoo Finance endpoints are unofficial and may change.
# Educational research tool, not investment advice.
# ============================================================

required_packages <- c(
  "shiny", "bslib", "quantmod", "TTR", "xts", "zoo",
  "dplyr", "ggplot2", "DT", "httr", "jsonlite",
  "forecast", "tseries", "FinTS", "writexl"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))

# Packages loaded later mask these; restore the right versions.
validate <- shiny::validate      # jsonlite::validate otherwise causes "is.character(txt)" errors
need     <- shiny::need
first    <- function(x) head(x, 1)   # dplyr::first/last do not work on xts
last     <- function(x) tail(x, 1)

# ----------------------------- Helpers -----------------------------

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

safe_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  x
}

fetch_yahoo_search <- function(query) {
  url <- "https://query1.finance.yahoo.com/v1/finance/search"
  ans <- tryCatch({
    resp <- httr::GET(
      url, query = list(q = query, quotesCount = 10, newsCount = 0),
      httr::add_headers(`User-Agent` = "Mozilla/5.0"),
      httr::timeout(12)
    )
    httr::stop_for_status(resp)
    js <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))
    qs <- js$quotes
    if (is.null(qs) || !nrow(qs)) return(NULL)
    qs <- qs[qs$quoteType %in% c("EQUITY", "ETF", "MUTUALFUND"), , drop = FALSE]
    if (!nrow(qs)) return(NULL)
    nm <- if ("longname" %in% names(qs)) qs$longname else rep(NA_character_, nrow(qs))
    if ("shortname" %in% names(qs)) nm[is.na(nm) | !nzchar(nm)] <- qs$shortname[is.na(nm) | !nzchar(nm)]
    labels <- paste0(nm, " (", qs$symbol, ")")
    stats::setNames(qs$symbol, labels)
  }, error = function(e) NULL)
  ans
}

fetch_prices <- function(ticker, from, to) {
  ticker <- trimws(ticker)
  if (!nzchar(ticker)) stop("Enter a ticker symbol.")
  x <- tryCatch(
    suppressWarnings(quantmod::getSymbols(
      ticker, src = "yahoo", from = from, to = to + 1,
      auto.assign = FALSE, warnings = FALSE
    )),
    error = function(e) stop("Price download failed: ", conditionMessage(e))
  )
  if (NROW(x) < 2) stop("Not enough price data for this ticker and date range.")
  x <- x[!duplicated(zoo::index(x))]
  x <- x[order(zoo::index(x))]
  x
}

get_adjusted_close <- function(x) {
  a <- tryCatch(quantmod::Ad(x), error = function(e) NULL)
  if (is.null(a) || all(is.na(a))) a <- quantmod::Cl(x)
  a
}

make_returns <- function(x) {
  p <- get_adjusted_close(x)
  r <- na.omit(diff(log(p)))
  r[is.finite(r)]
}

band <- function(x, br, txt) if (length(x) != 1 || !is.finite(x)) "n/a" else txt[findInterval(x, br) + 1]

# Indicators that return NA (instead of erroring) when the history is too short
na_like <- function(x) { y <- x; y[] <- NA_real_; y }
safe_sma  <- function(x, n) if (NROW(x) >= n) TTR::SMA(x, n) else na_like(x)
safe_rsi  <- function(x, n = 14) if (NROW(x) > n) TTR::RSI(x, n) else na_like(x)
safe_macd <- function(x) {
  if (NROW(x) >= 40) return(TTR::MACD(x))
  m <- cbind(na_like(x), na_like(x)); colnames(m) <- c("macd", "signal"); m
}

safe_ratio <- function(a, b) {
  ifelse(is.na(b) | b == 0, NA_real_, a / b)
}

annualised_return <- function(r) {
  r <- as.numeric(na.omit(r))
  if (length(r) < 2) return(NA_real_)
  exp(mean(r) * 252) - 1
}

max_drawdown <- function(r) {
  r <- as.numeric(na.omit(r))
  if (!length(r)) return(NA_real_)
  wealth <- cumprod(1 + (exp(r) - 1))
  min(wealth / cummax(wealth) - 1, na.rm = TRUE)
}

risk_metrics <- function(r, rf_annual = 0.04) {
  r <- as.numeric(na.omit(r))
  if (length(r) < 10) return(data.frame(Metric = "Risk metrics", Value = NA, Interpretation = "Insufficient observations"))
  mu <- mean(r) * 252
  vol <- sd(r) * sqrt(252)
  downside <- sqrt(mean(pmin(r - rf_annual / 252, 0)^2)) * sqrt(252)
  q05 <- as.numeric(stats::quantile(r, 0.05, na.rm = TRUE))
  cvar <- mean(r[r <= q05], na.rm = TRUE)
  vals <- c(
    "Annualised return (%)" = 100 * (exp(mu) - 1),
    "Annualised volatility (%)" = 100 * vol,
    "Sharpe ratio" = ifelse(vol > 0, (mu - rf_annual) / vol, NA),
    "Sortino ratio" = ifelse(downside > 0, (mu - rf_annual) / downside, NA),
    "Maximum drawdown (%)" = 100 * max_drawdown(r),
    "Historical daily VaR 95% (%)" = 100 * q05,
    "Historical daily CVaR 95% (%)" = 100 * cvar,
    "Worst day (%)" = 100 * min(r),
    "Best day (%)" = 100 * max(r)
  )
  interpretation <- c(
    "Historical annualised estimate; not a forecast",
    "Annualised standard deviation of daily log returns",
    "Excess return divided by annualised volatility",
    "Excess return divided by downside deviation",
    "Largest peak-to-trough loss in the sample",
    "Historical 5th percentile daily log return",
    "Average daily log return in the worst 5% tail",
    "Worst observed daily log return",
    "Best observed daily log return"
  )
  data.frame(Metric = names(vals), Value = round(as.numeric(vals), 3), Interpretation = interpretation)
}

financial_template <- function(years = (as.integer(format(Sys.Date(), "%Y")) - 5):(as.integer(format(Sys.Date(), "%Y")) - 1)) {
  data.frame(
    Year = years, Revenue = NA_real_, COGS = NA_real_, EBITDA = NA_real_,
    EBIT = NA_real_, NetIncome = NA_real_, EPS = NA_real_, TotalDebt = NA_real_,
    Cash = NA_real_, Equity = NA_real_, TotalAssets = NA_real_,
    OpCashFlow = NA_real_, Capex = NA_real_, CurrentAssets = NA_real_,
    CurrentLiab = NA_real_, Shares = NA_real_,
    check.names = FALSE
  )
}

financial_ratios <- function(x) {
  x <- x[order(x$Year), , drop = FALSE]
  growth <- function(v) c(NA_real_, (v[-1] / head(v, -1) - 1) * 100)
  fcf <- x$OpCashFlow - abs(x$Capex)
  data.frame(
    Year = x$Year,
    `Revenue growth %` = growth(x$Revenue),
    `Gross margin %` = safe_ratio(x$Revenue - x$COGS, x$Revenue) * 100,
    `EBITDA margin %` = safe_ratio(x$EBITDA, x$Revenue) * 100,
    `EBIT margin %` = safe_ratio(x$EBIT, x$Revenue) * 100,
    `Net margin %` = safe_ratio(x$NetIncome, x$Revenue) * 100,
    `ROE %` = safe_ratio(x$NetIncome, x$Equity) * 100,
    `ROA %` = safe_ratio(x$NetIncome, x$TotalAssets) * 100,
    `Debt / Equity` = safe_ratio(x$TotalDebt, x$Equity),
    `Current ratio` = safe_ratio(x$CurrentAssets, x$CurrentLiab),
    `Net debt` = x$TotalDebt - x$Cash,
    `FCF` = fcf,
    `FCF margin %` = safe_ratio(fcf, x$Revenue) * 100,
    check.names = FALSE
  )
}

dcf_model <- function(fcf0, growth, years, discount, terminal_growth, net_debt, shares) {
  check_inputs <- function() {
    if (!is.finite(fcf0)) stop("Latest free cash flow must be a number.")
    if (discount <= terminal_growth) stop("Discount rate must be higher than terminal growth.")
    if (shares <= 0 || !is.finite(shares)) stop("Shares outstanding must be positive.")
    if (years < 1) stop("Forecast years must be positive.")
  }
  check_inputs()
  t <- seq_len(years)
  fcf <- fcf0 * (1 + growth)^t
  pv <- fcf / (1 + discount)^t
  tv <- fcf[years] * (1 + terminal_growth) / (discount - terminal_growth)
  pv_tv <- tv / (1 + discount)^years
  ev <- sum(pv) + pv_tv
  equity_value <- ev - net_debt
  list(year = t, fcf = fcf, pv = pv, terminal_value = tv,
       pv_terminal = pv_tv, ev = ev, equity_value = equity_value,
       per_share = equity_value / shares,
       terminal_share = ifelse(ev != 0, pv_tv / ev, NA_real_))
}

theme_app <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background = element_rect(fill = "#10162f", colour = NA),
      panel.background = element_rect(fill = "#10162f", colour = NA),
      text = element_text(colour = "#cbd5e1"),
      axis.text = element_text(colour = "#94a3b8"),
      plot.title = element_text(face = "bold", colour = "#e5e7ff"),
      panel.grid.major = element_line(colour = "#232b52"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom", legend.text = element_text(colour = "#cbd5e1")
    )
}
options(ggplot2.discrete.colour = c("#818cf8", "#34d399", "#fbbf24", "#f87171", "#38bdf8", "#c084fc"),
        ggplot2.discrete.fill = c("#818cf8", "#34d399", "#fbbf24", "#f87171", "#38bdf8", "#c084fc"))

# ----------------------------- Stylesheet -----------------------------

app_css <- r"---(
:root{--line:#232b52;--ink:#e5e7ff;--mute:#94a3b8}
body{background:radial-gradient(1200px 600px at 80% -10%,#1b1f58 0%,#0b1020 55%) fixed !important;color:var(--ink)}
.bslib-page-title{background:#0b1020 !important;color:#fff !important;border-bottom:1px solid var(--line);font-weight:600}
.bslib-sidebar-layout>.sidebar{background:#0e1430 !important;border-right:1px solid var(--line) !important}
.sidebar h5{color:#fff;font-weight:600;margin-bottom:14px}
label,.control-label{color:#cbd5e1;font-weight:500}
.form-control,.form-select,.selectize-input{background:#0b1230 !important;border:1px solid #2a3363 !important;color:var(--ink) !important;border-radius:10px !important}
.form-control:focus,.form-select:focus{border-color:#6366f1 !important;box-shadow:0 0 0 .2rem rgba(99,102,241,.25) !important}
.selectize-dropdown{background:#10162f !important;color:var(--ink) !important;border:1px solid #2a3363}
.help-block,.form-text{color:var(--mute) !important}
hr{border-color:var(--line);opacity:1}
.btn-primary,.btn-outline-primary{background:linear-gradient(90deg,#6366f1,#8b5cf6) !important;color:#fff !important;border:0 !important;border-radius:10px;font-weight:600;box-shadow:0 6px 18px rgba(99,102,241,.35)}
.btn-default,.btn-secondary{background:#1f2747 !important;color:var(--ink) !important;border:1px solid #2a3363 !important;border-radius:10px}
div.rounded-4.text-white{background:linear-gradient(110deg,#121744 0%,#2a1f6b 55%,#0f3a6b 100%) !important;position:relative;overflow:hidden;padding:30px 36px !important;border:1px solid #3b3f9e;border-radius:18px !important;box-shadow:0 10px 34px rgba(79,70,229,.28)}
div.rounded-4.text-white>*{position:relative;z-index:1}
div.rounded-4.text-white::after{content:"";position:absolute;inset:0;z-index:0;pointer-events:none;background:url("data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 420 160' preserveAspectRatio='xMaxYMax meet'><defs><linearGradient id='g' x1='0' y1='0' x2='0' y2='1'><stop offset='0' stop-color='%2338bdf8' stop-opacity='.6'/><stop offset='1' stop-color='%236366f1' stop-opacity='0'/></linearGradient></defs><path d='M0 160V122C40 112 70 132 110 102S180 72 220 82S300 42 350 27S400 8 420 0V160Z' fill='url(%23g)'/><g stroke-width='2'><path d='M262 50V95M286 35V80M310 55V100M334 20V70M358 30V75M382 5V50' stroke='%23a78bfa'/><rect x='257' y='62' width='10' height='22' fill='%23a78bfa'/><rect x='281' y='44' width='10' height='26' fill='%2338bdf8'/><rect x='305' y='66' width='10' height='24' fill='%23a78bfa'/><rect x='329' y='30' width='10' height='28' fill='%2338bdf8'/><rect x='353' y='40' width='10' height='24' fill='%23a78bfa'/><rect x='377' y='14' width='10' height='26' fill='%2338bdf8'/></g></svg>") no-repeat right bottom / auto 100%}
div.rounded-4.text-white h2{font-size:2.3rem;font-weight:700;background:linear-gradient(90deg,#fff 40%,#a78bfa);-webkit-background-clip:text;background-clip:text;color:transparent !important}
div.rounded-4.text-white p{color:#cbd5e1;margin:6px 0 0}
.nav-pills{margin-bottom:6px}
.nav-pills .nav-link{color:#a5b4fc;background:rgba(255,255,255,.04);border:1px solid var(--line);border-radius:12px;margin:0 6px 8px 0;font-weight:500;padding:9px 16px}
.nav-pills .nav-link:hover{background:rgba(99,102,241,.15);color:#fff}
.nav-pills .nav-link.active{background:linear-gradient(90deg,#6366f1,#8b5cf6) !important;color:#fff !important;border-color:transparent;box-shadow:0 6px 18px rgba(99,102,241,.45)}
.nav-pills .nav-link i{margin-right:6px}
.tab-content{background:#10162f;border:1px solid var(--line);border-radius:18px;padding:22px;margin-top:6px;box-shadow:0 8px 28px rgba(0,0,0,.35)}
h4{color:#c7d2fe !important;font-weight:600;margin-top:24px}
.kpi{background:linear-gradient(145deg,#141b40,#10162f) !important;border:1px solid var(--line);border-left:4px solid #6366f1;border-radius:14px !important;padding:16px 18px !important;margin:6px 0 !important}
.kpi-value{font-size:26px !important;font-weight:700 !important;color:#fff !important}
.kpi-label{color:var(--mute) !important;font-size:12px;letter-spacing:.06em;text-transform:uppercase}
.k-green{border-left-color:#34d399}.k-red{border-left-color:#f87171}.k-blue{border-left-color:#38bdf8}.k-violet{border-left-color:#a78bfa}
.alert-primary{background:#1b2150;border:1px solid #3b3f9e;color:#dbe0ff;border-radius:12px}
table.table{--bs-table-bg:transparent;--bs-table-color:var(--ink);--bs-table-striped-bg:rgba(255,255,255,.03);--bs-table-hover-bg:rgba(99,102,241,.12);color:var(--ink);width:100%}
table.table th{background:#161d40 !important;color:#a5b4fc !important;font-weight:600;border-bottom:1px solid #2a3363 !important}
table.table td{border-color:var(--line) !important;color:var(--ink) !important}
table.table td:last-child,table.table th:last-child{text-align:left}
table.dataTable tbody tr,table.dataTable tbody td{background:transparent !important;color:var(--ink) !important}
table.dataTable tbody tr:hover td{background:rgba(99,102,241,.12) !important}
table.dataTable thead th{background:#161d40 !important;color:#a5b4fc !important;border-bottom:1px solid #2a3363 !important}
.dataTables_wrapper,.dataTables_info,.dataTables_length label,.dataTables_filter label{color:var(--mute) !important}
.page-link{background:#161d40;border-color:var(--line);color:#c7d2fe}.active>.page-link{background:#6366f1;border-color:#6366f1;color:#fff}
.shiny-plot-output{border-radius:14px;overflow:hidden;border:1px solid var(--line);margin:8px 0}
pre.shiny-text-output{background:#0b1230;color:#c7d2fe;border:1px solid var(--line);border-radius:12px;padding:14px}
::-webkit-scrollbar{width:10px;height:10px}::-webkit-scrollbar-thumb{background:#2a3363;border-radius:8px}
.hero-eyebrow{display:inline-block;background:rgba(255,255,255,.12);border:1px solid rgba(255,255,255,.2);border-radius:999px;padding:4px 14px;font-size:12px;letter-spacing:.08em;text-transform:uppercase;color:#c7d2fe;margin-bottom:14px}
div.rounded-4.text-white.hero{padding:48px 44px !important;min-height:270px}
div.rounded-4.text-white.hero h2{font-size:3rem}
.hero p.lead{max-width:620px;font-size:1.1rem}
.hero-cta{margin-top:24px;display:flex;gap:12px;flex-wrap:wrap}
.cta{border:0;border-radius:12px;padding:12px 26px;font-weight:600;color:#fff;background:linear-gradient(90deg,#6366f1,#8b5cf6);box-shadow:0 8px 22px rgba(99,102,241,.45);cursor:pointer}
.cta.ghost{background:rgba(255,255,255,.08);border:1px solid rgba(255,255,255,.25);box-shadow:none}
.cta:hover{filter:brightness(1.12)}
.stat-strip{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:12px;margin:18px 0}
.stat{background:linear-gradient(145deg,#141b40,#10162f);border:1px solid var(--line);border-radius:14px;padding:14px 18px;text-align:center}
.stat b{display:block;font-size:26px;color:#fff}.stat span{color:var(--mute);font-size:12px;text-transform:uppercase;letter-spacing:.06em}
.chips{display:flex;flex-wrap:wrap;gap:10px;margin:8px 0 6px}
.chip{background:rgba(99,102,241,.14);border:1px solid #3b3f9e;color:#c7d2fe;border-radius:999px;padding:7px 16px;font-weight:600;cursor:pointer}
.chip:hover{background:#6366f1;color:#fff}
.feat-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(250px,1fr));gap:14px;margin:10px 0 6px}
.feat{background:linear-gradient(145deg,#141b40,#10162f);border:1px solid var(--line);border-radius:16px;padding:18px;cursor:pointer;transition:.2s}
.feat:hover{transform:translateY(-3px);border-color:#6366f1;box-shadow:0 10px 26px rgba(99,102,241,.25)}
.feat-ic{width:42px;height:42px;border-radius:12px;display:flex;align-items:center;justify-content:center;background:linear-gradient(135deg,#6366f1,#8b5cf6);color:#fff;font-size:18px;margin-bottom:12px}
.feat h5{color:#fff;font-weight:600;margin:0 0 6px}.feat p{color:var(--mute);font-size:14px;margin:0}
.steps{display:grid;grid-template-columns:repeat(auto-fit,minmax(240px,1fr));gap:14px;margin:10px 0}
.step{border-left:3px solid #6366f1;padding:6px 16px}.step b{color:#fff}.step span{color:var(--mute);display:block;font-size:14px}
.interp-card{background:linear-gradient(145deg,#141b40,#10162f);border:1px solid var(--line);border-radius:16px;padding:6px 20px 14px;margin:14px 0}
.interp-card h4{margin-top:14px}
.interp-list{list-style:none;padding:0;margin:8px 0 0}
.interp-list li{padding:10px 12px;border-radius:10px;margin:6px 0;background:rgba(255,255,255,.03);border-left:4px solid #64748b;color:#dbe0ff;line-height:1.5}
.interp-list li.good{border-left-color:#34d399}.interp-list li.warn{border-left-color:#f87171}.interp-list li.neutral{border-left-color:#818cf8}
.badge-lvl{display:inline-block;min-width:78px;text-align:center;font-size:11px;font-weight:700;letter-spacing:.05em;text-transform:uppercase;border-radius:999px;padding:2px 10px;margin-right:10px}
.good .badge-lvl{background:rgba(52,211,153,.18);color:#34d399}.warn .badge-lvl{background:rgba(248,113,113,.18);color:#f87171}.neutral .badge-lvl{background:rgba(129,140,248,.2);color:#a5b4fc}
details.glossary{margin-top:18px;background:#10162f;border:1px solid var(--line);border-radius:14px;padding:12px 18px}
details.glossary summary{cursor:pointer;color:#c7d2fe;font-weight:600}
details.glossary dt{color:#c7d2fe;margin-top:10px}details.glossary dd{color:var(--mute);margin:0}
.disclaimer{margin-top:22px;padding:12px 16px;border-radius:12px;background:#1a1530;border:1px solid #4c3a7a;color:#c4b5fd;font-size:13px}
)---"

# ----------------------------- Home page -----------------------------

goto <- function(tab) sprintf("Shiny.setInputValue('goto_tab','%s',{priority:'event'})", tab)
feature <- function(ic, title, desc, tab)
  div(class = "feat", onclick = goto(tab), div(class = "feat-ic", icon(ic)), h5(title), p(desc))
chip <- function(tk) tags$button(type = "button", class = "chip", tk,
                                 onclick = sprintf("Shiny.setInputValue('quick_ticker','%s',{priority:'event'})", tk))
cta <- function(label, tab, ghost = FALSE)
  tags$button(type = "button", class = paste("cta", if (ghost) "ghost"), label, onclick = goto(tab))
step <- function(n, title, desc) div(class = "step", tags$b(paste0(n, ". ", title)), span(desc))

home_page <- tagList(
  br(),
  div(class = "p-3 mb-3 rounded-4 text-white hero",
      div(class = "hero-eyebrow", "Research workspace for stocks"),
      h2("Stock Analysis Studio"),
      p(class = "lead", "Search any listed company, choose a date range, and get price analysis, risk metrics, forecasts, valuation and exports in one place."),
      div(class = "hero-cta", cta("Start analysing", "Overview"), cta("Open the screener", "Screener", TRUE))
  ),
  div(class = "stat-strip",
      div(class = "stat", tags$b("13"), span("Analysis modules")),
      div(class = "stat", tags$b("5 yrs"), span("Quick-start history")),
      div(class = "stat", tags$b("3"), span("Export formats")),
      div(class = "stat", tags$b("NSE | BSE | Global"), span("Ticker coverage"))
  ),
  h4("Quick start"),
  p("Click a ticker to load it instantly, or use the search box in the sidebar."),
  div(class = "chips", lapply(c("RELIANCE.NS", "TCS.NS", "INFY.NS", "HDFCBANK.NS", "ICICIBANK.NS", "^NSEI", "AAPL", "MSFT"), chip)),
  h4("What you can do"),
  div(class = "feat-grid",
      feature("gauge-high", "Overview", "Price trend, moving averages, key figures and a plain-English summary.", "Overview"),
      feature("lightbulb", "Interpretation", "Plain-English reading of every result: trend, risk, tests, backtest, forecast and valuation.", "Interpretation"),
      feature("chart-line", "Technical", "Candlesticks, Bollinger Bands, RSI, MACD and crossover signals.", "Technical"),
      feature("chart-column", "Statistics & Risk", "Sharpe, Sortino, VaR, drawdown, normality tests and CAPM beta.", "Statistics & Risk"),
      feature("clock-rotate-left", "Backtest", "Test SMA and MACD strategies against buy-and-hold, with costs.", "Backtest"),
      feature("user-group", "Peers", "Compare returns, volatility and correlation across companies.", "Peers"),
      feature("arrow-trend-up", "Forecast", "ARIMA projections and Monte Carlo price scenarios.", "Forecast"),
      feature("building", "Fundamentals", "Edit annual financials and see margins, ROE, leverage and growth.", "Fundamentals"),
      feature("calculator", "DCF Valuation", "Intrinsic value with sensitivity tables and bear/base/bull cases.", "DCF Valuation"),
      feature("sliders", "Forecast Model", "Build a simple revenue-to-free-cash-flow projection.", "Forecast Model"),
      feature("filter", "Screener", "Rank a list of stocks by return, volatility, Sharpe and drawdown.", "Screener"),
      feature("briefcase", "Portfolio", "Track holdings, market value and unrealised profit or loss.", "Portfolio"),
      feature("file-export", "Reports & Export", "Download CSV data, an Excel workbook and a research summary.", "Reports & Export")
  ),
  h4("How it works"),
  div(class = "steps",
      step(1, "Pick a company", "Search by name or ticker in the sidebar, or click a quick-start ticker above."),
      step(2, "Choose the dates", "Set the From and To dates and the risk-free rate, then load the analysis."),
      step(3, "Explore and export", "Move through the tabs and download your data or a summary report.")
  ),
  div(class = "disclaimer", icon("circle-info"), " Educational research tool, not investment advice. Market data comes from Yahoo Finance's unofficial endpoints and may be delayed, incomplete or change without notice. Verify important figures independently.")
)

# ----------------------------- UI -----------------------------

ui <- bslib::page_sidebar(
  title = "Stock Analysis Studio",
  theme = bslib::bs_theme(
    version = 5, bg = "#0b1020", fg = "#e5e7ff", primary = "#6366f1", secondary = "#1f2747",
    success = "#34d399", danger = "#f87171",
    base_font = bslib::font_google("Inter", local = FALSE)
  ),
  sidebar = bslib::sidebar(
    width = 310, open = "always",
    h5(icon("magnifying-glass"), " Company & date range"),
    textInput("query", "Company name or ticker", "", placeholder = "e.g. Reliance, TCS, Apple"),
    actionButton("search", "Search company", class = "btn-outline-primary"),
    selectizeInput("ticker", "Ticker", choices = NULL, options = list(placeholder = "Search a company first")),
    dateInput("from", "From", value = NA, max = Sys.Date(), startview = "year", format = "yyyy-mm-dd"),
    dateInput("to", "To", value = NA, max = Sys.Date(), startview = "year", format = "yyyy-mm-dd"),
    numericInput("rf", "Risk-free rate (% p.a.)", 7.3, min = 0, max = 30, step = 0.05),
    helpText("Type your own rate. Default is about the India 10-year G-Sec yield (7.37% on 12 Oct 2026); update it if it has moved."),
    actionButton("run", "Load / refresh analysis", class = "btn-primary"),
    hr(),
    helpText("Indian examples: RELIANCE.NS, TCS.NS, INFY.NS. Some tickers may be unavailable."),
    helpText("Market data provider: Yahoo Finance. Unofficial endpoints can change.")
  ),
  tags$head(tags$style(HTML(app_css))),
  tags$head(tags$script(HTML("$(document).on('shiny:connected', function(){ $('#from input, #to input').attr('placeholder', 'Select date'); });"))),
  tabsetPanel(
    id = "main_tabs", type = "pills", selected = "Home",
    tabPanel("Home", icon = icon("house"), home_page),
    tabPanel("Overview", icon = icon("gauge-high"),
             br(), uiOutput("overview_kpis"), uiOutput("overview_text"),
             h4("Historical price and moving averages"), plotOutput("price_plot", height = 380),
             h4("Latest market data"), tableOutput("latest_market")
    ),
    tabPanel("Interpretation", icon = icon("lightbulb"), br(), uiOutput("interpretation_page")),
    tabPanel("Technical", icon = icon("chart-line"),
             br(), plotOutput("candles", height = 520),
             fluidRow(column(6, plotOutput("rsi_plot", height = 280)),
                      column(6, plotOutput("macd_plot", height = 280))),
             h4("Recent technical signals"), DTOutput("signals")
    ),
    tabPanel("Statistics & Risk", icon = icon("chart-column"),
             br(), tableOutput("risk_table"),
             fluidRow(column(6, plotOutput("hist_plot", height = 300)),
                      column(6, plotOutput("qq_plot", height = 300))),
             plotOutput("drawdown_plot", height = 300),
             h4("Statistical tests"), tableOutput("tests_table"),
             h4("Benchmark CAPM"), textInput("benchmark", "Benchmark ticker", "^NSEI"),
             actionButton("run_capm", "Calculate CAPM"), tableOutput("capm_table")
    ),
    tabPanel("Backtest", icon = icon("clock-rotate-left"),
             br(),
             fluidRow(
               column(4, selectInput("strategy", "Strategy", c("SMA 20/50", "SMA 50/200", "MACD"))),
               column(4, numericInput("trade_cost", "Transaction cost per position change (%)", 0.1, min = 0, step = 0.05)),
               column(4, helpText("Long-only. Signals are shifted by one session to reduce look-ahead bias."))
             ),
             plotOutput("backtest_plot", height = 360), tableOutput("backtest_table")
    ),
    tabPanel("Peers", icon = icon("user-group"),
             br(), textInput("peers", "Peer tickers, comma-separated", "TCS.NS,INFY.NS"),
             actionButton("compare", "Compare tickers"),
             plotOutput("peer_plot", height = 350), tableOutput("peer_table"),
             h4("Return correlation"), tableOutput("cor_table")
    ),
    tabPanel("Forecast", icon = icon("arrow-trend-up"),
             br(), fluidRow(
               column(4, numericInput("horizon", "Trading-day horizon", 30, min = 5, max = 250)),
               column(4, numericInput("paths", "Monte Carlo paths", 500, min = 50, max = 3000)),
               column(4, actionButton("forecast_run", "Run forecast", class = "btn-primary"))
             ),
             h4("ARIMA forecast"), plotOutput("arima_plot", height = 350),
             h4("Monte Carlo simulation"), plotOutput("mc_plot", height = 350),
             verbatimTextOutput("mc_text")
    ),
    tabPanel("Fundamentals", icon = icon("building"),
             br(),
             p("Edit annual figures in the table (double-click a cell). Use one consistent currency and unit (for example, INR crore). Blank cells remain missing; they are not treated as zero."),
             fluidRow(
               column(4, actionButton("blank_fin", "Create blank template")),
               column(4, numericInput("fin_start", "First fiscal year", as.integer(format(Sys.Date(), "%Y")) - 5)),
               column(4, numericInput("fin_end", "Last fiscal year", as.integer(format(Sys.Date(), "%Y")) - 1))
             ),
             DTOutput("financial_table"),
             h4("Calculated financial ratios"), tableOutput("ratio_table"),
             fluidRow(column(6, plotOutput("revenue_plot", height = 300)),
                      column(6, plotOutput("profitability_plot", height = 300))),
             h4("Quarterly results / ownership / dividends / filings"),
             p("These require a reliable provider or manually imported data. This prototype does not fabricate unavailable figures."),
             fileInput("import_fin", "Import your financial CSV", accept = ".csv")
    ),
    tabPanel("DCF Valuation", icon = icon("calculator"),
             br(), p("Enter figures in a consistent unit. DCF is sensitive to assumptions; results are not a price target guarantee."),
             fluidRow(
               column(3, numericInput("fcf0", "Latest annual FCF", 1000)),
               column(3, numericInput("growth", "FCF growth (%)", 8)),
               column(3, numericInput("forecast_years", "Forecast years", 5, min = 1, max = 15)),
               column(3, numericInput("wacc", "Discount rate / WACC (%)", 12))
             ),
             fluidRow(
               column(3, numericInput("terminal_growth", "Terminal growth (%)", 4)),
               column(3, numericInput("net_debt", "Net debt", 0)),
               column(3, numericInput("shares", "Shares outstanding", 100)),
               column(3, numericInput("current_price", "Current price (optional)", NA_real_, min = 0))
             ),
             actionButton("calculate_dcf", "Calculate DCF", class = "btn-primary"),
             h4("Base case"), verbatimTextOutput("dcf_summary"),
             tableOutput("dcf_table"),
             h4("Sensitivity: discount rate vs terminal growth"), tableOutput("dcf_sensitivity"),
             h4("Scenario analysis"), tableOutput("dcf_scenarios")
    ),
    tabPanel("Forecast Model", icon = icon("sliders"),
             br(), p("A simplified operating forecast template. Fill in assumptions; a complete three-statement model needs detailed debt, tax, working-capital and accounting schedules."),
             fluidRow(
               column(3, numericInput("model_revenue", "Latest revenue", 10000)),
               column(3, numericInput("model_growth", "Annual revenue growth (%)", 8)),
               column(3, numericInput("model_margin", "EBIT margin (%)", 15)),
               column(3, numericInput("model_tax", "Tax rate (%)", 25))
             ),
             fluidRow(
               column(3, numericInput("model_da", "D&A (% revenue)", 3)),
               column(3, numericInput("model_capex", "Capex (% revenue)", 4)),
               column(3, numericInput("model_nwc", "Change in NWC (% revenue)", 1)),
               column(3, numericInput("model_years", "Forecast years", 5, min = 1, max = 10))
             ),
             actionButton("run_model", "Build forecast"),
             tableOutput("model_table"), plotOutput("model_plot", height = 320),
             helpText("Illustrative operating forecast, not a fully integrated three-statement model.")
    ),
    tabPanel("Screener", icon = icon("filter"),
             br(), p("Paste tickers to screen. Return, volatility, Sharpe ratio and drawdown are calculated from the selected date range and risk-free rate. Fundamental filters can be added once a verified fundamentals feed is connected."),
             textAreaInput("screen_tickers", "Tickers (one per line or comma-separated)", "RELIANCE.NS\nTCS.NS\nINFY.NS\nHDFCBANK.NS\nICICIBANK.NS", rows = 5),
             actionButton("screen_run", "Run screener", class = "btn-primary"),
             br(), br(),
             uiOutput("screen_kpis"), DTOutput("screen_table")
    ),
    tabPanel("Portfolio", icon = icon("briefcase"),
             br(), p("Enter holdings manually (double-click a cell to edit). Quantity and purchase price are used with the latest available price downloaded from Yahoo Finance."),
             p("For multiple holdings, enter one row per ticker. Values are estimates and exclude taxes, fees, dividends and currency effects."),
             DTOutput("portfolio_table"),
             actionButton("portfolio_add", "Add holding"),
             actionButton("portfolio_value", "Refresh portfolio valuation"),
             tableOutput("portfolio_summary")
    ),
    tabPanel("Reports & Export", icon = icon("file-export"),
             br(), h4("Download current analysis"),
             downloadButton("download_prices", "Download price data CSV"),
             downloadButton("download_financials", "Download financials CSV"),
             downloadButton("download_excel", "Download analysis workbook (.xlsx)"),
             downloadButton("download_report", "Download research summary (.txt)"),
             hr(),
             h4("Report contents"),
             p("The report includes ticker/date range, return and risk metrics, available financial ratios, and DCF assumptions/results. Review all source data and assumptions before relying on the output.")
    )
  )
)

# ----------------------------- Server -----------------------------

server <- function(input, output, session) {
  prices <- reactiveVal(NULL)
  ticker_active <- reactiveVal("")
  financials <- reactiveVal(financial_template())
  comparison_data <- eventReactive(input$compare, {
    req(prices())
    tks <- unique(trimws(unlist(strsplit(input$peers, "[,\\n;]+"))))
    tks <- tks[nzchar(tks)]
    tks <- unique(c(ticker_active(), tks))
    validate(need(length(tks) >= 2, "Enter at least one peer ticker."))
    from <- input$from; to <- input$to
    out <- lapply(tks, function(t) {
      tryCatch({
        z <- fetch_prices(t, from, to)
        p <- get_adjusted_close(z)
        r <- make_returns(z)
        data.frame(
          Ticker = t,
          Start = as.numeric(first(p)),
          End = as.numeric(last(p)),
          `Total return %` = (as.numeric(last(p)) / as.numeric(first(p)) - 1) * 100,
          `Annualised volatility %` = sd(as.numeric(r), na.rm = TRUE) * sqrt(252) * 100,
          `Annualised return %` = annualised_return(r) * 100,
          check.names = FALSE
        )
      }, error = function(e) NULL)
    })
    out <- Filter(Negate(is.null), out)
    validate(need(length(out) >= 2, "Could not download at least two tickers. Check symbols and date range."))
    do.call(rbind, out)
  })
  
  observeEvent(input$search, {
    req(nzchar(trimws(input$query)))
    hits <- fetch_yahoo_search(trimws(input$query))
    if (is.null(hits) || !length(hits)) {
      candidate <- toupper(trimws(input$query))
      hits <- stats::setNames(candidate, paste0(candidate, " (manual ticker)"))
      showNotification("No search results. Trying your text as a ticker.", type = "warning")
    }
    updateSelectInput(session, "ticker", choices = hits, selected = unname(hits[1]))
  })
  
  load_ticker <- function(tk, from, to) {
    x <- withProgress(message = paste("Downloading", tk), value = 0.3,
                      tryCatch(fetch_prices(tk, from, to), error = function(e) e))
    if (inherits(x, "error")) {
      showNotification(conditionMessage(x), type = "error", duration = 10)
      return(invisible(FALSE))
    }
    prices(x); ticker_active(tk)
    showNotification(paste("Loaded", tk), type = "message")
    if (NROW(x) < 250) showNotification(
      paste0("Only ", NROW(x), " trading days loaded. Indicators and models that need a longer history (200-day average, statistical tests, forecasts) will be blank or limited. Choose an earlier From date for the full analysis."),
      type = "warning", duration = 12)
    invisible(TRUE)
  }
  
  observeEvent(input$run, {
    if (!nzchar(input$ticker %||% "")) return(showNotification("Search for a company and select a match first.", type = "warning"))
    if (!isTruthy(input$from) || !isTruthy(input$to)) return(showNotification("Select both a From and a To date.", type = "warning"))
    if (input$from > input$to) return(showNotification("From date must be before To date.", type = "warning"))
    ok <- load_ticker(input$ticker, input$from, input$to)
    if (isTRUE(ok) && identical(input$main_tabs, "Home")) updateTabsetPanel(session, "main_tabs", selected = "Overview")
  }, ignoreInit = TRUE)
  
  observeEvent(input$goto_tab, updateTabsetPanel(session, "main_tabs", selected = input$goto_tab))
  
  observeEvent(input$quick_ticker, {
    tk <- input$quick_ticker
    f <- if (isTruthy(input$from)) input$from else Sys.Date() - 365 * 5
    t <- if (isTruthy(input$to)) input$to else Sys.Date()
    updateSelectInput(session, "ticker", choices = stats::setNames(tk, tk), selected = tk)
    updateTextInput(session, "query", value = tk)
    updateDateInput(session, "from", value = f)
    updateDateInput(session, "to", value = t)
    if (isTRUE(load_ticker(tk, f, t))) updateTabsetPanel(session, "main_tabs", selected = "Overview")
  })
  
  current_prices <- reactive({
    req(prices())
    x <- prices()
    p <- get_adjusted_close(x)
    cl <- quantmod::Cl(x)
    hi <- quantmod::Hi(x); lo <- quantmod::Lo(x); op <- quantmod::Op(x); vo <- quantmod::Vo(x)
    data.frame(
      Date = as.Date(zoo::index(x)),
      Open = as.numeric(op), High = as.numeric(hi), Low = as.numeric(lo),
      Close = as.numeric(cl), Adjusted = as.numeric(p),
      Volume = as.numeric(vo),
      SMA20 = as.numeric(safe_sma(p, 20)),
      SMA50 = as.numeric(safe_sma(p, 50)),
      SMA200 = as.numeric(safe_sma(p, 200)),
      RSI14 = as.numeric(safe_rsi(p, 14)),
      stringsAsFactors = FALSE
    )
  })
  
  output$overview_kpis <- renderUI({
    if (is.null(prices())) return(div(class = "alert alert-primary", icon("magnifying-glass"),
                                      " Search for a company, select both dates in the sidebar, then click Load / refresh analysis. You can also pick a quick-start ticker on the Home page."))
    p <- get_adjusted_close(prices())
    r <- make_returns(prices())
    total <- (as.numeric(last(p)) / as.numeric(first(p)) - 1) * 100
    vol <- sd(as.numeric(r), na.rm = TRUE) * sqrt(252) * 100
    dd <- max_drawdown(r) * 100
    val <- as.numeric(last(p))
    kpi <- function(label, value, cls = "") div(class = paste("kpi", cls), div(class = "kpi-value", value), div(class = "kpi-label", label))
    fluidRow(
      column(3, kpi("Ticker", ticker_active(), "k-violet")),
      column(3, kpi("Latest adjusted close", format(round(val, 2), nsmall = 2), "k-blue")),
      column(3, kpi("Total return", sprintf("%+.2f%%", total), if (total >= 0) "k-green" else "k-red")),
      column(3, kpi("Annualised volatility", sprintf("%.2f%%", vol))),
      column(3, kpi("Maximum drawdown", sprintf("%.2f%%", dd), "k-red")),
      column(3, kpi("Observations", format(length(p), big.mark = ","))),
      column(3, kpi("From", as.character(first(zoo::index(p))))),
      column(3, kpi("To", as.character(last(zoo::index(p)))))
    )
  })
  
  output$overview_text <- renderUI({
    req(prices())
    df <- current_prices()
    lastrow <- tail(df, 1)
    trend <- if (is.finite(lastrow$SMA200) && lastrow$Adjusted > lastrow$SMA200) {
      "The latest adjusted close is above the 200-day moving average."
    } else if (is.finite(lastrow$SMA200)) {
      "The latest adjusted close is below the 200-day moving average."
    } else "There are not enough observations to calculate the 200-day moving average."
    div(class = "alert alert-primary mt-3", strong("Plain-English overview: "), trend,
        " Historical performance and indicators describe the selected sample; they do not guarantee future performance.")
  })
  
  output$price_plot <- renderPlot({
    df <- current_prices()
    long <- rbind(
      data.frame(Date = df$Date, Value = df$Adjusted, Series = "Adjusted close"),
      data.frame(Date = df$Date, Value = df$SMA50, Series = "SMA 50"),
      data.frame(Date = df$Date, Value = df$SMA200, Series = "SMA 200")
    )
    ggplot(long, aes(Date, Value, colour = Series)) +
      geom_line(na.rm = TRUE) + labs(title = paste("Price trend -", ticker_active()), x = NULL, y = "Price") + theme_app()
  })
  
  output$latest_market <- renderTable({
    df <- current_prices()
    x <- tail(df, 1)
    data.frame(
      Metric = c("Open", "High", "Low", "Close", "Adjusted close", "Volume", "RSI (14)", "SMA (20)", "SMA (50)", "SMA (200)"),
      Value = round(as.numeric(unlist(x[1, c("Open","High","Low","Close","Adjusted","Volume","RSI14","SMA20","SMA50","SMA200")], use.names = FALSE)), 3)
    )
  })
  
  output$candles <- renderPlot({
    req(prices())
    x <- prices()
    n <- NROW(x)
    ta <- c(if (n > 20) "addSMA(20)", if (n > 50) "addSMA(50)", if (n > 20) "addBBands()")
    quantmod::chartSeries(x, name = paste("OHLC chart -", ticker_active()),
                          theme = quantmod::chartTheme("black"),
                          TA = if (length(ta)) paste(ta, collapse = ";") else NULL)
  })
  
  output$rsi_plot <- renderPlot({
    df <- current_prices()
    ggplot(df, aes(Date, RSI14)) + geom_line(colour = "#a78bfa") +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", colour = "grey50") +
      labs(title = "RSI (14)", x = NULL, y = "RSI") + theme_app()
  })
  
  output$macd_plot <- renderPlot({
    p <- get_adjusted_close(prices())
    m <- safe_macd(p)
    df <- data.frame(Date = as.Date(zoo::index(m)), MACD = as.numeric(m[,1]), Signal = as.numeric(m[,2]))
    ggplot(df, aes(Date)) + geom_line(aes(y = MACD, colour = "MACD")) +
      geom_line(aes(y = Signal, colour = "Signal")) +
      labs(title = "MACD (12, 26, 9)", x = NULL, y = NULL, colour = NULL) + theme_app()
  })
  
  output$signals <- renderDT({
    df <- current_prices()
    m <- safe_macd(get_adjusted_close(prices()))
    df$MACD <- as.numeric(m[,1]); df$MACDSignal <- as.numeric(m[,2])
    df$SMA_Signal <- ifelse(dplyr::lag(df$SMA20) <= dplyr::lag(df$SMA50) & df$SMA20 > df$SMA50, "Bullish crossover",
                            ifelse(dplyr::lag(df$SMA20) >= dplyr::lag(df$SMA50) & df$SMA20 < df$SMA50, "Bearish crossover", ""))
    df$MACD_Signal <- ifelse(dplyr::lag(df$MACD) <= dplyr::lag(df$MACDSignal) & df$MACD > df$MACDSignal, "Bullish crossover",
                             ifelse(dplyr::lag(df$MACD) >= dplyr::lag(df$MACDSignal) & df$MACD < df$MACDSignal, "Bearish crossover", ""))
    df$SMA_Signal[is.na(df$SMA_Signal)] <- ""; df$MACD_Signal[is.na(df$MACD_Signal)] <- ""
    df <- df[df$SMA_Signal != "" | df$MACD_Signal != "", c("Date","Adjusted","SMA_Signal","MACD_Signal")]
    df <- head(df[order(df$Date, decreasing = TRUE), ], 150)
    DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })
  
  output$risk_table <- renderTable({
    req(prices())
    risk_metrics(make_returns(prices()), input$rf / 100)
  })
  
  output$hist_plot <- renderPlot({
    r <- as.numeric(make_returns(prices()))
    ggplot(data.frame(Return = r * 100), aes(Return)) +
      geom_histogram(bins = 40, fill = "#818CF8", colour = "#10162f") +
      labs(title = "Daily log returns", x = "Return (%)", y = "Count") + theme_app()
  })
  
  output$qq_plot <- renderPlot({
    r <- as.numeric(make_returns(prices()))
    par(bg = "#10162f", fg = "#94a3b8", col.axis = "#94a3b8", col.lab = "#cbd5e1", col.main = "#e5e7ff")
    qqnorm(r, main = "Normal Q-Q plot", col = "#818cf8", pch = 16); qqline(r, col = "#f472b6", lwd = 2)
  })
  
  output$drawdown_plot <- renderPlot({
    r <- as.numeric(make_returns(prices()))
    wealth <- cumprod(exp(r))
    dd <- wealth / cummax(wealth) - 1
    df <- data.frame(Date = as.Date(zoo::index(make_returns(prices()))), Drawdown = 100 * dd)
    ggplot(df, aes(Date, Drawdown)) + geom_area(fill = "#EF4444", alpha = 0.4) +
      labs(title = "Historical drawdown", x = NULL, y = "Drawdown (%)") + theme_app()
  })
  
  output$tests_table <- renderTable({
    r <- as.numeric(make_returns(prices()))
    validate(need(length(r) >= 50, "At least 50 daily returns are recommended for these tests."))
    safe_test <- function(label, fn, good, bad) {
      z <- tryCatch(fn(), error = function(e) NULL)
      if (is.null(z)) return(data.frame(Test = label, Statistic = NA, `p-value` = NA, Interpretation = "Test failed or data insufficient"))
      p <- z$p.value
      data.frame(Test = label, Statistic = unname(z$statistic[1]), `p-value` = p,
                 Interpretation = if (p < 0.05) good else bad)
    }
    do.call(rbind, list(
      safe_test("ADF test", function() tseries::adf.test(r), "Reject unit-root null; returns appear stationary", "Cannot reject unit-root null at 5%"),
      safe_test("Ljung-Box", function() Box.test(r, lag = 10, type = "Ljung-Box"), "Evidence of serial correlation", "No significant serial correlation detected"),
      safe_test("ARCH effects", function() FinTS::ArchTest(r, lags = 12), "Evidence of ARCH effects / volatility clustering", "No significant ARCH effect detected"),
      safe_test("Jarque-Bera", function() tseries::jarque.bera.test(r), "Reject normality; tails/skew may matter", "Cannot reject normality at 5%")
    ))
  })
  
  capm_result <- eventReactive(input$run_capm, {
    req(prices(), nzchar(trimws(input$benchmark)))
    bench <- tryCatch(fetch_prices(trimws(input$benchmark), input$from, input$to), error = function(e) NULL)
    validate(need(!is.null(bench), "Could not download benchmark."))
    a <- make_returns(prices()); b <- make_returns(bench)
    z <- na.omit(merge(a, b, join = "inner"))
    validate(need(NROW(z) >= 50, "Not enough overlapping dates."))
    fit <- lm(as.numeric(z[,1]) ~ as.numeric(z[,2]))
    co <- coef(fit)
    data.frame(
      Metric = c("Beta", "Annualised alpha (%)", "R-squared", "Correlation"),
      Value = round(c(co[2], co[1] * 252 * 100, summary(fit)$r.squared,
                      cor(as.numeric(z[,1]), as.numeric(z[,2]))), 4),
      Interpretation = c("Sensitivity to benchmark daily returns",
                         "Regression intercept annualised; estimate is noisy",
                         "Share of sample daily-return variation explained by benchmark",
                         "Historical return correlation")
    )
  })
  output$capm_table <- renderTable(capm_result())
  
  # FIXED: drops the leading NA from diff(), applies a single one-session
  # signal lag, and removes any remaining non-finite values.
  backtest_result <- reactive({
    req(prices())
    p <- get_adjusted_close(prices())
    ret_xts <- diff(log(p))[-1]            # drop the leading NA
    r <- as.numeric(ret_xts)
    dates <- as.Date(zoo::index(ret_xts))
    
    if (input$strategy == "SMA 20/50") {
      pos0 <- as.numeric(safe_sma(p, 20) > safe_sma(p, 50))
    } else if (input$strategy == "SMA 50/200") {
      pos0 <- as.numeric(safe_sma(p, 50) > safe_sma(p, 200))
    } else {
      m <- safe_macd(p)
      pos0 <- as.numeric(m[,1] > m[,2])
    }
    
    # Return i (price i -> i+1) uses the signal known at price i: one-session lag.
    held <- head(pos0, -1)
    held[!is.finite(held)] <- 0
    
    n <- min(length(r), length(held), length(dates))
    r <- r[seq_len(n)]; held <- held[seq_len(n)]; dates <- dates[seq_len(n)]
    
    changes <- c(abs(held[1]), abs(diff(held)))
    strat <- held * r - changes * input$trade_cost / 100
    ok <- is.finite(r) & is.finite(strat)
    list(date = dates[ok], strategy = strat[ok], buyhold = r[ok], position = held[ok])
  })
  
  output$backtest_plot <- renderPlot({
    b <- backtest_result()
    df <- rbind(
      data.frame(Date = b$date, Wealth = exp(cumsum(b$strategy)), Series = "Strategy"),
      data.frame(Date = b$date, Wealth = exp(cumsum(b$buyhold)), Series = "Buy & hold")
    )
    ggplot(df, aes(Date, Wealth, colour = Series)) + geom_line() +
      labs(title = "Growth of 1 unit (illustrative backtest)", x = NULL, y = "Portfolio value") + theme_app()
  })
  
  output$backtest_table <- renderTable({
    b <- backtest_result()
    summarize <- function(r) {
      data.frame(
        `Total return %` = (exp(sum(r)) - 1) * 100,
        `Annualised return %` = (exp(mean(r) * 252) - 1) * 100,
        `Annualised volatility %` = sd(r) * sqrt(252) * 100,
        `Max drawdown %` = max_drawdown(r) * 100,
        check.names = FALSE
      )
    }
    s <- summarize(b$strategy); h <- summarize(b$buyhold)
    data.frame(Series = c("Strategy", "Buy & hold"), rbind(s, h), check.names = FALSE)
  })
  
  output$peer_plot <- renderPlot({
    tb <- comparison_data()
    ggplot(tb, aes(x = reorder(Ticker, `Total return %`), y = `Total return %`, fill = Ticker)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(title = "Total return comparison", x = NULL, y = "Total return (%)") + theme_app()
  })
  output$peer_table <- renderTable(comparison_data())
  output$cor_table <- renderTable({
    req(prices())
    tks <- unique(trimws(unlist(strsplit(input$peers, "[,\\n;]+"))))
    tks <- tks[nzchar(tks)]
    tks <- unique(c(ticker_active(), tks))
    vals <- lapply(tks, function(t) tryCatch(make_returns(fetch_prices(t, input$from, input$to)), error = function(e) NULL))
    names(vals) <- tks
    vals <- Filter(Negate(is.null), vals)
    validate(need(length(vals) >= 2, "At least two downloadable tickers are needed."))
    z <- na.omit(do.call(merge, vals))
    round(cor(as.matrix(z)), 3)
  }, rownames = TRUE)
  
  forecast_result <- eventReactive(input$forecast_run, {
    req(prices())
    p <- get_adjusted_close(prices())
    validate(need(length(p) >= 100, "At least 100 price observations are recommended."))
    h <- input$horizon
    fit <- forecast::auto.arima(log(as.numeric(tail(p, 1000))))
    fc <- forecast::forecast(fit, h = h, level = 95)
    r <- as.numeric(make_returns(prices()))
    sims <- replicate(input$paths, {
      as.numeric(last(p)) * exp(cumsum(stats::rnorm(h, mean(r, na.rm = TRUE), sd(r, na.rm = TRUE))))
    })
    list(forecast = fc, fit = fit, sims = sims, h = h, last = as.numeric(last(p)))
  })
  
  output$arima_plot <- renderPlot({
    f <- forecast_result()
    h <- f$h
    histp <- tail(get_adjusted_close(prices()), 250)
    histdf <- data.frame(Date = as.Date(zoo::index(histp)), Price = as.numeric(histp))
    # Future dates are approximate business days; exchange holidays are not modelled.
    fut_dates <- seq(max(histdf$Date) + 1, by = "day", length.out = h * 2 + 20)
    fut_dates <- fut_dates[as.POSIXlt(fut_dates)$wday %in% 1:5][seq_len(h)]
    fcdf <- data.frame(Date = fut_dates, Mean = exp(as.numeric(f$forecast$mean)),
                       Lo = exp(as.numeric(f$forecast$lower[,1])),
                       Hi = exp(as.numeric(f$forecast$upper[,1])))
    ggplot() + geom_line(data = histdf, aes(Date, Price), colour = "#818cf8") +
      geom_ribbon(data = fcdf, aes(Date, ymin = Lo, ymax = Hi), fill = "#F59E0B", alpha = 0.25) +
      geom_line(data = fcdf, aes(Date, Mean), colour = "#fbbf24", linetype = "dashed") +
      labs(title = "ARIMA forecast with approximate 95% interval", x = NULL, y = "Price") + theme_app()
  })
  
  output$mc_plot <- renderPlot({
    f <- forecast_result()
    q <- apply(f$sims, 1, quantile, probs = c(.05, .5, .95), na.rm = TRUE)
    df <- data.frame(Day = seq_len(f$h), Low = q[1,], Median = q[2,], High = q[3,])
    ggplot(df, aes(Day)) + geom_ribbon(aes(ymin = Low, ymax = High), fill = "#818CF8", alpha = .3) +
      geom_line(aes(y = Median), colour = "#a5b4fc") +
      geom_hline(yintercept = f$last, linetype = "dashed", colour = "#94a3b8") +
      labs(title = "Monte Carlo price distribution", x = "Trading days ahead", y = "Simulated price") + theme_app()
  })
  
  output$mc_text <- renderText({
    f <- forecast_result()
    e <- f$sims[f$h,]
    sprintf("After %d trading days: median %.2f; 5th-95th percentile %.2f to %.2f. Simulated probability of ending above current price: %.1f%%. This is a model-based scenario, not a real-world probability guarantee.",
            f$h, median(e), quantile(e,.05), quantile(e,.95), mean(e > f$last) * 100)
  })
  
  observeEvent(input$blank_fin, {
    yrs <- seq(input$fin_start, input$fin_end)
    financials(financial_template(yrs))
  })
  
  observeEvent(input$import_fin, {
    req(input$import_fin$datapath)
    x <- tryCatch(read.csv(input$import_fin$datapath, check.names = FALSE), error = function(e) NULL)
    if (is.null(x) || !("Year" %in% names(x))) {
      showNotification("CSV must contain a Year column.", type = "error")
    } else {
      financials(x)
      showNotification("Financial CSV imported. Check the column names and units.", type = "message")
    }
  })
  
  output$financial_table <- renderDT({
    DT::datatable(financials(), rownames = FALSE, editable = TRUE,
                  options = list(pageLength = 8, scrollX = TRUE))
  })
  
  observeEvent(input$financial_table_cell_edit, {
    financials(DT::editData(financials(), input$financial_table_cell_edit, rownames = FALSE))
  })
  
  ratios <- reactive({
    x <- financials()
    validate(need(!is.null(x) && nrow(x) >= 1, "Import or create financial data first."))
    needed <- c("Year","Revenue","COGS","EBITDA","EBIT","NetIncome","EPS","TotalDebt","Cash","Equity","TotalAssets","OpCashFlow","Capex","CurrentAssets","CurrentLiab","Shares")
    absent <- setdiff(needed, names(x))
    validate(need(!length(absent), paste("Missing columns:", paste(absent, collapse = ", "))))
    financial_ratios(x)
  })
  
  output$ratio_table <- renderTable({
    x <- ratios()
    x[-1] <- lapply(x[-1], function(v) round(as.numeric(v), 2))
    x
  })
  
  output$revenue_plot <- renderPlot({
    x <- financials()
    validate(need(all(c("Year","Revenue","NetIncome") %in% names(x)), "Financial columns unavailable."))
    long <- rbind(data.frame(Year = x$Year, Value = x$Revenue, Metric = "Revenue"),
                  data.frame(Year = x$Year, Value = x$NetIncome, Metric = "Net income"))
    ggplot(long, aes(factor(Year), Value, fill = Metric)) + geom_col(position = "dodge", na.rm = TRUE) +
      labs(title = "Revenue and net income (entered units)", x = NULL, y = NULL) + theme_app()
  })
  
  output$profitability_plot <- renderPlot({
    x <- ratios()
    long <- rbind(data.frame(Year = x$Year, Value = x$`Net margin %`, Metric = "Net margin"),
                  data.frame(Year = x$Year, Value = x$`EBITDA margin %`, Metric = "EBITDA margin"),
                  data.frame(Year = x$Year, Value = x$`ROE %`, Metric = "ROE"))
    ggplot(long, aes(Year, Value, colour = Metric)) + geom_line(na.rm = TRUE) + geom_point(na.rm = TRUE) +
      labs(title = "Profitability ratios", x = NULL, y = "%") + theme_app()
  })
  
  dcf_result <- eventReactive(input$calculate_dcf, {
    tryCatch(
      dcf_model(input$fcf0, input$growth/100, input$forecast_years,
                input$wacc/100, input$terminal_growth/100,
                input$net_debt, input$shares),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
  })
  
  output$dcf_summary <- renderText({
    x <- dcf_result()
    validate(need(!is.null(x), "Enter valid assumptions and click Calculate DCF."))
    price_text <- if (is.finite(input$current_price) && input$current_price > 0) {
      sprintf("\nCompared with current price %.2f: implied upside/downside %.1f%%",
              input$current_price, (x$per_share/input$current_price - 1)*100)
    } else ""
    sprintf(paste0(
      "Enterprise value: %.2f\nEquity value after net debt: %.2f\n",
      "Implied value per share: %.2f\nPresent value of terminal value / enterprise value: %.1f%%%s\n\n",
      "Units follow your inputs. If terminal value contributes a large share of enterprise value, the result is especially sensitive to WACC and terminal growth."
    ), x$ev, x$equity_value, x$per_share, 100*x$terminal_share, price_text)
  })
  
  output$dcf_table <- renderTable({
    x <- dcf_result()
    validate(need(!is.null(x), "Click Calculate DCF."))
    data.frame(Year = x$year, Forecast_FCF = round(x$fcf,2), Present_Value = round(x$pv,2))
  })
  
  output$dcf_sensitivity <- renderTable({
    r0 <- input$wacc/100
    tg0 <- input$terminal_growth/100
    rs <- r0 + seq(-.02,.02,by=.01)
    gs <- tg0 + c(-.01,0,.01)
    out <- sapply(gs, function(g) sapply(rs, function(r) {
      if (r <= g || input$shares <= 0) return(NA_real_)
      tryCatch(dcf_model(input$fcf0, input$growth/100, input$forecast_years, r, g,
                         input$net_debt, input$shares)$per_share, error=function(e) NA_real_)
    }))
    out <- as.data.frame(round(out,2))
    names(out) <- paste0("Terminal growth ", round(gs*100,1), "%")
    out <- cbind(`Discount rate` = paste0(round(rs*100,1), "%"), out)
    out
  })
  
  output$dcf_scenarios <- renderTable({
    scenarios <- data.frame(
      Scenario = c("Bear", "Base", "Bull"),
      Growth = c(input$growth - 5, input$growth, input$growth + 5),
      Discount = c(input$wacc + 2, input$wacc, max(0.1, input$wacc - 1)),
      Terminal_growth = c(input$terminal_growth - 1, input$terminal_growth, input$terminal_growth + 0.5)
    )
    scenarios$Value_per_share <- mapply(function(g,r,tg) {
      if (r <= tg) return(NA_real_)
      tryCatch(dcf_model(input$fcf0, g/100, input$forecast_years, r/100, tg/100,
                         input$net_debt, input$shares)$per_share, error=function(e) NA_real_)
    }, scenarios$Growth, scenarios$Discount, scenarios$Terminal_growth)
    scenarios$Growth <- paste0(round(scenarios$Growth,1), "%")
    scenarios$Discount <- paste0(round(scenarios$Discount,1), "%")
    scenarios$Terminal_growth <- paste0(round(scenarios$Terminal_growth,1), "%")
    scenarios$Value_per_share <- round(scenarios$Value_per_share,2)
    scenarios
  })
  
  model_result <- eventReactive(input$run_model, {
    n <- input$model_years
    yr <- seq_len(n)
    revenue <- input$model_revenue * (1 + input$model_growth/100)^yr
    ebit <- revenue * input$model_margin/100
    tax <- pmax(ebit, 0) * input$model_tax/100
    da <- revenue * input$model_da/100
    capex <- revenue * input$model_capex/100
    nwc_change <- revenue * input$model_nwc/100
    fcf <- ebit - tax + da - capex - nwc_change
    data.frame(Year = yr, Revenue = revenue, EBIT = ebit, Taxes = tax,
               DandA = da, Capex = capex, Change_NWC = nwc_change,
               Illustrative_FCF = fcf)
  })
  
  output$model_table <- renderTable({
    x <- model_result()
    validate(need(!is.null(x), "Set assumptions and click Build forecast."))
    x[-1] <- lapply(x[-1], round, 2)
    x
  })
  output$model_plot <- renderPlot({
    x <- model_result()
    validate(need(!is.null(x), "Click Build forecast."))
    ggplot(x, aes(Year, Revenue)) + geom_line(colour = "#818cf8", linewidth = 1) +
      geom_point(colour = "#818cf8") + labs(title = "Forecast revenue", x = "Forecast year", y = "Revenue") + theme_app()
  })
  
  # ---- Interpretation (plain-English reading of all results) ----
  output$interpretation_page <- renderUI({
    if (is.null(prices())) return(div(class = "alert alert-primary", icon("lightbulb"),
                                      " Load a company first (sidebar, or a quick-start ticker on Home). Everything on this page is generated from the analysis of the loaded company."))
    ins <- function(level, ...) list(level = level, text = paste0(...))
    p <- get_adjusted_close(prices()); r <- as.numeric(make_returns(prices())); rf <- input$rf / 100
    df <- current_prices(); lr <- tail(df, 1); n <- length(r)
    mu <- mean(r) * 252; vol <- sd(r) * sqrt(252); sh <- (mu - rf) / vol
    tot <- (as.numeric(last(p)) / as.numeric(first(p)) - 1) * 100
    add <- function(l, x) c(l, list(x))
    
    # 1. Trend and momentum
    s1 <- list(ins(if (tot >= 0) "good" else "warn",
                   sprintf("Over the selected period the stock returned %+.1f%% (about %+.1f%% a year).", tot, (exp(mu) - 1) * 100)))
    if (is.finite(lr$SMA200)) { up <- lr$Adjusted > lr$SMA200
    s1 <- add(s1, ins(if (up) "good" else "warn", "The latest price is ", if (up) "above" else "below",
                      " its 200-day average, so the long-term trend is ", if (up) "up." else "down.")) }
    if (is.finite(lr$SMA50)) { up <- lr$Adjusted > lr$SMA50
    s1 <- add(s1, ins(if (up) "good" else "warn", "The price is ", if (up) "above" else "below",
                      " its 50-day average, so the medium-term trend is ", if (up) "up." else "down.")) }
    if (is.finite(lr$SMA50) && is.finite(lr$SMA200))
      s1 <- add(s1, ins(if (lr$SMA50 > lr$SMA200) "good" else "warn", if (lr$SMA50 > lr$SMA200)
        "The 50-day average is above the 200-day average (golden-cross regime), which usually reflects a sustained uptrend."
        else "The 50-day average is below the 200-day average (death-cross regime), which usually reflects a sustained downtrend."))
    if (is.finite(lr$RSI14)) s1 <- add(s1, ins(
      if (lr$RSI14 > 70) "warn" else "neutral",
      sprintf("RSI is %.0f. ", lr$RSI14), if (lr$RSI14 > 70) "Above 70 is overbought: the rally may be stretched and a pullback is more likely."
      else if (lr$RSI14 < 30) "Below 30 is oversold: selling may be overdone and a rebound is possible, but not guaranteed."
      else "Between 30 and 70, momentum is neutral."))
    m <- safe_macd(p); mm <- as.numeric(tail(m[, 1], 1)); ms <- as.numeric(tail(m[, 2], 1))
    if (is.finite(mm) && is.finite(ms)) s1 <- add(s1, ins(if (mm > ms) "good" else "warn",
                                                          "MACD is ", if (mm > ms) "above" else "below", " its signal line: short-term momentum is ", if (mm > ms) "bullish." else "bearish."))
    
    # 2. Risk and return
    mdd <- max_drawdown(r) * 100; q5 <- as.numeric(quantile(r, 0.05)); cv <- mean(r[r <= q5])
    dn <- sqrt(mean(pmin(r - rf / 252, 0)^2)) * sqrt(252); so <- (mu - rf) / dn
    vb <- band(vol * 100, c(15, 25, 40), c("Low", "Moderate", "High", "Very high"))
    shb <- band(sh, c(0, 0.5, 1), c("it did not beat the risk-free rate", "weak", "acceptable", "good"))
    s2 <- list(
      ins(if (isTRUE(vol * 100 < 25)) "good" else "warn", sprintf("Annualised volatility is %.1f%%: %s. Typical yearly swings of this size are normal for this level.", vol * 100, vb)),
      ins(if (isTRUE(sh >= 0.5)) "good" else "warn", sprintf("Sharpe ratio is %.2f at a %.2f%% risk-free rate: risk-adjusted performance is %s.", sh, rf * 100, shb)),
      ins(if (isTRUE(is.finite(so) && so >= 0.5)) "good" else "neutral", sprintf("Sortino ratio is %.2f. It is like Sharpe but only penalises downside moves, so a Sortino above Sharpe means the volatility was mostly on the upside.", so)),
      ins("warn", sprintf("The worst peak-to-trough fall was %.1f%%. An investor who bought at the peak needed a %.0f%% gain just to break even.", abs(mdd), (1 / (1 - abs(mdd) / 100) - 1) * 100)),
      ins("neutral", sprintf("On 95%% of days the loss was no worse than %.2f%%. On the worst 5%% of days the average loss was %.2f%% (VaR and CVaR).", abs(q5) * 100, abs(cv) * 100)))
    
    # 3. Statistical tests
    s3 <- list()
    if (n >= 50) {
      tp <- function(fn) tryCatch(fn()$p.value, error = function(e) NA_real_)
      pa <- tp(function() suppressWarnings(tseries::adf.test(r))); pl <- tp(function() Box.test(r, lag = 10, type = "Ljung-Box"))
      pc <- tp(function() FinTS::ArchTest(r, lags = 12)); pj <- tp(function() tseries::jarque.bera.test(r))
      if (is.finite(pa)) s3 <- add(s3, if (pa < 0.05) ins("good", sprintf("Returns are stationary (ADF p = %.3f): their average and spread are stable over time, so standard time-series models are valid.", pa))
                                   else ins("warn", sprintf("The ADF test cannot reject a unit root (p = %.3f): treat model results with extra care.", pa)))
      if (is.finite(pl)) s3 <- add(s3, if (pl < 0.05) ins("neutral", sprintf("Returns are autocorrelated (Ljung-Box p = %.3f): one day's move carries some information about the next, which momentum or mean-reversion strategies try to use.", pl))
                                   else ins("neutral", sprintf("No significant autocorrelation (Ljung-Box p = %.3f): past returns do not help predict the next day, consistent with an efficient market.", pl)))
      if (is.finite(pc)) s3 <- add(s3, if (pc < 0.05) ins("warn", sprintf("Volatility clusters (ARCH p = %.3f): calm and turbulent spells persist, so risk is not constant and recent volatility matters more than the long-run average.", pc))
                                   else ins("good", sprintf("No ARCH effect (p = %.3f): volatility is fairly stable over time.", pc)))
      if (is.finite(pj)) s3 <- add(s3, if (pj < 0.05) ins("warn", sprintf("Returns are not normally distributed (Jarque-Bera p = %.3f): extreme days happen more often than a bell curve implies, so normal-based risk estimates understate tail losses.", pj))
                                   else ins("good", sprintf("Returns are close to normal (Jarque-Bera p = %.3f), so normal-based risk estimates are reasonable.", pj)))
    } else s3 <- list(ins("neutral", "At least 50 daily returns are needed for the statistical tests. Choose an earlier From date."))
    
    # 4. Backtest
    b <- tryCatch(backtest_result(), error = function(e) NULL)
    s4 <- if (is.null(b) || length(b$strategy) < 2) list(ins("neutral", "Not enough data to backtest.")) else {
      st <- (exp(sum(b$strategy)) - 1) * 100; bh <- (exp(sum(b$buyhold)) - 1) * 100
      d1 <- max_drawdown(b$strategy) * 100; d2 <- max_drawdown(b$buyhold) * 100
      list(ins(if (isTRUE(st > bh)) "good" else "warn", sprintf("The %s strategy returned %.1f%% vs %.1f%% for buy-and-hold over the same period.", input$strategy, st, bh)),
           ins(if (isTRUE(d1 > d2)) "good" else "neutral", sprintf("Its worst drawdown was %.1f%% vs %.1f%% for buy-and-hold: %s", d1, d2, if (isTRUE(d1 > d2)) "it protected capital better in falls." else "it did not reduce the worst fall.")),
           ins("neutral", "A backtest only shows what would have happened in the past. Real trading adds slippage, taxes and changing market conditions."))
    }
    
    # 5. CAPM
    cr <- tryCatch(capm_result(), error = function(e) NULL)
    s5 <- if (is.null(cr)) list(ins("neutral", "Run Calculate CAPM in the Statistics & Risk tab to see beta, alpha and market sensitivity here.")) else {
      be <- cr$Value[1]; al <- cr$Value[2]; r2 <- cr$Value[3]
      list(ins(if (isTRUE(be > 1.2)) "warn" else "neutral", sprintf("Beta is %.2f: %s", be, band(be, c(0.8, 1.2), c("defensive, it moves less than the benchmark.", "it moves roughly in line with the benchmark.", "aggressive, it amplifies benchmark moves in both directions.")))),
           ins(if (isTRUE(al > 0)) "good" else "warn", sprintf("Annualised alpha is %.1f%%: %s This estimate is noisy.", al, if (isTRUE(al > 0)) "the stock earned more than its market exposure explains." else "the stock earned less than its market exposure explains.")),
           ins("neutral", sprintf("The benchmark explains %.0f%% of the stock's daily movement; the rest is company-specific.", r2 * 100)))
    }
    
    # 6. Forecast
    fc <- tryCatch(forecast_result(), error = function(e) NULL)
    s6 <- if (is.null(fc)) list(ins("neutral", "Run the forecast in the Forecast tab to see projected ranges and probabilities here.")) else {
      e <- fc$sims[fc$h, ]; pu <- mean(e > fc$last) * 100; am <- exp(tail(as.numeric(fc$forecast$mean), 1))
      list(ins("neutral", sprintf("Monte Carlo: after %d trading days the median simulated price is %.2f (5th to 95th percentile: %.2f to %.2f), versus %.2f today.", fc$h, median(e), quantile(e, 0.05), quantile(e, 0.95), fc$last)),
           ins(if (isTRUE(pu >= 55)) "good" else if (isTRUE(pu <= 45)) "warn" else "neutral", sprintf("The simulation ends above today's price in %.0f%% of paths. This comes from past average return and volatility, not from fundamentals or news.", pu)),
           ins("neutral", sprintf("ARIMA central projection after %d days is %.2f. ARIMA on prices usually stays close to the last price, so treat it as a baseline, not a target.", fc$h, am)))
    }
    
    # 7. Peers
    cd <- tryCatch(comparison_data(), error = function(e) NULL); tk <- ticker_active()
    s7 <- if (is.null(cd) || !(tk %in% cd$Ticker)) list(ins("neutral", "Run Compare tickers in the Peers tab to see how this stock ranks against peers.")) else {
      i <- match(tk, cd$Ticker); rr <- rank(-cd$`Total return %`)[i]; rv <- rank(cd$`Annualised volatility %`)[i]; k <- nrow(cd)
      list(ins(if (isTRUE(rr <= k / 2)) "good" else "warn", sprintf("%s ranks %d of %d on total return in this peer group.", tk, rr, k)),
           ins(if (isTRUE(rv <= k / 2)) "good" else "warn", sprintf("It ranks %d of %d on volatility (1 = least volatile), so it was %s risky than the typical peer.", rv, k, if (isTRUE(rv <= k / 2)) "less" else "more")))
    }
    
    # 8. Fundamentals
    fin <- financials(); rt <- if (any(is.finite(safe_num(fin$Revenue)))) tryCatch(ratios(), error = function(e) NULL) else NULL
    s8 <- if (is.null(rt) || !nrow(rt)) list(ins("neutral", "Enter or import annual figures in the Fundamentals tab to see margin, return, leverage and growth interpretations here.")) else {
      l <- tail(rt, 1); g <- function(nm) safe_num(l[[nm]]); out <- list(); yr <- l$Year
      nm <- g("Net margin %"); if (is.finite(nm)) out <- add(out, ins(if (nm >= 15) "good" else if (nm < 5) "warn" else "neutral", sprintf("Net margin %.1f%% (%s): %s", nm, yr, band(nm, c(5, 15), c("thin, with little cushion against cost increases.", "healthy.", "strong, pointing to pricing power or a low cost base.")))))
      ro <- g("ROE %"); if (is.finite(ro)) out <- add(out, ins(if (ro >= 15) "good" else if (ro < 8) "warn" else "neutral", sprintf("Return on equity %.1f%%: %s", ro, band(ro, c(8, 15), c("low return on shareholder capital.", "decent.", "strong, but check that it is not driven by high debt.")))))
      de <- g("Debt / Equity"); if (is.finite(de)) out <- add(out, ins(if (de < 0.5) "good" else if (de > 1.5) "warn" else "neutral", sprintf("Debt to equity %.2f: %s", de, band(de, c(0.5, 1.5), c("conservative leverage.", "moderate leverage.", "high leverage, so interest and refinancing risk matter.")))))
      cu <- g("Current ratio"); if (is.finite(cu)) out <- add(out, ins(if (cu >= 1) "good" else "warn", sprintf("Current ratio %.2f: %s", cu, band(cu, c(1, 2), c("short-term obligations may be hard to meet.", "adequate liquidity.", "very liquid, possibly with idle cash.")))))
      rg <- g("Revenue growth %"); if (is.finite(rg)) out <- add(out, ins(if (rg >= 8) "good" else if (rg < 0) "warn" else "neutral", sprintf("Revenue growth %.1f%%: %s", rg, band(rg, c(0, 8, 20), c("sales are shrinking.", "slow growth.", "solid growth.", "rapid growth.")))))
      fm <- g("FCF margin %"); if (is.finite(fm)) out <- add(out, ins(if (fm > 0) "good" else "warn", sprintf("Free cash flow margin %.1f%%: %s", fm, if (fm > 0) "the business generates cash after capital spending." else "the business is burning cash after capital spending.")))
      if (length(out)) out else list(ins("neutral", "Fill in more of the financial table to generate ratio interpretations."))
    }
    
    # 9. Valuation
    dc <- tryCatch(dcf_result(), error = function(e) NULL)
    s9 <- if (is.null(dc)) list(ins("neutral", "Run Calculate DCF in the DCF Valuation tab to see the valuation reading here.")) else {
      out <- list(ins("neutral", sprintf("The DCF gives an implied value of %.2f per share, using your growth, discount-rate and terminal-growth assumptions.", dc$per_share)))
      if (is.finite(input$current_price) && input$current_price > 0) { up <- dc$per_share / input$current_price - 1
      out <- add(out, ins(if (isTRUE(up > 0.15)) "good" else if (isTRUE(up < -0.15)) "warn" else "neutral", sprintf("Versus your current price of %.2f that is %+.1f%%: %s", input$current_price, up * 100,
                                                                                                                    band(up, c(-0.15, 0.15), c("the stock looks overvalued on these assumptions.", "roughly fairly valued (within 15%).", "the stock looks undervalued, with a margin of safety."))))) }
      if (is.finite(dc$terminal_share) && dc$terminal_share > 0.75) out <- add(out, ins("warn", sprintf("%.0f%% of the value comes from the terminal value, so the result is very sensitive to the discount rate and terminal growth. Check the sensitivity table.", dc$terminal_share * 100)))
      out
    }
    
    secs <- list(list("Trend and momentum", "chart-line", s1), list("Risk and return", "chart-column", s2),
                 list("Statistical tests", "flask", s3), list("Backtest", "clock-rotate-left", s4),
                 list("Market sensitivity (CAPM)", "link", s5), list("Forecast", "arrow-trend-up", s6),
                 list("Peer comparison", "user-group", s7), list("Fundamentals", "building", s8), list("Valuation (DCF)", "calculator", s9))
    lv <- unlist(lapply(secs, function(x) vapply(x[[3]], function(i) i$level, "")))
    badge <- c(good = "Positive", neutral = "Neutral", warn = "Caution")
    sec <- function(x) div(class = "interp-card", h4(icon(x[[2]]), " ", x[[1]]),
                           tags$ul(class = "interp-list", lapply(x[[3]], function(i) tags$li(class = i$level, span(class = "badge-lvl", badge[[i$level]]), i$text))))
    gl <- list(c("Sharpe ratio", "Return above the risk-free rate per unit of volatility. Above 1 is good, below 0.5 is weak."),
               c("Sortino ratio", "Like Sharpe, but only downside volatility counts as risk."),
               c("Max drawdown", "Largest fall from a peak to a later low."),
               c("VaR / CVaR", "VaR is the loss not exceeded on 95% of days. CVaR is the average loss on the worst 5% of days."),
               c("Beta / alpha", "Beta is sensitivity to the market. Alpha is return not explained by market exposure."),
               c("RSI", "Momentum from 0 to 100. Above 70 is overbought, below 30 is oversold."),
               c("MACD", "Trend-momentum indicator. MACD above its signal line is bullish."),
               c("ADF / Ljung-Box / ARCH / Jarque-Bera", "Tests for stationarity, autocorrelation, volatility clustering and normality (p below 0.05 means the effect is present)."),
               c("Terminal value", "The part of a DCF valuation that comes from cash flows beyond the explicit forecast years."))
    tagList(
      div(class = "alert alert-primary", strong(sprintf("%s, %s to %s: ", tk, format(input$from), format(input$to))),
          sprintf("%d positive signals, %d cautions and %d neutral notes across %d checks. ", sum(lv == "good"), sum(lv == "warn"), sum(lv == "neutral"), length(lv)),
          "These describe past data. They are not a forecast or a recommendation."),
      lapply(secs, sec),
      tags$details(class = "glossary", tags$summary("How to read these terms"),
                   tags$dl(lapply(gl, function(x) tagList(tags$dt(x[1]), tags$dd(x[2]))))))
  })
  
  # ---- Screener ----
  screen_result <- eventReactive(input$screen_run, {
    tks <- unique(trimws(unlist(strsplit(input$screen_tickers, "[,\\n;]+"))))
    tks <- toupper(tks[nzchar(tks)])
    shiny::validate(shiny::need(length(tks) > 0, "Enter at least one ticker."))
    shiny::validate(shiny::need(isTruthy(input$from) && isTruthy(input$to), "Select the From and To dates in the sidebar first."))
    rf <- input$rf / 100
    rows <- withProgress(message = "Screening...", value = 0, lapply(seq_along(tks), function(i) {
      incProgress(1 / length(tks), detail = tks[i])
      tryCatch({
        x <- fetch_prices(tks[i], input$from, input$to)
        p <- get_adjusted_close(x); r <- as.numeric(make_returns(x))
        vol <- sd(r) * sqrt(252)
        data.frame(Ticker = tks[i], Price = round(as.numeric(last(p)), 2),
                   `Total return %` = round((as.numeric(last(p)) / as.numeric(first(p)) - 1) * 100, 1),
                   `Annual return %` = round((exp(mean(r) * 252) - 1) * 100, 1),
                   `Volatility %` = round(vol * 100, 1),
                   Sharpe = round((mean(r) * 252 - rf) / vol, 2),
                   `Max drawdown %` = round(max_drawdown(r) * 100, 1),
                   Days = length(p), Status = "OK", check.names = FALSE)
      }, error = function(e) data.frame(Ticker = tks[i], Price = NA, `Total return %` = NA,
                                        `Annual return %` = NA, `Volatility %` = NA, Sharpe = NA, `Max drawdown %` = NA,
                                        Days = 0, Status = conditionMessage(e), check.names = FALSE))
    }))
    do.call(rbind, rows)
  })
  output$screen_kpis <- renderUI({
    x <- screen_result(); ok <- x[x$Status == "OK", ]
    tile <- function(v, l, cls) div(class = paste("kpi", cls), div(class = "kpi-value", v), div(class = "kpi-label", l))
    fluidRow(
      column(3, tile(nrow(ok), "Stocks screened", "k-green")),
      column(3, tile(sum(ok$`Total return %` > 0), "Positive returns", "k-violet")),
      column(3, tile(sum(ok$`Total return %` < 0), "Negative returns", "k-red")),
      column(3, tile(if (nrow(ok)) sprintf("%.1f%%", mean(ok$`Annual return %`)) else "-", "Avg annual return", "k-blue")))
  })
  output$screen_table <- renderDT({
    DT::datatable(screen_result(), rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE, dom = "tip")) %>%
      DT::formatStyle(c("Total return %", "Annual return %", "Max drawdown %"),
                      color = DT::styleInterval(0, c("#f87171", "#34d399")), fontWeight = "600") %>%
      DT::formatStyle("Sharpe", color = DT::styleInterval(0, c("#f87171", "#e5e7ff")))
  })
  
  # ---- Portfolio ----
  portfolio <- reactiveVal(data.frame(Ticker = c("RELIANCE.NS","TCS.NS"), Quantity = c(1,1),
                                      PurchasePrice = c(0,0), stringsAsFactors = FALSE))
  output$portfolio_table <- renderDT({
    DT::datatable(portfolio(), editable=TRUE, rownames=FALSE, options=list(dom="t"))
  })
  observeEvent(input$portfolio_table_cell_edit, {
    portfolio(DT::editData(portfolio(), input$portfolio_table_cell_edit, rownames = FALSE))
  })
  observeEvent(input$portfolio_add, {
    portfolio(rbind(portfolio(), data.frame(Ticker="", Quantity=1, PurchasePrice=0)))
  })
  portfolio_values <- eventReactive(input$portfolio_value, {
    x <- portfolio()
    rows <- lapply(seq_len(nrow(x)), function(i) {
      t <- trimws(as.character(x$Ticker[i]))
      if (!nzchar(t)) return(NULL)
      tryCatch({
        px <- fetch_prices(t, Sys.Date()-30, Sys.Date())
        current <- as.numeric(last(get_adjusted_close(px)))
        qty <- as.numeric(x$Quantity[i]); cost <- as.numeric(x$PurchasePrice[i])
        data.frame(Ticker=t, Quantity=qty, PurchasePrice=cost, LatestPrice=current,
                   MarketValue=qty*current, CostValue=qty*cost,
                   UnrealisedPL=qty*(current-cost), ReturnPct=ifelse(cost>0,(current/cost-1)*100,NA_real_))
      }, error=function(e) data.frame(Ticker=t, Quantity=as.numeric(x$Quantity[i]),
                                      PurchasePrice=as.numeric(x$PurchasePrice[i]), LatestPrice=NA,
                                      MarketValue=NA, CostValue=NA, UnrealisedPL=NA, ReturnPct=NA))
    })
    do.call(rbind, Filter(Negate(is.null), rows))
  })
  output$portfolio_summary <- renderTable({
    x <- portfolio_values()
    validate(need(!is.null(x) && nrow(x)>0, "Click Refresh portfolio valuation."))
    total <- sum(x$MarketValue, na.rm=TRUE)
    data.frame(Metric=c("Total market value","Total cost value","Unrealised P/L"),
               Value=round(c(total,sum(x$CostValue,na.rm=TRUE),sum(x$UnrealisedPL,na.rm=TRUE)),2))
  })
  
  # ---- Downloads ----
  output$download_prices <- downloadHandler(
    filename=function() paste0(gsub("[^A-Za-z0-9._-]","_",ticker_active()), "_prices.csv"),
    content=function(file) write.csv(current_prices(), file, row.names=FALSE)
  )
  output$download_financials <- downloadHandler(
    filename=function() paste0(gsub("[^A-Za-z0-9._-]","_",ticker_active()), "_financials.csv"),
    content=function(file) write.csv(financials(), file, row.names=FALSE)
  )
  output$download_excel <- downloadHandler(
    filename=function() paste0(gsub("[^A-Za-z0-9._-]","_",ticker_active()), "_analysis.xlsx"),
    content=function(file) {
      sheets <- list(Prices=current_prices(), Financials=financials())
      rr <- tryCatch(ratios(), error=function(e) NULL)
      if (!is.null(rr)) sheets$Ratios <- rr
      rk <- tryCatch(risk_metrics(make_returns(prices()), input$rf/100), error=function(e) NULL)
      if (!is.null(rk)) sheets$Risk <- rk
      writexl::write_xlsx(sheets, path=file)
    }
  )
  output$download_report <- downloadHandler(
    filename=function() paste0(gsub("[^A-Za-z0-9._-]","_",ticker_active()), "_research_summary.txt"),
    content=function(file) {
      r <- make_returns(prices())
      risk <- risk_metrics(r, input$rf/100)
      lines <- c(
        "STOCK ANALYSIS STUDIO - RESEARCH SUMMARY",
        paste("Ticker:", ticker_active()),
        paste("Analysis period:", input$from, "to", input$to),
        paste("Generated:", Sys.time()),
        "",
        "IMPORTANT: Educational analysis only. Not investment advice.",
        "Data may be incomplete; verify sources, units, corporate actions and assumptions.",
        "",
        "RISK METRICS",
        capture.output(print(risk, row.names=FALSE)),
        "",
        "FINANCIAL INPUTS",
        capture.output(print(financials(), row.names=FALSE)),
        "",
        "DCF ASSUMPTIONS",
        paste("Latest FCF:", input$fcf0),
        paste("FCF growth (%):", input$growth),
        paste("Forecast years:", input$forecast_years),
        paste("Discount rate (%):", input$wacc),
        paste("Terminal growth (%):", input$terminal_growth),
        paste("Net debt:", input$net_debt),
        paste("Shares outstanding:", input$shares)
      )
      writeLines(lines, file)
    }
  )
}

shinyApp(ui, server)




#####upload##
install.packages("rsconnect")

rsconnect::connectCloudUser()

rsconnect::deployApp()