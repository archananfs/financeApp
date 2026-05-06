# Personal Finance Analytics Platform
### R Shiny + Quarto

---

## Quick start

```r
# 1. Install required packages
install.packages(c(
  "shiny", "bslib", "dplyr", "lubridate", "readr",
  "plotly", "reactable", "stringr", "quarto", "base64enc"
))

# 2. Run the app
shiny::runApp(".")
```

---

## Project structure

```
finance_app/
├── app.R                  # Main Shiny application
├── report.qmd             # Quarto monthly report template
├── R/
│   └── data_pipeline.R    # Shared data helpers + constants
├── data/
│   ├── sample_expenses.csv   # 40 sample transactions (Jan–Apr 2026)
│   └── import_template.csv   # Blank CSV template for import
├── bill_uploads/          # Created automatically — stores bill photos
└── README.md
```

---

## Features

### Dashboard tab
- Monthly KPI cards (total spend, transaction count, daily average, top category)
- Monthly trend bar chart (Plotly)
- Category donut chart (Plotly)
- Live budget progress bars per category
- Top merchants table
- Searchable, paginated transaction table with CSV export

### Add Expense tab

**Manual entry:**
- Date picker, category dropdown, merchant, description, amount
- Optional bill photo attachment (previewed inline)
- All fields saved to the live data store instantly

**Scan bill:**
- Upload JPG/PNG/PDF receipt image (previewed full-size)
- Fill in date, category, merchant, amount alongside the image
- Photo stored to `bill_uploads/` with a timestamped filename
- In production: wire `input$bill_photo$datapath` to Google Vision or AWS Textract for OCR auto-fill

### Import CSV tab
- Upload any CSV with columns: `date, category, merchant, description, amount`
- Preview first 10 rows before confirming import
- Download blank template CSV

### Budgets tab
- Set a monthly EUR budget for each of the 12 categories
- Changes immediately reflected in Dashboard budget bars

### Report tab
- Select month + output format (HTML or PDF)
- Renders `report.qmd` via `quarto::quarto_render()`
- Download generated file directly from the browser
- Requires: `install.packages("quarto")` + Quarto CLI (`quarto.org/docs/get-started/`)

---

## CSV format

```csv
date,category,merchant,description,amount,currency,source,bill_photo
2026-05-01,Groceries,REWE,Weekly shop,87.50,EUR,csv,
2026-05-03,Dining,Vapiano,Lunch,18.40,EUR,csv,
```

- `date`: YYYY-MM-DD
- `category`: one of Groceries / Dining / Transport / Utilities / Health / Leisure / Clothing / Education / Travel / Rent / Insurance / Other
- `currency`: defaults to EUR if blank
- `source` / `bill_photo`: optional, auto-filled by the app

---

## Extending the app

| Feature | How |
|---------|-----|
| OCR bill reading | POST `bill_uploads/<file>` to Google Cloud Vision API; parse returned JSON for total/date/merchant |
| Persistent storage | Replace `rv$expenses` with a SQLite backend using `RSQLite` + `DBI` |
| Email reports | Use `blastula::smtp_send()` to email the rendered Quarto HTML/PDF |
| Multi-user auth | Add `shinyauthr` login panel; store per-user CSVs in `data/<user_id>/` |
| Deploy | `rsconnect::deployApp()` to shinyapps.io or Posit Connect |
