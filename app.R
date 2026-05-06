# app.R - Personal Finance Analytics Platform
# Run with: shiny::runApp(".")

library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(readr)
library(plotly)
library(reactable)
library(stringr)
library(tidyr)

source("R/data_pipeline.R")

# ---- Helpers -----------------------------------------------------------------

fmt_eur <- function(x) paste0("EUR ", formatC(x, format = "f", digits = 2, big.mark = ","))

status_badge <- function(s) {
        switch(s,
               over    = tags$span(class = "badge bg-danger",              "Over budget"),
               warning = tags$span(class = "badge bg-warning text-dark",   "Near limit"),
               ok      = tags$span(class = "badge bg-success",             "On track")
        )
}

ensure_ids <- function(df) {
        if (!"row_id" %in% names(df) || anyNA(df$row_id)) {
                df$row_id <- seq_len(nrow(df))
        }
        df
}

next_id <- function(df) {
        if (nrow(df) == 0 || !"row_id" %in% names(df)) return(1L)
        max(df$row_id, na.rm = TRUE) + 1L
}

# ---- Edit modal --------------------------------------------------------------

edit_modal <- function(row) {
        modalDialog(
                title     = "Edit expense",
                size      = "m",
                easyClose = TRUE,
                footer = tagList(
                        actionButton("modal_delete", "Delete",       class = "btn btn-danger me-auto", icon = icon("trash")),
                        modalButton("Cancel"),
                        actionButton("modal_save",  "Save changes",  class = "btn btn-primary",        icon = icon("check"))
                ),
                div(class = "row g-3",
                    div(class = "col-md-6",
                        tags$label("Date", class = "form-label fw-medium"),
                        dateInput("modal_date", NULL, value = as.Date(row$date),
                                  format = "yyyy-mm-dd", width = "100%")
                    ),
                    div(class = "col-md-6",
                        tags$label("Category", class = "form-label fw-medium"),
                        selectInput("modal_cat", NULL, choices = CATEGORIES,
                                    selected = row$category, width = "100%", selectize = FALSE)
                    ),
                    div(class = "col-md-6",
                        tags$label("Merchant", class = "form-label fw-medium"),
                        textInput("modal_merchant", NULL, value = row$merchant,
                                  placeholder = "e.g. REWE", width = "100%")
                    ),
                    div(class = "col-md-6",
                        tags$label("Amount (EUR)", class = "form-label fw-medium"),
                        numericInput("modal_amount", NULL, value = row$amount,
                                     min = 0, step = 0.01, width = "100%")
                    ),
                    div(class = "col-12",
                        tags$label("Description", class = "form-label fw-medium"),
                        textInput("modal_desc", NULL, value = row$description,
                                  placeholder = "Brief description", width = "100%")
                    ),
                    div(class = "col-12", uiOutput("modal_validation"))
                )
        )
}

# ---- UI ----------------------------------------------------------------------

ui <- page_navbar(
        title    = "Finance Tracker",
        theme = bs_theme(
                version    = 5,
                bg         = "#FAFAF9",
                fg         = "#1A1A18",
                primary    = "#1D9E75",
                secondary  = "#888780",
                base_font  = font_google("Inter"),
                bootswatch = NULL
        ),
        bg       = "#FFFFFF",
        fillable = FALSE,
        
        tags$head(tags$script(HTML(paste(
                "$(document).on('click','.btn-edit-row',function(){",
                "  Shiny.setInputValue('edit_row_id',$(this).data('id'),{priority:'event'});",
                "});",
                "$(document).on('click','.btn-delete-row',function(){",
                "  Shiny.setInputValue('delete_row_id',$(this).data('id'),{priority:'event'});",
                "});"
        )))),
        
        # ---- Dashboard -------------------------------------------------------------
        nav_panel(
                title = "Dashboard", icon = icon("chart-bar"),
                div(class = "container-fluid py-3",
                    
                    card(class = "mb-3 border-0 shadow-sm",
                         card_body(
                                 div(class = "row g-2 align-items-end",
                                     div(class = "col-auto",
                                         selectInput("dash_month", "Month", choices = NULL,
                                                     width = "180px", selectize = FALSE)
                                     ),
                                     div(class = "col-auto",
                                         selectInput("dash_category", "Category",
                                                     choices = c("All categories" = "all", CATEGORIES),
                                                     width = "180px", selectize = FALSE)
                                     ),
                                     div(class = "col-auto",
                                         actionButton("refresh_dash", "Refresh",
                                                      class = "btn btn-outline-secondary btn-sm mt-1",
                                                      icon  = icon("rotate"))
                                     )
                                 )
                         )
                    ),
                    
                    div(class = "row g-3 mb-3",
                        div(class = "col-6 col-md-3",
                            value_box("Total spent",  textOutput("kpi_total",   inline = TRUE),
                                      showcase = icon("euro-sign"),    theme = "primary")
                        ),
                        div(class = "col-6 col-md-3",
                            value_box("Transactions", textOutput("kpi_count",   inline = TRUE),
                                      showcase = icon("receipt"),      theme = "secondary")
                        ),
                        div(class = "col-6 col-md-3",
                            value_box("Avg per day",  textOutput("kpi_daily",   inline = TRUE),
                                      showcase = icon("calendar-day"), theme = "info")
                        ),
                        div(class = "col-6 col-md-3",
                            value_box("Top category", textOutput("kpi_top_cat", inline = TRUE),
                                      showcase = icon("tag"),          theme = "warning")
                        )
                    ),
                    
                    div(class = "row g-3 mb-3",
                        div(class = "col-md-7",
                            card(class = "shadow-sm h-100",
                                 card_header("Monthly spending trend"),
                                 card_body(plotlyOutput("plot_trend", height = "280px"))
                            )
                        ),
                        div(class = "col-md-5",
                            card(class = "shadow-sm h-100",
                                 card_header("Spending by category"),
                                 card_body(plotlyOutput("plot_donut", height = "280px"))
                            )
                        )
                    ),
                    
                    div(class = "row g-3 mb-3",
                        div(class = "col-md-6",
                            card(class = "shadow-sm",
                                 card_header("Budget tracker"),
                                 card_body(uiOutput("budget_bars"))
                            )
                        ),
                        div(class = "col-md-6",
                            card(class = "shadow-sm",
                                 card_header("Top merchants"),
                                 card_body(reactableOutput("tbl_merchants", height = "260px"))
                            )
                        )
                    ),
                    
                    card(class = "shadow-sm",
                         card_header(
                                 div(class = "d-flex justify-content-between align-items-center",
                                     span("All transactions"),
                                     downloadButton("download_csv", "Export CSV",
                                                    class = "btn btn-sm btn-outline-secondary")
                                 )
                         ),
                         card_body(reactableOutput("tbl_transactions"))
                    )
                )
        ),
        
        # ---- Add Expense -----------------------------------------------------------
        nav_panel(
                title = "Add Expense", icon = icon("plus-circle"),
                div(class = "container py-4", style = "max-width:780px;",
                    
                    navset_card_tab(
                            nav_panel(
                                    title = tags$span(icon("pencil"), " Manual entry"),
                                    div(class = "p-3",
                                        div(class = "row g-3",
                                            div(class = "col-md-6",
                                                tags$label("Date", class = "form-label fw-medium"),
                                                dateInput("entry_date", NULL, value = Sys.Date(),
                                                          format = "yyyy-mm-dd", width = "100%")
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Category", class = "form-label fw-medium"),
                                                selectInput("entry_cat", NULL, choices = CATEGORIES,
                                                            width = "100%", selectize = FALSE)
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Merchant / store", class = "form-label fw-medium"),
                                                textInput("entry_merchant", NULL,
                                                          placeholder = "e.g. REWE, Netflix", width = "100%")
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Amount (EUR)", class = "form-label fw-medium"),
                                                numericInput("entry_amount", NULL, value = NA,
                                                             min = 0, step = 0.01, width = "100%")
                                            ),
                                            div(class = "col-12",
                                                tags$label("Description", class = "form-label fw-medium"),
                                                textInput("entry_desc", NULL,
                                                          placeholder = "e.g. Weekly groceries", width = "100%")
                                            ),
                                            div(class = "col-12",
                                                tags$label("Attach bill photo (optional)", class = "form-label fw-medium"),
                                                fileInput("entry_photo", NULL,
                                                          accept        = c("image/jpeg","image/png","image/gif","image/webp"),
                                                          placeholder   = "No photo selected",
                                                          buttonLabel   = tags$span(icon("camera"), " Choose photo")),
                                                uiOutput("entry_photo_preview")
                                            ),
                                            div(class = "col-12",
                                                actionButton("btn_add", "Add expense",
                                                             class = "btn btn-primary px-4", icon = icon("check")),
                                                tags$span(class = "ms-3", uiOutput("add_status"))
                                            )
                                        )
                                    )
                            ),
                            
                            nav_panel(
                                    title = tags$span(icon("camera"), " Scan bill"),
                                    div(class = "p-3",
                                        div(class = "alert alert-info",
                                            icon("circle-info"), " ",
                                            tags$strong("Upload a bill photo."),
                                            " Fill in the details alongside the image."
                                        ),
                                        div(class = "row g-3",
                                            div(class = "col-12",
                                                fileInput("bill_photo", "Bill / receipt image",
                                                          accept      = c("image/jpeg","image/png","image/gif",
                                                                          "image/webp","application/pdf"),
                                                          buttonLabel = tags$span(icon("upload"), " Upload"),
                                                          placeholder = "JPG, PNG or PDF"),
                                                uiOutput("bill_preview")
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Date on bill", class = "form-label fw-medium"),
                                                dateInput("bill_date", NULL, value = Sys.Date(), width = "100%")
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Category", class = "form-label fw-medium"),
                                                selectInput("bill_cat", NULL, choices = CATEGORIES,
                                                            width = "100%", selectize = FALSE)
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Merchant", class = "form-label fw-medium"),
                                                textInput("bill_merchant", NULL,
                                                          placeholder = "As shown on receipt", width = "100%")
                                            ),
                                            div(class = "col-md-6",
                                                tags$label("Total amount (EUR)", class = "form-label fw-medium"),
                                                numericInput("bill_amount", NULL, value = NA,
                                                             min = 0, step = 0.01, width = "100%")
                                            ),
                                            div(class = "col-12",
                                                tags$label("Description", class = "form-label fw-medium"),
                                                textInput("bill_desc", NULL,
                                                          placeholder = "Brief note", width = "100%")
                                            ),
                                            div(class = "col-12",
                                                actionButton("btn_add_bill", "Save bill expense",
                                                             class = "btn btn-primary px-4", icon = icon("check")),
                                                tags$span(class = "ms-3", uiOutput("bill_status"))
                                            )
                                        )
                                    )
                            )
                    ),
                    
                    div(class = "mt-4",
                        h6("Recently added", class = "text-secondary mb-2"),
                        reactableOutput("tbl_recent")
                    )
                )
        ),
        
        # ---- Import CSV ------------------------------------------------------------
        nav_panel(
                title = "Import CSV", icon = icon("file-csv"),
                div(class = "container py-4", style = "max-width:780px;",
                    card(class = "mb-3 shadow-sm",
                         card_header("Upload expense file"),
                         card_body(
                                 div(class = "alert alert-secondary mb-3",
                                     icon("circle-info"), " ",
                                     "CSV must have: ", tags$code("date, category, merchant, description, amount"),
                                     tags$br(),
                                     downloadLink("dl_template",
                                                  tags$span(icon("download"), " Download template CSV"))
                                 ),
                                 fileInput("csv_upload", NULL, accept = ".csv",
                                           buttonLabel = tags$span(icon("folder-open"), " Choose CSV"),
                                           placeholder = "No file chosen", width = "100%"),
                                 uiOutput("csv_preview_ui")
                         )
                    ),
                    uiOutput("csv_import_btn_ui")
                )
        ),
        
        # ---- Budgets ---------------------------------------------------------------
        nav_panel(
                title = "Budgets", icon = icon("sliders"),
                div(class = "container py-4", style = "max-width:680px;",
                    card(class = "shadow-sm",
                         card_header("Set monthly budgets (EUR)"),
                         card_body(
                                 div(class = "row g-2",
                                     lapply(names(DEFAULT_BUDGETS), function(cat) {
                                             div(class = "col-md-6",
                                                 div(class = "d-flex align-items-center gap-2 mb-2",
                                                     div(style = paste0(
                                                             "width:10px;height:10px;border-radius:50%;flex-shrink:0;background:",
                                                             CATEGORY_COLORS[[cat]]
                                                     )),
                                                     tags$label(cat, class = "form-label mb-0 flex-grow-1",
                                                                style = "font-size:14px;"),
                                                     numericInput(paste0("budget_", gsub(" ", "_", cat)), NULL,
                                                                  value = DEFAULT_BUDGETS[[cat]],
                                                                  min = 0, step = 10, width = "110px")
                                                 )
                                             )
                                     })
                                 ),
                                 actionButton("save_budgets", "Save budgets",
                                              class = "btn btn-primary mt-2", icon = icon("save"))
                         )
                    )
                )
        ),
        
        # ---- Report ----------------------------------------------------------------
        nav_panel(
                title = "Report", icon = icon("file-pdf"),
                div(class = "container py-4", style = "max-width:680px;",
                    card(class = "shadow-sm",
                         card_header("Generate monthly report"),
                         card_body(
                                 div(class = "row g-3",
                                     div(class = "col-md-6",
                                         selectInput("report_month", "Select month", choices = NULL,
                                                     width = "100%", selectize = FALSE)
                                     ),
                                     div(class = "col-md-6",
                                         selectInput("report_format", "Output format",
                                                     choices = c("HTML" = "html", "PDF" = "pdf"),
                                                     width = "100%", selectize = FALSE)
                                     ),
                                     div(class = "col-12",
                                         div(class = "alert alert-info",
                                             icon("info-circle"), " Requires Quarto CLI to be installed."
                                         )
                                     ),
                                     div(class = "col-12",
                                         actionButton("btn_render", "Render report",
                                                      class = "btn btn-success px-4", icon = icon("file-export")),
                                         tags$span(class = "ms-3", uiOutput("render_status"))
                                     )
                                 )
                         )
                    )
                )
        )
)

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {
        
        rv <- reactiveValues(
                expenses   = load_csv("data/sample_expenses.csv") %>% ensure_ids(),
                budgets    = DEFAULT_BUDGETS,
                editing_id = NULL
        )
        
        observe({
                months  <- rv$expenses %>% pull(month) %>% unique() %>% sort(decreasing = TRUE)
                choices <- setNames(as.character(months), format(months, "%B %Y"))
                updateSelectInput(session, "dash_month",   choices = choices, selected = choices[1])
                updateSelectInput(session, "report_month", choices = choices, selected = choices[1])
        })
        
        dash_data <- reactive({
                df <- rv$expenses
                if (!is.null(input$dash_month) && nchar(input$dash_month) > 0)
                        df <- df %>% filter(month == as.Date(input$dash_month))
                if (!is.null(input$dash_category) && input$dash_category != "all")
                        df <- df %>% filter(category == input$dash_category)
                df
        }) %>% bindEvent(input$dash_month, input$dash_category, rv$expenses, ignoreNULL = FALSE)
        
        output$kpi_total   <- renderText(fmt_eur(sum(dash_data()$amount, na.rm = TRUE)))
        output$kpi_count   <- renderText(nrow(dash_data()))
        output$kpi_daily   <- renderText({
                df <- dash_data()
                if (nrow(df) == 0) return("EUR 0.00")
                days <- as.numeric(difftime(max(df$date), min(df$date), units = "days")) + 1
                fmt_eur(sum(df$amount, na.rm = TRUE) / max(days, 1))
        })
        output$kpi_top_cat <- renderText({
                df <- dash_data()
                if (nrow(df) == 0) return("-")
                df %>% group_by(category) %>% summarise(t = sum(amount), .groups = "drop") %>%
                        slice_max(t, n = 1) %>% pull(category)
        })
        
        output$plot_trend <- renderPlotly({
                df <- rv$expenses %>%
                        group_by(month) %>%
                        summarise(total = sum(amount, na.rm = TRUE), .groups = "drop") %>%
                        arrange(month)
                plot_ly(df, x = ~month, y = ~total, type = "bar",
                        marker = list(color = "#1D9E75", opacity = 0.85),
                        hovertemplate = "%{x|%b %Y}: EUR %{y:.2f}<extra></extra>") %>%
                        layout(
                                yaxis = list(title = "", tickprefix = "EUR ", gridcolor = "#EBEBEB"),
                                xaxis = list(title = "", tickformat = "%b %Y"),
                                plot_bgcolor  = "rgba(0,0,0,0)",
                                paper_bgcolor = "rgba(0,0,0,0)",
                                margin = list(t = 10, b = 40, l = 60, r = 10),
                                bargap = 0.35
                        ) %>%
                        config(displayModeBar = FALSE)
        })
        
        output$plot_donut <- renderPlotly({
                df <- dash_data() %>%
                        group_by(category) %>%
                        summarise(total = sum(amount, na.rm = TRUE), .groups = "drop")
                colors <- unname(CATEGORY_COLORS[df$category])
                plot_ly(df, labels = ~category, values = ~total, type = "pie", hole = 0.52,
                        marker = list(colors = colors, line = list(color = "#FFFFFF", width = 2)),
                        textinfo = "label+percent",
                        hovertemplate = "%{label}: EUR %{value:.2f}<extra></extra>") %>%
                        layout(
                                showlegend    = FALSE,
                                plot_bgcolor  = "rgba(0,0,0,0)",
                                paper_bgcolor = "rgba(0,0,0,0)",
                                margin = list(t = 10, b = 10, l = 10, r = 10)
                        ) %>%
                        config(displayModeBar = FALSE)
        })
        
        output$budget_bars <- renderUI({
                status <- budget_status(dash_data(), rv$budgets)
                tagList(lapply(seq_len(nrow(status)), function(i) {
                        row   <- status[i, ]
                        pct   <- min(row$pct, 100)
                        color <- switch(row$status, over = "danger", warning = "warning", ok = "success")
                        div(class = "mb-3",
                            div(class = "d-flex justify-content-between align-items-center mb-1",
                                div(class = "d-flex align-items-center gap-2",
                                    div(style = paste0("width:8px;height:8px;border-radius:50%;background:",
                                                       CATEGORY_COLORS[[row$category]])),
                                    tags$strong(row$category, style = "font-size:13px;")
                                ),
                                div(class = "d-flex align-items-center gap-2",
                                    tags$small(class = "text-secondary",
                                               fmt_eur(row$total), " / ", fmt_eur(row$budget)
                                    ),
                                    status_badge(row$status)
                                )
                            ),
                            div(class = "progress", style = "height:7px;",
                                div(class = paste0("progress-bar bg-", color),
                                    role = "progressbar",
                                    style = paste0("width:", pct, "%;"),
                                    `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100)
                            )
                        )
                }))
        })
        
        output$tbl_merchants <- renderReactable({
                reactable(top_merchants(dash_data()),
                          columns = list(
                                  merchant = colDef(name = "Merchant"),
                                  total    = colDef(name = "Total (EUR)", format = colFormat(digits = 2, prefix = "EUR ")),
                                  n        = colDef(name = "Visits", maxWidth = 70)
                          ),
                          striped = TRUE, highlight = TRUE, compact = TRUE,
                          defaultPageSize = 8, showPageSizeOptions = FALSE
                )
        })
        
        output$tbl_transactions <- renderReactable({
                df <- dash_data() %>%
                        arrange(desc(date)) %>%
                        ensure_ids() %>%
                        mutate(actions = NA_character_)
                
                reactable(df,
                          columns = list(
                                  row_id      = colDef(show = FALSE),
                                  month       = colDef(show = FALSE),
                                  week        = colDef(show = FALSE),
                                  year        = colDef(show = FALSE),
                                  currency    = colDef(show = FALSE),
                                  bill_photo  = colDef(show = FALSE),
                                  date        = colDef(name = "Date",
                                                       format = colFormat(date = TRUE, locales = "de-DE"),
                                                       maxWidth = 105),
                                  category    = colDef(name = "Category", maxWidth = 130,
                                                       cell = function(v) {
                                                               col <- CATEGORY_COLORS[[v]]
                                                               tags$span(style = paste0(
                                                                       "background:", col, "22;color:", col,
                                                                       ";padding:2px 8px;border-radius:12px;font-size:12px;font-weight:500"
                                                               ), v)
                                                       }
                                  ),
                                  merchant    = colDef(name = "Merchant"),
                                  description = colDef(name = "Description"),
                                  amount      = colDef(name = "Amount",
                                                       format = colFormat(digits = 2, prefix = "EUR "),
                                                       maxWidth = 110, align = "right"),
                                  source      = colDef(name = "Via", maxWidth = 70),
                                  actions     = colDef(name = "", maxWidth = 95, sortable = FALSE,
                                                       cell = function(value, index) {
                                                               rid <- df$row_id[index]
                                                               div(style = "display:flex;gap:4px;",
                                                                   tags$button(
                                                                           class     = "btn btn-sm btn-outline-secondary btn-edit-row",
                                                                           style     = "padding:2px 8px;font-size:13px;",
                                                                           `data-id` = rid,
                                                                           "Edit"
                                                                   ),
                                                                   tags$button(
                                                                           class     = "btn btn-sm btn-outline-danger btn-delete-row",
                                                                           style     = "padding:2px 8px;font-size:13px;",
                                                                           `data-id` = rid,
                                                                           "Del"
                                                                   )
                                                               )
                                                       }
                                  )
                          ),
                          searchable = TRUE, striped = TRUE, highlight = TRUE,
                          compact = TRUE, defaultPageSize = 12
                )
        })
        
        observeEvent(input$edit_row_id, {
                rid <- as.integer(input$edit_row_id)
                row <- rv$expenses %>% filter(row_id == rid)
                if (nrow(row) == 0) return()
                rv$editing_id <- rid
                showModal(edit_modal(row[1, ]))
        })
        
        observeEvent(input$modal_save, {
                if (is.null(input$modal_merchant) || nchar(trimws(input$modal_merchant)) == 0) {
                        output$modal_validation <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Merchant required.")
                        )
                        return()
                }
                if (is.null(input$modal_amount) || is.na(input$modal_amount)) {
                        output$modal_validation <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Amount required.")
                        )
                        return()
                }
                rid      <- rv$editing_id
                new_date <- as.Date(input$modal_date)
                rv$expenses <- rv$expenses %>%
                        mutate(
                                date        = if_else(row_id == rid, new_date,                       date),
                                category    = if_else(row_id == rid, input$modal_cat,                category),
                                merchant    = if_else(row_id == rid, input$modal_merchant,           merchant),
                                description = if_else(row_id == rid, input$modal_desc,               description),
                                amount      = if_else(row_id == rid, as.numeric(input$modal_amount), amount),
                                month       = if_else(row_id == rid, floor_date(new_date, "month"),  month),
                                week        = if_else(row_id == rid, floor_date(new_date, "week"),   week),
                                year        = if_else(row_id == rid, year(new_date),                 year)
                        )
                rv$editing_id <- NULL
                removeModal()
                showNotification("Expense updated!", type = "message", duration = 2)
        })
        
        observeEvent(input$delete_row_id, {
                rv$expenses <- rv$expenses %>% filter(row_id != as.integer(input$delete_row_id))
                showNotification("Expense deleted.", type = "warning", duration = 2)
        })
        
        observeEvent(input$modal_delete, {
                if (!is.null(rv$editing_id))
                        rv$expenses <- rv$expenses %>% filter(row_id != rv$editing_id)
                rv$editing_id <- NULL
                removeModal()
                showNotification("Expense deleted.", type = "warning", duration = 2)
        })
        
        output$download_csv <- downloadHandler(
                filename = function() paste0("expenses_", Sys.Date(), ".csv"),
                content  = function(file) write_csv(rv$expenses, file)
        )
        
        output$dl_template <- downloadHandler(
                filename = "import_template.csv",
                content  = function(file) file.copy("data/import_template.csv", file)
        )
        
        output$entry_photo_preview <- renderUI({
                req(input$entry_photo)
                f <- input$entry_photo
                tags$img(
                        src   = base64enc::dataURI(file = f$datapath, mime = f$type),
                        style = "max-height:200px;border-radius:8px;border:1px solid #ddd;margin-top:4px;"
                )
        })
        
        observeEvent(input$btn_add, {
                if (is.null(input$entry_merchant) || nchar(trimws(input$entry_merchant)) == 0) {
                        output$add_status <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Merchant required.")
                        )
                        return()
                }
                if (is.null(input$entry_amount) || is.na(input$entry_amount)) {
                        output$add_status <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Amount required.")
                        )
                        return()
                }
                photo_name <- ""
                if (!is.null(input$entry_photo)) {
                        photo_name <- paste0("manual_", format(Sys.time(), "%Y%m%d_%H%M%S"),
                                             "_", input$entry_photo$name)
                        dir.create("bill_uploads", showWarnings = FALSE)
                        file.copy(input$entry_photo$datapath, file.path("bill_uploads", photo_name))
                }
                d <- as.Date(input$entry_date)
                new_row <- tibble(
                        row_id      = next_id(rv$expenses),
                        date        = d,
                        category    = input$entry_cat,
                        merchant    = trimws(input$entry_merchant),
                        description = ifelse(nchar(trimws(input$entry_desc)) > 0,
                                             trimws(input$entry_desc), input$entry_cat),
                        amount      = as.numeric(input$entry_amount),
                        currency    = "EUR",
                        source      = "manual",
                        bill_photo  = photo_name,
                        month       = floor_date(d, "month"),
                        week        = floor_date(d, "week"),
                        year        = year(d)
                )
                rv$expenses <- bind_rows(rv$expenses, new_row) %>% arrange(date)
                output$add_status <- renderUI(
                        tags$span(class = "text-success", icon("check-circle"), " Expense added!")
                )
                updateTextInput(session, "entry_merchant", value = "")
                updateNumericInput(session, "entry_amount", value = NA)
                updateTextInput(session, "entry_desc", value = "")
        })
        
        output$bill_preview <- renderUI({
                req(input$bill_photo)
                f <- input$bill_photo
                if (grepl("image", f$type)) {
                        tagList(
                                tags$img(
                                        src   = base64enc::dataURI(file = f$datapath, mime = f$type),
                                        style = "max-height:320px;border-radius:8px;border:1px solid #ddd;margin-bottom:12px;"
                                ),
                                tags$p(class = "text-secondary", style = "font-size:12px;",
                                       icon("paperclip"), " ", f$name, " - ", round(f$size / 1024, 1), " KB")
                        )
                } else {
                        div(class = "alert alert-secondary", icon("file-pdf"), " PDF: ", f$name)
                }
        })
        
        observeEvent(input$btn_add_bill, {
                if (is.null(input$bill_photo)) {
                        output$bill_status <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Upload a photo first.")
                        )
                        return()
                }
                if (is.null(input$bill_merchant) || nchar(trimws(input$bill_merchant)) == 0) {
                        output$bill_status <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Merchant required.")
                        )
                        return()
                }
                if (is.null(input$bill_amount) || is.na(input$bill_amount)) {
                        output$bill_status <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"), " Amount required.")
                        )
                        return()
                }
                photo_name <- paste0("bill_", format(Sys.time(), "%Y%m%d_%H%M%S"),
                                     "_", input$bill_photo$name)
                dir.create("bill_uploads", showWarnings = FALSE)
                file.copy(input$bill_photo$datapath, file.path("bill_uploads", photo_name))
                d <- as.Date(input$bill_date)
                new_row <- tibble(
                        row_id      = next_id(rv$expenses),
                        date        = d,
                        category    = input$bill_cat,
                        merchant    = trimws(input$bill_merchant),
                        description = ifelse(nchar(trimws(input$bill_desc)) > 0,
                                             trimws(input$bill_desc), input$bill_cat),
                        amount      = as.numeric(input$bill_amount),
                        currency    = "EUR",
                        source      = "bill_scan",
                        bill_photo  = photo_name,
                        month       = floor_date(d, "month"),
                        week        = floor_date(d, "week"),
                        year        = year(d)
                )
                rv$expenses <- bind_rows(rv$expenses, new_row) %>% arrange(date)
                output$bill_status <- renderUI(
                        tags$span(class = "text-success", icon("check-circle"), " Bill saved!")
                )
                updateTextInput(session, "bill_merchant", value = "")
                updateNumericInput(session, "bill_amount", value = NA)
                updateTextInput(session, "bill_desc", value = "")
        })
        
        output$tbl_recent <- renderReactable({
                df <- rv$expenses %>% arrange(desc(date)) %>% head(8) %>%
                        select(date, category, merchant, amount, source, bill_photo)
                reactable(df,
                          columns = list(
                                  date       = colDef(name = "Date",
                                                      format = colFormat(date = TRUE, locales = "de-DE"),
                                                      maxWidth = 110),
                                  category   = colDef(name = "Category", maxWidth = 120),
                                  merchant   = colDef(name = "Merchant"),
                                  amount     = colDef(name = "Amount",
                                                      format = colFormat(digits = 2, prefix = "EUR "),
                                                      maxWidth = 110, align = "right"),
                                  source     = colDef(name = "Via", maxWidth = 90),
                                  bill_photo = colDef(name = "Photo", maxWidth = 70,
                                                      cell = function(v) {
                                                              if (!is.na(v) && nchar(v) > 0)
                                                                      tags$span(style = "color:#1D9E75;", icon("image"))
                                                              else
                                                                      tags$span(style = "color:#ccc;", "-")
                                                      }
                                  )
                          ),
                          compact = TRUE, highlight = TRUE,
                          defaultPageSize = 8, showPageSizeOptions = FALSE
                )
        })
        
        imported_df <- reactive({
                req(input$csv_upload)
                tryCatch(load_csv(input$csv_upload$datapath), error = function(e) NULL)
        })
        
        output$csv_preview_ui <- renderUI({
                df <- imported_df(); req(df)
                div(
                        div(class = "alert alert-success mt-2",
                            icon("check-circle"), " ", nrow(df), " rows found - preview below."),
                        reactableOutput("csv_preview_tbl")
                )
        })
        
        output$csv_preview_tbl <- renderReactable({
                df <- imported_df(); req(df)
                reactable(
                        df %>% head(10) %>% select(date, category, merchant, description, amount),
                        compact = TRUE, striped = TRUE, highlight = TRUE
                )
        })
        
        output$csv_import_btn_ui <- renderUI({
                req(imported_df())
                div(class = "mt-2",
                    actionButton("btn_import_csv", "Import into tracker",
                                 class = "btn btn-primary", icon = icon("file-import")),
                    tags$span(class = "ms-3", uiOutput("import_status"))
                )
        })
        
        observeEvent(input$btn_import_csv, {
                df <- imported_df(); req(df)
                df <- df %>% mutate(row_id = seq(next_id(rv$expenses), length.out = nrow(df)))
                rv$expenses <- bind_rows(rv$expenses, df) %>% distinct() %>% arrange(date)
                output$import_status <- renderUI(
                        tags$span(class = "text-success", icon("check-circle"),
                                  paste0(" ", nrow(df), " rows imported!"))
                )
        })
        
        observeEvent(input$save_budgets, {
                new_budgets <- sapply(names(DEFAULT_BUDGETS), function(cat) {
                        val <- input[[paste0("budget_", gsub(" ", "_", cat))]]
                        if (is.null(val) || is.na(val)) DEFAULT_BUDGETS[[cat]] else val
                })
                rv$budgets <- new_budgets
                showNotification("Budgets saved!", type = "message", duration = 3)
        })
        
        observeEvent(input$btn_render, {
                output$render_status <- renderUI(
                        tags$span(class = "text-secondary", icon("spinner", class = "fa-spin"), " Rendering...")
                )
                month_sel <- input$report_month
                fmt       <- input$report_format
                tryCatch({
                        tmp_data <- file.path(tempdir(), "report_data.csv")
                        rv$expenses %>% filter(as.character(month) == month_sel) %>% write_csv(tmp_data)
                        out_file <- file.path(tempdir(), paste0("report_", month_sel, ".", fmt))
                        quarto::quarto_render(
                                input          = "report.qmd",
                                output_format  = fmt,
                                execute_params = list(month = month_sel, data_path = tmp_data),
                                output_file    = out_file
                        )
                        output$render_status <- renderUI(tagList(
                                tags$span(class = "text-success", icon("check-circle"), " Done! "),
                                downloadButton("dl_report", "Download report",
                                               class = "btn btn-sm btn-outline-success")
                        ))
                        output$dl_report <- downloadHandler(
                                filename = function() basename(out_file),
                                content  = function(file) file.copy(out_file, file)
                        )
                }, error = function(e) {
                        output$render_status <- renderUI(
                                tags$span(class = "text-danger", icon("exclamation-circle"),
                                          " Error: ", conditionMessage(e))
                        )
                })
        })
}

shinyApp(ui = ui, server = server)