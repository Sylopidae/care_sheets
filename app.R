# ============================================================
#  Spider Farm v9.4 – The Growth Edition
#  Features: Dedicated Moult Tracking & Auto-DLS Updates
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(qrcode)
library(base64enc)

# ── Directory Setup ──────────────────────────────────────────
DATA_FILE      <- "spider_inventory.csv"
FEED_FILE      <- "feeding_history.csv"
MOULT_LOG_FILE <- "moult_history.csv" # New tracking file
CONFIG_FILE    <- "spider_config.rds"
CARESHEET_DIR  <- "sheets"
QRCODE_DIR     <- "qrcodes"
MOULT_DIR      <- "moults"

for (d in c(CARESHEET_DIR, QRCODE_DIR, MOULT_DIR, "species_templates")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ── Helper Functions ──────────────────────────────────────────

load_config <- function() {
  if (file.exists(CONFIG_FILE)) readRDS(CONFIG_FILE)
  else list(pages_base_url = "", git_name = "Sylopidae", git_email = "joeballenger2005@gmail.com")
}

save_config <- function(cfg) saveRDS(cfg, CONFIG_FILE)

slug <- function(s) tolower(gsub("[^a-zA-Z0-9]+", "_", trimws(s)))

img_to_b64 <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return("")
  ext <- tools::file_ext(path)
  paste0("data:image/", ext, ";base64,", base64enc::base64encode(path))
}

generate_qr_png <- function(spider_id, species, base_url) {
  base_url <- sub("/$", "", trimws(base_url))
  target_url <- paste0(base_url, "/", CARESHEET_DIR, "/", spider_id, "_", slug(species), ".html")
  path <- file.path(QRCODE_DIR, paste0(spider_id, "_qr.png"))
  
  # Open PNG device with extra height for the label
  png(path, width = 300, height = 350) # Increased height from 300 to 350
  
  # Set margins: bottom margin is larger to fit text
  par(mar = c(4, 0, 0, 0)) 
  
  # Plot the QR code
  plot(qrcode::qr_code(target_url))
  
  # Add the Label (ID and Species)
  # 'line' controls vertical placement, 'cex' controls font size
  mtext(paste0("ID: ", spider_id), side = 1, line = 1, cex = 1.5, font = 2)
  mtext(species, side = 1, line = 2.5, cex = 1.2)
  
  dev.off()
  return(path)
}

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Spider Farm v9.4"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inventory", tabName = "inventory", icon = icon("edit")),
      menuItem("Feeding Logs", tabName = "feedings", icon = icon("list-ol")),
      menuItem("Moult Tracker", tabName = "moults", icon = icon("expand-arrows-alt")), # New Tab
      menuItem("Caresheets", tabName = "sheets", icon = icon("qrcode")),
      menuItem("GitHub Sync", tabName = "github", icon = icon("github"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "inventory",
              fluidRow(
                box(title = "Specimen Editor", width = 4, status = "primary",
                    textInput("inv_id", "Specimen ID"),
                    textInput("inv_species", "Species"),
                    selectInput("inv_sex", "Sex", choices = c("Unknown", "Female", "Male", "Unconfirmed")),
                    numericInput("inv_dls", "Current DLS (Inches)", 0, step = 0.1),
                    fileInput("moult_upload", "Upload Photo"),
                    textAreaInput("inv_notes", "Notes"),
                    actionButton("btn_save", "Save/Update", class = "btn-success", width="100%")),
                box(title = "Inventory List", width = 8, DTOutput("tbl_spiders"))
              )
      ),
      tabItem(tabName = "feedings",
              fluidRow(
                box(title = "Log Feeding", width = 4, status = "warning",
                    textInput("feed_id", "Specimen ID"),
                    dateInput("feed_date", "Date", value = Sys.Date()),
                    numericInput("feed_count", "Qty", 1),
                    textInput("feed_type", "Prey Type"),
                    actionButton("btn_log_feed", "Add Entry", class = "btn-warning")),
                box(title = "History", width = 8, DTOutput("tbl_feeds"))
              )
      ),
      # --- NEW MOULT TRACKER TAB ---
      tabItem(tabName = "moults",
              fluidRow(
                box(title = "Record Moult", width = 4, status = "info",
                    textInput("moult_id", "Specimen ID"),
                    dateInput("moult_date", "Date of Moult", value = Sys.Date()),
                    numericInput("moult_new_dls", "New DLS (Inches)", 0, step = 0.1),
                    p(tags$small("Note: This will update the primary inventory DLS.")),
                    actionButton("btn_log_moult", "Record Moult", class = "btn-info")),
                box(title = "Moult History", width = 8, DTOutput("tbl_moults"))
              )
      ),
      tabItem(tabName = "sheets",
              box(title = "Batch Production", width = 12,
                  p("Generating all caresheets will update every HTML file and QR code in your local folders."),
                  actionButton("btn_gen_all", "Generate All Caresheets", class="btn-danger", icon=icon("sync")))
      ),
      tabItem(tabName = "github",
              box(title = "Cloud Sync", width = 12,
                  textInput("pages_url", "GitHub Pages Base URL", load_config()$pages_base_url),
                  actionButton("btn_save_cfg", "Save URL"), hr(),
                  textInput("commit_msg", "Commit Message", "Update inventory and photos"),
                  actionButton("btn_push", "Push to GitHub", class="btn-success"),
                  verbatimTextOutput("git_log"))
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(
    df = if(file.exists(DATA_FILE)) read.csv(DATA_FILE, stringsAsFactors=F) else data.frame(id=character(), species=character(), sex=character(), dls_in=numeric(), notes=character(), moult_photo=character(), stringsAsFactors=F),
    feeds = if(file.exists(FEED_FILE)) read.csv(FEED_FILE, stringsAsFactors=F) else data.frame(),
    moults = if(file.exists(MOULT_LOG_FILE)) read.csv(MOULT_LOG_FILE, stringsAsFactors=F) else data.frame(id=character(), date=character(), dls_at_moult=numeric(), stringsAsFactors=F),
    cfg = load_config()
  )
  
  output$tbl_spiders <- renderDT({ datatable(rv$df, selection="single", rownames=F) })
  
  output$tbl_feeds <- renderDT({ 
    req(input$feed_id)
    datatable(rv$feeds %>% filter(id == input$feed_id), rownames=F) 
  })
  
  # Render Moult History
  output$tbl_moults <- renderDT({
    req(input$moult_id)
    datatable(rv$moults %>% filter(id == input$moult_id) %>% arrange(desc(date)), rownames=F)
  })
  
  observeEvent(input$btn_save, {
    req(input$inv_id, input$inv_species)
    photo_path <- NA
    if (!is.null(input$moult_upload)) {
      ext <- tools::file_ext(input$moult_upload$datapath)
      photo_path <- file.path(MOULT_DIR, paste0(input$inv_id, "_latest.", ext))
      file.copy(input$moult_upload$datapath, photo_path, overwrite = TRUE)
    } else {
      existing <- rv$df %>% filter(id == input$inv_id) %>% pull(moult_photo)
      if(length(existing) > 0) photo_path <- existing[1]
    }
    new_row <- data.frame(id=input$inv_id, species=input$inv_species, sex=input$inv_sex, dls_in=input$inv_dls, notes=input$inv_notes, moult_photo=photo_path, stringsAsFactors=F)
    idx <- which(rv$df$id == input$inv_id)
    if(length(idx) > 0) rv$df[idx, ] <- new_row else rv$df <- rbind(rv$df, new_row)
    write.csv(rv$df, DATA_FILE, row.names=F)
    showNotification("Inventory Updated.")
  })
  
  observeEvent(input$btn_log_feed, {
    req(input$feed_id)
    new_f <- data.frame(id=input$feed_id, date=as.character(input$feed_date), count=input$feed_count, type=input$feed_type, stringsAsFactors=F)
    rv$feeds <- rbind(rv$feeds, new_f)
    write.csv(rv$feeds, FEED_FILE, row.names=F)
    showNotification("Feeding Logged.")
  })
  
  # Logic for Logging a Moult
  observeEvent(input$btn_log_moult, {
    req(input$moult_id, input$moult_new_dls)
    
    # 1. Update Moult History
    new_m <- data.frame(id=input$moult_id, date=as.character(input$moult_date), dls_at_moult=input$moult_new_dls, stringsAsFactors=F)
    rv$moults <- rbind(rv$moults, new_m)
    write.csv(rv$moults, MOULT_LOG_FILE, row.names=F)
    
    # 2. Automatically update DLS in main Inventory
    idx <- which(rv$df$id == input$moult_id)
    if(length(idx) > 0) {
      rv$df$dls_in[idx] <- input$moult_new_dls
      write.csv(rv$df, DATA_FILE, row.names=F)
    }
    
    showNotification(paste("Moult recorded for", input$moult_id))
  })
  
  observeEvent(input$btn_gen_all, {
    if(nchar(rv$cfg$pages_base_url) < 5) {
      return(showNotification("Please set your GitHub Pages URL in the GitHub Sync tab first!", type="error"))
    }
    
    logo_b64 <- if(file.exists("logo.png")) img_to_b64("logo.png") else ""
    user_name  <- if(nchar(rv$cfg$git_name) > 0) rv$cfg$git_name else "Joe Ballenger"
    user_email <- if(nchar(rv$cfg$git_email) > 0) rv$cfg$git_email else "joeballenger2005@gmail.com"
    
    withProgress(message = 'Rendering Caresheets & QR Codes...', value = 0, {
      for(i in 1:nrow(rv$df)) {
        tryCatch({
          row <- rv$df[i,]
          
          # 1. Generate the labeled QR code image
          generate_qr_png(row$id, row$species, rv$cfg$pages_base_url)
          
          # 2. Prepare data for HTML
          moult_b64 <- img_to_b64(row$moult_photo)
          spec_slug <- slug(row$species)
          temp_path <- file.path("species_templates", paste0(spec_slug, ".txt"))
          husbandry <- if(file.exists(temp_path)) paste(readLines(temp_path), collapse="<br>") else "General husbandry applies."
          
          # 3. Generate the HTML Caresheet
          html_content <- paste0(
            '<html><head><style>
            body { font-family: sans-serif; margin: 20px; line-height: 1.6; }
            .header { text-align: center; border-bottom: 2px solid #333; padding-bottom: 10px; }
            .moult-img { max-width: 300px; display: block; margin: 20px auto; border: 5px solid #eee; }
            .data-box { background: #f9f9f9; padding: 15px; border-radius: 8px; }
            </style></head><body>',
            '<div class="header">',
            if(logo_b64 != "") sprintf('<img src="%s" width="100"><br>', logo_b64) else "",
            '<h1>Specimen: ', row$id, '</h1>',
            '<h3>Species: <i>', row$species, '</i></h3></div>',
            '<div class="data-box">',
            '<p><b>Sex:</b> ', row$sex, '</p>',
            '<p><b>Current DLS:</b> ', row$dls_in, ' inches</p>',
            '<p><b>Notes:</b> ', row$notes, '</p></div>',
            '<h4>Husbandry Requirements</h4><p>', husbandry, '</p>',
            if(moult_b64 != "") sprintf('<h4>Latest Photo</h4><img class="moult-img" src="%s">', moult_b64) else "",
            '<hr><footer style="font-size: 0.8em; text-align: center;">Managed by ', user_name, ' (', user_email, ')</footer>',
            '</body></html>'
          )
          
          writeLines(html_content, file.path(CARESHEET_DIR, paste0(row$id, "_", spec_slug, ".html")))
          
        }, error = function(e) { message(paste("Error ID", row$id, ":", e$message)) })
        incProgress(1/nrow(rv$df))
      }
    })
    showNotification("All caresheets and labeled QR codes updated.")
  })
  
  observeEvent(input$btn_save_cfg, {
    rv$cfg$pages_base_url <- input$pages_url
    save_config(rv$cfg)
    showNotification("URL Configuration Saved.")
  })
  
  observeEvent(input$btn_push, {
    output$git_log <- renderText({
      tryCatch({
        cfg <- load_config()
        system(sprintf('git config user.email "%s"', cfg$git_email))
        system(sprintf('git config user.name "%s"', cfg$git_name))
        system("git add .", wait = TRUE)
        system(sprintf('git commit -m "%s"', input$commit_msg), wait = TRUE)
        res <- system("git push origin main", wait = TRUE, intern = TRUE)
        paste(res, collapse = "\n")
      }, error = function(e) { paste("Error:", e$message) })
    })
  })
}

shinyApp(ui, server)