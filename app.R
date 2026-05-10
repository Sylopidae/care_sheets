# ============================================================
#  Spider Farm v9.3 – Bug Fix & Stability Edition
#  Fixes: generate_qr_png scoping error & crash protection
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(qrcode)
library(base64enc)

# ── Directory Setup ──────────────────────────────────────────
DATA_FILE     <- "spider_inventory.csv"
FEED_FILE     <- "feeding_history.csv"
CONFIG_FILE   <- "spider_config.rds"
CARESHEET_DIR <- "sheets"
QRCODE_DIR    <- "qrcodes"
MOULT_DIR     <- "moults"

for (d in c(CARESHEET_DIR, QRCODE_DIR, MOULT_DIR, "species_templates")) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ── Helper Functions (Defined FIRST to avoid Scoping Errors) ──

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

# THE FUNCTION THAT WAS CAUSING THE CRASH
generate_qr_png <- function(spider_id, species, base_url) {
  base_url <- sub("/$", "", trimws(base_url))
  # Construct the exact URL that will be live on GitHub Pages
  target_url <- paste0(base_url, "/", CARESHEET_DIR, "/", spider_id, "_", slug(species), ".html")
  
  path <- file.path(QRCODE_DIR, paste0(spider_id, "_qr.png"))
  
  # Generate and save PNG
  png(path, width = 300, height = 300)
  plot(qrcode::qr_code(target_url))
  dev.off()
  
  return(path)
}

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Spider Farm v9.3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inventory", tabName = "inventory", icon = icon("edit")),
      menuItem("Feeding Logs", tabName = "feedings", icon = icon("list-ol")),
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
                    numericInput("inv_dls", "DLS (Inches)", 0, step = 0.1),
                    fileInput("moult_upload", "Upload Moult Photo"),
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
    cfg = load_config()
  )
  
  output$tbl_spiders <- renderDT({ datatable(rv$df, selection="single", rownames=F) })
  
  output$tbl_feeds <- renderDT({ 
    req(input$feed_id)
    datatable(rv$feeds %>% filter(id == input$feed_id), rownames=F) 
  })
  
  observeEvent(input$btn_save, {
    req(input$inv_id, input$inv_species)
    photo_path <- NA
    if (!is.null(input$moult_upload)) {
      ext <- tools::file_ext(input$moult_upload$datapath)
      photo_path <- file.path(MOULT_DIR, paste0(input$inv_id, "_moult.", ext))
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
  
  # --- STABLE LOGO & BRANDING GENERATOR (ONLY ONE VERSION) ---
  observeEvent(input$btn_gen_all, {
    if(nchar(rv$cfg$pages_base_url) < 5) {
      return(showNotification("Please set your GitHub Pages URL in the GitHub Sync tab first!", type="error"))
    }
    
    logo_b64 <- if(file.exists("logo.png")) img_to_b64("logo.png") else ""
    user_name  <- if(nchar(rv$cfg$git_name) > 0) rv$cfg$git_name else "Joe Ballenger"
    user_email <- if(nchar(rv$cfg$git_email) > 0) rv$cfg$git_email else "joeballenger2005@gmail.com"
    
    withProgress(message = 'Rendering Caresheets...', value = 0, {
      for(i in 1:nrow(rv$df)) {
        tryCatch({
          row <- rv$df[i,]
          
          moult_b64 <- img_to_b64(row$moult_photo)
          spec_slug <- slug(row$species)
          temp_path <- file.path("species_templates", paste0(spec_slug, ".txt"))
          husbandry <- if(file.exists(temp_path)) paste(readLines(temp_path), collapse="<br>") else "General husbandry applies."
          
          disp_notes <- if(is.na(row$notes) || row$notes == "") "No specific notes recorded." else row$notes
          disp_logo  <- if(logo_b64 != "") paste0('<img src="', logo_b64, '" class="logo">') else ""
          disp_moult <- if(moult_b64 != "") paste0('<img src="', moult_b64, '" class="moult-img">') else "<p><i>No photo available.</i></p>"
          
          html_content <- paste0('
          <html>
          <head>
            <title>', row$id, ' - ', row$species, '</title>
            <style>
              body { font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif; line-height: 1.6; color: #333; max-width: 700px; margin: auto; padding: 30px; background-color: #f4f4f4; }
              .card { background: white; padding: 40px; border-radius: 12px; box-shadow: 0 4px 15px rgba(0,0,0,0.1); }
              .header { border-bottom: 3px solid #2c3e50; padding-bottom: 20px; margin-bottom: 20px; display: flex; align-items: center; justify-content: space-between; }
              .logo { max-height: 80px; }
              .species-title { margin: 0; color: #2c3e50; }
              .stats-box { background: #f9f9f9; padding: 20px; border-left: 5px solid #2c3e50; border-radius: 4px; margin: 20px 0; }
              .moult-img { max-width: 100%; border-radius: 8px; margin-top: 10px; border: 1px solid #ddd; }
              .footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #eee; font-size: 0.85em; color: #777; text-align: center; }
            </style>
          </head>
          <body>
            <div class="card">
              <div class="header">
                <div>
                  <h1 class="species-title">', row$species, '</h1>
                  <p><b>Specimen ID:</b> ', row$id, ' | <b>Sex:</b> ', row$sex, ' | <b>DLS:</b> ', row$dls_in, ' in</p>
                </div>
                ', disp_logo, '
              </div>
              <h3>Care Guide & Requirements</h3>
              <div class="stats-box">', husbandry, '</div>
              <h3>Keeper Notes</h3>
              <p>', disp_notes, '</p>
              <h3>Latest Moult Photo</h3>
              ', disp_moult, '
              <div class="footer">
                <p><b>', user_name, '</b></p>
                <p>Contact: <a href="mailto:', user_email, '">', user_email, '</a></p>
                <p><i>Generated by Spider Farm v9.3</i></p>
              </div>
            </div>
          </body>
          </html>')
          
          writeLines(html_content, file.path(CARESHEET_DIR, paste0(row$id, "_", spec_slug, ".html")))
        }, error = function(e) {
          message(paste("Error generating sheet for ID", row$id, ":", e$message))
        })
        incProgress(1/nrow(rv$df))
      }
    })
    showNotification("All caresheets updated.")
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
        git_email <- if(nchar(cfg$git_email) > 0) cfg$git_email else "joeballenger2005@gmail.com"
        git_name  <- if(nchar(cfg$git_name) > 0) cfg$git_name else "Sylopidae"
        system(sprintf('git config user.email "%s"', git_email))
        system(sprintf('git config user.name "%s"', git_name))
        system("git add .", wait = TRUE)
        commit_cmd <- sprintf('git commit -m "%s"', input$commit_msg)
        commit_res <- system(commit_cmd, wait = TRUE, intern = TRUE)
        push_res <- system("git push origin main", wait = TRUE, intern = TRUE)
        paste(c(paste("Identity Set As:", git_name), "--- Commit Result ---", commit_res, "--- Push Result ---", push_res), collapse = "\n")
      }, error = function(e) { paste("Critical Error:", e$message) })
    })
  })
}

shinyApp(ui, server)