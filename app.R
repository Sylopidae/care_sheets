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
  
  # --- CRASH-PROOFED GENERATOR ---
  observeEvent(input$btn_gen_all, {
    if(nchar(rv$cfg$pages_base_url) < 5) {
      return(showNotification("Please set your GitHub Pages URL in the GitHub Sync tab first!", type="error"))
    }
    
    withProgress(message = 'Rendering Caresheets...', value = 0, {
      for(i in 1:nrow(rv$df)) {
        tryCatch({
          row <- rv$df[i,]
          # Generate Unique QR
          qr_path <- generate_qr_png(row$id, row$species, rv$cfg$pages_base_url)
          
          # Encode Images
          qr_b64  <- img_to_b64(qr_path)
          moult_b64 <- img_to_b64(row$moult_photo)
          
          # Template Handling
          spec_slug <- slug(row$species)
          temp_path <- file.path("species_templates", paste0(spec_slug, ".txt"))
          husbandry <- if(file.exists(temp_path)) paste(readLines(temp_path), collapse="<br>") else "General husbandry applies."
          
          # Build HTML
          html_content <- paste0('<html><head><title>', row$id, '</title></head><body>
            <h1>', row$species, ' (ID: ', row$id, ')</h1>
            <p><b>Sex:</b> ', row$sex, ' | <b>DLS:</b> ', row$dls_in, ' in</p>
            <h3>Notes</h3><p>', row$notes, '</p>
            <h3>Moult Photo</h3><img src="', moult_b64, '" style="max-width:400px;">
            <h3>Care Guide</h3><p>', husbandry, '</p>
            <hr><img src="', qr_b64, '" style="width:150px;">
            </body></html>')
          
          writeLines(html_content, file.path(CARESHEET_DIR, paste0(row$id, "_", spec_slug, ".html")))
        }, error = function(e) {
          message(paste("Error generating sheet for ID", row$id, ":", e$message))
        })
        incProgress(1/nrow(rv$df))
      }
    })
    showNotification("All caresheets updated successfully.")
  })
  
  observeEvent(input$btn_save_cfg, {
    rv$cfg$pages_base_url <- input$pages_url
    save_config(rv$cfg)
    showNotification("URL Configuration Saved.")
  })
  
  observeEvent(input$btn_push, {
    output$git_log <- renderText({
      tryCatch({
        # 1. Configuration - Loading your specific identity
        cfg <- load_config()
        # Fallbacks if config is empty
        git_email <- if(nchar(cfg$git_email) > 0) cfg$git_email else "joeballenger2005@gmail.com"
        git_name  <- if(nchar(cfg$git_name) > 0) cfg$git_name else "Sylopidae"
        
        # 2. Set Identity (The Fix)
        # We set these locally for this repository to avoid permission issues
        system(sprintf('git config user.email "%s"', git_email))
        system(sprintf('git config user.name "%s"', git_name))
        
        # 3. Add and Commit
        system("git add .", wait = TRUE)
        
        commit_cmd <- sprintf('git commit -m "%s"', input$commit_msg)
        # Capture commit output to see if it worked
        commit_res <- system(commit_cmd, wait = TRUE, intern = TRUE)
        
        # 4. Push
        # Change 'main' to 'master' if your branch uses the old naming convention
        push_res <- system("git push origin main", wait = TRUE, intern = TRUE)
        
        # 5. UI Feedback
        paste(c(
          paste("Identity Set As:", git_name),
          "--- Commit Result ---", commit_res,
          "--- Push Result ---", push_res
        ), collapse = "\n")
        
      }, error = function(e) {
        paste("Critical Error:", e$message)
      })
    })
  })
}

shinyApp(ui, server)