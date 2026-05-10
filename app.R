# ============================================================
#  Spider Farm v9.1  –  Full GitHub & QR Integration
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(base64enc)
library(qrcode)

# ── Files & Directories ──────────────────────────────────────
DATA_FILE         <- "spider_data.csv"
SPECIES_CARE_FILE <- "species_care.csv"
CONFIG_FILE       <- "spider_config.rds"
MOULT_DIR         <- "moults"
CARESHEET_DIR     <- "sheets"
QRCODE_DIR        <- "qrcodes"
REPO_PATH         <- getwd()

for (d in c(MOULT_DIR, CARESHEET_DIR, QRCODE_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ── Config & Data I/O ────────────────────────────────────────
load_config <- function() {
  if (file.exists(CONFIG_FILE)) readRDS(CONFIG_FILE)
  else list(pages_base_url = "", git_name = "Sylopidae", git_email = "joeballenger2005@gmail.com")
}
save_config <- function(cfg) saveRDS(cfg, CONFIG_FILE)

load_data <- function() {
  cols <- c("id", "species", "dls", "sex", "moult_photo", "timestamp")
  if (file.exists(DATA_FILE)) read.csv(DATA_FILE, stringsAsFactors = FALSE)
  else as.data.frame(setNames(replicate(length(cols), character(0)), cols))
}

load_species_care <- function() {
  cols <- c("species", "temp_low_f", "temp_high_f", "humidity_pct", "substrate", 
            "enclosure_type", "enclosure_size", "prey_items", "water_notes", 
            "temperament", "venom_potency", "lifespan_yrs", "care_notes")
  if (file.exists(SPECIES_CARE_FILE)) read.csv(SPECIES_CARE_FILE, stringsAsFactors = FALSE)
  else as.data.frame(setNames(replicate(length(cols), character(0)), cols))
}

# ── Helpers ──────────────────────────────────────────────────
slug <- function(s) tolower(gsub("[^a-zA-Z0-9]+", "_", trimws(s)))
he   <- function(x) { if(is.na(x) || is.null(x)) "" else htmltools::htmlEscape(as.character(x)) }

# ── QR & Caresheet Engine ─────────────────────────────────────
generate_qr_png <- function(spider_id, species, base_url) {
  url  <- paste0(sub("/$", "", base_url), "/", CARESHEET_DIR, "/", spider_id, "_", slug(species), ".html")
  path <- file.path(QRCODE_DIR, paste0(spider_id, "_", slug(species), "_qr.png"))
  png(path, width = 300, height = 300); plot(qrcode::qr_code(url)); dev.off()
  path
}

generate_caresheet_html <- function(row, care, cs_url = "", qr_b64 = NULL) {
  moult_b64 <- if(!is.na(row$moult_photo) && file.exists(row$moult_photo)) base64enc::dataURI(file = row$moult_photo) else ""
  
  paste0('<!DOCTYPE html><html><head><style>
    body { font-family: "Segoe UI", sans-serif; max-width: 800px; margin: auto; padding: 30px; color: #333; }
    .hdr { background: #1a1a2e; color: white; padding: 25px; border-radius: 10px; }
    .section { border: 1px solid #eee; padding: 20px; margin-top: 20px; border-radius: 8px; }
    .photo { text-align: center; margin: 20px 0; }
    .photo img { max-width: 100%; border: 4px solid #f0f0f0; border-radius: 10px; }
    table { width: 100%; border-collapse: collapse; }
    td { padding: 8px; border-bottom: 1px solid #eee; }
    .label { font-weight: bold; width: 30%; color: #666; }
  </style></head><body>
    <div class="hdr">
      <h1>', he(row$species), '</h1>
      <p>Specimen ID: #', row$id, ' | Sex: ', row$sex, ' | DLS: ', row$dls, '"</p>
    </div>
    <div class="section">
      <h3>Species Care Guide</h3>
      <table>
        <tr><td class="label">Temperature</td><td>', care$temp_low_f, ' - ', care$temp_high_f, '°F</td></tr>
        <tr><td class="label">Humidity</td><td>', care$humidity_pct, '%</td></tr>
        <tr><td class="label">Diet</td><td>', he(care$prey_items), '</td></tr>
        <tr><td class="label">Notes</td><td>', he(care$care_notes), '</td></tr>
      </table>
    </div>
    <div class="section">
      <h3>Specimen Sex Verification</h3>
      <div class="photo">', if(nchar(moult_b64)>0) paste0('<img src="', moult_b64, '">') else "No photo.", '</div>
    </div>
    <div style="text-align:center; margin-top:30px;">
      <img src="', qr_b64, '" width="120"><br>
      <small><a href="', cs_url, '">', cs_url, '</a></small>
    </div>
  </body></html>')
}

# ── GitHub Engine ────────────────────────────────────────────
github_sync <- function(cfg, msg = "update caresheets") {
  old_wd <- setwd(REPO_PATH); on.exit(setwd(old_wd))
  run_git <- function(args) {
    out <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
    status <- as.integer(attr(out, "status") %||% 0L)
    list(output = paste(out, collapse = "\n"), status = status)
  }
  run_git(c("add", "-A"))
  run_git(c("-c", paste0("user.name=", cfg$git_name), "-c", paste0("user.email=", cfg$git_email), "commit", "-m", paste0('"', msg, '"')))
  res <- run_git("push")
  res
}

# ── UI ───────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Spider Farm v9.1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inventory", tabName = "inv", icon = icon("th")),
      menuItem("Add Specimen", tabName = "add", icon = icon("plus")),
      menuItem("Care Library", tabName = "lib", icon = icon("book")),
      menuItem("Settings & Sync", tabName = "sync", icon = icon("github"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("inv", fluidRow(box(width = 12, DTOutput("tbl")))),
      tabItem("add", fluidRow(
        box(title = "New Specimen", width = 6, status = "success",
            textInput("in_sp", "Species"),
            numericInput("in_dls", "DLS (inches)", 1, step = 0.1),
            selectInput("in_sex", "Sex", c("Unknown", "Female", "Male")),
            fileInput("in_img", "Moult Photo"),
            actionButton("btn_add", "Save & Generate Caresheet", class = "btn-success"))
      )),
      tabItem("lib", fluidRow(
        box(title = "Species Care Standards", width = 12,
            uiOutput("sel_sp_ui"),
            fluidRow(
              column(4, numericInput("c_tlo", "Temp Low", 70), numericInput("c_thi", "Temp High", 80)),
              column(4, numericInput("c_hum", "Humidity %", 60), textInput("c_prey", "Prey")),
              column(4, textAreaInput("c_notes", "Care Notes", rows = 4))
            ),
            actionButton("btn_save_care", "Update Library", class = "btn-info"))
      )),
      tabItem("sync", fluidRow(
        box(title = "GitHub Configuration", width = 6,
            textInput("g_url", "GitHub Pages Base URL", value = load_config()$pages_base_url),
            textInput("g_name", "Git Name", value = load_config()$git_name),
            textInput("g_mail", "Git Email", value = load_config()$git_email),
            actionButton("btn_cfg", "Save Settings")),
        box(title = "Push to Cloud", width = 6,
            actionButton("btn_push", "Sync to GitHub", class = "btn-primary", icon = icon("rocket")),
            verbatimTextOutput("git_out"))
      ))
    )
  )
)

# ── Server ───────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(df = load_data(), care = load_species_care(), cfg = load_config())
  
  output$tbl <- renderDT({ datatable(rv$df, rownames = FALSE) })
  output$sel_sp_ui <- renderUI({ selectizeInput("sel_sp", "Select Species", choices = unique(c(rv$df$species, rv$care$species)), options = list(create = TRUE)) })
  
  observeEvent(input$btn_cfg, {
    rv$cfg <- list(pages_base_url = input$g_url, git_name = input$g_name, git_email = input$g_mail)
    save_config(rv$cfg)
  })
  
  observeEvent(input$btn_add, {
    req(input$in_sp)
    new_id <- if(nrow(rv$df)==0) 1 else max(as.integer(rv$df$id)) + 1
    img_path <- NA
    if(!is.null(input$in_img)) {
      img_path <- file.path(MOULT_DIR, paste0(new_id, "_moult.png"))
      file.copy(input$in_img$datapath, img_path)
    }
    
    new_row <- data.frame(id=new_id, species=input$in_sp, dls=input$in_dls, sex=input$in_sex, moult_photo=img_path, timestamp=as.character(Sys.time()))
    rv$df <- rbind(rv$df, new_row)
    write.csv(rv$df, DATA_FILE, row.names = FALSE)
    
    # Generate Caresheet Immediately
    care_row <- rv$care[rv$care$species == input$in_sp, ]
    if(nrow(care_row) == 0) care_row <- as.data.frame(setNames(replicate(13, "Not set"), names(rv$care)))
    
    qr_p <- generate_qr_png(new_id, input$in_sp, rv$cfg$pages_base_url)
    qr_b64 <- base64enc::dataURI(file = qr_p)
    html <- generate_caresheet_html(new_row, care_row, sub("/$", "", rv$cfg$pages_base_url), qr_b64)
    writeLines(html, file.path(CARESHEET_DIR, paste0(new_id, "_", slug(input$in_sp), ".html")))
  })
  
  observeEvent(input$btn_save_care, {
    new_c <- data.frame(species=input$sel_sp, temp_low_f=input$c_tlo, temp_high_f=input$c_thi, humidity_pct=input$c_hum, 
                        substrate="", enclosure_type="", enclosure_size="", prey_items=input$c_prey, water_notes="", 
                        temperament="", venom_potency="", lifespan_yrs=NA, care_notes=input$c_notes)
    rv$care <- rbind(rv$care[rv$care$species != input$sel_sp, ], new_c)
    write.csv(rv$care, SPECIES_CARE_FILE, row.names = FALSE)
  })
  
  observeEvent(input$btn_push, {
    res <- github_sync(rv$cfg)
    output$git_out <- renderText(res$output)
  })
}

shinyApp(ui, server)