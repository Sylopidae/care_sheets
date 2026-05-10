# ============================================================
#  Spider Farm v8.6  –  app.R
#  All fixes consolidated (May 2026):
#    • QR codes embedded in self-contained HTML caresheets
#    • Moult photo base64-embedded for sex verification
#    • GitHub Pages URL + git identity stored in spider_config.rds
#    • github_push_sync: system2() + -F tempfile commit + -c identity
#      (no CMD shell, no quoting issues, no global git config needed)
#    • No invalid box statuses
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(base64enc)
library(qrcode)          # install.packages("qrcode") if missing

# ── directory layout ─────────────────────────────────────────
DATA_FILE     <- "spider_data.csv"
CONFIG_FILE   <- "spider_config.rds"
MOULT_DIR     <- "moults"
CARESHEET_DIR <- "sheets"
QRCODE_DIR    <- "qrcodes"
REPO_PATH     <- getwd()

for (d in c(MOULT_DIR, CARESHEET_DIR, QRCODE_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  gk <- file.path(d, ".gitkeep")
  if (!file.exists(gk)) writeLines("", gk)
}

# ── persistent config ────────────────────────────────────────
load_config <- function() {
  if (file.exists(CONFIG_FILE)) readRDS(CONFIG_FILE)
  else list(pages_base_url = "",
            git_name       = "Sylopidae",
            git_email      = "joeballenger2005@gmail.com")
}
save_config <- function(cfg) saveRDS(cfg, CONFIG_FILE)
CFG <- load_config()

# ── column definitions ───────────────────────────────────────
SPIDER_COLS <- c(
  "id", "species", "sex", "last_fed", "moult_photo", "notes")

# ── data I/O ─────────────────────────────────────────────────
load_data <- function() {
  if (file.exists(DATA_FILE)) {
    df <- read.csv(DATA_FILE, stringsAsFactors = FALSE)
    for (col in SPIDER_COLS)
      if (!col %in% names(df)) df[[col]] <- NA_character_
    df[, SPIDER_COLS]
  } else {
    as.data.frame(
      setNames(replicate(length(SPIDER_COLS), character(0), simplify = FALSE),
               SPIDER_COLS), stringsAsFactors = FALSE)
  }
}
save_data <- function(df) write.csv(df[, SPIDER_COLS], DATA_FILE, row.names = FALSE)
next_id   <- function(df) if (nrow(df) == 0) 1L else max(as.integer(df$id), na.rm = TRUE) + 1L
slug      <- function(s)  tolower(gsub("[^a-zA-Z0-9]+", "_", trimws(s)))

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nchar(as.character(a[1])) > 0) a else b
}

# ── HTML escape ──────────────────────────────────────────────
he <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("")
  x <- as.character(x)
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"',  "&quot;", x, fixed = TRUE)
  x
}

# ── moult photo helpers ──────────────────────────────────────
save_moult_photo <- function(tmp_path, spider_id, species) {
  ext  <- tools::file_ext(tmp_path)
  dest <- file.path(MOULT_DIR, paste0(spider_id, "_", slug(species), ".", ext))
  file.copy(tmp_path, dest, overwrite = TRUE)
  dest
}

file_to_base64 <- function(path, mime = NULL) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NULL)
  if (is.null(mime)) {
    mime <- switch(tolower(tools::file_ext(path)),
                   jpg = "image/jpeg", jpeg = "image/jpeg",
                   png = "image/png",  gif  = "image/gif",
                   webp = "image/webp", "image/png")
  }
  paste0("data:", mime, ";base64,", base64enc::base64encode(path))
}

# ── QR code helpers ──────────────────────────────────────────
caresheet_url <- function(spider_id, species, base_url) {
  base_url <- sub("/$", "", trimws(base_url))
  paste0(base_url, "/", CARESHEET_DIR, "/", spider_id, "_", slug(species), ".html")
}

generate_qr_png <- function(spider_id, species, base_url) {
  url  <- caresheet_url(spider_id, species, base_url)
  path <- file.path(QRCODE_DIR, paste0(spider_id, "_", slug(species), "_qr.png"))
  png(path, width = 300, height = 300, bg = "white")
  plot(qrcode::qr_code(url))
  dev.off()
  path
}

# ── caresheet HTML generator ─────────────────────────────────
generate_caresheet <- function(row, cs_url = "", qr_b64 = NULL) {
  
  moult_block <- if (!is.na(row$moult_photo) && nchar(row$moult_photo) > 0 &&
                     file.exists(row$moult_photo)) {
    b64 <- file_to_base64(row$moult_photo)
    paste0(
      '<figure style="text-align:center;margin:24px 0;">',
      '<img src="', b64, '" alt="Moult" ',
      'style="max-width:520px;width:100%;border:2px solid #ccc;border-radius:8px;">',
      '<figcaption style="font-size:.85em;color:#555;margin-top:8px;">',
      '&#128247; Moult photograph provided for sex verification. ',
      '<strong>Sex: ', he(row$sex), '</strong></figcaption></figure>'
    )
  } else {
    '<p style="padding:16px;color:#888;font-style:italic;">
     No moult photograph attached to this record.</p>'
  }
  
  qr_block <- if (!is.null(qr_b64) && nchar(cs_url) > 0) {
    paste0(
      '<div style="text-align:center;padding:20px 16px;">',
      '<img src="', qr_b64, '" alt="QR code" ',
      'style="width:160px;height:160px;border:1px solid #ddd;border-radius:6px;"><br>',
      '<small style="color:#555;">Scan to view live caresheet<br>',
      '<a href="', he(cs_url), '" style="color:#1a1a2e;word-break:break-all;">',
      he(cs_url), '</a></small></div>'
    )
  } else {
    '<p style="padding:16px;color:#888;font-style:italic;">
     No GitHub Pages URL configured — QR code not generated.</p>'
  }
  
  sex_col <- switch(row$sex %||% "Unknown",
                    Female = "#e83e8c", Male = "#007bff", "#6c757d")
  
  temp_str <- if (!is.na(row$temp_low_f) && !is.na(row$temp_high_f))
    paste0(row$temp_low_f, "\u00b0F \u2013 ", row$temp_high_f, "\u00b0F")
  else "Not specified"
  
  age_str <- tryCatch({
    yrs <- as.numeric(difftime(Sys.Date(), as.Date(row$dob), units = "days")) / 365.25
    paste0(round(yrs, 1), " yrs (DOB: ", row$dob, ")")
  }, error = function(e) row$dob %||% "Not specified")
  
  field <- function(label, val, default = "Not specified") {
    v <- if (is.null(val) || is.na(val) || nchar(trimws(as.character(val))) == 0)
      default else he(as.character(val))
    paste0('<tr>',
           '<td style="font-weight:600;padding:6px 12px;width:200px;background:#f8f8f8;',
           'border-bottom:1px solid #e0e0e0;">', label, '</td>',
           '<td style="padding:6px 12px;border-bottom:1px solid #e0e0e0;">', v, '</td>',
           '</tr>')
  }
  
  paste0('<!DOCTYPE html><html lang="en"><head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Care Sheet \u2013 ', he(row$species), ' (ID ', row$id, ')</title>
<style>
  body{font-family:"Segoe UI",Arial,sans-serif;max-width:820px;margin:40px auto;
       padding:0 20px;color:#222;background:#fff;}
  h1{margin:0;font-size:1.7em;color:#1a1a2e;}
  .hdr{display:flex;justify-content:space-between;align-items:flex-start;
       flex-wrap:wrap;gap:12px;padding:24px;background:#1a1a2e;color:#fff;
       border-radius:10px 10px 0 0;}
  .badge{display:inline-block;padding:4px 14px;border-radius:20px;
         font-size:.85em;font-weight:700;color:#fff;background:', sex_col, ';}
  .chip{background:rgba(255,255,255,.15);border-radius:6px;
        padding:4px 10px;font-size:.82em;font-family:monospace;}
  table{width:100%;border-collapse:collapse;font-size:.95em;}
  .sec{background:#fff;border:1px solid #e0e0e0;border-radius:8px;
       margin-bottom:20px;overflow:hidden;}
  .sec-hdr{background:#2d2d44;color:#fff;padding:8px 14px;font-weight:700;
            font-size:.9em;letter-spacing:.04em;text-transform:uppercase;}
  .notes{padding:12px;line-height:1.65;white-space:pre-wrap;font-size:.93em;}
  footer{text-align:center;font-size:.78em;color:#888;margin-top:32px;
         border-top:1px solid #e0e0e0;padding-top:12px;}
</style></head><body>
<div class="hdr">
  <div>
    <div class="chip">Spider Farm \u2013 ID #', row$id, '</div>
    <h1 style="margin-top:10px;">', he(row$species), '</h1>
    <div style="font-size:.95em;opacity:.75;margin-top:4px;">
      Individual Care Sheet &amp; Sex Verification Record</div>
  </div>
  <div style="text-align:right;margin-top:8px;">
    <span class="badge">', he(row$sex), '</span><br>
    <span style="font-size:.8em;opacity:.7;margin-top:6px;display:block;">
      Generated ', format(Sys.Date(), "%B %d, %Y"), '</span>
  </div>
</div>
<div class="sec" style="margin-top:0;border-radius:0;">
  <div class="sec-hdr">&#128241; Online Caresheet &amp; QR Code</div>',
         qr_block, '</div>
<div class="sec">
  <div class="sec-hdr">&#9888; Sex Verification \u2013 Moult Photograph</div>
  <div style="padding:16px;">', moult_block,
         '<p style="font-size:.82em;color:#555;text-align:center;margin-top:4px;">
  Compare to published sexing guides for <em>', he(row$species), '</em>.
  Spermathecae (female) appear as small rounded projections between the
  book lungs on the underside of the abdomen.</p></div></div>
<div class="sec">
  <div class="sec-hdr">&#128203; Spider Identity</div><table>',
         field("Record ID",    row$id),
         field("Species",      row$species),
         field("Sex",          row$sex),
         field("Enclosure ID", row$enclosure),
         field("Age",          age_str),
         field("Weight",       if (!is.na(row$weight_g)) paste0(row$weight_g, " g") else NA),
         field("Adult Size",   if (!is.na(row$adult_size_in)) paste0(row$adult_size_in, '"') else NA),
         field("Lifespan",     if (!is.na(row$lifespan_yrs)) paste0(row$lifespan_yrs, " yrs") else NA),
         '</table></div>
<div class="sec">
  <div class="sec-hdr">&#127968; Husbandry Requirements</div><table>',
         field("Temperature",    temp_str),
         field("Humidity",       if (!is.na(row$humidity_pct)) paste0(row$humidity_pct, "%") else NA),
         field("Substrate",      row$substrate),
         field("Enclosure Type", row$enclosure_type),
         field("Enclosure Size", row$enclosure_size),
         field("Temperament",    row$temperament),
         field("Venom Potency",  row$venom_potency),
         '</table></div>
<div class="sec">
  <div class="sec-hdr">&#127829; Feeding &amp; Water</div><table>',
         field("Prey Items",  row$prey_items),
         field("Last Fed",    row$last_fed),
         field("Water Notes", row$water_notes),
         '</table></div>
<div class="sec">
  <div class="sec-hdr">&#128221; Additional Care Notes</div>
  <div class="notes">',
         if (!is.na(row$care_notes) && nchar(trimws(row$care_notes)) > 0)
           he(row$care_notes)
         else '<span style="color:#888;font-style:italic;">No additional notes.</span>',
         '</div></div>
<div class="sec">
  <div class="sec-hdr">&#10084; Health Status at Sale</div><table>',
         field("Health", row$health),
         field("Notes",  row$notes),
         '</table></div>
<footer>Spider Farm &bull; ID #', row$id,
         ' &bull; <em>', he(row$species), '</em> &bull; Generated ',
         format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '<br>
  For the buyer\'s personal reference.</footer>
</body></html>')
}

save_caresheet <- function(row, base_url = "") {
  qr_path <- NULL; qr_b64 <- NULL; cs_url <- ""
  if (nchar(trimws(base_url)) > 0) {
    cs_url  <- caresheet_url(row$id, row$species, base_url)
    qr_path <- tryCatch(generate_qr_png(row$id, row$species, base_url),
                        error = function(e) NULL)
    if (!is.null(qr_path)) qr_b64 <- file_to_base64(qr_path, "image/png")
  }
  html    <- generate_caresheet(row, cs_url = cs_url, qr_b64 = qr_b64)
  cs_path <- file.path(CARESHEET_DIR, paste0(row$id, "_", slug(row$species), ".html"))
  writeLines(html, cs_path, useBytes = FALSE)
  list(cs_path = cs_path, qr_path = qr_path, cs_url = cs_url)
}

# ── GitHub sync ──────────────────────────────────────────────
#   system2() calls git directly — no CMD shell, no quoting issues.
#   Commit message written to temp file (-F) — any characters safe.
#   Identity via -c flags — no global git config required.
github_push_sync <- function(repo_path = REPO_PATH,
                             msg       = format(Sys.time(), "auto-sync %Y-%m-%d %H:%M:%S"),
                             git_name  = "Spider Farm",
                             git_email = "joeballenger2005@gmail.com") {
  
  old_wd <- setwd(repo_path)
  on.exit(setwd(old_wd), add = TRUE)
  
  msg_file <- tempfile(fileext = ".txt")
  writeLines(trimws(msg), msg_file)
  on.exit(unlink(msg_file), add = TRUE)
  
  run_git <- function(args) {
    out    <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
    status <- as.integer(attr(out, "status") %||% 0L)
    list(output = paste(out, collapse = "\n"), status = status)
  }
  
  chk <- run_git(c("rev-parse", "--is-inside-work-tree"))
  if (!grepl("true", chk$output, ignore.case = TRUE))
    return(list(
      cmd    = "git rev-parse",
      output = paste0(
        "ERROR: Not a git repository: ", repo_path,
        "\n\nRun once in a terminal:\n",
        "  git init\n",
        "  git add -A\n",
        "  git commit -m \"initial commit\"\n",
        "  git branch -M main\n",
        "  git remote add origin https://github.com/Sylopidae/care_sheets.git\n",
        "  git push -u origin main --force"),
      status = "FAILED \u2014 not a git repository"
    ))
  
  add    <- run_git(c("add", "-A"))
  commit <- run_git(c(
    "-c", paste0("user.name=",  git_name),
    "-c", paste0("user.email=", git_email),
    "commit", "-F", msg_file
  ))
  push   <- run_git("push")
  
  all_out <- paste0(
    "-- git add --\n",     add$output,
    "\n-- git commit --\n", commit$output,
    "\n-- git push --\n",   push$output
  )
  
  push_failed   <- push$status > 0 ||
    grepl("fatal|error", push$output, ignore.case = TRUE)
  commit_failed <- commit$status > 0 &&
    grepl("error|pathspec|fatal", commit$output, ignore.case = TRUE)
  
  status_msg <- if (push_failed)   paste0("FAILED (push exit ",   push$status,   ")")
  else if (commit_failed) paste0("FAILED (commit exit ", commit$status, ")")
  else "OK"
  
  list(cmd = paste0("system2(git) in ", repo_path),
       output = all_out, status = status_msg)
}

# ── UI ────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "\U0001f577 Spider Farm v8.6"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",    tabName = "dashboard",  icon = icon("tachometer-alt")),
      menuItem("Add / Edit",   tabName = "add_edit",   icon = icon("plus-circle")),
      menuItem("Husbandry",    tabName = "husbandry",  icon = icon("leaf")),
      menuItem("Moult Photos", tabName = "moults",     icon = icon("camera")),
      menuItem("Caresheets",   tabName = "caresheets", icon = icon("file-alt")),
      menuItem("Feeding Log",  tabName = "feeding",    icon = icon("drumstick-bite")),
      menuItem("Analytics",    tabName = "analytics",  icon = icon("chart-bar")),
      menuItem("GitHub Sync",  tabName = "github",     icon = icon("github"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("box_total",   width = 3),
                valueBoxOutput("box_healthy", width = 3),
                valueBoxOutput("box_fed",     width = 3),
                valueBoxOutput("box_species", width = 3)
              ),
              fluidRow(
                box(title = "All Spiders", width = 12, status = "primary",
                    DTOutput("tbl_all"), br(),
                    actionButton("btn_delete", "Delete Selected",
                                 icon = icon("trash"), class = "btn-danger"))
              )
      ),
      
      tabItem(tabName = "add_edit",
              fluidRow(
                box(title = "Add New Spider", width = 6, status = "success",
                    textInput("in_species",   "Species",
                              placeholder = "e.g. Brachypelma hamorii"),
                    textInput("in_enclosure", "Enclosure ID", placeholder = "e.g. T-04"),
                    dateInput("in_dob",       "Date of Birth", value = Sys.Date()),
                    selectInput("in_sex",     "Sex", choices = c("Unknown","Female","Male")),
                    numericInput("in_weight", "Weight (g)", value = 0, min = 0, step = 0.1),
                    dateInput("in_last_fed",  "Last Fed", value = Sys.Date()),
                    selectInput("in_health",  "Health Status",
                                choices = c("Excellent","Good","Fair","Poor","Critical")),
                    textAreaInput("in_notes", "General Notes", rows = 2),
                    actionButton("btn_add", "Add Spider",
                                 icon = icon("plus"), class = "btn-success btn-lg"),
                    br(), br(), verbatimTextOutput("out_add_status")),
                box(title = "Edit Existing Record", width = 6, status = "warning",
                    p("Enter a spider ID, load it, adjust fields, then save."),
                    numericInput("in_edit_id", "Record ID to Edit", value = NA, min = 1),
                    actionButton("btn_load_edit", "Load Record",
                                 icon = icon("download"), class = "btn-default"),
                    hr(),
                    actionButton("btn_update", "Save Changes",
                                 icon = icon("save"), class = "btn-warning btn-lg"),
                    br(), br(), verbatimTextOutput("out_edit_status"))
              )
      ),
      
      tabItem(tabName = "husbandry",
              box(title = "Husbandry Details", width = 8, status = "info",
                  p("Fill in care requirements for a specific spider ID.
               All fields are embedded into the generated caresheet."),
                  numericInput("hus_id", "Spider ID", value = NA, min = 1),
                  fluidRow(
                    column(6,
                           numericInput("hus_temp_lo",  "Temp Low (\u00b0F)",  value=68, min=50, max=110),
                           numericInput("hus_temp_hi",  "Temp High (\u00b0F)", value=78, min=50, max=110),
                           numericInput("hus_humidity", "Humidity (%)",         value=60, min=0,  max=100),
                           textInput("hus_substrate",   "Substrate",
                                     placeholder = "e.g. Coco fibre + peat 4:1"),
                           textInput("hus_enc_type",    "Enclosure Type",
                                     placeholder = "e.g. Terrestrial, Arboreal, Fossorial")),
                    column(6,
                           textInput("hus_enc_size",  "Enclosure Size",
                                     placeholder = 'e.g. 8"x8"x12"'),
                           textInput("hus_prey",      "Prey Items",
                                     placeholder = "e.g. Crickets, dubia roaches"),
                           textInput("hus_water",     "Water Notes",
                                     placeholder = "e.g. Shallow dish; overflow corner monthly"),
                           selectInput("hus_temperament", "Temperament",
                                       choices = c("Docile","Calm","Defensive",
                                                   "Skittish","Aggressive","Unpredictable")),
                           selectInput("hus_venom", "Venom Potency",
                                       choices = c("Not medically significant","Mild",
                                                   "Medically significant","Unknown")),
                           numericInput("hus_size",     'Adult Size (DLS, in")',
                                        value=NA, min=0, step=0.25),
                           numericInput("hus_lifespan", "Typical Lifespan (yrs)",
                                        value=NA, min=0, step=0.5))
                  ),
                  textAreaInput("hus_care_notes", "Additional Care Notes", rows = 4,
                                placeholder = "Moulting notes, communal tolerance, triggers..."),
                  actionButton("btn_save_hus", "Save Husbandry",
                               icon = icon("save"), class = "btn-info btn-lg"),
                  br(), br(), verbatimTextOutput("out_hus_status"))
      ),
      
      tabItem(tabName = "moults",
              fluidRow(
                box(title = "Upload Moult Photo", width = 5, status = "primary",
                    p("Attach a photograph of the moult for sex verification.
                 The image is base64-embedded directly into the caresheet HTML."),
                    numericInput("moult_id", "Spider ID", value = NA, min = 1),
                    fileInput("moult_file", "Moult Image",
                              accept = c("image/jpeg","image/png","image/gif","image/webp"),
                              placeholder = "JPG / PNG / WEBP"),
                    tags$small(tags$em(
                      "Tip: photograph the moult flat, underside up, clearly showing",
                      "the epigastric furrow / spermathecae region.")),
                    br(), br(),
                    actionButton("btn_moult_upload", "Attach Photo",
                                 icon = icon("upload"), class = "btn-primary btn-lg"),
                    br(), br(), verbatimTextOutput("out_moult_status")),
                box(title = "Photo Preview", width = 7, status = "primary",
                    numericInput("moult_preview_id", "Preview Spider ID", value = NA, min = 1),
                    actionButton("btn_moult_preview", "Load Preview",
                                 icon = icon("eye"), class = "btn-default"),
                    br(), br(), uiOutput("moult_preview_ui"))
              ),
              fluidRow(
                box(title = "Moult Photo Index", width = 12, status = "primary",
                    DTOutput("tbl_moults"))
              )
      ),
      
      tabItem(tabName = "caresheets",
              fluidRow(
                box(title = "Generate Caresheet", width = 5, status = "success",
                    p("Builds a self-contained HTML caresheet with the moult photo and",
                      "QR code embedded. Set your GitHub Pages URL in the",
                      strong("GitHub Sync"), "tab first so QR codes generate."),
                    numericInput("cs_id", "Spider ID", value = NA, min = 1),
                    actionButton("btn_gen_cs", "Generate & Save Caresheet",
                                 icon = icon("file-alt"), class = "btn-success btn-lg"),
                    br(), br(), verbatimTextOutput("out_cs_status"),
                    hr(),
                    p(strong("Bulk:"), "generate caresheets for every spider at once."),
                    actionButton("btn_gen_all_cs", "Generate All Caresheets",
                                 icon = icon("layer-group"), class = "btn-warning"),
                    br(), br(), verbatimTextOutput("out_cs_bulk_status")),
                box(title = "Caresheet Preview", width = 7, status = "success",
                    uiOutput("cs_preview_ui"))
              ),
              fluidRow(
                box(title = "Caresheets on Disk", width = 12, status = "primary",
                    DTOutput("tbl_caresheets"), br(),
                    actionButton("btn_refresh_cs", "Refresh List",
                                 icon = icon("sync"), class = "btn-default"))
              )
      ),
      
      tabItem(tabName = "feeding",
              box(title = "Log a Feeding", width = 5, status = "info",
                  numericInput("feed_id",    "Spider ID", value = NA, min = 1),
                  dateInput(  "feed_date",   "Date Fed",  value = Sys.Date()),
                  textInput(  "feed_prey",   "Prey Item", placeholder = "e.g. cricket"),
                  numericInput("feed_count", "Quantity",  value = 1, min = 1),
                  actionButton("btn_feed", "Log Feeding",
                               icon = icon("check"), class = "btn-info"),
                  br(), br(), verbatimTextOutput("out_feed_status")),
              box(title = "Feeding Summary", width = 7, status = "info",
                  DTOutput("tbl_feed_summary"))
      ),
      
      tabItem(tabName = "analytics",
              fluidRow(
                box(title = "Weight Distribution",     width = 6, status = "primary",
                    plotOutput("plot_weight")),
                box(title = "Health Status Breakdown", width = 6, status = "primary",
                    plotOutput("plot_health"))
              ),
              fluidRow(
                box(title = "Species Count", width = 6, status = "primary",
                    plotOutput("plot_species")),
                box(title = "Sex Ratio",     width = 6, status = "primary",
                    plotOutput("plot_sex"))
              )
      ),
      
      tabItem(tabName = "github",
              fluidRow(
                box(title = "Git Identity & GitHub Pages", width = 12, status = "warning",
                    p("These settings are saved permanently to", code("spider_config.rds"),
                      "and used on every push."),
                    fluidRow(
                      column(4,
                             textInput("git_name",  "Git Name",
                                       value = CFG$git_name  %||% "Sylopidae",
                                       placeholder = "Sylopidae")),
                      column(4,
                             textInput("git_email", "Git Email",
                                       value = CFG$git_email %||% "joeballenger2005@gmail.com",
                                       placeholder = "joeballenger2005@gmail.com")),
                      column(4,
                             textInput("pages_url", "GitHub Pages Base URL",
                                       value = CFG$pages_base_url %||% "",
                                       placeholder = "https://sylopidae.github.io/care_sheets"))
                    ),
                    actionButton("btn_save_cfg", "Save Settings",
                                 icon = icon("save"), class = "btn-warning"),
                    br(), br(), verbatimTextOutput("out_cfg_status"))
              ),
              fluidRow(
                box(title = "Push to GitHub", width = 8, status = "success",
                    p("Commits and pushes", code("spider_data.csv"), ",",
                      code("sheets/"), ",", code("moults/"), ", and",
                      code("qrcodes/"), "to your remote repository."),
                    textInput("sync_msg", "Commit Message",
                              value = paste0("update caresheets ",
                                             format(Sys.time(), "%Y-%m-%d"))),
                    textInput("sync_repo", "Repo Path (blank = app directory)",
                              value = REPO_PATH),
                    fluidRow(
                      column(6,
                             actionButton("btn_gen_then_sync", "Generate All + Push",
                                          icon = icon("rocket"), class = "btn-success btn-lg")),
                      column(6,
                             actionButton("btn_sync_only", "Push Current Files",
                                          icon = icon("upload"), class = "btn-info btn-lg"))
                    ),
                    br(), br(), verbatimTextOutput("out_sync_result")),
                box(title = "Last Command", width = 4, status = "primary",
                    verbatimTextOutput("out_sync_cmd"))
              )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    df       = load_data(),
    cfg      = load_config(),
    feed_log = data.frame(spider_id=integer(0), date=character(0),
                          prey=character(0), qty=integer(0),
                          stringsAsFactors=FALSE)
  )
  
  observeEvent(input$btn_save_cfg, {
    rv$cfg$git_name       <- trimws(input$git_name)
    rv$cfg$git_email      <- trimws(input$git_email)
    rv$cfg$pages_base_url <- trimws(input$pages_url)
    save_config(rv$cfg)
    output$out_cfg_status <- renderText(paste0(
      "\u2714 Saved.",
      "\n  Name  : ", rv$cfg$git_name,
      "\n  Email : ", rv$cfg$git_email,
      "\n  Pages : ", rv$cfg$pages_base_url))
  })
  
  output$box_total   <- renderValueBox(
    valueBox(nrow(rv$df), "Total Spiders", icon=icon("spider"), color="purple"))
  output$box_healthy <- renderValueBox({
    n <- sum(rv$df$health %in% c("Excellent","Good"), na.rm=TRUE)
    valueBox(n, "Healthy", icon=icon("heart"), color="green")
  })
  output$box_fed <- renderValueBox({
    n <- sum(as.Date(rv$df$last_fed) >= Sys.Date()-7, na.rm=TRUE)
    valueBox(n, "Fed (last 7d)", icon=icon("drumstick-bite"), color="blue")
  })
  output$box_species <- renderValueBox({
    n <- length(unique(rv$df$species[!is.na(rv$df$species)]))
    valueBox(n, "Species", icon=icon("list"), color="orange")
  })
  
  DISP <- c("id","species","sex","enclosure","weight_g","last_fed","health","moult_photo")
  output$tbl_all <- renderDT({
    d <- rv$df[, intersect(DISP, names(rv$df))]
    d$moult_photo <- ifelse(
      !is.na(d$moult_photo) & nchar(d$moult_photo) > 0 & file.exists(d$moult_photo),
      "\u2705 Attached", "\u274c None")
    datatable(d, selection="single", rownames=FALSE,
              options=list(pageLength=15, scrollX=TRUE))
  })
  
  observeEvent(input$btn_delete, {
    sel <- input$tbl_all_rows_selected; req(length(sel)>0)
    rv$df <- rv$df[-sel,]; save_data(rv$df)
  })
  
  observeEvent(input$btn_add, {
    req(nchar(trimws(input$in_species))>0)
    nr <- as.data.frame(
      setNames(as.list(rep(NA_character_, length(SPIDER_COLS))), SPIDER_COLS),
      stringsAsFactors=FALSE)
    nr$id <- as.character(next_id(rv$df)); nr$species <- trimws(input$in_species)
    nr$enclosure <- trimws(input$in_enclosure); nr$dob <- as.character(input$in_dob)
    nr$sex <- input$in_sex; nr$weight_g <- as.character(input$in_weight)
    nr$last_fed <- as.character(input$in_last_fed); nr$health <- input$in_health
    nr$notes <- trimws(input$in_notes)
    nr$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rv$df <- rbind(rv$df, nr); save_data(rv$df)
    output$out_add_status <- renderText(
      paste0("\u2714 Added: ", nr$species, " (ID ", nr$id, ")"))
  })
  
  observeEvent(input$btn_load_edit, {
    req(!is.na(input$in_edit_id))
    idx <- which(as.integer(rv$df$id)==input$in_edit_id); req(length(idx)==1)
    row <- rv$df[idx,]
    updateTextInput(session,    "in_species",   value=row$species   %||% "")
    updateTextInput(session,    "in_enclosure", value=row$enclosure %||% "")
    updateDateInput(session,    "in_dob",
                    value=tryCatch(as.Date(row$dob), error=function(e) Sys.Date()))
    updateSelectInput(session,  "in_sex",       selected=row$sex    %||% "Unknown")
    updateNumericInput(session, "in_weight",    value=as.numeric(row$weight_g) %||% 0)
    updateDateInput(session,    "in_last_fed",
                    value=tryCatch(as.Date(row$last_fed), error=function(e) Sys.Date()))
    updateSelectInput(session,  "in_health",    selected=row$health %||% "Good")
    updateTextAreaInput(session,"in_notes",     value=row$notes     %||% "")
    output$out_edit_status <- renderText(
      "Record loaded \u2013 adjust fields above then click Save Changes.")
  })
  
  observeEvent(input$btn_update, {
    req(!is.na(input$in_edit_id))
    idx <- which(as.integer(rv$df$id)==input$in_edit_id); req(length(idx)==1)
    rv$df[idx,"species"]   <- trimws(input$in_species)
    rv$df[idx,"enclosure"] <- trimws(input$in_enclosure)
    rv$df[idx,"dob"]       <- as.character(input$in_dob)
    rv$df[idx,"sex"]       <- input$in_sex
    rv$df[idx,"weight_g"]  <- as.character(input$in_weight)
    rv$df[idx,"last_fed"]  <- as.character(input$in_last_fed)
    rv$df[idx,"health"]    <- input$in_health
    rv$df[idx,"notes"]     <- trimws(input$in_notes)
    rv$df[idx,"timestamp"] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    save_data(rv$df)
    output$out_edit_status <- renderText(paste0("\u2714 Updated ID ", input$in_edit_id))
  })
  
  observeEvent(input$btn_save_hus, {
    req(!is.na(input$hus_id))
    idx <- which(as.integer(rv$df$id)==input$hus_id); req(length(idx)==1)
    rv$df[idx,"temp_low_f"]     <- as.character(input$hus_temp_lo)
    rv$df[idx,"temp_high_f"]    <- as.character(input$hus_temp_hi)
    rv$df[idx,"humidity_pct"]   <- as.character(input$hus_humidity)
    rv$df[idx,"substrate"]      <- trimws(input$hus_substrate)
    rv$df[idx,"enclosure_type"] <- input$hus_enc_type
    rv$df[idx,"enclosure_size"] <- trimws(input$hus_enc_size)
    rv$df[idx,"prey_items"]     <- trimws(input$hus_prey)
    rv$df[idx,"water_notes"]    <- trimws(input$hus_water)
    rv$df[idx,"temperament"]    <- input$hus_temperament
    rv$df[idx,"venom_potency"]  <- input$hus_venom
    rv$df[idx,"adult_size_in"]  <- as.character(input$hus_size)
    rv$df[idx,"lifespan_yrs"]   <- as.character(input$hus_lifespan)
    rv$df[idx,"care_notes"]     <- input$hus_care_notes
    rv$df[idx,"timestamp"]      <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    save_data(rv$df)
    output$out_hus_status <- renderText(
      paste0("\u2714 Husbandry saved for ID ", input$hus_id,
             " (", rv$df[idx,"species"], ")"))
  })
  
  observeEvent(input$btn_moult_upload, {
    req(!is.na(input$moult_id), !is.null(input$moult_file))
    idx <- which(as.integer(rv$df$id)==input$moult_id); req(length(idx)==1)
    dest <- save_moult_photo(input$moult_file$datapath,
                             input$moult_id, rv$df[idx,"species"])
    rv$df[idx,"moult_photo"] <- dest
    rv$df[idx,"timestamp"]   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    save_data(rv$df)
    output$out_moult_status <- renderText(paste0(
      "\u2714 Photo saved: ", dest,
      "\nRegenerate the caresheet to embed the updated image."))
  })
  
  observeEvent(input$btn_moult_preview, {
    req(!is.na(input$moult_preview_id))
    idx <- which(as.integer(rv$df$id)==input$moult_preview_id)
    output$moult_preview_ui <- renderUI({
      if (length(idx)==0) return(p("Spider ID not found."))
      path <- rv$df[idx,"moult_photo"]
      if (is.na(path) || !file.exists(path))
        return(p(style="color:#888;font-style:italic;", "No moult photo attached yet."))
      b64 <- file_to_base64(path)
      tags$div(
        tags$p(strong("Species: "), rv$df[idx,"species"],
               " | ", strong("Sex: "), rv$df[idx,"sex"]),
        tags$img(src=b64,
                 style="max-width:100%;max-height:420px;
                        border-radius:6px;border:2px solid #ccc;"),
        tags$p(style="color:#555;font-size:.85em;margin-top:6px;",
               "Saved as: ", path))
    })
  })
  
  output$tbl_moults <- renderDT({
    dm <- rv$df[!is.na(rv$df$moult_photo) & nzchar(rv$df$moult_photo),]
    if (nrow(dm)==0)
      return(datatable(data.frame(Message="No moult photos attached yet.")))
    sh <- dm[, c("id","species","sex","moult_photo")]
    sh$exists <- file.exists(sh$moult_photo)
    datatable(sh, rownames=FALSE, options=list(pageLength=20, scrollX=TRUE))
  })
  
  do_generate_cs <- function(spider_id) {
    idx <- which(as.integer(rv$df$id)==spider_id)
    if (length(idx)!=1) return(list(ok=FALSE, msg=paste("ID", spider_id, "not found.")))
    row      <- rv$df[idx,]
    base_url <- rv$cfg$pages_base_url %||% ""
    res <- tryCatch(save_caresheet(row, base_url=base_url), error=function(e) NULL)
    if (is.null(res)) return(list(ok=FALSE, msg=paste("Error generating ID", spider_id)))
    qr_note <- if (!is.null(res$qr_path))
      paste0("\n   QR \u2192 ", res$qr_path, "\n   URL: ", res$cs_url)
    else "\n   (No QR \u2014 save GitHub Pages URL in Sync tab first)"
    qr_b64 <- if (!is.null(res$qr_path)) file_to_base64(res$qr_path,"image/png") else NULL
    html_pv <- generate_caresheet(row, cs_url=res$cs_url, qr_b64=qr_b64)
    list(ok=TRUE, res=res, html=html_pv,
         msg=paste0("\u2714 ", res$cs_path, qr_note))
  }
  
  observeEvent(input$btn_gen_cs, {
    req(!is.na(input$cs_id))
    out <- do_generate_cs(input$cs_id)
    output$out_cs_status <- renderText(out$msg)
    if (out$ok)
      output$cs_preview_ui <- renderUI(
        tags$iframe(srcdoc=out$html,
                    style="width:100%;height:600px;
                           border:1px solid #ddd;border-radius:6px;"))
  })
  
  observeEvent(input$btn_gen_all_cs, {
    ids  <- as.integer(rv$df$id)
    msgs <- vapply(ids, function(id) do_generate_cs(id)$msg, character(1))
    output$out_cs_bulk_status <- renderText(
      paste0("Done (", length(ids), " spider(s)):\n", paste(msgs, collapse="\n")))
  })
  
  output$tbl_caresheets <- renderDT({
    input$btn_refresh_cs
    files <- list.files(CARESHEET_DIR, pattern="\\.html$", full.names=FALSE)
    if (length(files)==0)
      return(datatable(data.frame(File="No caresheets generated yet.")))
    info <- file.info(file.path(CARESHEET_DIR, files))
    datatable(
      data.frame(File=files, Size_KB=round(info$size/1024,1),
                 Modified=format(info$mtime,"%Y-%m-%d %H:%M"),
                 stringsAsFactors=FALSE),
      rownames=FALSE, options=list(pageLength=20, scrollX=TRUE))
  })
  
  observeEvent(input$btn_feed, {
    req(!is.na(input$feed_id), nchar(trimws(input$feed_prey))>0)
    idx <- which(as.integer(rv$df$id)==input$feed_id)
    if (length(idx)==1) {
      rv$df[idx,"last_fed"]  <- as.character(input$feed_date)
      rv$df[idx,"timestamp"] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      save_data(rv$df)
    }
    rv$feed_log <- rbind(rv$feed_log, data.frame(
      spider_id=input$feed_id, date=as.character(input$feed_date),
      prey=trimws(input$feed_prey), qty=input$feed_count, stringsAsFactors=FALSE))
    output$out_feed_status <- renderText(paste0("\u2714 Logged for ID ", input$feed_id))
  })
  output$tbl_feed_summary <- renderDT(
    datatable(rv$feed_log, rownames=FALSE,
              options=list(pageLength=10, order=list(list(1,"desc")))))
  
  output$plot_weight <- renderPlot({
    req(nrow(rv$df)>0)
    ggplot(rv$df, aes(x=as.numeric(weight_g))) +
      geom_histogram(fill="#3c8dbc", color="white", bins=20) +
      labs(x="Weight (g)", y="Count") + theme_minimal()
  })
  output$plot_health <- renderPlot({
    req(nrow(rv$df)>0)
    lvls <- c("Excellent","Good","Fair","Poor","Critical")
    ggplot(rv$df %>% count(health) %>% mutate(health=factor(health,levels=lvls)),
           aes(x=health, y=n, fill=health)) +
      geom_col(show.legend=FALSE) +
      scale_fill_manual(values=c(Excellent="#00a65a", Good="#00c0ef",
                                 Fair="#f39c12", Poor="#dd4b39", Critical="#6c757d")) +
      labs(x=NULL, y="Count") + theme_minimal()
  })
  output$plot_species <- renderPlot({
    req(nrow(rv$df)>0)
    ggplot(rv$df %>% count(species) %>% arrange(desc(n)) %>% head(15),
           aes(x=reorder(species,n), y=n)) +
      geom_col(fill="#605ca8") + coord_flip() + labs(x=NULL, y="Count") + theme_minimal()
  })
  output$plot_sex <- renderPlot({
    req(nrow(rv$df)>0)
    ggplot(rv$df %>% count(sex), aes(x="", y=n, fill=sex)) +
      geom_col(width=1) + coord_polar("y") +
      scale_fill_manual(values=c(Female="#e83e8c", Male="#007bff", Unknown="#adb5bd")) +
      labs(fill="Sex", title="Sex Ratio") + theme_void()
  })
  
  do_sync <- function(regen_first=FALSE) {
    repo <- if (nchar(trimws(input$sync_repo))>0) trimws(input$sync_repo) else REPO_PATH
    msg  <- if (nchar(trimws(input$sync_msg))>0) trimws(input$sync_msg)
    else paste0("auto-sync ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    log_lines <- character(0)
    if (regen_first) {
      ids <- as.integer(rv$df$id)
      log_lines <- c(log_lines,
                     paste0("Generating caresheets + QR codes for ", length(ids), " spider(s)..."))
      log_lines <- c(log_lines,
                     vapply(ids, function(id) do_generate_cs(id)$msg, character(1)), "")
    }
    log_lines <- c(log_lines, "Running git add / commit / push...")
    withProgress(message="Pushing to GitHub\u2026", value=0.6, {
      res <- github_push_sync(
        repo_path = repo,
        msg       = msg,
        git_name  = rv$cfg$git_name  %||% "Spider Farm",
        git_email = rv$cfg$git_email %||% "joeballenger2005@gmail.com"
      )
    })
    log_lines <- c(log_lines, paste0("Status : ", res$status), "", res$output)
    output$out_sync_result <- renderText(paste(log_lines, collapse="\n"))
    output$out_sync_cmd    <- renderText(res$cmd)
  }
  
  observeEvent(input$btn_gen_then_sync, { do_sync(regen_first=TRUE)  })
  observeEvent(input$btn_sync_only,     { do_sync(regen_first=FALSE) })
}

shinyApp(ui=ui, server=server)
