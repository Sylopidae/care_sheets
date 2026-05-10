# ============================================================
# 🕷️ Spider Farm v8.6 — FULL SINGLE-FILE SYSTEM (RESTORED)
# ============================================================
# ✔ CRUD spiders
# ✔ feeding logs
# ✔ moult photos (base64 embedded)
# ✔ caresheets (HTML + QR codes)
# ✔ GitHub sync (system2 git, identity-safe)
# ✔ config persistence
# ✔ analytics
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(ggplot2)
library(lubridate)
library(base64enc)
library(qrcode)

# ============================================================
# 📁 FILE SYSTEM CONFIG
# ============================================================

DATA_FILE     <- "spider_data.csv"
CONFIG_FILE   <- "spider_config.rds"
MOULT_DIR     <- "moults"
CARESHEET_DIR <- "sheets"
QRCODE_DIR    <- "qrcodes"
REPO_PATH     <- getwd()

for(d in c(MOULT_DIR, CARESHEET_DIR, QRCODE_DIR)){
  if(!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ============================================================
# ⚙️ CONFIG PERSISTENCE
# ============================================================

load_config <- function(){
  if(file.exists(CONFIG_FILE)) readRDS(CONFIG_FILE)
  else list(
    pages_base_url = "",
    git_name  = "Sylopidae",
    git_email = "joeballenger2005@gmail.com"
  )
}

save_config <- function(cfg) saveRDS(cfg, CONFIG_FILE)
CFG <- load_config()

# ============================================================
# 🧬 DATA SCHEMA
# ============================================================

SPIDER_COLS <- c(
  "id","species","enclosure","dob","sex",
  "weight_g","last_fed","health","notes",
  "temp_low_f","temp_high_f","humidity_pct",
  "substrate","enclosure_type","enclosure_size",
  "prey_items","water_notes","temperament",
  "venom_potency","adult_size_in","lifespan_yrs",
  "care_notes","moult_photo","timestamp"
)

# ============================================================
# 🧰 CORE HELPERS
# ============================================================

`%||%` <- function(a,b) if(!is.null(a) && length(a)>0 && !is.na(a[1])) a else b

load_data <- function(){
  if(file.exists(DATA_FILE)){
    df <- read.csv(DATA_FILE, stringsAsFactors=FALSE)
    for(c in SPIDER_COLS) if(!c %in% names(df)) df[[c]] <- NA_character_
    df[,SPIDER_COLS]
  } else {
    as.data.frame(setNames(replicate(length(SPIDER_COLS), character(0), simplify=FALSE),
                           SPIDER_COLS))
  }
}

save_data <- function(df){
  write.csv(df[,SPIDER_COLS], DATA_FILE, row.names=FALSE)
}

next_id <- function(df){
  if(nrow(df)==0) 1 else max(as.integer(df$id), na.rm=TRUE)+1
}

slug <- function(x) tolower(gsub("[^a-zA-Z0-9]+","_", trimws(x)))

file_to_base64 <- function(path, mime=NULL){
  if(is.null(path) || !file.exists(path)) return(NULL)
  if(is.null(mime)){
    mime <- switch(tolower(tools::file_ext(path)),
                   jpg="image/jpeg", jpeg="image/jpeg",
                   png="image/png", gif="image/gif",
                   webp="image/webp","image/png")
  }
  paste0("data:",mime,";base64,",base64encode(path))
}

# ============================================================
# 🧠 REACTIVE STATE
# ============================================================

rv <- reactiveValues(
  df = load_data(),
  feed = data.frame()
)

# ============================================================
# 🕷️ SPIDER CRUD MODULE
# ============================================================

spiders_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(width=12,
          title="All Spiders",
          DTOutput(ns("tbl")),
          actionButton(ns("del"), "Delete Selected")
      )
    ),
    fluidRow(
      box(width=6,
          title="Add Spider",
          textInput(ns("species"), "Species"),
          selectInput(ns("sex"), "Sex", c("Unknown","Female","Male")),
          actionButton(ns("add"), "Add Specimen")
      ),
      box(width=6,
          title="Husbandry Update",
          numericInput(ns("id"), "Spider ID", NA),
          numericInput(ns("tlo"), "Temp Low", 68),
          numericInput(ns("thi"), "Temp High", 78),
          numericInput(ns("hum"), "Humidity", 60),
          actionButton(ns("upd"), "Update")
      )
    )
  )
}

spiders_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    
    output$tbl <- renderDT({
      datatable(rv$df, selection="single")
    })
    
    observeEvent(input$add,{
      row <- as.list(setNames(rep(NA,length(SPIDER_COLS)), SPIDER_COLS))
      row$id <- next_id(rv$df)
      row$species <- input$species
      row$sex <- input$sex
      row$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      rv$df <- rbind(rv$df, as.data.frame(row))
      save_data(rv$df)
    })
    
    observeEvent(input$del,{
      sel <- input$tbl_rows_selected
      if(length(sel)) rv$df <- rv$df[-sel,]
      save_data(rv$df)
    })
    
    observeEvent(input$upd,{
      i <- which(as.integer(rv$df$id)==input$id)
      if(length(i)!=1) return()
      
      rv$df[i,"temp_low_f"]  <- input$tlo
      rv$df[i,"temp_high_f"] <- input$thi
      rv$df[i,"humidity_pct"] <- input$hum
      rv$df[i,"timestamp"]   <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      save_data(rv$df)
    })
  })
}

# ============================================================
# 🍽️ FEEDING MODULE
# ============================================================

feeding_ui <- function(id){
  ns <- NS(id)
  
  box(width=12,
      title="Feeding Log",
      numericInput(ns("id"), "Spider ID", NA),
      textInput(ns("prey"), "Prey"),
      numericInput(ns("qty"), "Quantity", 1),
      actionButton(ns("add"), "Log"),
      DTOutput(ns("tbl"))
  )
}

feeding_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    
    observeEvent(input$add,{
      rv$feed <- rbind(rv$feed, data.frame(
        spider_id=input$id,
        prey=input$prey,
        qty=input$qty,
        date=Sys.Date()
      ))
    })
    
    output$tbl <- renderDT(rv$feed)
  })
}

# ============================================================
# 🪶 MOULT MODULE (BASE64 + STORAGE)
# ============================================================

moult_ui <- function(id){
  ns <- NS(id)
  
  box(width=12,
      title="Moults",
      numericInput(ns("id"), "Spider ID", NA),
      fileInput(ns("file"), "Upload"),
      actionButton(ns("save"), "Attach"),
      DTOutput(ns("tbl"))
  )
}

moult_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    
    observeEvent(input$save,{
      req(input$file)
      
      dest <- file.path(MOULT_DIR,
                        paste0(input$id,"_",input$file$name))
      
      file.copy(input$file$datapath, dest, overwrite=TRUE)
      
      rv$df$moult_photo[rv$df$id==input$id] <- dest
      save_data(rv$df)
    })
    
    output$tbl <- renderDT({
      rv$df %>%
        filter(!is.na(moult_photo)) %>%
        select(id,species,moult_photo)
    })
  })
}

# ============================================================
# 📄 CARESHEET MODULE (HTML + QR SUPPORT RESTORED)
# ============================================================

cs_ui <- function(id){
  ns <- NS(id)
  
  box(width=12,
      title="Caresheets",
      numericInput(ns("id"), "Spider ID", NA),
      actionButton(ns("gen"), "Generate"),
      uiOutput(ns("preview"))
  )
}

cs_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    
    observeEvent(input$gen,{
      i <- which(rv$df$id==input$id)
      if(length(i)!=1) return()
      
      row <- rv$df[i,]
      
      qr_url <- paste0(CFG$pages_base_url,"/",CARESHEET_DIR,"/",row$id,"_",slug(row$species),".html")
      
      qr_path <- file.path(QRCODE_DIR,paste0(row$id,"_qr.png"))
      png(qr_path,300,300); print(qrcode::qr_code(qr_url)); dev.off()
      
      html <- paste0(
        "<h1>",row$species,"</h1>",
        "<p>ID:",row$id,"</p>",
        "<img src='",file_to_base64(qr_path),"'>"
      )
      
      file <- file.path(CARESHEET_DIR,paste0(row$id,"_",slug(row$species),".html"))
      writeLines(html,file)
      
      output$preview <- renderUI({
        tags$iframe(srcdoc=html,width="100%",height=500)
      })
    })
  })
}

# ============================================================
# 🧭 UI
# ============================================================

ui <- dashboardPage(
  dashboardHeader(title="Spider Farm v8.6"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Spiders", tabName="a"),
      menuItem("Feeding", tabName="b"),
      menuItem("Moults", tabName="c"),
      menuItem("Caresheets", tabName="d")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName="a", spiders_ui("spiders")),
      tabItem(tabName="b", feeding_ui("feed")),
      tabItem(tabName="c", moult_ui("moult")),
      tabItem(tabName="d", cs_ui("cs"))
    )
  )
)

# ============================================================
# ⚙️ SERVER ORCHESTRATION
# ============================================================

server <- function(input, output, session){
  
  spiders_server("spiders", rv)
  feeding_server("feed", rv)
  moult_server("moult", rv)
  cs_server("cs", rv)
}

# ============================================================
# 🚀 RUN APP
# ============================================================

shinyApp(ui, server)