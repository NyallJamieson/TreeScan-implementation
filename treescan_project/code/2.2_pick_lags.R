# Set to true if you want to change (or re-assess) the lags you're using
initial_lags <- c(1)

script_path <- file.path(parent_dir, "code", "2.2_pick_lags.R")

library(png)
library(tcltk)
library(timeDate)

get_lags_with_timeout <- function(timeout = 30, default = initial_lags) {
  tt <- tktoplevel()
  tkwm.title(tt, "Lag Selection")
  
  label <- tklabel(
    tt,
    text = paste(
      "Look at your lag plot.\n",
      "Enter new lags separated by commas,\n",
      "or wait", timeout, "seconds to keep existing lags:",
      paste(default, collapse = ", ")
    )
  )
  tkpack(label, padx = 10, pady = 10)
  
  entry <- tkentry(tt, width = 20)
  tkpack(entry, padx = 10, pady = 5)
  
  result <- default
  
  on_ok <- function() {
    val <- trimws(tclvalue(tkget(entry)))
    if (nzchar(val)) {
      parsed <- suppressWarnings(as.numeric(trimws(strsplit(val, ",")[[1]])))
      if (!any(is.na(parsed))) {
        result <<- parsed
      } else {
        message("Invalid input. Keeping existing lags: ",
                paste(default, collapse = ", "))
      }
    }
    if (as.logical(tkwinfo("exists", tt))) tkdestroy(tt)
  }
  
  ok_button <- tkbutton(tt, text = "OK", command = on_ok)
  tkpack(ok_button, pady = 10)
  
  tcl("after", as.integer(timeout * 1000), function() {
    if (as.logical(tkwinfo("exists", tt))) {
      message("No input received. Keeping existing lags: ",
              paste(default, collapse = ", "))
      tkdestroy(tt)
    }
  })
  
  tkwait.window(tt)
  result
}

is_weekend_or_us_holiday <- function(date = final_date) {
  date <- as.Date(date)
  
  # Weekend?
  is_weekend <- weekdays(date) %in% c("Saturday", "Sunday")
  
  # US federal holidays for the relevant year
  hols <- as.Date(holidayNYSE(as.numeric(format(date, "%Y"))))
  is_holiday <- date %in% hols
  
  is_weekend || is_holiday
}

if (isTRUE(new_month) || isTRUE(first_time)) {
  if (is_weekend_or_us_holiday()) {
    message("Today is a weekend or US holiday. Keeping existing lags: ",
            paste(initial_lags, collapse = ", "))
  } else {
    img <- readPNG(file.path(parent_dir, "lag", "plots",
                             paste0("lag_curve_", year_month, ".png")))
    plot.new()
    rasterImage(img, 0, 0, 1, 1)
    
    line_x <- 2
    
    if (!isTRUE(server)){
      
      new_lags <- get_lags_with_timeout(timeout = 30, default = initial_lags)
    
    } else {
      
      get_lags_with_timeout <- function(timeout = 30, default = initial_lags) {
        if (!requireNamespace("shiny", quietly = TRUE)) {
          message("Package 'shiny' is not installed. Keeping existing lags: ",
                  paste(default, collapse = ", "))
          return(default)
        }
        
        default_text <- paste(default, collapse = ",")
        
        ui <- shiny::fluidPage(
          shiny::tags$head(
            shiny::tags$script(shiny::HTML(sprintf("
        setTimeout(function() {
          var btn = document.getElementById('keep_default');
          if (btn) btn.click();
        }, %d);
      ", as.integer(timeout * 1000))))
          ),
          shiny::titlePanel("Lag Selection"),
          shiny::p("Look at your lag plot."),
          shiny::p(sprintf(
            "Enter new lags separated by commas, or wait %s seconds to keep: %s",
            timeout, paste(default, collapse = ", ")
          )),
          shiny::textInput("lags", "Lags", value = default_text),
          shiny::fluidRow(
            shiny::column(6, shiny::actionButton("ok", "OK")),
            shiny::column(6, shiny::actionButton("keep_default", "Keep Existing"))
          )
        )
        
        server <- function(input, output, session) {
          parse_lags <- function(x) {
            x <- trimws(x)
            if (!nzchar(x)) return(default)
            vals <- suppressWarnings(as.numeric(trimws(strsplit(x, ",")[[1]])))
            if (any(is.na(vals))) return(NULL)
            vals
          }
          
          shiny::observeEvent(input$ok, {
            parsed <- parse_lags(input$lags)
            if (is.null(parsed)) {
              shiny::showNotification(
                "Invalid input. Use comma-separated numbers like 1,4,8",
                type = "error"
              )
            } else {
              shiny::stopApp(parsed)
            }
          })
          
          shiny::observeEvent(input$keep_default, {
            shiny::stopApp(default)
          })
        }
        
        viewer <- shiny::dialogViewer("Lag Selection", width = 500, height = 220)
        
        result <- shiny::runGadget(
          shiny::shinyApp(ui, server),
          viewer = viewer,
          stopOnCancel = TRUE
        )
        
        if (is.null(result)) {
          message("No input received. Keeping existing lags: ",
                  paste(default, collapse = ", "))
          return(default)
        }
        
        result
      }
      
      new_lags <- get_lags_with_timeout(timeout = 30, default = initial_lags)
    }
    
    new_line <- paste0("initial_lags <- c(", paste(new_lags, collapse = ","), ")")
    
    lines <- readLines(script_path)
    lines[line_x] <- new_line
    writeLines(lines, script_path)
    
    initial_lags <- new_lags
  }
}
