library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(googlesheets4)
gs4_auth(path = "service-account.json")

states <- c(
  state.name,
  "",
  "District of Columbia",
  "Puerto Rico"
)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
  body {
    background-color: #120018;
    color: white;
  }

  .container-fluid {
    background-color: #120018;
    color: white;
  }

  .well {
    background-color: #1c0026;
    color: white;
    border: 1px solid #3a0a52;
  }

  .form-control,
  .selectize-input,
  .selectize-control.single .selectize-input {
    background-color: #120018 !important;
    color: white !important;
    border: 1px solid #4d1a66 !important;
  }

  .selectize-dropdown,
  .selectize-dropdown-content {
    background-color: #120018 !important;
    color: white !important;
  }

  .selectize-dropdown .option {
    background-color: #120018 !important;
    color: white !important;
  }

  .selectize-dropdown .active {
    background-color: #3a0a52 !important;
    color: white !important;
  }

  .form-control:focus,
  .selectize-input.focus {
    background-color: #120018 !important;
    color: white !important;
    border-color: #ff8c00 !important;
    box-shadow: 0 0 5px #ff8c00 !important;
  }

  .control-label,
  .help-block,
  .text-output,
  p,
  div,
  label,
  h1,
  h2,
  h3,
  h4,
  h5,
  h6 {
    color: white !important;
  }

  .btn-primary {
    background-color: #ff8c00;
    border-color: #ff8c00;
    color: white;
    font-weight: bold;
  }

  .btn-primary:hover,
  .btn-primary:focus,
  .btn-primary:active {
    background-color: #e67e00;
    border-color: #e67e00;
    color: white;
  }

  hr {
    border-top: 1px solid #4d1a66;
  }
"))
  ),
  
  titlePanel("Trick-or-Treat Tracker"),
  
  div(
    style = "margin-bottom: 20px; font-size: 16px;",
    p(
      "Press the ",
      strong("Trick-or-Treat"),
      " button every time someone comes to your door to trick-or-treat. ",
      "As trick-or-treaters begin to arrive, a graph will appear showing the distribution of arrival times.",
      "Providing your time zone, state/territory, community type, and house type is completely optional."
    )),
  
  fluidRow(
    
    column(
      width = 4,
      
      selectInput(
        "timezone",
        "Time Zone",
        choices = c("", "Eastern", "Central", "Mountain", "Pacific", "Alaska", "Hawaii")
      ),
      
      selectInput(
        "state",
        "State/Territory",
        choices = sort(states)
      ),
      
      selectInput(
        "community",
        "Type of Community",
        choices = c("", "Rural", "Suburban", "Urban")
      ),
      
      selectInput(
        "house_type",
        "Type of House",
        choices = c(
          "",
          "Detached House",
          "Townhouse",
          "Apartment/Condo"
        )
      ),
      
      br(),
      
      actionButton(
        "submit",
        "Trick-or-Treat",
        class = "btn-primary"
      ),
      
      br(),
      br(),
      
      textOutput("status")
    ),
    
    column(
      width = 8,
      plotOutput("history_plot", height = "400px")
    )
  ),
  
  hr(),
  
  wellPanel(
    style = "font-size: 12px;",
    strong("Privacy Notice: "),
    "Trick-or-Treat Tracker does not collect any personal information. The only data collected are the date and time the trick-or-treat button is pressed, a randomly generated device ID that has no information about the device or the user, and the information input by the user (time zone, state/territory, type of community, and type of house)."
  ),
  
  tags$head(
    tags$script(HTML("
      if (!localStorage.getItem('trick_or_treat_id')) {
        localStorage.setItem(
          'trick_or_treat_id',
          ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
            (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
          )
        );
      }

      $(document).on('shiny:connected', function() {
        Shiny.setInputValue(
          'device_id',
          localStorage.getItem('trick_or_treat_id'),
          {priority: 'event'}
        );
      });
    "))
  )
)

server <- function(input, output, session) {
  
  google_sheet_url <- "https://docs.google.com/spreadsheets/d/1vHhvYh7GaswzGAkXL0bz6iKMR6BL4Ahim2NEXkCTObw/edit?usp=sharing"
  
  refresh_counter <- reactiveVal(0)
  
  observeEvent(input$submit, {
    
    req(input$device_id)
    
    if (is.null(input$timezone) || input$timezone == "") {
      showNotification("Please select a time zone", type = "error")
      return()
    }
    
    new_row <- tibble(
      timestamp = as.character(Sys.time()),
      device_id = input$device_id,
      timezone = input$timezone,
      state = input$state,
      community_type = input$community,
      house_type = input$house_type
    )
    
    sheet_append(
      ss = google_sheet_url,
      data = new_row
    )
    
    refresh_counter(refresh_counter() + 1)
    
    output$status <- renderText(" ")
  })
  
  user_history <- reactive({
    
    req(input$device_id)
    
    input$submit
    
    sheet_data <- read_sheet(google_sheet_url)
    
    print(names(sheet_data))
    
    sheet_data %>%
      filter(device_id == input$device_id)
  })
  
  output$history_plot <- renderPlot({
    
    history <- user_history()
    
    req(nrow(history) > 0)
    
    timestamp_col <- names(history)[1]
    
    plot_data <- history %>%
      mutate(
        plot_time = as.POSIXct(.data[[timestamp_col]]) - case_when(
          timezone == "Eastern" ~ hours(4),
          timezone == "Central" ~ hours(5),
          timezone == "Mountain" ~ hours(6),
          timezone == "Pacific" ~ hours(7),
          timezone == "Alaska" ~ hours(8),
          timezone == "Hawaii" ~ hours(9),
          TRUE ~ hours(0)
        ),
        five_minute = format(
          as.POSIXct(cut(plot_time, "5 min")),
          "%I:%M %p"
        )
      ) %>%
      filter(as.Date(plot_time) == Sys.Date()) %>%
      count(five_minute)
    
    max_y <- ceiling(max(plot_data$n, na.rm = TRUE) / 5) * 5
    
    ggplot(plot_data, aes(five_minute, n)) +
      geom_col(fill = "orange") +
      scale_y_continuous(
        limits = c(0, max_y),
        breaks = seq(0, max_y, by = 5)
      ) +
      labs(
        title = "Your Trick-or-Treat Activity",
        x = "Time",
        y = "Trick-or-Treaters"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#120018", color = NA),
        panel.background = element_rect(fill = "#120018", color = NA),
        plot.title = element_text(color = "white", size = 18, face = "bold", hjust = .5),
        axis.title = element_text(color = "white", size = 16),
        axis.text = element_text(color = "white", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank()
      )  })  
}

shinyApp(ui, server)

# rsconnect::deployApp(
#   appDir = "C:/Users/jwest/github/trick-or-tracker",
#   account = "jayspositapps",
#   server = "connect.posit.cloud"
# )