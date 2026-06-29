## a shiny app for loading data for a SEM
library(shiny)
library(ggplot2)
library(glmmTMB)
library(nlme)
# for events
rain_weather.dat <- read.csv("df_events(in).csv", header = TRUE) # j
rain_weather.dat <- rain_weather.dat[, c(
  "event_WWTW",
  "event_harbour",
  "rainfall_warkworth",
  "Wark_rainfall_lag1",
  "wwtwmin1"
)]
rain_weather.dat <- na.omit(rain_weather.dat)
rain_weather.dat$days <- seq_len(nrow(rain_weather.dat))

# for coliforms
coli.dat <- read.csv("coliform_data.csv", header = TRUE)
coli.dat <- coli.dat[, c("Ecoli", "flow_roth_t0", "flow_roth_t2", "mean_rain_wark", "mean_rain_roth", "WWTW_t1")]
# need these as radiobuttons

# flow roth is today flo
# flow sum is  2days prior sum
# mean rain is mean for 7 days prior
# WWTW_t1 is waste water event yesterday (0 or 1)


##### the server bit
ui <- fluidPage(
  titlePanel("Modelling storm water events, weather in the Coquet and coliform count in Little Shore"),
  sidebarLayout(
    sidebarPanel(
      h4("Main parameters"),
      radioButtons(
        "bins1",
        " Analysis to undertake ",
        choices = c("Time series", "Models and prediction", "Coliform risk Little Shore"),
        selected = "Time series"
      ),
      radioButtons(
        # drop this and see below
        "bins2",
        "Pick one variable to plot/model ",
        choices = c("Rainfall", "Storm water events"),
        selected = "Rainfall"
      ),
      radioButtons(
        "bins3",
        "Pick the sewage outflow to plot/model ",
        choices = c("Amble waste water", "Amble harbour"),
        selected = "Amble waste water"
      ),
      sliderInput(
        "bins4",
        "Rainfall today Warkworth mm:",
        min = 0.01,
        max = 30,
        value = 0.01
      ),
      sliderInput(
        "bins5",
        "Rainfall yesterday mm:",
        min = 0.01,
        max = 10,
        value = 0.01
      ),
      sliderInput(
        "bins6",
        "Event yesterday? (waste water model only)",
        min = 0,
        max = 1,
        value = 0,
        step = 1
      ),
      helpText("Used only for 'Models and prediction' with 'Amble waste water'."),
      hr(),
      h4("Coliform risk model parameters"),
      sliderInput(
        "bins7",
        "Flow today Rothbury (m3/s):",
        min = 0.01,
        max = 10,
        value = 0.01
      ),
      sliderInput(
        "bins8",
        "Flow 2 days prior (m3/s):",
        min = 0.01,
        max = 10,
        value = 0.01
      ),
      sliderInput(
        "bins9",
        "Mean rainfall 7 days prior Warkworth (mm):",
        min = 0.01,
        max = 30,
        value = 0.01
      ),
      sliderInput(
        "bins10",
        "Mean rainfall 7 days prior Rothbury (mm):",
        min = 0.01,
        max = 30,
        value = 0.01
      ),
      radioButtons(
        "bins11",
        "Waste water event yesterday? (0/1):",
        choices = c("No" = 0, "Yes" = 1),
        selected = 0
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      textOutput("coliform_prediction")
    )
  )
)

server <- function(input, output) {
  coli.glm <- glm(
    log10(Ecoli) ~ flow_roth_t0 + flow_roth_t2 + mean_rain_wark + mean_rain_roth + WWTW_t1,
    gaussian,
    coli.dat
  )

  plot_info <- reactive({
    choice1 <- input$bins1
    choice2 <- input$bins2
    choice3 <- input$bins3
    new_rain <- input$bins4
    new_lag <- input$bins5
    new_yesterday <- ifelse(input$bins6 >= 0.5, 1, 0)

    if (choice1 == "Time series") {
      if (choice2 == "Rainfall") {
        myplot <- ggplot(rain_weather.dat, aes(x = days, y = rainfall_warkworth)) +
          geom_line(color = "blue") +
          labs(y = "rain (mm)", title = "Rainfall")
        return(myplot)
      }

      if (choice2 == "Storm water events") {
        event_col <- ifelse(choice3 == "Amble harbour", "event_harbour", "event_WWTW")
        myplot <- ggplot(rain_weather.dat, aes(x = days, y = .data[[event_col]])) +
          geom_line(color = "blue") +
          labs(y = "event occurring 0/1", title = paste("Storm water events:", choice3))
        return(myplot)
      }
    }

    if (choice1 == "Models and prediction") {
      if (choice3 == "Amble waste water") {
        model <- glm(
          event_WWTW ~ rainfall_warkworth + Wark_rainfall_lag1 + wwtwmin1,
          family = binomial,
          data = rain_weather.dat
        )

        rain_seq <- seq(
          min(rain_weather.dat$rainfall_warkworth),
          max(rain_weather.dat$rainfall_warkworth),
          length.out = 100
        )
        lag_seq <- seq(
          min(rain_weather.dat$Wark_rainfall_lag1),
          max(rain_weather.dat$Wark_rainfall_lag1),
          length.out = 100
        )

        events <- expand.grid(rain = rain_seq, lag = lag_seq)
        pred_data <- data.frame(
          rainfall_warkworth = events$rain,
          Wark_rainfall_lag1 = events$lag,
          wwtwmin1 = new_yesterday
        )
        events$SWE <- predict(model, newdata = pred_data, type = "response")

        plot_data <- data.frame(rain = new_rain, lag = new_lag)
        point_data <- data.frame(
          rainfall_warkworth = new_rain,
          Wark_rainfall_lag1 = new_lag,
          wwtwmin1 = new_yesterday
        )
        selected_prob <- predict(model, newdata = point_data, type = "response")
        myplot <- ggplot(events, aes(rain, lag, z = SWE)) +
          geom_contour_filled() +
          geom_point(data = plot_data, aes(x = rain, y = lag), inherit.aes = FALSE, color = "red", size = 4, shape = 19) +
          labs(
            title = "Predicted Amble waste water event probability",
            subtitle = sprintf("Selected point probability: %.3f | Event yesterday: %d", selected_prob, new_yesterday),
            x = "Rainfall today (mm)",
            y = "Rainfall yesterday (mm)"
          )
        return(myplot)
      }

      if (choice3 == "Amble harbour") {
        model <- glm(
          event_harbour ~ rainfall_warkworth + Wark_rainfall_lag1,
          family = binomial,
          data = rain_weather.dat
        )

        rain_seq <- seq(
          min(rain_weather.dat$rainfall_warkworth),
          max(rain_weather.dat$rainfall_warkworth),
          length.out = 100
        )
        lag_seq <- seq(
          min(rain_weather.dat$Wark_rainfall_lag1),
          max(rain_weather.dat$Wark_rainfall_lag1),
          length.out = 100
        )

        events <- expand.grid(rain = rain_seq, lag = lag_seq)
        pred_data <- data.frame(
          rainfall_warkworth = events$rain,
          Wark_rainfall_lag1 = events$lag
        )
        events$SWE <- predict(model, newdata = pred_data, type = "response")

        plot_data <- data.frame(rain = new_rain, lag = new_lag)
        point_data <- data.frame(
          rainfall_warkworth = new_rain,
          Wark_rainfall_lag1 = new_lag
        )
        selected_prob <- predict(model, newdata = point_data, type = "response")
        myplot <- ggplot(events, aes(rain, lag, z = SWE)) +
          geom_contour_filled() +
          geom_point(data = plot_data, aes(x = rain, y = lag), inherit.aes = FALSE, color = "red", size = 4, shape = 19) +
          labs(
            title = "Predicted Amble harbour event probability",
            subtitle = sprintf("Selected point probability: %.3f | Event yesterday is not used in harbour model", selected_prob),
            x = "Rainfall today (mm)",
            y = "Rainfall yesterday (mm)"
          )
        return(myplot)
      }
    }

    if (choice1 == "Coliform risk Little Shore") {
      return(
        ggplot() +
          theme_void() +
          labs(title = "Coliform prediction shown below")
      )
    }

    ggplot() +
      theme_void() +
      labs(title = "Please select a valid analysis option")
  })

  output$coliform_prediction <- renderText({
    if (input$bins1 != "Coliform risk Little Shore") {
      return("")
    }

    new_data <- data.frame(
      flow_roth_t0 = input$bins7,
      flow_roth_t2 = input$bins8,
      mean_rain_wark = input$bins9,
      mean_rain_roth = input$bins10,
      WWTW_t1 = as.numeric(input$bins11)
    )
    predicted_log10 <- predict(coli.glm, newdata = new_data, type = "response")
    predicted_value <- 10^predicted_log10
    paste("Predicted E. coli count (CFU/100ml):", round(predicted_value, 2))
  })

  ####### table_info<-reactive(
  output$distPlot <- renderPlot({
    plot_info()
  })
}


app <- shinyApp(ui = ui, server = server)
# shiny::runApp(app)
