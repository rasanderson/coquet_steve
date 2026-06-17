## a shiny app for loading data for a SEM
library(shiny)
library(ggplot2)
library(glmmTMB)
library(nlme)

rain_weather.dat <- read.csv("df_events(in).csv", header = TRUE)
rain_weather.dat <- rain_weather.dat[, c(
  "event_WWTW",
  "event_harbour",
  "rainfall_warkworth",
  "Wark_rainfall_lag1",
  "wwtwmin1"
)]
rain_weather.dat <- na.omit(rain_weather.dat)
rain_weather.dat$days <- seq_len(nrow(rain_weather.dat))

##### the server bit
ui <- fluidPage(
  titlePanel("Modelling storm water events and weather in the Coquet"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "bins1",
        " Analysis to undertake ",
        choices = c("Time series", "Models and prediction"),
        selected = "Time series"
      ),
      radioButtons(
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
        "Rainfall today mm:",
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
        "Event yesterday ? :",
        min = 0,
        max = 1,
        value = 0,
        step = 1
      )
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
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
        myplot <- ggplot(events, aes(rain, lag, z = SWE)) +
          geom_contour_filled() +
          geom_point(data = plot_data, aes(x = rain, y = lag), inherit.aes = FALSE, color = "red", size = 4, shape = 19) +
          labs(
            title = "Predicted Amble waste water event probability",
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
        myplot <- ggplot(events, aes(rain, lag, z = SWE)) +
          geom_contour_filled() +
          geom_point(data = plot_data, aes(x = rain, y = lag), inherit.aes = FALSE, color = "red", size = 4, shape = 19) +
          labs(
            title = "Predicted Amble harbour event probability",
            x = "Rainfall today (mm)",
            y = "Rainfall yesterday (mm)"
          )
        return(myplot)
      }
    }

    ggplot() +
      theme_void() +
      labs(title = "Please select a valid analysis option")
  })

  output$distPlot <- renderPlot({
    plot_info()
  })
}

app <- shinyApp(ui = ui, server = server)
