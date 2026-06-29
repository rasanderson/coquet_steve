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
      plotOutput("distPlot", height = "700px"),
      tableOutput("coliform_prediction")
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
    choice3 <- input$bins3
    new_rain <- input$bins4
    new_lag <- input$bins5
    new_yesterday <- ifelse(input$bins6 >= 0.5, 1, 0)

    if (choice1 == "Time series") {
      event_col <- ifelse(choice3 == "Amble harbour", "event_harbour", "event_WWTW")

      plot_data <- rbind(
        data.frame(
          days = rain_weather.dat$days,
          value = rain_weather.dat$rainfall_warkworth,
          series = "Rainfall",
          y_label = "rain (mm)"
        ),
        data.frame(
          days = rain_weather.dat$days,
          value = rain_weather.dat[[event_col]],
          series = paste("Storm water events:", choice3),
          y_label = "event occurring 0/1"
        )
      )

      myplot <- ggplot(plot_data, aes(x = days, y = .data$value)) +
        geom_line(color = "blue") +
        facet_wrap(~series, ncol = 1, scales = "free_y") +
        labs(x = "Days", y = NULL, title = "Rainfall and storm water events") +
        theme(
          plot.title = element_text(size = 17, face = "bold"),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          strip.text = element_text(size = 13, face = "bold")
        )

      return(myplot)
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

  output$coliform_prediction <- renderTable({
    if (input$bins1 != "Coliform risk Little Shore") {
      return(NULL)
    }

    new_data <- data.frame(
      flow_roth_t0 = input$bins7,
      flow_roth_t2 = input$bins8,
      mean_rain_wark = input$bins9,
      mean_rain_roth = input$bins10,
      WWTW_t1 = as.numeric(input$bins11)
    )
    predicted <- predict(coli.glm, newdata = new_data, type = "response", se.fit = TRUE)
    fit_log10 <- as.numeric(predicted$fit)
    se_log10 <- as.numeric(predicted$se.fit)
    predicted_value <- 10^fit_log10
    lower_bound <- 10^(fit_log10 - 1.96 * se_log10)
    upper_bound <- 10^(fit_log10 + 1.96 * se_log10)

    data.frame(
      "Flow today at Rothbury (m3/s)" = round(input$bins7, 2),
      "Flow 2 days prior at Rothbury (m3/s)" = round(input$bins8, 2),
      "Mean rain 7 days prior at Warkworth (mm)" = round(input$bins9, 2),
      "Mean rain 7 days prior at Rothbury (mm)" = round(input$bins10, 2),
      "Waste water event yesterday (0/1)" = as.numeric(input$bins11),
      "Predicted log10(E. coli)" = round(fit_log10, 4),
      "Predicted E. coli (CFU/100ml)" = round(predicted_value, 2),
      "95% CI for E. coli (CFU/100ml)" = paste0(round(lower_bound, 2), " - ", round(upper_bound, 2)),
      check.names = FALSE
    )
  })

  ####### table_info<-reactive(
  output$distPlot <- renderPlot({
    plot_info()
  })
}


app <- shinyApp(ui = ui, server = server)
# shiny::runApp(app)
