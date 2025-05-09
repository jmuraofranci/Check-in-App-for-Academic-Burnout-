# -----------------------------
# Load libraries

library(shiny)
library(ggplot2)
library(mirt)

# -----------------------------
# Load data

burnout_data <- readRDS("data.rds")

# -----------------------------
# Assign loaded variables correctly

item_questions <- burnout_data$question_data
response_data <- burnout_data$response_data
a.estimate <- burnout_data$a.estimate
b.estimate <- burnout_data$b.estimate

# -----------------------------
# Calculate Reliability (Cronbach's Alpha)

theta_seq <- seq(-4, 4, 0.1)

coeff_alpha <- function(responses) {
  n_items <- ncol(responses)
  n_persons <- nrow(responses)
  x <- rowSums(responses)
  var_x <- var(x) * (n_persons - 1) / n_persons
  var_y <- numeric(n_items)
  for (j in 1:n_items) {
    var_y[j] <- var(responses[, j]) * (n_persons - 1) / n_persons
  }
  alpha <- (n_items / (n_items - 1)) * (1 - sum(var_y) / var_x)
  alpha
}

round(coeff_alpha(response_data), 2)

# -----------------------------
# Define 2PL IRT model functions

prob_correct <- function(theta, discrimination, difficulty) {
  D <- 1.7
  1 / (1 + exp(-D * discrimination * (theta - difficulty)))
}

calc_item_info <- function(theta, discrimination, difficulty) {
  D <- 1.7
  P <- prob_correct(theta, discrimination, difficulty)
  Q <- 1 - P
  (D^2) * (discrimination^2) * P * Q
}

log_likelihood <- function(theta, discrimination, difficulty, responses) {
  P <- prob_correct(theta, discrimination, difficulty)
  sum(log(P) * responses + log(1 - P) * (1 - responses))
}

estimate_theta <- function(discrimination, difficulty, responses, minimum_theta = -4, maximum_theta = 4) {
  if (sum(responses) == length(responses)) {
    estimated_theta <- maximum_theta
  } else if (sum(responses) == 0) {
    estimated_theta <- minimum_theta
  } else {
    estimated_theta <- optimize(log_likelihood,
                                interval = c(minimum_theta, maximum_theta),
                                maximum = TRUE,
                                discrimination = discrimination,
                                difficulty = difficulty,
                                responses = responses)$maximum
  }
  estimated_theta
}

# -----------------------------
# Define plotting functions

plot_item_info <- function(item_index, title_text) {
  values <- numeric(length(theta_seq))
  for (index in seq_along(theta_seq)) {
    values[index] <- calc_item_info(theta_seq[index], a.estimate[item_index], b.estimate[item_index])
  }
  plot(theta_seq, values, type = "l", col = "red", lwd = 2,
       main = title_text,
       xlab = expression(theta),
       ylab = expression(Information(theta)))
}

plot_item_char <- function(item_index, title_text) {
  values <- numeric(length(theta_seq))
  for (index in seq_along(theta_seq)) {
    values[index] <- prob_correct(theta_seq[index], a.estimate[item_index], b.estimate[item_index])
  }
  plot(theta_seq, values, type = "l", col = "blue", lwd = 2,
       main = title_text,
       xlab = expression(theta),
       ylab = expression(P(theta)))
}

plot_standard_error <- function(steps_df) {
  if (nrow(steps_df) >= 1) {
    plot(steps_df$Step, steps_df$SE, type = "b", pch = 19, col = "blue", lwd = 2,
         xlab = "Number of questions", ylab = "Standard Error",
         main = expression(paste("Standard Error throughout Test")),
         ylim = c(0, max(steps_df$SE) + 0.2))
    abline(h = 0.1, col = "red", lty = 2)
  }
}

plot_estimated_theta <- function(steps_df) {
  if (nrow(steps_df) >= 1) {
    plot(steps_df$Step, steps_df$EstTheta, type = "b", pch = 19,
         col = "blue", lwd = 2,
         xlab = "Number of questions", ylab = expression(hat(theta)),
         main = expression(paste("Estimated Ability throughout Test")),
         ylim = c(min(steps_df$EstTheta) - 1, max(steps_df$EstTheta) + 1))
    abline(h = 0, col = "red", lty = 2)
  }
}

# -----------------------------
# Define UI

ui <- fluidPage(
  tags$head(
    tags$style(HTML("body {background-color: #f8f9fa;} h3, h4, h5 {color: #041E42;} .btn-primary {background-color: #041E42; border-color: #041E42;} .result-text {font-size: 32px; font-weight: bold; color: #041E42; margin-top: 20px;}"))
  ),
  
  div(
    style = "position: fixed; bottom: -50px; right: 30px; z-index: 9999;",
    tags$img(
      src = "https://images.squarespace-cdn.com/content/v1/67eea9fb2f2c5541c52b35d2/e5dd7762-256b-4f35-b265-23660d3fb517/HoyaMind_logo.png?format=500w",
      height = "200px"
    )
  ),
  
  conditionalPanel(
    condition = "input.start == 0",
    fluidRow(
      column(12, align = "center",
             h1("Graduate Student Burnout Assessment", style = "font-weight: bold; color: #041E42; text-align: center;")
      )
    ),
    fluidRow(
      column(12, align = "center",
             h4("Hi Hoya Grad Student!"),
             p("We know how tough school has been lately. Take a few minutes for yourself — complete this quick assessment and discover personalized tips to recharge, refocus, and beat academic burnout. You've got this!"),
             br()
      )
    )
  ),
  
  conditionalPanel(
    condition = "input.start == 0",
    fluidRow(
      column(12, align = "center",
             actionButton("start", "Start New Test", class = "btn-primary", style = "margin-bottom: 20px;")
      )
    )
  ),
  
  fluidRow(
    column(12, align = "center",
           h3(textOutput("question_text")),
           conditionalPanel(
             condition = "output.show_response == true",
             radioButtons("user_response", label = NULL, choices = c("True" = 1, "False" = 0), inline = TRUE),
             actionButton("submit_response", "Submit Response", class = "btn-primary", style = "margin-top:10px;")
           ),
           br()
    )
  ),
  
  conditionalPanel(
    condition = "output.show_plots == true",
    fluidRow(
      column(6, plotOutput("icc_plot", height = "250px")),
      column(6, plotOutput("iic_plot", height = "250px"))
    ),
    fluidRow(
      column(6, plotOutput("se_plot", height = "250px")),
      column(6, plotOutput("theta_plot", height = "250px"))
    )
  ),
  
  hr(),
  
  fluidRow(
    column(12, align = "center",
           htmlOutput("result_text")
    )
  ),
  
  conditionalPanel(
    condition = "!output.show_response && !output.show_plots && input.start > 0",
    fluidRow(
      column(12, align = "center",
             br(),
             h4("We appreciate you for completing the test."),
             actionButton("restart", "Take the Test Again.", class = "btn-primary", style = "margin-top: 20px; margin-bottom: 20px;")
      )
    )
  )
)


# -----------------------------
# Define Server
server <- function(input, output, session) {
  max_items <- 10
  stopping_se_threshold <- 0.1
  
  state <- reactiveValues(
    current_theta = 0,
    used_items = c(),
    responses = c(),
    current_item = NULL,
    steps = data.frame(),
    step_num = 0,
    test_active = FALSE
  )
  
  output$show_response <- reactive({
    state$test_active && !is.null(state$current_item)
  })
  outputOptions(output, "show_response", suspendWhenHidden = FALSE)
  
  output$show_plots <- reactive({
    state$test_active && length(state$responses) >= 1
  })
  outputOptions(output, "show_plots", suspendWhenHidden = FALSE)
  
  observeEvent(input$start, {
    state$current_theta <- 0
    state$used_items <- c()
    state$responses <- c()
    state$steps <- data.frame()
    state$step_num <- 0
    state$test_active <- TRUE
    state$current_item <- which.min(abs(b.estimate))
  })
  
  observeEvent(input$submit_response, {
    if (is.null(state$current_item)) return()
    
    response <- as.numeric(input$user_response)
    state$responses <- c(state$responses, response)
    state$used_items <- c(state$used_items, state$current_item)
    state$step_num <- state$step_num + 1
    
    if (length(state$responses) >= 2) {
      state$current_theta <- estimate_theta(
        a.estimate[state$used_items],
        b.estimate[state$used_items],
        state$responses
      )
    }
    
    total_info_now <- sum(sapply(state$used_items, function(j)
      calc_item_info(state$current_theta, a.estimate[j], b.estimate[j])))
    current_se_now <- 1 / sqrt(total_info_now)
    
    state$steps <- rbind(state$steps, data.frame(
      Step = state$step_num,
      Item = paste0("Item_", state$current_item),
      Response = response,
      EstTheta = round(state$current_theta, 3),
      SE = round(current_se_now, 3)
    ))
    
    available <- setdiff(1:length(a.estimate), state$used_items)
    
    if (length(available) == 0 || (state$step_num >= 5 && (current_se_now < stopping_se_threshold || abs(state$current_theta) > 1)) || state$step_num >= max_items) {
      state$current_item <- NULL
      state$test_active <- FALSE
      return()
    }
    
    infos <- sapply(available, function(j) calc_item_info(state$current_theta, a.estimate[j], b.estimate[j]))
    max_info <- max(infos)
    best_items <- available[which(abs(infos - max_info) < 1e-6)]
    state$current_item <- sample(best_items, 1)
  })
  
  observeEvent(input$restart, {
    state$current_theta <- 0
    state$used_items <- c()
    state$responses <- c()
    state$steps <- data.frame()
    state$step_num <- 0
    state$test_active <- TRUE
    state$current_item <- which.min(abs(b.estimate))
  })
  
  output$question_text <- renderText({
    if (!is.null(state$current_item)) {
      question_text <- item_questions[state$current_item, 1]
      paste("Q", state$step_num + 1, ":", question_text)
    } else if (!state$test_active && state$step_num > 0) {
      "Test Completed!"
    } else {
      ""
    }
  })
  
  output$icc_plot <- renderPlot({
    if (!is.null(state$current_item)) {
      plot_item_char(state$current_item, paste("ICC for Item", state$current_item))
    }
  })
  
  output$iic_plot <- renderPlot({
    if (!is.null(state$current_item)) {
      plot_item_info(state$current_item, paste("IIC for Item", state$current_item))
    }
  })
  
  output$se_plot <- renderPlot({
    plot_standard_error(state$steps)
  })
  
  output$theta_plot <- renderPlot({
    plot_estimated_theta(state$steps)
  })
  
  output$result_text <- renderUI({
    if (!state$test_active && state$step_num > 0) {
      theta <- state$current_theta
      if (theta < -1) {
        HTML("<div class='result-text' style='font-size: 1em;'> <h3>Low Burnout</h3> <p>Hey, it looks like you're managing things pretty well, but even small stress deserves care! Here are a few free ways to recharge on campus:</p> <ul style='list-style-type: none; padding-left: 0;'> <li><a href='https://studenthealth.georgetown.edu/hoya-wellness-wheel/' target='_blank'>HOYA Wellness Wheel</a></li> <li><a href='https://recreation.georgetown.edu/programs/group-fitness/' target='_blank'>Group Fitness Classes</a></li> <li><a href='https://recreation.georgetown.edu/programs/lred/' target='_blank'>Leisure and Recreation Education (LRED)</a></li> <li><a href='https://studenthealth.georgetown.edu/health-promotion/self-care/' target='_blank'>Self-Care Resources</a></li> </ul> </div>")
      } else if (theta <= 1) {
        HTML("<div class='result-text' style='font-size: 1em;'> <h3>Moderate Burnout</h3> <p>Hey, it seems like you’ve been juggling a lot lately. Here are some free campus resources you might find helpful:</p> <ul style='list-style-type: none; padding-left: 0;'> <li><a href='https://studenthealth.georgetown.edu/mental-health/services/' target='_blank'>CAPS Services</a></li> <li><a href='https://studenthealth.georgetown.edu/mental-health/services/group-therapy/' target='_blank'>HoyaWell</a></li> <li><a href='https://studenthealth.georgetown.edu/workshops/' target='_blank'>Health Education Workshops</a></li> <li><a href='https://studentaffairs.georgetown.edu/studentoutreach/' target='_blank'>Student Outreach & Support (SOS)</a></li> </ul> </div>")
      } else {
        HTML("<div class='result-text' style='font-size: 1em;'> <h3>High Burnout</h3> <p>Hey, we’re glad you took the time to check in. You matter. Here are safe, confidential resources you can reach out to right now:</p> <ul style='list-style-type: none; padding-left: 0;'> <li><a href='https://studenthealth.georgetown.edu/mental-health/emergency/' target='_blank'>CAPS Emergency Services</a></li> <li><a href='https://studenthealth.georgetown.edu/mental-health/medical-leave/' target='_blank'>Medical Leave of Absence (MLOA)</a></li> <li><a href='https://studenthealth.georgetown.edu/georgetown-student-mental-health-fund/' target='_blank'>Mental Health Fund</a></li> <li><a href='https://studenthealth.georgetown.edu/mental-health/helpful-resources/' target='_blank'>Mental Health Screening Tools</a></li> </ul> </div>")
      }
    }
  })
}

# ----------------------------- 
# Run the app 

shinyApp(ui = ui, server = server)



