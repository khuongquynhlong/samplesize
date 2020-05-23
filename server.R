library(shiny)
library(shinydashboard)
library(DT)

# Function estimating a population proportion
fun1_1prop_est <- function(p, d, alpha) {
  z <- qnorm(1-alpha/2)
  n <- z^2*p*(1-p)/d^2
  return(n)
}

fun2_1prop_est <- function(p, eps, alpha) {
  z <- qnorm(1-alpha/2)
  n <- z^2*(1-p)/(eps^2*p)
  return(n)
}

# Function hypothesis test a population proportion
fun_1prop_hypo <- function(p_0, p_a, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- (z_a*sqrt(p_0*(1-p_0))+z_b*sqrt(p_a*(1-p_a)))^2/(p_a-p_0)^2
  return(n)
}

fun_1prop_hypo_power <- function(p_0, p_a, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  z_b <- (sqrt(n*(p_a-p_0)^2)-z_a*sqrt(p_0*(1-p_0)))/sqrt(p_a*(1-p_a))
  power <- pnorm(z_b)
  return(power)
}

# Function estimate 2 props difference
fun_2props_est <- function(p1, p2, alpha, d) {
  z <- qnorm(1-alpha/2)
  n <- z^2*(p1*(1-p1)+p2*(1-p2))/d^2
  return(n)
}

# Function hypothesis test for 2 props
fun_2props_hypo <- function(p1, p2, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- mean(c(p1, p2))
  n <- (z_a*sqrt(2*p*(1-p))+z_b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/(p1-p2)^2
  return(n)
}

fun_2props_hypo_power <- function(p1, p2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  p <- mean(c(p1, p2))
  z_b <- (sqrt(n*(p1-p2)^2)-z_a*sqrt(2*p*(1-p)))/sqrt(p1*(1-p1)+p2*(1-p2))
  power <- pnorm(z_b)
  return(power)
}

# Function estimating the population mean
fun1_1mean_est <- function(sd, d, alpha) {
  z <- qnorm(1-alpha/2)
  n <- z^2*sd^2/d^2
  return(n)
}

fun2_1mean_est <- function(sd, alpha, mean, eps) {
  z <- qnorm(1-alpha/2)
  n <- z^2*sd^2/(eps^2*mean^2)
  return(n)
}

# Function hypothesis testing for 1 population mean
fun_1mean_hypo <- function(sd, m_0, m_a, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  n <- sd^2*(z_a+z_b)^2/(m_0-m_a)^2
  return(n)
}

fun_1mean_hypo_power <- function(sd, m_0, m_a, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  z_b <- sqrt(n*(m_0-m_a)^2/sd^2)-z_a
  power <- pnorm(z_b)
  return(power)
}

# Function estimating the difference between 2 population means
fun_2means_est <- function(sd, d, alpha) {
  z <- qnorm(1-alpha/2)
  n <- 2*z^2*sd^2/d^2
  return(n)
}

# Function for 2 means hypothesis testing
fun_2means_hypo <- function(m1, m2, sd1, sd2, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  sd <- sqrt((sd1^2+sd2^2)/2)
  n <- 2*sd^2*(z_a+z_b)^2/(m1-m2)^2
  return(n)
}

fun_2means_hypo_power <- function(m1, m2, sd1, sd2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  sd <- sqrt((sd1^2+sd2^2)/2)
  z_b <- sqrt(n*(m1-m2)^2/(2*sd^2))-z_a
  power <- pnorm(z_b)
  return(power)
}

# Function for estimating a RR
fun_cohort_est <- function(p1, p2, rr, alpha, eps) {
  z <- qnorm(1-alpha/2)
  n <- z^2*((1-p1)/p1+(1-p2)/p2)/log(1-eps, base = exp(1))^2
  return(n)
}

# Function for hypothesis test for a RR
fun_cohort_hypo <- function(p1, p2, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- mean(c(p1, p2))
  n <- (z_a*sqrt(2*p*(1-p))+z_b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/(p1-p2)^2
  return(n)
}

fun_cohort_hypo_power <- function(p1, p2, alpha, n) {
  z_a <- qnorm(1-alpha/2)
  p <- mean(c(p1, p2))
  z_b <- (sqrt(n*(p1-p2)^2)-z_a*sqrt(2*p*(1-p)))/sqrt(p1*(1-p1)+p2*(1-p2))
  power <- pnorm(z_b)
  return(power)
}

# Function for simple random sampling
fun_simple_random <- function(N, P, alpha, d, eps) {
  z <- qnorm(1-alpha/2)
  n <- z^2*P*(1-P)*N/(d^2*(N-1)+z^2*P*(1-P))
}

shinyServer(function(input, output) {
  
  ##### Categorical variables #####
  ##### Estimate 1 prop #####
  output$precision_1prop_est <- renderUI({
    if (input$precision_type_1prop_est == 1) {
      textInput(inputId = "d_1prop_est",
                label = "Absolute precision",
                value = 0.05)
    } else if (input$precision_type_1prop_est == 2) {
      textInput(inputId = "eps_1prop_est",
                label = "Relative precision",
                value = 0.2)
    }
  })
  n_1prop_est <- reactive({
    req(as.numeric(input$p_1prop_est)>0&
          as.numeric(input$alpha_1prop_est)>0&
          (as.numeric(input$d_1prop_est)>0||
             as.numeric(input$eps_1prop_est)>0),
        cancelOutput = TRUE)
    if (input$precision_type_1prop_est == 1) {
      fun1_1prop_est(p = as.numeric(input$p_1prop_est), 
                     d = as.numeric(input$d_1prop_est), 
                     alpha = as.numeric(input$alpha_1prop_est))
    } else if (input$precision_type_1prop_est == 2) {
      fun2_1prop_est(p = as.numeric(input$p_1prop_est), 
                     eps = as.numeric(input$eps_1prop_est), 
                     alpha = as.numeric(input$alpha_1prop_est))
    }
  })
  output$n_1prop_est <- renderValueBox({
    valueBox(
      value = ceiling(n_1prop_est()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Hypothesis test for a population proportion #####
  # Sample size
  n_1prop_hypo <- reactive({
    req(as.numeric(input$p0_1prop_hypo)>0&
          as.numeric(input$pa_1prop_hypo)>0&
          as.numeric(input$alpha_1prop_hypo)>0&
          as.numeric(input$power_1prop_hypo)>0,
        cancelOutput = TRUE)
    fun_1prop_hypo(p_0 = as.numeric(input$p0_1prop_hypo), 
                   p_a = as.numeric(input$pa_1prop_hypo), 
                   alpha = as.numeric(input$alpha_1prop_hypo), 
                   power = as.numeric(input$power_1prop_hypo))
  })
  output$n_1prop_hypo <- renderValueBox({
    valueBox(
      value = ceiling(n_1prop_hypo()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  # Power
  power_1prop_hypo <- reactive({
    req(as.numeric(input$p0_1prop_hypo_power)>0&
          as.numeric(input$pa_1prop_hypo_power)>0&
          as.numeric(input$alpha_1prop_hypo_power)>0&
          as.numeric(input$n_1prop_hypo_power)>0,
        cancelOutput = TRUE)
    fun_1prop_hypo_power(p_0 = as.numeric(input$p0_1prop_hypo_power), 
                         p_a = as.numeric(input$pa_1prop_hypo_power), 
                         alpha = as.numeric(input$alpha_1prop_hypo_power), 
                         n = as.numeric(input$n_1prop_hypo_power))
  })
  output$power_1prop_hypo <- renderValueBox({
    valueBox(
      value = round(power_1prop_hypo(), 2),
      subtitle = "Power",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Estimate 2 proportions difference #####
  n_2props_est <- reactive({
    req(as.numeric(input$p1_2props_est)>0&
          as.numeric(input$p2_2props_est)>0&
          as.numeric(input$alpha_2props_est)>0&
          as.numeric(input$d_2props_est)>0,
        cancelOutput = TRUE)
    fun_2props_est(p1 = as.numeric(input$p1_2props_est), 
                   p2 = as.numeric(input$p2_2props_est), 
                   alpha = as.numeric(input$alpha_2props_est), 
                   d = as.numeric(input$d_2props_est))
  })
  output$n_2props_est <- renderValueBox({
    valueBox(
      value = ceiling(n_2props_est()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Hypothesis test for 2 proportions #####
  n_2props_hypo <- reactive({
    req(as.numeric(input$p1_2props_hypo)>0&
          as.numeric(input$p2_2props_hypo)>0&
          as.numeric(input$alpha_2props_hypo)>0&
          as.numeric(input$power_2props_hypo)>0,
        cancelOutput = TRUE)
    fun_2props_hypo(p1 = as.numeric(input$p1_2props_hypo),
                    p2 = as.numeric(input$p2_2props_hypo),
                    alpha = as.numeric(input$alpha_2props_hypo),
                    power = as.numeric(input$power_2props_hypo))
  })
  n1_2props_hypo <- reactive({
    req(input$k_2props_hypo>=1, cancelOutput = TRUE)
    big_n <- 2*n_2props_hypo()*(1+input$k_2props_hypo)^2/(4*input$k_2props_hypo)
    big_n/(1+input$k_2props_hypo)
  })
  output$n1_2props_hypo <- renderValueBox({
    valueBox(
      value = ceiling(n1_2props_hypo()),
      subtitle = "Nhóm 1",
      icon = icon("capsules"),
      color = "green",
    )
  })
  output$n2_2props_hypo <- renderValueBox({
    valueBox(
      value = input$k_2props_hypo*ceiling(n1_2props_hypo()),
      subtitle = "Nhóm 2",
      icon = icon("tablets"),
      color = "orange",
    )
  })
  
  # Power
  power_2props_hypo <- reactive({
    req(as.numeric(input$p1_2props_hypo_power)>0&
          as.numeric(input$p2_2props_hypo_power)>0&
          as.numeric(input$alpha_2props_hypo_power)>0&
          as.numeric(input$n_2props_hypo_power)>0,
        cancelOutput = TRUE)
    fun_2props_hypo_power(p1 = as.numeric(input$p1_2props_hypo_power), 
                          p2 = as.numeric(input$p2_2props_hypo_power), 
                          alpha = as.numeric(input$alpha_2props_hypo_power), 
                          n = as.numeric(input$n_2props_hypo_power))
  })
  output$power_2props_hypo <- renderValueBox({
    valueBox(
      value = round(power_2props_hypo(), 2),
      subtitle = "Power",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  
  
# ##### Two proportions #####
#   ##### Fast calculation #####
#   n_2props <- reactive({
#     req(as.numeric(input$p1_2props)>0&
#           as.numeric(input$p2_2props)>0&
#           as.numeric(input$alpha_2props)>0&
#           as.numeric(input$power_2props)>0, 
#         cancelOutput = TRUE)
#     pwr <- pwr.2p.test(h = ES.h(p1 = as.numeric(input$p1_2props), p2 = as.numeric(input$p2_2props)),
#                        sig.level = as.numeric(input$alpha_2props), 
#                        power = as.numeric(input$power_2props))
#     pwr$n
#   })
#   n1_2props <- reactive({
#     req(input$k_2props>=1, cancelOutput = TRUE)
#     big_n <- 2*n_2props()*(1+input$k_2props)^2/(4*input$k_2props)
#     big_n/(1+input$k_2props)
#   })
#   output$n1_2props <- renderValueBox({
#     valueBox(
#       value = ceiling(n1_2props()),
#       subtitle = "Nhóm 1",
#       icon = icon("capsules"),
#       color = "green",
#     )
#   })
#   output$n2_2props <- renderValueBox({
#     valueBox(
#       value = input$k_2props*ceiling(n1_2props()),
#       subtitle = "Nhóm 2",
#       icon = icon("tablets"),
#       color = "orange",
#     )
#   })
# 
#   ##### Input file #####
#   # Make an example
#   temp_2props <- reactive({
#     data.frame(Outcome = c("Live birth", "Ongoing pregnancy"), 
#                Treatment = c(0.513, 0.565), 
#                Control = c(0.233, 0.288))
#   })
#   # Download template
#   output$temp_2props <- downloadHandler(
#     filename = "template_2props.csv",
#     content = function(con) {
#       write.csv(temp_2props(), row.names = F, con)
#     }
#   )
#   # Estimate sample size
#   df_2props <- reactive({
#     req(as.numeric(input$alpha_df_2props)>0&
#           as.numeric(input$power_df_2props)>0&
#           input$k_df_2props>=1,
#         cancelOutput = TRUE)
#     if (is.null(input$upload_2props)) {
#       df <- temp_2props()
#     } else {
#       df <- read.csv(file = input$upload_2props$datapath, 
#                      header = T, stringsAsFactors = F, check.names = F, fill = T)
#     }
#     for (i in 1:nrow(df)) {
#       pwr <- pwr.2p.test(h = ES.h(p1 = df$Treatment[i], p2 = df$Control[i]),
#                          sig.level = as.numeric(input$alpha_df_2props), 
#                          power = as.numeric(input$power_df_2props))
#       df$n[i] <- pwr$n
#     }
#     df$big_n <- 2*df$n*(1+input$k_df_2props)^2/(4*input$k_df_2props)
#     df$n1 <- ceiling(df$big_n/(1+input$k_df_2props))
#     df$n2 <- input$k_df_2props*df$n1
#     df$Total <- df$n1 + df$n2
#     df$n <- NULL
#     df$big_n <- NULL
#     df
#   })
#   # Display text
#   output$text_df_2props <- renderText({
#     req(as.numeric(input$alpha_df_2props)>0&
#           as.numeric(input$power_df_2props)>0&
#           input$k_df_2props>=1,
#         cancelOutput = TRUE)
#     paste0("alpha = ", 
#            input$alpha_df_2props, ", power = ", input$power_df_2props, 
#            " và k = ", input$k_df_2props)
#   })
#   # Display sample size dataframe
#   output$ss_df_2props <- DT::renderDataTable({
#     df_2props()
#   })
#   # Download sample size dataframe
#   output$download_df_2props <- downloadHandler(
#     filename = "df_2props.csv",
#     content = function(con) {
#       write.csv(df_2props(), row.names = F, con)
#     }
#   )

  ##### Continuous variables #####
  ##### Estimating the population mean #####
  output$precision_1mean_est <- renderUI({
    if (input$precision_type_1mean_est == 1) {
      textInput(inputId = "d_1mean_est",
                label = "Absolute precision",
                value = 0.1)
    } else if (input$precision_type_1mean_est == 2) {
      list(
        textInput(inputId = "mean_1mean_est", 
                  label = "Population mean", 
                  value = 0.1),
        textInput(inputId = "eps_1mean_est",
                  label = "Relative precision",
                  value = 2)
      )
    }
  })
  n_1mean_est <- reactive({
    req(as.numeric(input$sd_1mean_est)>0&
          as.numeric(input$alpha_1mean_est)>0&
          (as.numeric(input$d_1mean_est)>0||
          as.numeric(input$eps_1mean_est)>0||
          as.numeric(input$mean_1mean_est)>=0),
        cancelOutput = TRUE)
    if (input$precision_type_1mean_est == 1) {
      fun1_1mean_est(sd = as.numeric(input$sd_1mean_est), 
                     d = as.numeric(input$d_1mean_est), 
                     alpha = as.numeric(input$alpha_1mean_est))
    } else if (input$precision_type_1mean_est == 2) {
      fun2_1mean_est(sd = as.numeric(input$sd_1mean_est), 
                     mean = as.numeric(input$mean_1mean_est),
                     eps = as.numeric(input$eps_1mean_est), 
                     alpha = as.numeric(input$alpha_1mean_est))
    }
  })
  output$n_1mean_est <- renderValueBox({
    valueBox(
      value = ceiling(n_1mean_est()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Hypothesis testing for 1 population mean #####
  # Sample size
  n_1mean_hypo <- reactive({
    req(as.numeric(input$m0_1mean_hypo)>0&
          as.numeric(input$ma_1mean_hypo)>0&
          as.numeric(input$sd_1mean_hypo)>0&
          as.numeric(input$alpha_1mean_hypo)>0&
          as.numeric(input$power_1mean_hypo)>0,
        cancelOutput = TRUE)
    fun_1mean_hypo(sd = as.numeric(input$sd_1mean_hypo), 
                   m_0 = as.numeric(input$m0_1mean_hypo), 
                   m_a = as.numeric(input$ma_1mean_hypo), 
                   alpha = as.numeric(input$alpha_1mean_hypo), 
                   power = as.numeric(input$power_1mean_hypo))
  })
  output$n_1mean_hypo <- renderValueBox({
    valueBox(
      value = ceiling(n_1mean_hypo()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  # Power
  power_1mean_hypo <- reactive({
    req(as.numeric(input$m0_1mean_hypo_power)>=0&
          as.numeric(input$ma_1mean_hypo_power)>=0&
          as.numeric(input$sd_1mean_hypo_power)>0&
          as.numeric(input$alpha_1mean_hypo_power)>0&
          as.numeric(input$n_1mean_hypo_power)>0,
        cancelOutput = TRUE)
    fun_1mean_hypo_power(m_0 = as.numeric(input$m0_1mean_hypo_power), 
                         m_a = as.numeric(input$ma_1mean_hypo_power), 
                         sd = as.numeric(input$sd_1mean_hypo_power), 
                         alpha = as.numeric(input$alpha_1mean_hypo_power), 
                         n = as.numeric(input$n_1mean_hypo_power))
  })
  output$power_1mean_hypo <- renderValueBox({
    valueBox(
      value = round(power_1mean_hypo(), 2),
      subtitle = "Power",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Estimating the difference between 2 population means #####
  n_2means_est <- reactive({
    req(as.numeric(input$sd_2means_est)>0&
          as.numeric(input$alpha_2means_est)>0&
          as.numeric(input$d_2means_est)>0,
        cancelOutput = TRUE)
    fun_2means_est(sd = as.numeric(input$sd_2means_est), 
                   d = as.numeric(input$d_2means_est), 
                   alpha = as.numeric(input$alpha_2means_est))
  })
  output$n_2means_est <- renderValueBox({
    valueBox(
      value = ceiling(n_2means_est()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Hypothesis test for 2 means #####
  # Sample size
  n_2means_hypo <- reactive({
    req(as.numeric(input$m1_2means_hypo)>=0&
          as.numeric(input$sd1_2means_hypo)>0&
          as.numeric(input$m2_2means_hypo)>=0&
          as.numeric(input$sd2_2means_hypo)>0&
          as.numeric(input$alpha_2means_hypo)>0&
          as.numeric(input$power_2means_hypo)>0, 
        cancelOutput = TRUE)
    fun_2means_hypo(m1 = as.numeric(input$m1_2means_hypo), 
                    m2 = as.numeric(input$m2_2means_hypo), 
                    sd1 = as.numeric(input$sd1_2means_hypo), 
                    sd2 = as.numeric(input$sd2_2means_hypo), 
                    alpha = as.numeric(input$alpha_2means_hypo), 
                    power = as.numeric(input$power_2means_hypo))
  })
  n1_2means_hypo <- reactive({
    req(input$k_2means_hypo>=1, cancelOutput = TRUE)
    big_n <- 2*n_2means_hypo()*(1+input$k_2means_hypo)^2/(4*input$k_2means_hypo)
    big_n/(1+input$k_2means_hypo)
  })
  output$n1_2means_hypo <- renderValueBox({
    valueBox(
      value = ceiling(n1_2means_hypo()),
      subtitle = "Nhóm 1",
      icon = icon("capsules"),
      color = "green",
    )
  })
  output$n2_2means_hypo <- renderValueBox({
    valueBox(
      value = input$k_2means_hypo*ceiling(n1_2means_hypo()),
      subtitle = "Nhóm 2",
      icon = icon("tablets"),
      color = "orange",
    )
  })
  
  # Power
  power_2means_hypo <- reactive({
    req(as.numeric(input$m1_2means_hypo_power)>=0&
          as.numeric(input$sd1_2means_hypo_power)>0&
          as.numeric(input$m2_2means_hypo_power)>=0&
          as.numeric(input$sd2_2means_hypo_power)>0&
          as.numeric(input$alpha_2means_hypo_power)>0&
          as.numeric(input$n_2means_hypo_power)>0,
        cancelOutput = TRUE)
    fun_2means_hypo_power(m1 = as.numeric(input$m1_2means_hypo_power), 
                          m2 = as.numeric(input$m2_2means_hypo_power), 
                          sd1 = as.numeric(input$sd1_2means_hypo_power), 
                          sd2 = as.numeric(input$sd2_2means_hypo_power), 
                          alpha = as.numeric(input$alpha_2means_hypo_power), 
                          n = as.numeric(input$n_2means_hypo_power))
  })
  output$power_2means_hypo <- renderValueBox({
    valueBox(
      value = round(power_2means_hypo(), 2),
      subtitle = "Power",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Cohort studies #####
  ##### Estimating a RR with specified relative precision #####
  n_cohort_est <- reactive({
    req(as.numeric(input$p1_cohort_est)>0&
          as.numeric(input$p2_cohort_est)>0&
          as.numeric(input$rr_cohort_est)>0&
          as.numeric(input$alpha_cohort_est)>0&
          as.numeric(input$eps_cohort_est)>0,
        cancelOutput = TRUE)
    fun_cohort_est(p1 = as.numeric(input$p1_cohort_est), 
                   p2 = as.numeric(input$p2_cohort_est), 
                   rr = as.numeric(input$rr_cohort_est), 
                   alpha = as.numeric(input$alpha_cohort_est), 
                   eps = as.numeric(input$eps_cohort_est))
  })
  output$n_cohort_est <- renderValueBox({
    valueBox(
      value = ceiling(n_cohort_est()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Hypothesis test for a RR #####
  # Sample size
  n_cohort_hypo <- reactive({
    req(as.numeric(input$p1_cohort_hypo)>0&
          as.numeric(input$p2_cohort_hypo)>0&
          as.numeric(input$rr0_cohort_hypo)>0&
          as.numeric(input$rra_cohort_hypo)>0&
          as.numeric(input$alpha_cohort_hypo)>0&
          as.numeric(input$power_cohort_hypo)>0,
        cancelOutput = TRUE)
    fun_cohort_hypo(p1 = as.numeric(input$p1_cohort_hypo),
                    p2 = as.numeric(input$p2_cohort_hypo),
                    alpha = as.numeric(input$alpha_cohort_hypo),
                    power = as.numeric(input$power_cohort_hypo))
  })
  output$n_cohort_hypo <- renderValueBox({
    valueBox(
      value = ceiling(n_cohort_hypo()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  # Power
  power_cohort_hypo <- reactive({
    req(as.numeric(input$p1_cohort_hypo_power)>0&
          as.numeric(input$p2_cohort_hypo_power)>0&
          as.numeric(input$alpha_cohort_hypo_power)>0&
          as.numeric(input$n_cohort_hypo_power)>0,
        cancelOutput = TRUE)
    fun_cohort_hypo_power(p1 = as.numeric(input$p1_cohort_hypo_power), 
                          p2 = as.numeric(input$p2_cohort_hypo_power), 
                          alpha = as.numeric(input$alpha_cohort_hypo_power), 
                          n = as.numeric(input$n_cohort_hypo_power))
  })
  output$power_cohort_hypo <- renderValueBox({
    valueBox(
      value = round(power_cohort_hypo(), 2),
      subtitle = "Power",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  
  ##### Sample survey #####
  ##### Simple random sampling #####
  n_simple_random <- reactive({
    req(as.numeric(input$N_simple_random)>0&
          as.numeric(input$P_simple_random)>0&
          as.numeric(input$alpha_simple_random)>0&
          as.numeric(input$d_simple_random)>0,
        cancelOutput = TRUE)
    fun_simple_random(N = as.numeric(input$N_simple_random), 
                      P = as.numeric(input$P_simple_random), 
                      alpha = as.numeric(input$alpha_simple_random), 
                      d = as.numeric(input$d_simple_random))
  })
  output$n_simple_random <- renderValueBox({
    valueBox(
      value = ceiling(n_simple_random()),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
  ##### Stratified sampling #####
  # observeEvent(input$no_strata_stratified, {
  #   output$strata <- renderUI({
  #     input_list <- lapply(1:input$no_strata_stratified, function(i) {
  #       inputName <- paste("N", i)
  #       numericInput(inputName, inputName, 1)
  #     })
  #     do.call(tagList, input_list)
  #   })
  # })
  
  ##### Correlation #####
  n_corr <- reactive({
    req(as.numeric(input$r_corr)>0&
          as.numeric(input$alpha_corr)>0&
          as.numeric(input$power_corr)>0, 
        cancelOutput = TRUE)
    pwr <- pwr.r.test(r = as.numeric(input$r_corr), 
                      sig.level = as.numeric(input$alpha_corr), 
                      power = as.numeric(input$power_corr))
    pwr$n
  })
  output$n_corr <- renderValueBox({
    valueBox(
      value = ceiling(n_corr()),
      subtitle = "Cỡ mẫu",
      icon = icon("users"),
      color = "green",
    )
  })
})
