library(shiny)
library(shinydashboard)
library(DT)
library(pwr)

# Function estimating the population mean
fun1_1mean_est <- function(sd, d, alpha) {
  z <- qnorm(1-alpha/2)
  n <- z^2*sd^2/d^2
  return(n)
}

fun2_1mean_est <- function(sd, eps, mean, alpha) {
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

# Function estimating the difference between 2 population means
fun_2means_est <- function(sd, d, alpha) {
  z <- qnorm(1-alpha/2)
  n <- 2*z^2*sd^2/d^2
  return(n)
}

# Function for estimating a RR
fun_cohort_est <- function(p1, p2, rr, alpha, eps) {
  z <- qnorm(1-alpha/2)
  n <- z^2*((1-p1)/p1+(1-p2)/p2)/log(1-eps, base = exp(1))^2
  return(n)
}

# Function for hypothesis test for a RR
fun_cohort_hypo <- function(p1, p2, rr_0, rr_a, alpha, power) {
  z_a <- qnorm(1-alpha/2)
  z_b <- qnorm(power)
  p <- mean(c(p1, p2))
  n <- (z_a*sqrt(2*p*(1-p))+z_b*sqrt(p1*(1-p1)+p2*(1-p2)))^2/(p1-p2)^2
}

# Function for simple random sampling
fun_simple_random <- function(N, P, alpha, d, eps) {
  z <- qnorm(1-alpha/2)
  n <- z^2*P*(1-P)*N/(d^2*(N-1)+z^2*P*(1-P))
}

shinyServer(function(input, output) {
  
##### Two proportions #####
  ##### Fast calculation #####
  n_2props <- reactive({
    req(as.numeric(input$p1_2props)>0&
          as.numeric(input$p2_2props)>0&
          as.numeric(input$alpha_2props)>0&
          as.numeric(input$power_2props)>0, 
        cancelOutput = TRUE)
    pwr <- pwr.2p.test(h = ES.h(p1 = as.numeric(input$p1_2props), p2 = as.numeric(input$p2_2props)),
                       sig.level = as.numeric(input$alpha_2props), 
                       power = as.numeric(input$power_2props))
    pwr$n
  })
  n1_2props <- reactive({
    req(input$k_2props>=1, cancelOutput = TRUE)
    big_n <- 2*n_2props()*(1+input$k_2props)^2/(4*input$k_2props)
    big_n/(1+input$k_2props)
  })
  output$n1_2props <- renderValueBox({
    valueBox(
      value = ceiling(n1_2props()),
      subtitle = "Nhóm 1",
      icon = icon("capsules"),
      color = "green",
    )
  })
  output$n2_2props <- renderValueBox({
    valueBox(
      value = input$k_2props*ceiling(n1_2props()),
      subtitle = "Nhóm 2",
      icon = icon("tablets"),
      color = "orange",
    )
  })

  ##### Input file #####
  # Make an example
  temp_2props <- reactive({
    data.frame(Outcome = c("Live birth", "Ongoing pregnancy"), 
               Treatment = c(0.513, 0.565), 
               Control = c(0.233, 0.288))
  })
  # Download template
  output$temp_2props <- downloadHandler(
    filename = "template_2props.csv",
    content = function(con) {
      write.csv(temp_2props(), row.names = F, con)
    }
  )
  # Estimate sample size
  df_2props <- reactive({
    req(as.numeric(input$alpha_df_2props)>0&
          as.numeric(input$power_df_2props)>0&
          input$k_df_2props>=1,
        cancelOutput = TRUE)
    if (is.null(input$upload_2props)) {
      df <- temp_2props()
    } else {
      df <- read.csv(file = input$upload_2props$datapath, 
                     header = T, stringsAsFactors = F, check.names = F, fill = T)
    }
    for (i in 1:nrow(df)) {
      pwr <- pwr.2p.test(h = ES.h(p1 = df$Treatment[i], p2 = df$Control[i]),
                         sig.level = as.numeric(input$alpha_df_2props), 
                         power = as.numeric(input$power_df_2props))
      df$n[i] <- pwr$n
    }
    df$big_n <- 2*df$n*(1+input$k_df_2props)^2/(4*input$k_df_2props)
    df$n1 <- ceiling(df$big_n/(1+input$k_df_2props))
    df$n2 <- input$k_df_2props*df$n1
    df$Total <- df$n1 + df$n2
    df$n <- NULL
    df$big_n <- NULL
    df
  })
  # Display text
  output$text_df_2props <- renderText({
    req(as.numeric(input$alpha_df_2props)>0&
          as.numeric(input$power_df_2props)>0&
          input$k_df_2props>=1,
        cancelOutput = TRUE)
    paste0("alpha = ", 
           input$alpha_df_2props, ", power = ", input$power_df_2props, 
           " và k = ", input$k_df_2props)
  })
  # Display sample size dataframe
  output$ss_df_2props <- DT::renderDataTable({
    df_2props()
  })
  # Download sample size dataframe
  output$download_df_2props <- downloadHandler(
    filename = "df_2props.csv",
    content = function(con) {
      write.csv(df_2props(), row.names = F, con)
    }
  )

  ##### Continuous variables #####
  ##### Estimating the population mean #####
  n_1mean_est <- reactive({
    req(as.numeric(input$mean_1mean_est)>0&
          as.numeric(input$sd_1mean_est)>0&
          as.numeric(input$alpha_1mean_est)>0&
          as.numeric(input$d_1mean_est)>0&
          as.numeric(input$eps_1mean_est)>0,
        cancelOutput = TRUE)
    fun1_1mean_est(sd = as.numeric(input$sd_1mean_est), 
                   d = as.numeric(input$d_1mean_est), 
                   alpha = as.numeric(input$alpha_1mean_est))
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
  
  ##### Two means #####
  # Sample size
  n_2means_hypo <- reactive({
    req(as.numeric(input$m1_2means_hypo)>=0&
          as.numeric(input$sd1_2means_hypo)>0&
          as.numeric(input$m2_2means_hypo)>=0&
          as.numeric(input$sd2_2means_hypo)>0&
          as.numeric(input$alpha_2means_hypo)>0&
          as.numeric(input$power_2means_hypo)>0, 
        cancelOutput = TRUE)
    numerator <- abs(as.numeric(input$m1_2means_hypo)-as.numeric(input$m2_2means_hypo))
    denominator<- sqrt(((as.numeric(input$sd1_2means_hypo)^2)+(as.numeric(input$sd2_2means_hypo)^2))/2)
    d <- numerator/denominator
    pwr <- pwr.t.test(d = d, 
                      sig.level = as.numeric(input$alpha_2means_hypo), 
                      power = as.numeric(input$power_2means_hypo), 
                      type = "two.sample",
                      alternative = "two.sided")
    pwr$n
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
    numerator <- abs(as.numeric(input$m1_2means_hypo_power)-as.numeric(input$m2_2means_hypo_power))
    denominator<- sqrt(((as.numeric(input$sd1_2means_hypo_power)^2)+(as.numeric(input$sd2_2means_hypo_power)^2))/2)
    d <- numerator/denominator
    pwr <- pwr.t.test(d = d, 
                      sig.level = as.numeric(input$alpha_2means_hypo_power), 
                      n = as.numeric(input$n_2means_hypo_power),
                      type = "two.sample",
                      alternative = "two.sided")
    pwr$power
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
  
  ##### Sample survey #####
  ##### Simple random sampling #####
  n_simple_random <- reactive({
    req(as.numeric(input$N_simple_random)>0&
          as.numeric(input$P_simple_random)>0&
          as.numeric(input$alpha_simple_random)>0&
          as.numeric(input$d_simple_random)>0&
          as.numeric(input$eps_simple_random)>0,
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
