library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# Load functions
source(file = "functions/categorical.R", local = TRUE)
source(file = "functions/continuous.R", local = TRUE)
source(file = "functions/cohort.R", local = TRUE)
source(file = "functions/case_control.R", local = TRUE)
source(file = "functions/simple_random.R", local = TRUE)
source(file = "functions/diagnosis.R", local = TRUE)

shinyServer(function(input, output) {
  
  ##### Categorical variables #####
  ##### Estimate 1 prop #####
  output$precision_1prop_est <- renderUI({
    if (input$precision_type_1prop_est == 1) {
      textInput(inputId = "d_1prop_est",
                label = "Sai số tuyệt đối (d)",
                value = 0.05)
    } else if (input$precision_type_1prop_est == 2) {
      textInput(inputId = "eps_1prop_est",
                label = HTML("Sai số tương đối (&epsilon;)"),
                value = 0.2)
    }
  })
  n_1prop_est <- reactive({
    req(as.numeric(input$p_1prop_est)>0&
          as.numeric(input$alpha_1prop_est)>0&
          (as.numeric(input$d_1prop_est)>0||
             as.numeric(input$eps_1prop_est)>0)&
          as.numeric(input$nonrep_1prop_est)>=0&
          as.numeric(input$nonrep_1prop_est)<=1,
        cancelOutput = TRUE)
    if (input$precision_type_1prop_est == 1) {
      fun1_1prop_est(p = as.numeric(input$p_1prop_est), 
                     d = as.numeric(input$d_1prop_est), 
                     alpha = as.numeric(input$alpha_1prop_est),
                     nonrep = as.numeric(input$nonrep_1prop_est),
                     deseff = input$deseff_1prop_est)
    } else if (input$precision_type_1prop_est == 2) {
      fun2_1prop_est(p = as.numeric(input$p_1prop_est), 
                     eps = as.numeric(input$eps_1prop_est), 
                     alpha = as.numeric(input$alpha_1prop_est),
                     nonrep = as.numeric(input$nonrep_1prop_est),
                     deseff = input$deseff_1prop_est)
    }
  })
  output$n_1prop_est <- renderText({
    n_1prop_est()
  })
  
  # Plot
  output$precision_1prop_est_plot <- renderUI({
    if (input$precision_type_1prop_est_plot == 1) {
      textInput(inputId = "d_1prop_est_plot",
                label = "Sai số tuyệt đối",
                value = "0.05 0.1 0.15")
    } else if (input$precision_type_1prop_est_plot == 2) {
      textInput(inputId = "eps_1prop_est_plot",
                label = "Sai số tương đối",
                value = "0.1 0.2 0.3")
    }
  })
  df_plot_1prop_est <- reactive({
    nonrep <- as.numeric(input$nonrep_1prop_est_plot)
    deseff <- input$deseff_1prop_est_plot
    if (input$precision_type_1prop_est_plot == 1) {
      req(!is.null(input$d_1prop_est_plot)&
              as.numeric(input$p_by_1prop_est_plot)>0,
          cancelOutput = TRUE)
      p <- seq(from = input$p_range_1prop_est_plot[1], 
               to = input$p_range_1prop_est_plot[2], 
               by = as.numeric(input$p_by_1prop_est_plot))
      d <- as.numeric(unlist(strsplit(input$d_1prop_est_plot, " ")))
      alpha <- as.numeric(input$alpha_1prop_est_plot)
      df <- expand.grid(d, p)
      names(df) <- c("d", "p")
      df$n <- fun1_1prop_est(p = df$p, d = df$d, alpha = alpha) 
                             # nonrep = nonrep, deseff = deseff)
      df$d <- as.factor(df$d)
      return(df)
    } else if (input$precision_type_1prop_est_plot == 2) {
      req(!is.null(input$eps_1prop_est_plot)&
              as.numeric(input$p_by_1prop_est_plot)>0,
          cancelOutput = TRUE)
      p <- seq(from = input$p_range_1prop_est_plot[1], 
               to = input$p_range_1prop_est_plot[2], 
               by = as.numeric(input$p_by_1prop_est_plot))
      eps <- as.numeric(unlist(strsplit(input$eps_1prop_est_plot, " ")))
      alpha <- as.numeric(input$alpha_1prop_est_plot)
      df <- expand.grid(eps, p)
      names(df) <- c("eps", "p")
      df$n <- fun2_1prop_est(p = df$p, eps = df$eps, alpha = alpha)
                             # nonrep = nonrep, deseff = deseff)
      df$eps <- as.factor(df$eps)
      return(df)
    }
  })
  output$plot_1prop_est <- renderPlotly({
    if (input$precision_type_1prop_est_plot == 1) {
      plot_ly(df_plot_1prop_est(), x = ~p, y = ~n, color = ~d,
              type = "scatter", mode = "lines+markers",
              text = paste0("<b>Sai số tuyệt đối:</b> ", df_plot_1prop_est()$d, "<br>",
                            "<b>Tỷ lệ:</b> ", df_plot_1prop_est()$p, "<br>",
                            "<b>Cỡ mẫu:</b> ", df_plot_1prop_est()$n),
              hoverinfo = "text") %>%
        layout(
          xaxis = list(title = list(text = "<b>Tỷ lệ</b>"),
                       zeroline = F),
          yaxis = list(title = list(text = "<b>Cỡ mẫu</b>")),
          legend = list(title = list(text = "<b>Sai số tuyệt đối</b>")),
          font = list(family = "Arial")
        )
    } else if (input$precision_type_1prop_est_plot == 2) {
      plot_ly(df_plot_1prop_est(), x = ~p, y = ~n, color = ~eps,
              type = "scatter", mode = "lines+markers",
              text = paste0("<b>Sai số tương đối:</b> ", df_plot_1prop_est()$eps, "<br>",
                            "<b>Tỷ lệ:</b> ", df_plot_1prop_est()$p, "<br>",
                            "<b>Cỡ mẫu:</b> ", df_plot_1prop_est()$n),
              hoverinfo = "text") %>%
        layout(
          xaxis = list(title = list(text = "<b>Tỷ lệ</b>"),
                       zeroline = F),
          yaxis = list(title = list(text = "<b>Cỡ mẫu</b>")),
          legend = list(title = list(text = "<b>Sai số tương đối</b>")),
          font = list(family = "Arial")
        )
    }
  })
  output$table_1prop_est <- renderDataTable({
    df_plot_1prop_est()
  })
  
  ##### Hypothesis test for a population proportion #####
  # Sample size
  n_1prop_hypo <- reactive({
    req(as.numeric(input$p0_1prop_hypo)>0&
          as.numeric(input$pa_1prop_hypo)>0&
          as.numeric(input$alpha_1prop_hypo)>0&
          as.numeric(input$power_1prop_hypo)>0&
          as.numeric(input$nonrep_1prop_hypo)>=0&
          as.numeric(input$nonrep_1prop_hypo)<=1,
        cancelOutput = TRUE)
    fun_1prop_hypo(p_0 = as.numeric(input$p0_1prop_hypo), 
                   p_a = as.numeric(input$pa_1prop_hypo), 
                   alpha = as.numeric(input$alpha_1prop_hypo), 
                   power = as.numeric(input$power_1prop_hypo),
                   nonrep = as.numeric(input$nonrep_1prop_hypo), 
                   deseff = input$deseff_1prop_hypo)
  })
  output$n_1prop_hypo <- renderText({
    n_1prop_hypo()
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
  output$power_1prop_hypo <- renderText({
    power_1prop_hypo()
  })
  
  ##### Estimate 2 proportions difference #####
  n_2props_est <- reactive({
    req(as.numeric(input$p1_2props_est)>0&
          as.numeric(input$p2_2props_est)>0&
          as.numeric(input$alpha_2props_est)>0&
          as.numeric(input$d_2props_est)>0&
          as.numeric(input$nonrep_2props_est)>=0&
          as.numeric(input$nonrep_2props_est)<=1,
        cancelOutput = TRUE)
    fun_2props_est(p1 = as.numeric(input$p1_2props_est), 
                   p2 = as.numeric(input$p2_2props_est), 
                   alpha = as.numeric(input$alpha_2props_est), 
                   d = as.numeric(input$d_2props_est),
                   nonrep = as.numeric(input$nonrep_2props_est), 
                   deseff = input$deseff_2props_est)
  })
  output$n_2props_est <- renderText({
    n_2props_est()
  })
  
  ##### Hypothesis test for 2 proportions #####
  n_2props_hypo <- reactive({
    req(as.numeric(input$p1_2props_hypo)>0&
          as.numeric(input$p2_2props_hypo)>0&
          as.numeric(input$alpha_2props_hypo)>0&
          as.numeric(input$power_2props_hypo)>0&
          as.numeric(input$nonrep_2props_hypo)>=0&
          as.numeric(input$nonrep_2props_hypo)<=1,
        cancelOutput = TRUE)
    fun_2props_hypo(p1 = as.numeric(input$p1_2props_hypo),
                    p2 = as.numeric(input$p2_2props_hypo),
                    alpha = as.numeric(input$alpha_2props_hypo),
                    power = as.numeric(input$power_2props_hypo),
                    nonrep = as.numeric(input$nonrep_2props_hypo), 
                    deseff = input$deseff_2props_hypo)
  })
  n1_2props_hypo <- reactive({
    req(input$k_2props_hypo>=1, cancelOutput = TRUE)
    big_n <- 2*n_2props_hypo()*(1+input$k_2props_hypo)^2/(4*input$k_2props_hypo)
    big_n/(1+input$k_2props_hypo)
  })
  output$n1_2props_hypo <- renderText({
    n1_2props_hypo()
  })
  output$n2_2props_hypo <- renderText({
    input$k_2props_hypo*n1_2props_hypo()
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
  output$power_2props_hypo <- renderText({
    power_2props_hypo()
  })
  
  # Plot 1
  output$diff_range_2props_hypo_plot1 <- renderUI({
    sliderInput(inputId = "diff_range_2props_hypo_plot1",
                label = "Chọn khoảng khác biệt muốn vẽ",
                min = 0, max = 1-as.numeric(input$p1_2props_hypo_plot1), 
                value = c(0.1, 1-as.numeric(input$p1_2props_hypo_plot1))
    )
  })
  df_plot1_2props_hypo <- reactive({
    req(as.numeric(input$p1_2props_hypo_plot1)>=0&
          as.numeric(input$alpha_2props_hypo_plot1)>0&
          as.numeric(input$diff_by_2props_hypo_plot1)>0&
          !is.null(input$power_2props_hypo_plot1)>0&
          !is.null(input$diff_range_2props_hypo_plot1[1]),
        cancelOutput = TRUE)
    p1 <- as.numeric(input$p1_2props_hypo_plot1)
    diff <- seq(from = input$diff_range_2props_hypo_plot1[1],
                to = input$diff_range_2props_hypo_plot1[2],
                by = as.numeric(input$diff_by_2props_hypo_plot1))
    alpha <- as.numeric(input$alpha_2props_hypo_plot1)
    power <- as.numeric(unlist(strsplit(input$power_2props_hypo_plot1, " ")))
    df <- expand.grid(p1, diff, alpha, power)
    colnames(df) <- c("p1", "diff", "alpha", "power")
    df$p2 <- df$p1+df$diff
    df$n <- fun_2props_hypo(p1 = df$p1, p2 = df$p2, alpha = df$alpha, power = df$power)
    df$power <- as.factor(df$power)
    return(df)
  })
  output$plot1_2props_hypo <- renderPlotly({
    plot_ly(df_plot1_2props_hypo(), x = ~diff, y = ~n, color = ~power,
            type = "scatter", mode = "lines+markers",
            text = paste0("<b>p1:</b> ", df_plot1_2props_hypo()$p1, "<br>",
                          "<b>p2:</b> ", df_plot1_2props_hypo()$p2, "<br>",
                          "<b>Power:</b> ", df_plot1_2props_hypo()$power, "<br>",
                          "<b>Cỡ mẫu:</b> ", df_plot1_2props_hypo()$n),
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title = list(text = "<b>Sự khác biệt</b>"),
                     zeroline = F),
        yaxis = list(title = list(text = "<b>Cỡ mẫu</b>")),
        legend = list(title = list(text = "<b>Power</b>")),
        font = list(family = "Arial")
      )
  })
  
  # Plot 2
  df_plot2_2props_hypo <- reactive({
    req(as.numeric(input$p1_2props_hypo_plot2)>=0&
          as.numeric(input$p2_2props_hypo_plot2)>=0&
          as.numeric(input$alpha_2props_hypo_plot2)>0&
          as.numeric(input$power_by_2props_hypo_plot2)>0,
        cancelOutput = TRUE)
    p1 <- as.numeric(input$p1_2props_hypo_plot2)
    p2 <- as.numeric(input$p2_2props_hypo_plot2)
    alpha <- as.numeric(input$alpha_2props_hypo_plot2)
    power <- seq(from = input$power_range_2props_hypo_plot2[1],
                 to = input$power_range_2props_hypo_plot2[2],
                 by = as.numeric(input$power_by_2props_hypo_plot2))
    df <- expand.grid(p1, p2, alpha, power)
    colnames(df) <- c("p1", "p2", "alpha", "power")
    df$n <- fun_2props_hypo(p1 = df$p1, p2 = df$p2, alpha = df$alpha, power = df$power)
    return(df)
  })
  output$plot2_2props_hypo <- renderPlotly({
    plot_ly(df_plot2_2props_hypo(), x = ~n, y = ~power,
            type = "scatter", mode = "lines+markers",
            text = paste0("<b>p1:</b> ", df_plot2_2props_hypo()$p1, "<br>",
                          "<b>p2:</b> ", df_plot2_2props_hypo()$p2, "<br>",
                          "<b>Power:</b> ", df_plot2_2props_hypo()$power, "<br>",
                          "<b>Cỡ mẫu:</b> ", df_plot2_2props_hypo()$n),
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title = list(text = "<b>Cỡ mẫu</b>"),
                     zeroline = F),
        yaxis = list(title = list(text = "<b>Power</b>")),
        font = list(family = "Arial")
      )
  })

  ##### Hypothesis test for 2 proportions (small proportion) #####
  n_2props_hypo_small <- reactive({
    req(as.numeric(input$p1_2props_hypo_small)>0&
          as.numeric(input$p2_2props_hypo_small)>0&
          as.numeric(input$alpha_2props_hypo_small)>0&
          as.numeric(input$power_2props_hypo_small)>0&
          as.numeric(input$nonrep_2props_hypo_small)>=0&
          as.numeric(input$nonrep_2props_hypo_small)<=1,
        cancelOutput = TRUE)
    fun_2props_hypo_small(p1 = as.numeric(input$p1_2props_hypo_small),
                    p2 = as.numeric(input$p2_2props_hypo_small),
                    alpha = as.numeric(input$alpha_2props_hypo_small),
                    power = as.numeric(input$power_2props_hypo_small),
                    nonrep = as.numeric(input$nonrep_2props_hypo_small), 
                    deseff = input$deseff_2props_hypo_small)
  })
  n1_2props_hypo_small <- reactive({
    req(input$k_2props_hypo_small>=1, cancelOutput = TRUE)
    big_n <- 2*n_2props_hypo_small()*(1+input$k_2props_hypo_small)^2/(4*input$k_2props_hypo_small)
    big_n/(1+input$k_2props_hypo_small)
  })
  output$n1_2props_hypo_small <- renderText({
    n1_2props_hypo_small()
  })
  output$n2_2props_hypo_small <- renderText({
    input$k_2props_hypo_small*n1_2props_hypo_small()
  })
  
  # Power
  power_2props_hypo_small <- reactive({
    req(as.numeric(input$p1_2props_hypo_small_power)>0&
          as.numeric(input$p2_2props_hypo_small_power)>0&
          as.numeric(input$alpha_2props_hypo_small_power)>0&
          as.numeric(input$n_2props_hypo_small_power)>0,
        cancelOutput = TRUE)
    fun_2props_hypo_small_power(p1 = as.numeric(input$p1_2props_hypo_small_power), 
                                p2 = as.numeric(input$p2_2props_hypo_small_power), 
                                alpha = as.numeric(input$alpha_2props_hypo_small_power), 
                                n = as.numeric(input$n_2props_hypo_small_power))
  })
  output$power_2props_hypo_small <- renderText({
    power_2props_hypo_small()
  })
  
 

  ##### Continuous variables #####
  ##### Estimating the population mean #####
  output$precision_1mean_est <- renderUI({
    if (input$precision_type_1mean_est == 1) {
      textInput(inputId = "d_1mean_est",
                label = "Sai số tuyệt đối (d)",
                value = 0.1)
    } else if (input$precision_type_1mean_est == 2) {
      list(
        textInput(inputId = "eps_1mean_est",
                  label = HTML("Sai số tương đối (&epsilon;)"),
                  value = 0.05),
        textInput(inputId = "mean_1mean_est", 
                  label = HTML("Trung bình quần thể (&mu;)"), 
                  value = 10)
      )
    }
  })
  n_1mean_est <- reactive({
    req(as.numeric(input$sd_1mean_est)>0&
          as.numeric(input$alpha_1mean_est)>0&
          (as.numeric(input$d_1mean_est)>0||
          as.numeric(input$eps_1mean_est)>0||
          as.numeric(input$mean_1mean_est)>=0)&
          as.numeric(input$nonrep_1mean_est)>=0&
          as.numeric(input$nonrep_1mean_est)<=1,
        cancelOutput = TRUE)
    if (input$precision_type_1mean_est == 1) {
      fun1_1mean_est(sd = as.numeric(input$sd_1mean_est), 
                     d = as.numeric(input$d_1mean_est), 
                     alpha = as.numeric(input$alpha_1mean_est),
                     nonrep = as.numeric(input$nonrep_1mean_est), 
                     deseff = input$deseff_1mean_est)
    } else if (input$precision_type_1mean_est == 2) {
      fun2_1mean_est(sd = as.numeric(input$sd_1mean_est), 
                     mean = as.numeric(input$mean_1mean_est),
                     eps = as.numeric(input$eps_1mean_est), 
                     alpha = as.numeric(input$alpha_1mean_est),
                     nonrep = as.numeric(input$nonrep_1mean_est), 
                     deseff = input$deseff_1mean_est)
    }
  })
  output$n_1mean_est <- renderText({
    n_1mean_est()
  })
  
  ##### Hypothesis testing for 1 population mean #####
  # Sample size
  n_1mean_hypo <- reactive({
    req(as.numeric(input$m0_1mean_hypo)>0&
          as.numeric(input$ma_1mean_hypo)>0&
          as.numeric(input$sd_1mean_hypo)>0&
          as.numeric(input$alpha_1mean_hypo)>0&
          as.numeric(input$power_1mean_hypo)>0&
          as.numeric(input$nonrep_1mean_hypo)>=0&
          as.numeric(input$nonrep_1mean_hypo)<=1,
        cancelOutput = TRUE)
    fun_1mean_hypo(sd = as.numeric(input$sd_1mean_hypo), 
                   m_0 = as.numeric(input$m0_1mean_hypo), 
                   m_a = as.numeric(input$ma_1mean_hypo), 
                   alpha = as.numeric(input$alpha_1mean_hypo), 
                   power = as.numeric(input$power_1mean_hypo),
                   nonrep = as.numeric(input$nonrep_1mean_hypo), 
                   deseff = input$deseff_1mean_hypo)
  })
  output$n_1mean_hypo <- renderText({
    n_1mean_hypo()
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
  output$power_1mean_hypo <- renderText({
    power_1mean_hypo()
  })
  
  ##### Estimating the difference between 2 population means #####
  n_2means_est <- reactive({
    req(as.numeric(input$sd_2means_est)>0&
          as.numeric(input$alpha_2means_est)>0&
          as.numeric(input$d_2means_est)>0&
          as.numeric(input$nonrep_2means_est)>=0&
          as.numeric(input$nonrep_2means_est)<=1,
        cancelOutput = TRUE)
    fun_2means_est(sd = as.numeric(input$sd_2means_est), 
                   d = as.numeric(input$d_2means_est), 
                   alpha = as.numeric(input$alpha_2means_est),
                   nonrep = as.numeric(input$nonrep_2means_est), 
                   deseff = input$deseff_2means_est)
  })
  output$n_2means_est <- renderText({
    n_2means_est()
  })
  
  ##### Hypothesis test for 2 means #####
  # Sample size
  n_2means_hypo <- reactive({
    req(as.numeric(input$m1_2means_hypo)>=0&
          as.numeric(input$sd1_2means_hypo)>0&
          as.numeric(input$m2_2means_hypo)>=0&
          as.numeric(input$sd2_2means_hypo)>0&
          as.numeric(input$alpha_2means_hypo)>0&
          as.numeric(input$power_2means_hypo)>0&
          as.numeric(input$nonrep_2means_hypo)>=0&
          as.numeric(input$nonrep_2means_hypo)<=1, 
        cancelOutput = TRUE)
    fun_2means_hypo(m1 = as.numeric(input$m1_2means_hypo), 
                    m2 = as.numeric(input$m2_2means_hypo), 
                    sd1 = as.numeric(input$sd1_2means_hypo), 
                    sd2 = as.numeric(input$sd2_2means_hypo), 
                    alpha = as.numeric(input$alpha_2means_hypo), 
                    power = as.numeric(input$power_2means_hypo),
                    nonrep = as.numeric(input$nonrep_2means_hypo), 
                    deseff = input$deseff_2means_hypo)
  })
  n1_2means_hypo <- reactive({
    req(input$k_2means_hypo>=1, cancelOutput = TRUE)
    big_n <- 2*n_2means_hypo()*(1+input$k_2means_hypo)^2/(4*input$k_2means_hypo)
    big_n/(1+input$k_2means_hypo)
  })
  output$n1_2means_hypo <- renderText({
    ceiling(n1_2means_hypo())
  })
  output$n2_2means_hypo <- renderText({
    ceiling(input$k_2means_hypo*n1_2means_hypo())
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
  output$power_2means_hypo <- renderText({
    power_2means_hypo()
  })
  
  ##### Cohort studies #####
  ##### Estimating a RR with specified relative precision #####
  n_cohort_est <- reactive({
    req(as.numeric(input$p1_cohort_est)>0&
          as.numeric(input$p2_cohort_est)>0&
          as.numeric(input$alpha_cohort_est)>0&
          as.numeric(input$eps_cohort_est)>0&
          as.numeric(input$nonrep_cohort_est)>=0&
          as.numeric(input$nonrep_cohort_est)<=1,
        cancelOutput = TRUE)
    fun_cohort_est(p1 = as.numeric(input$p1_cohort_est), 
                   p2 = as.numeric(input$p2_cohort_est), 
                   alpha = as.numeric(input$alpha_cohort_est), 
                   eps = as.numeric(input$eps_cohort_est),
                   nonrep = as.numeric(input$nonrep_cohort_est), 
                   deseff = input$deseff_cohort_est)
  })
  output$n_cohort_est <- renderText({
    n_cohort_est()
  })
  
  ##### Hypothesis test for a RR #####
  # Sample size
  n_cohort_hypo <- reactive({
    req(as.numeric(input$p1_cohort_hypo)>0&
          as.numeric(input$p2_cohort_hypo)>0&
          as.numeric(input$alpha_cohort_hypo)>0&
          as.numeric(input$power_cohort_hypo)>0&
          as.numeric(input$nonrep_cohort_hypo)>=0&
          as.numeric(input$nonrep_cohort_hypo)<=1,
        cancelOutput = TRUE)
    fun_cohort_hypo(p1 = as.numeric(input$p1_cohort_hypo),
                    p2 = as.numeric(input$p2_cohort_hypo),
                    alpha = as.numeric(input$alpha_cohort_hypo),
                    power = as.numeric(input$power_cohort_hypo),
                    nonrep = as.numeric(input$nonrep_cohort_hypo), 
                    deseff = input$deseff_cohort_hypo)
  })
  output$n_cohort_hypo <- renderText({
    n_cohort_hypo()
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
  output$power_cohort_hypo <- renderText({
    power_cohort_hypo()
  })
  
  # Case control studies #
  # Estimating a OR with specified relative precision #
  n_case_est <- reactive({
    req(as.numeric(input$p1_case_est)>0&
          as.numeric(input$p2_case_est)>0&
          as.numeric(input$alpha_case_est)>0&
          as.numeric(input$eps_case_est)>0&
          as.numeric(input$nonrep_case_est)>=0&
          as.numeric(input$nonrep_case_est)<=1,
        cancelOutput = TRUE)
    fun_case_est(p1 = as.numeric(input$p1_case_est),
                 p2 = as.numeric(input$p2_case_est),
                 alpha = as.numeric(input$alpha_case_est),
                 eps = as.numeric(input$eps_case_est),
                 nonrep = as.numeric(input$nonrep_case_est), 
                 deseff = input$deseff_case_est)
  })
  
  output$n_case_est <- renderText({
    n_case_est()
  })
  
  # Hypothesis test for a OR
  n_case_hypo <- reactive({
    req(as.numeric(input$p1_case_hypo)>0&
          as.numeric(input$p2_case_hypo)>0&
          as.numeric(input$alpha_case_hypo)>0&
          as.numeric(input$power_case_hypo)>0&
          as.numeric(input$nonrep_case_hypo)>=0&
          as.numeric(input$nonrep_case_hypo)<=1,
        cancelOutput = TRUE)
    fun_case_hypo(p1 = as.numeric(input$p1_case_hypo),
                  p2 = as.numeric(input$p2_case_hypo),
                  alpha = as.numeric(input$alpha_case_hypo),
                  power = as.numeric(input$power_case_hypo),
                  nonrep = as.numeric(input$nonrep_case_hypo), 
                  deseff = input$deseff_case_hypo)
  })
  output$n_case_hypo <- renderText({
    n_case_hypo()
  })
  
  # Power
  power_case_hypo <- reactive({
    req(as.numeric(input$p1_case_hypo_power)>0 &
          as.numeric(input$p2_case_hypo_power)>0 &
          as.numeric(input$alpha_case_hypo_power)>0 &
          as.numeric(input$n_case_hypo_power)>0,
        cancelOutput = TRUE)
    fun_case_hypo_power(p1 = as.numeric(input$p1_case_hypo_power),
                        p2 = as.numeric(input$p2_case_hypo_power),
                        alpha = as.numeric(input$alpha_case_hypo_power),
                        n = as.numeric(input$n_case_hypo_power))
  })
  
  output$power_case_hypo <- renderText({
    power_case_hypo()
  })
  
  
  ##### Sample survey #####
  ##### Simple random sampling #####
  n_simple_random <- reactive({
    req(as.numeric(input$N_simple_random)>0&
          as.numeric(input$P_simple_random)>0&
          as.numeric(input$alpha_simple_random)>0&
          as.numeric(input$d_simple_random)>0&
          as.numeric(input$nonrep_simple_random)>=0&
          as.numeric(input$nonrep_simple_random)<=1,
        cancelOutput = TRUE)
    fun_simple_random(N = as.numeric(input$N_simple_random), 
                      P = as.numeric(input$P_simple_random), 
                      alpha = as.numeric(input$alpha_simple_random), 
                      d = as.numeric(input$d_simple_random),
                      nonrep = as.numeric(input$nonrep_simple_random), 
                      deseff = input$deseff_simple_random)
  })
  output$n_simple_random <- renderValueBox({
    valueBox(
      value = n_simple_random(),
      subtitle = "Cỡ mẫu",
      icon = icon("capsules"),
      color = "green",
    )
  })
  
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
  output$n_corr <- renderText({
    ceiling(n_corr())
  })
  
  ##### Sensitivity #####
  # Sample size
  n_sen <- reactive({
    req(as.numeric(input$alphasens)>0&
          as.numeric(input$sen_sens)>0&
          as.numeric(input$error_sens)>0&
          as.numeric(input$p_sens)>0,
        cancelOutput = TRUE)
    fun_sen_est(alpha = as.numeric(input$alphasens), 
                sens = as.numeric(input$sen_sens), 
                d = as.numeric(input$error_sens),
                p = as.numeric(input$p_sens))
  })
  
  output$n_sen <- renderText({
    n_sen()
  })
  
  ##### Specificity #####
  # Sample size
  n_spec <- reactive({
    req(as.numeric(input$alphaspec)>0&
          as.numeric(input$spec_spec)>0&
          as.numeric(input$error_spec)>0&
          as.numeric(input$p_spec)>0,
        cancelOutput = TRUE)
    fun_spec_est(alpha = as.numeric(input$alphaspec), 
                spec = as.numeric(input$spec_spec), 
                d = as.numeric(input$error_spec),
                p = as.numeric(input$p_spec))
  })
  
  output$n_spec <- renderText({
    n_spec()
  })
  
})

