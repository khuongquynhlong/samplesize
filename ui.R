library(shiny)
library(shinydashboard)
library(DT)
library(pwr)

# shinydashboard icons were obtained at:
# https://fontawesome.com/icons

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Biến định tính", 
            icon = icon("percentage"), startExpanded = FALSE,
            menuSubItem("Ước lượng 1 tỷ lệ với sai số tuyệt đối", tabName = "1prop_est_abs"),
            menuSubItem("Ước lượng 1 tỷ lệ với sai số tương đối", tabName = "1prop_est_rel"),
            menuSubItem("So sánh với tỷ lệ quần thể", tabName = "1prop_hypo_pop"),
            menuSubItem("So sánh 2 tỷ lệ", tabName = "2props"),
            menuSubItem("So sánh 2 tỷ lệ (tỷ lệ nhỏ)", tabName = "2props_small")
            ),
        menuItem(
            "Biến định lượng", 
            icon = icon("chart-line"), startExpanded = FALSE,
            menuSubItem("Ước lượng 1 trung bình", tabName = "1mean_est"),
            menuSubItem("Kiểm định giả thuyết cho 1 trung bình", tabName = "1mean_hypo"),
            menuSubItem("Ước lượng khác biệt giữa 2 trung bình", tabName = "2means_est"),
            menuSubItem("Kiểm định giả thuyết cho 2 trung bình", tabName = "2means_hypo")
            ),
        menuItem(
            "Nghiên cứu thuần tập",
            icon = icon("university"), startExpanded = FALSE,
            menuSubItem("Ước lượng nguy cơ tương đối", tabName = "cohort_est"),
            menuSubItem("Kiểm định giả thuyết cho nguy cơ tương đối", tabName = "cohort_hypo")
        ),
        menuItem(
            "Nghiên cứu bệnh chứng",
            icon = icon("university"), startExpanded = FALSE,
            menuSubItem("Ước lượng OR với sai số tương đối", tabName = "1OR_est"),
            menuSubItem("So sánh 2 OR", tabName = "2OR_hypo")
        ),
        menuItem(
            "Sample surveys",
            icon = icon("poll"), startExpanded = FALSE,
            menuSubItem("Simple random sampling", tabName = "simple_random"),
            menuSubItem("Stratified sampling", tabName = "stratified")
        ),
        menuItem(
            "Hệ số tương quan", 
            icon = icon("chart-bar"), startExpanded = FALSE,
            tabName = "corr"
            ),
        menuItem(
            "Credit", selected = TRUE,
            tabName = "credit"
        )
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        
        ##### Categorical variables #####
        ##### Estimating the population proportion with absolute precision #####
        
        tabItem(
            tabName = "1prop_est_abs",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2P(1-P)}{d^2}$$")
                        ),
                        box(
                            width = 6, side = "left",
                                title = "Tính cỡ mẫu",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p_1prop_est_abs", 
                                                  label = "Tỷ lệ ước lượng", 
                                                  value = 0.3)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_1prop_est_abs",
                                                  label = HTML("Khoảng tin cậy (1 - &alpha;)"),
                                                  value = 0.95),
                                        textInput(inputId = "d_1prop_est_abs",
                                                  label = "Sai số tuyệt đối (d)",
                                                  value = 0.05)
                                    ),
                                    valueBoxOutput(outputId = "n_1prop_est_abs", width = 6)
                                ),
                        )
                    )
                )
            )
        ),        

        
        ##### Estimating the population proportion with relative precision #####
        
        tabItem(
            tabName = "1prop_est_rel",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=Z_{1-\\frac{\\alpha}{2}}^2 \\frac{1-P}{\\varepsilon^2P}$$")
                        ),
                        box(
                            width = 6, side = "left",
                            title = "Tính cỡ mẫu",
                            fluidRow(
                                box(
                                    status = "success",
                                    textInput(inputId = "p_1prop_est_rel", 
                                              label = "Tỷ lệ ước lượng", 
                                              value = 0.3)
                                ),
                                box(
                                    status = "warning",
                                    textInput(inputId = "alpha_1prop_est_rel",
                                              label = HTML("Khoảng tin cậy (1 - &alpha;)"),
                                              value = 0.95),
                                    textInput(inputId = "d_1prop_est_rel",
                                              label = HTML("Sai số tương đối (&varepsilon;)"),
                                              value = 0.1)
                                ),
                                valueBoxOutput(outputId = "n_1prop_est_rel", width = 6)
                            ),
                        )
                    )
                )
            )
        ),        
        
        ##### Hypothesis test for 1 population proportion #####
        
        tabItem(
            tabName = "1prop_hypo_pop",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{(Z_{1-\\frac{\\alpha}{2}}\\sqrt{P_o(1-P_o)} + Z_{1-\\beta}\\sqrt{P_a(1-P_a)})^2}{(P_a - P_o)^2}$$")
                        ),
                        tabBox(
                            width = 6, side = "left",
                            tabPanel(
                                title = "Tính cỡ mẫu",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p0_1prop_hypo_pop", 
                                                  label = "Tỷ lệ quần thể", 
                                                  value = 0.30),
                                        textInput(inputId = "p1_1prop_hypo_pop",
                                                  label = "Tỷ lệ ước tính",
                                                  value = 0.20)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_1prop_hypo_pop",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_1prop_hypo_pop",
                                                  label = "Power",
                                                  value = 0.8)
                                    ),
                                    valueBoxOutput(outputId = "n_1prop_hypo_pop", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p0_1prop_hypo_pop_power", 
                                                  label = "Tỷ lệ quần thể", 
                                                  value = 0.30),
                                        textInput(inputId = "p1_1prop_hypo_pop_power",
                                                  label = "Tỷ lệ ước tính",
                                                  value = 0.20)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_1prop_hypo_pop_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_1prop_hypo_pop",
                                                  label = "Cỡ mẫu",
                                                  value = 100)
                                    ),
                                    valueBoxOutput(outputId = "power_1prop_hypo_pop", width = 6)
                                )
                            )
                        )
                    )
                )
            )
        ),       
        
        ##### Hypothesis test for two proportion #####
        
        tabItem(
            tabName = "2props",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tham số", width = 4,
                            box(
                                textInput(inputId = "p1_2props", 
                                          label = "Tỷ lệ nhóm 1", 
                                          value = 0.5),
                                textInput(inputId = "p2_2props", 
                                          label = "Tỷ lệ nhóm 2", 
                                          value = 0.3),
                                numericInput(inputId = "k_2props",
                                             label = "Tỷ số 2 nhóm",
                                             value = 1)
                            ),
                            box(
                                textInput(inputId = "alpha_2props",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "power_2props",
                                          label = "Power",
                                          value = 0.8)
                            ),
                        ),
                        box(title = "Hướng dẫn", width = 8,
                            withMathJax(),
                            p("Phần này sẽ dùng để giải thích ý nghĩa các tham số"),
                            p("Bao gồm cả công thức"),
                            p("$$n=\\frac{(Z_{1-\\frac{\\alpha}{2}}\\sqrt{P_0(1-P_0)}+Z_{1-\\beta}\\sqrt{P_a(1-P_a)})^2}{(P_a-P_0)^2}$$")
                        ),
                        valueBoxOutput(outputId = "n1_2props"),
                        valueBoxOutput(outputId = "n2_2props")
                    ),
                ),
                tabPanel(
                    title = "Nhập file",
                    fluidRow(
                        box(width = 4,
                            "Nhấn vào đây để tải mẫu về",
                            p(),
                            downloadButton(outputId = "temp_2props",
                                           label = "Tải về"),
                            p(),
                            "Điền các tỷ lệ của từng biến kết cuộc theo mẫu và tải lên",
                            p(),
                            fileInput(inputId = "upload_2props",
                                      label = "Tải lên",
                                      multiple = F),
                            box(
                                numericInput(inputId = "k_df_2props",
                                             label = "Tỷ số 2 nhóm",
                                             value = 1)
                            ),
                            box(
                                textInput(inputId = "alpha_df_2props",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "power_df_2props",
                                          label = "Power",
                                          value = 0.8)
                            )
                        ),
                        box(title = "Kết quả", width = 8, status = "success", solidHeader = TRUE,
                            div(downloadButton(outputId = "download_df_2props", label = "Tải về"), style = "float:right"),
                            textOutput(outputId = "text_df_2props"),
                            DT::dataTableOutput(outputId = "ss_df_2props")
                        ),
                    )
                )
            )
        ),

        
        ##### Hypothesis test for two proportion (small proportion) #####
        
        tabItem(
            tabName = "2props_small",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})^2}{0.00061(\\arcsin\\sqrt{P_2}-\\arcsin\\sqrt{P_1})^2}$$")
                        ),
                        tabBox(
                            width = 6, side = "left",
                            tabPanel(
                                title = "Tính cỡ mẫu",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p1_2props_small", 
                                                  label = "Tỷ lệ nhóm 1", 
                                                  value = 0.30),
                                        textInput(inputId = "p2_2props_small",
                                                  label = "Tỷ lệ nhóm 2",
                                                  value = 0.20)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_2props_small",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_2props_small",
                                                  label = "Power",
                                                  value = 0.8),
                                        numericInput(inputId = "k_2props_small",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    valueBoxOutput(outputId = "n1_2props_small", width = 6),
                                    valueBoxOutput(outputId = "n2_2props_small", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p1_2props_small_power", 
                                                  label = "Tỷ lệ nhóm 1", 
                                                  value = 0.30),
                                        textInput(inputId = "p2_2props_small_power",
                                                  label = "Tỷ lệ nhóm 2",
                                                  value = 0.20)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_2props_small_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_2props_small_power",
                                                  label = "Cỡ mẫu",
                                                  value = 100)
                                    ),
                                    valueBoxOutput(outputId = "power_2props_small_p", width = 6)
                                )
                            )
                        )
                    )
                )
            )
        ),        
        
        ##### Continuous variables #####
        ##### Estimating the population mean #####
        tabItem(
            tabName = "1mean_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                radioButtons(inputId = "precision_type_1mean_est", 
                                             label = "Chọn loại sai số", 
                                             choices = c("Absolute precision" = 1,
                                                         "Relative precision" = 2)),
                                textInput(inputId = "sd_1mean_est", 
                                          label = "Population standard deviation", 
                                          value = 0.85),
                                textInput(inputId = "alpha_1mean_est",
                                          label = "Alpha",
                                          value = 0.05)
                            ),
                            box(
                                uiOutput(outputId = "precision_1mean_est")
                            ),
                            valueBoxOutput(outputId = "n_1mean_est", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            conditionalPanel(
                                condition = "input.precision_type_1mean_est == 1",
                                withMathJax(),
                                p("Công thức 1: $$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2\\sigma^2}{d^2}$$")
                            ),
                            conditionalPanel(
                                condition = "input.precision_type_1mean_est == 2",
                                withMathJax(),
                                p("Công thức 2: $$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2\\sigma^2}{\\varepsilon^2\\mu^2}$$")
                            )
                        )
                    )
                )
            )
        ),
        
        ##### Hypothesis testing for 1 population mean #####
        tabItem(
            tabName = "1mean_hypo",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        tabBox(
                            width = 6, side = "left",
                            tabPanel(
                                title = "Tính cỡ mẫu",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m0_1mean_hypo", 
                                                  label = "Test value of the population mean", 
                                                  value = 90),
                                        textInput(inputId = "ma_1mean_hypo", 
                                                  label = "Anticipated population mean", 
                                                  value = 85),
                                        textInput(inputId = "sd_1mean_hypo",
                                                  label = "Population standard deviation",
                                                  value = 20)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_1mean_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_1mean_hypo",
                                                  label = "Power",
                                                  value = 0.8)
                                    ),
                                    valueBoxOutput(outputId = "n_1mean_hypo", width = 6)        )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m0_1mean_hypo_power", 
                                                  label = "Test value of the population mean", 
                                                  value = 90),
                                        textInput(inputId = "ma_1mean_hypo_power", 
                                                  label = "Anticipated population mean", 
                                                  value = 85),
                                        textInput(inputId = "sd_1mean_hypo_power",
                                                  label = "Population standard deviation",
                                                  value = 20)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_1mean_hypo_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_1mean_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 100)
                                    ),
                                    valueBoxOutput(outputId = "power_1mean_hypo", width = 6)
                                )
                            )
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("Phần này sẽ dùng để giải thích ý nghĩa các tham số"),
                            p("$$n=\\frac{\\sigma^2(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})^2}{(\\mu_0-\\mu_a)^2}$$")
                        )
                    )
                )
            )
        ),
        
        ##### Estimating the difference between 2 population means #####
        tabItem(
            tabName = "2means_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "sd_2means_est", 
                                          label = "Population standard deviation", 
                                          value = 0.75),
                                textInput(inputId = "d_2means_est", 
                                          label = "Absolute precision required", 
                                          value = 0.2)
                            ),
                            box(
                                textInput(inputId = "alpha_2means_est",
                                          label = "Alpha",
                                          value = 0.05)
                            ),
                            valueBoxOutput(outputId = "n_2means_est", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2(2\\sigma^2)}{d^2}$$"),
                            p("alpha, power")
                        ),
                    )
                )
            )
        ),
        
        ##### Hypothesis testing for 2 population means #####
        tabItem(
            tabName = "2means_hypo",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        tabBox(
                            width = 6, side = "left",
                            tabPanel(
                                title = "Tính cỡ mẫu",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "m1_2means_hypo", 
                                                  label = "Trung bình nhóm 1", 
                                                  value = 100.56),
                                        textInput(inputId = "sd1_2means_hypo",
                                                  label = "Độ lệch chuẩn nhóm 1",
                                                  value = 7.7)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "m2_2means_hypo", 
                                                  label = "Trung bình nhóm 2", 
                                                  value = 94.22),
                                        textInput(inputId = "sd2_2means_hypo",
                                                  label = "Độ lệch chuẩn nhóm 2",
                                                  value = 5.61)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_2means_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_2means_hypo",
                                                  label = "Power",
                                                  value = 0.8),
                                        numericInput(inputId = "k_2means_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    valueBoxOutput(outputId = "n1_2means_hypo", width = 6),
                                    valueBoxOutput(outputId = "n2_2means_hypo", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "m1_2means_hypo_power", 
                                                  label = "Trung bình nhóm 1", 
                                                  value = 0),
                                        textInput(inputId = "sd1_2means_hypo_power",
                                                  label = "Độ lệch chuẩn nhóm 1",
                                                  value = 7.7)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "m2_2means_hypo_power", 
                                                  label = "Trung bình nhóm 2", 
                                                  value = 2),
                                        textInput(inputId = "sd2_2means_hypo_power",
                                                  label = "Độ lệch chuẩn nhóm 2",
                                                  value = 5.61)
                                    ),
                                    box(
                                        textInput(inputId = "n_2means_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 100),
                                        textInput(inputId = "alpha_2means_hypo_power",
                                                  label = "Alpha",
                                                  value = 0.05)
                                    ),
                                    valueBoxOutput(outputId = "power_2means_hypo", width = 6)
                                )
                            )
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{2\\sigma^2(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})^2}{(\\mu_1-\\mu_2)^2}$$"),
                            p(HTML("&sigma;: được tính bằng công thức")),
                            p("$$\\sigma=\\sqrt{\\frac{\\sigma_1^2+\\sigma_2^2}{2}}$$")
                        )
                    )
                )
            )
        ),
        
        ##### Cohort studies #####
        ##### Estimating a RR with specified relative precision #####
        tabItem(
            tabName = "cohort_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "p1_cohort_est", 
                                          label = "Tỷ lệ biến cố nhóm 1", 
                                          value = 0.4),
                                textInput(inputId = "p2_cohort_est", 
                                          label = "Tỷ lệ biến cố nhóm 2", 
                                          value = 0.2),
                                textInput(inputId = "rr_cohort_est", 
                                          label = "Nguy cơ tương đối (RR)", 
                                          value = 2)
                            ),
                            box(
                                textInput(inputId = "alpha_cohort_est",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "eps_cohort_est",
                                          label = "Epsilon",
                                          value = 0.5),
                            ),
                            valueBoxOutput(outputId = "n_cohort_est", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2}{[log_e(1-\\varepsilon)]^2} \\left[ \\frac{1-P_1}{P_1}+\\frac{1-P_2}{P_2} \\right]$$"),
                            p("alpha, power")
                        )
                    )
                )
            )
        ),
        
        ##### Hypothesis test for a RR #####
        tabItem(
            tabName = "cohort_hypo",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        tabBox(
                            width = 6, side = "left",
                            tabPanel(
                                title = "Tính cỡ mẫu",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_cohort_hypo", 
                                                  label = "Tỷ lệ biến cố nhóm 1", 
                                                  value = 0.175),
                                        textInput(inputId = "p2_cohort_hypo", 
                                                  label = "Tỷ lệ biến cố nhóm 2", 
                                                  value = 0.35),
                                        textInput(inputId = "rr0_cohort_hypo", 
                                                  label = "Test value RR", 
                                                  value = 1),
                                        textInput(inputId = "rra_cohort_hypo", 
                                                  label = "Anticipated RR", 
                                                  value = 0.5)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_cohort_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_cohort_hypo",
                                                  label = "Power",
                                                  value = 0.9)
                                    ),
                                    valueBoxOutput(outputId = "n_cohort_hypo", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_cohort_hypo_power", 
                                                  label = "Tỷ lệ biến cố nhóm 1", 
                                                  value = 0.175),
                                        textInput(inputId = "p2_cohort_hypo_power", 
                                                  label = "Tỷ lệ biến cố nhóm 2", 
                                                  value = 0.35),
                                        textInput(inputId = "rr0_cohort_hypo_power", 
                                                  label = "Test value RR", 
                                                  value = 1),
                                        textInput(inputId = "rra_cohort_hypo_power", 
                                                  label = "Anticipated RR", 
                                                  value = 0.5)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_cohort_hypo_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_cohort_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 100)
                                    ),
                                    valueBoxOutput(outputId = "power_cohort_hypo", width = 6)
                                )
                            )
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            p("$$n=\\frac{ \\left\\{ Z_{1-\\frac{\\alpha}{2}}\\sqrt{2\\overline{P}(1-\\overline{P})}+Z_{1-\\beta}\\sqrt{P_1(1-P_1)+P_2(1-P_2)}\\right\\}^2}{(P_1-P_2)^2}$$"),
                            p("alpha, power")
                        )
                    )
                )
            )
        ),
        
        ##### Sample survey #####
        ##### Simple random sampling #####
        tabItem(
            tabName = "simple_random",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "N_simple_random", 
                                          label = "Population size", 
                                          value = 4000),
                                textInput(inputId = "P_simple_random", 
                                          label = "Anticipated population proportion", 
                                          value = 0.6)
                            ),
                            box(
                                textInput(inputId = "alpha_simple_random",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "d_simple_random",
                                          label = "Absolute precision required",
                                          value = 0.05)
                            ),
                            valueBoxOutput(outputId = "n_simple_random", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2P(1-P)N}{d^2(N-1)+Z_{1-\\frac{\\alpha}{2}}^2P(1-P)}$$"),
                            p("alpha, power")
                        )
                    )
                )
            )
        ),
        
        ##### Correlation #####
        tabItem(
            tabName = "corr",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tham số", width = 4,
                            box(
                                textInput(inputId = "r_corr", 
                                          label = "Hệ số tương quan", 
                                          value = 0.3)
                            ),
                            box(
                                textInput(inputId = "alpha_corr",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "power_corr",
                                          label = "Power",
                                          value = 0.8)
                            ),
                        ),
                        box(title = "Hướng dẫn", width = 8,
                            p("Phần này sẽ dùng để giải thích ý nghĩa các tham số"),
                            p("alpha, power")
                        ),
                        valueBoxOutput(outputId = "n_corr")
                    )
                )
            )
        ),
        tabItem(
            tabName = "credit",
            fluidRow(
                box(title = "Credit", width = 6,
                    p("Tác giả")),
                box(title = "Book", width = 6)
            )
        )
    )
)

dashboardPage(title = "SampleSizeCalc: Ước lượng cỡ mẫu",
    dashboardHeader(
        title = span(icon("dna"), "SampleSizeCalc"),
        tags$li(
            a(icon("question-circle"),
              strong("Help"),
              href = "https://github.com/khuongquynhlong/samplesize/blob/master/README.md"),
            class = "dropdown"
        ),
        tags$li(
            a(strong("Source code"),
              href = "https://github.com/khuongquynhlong/samplesize"),
            class = "dropdown"
        )
    ),
    skin = "black",
    sidebar,
    body
)
