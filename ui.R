library(shiny)
library(shinydashboard)
library(DT)
library(pwr)
library(plotly)

# shinydashboard icons were obtained at:
# https://fontawesome.com/icons

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(
            "Giới thiệu", selected = TRUE,
            tabName = "landing_page"
        ),
        menuItem(
            "Biến định tính", 
            icon = icon("percentage"), startExpanded = FALSE,
            menuSubItem("Ước lượng 1 tỷ lệ", tabName = "1prop_est"),
            menuSubItem("So sánh với tỷ lệ quần thể", tabName = "1prop_hypo"),
            menuSubItem("Ước lượng khác biệt 2 tỷ lệ", tabName = "2props_est"),
            menuSubItem("So sánh 2 tỷ lệ", tabName = "2props_hypo"),
            menuSubItem("So sánh 2 tỷ lệ (tỷ lệ nhỏ)", tabName = "2props_hypo_small")
            ),
        menuItem(
            "Biến định lượng", 
            icon = icon("chart-line"), startExpanded = FALSE,
            menuSubItem("Ước lượng 1 trung bình", tabName = "1mean_est"),
            menuSubItem("Kiểm định giả thuyết cho 1 trung bình", tabName = "1mean_hypo"),
            menuSubItem("Ước lượng khác biệt giữa 2 trung bình", tabName = "2means_est"),
            menuSubItem("So sánh 2 trung bình", tabName = "2means_hypo")
            ),
        menuItem(
            "Nghiên cứu thuần tập",
            icon = icon("university"), startExpanded = FALSE,
            menuSubItem("Ước lượng nguy cơ tương đối", tabName = "cohort_est"),
            menuSubItem("Kiểm định giả thuyết cho nguy cơ tương đối", tabName = "cohort_hypo")
        ),
        menuItem(
            "Nghiên cứu bệnh chứng",
            icon = icon("microscope"), startExpanded = FALSE,
            menuSubItem("Ước lượng OR với sai số tương đối", tabName = "case_est"),
            menuSubItem("So sánh 2 OR", tabName = "case_hypo")
        ),
        menuItem(
            "Lấy mẫu ngẫu nhiên đơn",
            icon = icon("poll"), startExpanded = FALSE,
            tabName = "simple_random"
        ),
        menuItem(
            "Hệ số tương quan", 
            icon = icon("chart-area"), startExpanded = FALSE,
            tabName = "corr"
            ),
        menuItem(
            "Test chẩn đoán", 
            icon = icon("vial"), startExpanded = FALSE,
            menuSubItem("Độ nhạy", tabName = "sens"),
            menuSubItem("Độ đặc hiệu", tabName = "spec"),
            menuSubItem("Diện tích dưới đường cong", tabName = "AUC")
        ),
        menuItem(
            "Hồi quy đa biến", 
            icon = icon("registered"), startExpanded = FALSE,
            tabName = "samregress"
        )
    )
)

body <- dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        
        ##### Categorical variables #####
        ##### Estimating 1 population proportion #####
        tabItem(
            tabName = "1prop_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                radioButtons(inputId = "precision_type_1prop_est", 
                                             label = "Chọn loại sai số", 
                                             choices = c("Sai số tuyệt đối" = 1,
                                                         "Sai số tương đối" = 2)),
                                textInput(inputId = "p_1prop_est", 
                                          label = "Tỷ lệ ước lượng", 
                                          value = 0.2),
                                textInput(inputId = "alpha_1prop_est",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "nonrep_1prop_est", 
                                          label = "Tỷ lệ không trả lời", 
                                          value = 0),
                                numericInput(inputId = "deseff_1prop_est", 
                                             label = "Design effect", 
                                             value = 1)
                            ),
                            box(
                                uiOutput(outputId = "precision_1prop_est")
                            ),
                            valueBoxOutput(outputId = "n_1prop_est", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            conditionalPanel(
                                condition = "input.precision_type_1prop_est == 1",
                                withMathJax(),
                                p("Công thức 1: $$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2P(1-P)}{d^2}$$")
                            ),
                            conditionalPanel(
                                condition = "input.precision_type_1prop_est == 2",
                                withMathJax(),
                                p("Công thức 2: $$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2(1-P)}{\\varepsilon^2P}$$")
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Biểu đồ",
                    fluidRow(
                        box(title = "Tham số", width = 4,
                            radioButtons(inputId = "precision_type_1prop_est_plot", 
                                         label = "Chọn loại sai số", 
                                         choices = c("Sai số tuyệt đối" = 1,
                                                     "Sai số tương đối" = 2)),
                            textInput(inputId = "alpha_1prop_est_plot",
                                      label = "Alpha",
                                      value = 0.05),
                            sliderInput(inputId = "p_range_1prop_est_plot",
                                        label = "Chọn khoảng tỷ lệ muốn vẽ",
                                        min = 0, max = 1, value = c(0.1, 0.99)),
                            textInput(inputId = "p_by_1prop_est_plot",
                                      label = "Khoảng cách tỷ lệ",
                                      value = 0.01),
                            uiOutput(outputId = "precision_1prop_est_plot"),
                            helpText("Có thể nhập nhiều sai số, cách nhau bằng dấu khoảng trắng (space)")
                        ),
                        box(title = "Biểu đồ", width = 8,
                            plotlyOutput(outputId = "plot_1prop_est")
                        )
                    )
                )
            )
        ),
        
        ##### Hypothesis test for 1 population proportion #####
        tabItem(
            tabName = "1prop_hypo",
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
                                        textInput(inputId = "p0_1prop_hypo", 
                                                  label = "Tỷ lệ quần thể", 
                                                  value = 0.30),
                                        textInput(inputId = "pa_1prop_hypo",
                                                  label = "Tỷ lệ ước tính",
                                                  value = 0.20),
                                        textInput(inputId = "nonrep_1prop_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_1prop_hypo",
                                                     label = "Design effect",
                                                     value = 1)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_1prop_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_1prop_hypo",
                                                  label = "Power",
                                                  value = 0.8)
                                    ),
                                    valueBoxOutput(outputId = "n_1prop_hypo", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p0_1prop_hypo_power", 
                                                  label = "Tỷ lệ quần thể", 
                                                  value = 0.30),
                                        textInput(inputId = "pa_1prop_hypo_power",
                                                  label = "Tỷ lệ ước tính",
                                                  value = 0.20)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_1prop_hypo_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_1prop_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 100)
                                    ),
                                    valueBoxOutput(outputId = "power_1prop_hypo", width = 6)
                                )
                            )
                        ),
                        
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{\\left\\{Z_{1-\\frac{\\alpha}{2}}\\sqrt{P_o(1-P_o)} + Z_{1-\\beta}\\sqrt{P_a(1-P_a)}\\right\\}^2}{(P_a - P_o)^2}$$")
                        )
                    )
                )
            )
        ),
        
        ##### Estimate difference between 2 proportions #####
        tabItem(
            tabName = "2props_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "p1_2props_est", 
                                          label = "Tỷ lệ nhóm 1", 
                                          value = 0.2),
                                textInput(inputId = "p2_2props_est", 
                                          label = "Tỷ lệ nhóm 2", 
                                          value = 0.5),
                                textInput(inputId = "nonrep_2props_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_2props_est",
                                             label = "Design effect",
                                             value = 1)
                            ),
                            box(
                                textInput(inputId = "alpha_2props_est",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "d_2props_est",
                                          label = "Sai số tuyệt đối",
                                          value = 0.05)
                            ),
                            valueBoxOutput(outputId = "n_2props_est", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2 \\left[P_1(1-P_1)+P_2(1-P_2)\\right]}{d^2}$$")
                        ),
                    )
                )
            )
        ),
        
        ##### Hypothesis test for two proportion #####
        tabItem(
            tabName = "2props_hypo",
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
                                        textInput(inputId = "p1_2props_hypo", 
                                                  label = "Tỷ lệ nhóm 1", 
                                                  value = 0.60),
                                        textInput(inputId = "p2_2props_hypo",
                                                  label = "Tỷ lệ nhóm 2",
                                                  value = 0.50),
                                        numericInput(inputId = "k_2props_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                        textInput(inputId = "nonrep_2props_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2props_hypo",
                                                     label = "Design effect",
                                                     value = 1)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_2props_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_2props_hypo",
                                                  label = "Power",
                                                  value = 0.8)
                                    ),
                                    valueBoxOutput(outputId = "n1_2props_hypo", width = 6),
                                    valueBoxOutput(outputId = "n2_2props_hypo", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p1_2props_hypo_power", 
                                                  label = "Tỷ lệ nhóm 1", 
                                                  value = 0.60),
                                        textInput(inputId = "p2_2props_hypo_power",
                                                  label = "Tỷ lệ nhóm 2",
                                                  value = 0.50)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_2props_hypo_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_2props_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 500)
                                    ),
                                    valueBoxOutput(outputId = "power_2props_hypo", width = 6)
                                )
                            )
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{ \\left\\{ Z_{1-\\frac{\\alpha}{2}}\\sqrt{2\\overline{P}(1-\\overline{P})}+Z_{1-\\beta}\\sqrt{P_1(1-P_1)+P_2(1-P_2)}\\right\\}^2}{(P_1-P_2)^2}$$"),
                            helpText("Công thức này giống với công thức kiểm định giả thuyết cho nguy cơ tương đối (nghiên cứu thuần tập)")
                        )
                    )
                ),
                tabPanel(
                    title = "Biểu đồ",
                    fluidRow(
                        box(title = "Tham số", width = 4,
                            radioButtons(inputId = "plot_type_2props_hypo_plot", 
                                         label = "Chọn loại biểu đồ", 
                                         choices = c("Biểu đồ cỡ mẫu theo sự khác biệt"= 1,
                                                     "Biểu đồ power theo cỡ mẫu" = 2)),
                            conditionalPanel(
                                condition = "input.plot_type_2props_hypo_plot == 1",
                                textInput(inputId = "p1_2props_hypo_plot1",
                                          label = "p1",
                                          value = 0.2),
                                uiOutput(outputId = "diff_range_2props_hypo_plot1"),
                                textInput(inputId = "diff_by_2props_hypo_plot1",
                                          label = "Khoảng cách khác biệt",
                                          value = 0.05),
                                textInput(inputId = "power_2props_hypo_plot1",
                                          label = "Power",
                                          value = "0.7 0.8 0.9"),
                                helpText("Có thể nhập nhiều power, cách nhau bằng dấu khoảng trắng (space)"),
                                textInput(inputId = "alpha_2props_hypo_plot1",
                                          label = "Alpha",
                                          value = 0.05)
                            ),
                            conditionalPanel(
                                condition = "input.plot_type_2props_hypo_plot == 2",
                                textInput(inputId = "p1_2props_hypo_plot2",
                                          label = "p1",
                                          value = 0.5),
                                textInput(inputId = "p2_2props_hypo_plot2",
                                          label = "p2",
                                          value = 0.3),
                                textInput(inputId = "alpha_2props_hypo_plot2",
                                          label = "Alpha",
                                          value = 0.05),
                                sliderInput(inputId = "power_range_2props_hypo_plot2",
                                            label = "Chọn khoảng power muốn vẽ",
                                            min = 0, max = 1, value = c(0.5, 0.95)),
                                textInput(inputId = "power_by_2props_hypo_plot2",
                                          label = "Khoảng cách power",
                                          value = 0.05)
                            ),
                        ),
                        box(title = "Biểu đồ", width = 8,
                            conditionalPanel(
                                condition = "input.plot_type_2props_hypo_plot == 1",
                                plotlyOutput(outputId = "plot1_2props_hypo")
                            ),
                            conditionalPanel(
                                condition = "input.plot_type_2props_hypo_plot == 2",
                                plotlyOutput(outputId = "plot2_2props_hypo")
                            )
                        )
                    )
                )
            )
        ),
        
        ##### Hypothesis test for two proportion (small proportion) #####
        tabItem(
            tabName = "2props_hypo_small",
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
                                        textInput(inputId = "p1_2props_hypo_small", 
                                                  label = "Tỷ lệ nhóm 1", 
                                                  value = 0.001),
                                        textInput(inputId = "p2_2props_hypo_small",
                                                  label = "Tỷ lệ nhóm 2",
                                                  value = 0.010),
                                        numericInput(inputId = "k_2props_hypo_small",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                        textInput(inputId = "nonrep_2props_hypo_small",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2props_hypo_small",
                                                     label = "Design effect",
                                                     value = 1)
                                    ),
                                    box(    
                                        textInput(inputId = "alpha_2props_hypo_small",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_2props_hypo_small",
                                                  label = "Power",
                                                  value = 0.8)
                                    ),
                                    valueBoxOutput(outputId = "n1_2props_hypo_small", width = 6),
                                    valueBoxOutput(outputId = "n2_2props_hypo_small", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        status = "success",
                                        textInput(inputId = "p1_2props_hypo_small_power", 
                                                  label = "Tỷ lệ nhóm 1", 
                                                  value = 0.001),
                                        textInput(inputId = "p2_2props_hypo_small_power",
                                                  label = "Tỷ lệ nhóm 2",
                                                  value = 0.010)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "alpha_2props_hypo_small_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_2props_hypo_small_power",
                                                  label = "Cỡ mẫu",
                                                  value = 1000)
                                    ),
                                    valueBoxOutput(outputId = "power_2props_hypo_small", width = 6)
                                )
                            )
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})^2}{2(\\arcsin\\sqrt{P_2}-\\arcsin\\sqrt{P_1})^2}$$")
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
                                             choices = c("Sai số tuyệt đối" = 1,
                                                         "Sai số tương đối" = 2)),
                                textInput(inputId = "sd_1mean_est", 
                                          label = "Độ lệch chuẩn", 
                                          value = 2),
                                textInput(inputId = "alpha_1mean_est",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "nonrep_1mean_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_1mean_est",
                                             label = "Design effect",
                                             value = 1)
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
                                                  label = "Trung bình quần thể", 
                                                  value = 90),
                                        textInput(inputId = "ma_1mean_hypo", 
                                                  label = "Trung bình nhóm so sánh", 
                                                  value = 85),
                                        textInput(inputId = "sd_1mean_hypo",
                                                  label = "Độ lệch chuẩn quần thể",
                                                  value = 20),
                                        textInput(inputId = "nonrep_1mean_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_1mean_hypo",
                                                     label = "Design effect",
                                                     value = 1)
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
                                                  label = "Trung bình quần thể", 
                                                  value = 90),
                                        textInput(inputId = "ma_1mean_hypo_power", 
                                                  label = "Trung bình nhóm so sánh", 
                                                  value = 85),
                                        textInput(inputId = "sd_1mean_hypo_power",
                                                  label = "Độ lệch chuẩn quần thể",
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
                                          value = 0.2),
                                textInput(inputId = "nonrep_2means_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_2means_est",
                                             label = "Design effect",
                                             value = 1)
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
                                                  value = 100),
                                        textInput(inputId = "sd1_2means_hypo",
                                                  label = "Độ lệch chuẩn nhóm 1",
                                                  value = 20),
                                        textInput(inputId = "m2_2means_hypo", 
                                                  label = "Trung bình nhóm 2", 
                                                  value = 95),
                                        textInput(inputId = "sd2_2means_hypo",
                                                  label = "Độ lệch chuẩn nhóm 2",
                                                  value = 15),
                                        textInput(inputId = "nonrep_2means_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2means_hypo",
                                                     label = "Design effect",
                                                     value = 1)
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
                                                  value = 100),
                                        textInput(inputId = "sd1_2means_hypo_power",
                                                  label = "Độ lệch chuẩn nhóm 1",
                                                  value = 20)
                                    ),
                                    box(
                                        status = "warning",
                                        textInput(inputId = "m2_2means_hypo_power", 
                                                  label = "Trung bình nhóm 2", 
                                                  value = 95),
                                        textInput(inputId = "sd2_2means_hypo_power",
                                                  label = "Độ lệch chuẩn nhóm 2",
                                                  value = 15)
                                    ),
                                    box(
                                        textInput(inputId = "n_2means_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 250),
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
                                          value = 2),
                                textInput(inputId = "nonrep_cohort_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_cohort_est",
                                             label = "Design effect",
                                             value = 1)
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
                                                  value = 0.5),
                                        textInput(inputId = "nonrep_cohort_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_cohort_hypo",
                                                     label = "Design effect",
                                                     value = 1)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_cohort_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_cohort_hypo",
                                                  label = "Power",
                                                  value = 0.8)
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
        

        ##### Case control studies #####
        ##### Estimating a OR with specified relative precision #####
        tabItem(
            tabName = "case_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "p1_case_est", 
                                          label = "Tỷ lệ phơi nhiễm trong nhóm bệnh", 
                                          value = 0.4),
                                textInput(inputId = "p2_case_est", 
                                          label = "Tỷ lệ phơi nhiễm trong nhóm chứng", 
                                          value = 0.2),
                                textInput(inputId = "or_case_est", 
                                          label = "Tỷ số số chênh (OR)", 
                                          value = 2),
                                textInput(inputId = "nonrep_case_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_case_est",
                                             label = "Design effect",
                                             value = 1)
                            ),
                            box(
                                textInput(inputId = "alpha_case_est",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "eps_case_est",
                                          label = HTML("Sai số tương đối (&epsilon;)"),
                                          value = 0.5),
                            ),
                            valueBoxOutput(outputId = "n_case_est", width = 6)
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2}{[log_e(1-\\varepsilon)]^2} \\left[ \\frac{1}{P_1(1-P_1)}+\\frac{1}{P_2(1-P_2)} \\right]$$"),
                            p("alpha, power")
                        )
                    )
                )
            )
        ),
        
        ##### Hypothesis test for a OR #####
        tabItem(
            tabName = "case_hypo",
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
                                        textInput(inputId = "p1_case_hypo", 
                                                  label = "Tỷ lệ phơi nhiễm trong nhóm bệnh", 
                                                  value = 0.175),
                                        textInput(inputId = "p2_case_hypo", 
                                                  label = "Tỷ lệ phơi nhiễm trong nhóm chứng", 
                                                  value = 0.35),
                                        textInput(inputId = "oro_case_hypo", 
                                                  label = "Test value OR", 
                                                  value = 1),
                                        textInput(inputId = "ora_case_hypo", 
                                                  label = "Anticipated OR", 
                                                  value = 0.5),
                                        textInput(inputId = "nonrep_case_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_case_hypo",
                                                     label = "Design effect",
                                                     value = 1)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_case_hypo",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "power_case_hypo",
                                                  label = "Power",
                                                  value = 0.8)
                                    ),
                                    valueBoxOutput(outputId = "n_case_hypo", width = 6)
                                )
                            ),
                            tabPanel(
                                title = "Tính power",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_case_hypo_power", 
                                                  label = "Tỷ lệ phơi nhiễm trong nhóm bệnh", 
                                                  value = 0.175),
                                        textInput(inputId = "p2_case_hypo_power", 
                                                  label = "Tỷ lệ phơi nhiễm trong nhóm chứng", 
                                                  value = 0.35),
                                        textInput(inputId = "oro_case_hypo_power", 
                                                  label = "Test value OR", 
                                                  value = 1),
                                        textInput(inputId = "ora_case_hypo_power", 
                                                  label = "Anticipated OR", 
                                                  value = 0.5)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_case_hypo_power",
                                                  label = "Alpha",
                                                  value = 0.05),
                                        textInput(inputId = "n_case_hypo_power",
                                                  label = "Cỡ mẫu",
                                                  value = 100)
                                    ),
                                    valueBoxOutput(outputId = "power_case_hypo", width = 6)
                                )
                            )
                        ),
                        box(title = "Hướng dẫn", width = 6,
                            p("$$n=\\frac{ \\left\\{ Z_{1-\\frac{\\alpha}{2}}\\sqrt{2P_2(1-P_2)}+Z_{1-\\beta}\\sqrt{P_1(1-P_1)+P_2(1-P_2)}\\right\\}^2}{(P_1-P_2)^2}$$"),
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
                                          value = 0.6),
                                textInput(inputId = "nonrep_simple_random",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_simple_random",
                                             label = "Design effect",
                                             value = 1)
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
        
        
        ##### Sensitivity, specificity, AUC #####
        ### Sensitivity
        
        tabItem(
            tabName = "sens",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(width = 6, side = "left", title = "Đang cập nhật"),
                        box(title = "Đang cập nhật", width = 6)
                    )
                )
            )
        ), 
        
        ### Specificity
        
        tabItem(
            tabName = "spec",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(width = 6, side = "left", title = "Đang cập nhật"),
                        box(title = "Đang cập nhật", width = 6)
                    )
                )
            )
        ), 
        
        ### AUC
        
        tabItem(
            tabName = "AUC",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(width = 6, side = "left", title = "Đang cập nhật"),
                        box(title = "Đang cập nhật", width = 6)
                    )
                )
            )
        ), 
        
        
        ##### Regression #####
        
        tabItem(
            tabName = "samregress",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(width = 6, side = "left", title = "Đang cập nhật"),
                        box(title = "Đang cập nhật", width = 6)
                    )
                )
            )
        ), 
        
        
        
        
        tabItem(
            tabName = "landing_page",
            fluidRow(
                box(title = "Giới thiệu", width = 12,
                    p("Tác giả")
                    )
                )
            )
        )
    )

dashboardPage(title = "Phần mềm tính cỡ mẫu hàng đầu Việt Nam",
    dashboardHeader(
        title = span(icon("dna"), "Ước lượng cỡ mẫu"),
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
