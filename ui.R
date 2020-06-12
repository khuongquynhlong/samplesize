library(shiny)
library(shinydashboard)
library(DT)
library(pwr)
library(plotly)

# shinydashboard icons were obtained at:
# https://fontawesome.com/icons
source(file = "functions/myBox.R", local = TRUE)

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
        # menuItem(
        #     "Lấy mẫu ngẫu nhiên đơn",
        #     icon = icon("poll"), startExpanded = FALSE,
        #     tabName = "simple_random"
        # ),
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
                                textInput(inputId = "alpha_1prop_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "p_1prop_est", 
                                          label = "Tỷ lệ ước lượng (P)", 
                                          value = 0.2),
                                radioButtons(inputId = "precision_type_1prop_est", 
                                             label = "Chọn loại sai số", 
                                             choices = c("Sai số tuyệt đối" = 1,
                                                         "Sai số tương đối" = 2)),
                                uiOutput(outputId = "precision_1prop_est")
                            ),
                            box(
                                textInput(inputId = "nonrep_1prop_est", 
                                          label = "Tỷ lệ không trả lời", 
                                          value = 0),
                                numericInput(inputId = "deseff_1prop_est", 
                                             label = "Hệ số thiết kế", 
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_1prop_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
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
                        box(width = 4,
                            radioButtons(inputId = "precision_type_1prop_est_plot", 
                                         label = "Chọn loại sai số", 
                                         choices = c("Sai số tuyệt đối" = 1,
                                                     "Sai số tương đối" = 2)),
                            textInput(inputId = "alpha_1prop_est_plot",
                                      label = HTML("Alpha (&alpha;)"),
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
                        tabBox(
                            width = 8, side = "left",
                            tabPanel(
                                title = "Biểu đồ",
                                fluidPage(
                                    plotlyOutput(outputId = "plot_1prop_est")
                                )
                            ),
                            tabPanel(
                                title = "Bảng",
                                fluidPage(
                                    dataTableOutput(outputId = "table_1prop_est")
                                )
                            )
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
                                        textInput(inputId = "alpha_1prop_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "p0_1prop_hypo", 
                                                  label = HTML("Tỷ lệ quần thể (P<sub>0</sub>)"), 
                                                  value = 0.30),
                                        textInput(inputId = "power_1prop_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "pa_1prop_hypo",
                                                  label = HTML("Tỷ lệ ước tính (P<sub>a</sub>)"),
                                                  value = 0.20)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_1prop_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_1prop_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n_1prop_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p0_1prop_hypo_power", 
                                                  label = HTML("Tỷ lệ quần thể (P<sub>0</sub>)"), 
                                                  value = 0.30),
                                        textInput(inputId = "pa_1prop_hypo_power",
                                                  label = HTML("Tỷ lệ ước tính (P<sub>a</sub>)"),
                                                  value = 0.20)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_1prop_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_1prop_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 100)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_1prop_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{\\left\\{Z_{1-\\frac{\\alpha}{2}}\\sqrt{P_0(1-P_0)} + Z_{1-\\beta}\\sqrt{P_a(1-P_a)}\\right\\}^2}{(P_a - P_0)^2}$$")
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
                                textInput(inputId = "alpha_2props_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "p1_2props_est", 
                                          label = HTML("Tỷ lệ nhóm 1 (P<sub>1</sub>)"), 
                                          value = 0.2),
                                textInput(inputId = "p2_2props_est", 
                                          label = HTML("Tỷ lệ nhóm 2 (P<sub>2</sub>)"), 
                                          value = 0.5),
                                textInput(inputId = "d_2props_est",
                                          label = "Sai số tuyệt đối (d)",
                                          value = 0.05)
                            ),
                            box(
                                textInput(inputId = "nonrep_2props_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_2props_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_2props_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
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
                                        textInput(inputId = "alpha_2props_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_2props_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_2props_hypo", 
                                                  label = HTML("Tỷ lệ nhóm 1 (P<sub>1</sub>)"), 
                                                  value = 0.60),
                                        textInput(inputId = "p2_2props_hypo",
                                                  label = HTML("Tỷ lệ nhóm 2 (P<sub>2</sub>)"),
                                                  value = 0.50),
                                        numericInput(inputId = "k_2props_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_2props_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2props_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 1</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_2props_hypo"), "</b>")), align = "center"))
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 2</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_2props_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        
                                        textInput(inputId = "p1_2props_hypo_power", 
                                                  label = HTML("Tỷ lệ nhóm 1 (P<sub>1</sub>)"), 
                                                  value = 0.60),
                                        textInput(inputId = "p2_2props_hypo_power",
                                                  label = HTML("Tỷ lệ nhóm 2 (P<sub>2</sub>)"),
                                                  value = 0.50)
                                    ),
                                    box(
                                        
                                        textInput(inputId = "alpha_2props_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_2props_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 500)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_2props_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
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
                                          label = HTML("Lực thống kê (1-&beta;)"),
                                          value = "0.7 0.8 0.9"),
                                helpText("Có thể nhập nhiều power, cách nhau bằng dấu khoảng trắng (space)"),
                                textInput(inputId = "alpha_2props_hypo_plot1",
                                          label = HTML("Alpha (&alpha;)"),
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
                                          label = HTML("Alpha (&alpha;)"),
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
                                        textInput(inputId = "alpha_2props_hypo_small",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_2props_hypo_small",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_2props_hypo_small", 
                                                  label = HTML("Tỷ lệ nhóm 1 (P<sub>1</sub>)"), 
                                                  value = 0.001),
                                        textInput(inputId = "p2_2props_hypo_small",
                                                  label = HTML("Tỷ lệ nhóm 2 (P<sub>2</sub>)"),
                                                  value = 0.010),
                                        numericInput(inputId = "k_2props_hypo_small",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_2props_hypo_small",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2props_hypo_small",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_2props_hypo_small"), "</b>")), align = "center"))
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_2props_hypo_small"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        
                                        textInput(inputId = "p1_2props_hypo_small_power", 
                                                  label = HTML("Tỷ lệ nhóm 1 (P<sub>1</sub>)"), 
                                                  value = 0.001),
                                        textInput(inputId = "p2_2props_hypo_small_power",
                                                  label = HTML("Tỷ lệ nhóm 2 (P<sub>2</sub>)"),
                                                  value = 0.010)
                                    ),
                                    box(
                                        
                                        textInput(inputId = "alpha_2props_hypo_small_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_2props_hypo_small_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 1000)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_2props_hypo_small"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
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
                                textInput(inputId = "alpha_1mean_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "sd_1mean_est", 
                                          label = HTML("Độ lệch chuẩn (&sigma;)"), 
                                          value = 2),
                                radioButtons(inputId = "precision_type_1mean_est", 
                                             label = "Chọn loại sai số", 
                                             choices = c("Sai số tuyệt đối" = 1,
                                                         "Sai số tương đối" = 2)),
                                uiOutput(outputId = "precision_1mean_est")
                            ),
                            box(
                                textInput(inputId = "nonrep_1mean_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_1mean_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_1mean_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
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
                                        textInput(inputId = "sd_1mean_hypo",
                                                  label = HTML("Độ lệch chuẩn quần thể (&sigma;)"),
                                                  value = 20),
                                        textInput(inputId = "alpha_1mean_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_1mean_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m0_1mean_hypo", 
                                                  label = HTML("Trung bình quần thể (&mu;<sub>0</sub>)"), 
                                                  value = 90),
                                        textInput(inputId = "ma_1mean_hypo", 
                                                  label = HTML("Trung bình nhóm so sánh (&mu;<sub>a</sub>)"), 
                                                  value = 85)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_1mean_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_1mean_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n_1mean_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m0_1mean_hypo_power", 
                                                  label = HTML("Trung bình quần thể (&mu;<sub>0</sub>)"), 
                                                  value = 90),
                                        textInput(inputId = "ma_1mean_hypo_power", 
                                                  label = HTML("Trung bình nhóm so sánh (&mu;<sub>a</sub>)"), 
                                                  value = 85),
                                        textInput(inputId = "sd_1mean_hypo_power",
                                                  label = HTML("Độ lệch chuẩn quần thể (&sigma;)"),
                                                  value = 20)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_1mean_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_1mean_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 100)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_1mean_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
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
                                textInput(inputId = "alpha_2means_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "sd_2means_est", 
                                          label = HTML("Độ lệch chuẩn quần thể (&sigma;)"), 
                                          value = 0.75),
                                textInput(inputId = "d_2means_est", 
                                          label = "Sai số tuyệt đối (d)", 
                                          value = 0.2)
                            ),
                            box(
                                textInput(inputId = "nonrep_2means_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_2means_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_2means_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2(2\\sigma^2)}{d^2}$$")
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
                                        textInput(inputId = "sd1_2means_hypo",
                                                  label = HTML("Độ lệch chuẩn nhóm 1 (&sigma;<sub>1</sub>)"),
                                                  value = 20),
                                        textInput(inputId = "sd2_2means_hypo",
                                                  label = HTML("Độ lệch chuẩn nhóm 2 (&sigma;<sub>2</sub>)"),
                                                  value = 15),
                                        textInput(inputId = "alpha_2means_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_2means_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m1_2means_hypo", 
                                                  label = HTML("Trung bình nhóm 1 (&mu;<sub>1</sub>)"), 
                                                  value = 100),
                                        textInput(inputId = "m2_2means_hypo", 
                                                  label = HTML("Trung bình nhóm 2 (&mu;<sub>2</sub>)"), 
                                                  value = 95),
                                        numericInput(inputId = "k_2means_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_2means_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2means_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 1</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_2means_hypo"), "</b>")), align = "center"))
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 2</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_2means_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        
                                        textInput(inputId = "m1_2means_hypo_power", 
                                                  label = HTML("Trung bình nhóm 1 (&mu;<sub>1</sub>)"), 
                                                  value = 100),
                                        textInput(inputId = "sd1_2means_hypo_power",
                                                  label = HTML("Độ lệch chuẩn nhóm 1 (&sigma;<sub>1</sub>)"),
                                                  value = 20)
                                    ),
                                    box(
                                        
                                        textInput(inputId = "m2_2means_hypo_power", 
                                                  label = HTML("Trung bình nhóm 2 (&mu;<sub>2</sub>)"), 
                                                  value = 95),
                                        textInput(inputId = "sd2_2means_hypo_power",
                                                  label = HTML("Độ lệch chuẩn nhóm 2 (&sigma;<sub>2</sub>)"),
                                                  value = 15)
                                    ),
                                    box(
                                        textInput(inputId = "n_2means_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 250),
                                        textInput(inputId = "alpha_2means_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_2means_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
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
                                textInput(inputId = "alpha_cohort_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "eps_cohort_est",
                                          label = HTML("Epsilon (&epsilon;)"),
                                          value = 0.5),
                                textInput(inputId = "p1_cohort_est", 
                                          label = HTML("Tỷ lệ phơi nhiễm trong nhóm bệnh (P<sub>1</sub>)"), 
                                          value = 0.4),
                                textInput(inputId = "p2_cohort_est", 
                                          label = HTML("Tỷ lệ phơi nhiễm trong nhóm chứng (P<sub>2</sub>)"), 
                                          value = 0.2)
                            ),
                            box(
                                textInput(inputId = "nonrep_cohort_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_cohort_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_cohort_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2}{[log_e(1-\\varepsilon)]^2} \\left[ \\frac{1-P_1}{P_1}+\\frac{1-P_2}{P_2} \\right]$$")
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
                                        textInput(inputId = "alpha_cohort_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_cohort_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_cohort_hypo", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm bệnh (P<sub>1</sub>)"), 
                                                  value = 0.175),
                                        textInput(inputId = "p2_cohort_hypo", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm chứng (P<sub>2</sub>)"), 
                                                  value = 0.35)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_cohort_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_cohort_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n_cohort_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_cohort_hypo_power", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm bệnh (P<sub>1</sub>)"), 
                                                  value = 0.175),
                                        textInput(inputId = "p2_cohort_hypo_power", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm chứng (P<sub>2</sub>)"), 
                                                  value = 0.35)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_cohort_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_cohort_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 100)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_cohort_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{ \\left\\{ Z_{1-\\frac{\\alpha}{2}}\\sqrt{2\\overline{P}(1-\\overline{P})}+Z_{1-\\beta}\\sqrt{P_1(1-P_1)+P_2(1-P_2)}\\right\\}^2}{(P_1-P_2)^2}$$")
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
                                textInput(inputId = "alpha_case_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "eps_case_est",
                                          label = HTML("Sai số tương đối (&epsilon;)"),
                                          value = 0.5),
                                textInput(inputId = "p1_case_est", 
                                          label = HTML("Tỷ lệ phơi nhiễm trong nhóm bệnh (P<sub>1</sub>)"), 
                                          value = 0.4),
                                textInput(inputId = "p2_case_est", 
                                          label = HTML("Tỷ lệ phơi nhiễm trong nhóm chứng (P<sub>2</sub>)"), 
                                          value = 0.2)
                            ),
                            box(
                                textInput(inputId = "nonrep_case_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_case_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_case_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2}{[log_e(1-\\varepsilon)]^2} \\left[ \\frac{1}{P_1(1-P_1)}+\\frac{1}{P_2(1-P_2)} \\right]$$")
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
                                        textInput(inputId = "alpha_case_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "p2_case_hypo", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm chứng (P<sub>2</sub>)"), 
                                                  value = 0.35),
                                        textInput(inputId = "power_case_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_case_hypo", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm bệnh (P<sub>1</sub>)"), 
                                                  value = 0.175)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_case_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_case_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n_case_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_case_hypo_power", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm bệnh (P<sub>1</sub>)"), 
                                                  value = 0.175),
                                        textInput(inputId = "p2_case_hypo_power", 
                                                  label = HTML("Tỷ lệ phơi nhiễm trong nhóm chứng (P<sub>2</sub>)"), 
                                                  value = 0.35),
                                    ),
                                    box(
                                        textInput(inputId = "alpha_case_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_case_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 100)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_case_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{ \\left\\{ Z_{1-\\frac{\\alpha}{2}}\\sqrt{2P_2(1-P_2)}+Z_{1-\\beta}\\sqrt{P_1(1-P_1)+P_2(1-P_2)}\\right\\}^2}{(P_1-P_2)^2}$$")
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
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                textInput(inputId = "alpha_simple_random",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "d_simple_random",
                                          label = "Absolute precision required",
                                          value = 0.05)
                            ),
                            valueBoxOutput(outputId = "n_simple_random", width = 6)
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2P(1-P)N}{d^2(N-1)+Z_{1-\\frac{\\alpha}{2}}^2P(1-P)}$$")
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
                        box(title = "Tham số", width = 6,
                            box(
                                textInput(inputId = "r_corr", 
                                          label = "Hệ số tương quan (r)", 
                                          value = 0.3),
                                textInput(inputId = "alpha_corr",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "power_corr",
                                          label = HTML("Lực thống kê (1-&beta;)"),
                                          value = 0.8)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_corr"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("Đang cập nhật")
                        )
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
                myBox(width = 12, height = 900, background = "#222d32",color = "white",
                    #p(HTML('<left><img src="Logo1.png" style="width: 10vw; min-width: 250x;"></left>')),
                    #tags$br(),
                    p(HTML("<left> <b> <p style='font-size:35px;color:white;'> PHẦN MỀM TÍNH TOÁN CỠ MẪU LTM 1.0 </sup> </p> </b> </left>")),
                    p(HTML("<left> <b> <p style='font-size:30px;color:white;'> Dành cho các nghiên cứu khoa học sức khỏe </p> </b> </left>")),
                    # p(HTML("<left> <i> <p style='font-size:15px;color:darkblue;'>  (Phiên bản v1.0-2020) </p> </i> </left>")),
                    p(HTML("<left> <b> <p style='font-size:17px;color:white;'> </b> <i>  Tác giả: Khương Quỳnh Long, Ong Phúc Thịnh, Hoàng Văn Minh </i> </p>  </left>")),
                    tags$br(),
                    # p(HTML('<left><img src="Sampling.gif" style="width: 20vw; min-width: 250x;"></left>')),
                    p(HTML('<left><img src="Sample1.jpg" style="width: 30vw; min-width: 250x;"></left>')),
                    tags$br(),
                    p(HTML("<i> <p style='font-size:12px;color:white;text-align:left'> (Phần mềm đang trong quá trình xây dựng và thử nghiệm, mọi góp ý xin vui lòng liên hệ nhóm tác giả; <u> Email: kql@huph.edu.vn </u>) </p> </i>")),
                    # p(HTML("<i> <p style='font-size:10px;color:darkblue;text-align:right'> @Phần mềm được viết bằng ngôn ngữ R với ứng dụng Shiny    </p> </i>"))
                    )
                )
            )
        )
    )

dashboardPage(title = "Phần mềm tính toán cỡ mẫu LTM 1.0",
    dashboardHeader(
        title = span(icon("dna"), HTML("Cỡ mẫu LTM 1.0")),
        tags$li(
            a(icon("question-circle"),
              strong("Help"),
              href = ""),
            class = "dropdown"
        ),
        tags$li(
            a(strong("Source code"),
              href = ""),
            class = "dropdown"
        )
    ),
    skin = "black",
    sidebar,
    body
)
