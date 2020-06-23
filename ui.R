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
            "Nghiên cứu dựa trên ước lượng khoảng", 
            icon = icon("percentage"), startExpanded = FALSE,
            menuSubItem("Nghiên cứu gồm 1 mẫu, xác định 1 trung bình", tabName = "1mean_est"),
            menuSubItem("Nghiên cứu gồm 1 mẫu, xác định 1 tỷ lệ", tabName = "1prop_est"),
            menuSubItem("Nghiên cứu 2 mẫu độc lập, xác định sự khác biệt 2 trung bình", tabName = "2means_ind_est"),
            menuSubItem("Nghiên cứu 2 mẫu ghép cặp, xác định sự khác biệt 2 trung bình", tabName = "2props_pair_est"),
            menuSubItem("Nghiên cứu 2 mẫu độc lập, xác định sự khác biệt 2 tỷ lệ", tabName = "2props_est")
            ),
        menuItem(
            "Nghiên cứu dựa trên kiểm định giả thuyết", 
            icon = icon("chart-line"), startExpanded = FALSE,
            menuSubItem("Nghiên cứu gồm 1 mẫu, kiểm định 1 trung bình", tabName = "1mean_hypo"),
            menuSubItem("Nghiên cứu gồm 1 mẫu, kiểm định 1 tỷ lệ", tabName = "1prop_hypo"),
            menuSubItem("Nghiên cứu gồm 2 mẫu độc lập, kiểm định 2 trung bình", tabName = "2means_ind_hypo"),
            menuSubItem("Nghiên cứu gồm 2 mẫu ghép cặp, kiểm định 2 trung bình", tabName = "2means_pair_hypo"),
            menuSubItem("Nghiên cứu gồm 2 mẫu độc lập, kiểm định 2 tỷ lệ", tabName = "2props_ind_hypo"),
            menuSubItem("Nghiên cứu gồm 2 mẫu ghép cặp, kiểm định 2 tỷ lệ (McNemar)", tabName = "2props_pair_hypo"),
            menuSubItem("Nghiên cứu bệnh chứng, kiểm định OR", tabName = "case_hypo"),
            menuSubItem("Nghiên cứu thuần tập, kiểm định RR", tabName = "cohort_hypo"),
            menuSubItem("Nghiên cứu sống còn", tabName = "survive")
        ),
        menuItem(
            "Nghiên cứu nghiệm pháp chẩn đoán",
            icon = icon("chart-line"), startExpanded = FALSE,
            menuSubItem("Độ nhạy", tabName = "sens"),
            menuSubItem("Độ đặc hiệu", tabName = "spec")
        ),
        menuItem(
            "Nghiên cứu tương đương",
            icon = icon("percentage"), startExpanded = FALSE,
            menuSubItem("Nghiên cứu tương đương với biến định lượng", tabName = "equi_cont"),
            menuSubItem("Nghiên cứu tương đương với biến định tính", tabName = "equi_cat")
        ),
        menuItem(
            "Nghiên cứu không kém hơn",
            icon = icon("percentage"), startExpanded = FALSE,
            menuSubItem("Nghiên cứu không kém hơn với biến định lượng", tabName = "noninfer_cont"),
            menuSubItem("Nghiên cứu không kém hơn với biến định tính", tabName = "noninfer_cat")
        ),
        menuItem(
            "Nghiên cứu thử nghiệm lâm sàng theo cụm", startExpanded = FALSE,
            tabName = "cluster_randomize"
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
        
        ## Nghiên cứu 2 mẫu độc lập, xác định sự khác biệt 2 trung bình"
        
        tabItem(
            tabName = "2means_ind_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "alpha_2means_ind_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "d_2means_ind_est", 
                                          label = HTML("Sai số (d)"), 
                                          value = 3),
                                radioButtons(inputId = "input_type_2means_ind_est", 
                                             label = "Chọn cách nhập", 
                                             choices = c("Độ lệch chuẩn gộp" = 1,
                                                         "Độ lệch chuẩn của 2 nhóm" = 2)),
                                uiOutput(outputId = "input_2means_ind_est"),
                                uiOutput(outputId = "sdpool_2means_ind_est")
                            ),
                            box(
                                textInput(inputId = "nonrep_2means_ind_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_2means_ind_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_2means_ind_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            conditionalPanel(
                                condition = "input.input_type_2means_ind_est == 1",
                                withMathJax(),
                                p("Công thức 1: $$n= 2\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}^2\\sigma^2}{d^2}\\right)$$")
                            ),
                            conditionalPanel(
                                condition = "input.input_type_2means_ind_est == 2",
                                withMathJax(),
                                p("Công thức 2: $$n= 2\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}^2\\sigma^2}{d^2}\\right)$$"),
                                p("Trong đó:"),
                                p("$$\\sigma=\\sqrt{\\frac{(n_1-1)s^2_1+(n_2-1)s^2_2}{n_1+n_2-2}}$$")
                            )
                        )
                    )
                )
            )
        ),
        
        ##### Nghiên cứu 2 mẫu ghép cặp, xác định sự khác biệt 2 trung bình
        tabItem(
            tabName = "2props_pair_est",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "alpha_2props_pair_est",
                                          label = HTML("Alpha (&alpha;)"),
                                          value = 0.05),
                                textInput(inputId = "sd_2props_pair_est", 
                                          label = HTML("Độ lệch chuẩn của sự khác biệt trước-sau (&sigma;<sub>d</sub>)"), 
                                          value = 9.1),
                                textInput(inputId = "d_2props_pair_est", 
                                          label = HTML("Sai số (d)"), 
                                          value = 3)
                            ),
                            box(
                                textInput(inputId = "nonrep_2props_pair_est",
                                          label = "Tỷ lệ không trả lời",
                                          value = 0),
                                numericInput(inputId = "deseff_2props_pair_est",
                                             label = "Hệ số thiết kế",
                                             value = 1)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_2props_pair_est"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("Công thức 1: $$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2\\sigma_d^2}{d^2}$$")
                        )
                    )
                )
            )
        ),
        
        ##### Nghiên cứu 2 mẫu độc lập, xác định sự khác biệt 2 tỷ lệ

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
                                          value = 0.34),
                                textInput(inputId = "p2_2props_est", 
                                          label = HTML("Tỷ lệ nhóm 2 (P<sub>2</sub>)"), 
                                          value = 0.17),
                                textInput(inputId = "d_2props_est",
                                          label = "Sai số (d)",
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
       
        ##### NC 1 mau, kiem dinh 1 trung binh #####
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
                                        textInput(inputId = "alpha_1mean_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_1mean_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m1_1mean_hypo", 
                                                  label = HTML("Trung bình theo giả thuyết H<sub>1</sub> (&mu;<sub>1</sub>)"), 
                                                  value = 100),
                                        textInput(inputId = "m0_1mean_hypo", 
                                                  label = HTML("Trung bình theo giả thuyết H<sub>0</sub> (&mu;<sub>0</sub>)"), 
                                                  value = 95),
                                        textInput(inputId = "sd_1mean_hypo",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 9.8)
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
                                        textInput(inputId = "m1_1mean_hypo_power", 
                                                  label = HTML("Trung bình theo giả thuyết H<sub>1</sub> (&mu;<sub>1</sub>)"), 
                                                  value = 100),
                                        textInput(inputId = "m0_1mean_hypo_power", 
                                                  label = HTML("Trung bình theo giả thuyết H<sub>0</sub> (&mu;<sub>0</sub>)"), 
                                                  value = 95),
                                        textInput(inputId = "sd_1mean_hypo_power",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 9.8)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_1mean_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_1mean_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 31)
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
                            p("$$n=\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta}}{ES}\\right)^2$$"),
                            p("$$ES=\\frac{\\mu_1-\\mu_0}{\\sigma}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC 1 mau, kiem dinh 1 ty le #####
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
                                        textInput(inputId = "power_1prop_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_1prop_hypo", 
                                                  label = HTML("Kết quả theo giả thuyết H<sub>1</sub> (p<sub>1</sub>)"), 
                                                  value = 0.31),
                                        textInput(inputId = "p0_1prop_hypo",
                                                  label = HTML("Kết quả theo giả thuyết H<sub>0</sub> (p<sub>0</sub>)"),
                                                  value = 0.26)
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
                            p("$$n=\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta}}{ES}\\right)^2$$"),
                            p("$$ES=\\frac{p_1-p_0}{\\sqrt{p_0(1-p_0)}}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC 2 mau doc lap, kiem dinh 2 trung binh #####
        tabItem(
            tabName = "2means_ind_hypo",
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
                                        textInput(inputId = "alpha_2means_ind_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_2means_ind_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m1_2means_ind_hypo", 
                                                  label = HTML("Trung bình nhóm 1 (&mu;<sub>1</sub>)"), 
                                                  value = 10),
                                        textInput(inputId = "m2_2means_ind_hypo", 
                                                  label = HTML("Trung bình nhóm 2 (&mu;<sub>2</sub>)"), 
                                                  value = 5),
                                        radioButtons(inputId = "input_type_2means_ind_hypo", 
                                                     label = "Chọn cách nhập", 
                                                     choices = c("Độ lệch chuẩn gộp" = 1,
                                                                 "Độ lệch chuẩn của 2 nhóm" = 2)),
                                        uiOutput(outputId = "input_2means_ind_hypo"),
                                        uiOutput(outputId = "sdpool_2means_ind_hypo")
                                    ),
                                    box(
                                        numericInput(inputId = "k_2means_ind_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                        textInput(inputId = "nonrep_2means_ind_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2means_ind_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 1</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_2means_ind_hypo"), "</b>")), align = "center"))
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 2</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_2means_ind_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m1_2means_ind_hypo_power", 
                                                  label = HTML("Trung bình nhóm 1 (&mu;<sub>1</sub>)"), 
                                                  value = 10),
                                        textInput(inputId = "m2_2means_ind_hypo_power", 
                                                  label = HTML("Trung bình nhóm 2 (&mu;<sub>2</sub>)"), 
                                                  value = 5),
                                        textInput(inputId = "sd_2means_ind_hypo_power",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 19)
                                    ),
                                    box(
                                        textInput(inputId = "n_2means_ind_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 250),
                                        textInput(inputId = "alpha_2means_ind_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_2means_ind_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=2\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta}}{ES}\\right)^2$$"),
                            p("$$ES=\\frac{\\mu_1-\\mu_2}{\\sigma}$$"),
                            p("$$\\sigma=\\sqrt{\\frac{(n_1-1)s^2_1+(n_2-1)s^2_2}{n_1+n_2-2}}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC 2 mau ghep cap, kiem dinh 2 trung binh #####
        tabItem(
            tabName = "2means_pair_hypo",
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
                                        textInput(inputId = "alpha_2means_pair_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_2means_pair_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m_2means_pair_hypo", 
                                                  label = HTML("Sự khác biệt giữa 2 lần đo (&mu;<sub>d</sub>)"), 
                                                  value = 3),
                                        radioButtons(inputId = "input_type_2means_pair_hypo", 
                                                     label = "Chọn cách nhập", 
                                                     choices = c("Độ lệch chuẩn gộp" = 1,
                                                                 "Độ lệch chuẩn của 2 nhóm" = 2)),
                                        uiOutput(outputId = "input_2means_pair_hypo"),
                                        uiOutput(outputId = "sdpool_2means_pair_hypo")
                                    ),
                                    box(
                                        numericInput(inputId = "k_2means_pair_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                        textInput(inputId = "nonrep_2means_pair_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2means_pair_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 1</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_2means_pair_hypo"), "</b>")), align = "center"))
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 2</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_2means_pair_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m_2means_pair_hypo_power", 
                                                  label = HTML("Sự khác biệt giữa 2 lần đo (&mu;<sub>d</sub>)"), 
                                                  value = 3),
                                        textInput(inputId = "sd_2means_pair_hypo_power",
                                                  label = HTML("Độ lệch chuẩn (&sigma;<sub>d</sub>)"),
                                                  value = 9.1)
                                    ),
                                    box(
                                        textInput(inputId = "n_2means_pair_hypo_power",
                                                  label = "Cỡ mẫu (n)",
                                                  value = 73),
                                        textInput(inputId = "alpha_2means_pair_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_2means_pair_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta}}{ES}\\right)^2$$"),
                            p("$$ES=\\frac{\\mu_d}{\\sigma_d}$$"),
                            p("$$\\sigma=\\sqrt{\\frac{(n_1-1)s^2_1+(n_2-1)s^2_2}{n_1+n_2-2}}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC 2 mau doc lap, kiem dinh 2 ty le #####
        tabItem(
            tabName = "2props_ind_hypo",
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
                                        textInput(inputId = "alpha_2props_ind_hypo",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_2props_ind_hypo",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_2props_ind_hypo", 
                                                  label = HTML("Tỷ lệ ở nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.001),
                                        textInput(inputId = "p2_2props_ind_hypo",
                                                  label = HTML("Tỷ lệ ở nhóm chứng (p<sub>2</sub>)"),
                                                  value = 0.015)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_2props_ind_hypo",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_2props_ind_hypo",
                                                     label = "Hệ số thiết kế",
                                                     value = 1),
                                        numericInput(inputId = "k_2props_ind_hypo",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 1</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_2props_ind_hypo"), "</b>")), align = "center")),
                                        p(HTML("<center><b>Cỡ mẫu nhóm 2</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_2props_ind_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_2props_ind_hypo_power", 
                                                  label = HTML("Tỷ lệ ở nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.001),
                                        textInput(inputId = "p2_2props_ind_hypo_power",
                                                  label = HTML("Tỷ lệ ở nhóm chứng (p<sub>2</sub>)"),
                                                  value = 0.015)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_2props_ind_hypo_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_2props_ind_hypo_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 550)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_2props_ind_hypo"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=2\\left(\\frac{Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta}}{ES}\\right)^2$$"),
                            p("$$ES=\\frac{p_1-p_2}{\\sqrt{p(1-p)}}$$"),
                            p("$$p=\\frac{p_1+p_2}{2}$$")
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
        
        ##### Nghien cuu thuan tap, kiem dinh RR #####
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
        
        ##### NC benh chung, kiem dinh OR #####
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
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "alphasens", 
                                          label = HTML("Alpha (&alpha;)"), 
                                          value = 0.05),
                                textInput(inputId = "sen_sens",
                                          label = HTML("Độ nhạy ước tính (Sens)"),
                                          value = 0.8),
                                textInput(inputId = "error_sens",
                                          label = HTML("Sai số tuyệt đối (d)"),
                                          value = 0.05),
                                textInput(inputId = "p_sens",
                                          label = HTML("Tỷ lệ bệnh hiện hành (P)"),
                                          value = 0.2)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_sen"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2Sens(1-Sens)}{d^2*P}$$")
                        )
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
                        box(title = "Tính cỡ mẫu", width = 6,
                            box(
                                textInput(inputId = "alphaspec", 
                                          label = HTML("Alpha (&alpha;)"), 
                                          value = 0.05),
                                textInput(inputId = "spec_spec",
                                          label = HTML("Độ đặc hiệu ước tính (Spec)"),
                                          value = 0.8),
                                textInput(inputId = "error_spec",
                                          label = HTML("Sai số tuyệt đối (d)"),
                                          value = 0.05),
                                textInput(inputId = "p_spec",
                                          label = HTML("Tỷ lệ bệnh hiện hành (P)"),
                                          value = 0.2)
                            ),
                            box(
                                p(HTML("<center><b>Cỡ mẫu</b></center>")),
                                p(h1(HTML(paste0("<b>", textOutput(outputId = "n_spec"), "</b>")), align = "center"))
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            p("$$n=\\frac{Z_{1-\\frac{\\alpha}{2}}^2Spec(1-Spec)}{d^2*(1-P)}$$")
                        )
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
        
        ##### NC song con #####
        tabItem(
            tabName = "survive",
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
                                        textInput(inputId = "p1_survive", 
                                                  label = HTML("Tỷ lệ ở nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.25),
                                        radioButtons(inputId = "select_survive", 
                                                     label = "Chọn loại tham số", 
                                                     choices = c("Tỷ số nguy hại" = 1,
                                                                 "Tỷ lệ ở nhóm chứng" = 2)),
                                        conditionalPanel(
                                            condition = "input.select_survive == 1",
                                            textInput(inputId = "hr_survive",
                                                      label = HTML("Tỷ số nguy hại (HR)"),
                                                      value = 1.5),
                                        ),
                                        conditionalPanel(
                                            condition = "input.select_survive == 2",
                                            textInput(inputId = "p2_survive",
                                                      label = HTML("Tỷ lệ ở nhóm chứng (p<sub>2</sub>)"),
                                                      value = 0.15),
                                        ),
                                        uiOutput(outputId = "select_survive"),
                                        textInput(inputId = "alpha_survive",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_survive",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_survive",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_survive",
                                                     label = "Hệ số thiết kế",
                                                     value = 1),
                                        numericInput(inputId = "k_survive",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1),
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm 1</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_survive"), "</b>")), align = "center")),
                                        p(HTML("<center><b>Cỡ mẫu nhóm 2</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_survive"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_survive_power", 
                                                  label = HTML("Tỷ lệ ở nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.25),
                                        radioButtons(inputId = "select_survive_power", 
                                                     label = "Chọn loại tham số", 
                                                     choices = c("Tỷ số nguy hại" = 1,
                                                                 "Tỷ lệ ở nhóm chứng" = 2)),
                                        conditionalPanel(
                                            condition = "input.select_survive_power == 1",
                                            textInput(inputId = "hr_survive_power",
                                                      label = HTML("Tỷ số nguy hại (HR)"),
                                                      value = 1.5),
                                        ),
                                        conditionalPanel(
                                            condition = "input.select_survive_power == 2",
                                            textInput(inputId = "p2_survive_power",
                                                      label = HTML("Tỷ lệ ở nhóm chứng (p<sub>2</sub>)"),
                                                      value = 0.15),
                                        ),
                                        uiOutput(outputId = "select_survive_power"),
                                    ),
                                    box(
                                        textInput(inputId = "alpha_survive_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_survive_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 50)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_survive_power"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{(HR+1)^2(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})}{(HR-1)^2(2-p_1-p_2)}$$"),
                            p("$$HR=\\frac{ln(p_1)}{ln(p_2)}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC tuong duong voi bien dinh luong #####
        tabItem(
            tabName = "equi_cont",
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
                                        textInput(inputId = "alpha_equi_cont",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_equi_cont",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m1_equi_cont", 
                                                  label = HTML("Trung bình của nhóm can thiệp (&mu;<sub>1</sub>)"), 
                                                  value = 7),
                                        textInput(inputId = "m2_equi_cont", 
                                                  label = HTML("Trung bình cúa nhóm đối chứng (&mu;<sub>2</sub>)"), 
                                                  value = 4),
                                        textInput(inputId = "d_equi_cont",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 2),
                                        textInput(inputId = "sd_equi_cont",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 10)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_equi_cont",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_equi_cont",
                                                     label = "Hệ số thiết kế",
                                                     value = 1),
                                        numericInput(inputId = "k_equi_cont",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm can thiệp</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_equi_cont"), "</b>")), align = "center")),
                                        p(HTML("<center><b>Cỡ mẫu nhóm đối chứng</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_equi_cont"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m1_equi_cont_power", 
                                                  label = HTML("Trung bình của nhóm can thiệp (&mu;<sub>1</sub>)"), 
                                                  value = 7),
                                        textInput(inputId = "m2_equi_cont_power", 
                                                  label = HTML("Trung bình của nhóm đối chứng (&mu;<sub>2</sub>)"), 
                                                  value = 4),
                                        textInput(inputId = "d_equi_cont_power",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 2),
                                        textInput(inputId = "sd_equi_cont_power",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 10)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_equi_cont_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_equi_cont_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 500)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_equi_cont"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{2(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})}{H^2}$$"),
                            p("$$H=\\frac{|\\mu_1-\\mu_2|-d}{\\sigma}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC tuong duong voi bien dinh tinh #####
        tabItem(
            tabName = "equi_cat",
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
                                        textInput(inputId = "alpha_equi_cat",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_equi_cat",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_equi_cat", 
                                                  label = HTML("Tỷ lệ của nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.4),
                                        textInput(inputId = "p2_equi_cat", 
                                                  label = HTML("Tỷ lệ của nhóm đối chứng (p<sub>2</sub>)"), 
                                                  value = 0.3),
                                        textInput(inputId = "d_equi_cat",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_equi_cat",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_equi_cat",
                                                     label = "Hệ số thiết kế",
                                                     value = 1),
                                        numericInput(inputId = "k_equi_cat",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm can thiệp</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_equi_cat"), "</b>")), align = "center")),
                                        p(HTML("<center><b>Cỡ mẫu nhóm đối chứng</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_equi_cat"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_equi_cat_power", 
                                                  label = HTML("Tỷ lệ của nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.4),
                                        textInput(inputId = "p2_equi_cat_power", 
                                                  label = HTML("Tỷ lệ của nhóm đối chứng (p<sub>2</sub>)"), 
                                                  value = 0.3),
                                        textInput(inputId = "d_equi_cat_power",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_equi_cat_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_equi_cat_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 500)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_equi_cat"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{2(Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta})}{H^2}$$"),
                            p("$$H=\\frac{|p_1-p_2|-d}{\\sqrt{p_1(1-p_1)+p_2(1-p_2)}}$$")
                        )
                    )
                )
            )
        ),
        
        ##### NC khong kem hon voi bien dinh luong #####
        tabItem(
            tabName = "noninfer_cont",
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
                                        textInput(inputId = "alpha_noninfer_cont",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_noninfer_cont",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "m1_noninfer_cont", 
                                                  label = HTML("Trung bình của nhóm can thiệp (&mu;<sub>1</sub>)"), 
                                                  value = 7),
                                        textInput(inputId = "m2_noninfer_cont", 
                                                  label = HTML("Trung bình cúa nhóm đối chứng (&mu;<sub>2</sub>)"), 
                                                  value = 4),
                                        textInput(inputId = "d_noninfer_cont",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 2),
                                        textInput(inputId = "sd_noninfer_cont",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 10)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_noninfer_cont",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_noninfer_cont",
                                                     label = "Hệ số thiết kế",
                                                     value = 1),
                                        numericInput(inputId = "k_noninfer_cont",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm can thiệp</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_noninfer_cont"), "</b>")), align = "center")),
                                        p(HTML("<center><b>Cỡ mẫu nhóm đối chứng</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_noninfer_cont"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "m1_noninfer_cont_power", 
                                                  label = HTML("Trung bình của nhóm can thiệp (&mu;<sub>1</sub>)"), 
                                                  value = 7),
                                        textInput(inputId = "m2_noninfer_cont_power", 
                                                  label = HTML("Trung bình của nhóm đối chứng (&mu;<sub>2</sub>)"), 
                                                  value = 4),
                                        textInput(inputId = "d_noninfer_cont_power",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 2),
                                        textInput(inputId = "sd_noninfer_cont_power",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 10)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_noninfer_cont_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_noninfer_cont_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 500)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_noninfer_cont"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{2(Z_{1-\\alpha}+Z_{1-\\beta})}{H^2}$$"),
                            p("$$H=\\frac{|\\mu_1-\\mu_2|-d}{\\sigma}$$"),
                            p(HTML("Công thức giống với công thức tính cỡ mẫu của 
                                   nghiên cứu tương đương, ngoại trừ việc dùng
                                   Z<sub>1-&alpha;</sub>=1.645 (kiểm định một phía)
                                   thay cho dùng Z<sub>1-&alpha;/2</sub> = 1.96
                                   trong nghiên cứu tương đương"))
                        )
                    )
                )
            )
        ),
        
        ##### NC khong kem hon voi bien dinh tinh #####
        tabItem(
            tabName = "noninfer_cat",
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
                                        textInput(inputId = "alpha_noninfer_cat",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_noninfer_cat",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "p1_noninfer_cat", 
                                                  label = HTML("Tỷ lệ của nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.4),
                                        textInput(inputId = "p2_noninfer_cat", 
                                                  label = HTML("Tỷ lệ của nhóm đối chứng (p<sub>2</sub>)"), 
                                                  value = 0.3),
                                        textInput(inputId = "d_noninfer_cat",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        textInput(inputId = "nonrep_noninfer_cat",
                                                  label = "Tỷ lệ không trả lời",
                                                  value = 0),
                                        numericInput(inputId = "deseff_noninfer_cat",
                                                     label = "Hệ số thiết kế",
                                                     value = 1),
                                        numericInput(inputId = "k_noninfer_cat",
                                                     label = "Tỷ số 2 nhóm",
                                                     value = 1)
                                    ),
                                    box(
                                        p(HTML("<center><b>Cỡ mẫu nhóm can thiệp</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n1_noninfer_cat"), "</b>")), align = "center")),
                                        p(HTML("<center><b>Cỡ mẫu nhóm đối chứng</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n2_noninfer_cat"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_noninfer_cat_power", 
                                                  label = HTML("Tỷ lệ của nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.4),
                                        textInput(inputId = "p2_noninfer_cat_power", 
                                                  label = HTML("Tỷ lệ của nhóm đối chứng (p<sub>2</sub>)"), 
                                                  value = 0.3),
                                        textInput(inputId = "d_noninfer_cat_power",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_noninfer_cat_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_noninfer_cat_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 500)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_noninfer_cat"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{2(Z_{1-\\alpha}+Z_{1-\\beta})}{H^2}$$"),
                            p("$$H=\\frac{|p_1-p_2|-d}{\\sqrt{p_1(1-p_1)+p_2(1-p_2)}}$$"),
                            p(HTML("Công thức giống với công thức tính cỡ mẫu của 
                                   nghiên cứu tương đương, ngoại trừ việc dùng
                                   Z<sub>1-&alpha;</sub>=1.645 (kiểm định một phía)
                                   thay cho dùng Z<sub>1-&alpha;/2</sub> = 1.96
                                   trong nghiên cứu tương đương"))
                        )
                    )
                )
            )
        ),
        
        ##### NC thu nghiem lam sang theo cum #####
        tabItem(
            tabName = "cluster_randomize",
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
                                        textInput(inputId = "sd_cluster_randomize",
                                                  label = HTML("Độ lệch chuẩn (&sigma;)"),
                                                  value = 3),
                                        textInput(inputId = "alpha_cluster_randomize",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "power_cluster_randomize",
                                                  label = HTML("Lực thống kê (1-&beta;)"),
                                                  value = 0.8),
                                        textInput(inputId = "gamma_cluster_randomize", 
                                                  label = HTML("Số lượng cá thể trong từng cụm (&gamma;)"), 
                                                  value = 100),
                                        textInput(inputId = "delta_cluster_randomize", 
                                                  label = HTML("Mức khác biệt về hiệu quả điều trị giữa nhóm bệnh và nhóm chứng (&delta;)"), 
                                                  value = 4)
                                    ),
                                    box(
                                        radioButtons(inputId = "select_icc_cluster_randomize",
                                                     label = "Lựa chọn",
                                                     choices = c("Nhập ICC" = 1,
                                                                 "Tính ICC" = 2)),
                                        conditionalPanel(
                                            condition = "input.select_icc_cluster_randomize == 1",
                                            textInput(inputId = "icc_cluster_randomize", 
                                                      label = HTML("Hệ số tương quan nội cụm (ICC)"), 
                                                      value = 2)
                                        ),
                                        conditionalPanel(
                                            condition = "input.select_icc_cluster_randomize == 2",
                                            textInput(inputId = "var_inter_cluster_randomize", 
                                                      label = HTML("Phương sai sự khác biệt giữa các cụm (&sigma;<sub>u</sub>)"), 
                                                      value = 9),
                                            textInput(inputId = "var_intra_cluster_randomize", 
                                                      label = HTML("Phương sai sự khác biệt giữa các cá thể trong từng cụm (&sigma;<sub>e</sub>)"), 
                                                      value = 5)
                                        ),
                                        uiOutput(outputId = "vif_cluster_randomize")
                                    ),
                                    box(
                                        p(HTML("<center><b>Số cụm</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "n_cluster_randomize"), "</b>")), align = "center"))
                                    )
                                )
                            ),
                            tabPanel(
                                title = "Tính lực thống kê",
                                fluidRow(
                                    box(
                                        textInput(inputId = "p1_cluster_randomize_power", 
                                                  label = HTML("Tỷ lệ của nhóm can thiệp (p<sub>1</sub>)"), 
                                                  value = 0.4),
                                        textInput(inputId = "p2_cluster_randomize_power", 
                                                  label = HTML("Tỷ lệ của nhóm đối chứng (p<sub>2</sub>)"), 
                                                  value = 0.3),
                                        textInput(inputId = "d_cluster_randomize_power",
                                                  label = HTML("Ngưỡng khác biệt (d)"),
                                                  value = 0.05)
                                    ),
                                    box(
                                        textInput(inputId = "alpha_cluster_randomize_power",
                                                  label = HTML("Alpha (&alpha;)"),
                                                  value = 0.05),
                                        textInput(inputId = "n_cluster_randomize_power",
                                                  label = "Cỡ mẫu mỗi nhóm (n)",
                                                  value = 500)
                                    ),
                                    box(
                                        p(HTML("<center><b>Lực thống kê</b></center>")),
                                        p(h1(HTML(paste0("<b>", textOutput(outputId = "power_cluster_randomize"), "</b>")), align = "center"))
                                    )
                                )
                            )
                        ),
                        box(title = "Công thức", width = 6,
                            withMathJax(),
                            p("$$n=\\frac{2\\sigma^2[Z_{1-\\frac{\\alpha}{2}}+Z_{1-\\beta}]^2VIF}{\\gamma\\delta^2}$$"),
                            p("$$VIF=1+(\\gamma-1)ICC$$"),
                            p("$$ICC=\\frac{\\sigma^2_u}{\\sigma^2_u+\\sigma^2_e}$$")
                        )
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
                myBox(width = 10, background = "#222d32",color = "white",
                    #p(HTML('<left><img src="Logo1.png" style="width: 10vw; min-width: 250x;"></left>')),
                    #tags$br(),
                    p(HTML("<left> <b> <p style='font-size:35px;color:white;'> PHẦN MỀM TÍNH TOÁN CỠ MẪU LTM 1.0 </sup> </p> </b> </left>")),
                    p(HTML("<left> <b> <p style='font-size:28px;color:white;'> Dành cho các nghiên cứu khoa học sức khỏe </p> </b> </left>")),
                    # p(HTML("<left> <i> <p style='font-size:15px;color:darkblue;'>  (Phiên bản v1.0-2020) </p> </i> </left>")),
                    p(HTML("<left> <b> <p style='font-size:15px;color:white;'> </b> <i>  Tác giả: Hoàng Văn Minh, Khương Quỳnh Long, Ong Phúc Thịnh </i> </p>  </left>")),
                    tags$br(),
                    p(HTML('<left><img src="Sampling2.gif" style="width: 30w; min-width: 1000x;"></left>')),
                    # p(HTML('<left><img src="Sample1.jpg" style="width: 30vw; min-width: 250x;"></left>')),
                    tags$br(),
                    tags$br(),
                    tags$br(),
                    p(HTML("<i> <p style='font-size:12px;color:white;text-align:left'> (Phần mềm đang trong quá trình xây dựng và thử nghiệm, mọi góp ý xin vui lòng liên hệ nhóm tác giả; <u> Email: kql@huph.edu.vn</u>) </p> </i>")),
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
