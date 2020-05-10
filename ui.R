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
            icon = icon("percentage"), startExpanded = TRUE,
            menuSubItem("So sánh 2 tỷ lệ", tabName = "2props"),
            menuSubItem("Ước lượng 1 tỷ lệ", tabName = "prop")
            ),
        menuItem(
            "Biến định lượng", 
            icon = icon("chart-line"), startExpanded = TRUE,
            menuSubItem("So sánh 2 trung bình", tabName = "2means")
            ),
        menuItem(
            "Hệ số tương quan", 
            tabName = "corr", icon = icon("chart-bar")
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
        tabItem(
            tabName = "proportion",
        ),
        tabItem(
            tabName = "2means",
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Nhập số",
                    fluidRow(
                        box(title = "Tham số", width = 4,
                            box(
                                textInput(inputId = "m1_2means", 
                                          label = "Trung bình nhóm 1", 
                                          value = 100.56),
                                textInput(inputId = "sd1_2means",
                                          label = "Độ lệch chuẩn nhóm 1",
                                          value = 7.7),
                                textInput(inputId = "m2_2means", 
                                          label = "Trung bình nhóm 2", 
                                          value = 94.22),
                                textInput(inputId = "sd2_2means",
                                          label = "Độ lệch chuẩn nhóm 2",
                                          value = 5.61),
                                numericInput(inputId = "k_2means",
                                             label = "Tỷ số 2 nhóm",
                                             value = 1)
                            ),
                            box(
                                textInput(inputId = "alpha_2means",
                                          label = "Alpha",
                                          value = 0.05),
                                textInput(inputId = "power_2means",
                                          label = "Power",
                                          value = 0.8)
                            ),
                        ),
                        box(title = "Hướng dẫn", width = 8,
                            p("Phần này sẽ dùng để giải thích ý nghĩa các tham số"),
                            p("$$n=\\frac{(Z_{1-\\frac{\\alpha}{2}}\\sqrt{P_0(1-P_0)}+Z_{1-\\beta}\\sqrt{P_a(1-P_a)})^2}{(P_a-P_0)^2}$$")
                        ),
                        valueBoxOutput(outputId = "n1_2means"),
                        valueBoxOutput(outputId = "n2_2means")
                    )
                )
            )
        ),
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
                                          value = 0.3),
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
                box(title = "Credit", width = 6),
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
              href = "https://github.com/thinhong/SampleSizeCalc/wiki"),
            class = "dropdown"
        ),
        tags$li(
            a(strong("Source code"),
              href = "https://github.com/thinhong/SampleSizeCalc/"),
            class = "dropdown"
        )
    ),
    skin = "black",
    sidebar,
    body
)
