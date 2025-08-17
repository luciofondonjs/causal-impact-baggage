# ============================================
# APLICACIÓN SHINY - CAUSAL IMPACT ANALYSIS
# Aplicación interactiva para análisis de impacto causal
# ============================================

# Cargar librerías necesarias
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(plotly)) install.packages("plotly")
if (!require(readxl)) install.packages("readxl")
if (!require(CausalImpact)) install.packages("CausalImpact")
if (!require(zoo)) install.packages("zoo")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readxl)
library(CausalImpact)
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)

# ============================================
# INTERFAZ DE USUARIO (UI)
# ============================================

ui <- dashboardPage(
  dashboardHeader(title = "🚀 Análisis Causal Impact - Equipaje JetSmart"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📤 Cargar Datos", tabName = "upload", icon = icon("upload")),
      menuItem("⚙️ Configuración", tabName = "config", icon = icon("cog")),
      menuItem("📊 Análisis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("📈 Resultados", tabName = "results", icon = icon("chart-bar")),
      menuItem("📋 Resumen", tabName = "summary", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f7f7f7;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # ==========================================
      # TAB 1: CARGAR DATOS
      # ==========================================
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "📤 Cargar Archivo de Datos", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  div(
                    h4("📋 Instrucciones:"),
                    tags$ul(
                      tags$li("🔹 Sube un archivo Excel (.xlsx) o CSV (.csv)"),
                      tags$li("🔹 El archivo debe tener una columna de fechas en formato YYYY-MM-DD"),
                      tags$li("🔹 Incluir la variable objetivo (KPI principal) y variables de control"),
                      tags$li("🔹 Las fechas deben ser diarias y consecutivas")
                    )
                  ),
                  
                  br(),
                  
                  fileInput("file", "Seleccionar archivo:",
                            accept = c(".xlsx", ".csv")),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    br(),
                    h4("✅ Archivo cargado exitosamente"),
                    p("Revisa la vista previa de los datos y continúa con la configuración."),
                    br(),
                    h4("👀 Vista previa de los datos:"),
                    DT::dataTableOutput("preview_table")
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 2: CONFIGURACIÓN
      # ==========================================
      tabItem(tabName = "config",
              fluidRow(
                box(
                  title = "⚙️ Configuración del Análisis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.file_uploaded",
                    div(
                      h4("⚠️ Primero carga un archivo de datos"),
                      p("Ve a la pestaña 'Cargar Datos' para subir tu archivo Excel o CSV.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    
                    fluidRow(
                      column(6,
                             h4("🗓️ Configuración de Fechas"),
                             
                             selectInput("date_column", 
                                         "Columna de fechas:",
                                         choices = NULL),
                             
                             dateInput("intervention_date",
                                       "📅 Fecha de intervención:",
                                       value = Sys.Date(),
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("pre_start",
                                       "📅 Inicio período pre-intervención:",
                                       value = Sys.Date() - 90,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("post_end",
                                       "📅 Fin período post-intervención:",
                                       value = Sys.Date() + 30,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1)
                      ),
                      
                      column(6,
                             h4("🎯 Configuración de Variables"),
                             
                             selectInput("target_variable",
                                         "Variable objetivo (KPI principal):",
                                         choices = NULL),
                             
                             selectInput("control_variables",
                                         "Variables de control (confounders):",
                                         choices = NULL,
                                         multiple = TRUE),
                             
                             numericInput("confidence_level",
                                          "Nivel de confianza (%):",
                                          value = 95,
                                          min = 80,
                                          max = 99,
                                          step = 1)
                      )
                    ),
                    
                    br(),
                      
                      conditionalPanel(
                      condition = "output.config_ready",
                      div(
                        class = "alert alert-info",
                        h4("✅ Configuración lista"),
                        p("Todos los parámetros están configurados. Puedes proceder al análisis en la siguiente pestaña.")
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 3: ANÁLISIS
      # ==========================================
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "📊 Ejecutar Análisis Causal Impact", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.config_ready",
                    div(
                      h4("⚠️ Completa la configuración primero"),
                      p("Ve a la pestaña 'Configuración' para establecer las variables y fechas del análisis.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.config_ready",
                    
                    div(
                      style = "text-align: center;",
                      h4("🚀 ¿Listo para ejecutar el análisis?"),
                      p("El análisis puede tomar unos segundos dependiendo del tamaño de los datos."),
                      br(),
                      
                      actionButton("run_analysis", 
                                   "🚀 Ejecutar Análisis Causal Impact", 
                                   class = "btn-primary btn-lg",
                                   style = "margin: 10px;")
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "input.run_analysis > 0",
                      
                      div(id = "analysis_progress",
                          h4("⏳ Ejecutando análisis..."),
                          div(class = "progress progress-striped active",
                              div(class = "progress-bar progress-bar-primary", 
                                  style = "width: 100%")
                          )
                      ),
                      
                      conditionalPanel(
                        condition = "output.analysis_complete",
                        
                        div(
                          class = "alert alert-success",
                          h4("✅ Análisis completado exitosamente"),
                          p("Los resultados están disponibles en las siguientes pestañas.")
                        ),
                        
                        br(),
                        
                        h4("📊 Gráfico Nativo - CausalImpact (Oficial)"),
                        p("Este es el gráfico estándar generado por la librería CausalImpact con 3 paneles: original, pointwise y cumulative."),
                        plotOutput("native_causal_plot", height = "600px"),
                        
                        br(),
                        
                        h4("📊 Gráfico Principal - Personalizado"),
                        plotlyOutput("main_plot", height = "500px"),
                        
                        br(),
                        
                        fluidRow(
                          column(6,
                                 h4("📈 Efectos Puntuales"),
                                 plotlyOutput("point_effects_plot", height = "400px")
                          ),
                          column(6,
                                 h4("📈 Efectos Acumulativos"),
                                 plotlyOutput("cumulative_effects_plot", height = "400px")
                          )
                        )
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 4: RESULTADOS DETALLADOS
      # ==========================================
      tabItem(tabName = "results",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Análisis no completado", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("Ve a la pestaña 'Análisis' para ejecutar el Causal Impact.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📊 Resultados Estadísticos", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📈 Resumen del Impacto"),
                    verbatimTextOutput("impact_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo"),
                    verbatimTextOutput("impact_narrative_report"),
                    
                    br(),
                    
                    h4("📋 Métricas Clave"),
                    tableOutput("key_metrics")
                  ),
                  
                  box(
                    title = "📈 Gráficos Adicionales", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Gráfico Nativo CausalImpact"),
                    plotOutput("native_results_plot", height = "400px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📊 Análisis Detallado", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Serie de Tiempo Completa"),
                    plotlyOutput("full_series_plot", height = "300px")
                  ),
                  
                  box(
                    title = "📊 Distribución de Efectos", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Distribución de Efectos Post-Intervención"),
                    plotlyOutput("effects_distribution", height = "300px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📋 Datos de Resultados", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h4("📊 Tabla de Resultados Detallados"),
                    p("Datos día por día del análisis causal. Puedes exportar esta tabla."),
                    
                    DT::dataTableOutput("results_table"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_results", 
                                     "📥 Descargar Resultados (CSV)", 
                                     class = "btn-info")
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 5: RESUMEN EJECUTIVO
      # ==========================================
      tabItem(tabName = "summary",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Resumen no disponible", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("El resumen ejecutivo se generará después de completar el análisis.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📋 Resumen Ejecutivo", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h3("🎯 Resumen del Análisis Causal Impact"),
                    
                    htmlOutput("executive_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo CausalImpact (Oficial)"),
                    p("Este es el reporte automático generado por la librería CausalImpact:"),
                    verbatimTextOutput("causal_impact_report"),
                    
                    br(),
                    
                    h4("📊 Gráfico Nativo CausalImpact - Resumen"),
                    plotOutput("native_summary_plot", height = "500px"),
                    
                    br(),
                    
                    h4("📊 Gráfico de Resumen Personalizado"),
                    plotlyOutput("summary_plot", height = "400px"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_report", 
                                     "📥 Descargar Reporte Completo (HTML)", 
                                     class = "btn-success btn-lg")
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================
# LÓGICA DEL SERVIDOR (SERVER)
# ============================================

server <- function(input, output, session) {
  
  # Variables reactivas para almacenar datos
  values <- reactiveValues(
    data = NULL,
    impact_results = NULL,
    processed_results = NULL
  )
  
  # ==========================================
  # CARGAR Y PROCESAR DATOS
  # ==========================================
  
  # Cargar archivo
  observe({
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if (ext == "xlsx") {
      values$data <- read_excel(input$file$datapath)
    } else if (ext == "csv") {
      values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    
    # Actualizar opciones de columnas
    if (!is.null(values$data)) {
      updateSelectInput(session, "date_column", 
                        choices = names(values$data))
      
      numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
      updateSelectInput(session, "target_variable", 
                        choices = numeric_cols)
      updateSelectInput(session, "control_variables", 
                        choices = numeric_cols)
    }
  })
  
  # Indicador de archivo cargado
  output$file_uploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Vista previa de datos
  output$preview_table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(
      head(values$data, 100), 
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # ==========================================
  # ACTUALIZACIÓN DINÁMICA DE VARIABLES DE CONTROL
  # ==========================================
  
  # Actualizar variables de control excluyendo la variable objetivo
  observe({
    req(values$data, input$target_variable)
    
    numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
    # Excluir la variable objetivo de las opciones de control
    control_choices <- numeric_cols[numeric_cols != input$target_variable]
    
    updateSelectInput(session, "control_variables", 
                      choices = control_choices)
  })
  
  # Indicador de configuración lista
  output$config_ready <- reactive({
    return(!is.null(values$data) && 
             !is.null(input$date_column) && 
             !is.null(input$target_variable) && 
             !is.null(input$control_variables) &&
             length(input$control_variables) > 0)
  })
  outputOptions(output, "config_ready", suspendWhenHidden = FALSE)
  
  # ==========================================
  # EJECUTAR ANÁLISIS CAUSAL IMPACT
  # ==========================================
  
  observeEvent(input$run_analysis, {
    req(values$data, input$date_column, input$target_variable, input$control_variables)
    
    tryCatch({
      # Preparar datos
      data_prep <- values$data[, c(input$date_column, input$target_variable, input$control_variables)]
      data_prep[[input$date_column]] <- as.Date(data_prep[[input$date_column]])
      
      # Limpiar nombres de columnas (reemplazar espacios con guiones bajos)
      analysis_vars <- c(input$target_variable, input$control_variables)
      clean_names <- make.names(analysis_vars)
      
      # Renombrar columnas para evitar problemas con espacios
      for (i in seq_along(analysis_vars)) {
        if (analysis_vars[i] != clean_names[i]) {
          names(data_prep)[names(data_prep) == analysis_vars[i]] <- clean_names[i]
        }
      }
      
      # Actualizar nombres de variables para el resto del análisis
      target_clean <- make.names(input$target_variable)
      controls_clean <- make.names(input$control_variables)
      
      # Convertir a numérico las variables de análisis
      for (col in c(target_clean, controls_clean)) {
        data_prep[[col]] <- as.numeric(data_prep[[col]])
      }
      
      # Remover NAs
      data_prep <- data_prep[complete.cases(data_prep), ]
      
      # Crear objeto zoo con nombres limpios
      data_zoo <- zoo(data_prep[, c(target_clean, controls_clean)], 
                      order.by = data_prep[[input$date_column]])
      
      # Definir períodos
      pre.period <- as.Date(c(input$pre_start, input$intervention_date - 1))
      post.period <- as.Date(c(input$intervention_date, input$post_end))
      
      # Ejecutar Causal Impact
      alpha <- (100 - input$confidence_level) / 100
      values$impact_results <- CausalImpact(data_zoo, pre.period, post.period, alpha = alpha)
      
      # Procesar resultados para visualización
      results_df <- data.frame(
        time = index(values$impact_results$series),
        actual = as.numeric(values$impact_results$series$response),
        expected = as.numeric(values$impact_results$series$point.pred),
        lower_bound = as.numeric(values$impact_results$series$point.pred.lower),
        upper_bound = as.numeric(values$impact_results$series$point.pred.upper),
        point_effect = as.numeric(values$impact_results$series$point.effect),
        cumulative_effect = as.numeric(values$impact_results$series$cum.effect)
      )
      
      results_df$period <- ifelse(results_df$time < input$intervention_date, "Pre-intervención", "Post-intervención")
      
      # Guardar nombres originales para etiquetas
      values$original_target_name <- input$target_variable
      values$original_control_names <- input$control_variables
      
      values$processed_results <- results_df
      
      showNotification("✅ Análisis completado exitosamente", type = "success")
      
    }, error = function(e) {
      showNotification(paste("❌ Error en el análisis:", e$message), type = "error")
    })
  })
  
  # Indicador de análisis completado
  output$analysis_complete <- reactive({
    return(!is.null(values$impact_results))
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  # ==========================================
  # VISUALIZACIONES
  # ==========================================
  
  # Gráfico nativo de CausalImpact (pestaña Análisis)
  output$native_causal_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resultados) 
  output$native_results_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resumen)
  output$native_summary_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact con título personalizado
    plot(values$impact_results)
    title(main = paste("Análisis CausalImpact -", values$original_target_name),
          sub = paste("Intervención:", input$intervention_date))
  }, height = 500, width = 800)
  
  # Gráfico principal personalizado
  output$main_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual, color = "Observado"), size = 1) +
      geom_line(aes(y = expected, color = "Esperado (contrafactual)"), size = 1) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                  fill = "lightblue", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = paste("Análisis Causal Impact -", values$original_target_name),
           subtitle = paste("Intervención:", input$intervention_date),
           y = values$original_target_name, x = "Fecha",
           color = "Serie") +
      scale_color_manual(values = c("Observado" = "black", "Esperado (contrafactual)" = "blue")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Efectos puntuales
  output$point_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = point_effect)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Puntuales",
           y = "Efecto Puntual", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Efectos acumulativos
  output$cumulative_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = cumulative_effect)) +
      geom_line(color = "purple", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Acumulativos",
           y = "Efecto Acumulativo", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Serie completa
  output$full_series_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual), color = "black", alpha = 0.7) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red") +
      labs(title = "Serie de Tiempo Completa", y = values$original_target_name, x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de efectos
  output$effects_distribution <- renderPlotly({
    req(values$processed_results)
    
    post_effects <- values$processed_results[values$processed_results$period == "Post-intervención", "point_effect"]
    
    p <- ggplot(data.frame(effects = post_effects), aes(x = effects)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = mean(post_effects), color = "red", linetype = "dashed") +
      labs(title = "Distribución de Efectos Post-Intervención",
           x = "Efecto Puntual", y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ==========================================
  # RESULTADOS Y ESTADÍSTICAS
  # ==========================================
  
  # Resumen del impacto
  output$impact_summary <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results)
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resultados)
  output$impact_narrative_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resumen)
  output$causal_impact_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Métricas clave
  output$key_metrics <- renderTable({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    
    metrics <- data.frame(
      Métrica = c("Efecto absoluto promedio", "Efecto relativo promedio", "Efecto acumulativo"),
      Valor = c(
        round(summary_data$AbsEffect[1], 4),
        paste0(round(summary_data$RelEffect[1] * 100, 2), "%"),
        round(summary_data$AbsEffect[2], 4)
      ),
      IC_Inferior = c(
        round(summary_data$AbsEffect.lower[1], 4),
        paste0(round(summary_data$RelEffect.lower[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.lower[2], 4)
      ),
      IC_Superior = c(
        round(summary_data$AbsEffect.upper[1], 4),
        paste0(round(summary_data$RelEffect.upper[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.upper[2], 4)
      )
    )
    
    return(metrics)
  })
  
  # Tabla de resultados
  output$results_table <- DT::renderDataTable({
    req(values$processed_results)
    
    results_display <- values$processed_results
    results_display$time <- as.character(results_display$time)
    
    DT::datatable(
      results_display,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("actual", "expected", "lower_bound", "upper_bound", 
                                  "point_effect", "cumulative_effect"), digits = 4)
  })
  
  # ==========================================
  # RESUMEN EJECUTIVO
  # ==========================================
  
  output$executive_summary <- renderText({
    req(values$impact_results)
    
    # Extraer métricas principales
    summary_data <- values$impact_results$summary
    avg_effect <- round(summary_data$RelEffect[1] * 100, 2)
    p_value <- summary_data$p[1]
    
    # Determinar significancia
    if (p_value < 0.01) {
      significance <- "altamente significativo (p < 0.01)"
      color <- "success"
    } else if (p_value < 0.05) {
      significance <- "significativo (p < 0.05)"
      color <- "info"
    } else {
      significance <- "no significativo (p > 0.05)"
      color <- "warning"
    }
    
    # Generar HTML del resumen
    html <- paste0(
      "<div class='alert alert-", color, "'>",
      "<h4>📊 Resultado Principal</h4>",
      "<p><strong>La intervención tuvo un efecto promedio de ", 
      ifelse(avg_effect >= 0, "+", ""), avg_effect, "%</strong> en la variable objetivo.</p>",
      "<p>Este efecto es <strong>", significance, "</strong>.</p>",
      "</div>",
      
      "<h5>📋 Detalles del Análisis:</h5>",
      "<ul>",
      "<li><strong>Variable objetivo:</strong> ", values$original_target_name, "</li>",
      "<li><strong>Fecha de intervención:</strong> ", input$intervention_date, "</li>",
      "<li><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</li>",
      "<li><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</li>",
      "<li><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</li>",
      "<li><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</li>",
      "</ul>",
      
      "<h5>🎯 Interpretación:</h5>",
      if (avg_effect > 0 && p_value < 0.05) {
        paste0("<p class='text-success'><strong>✅ Impacto positivo confirmado:</strong> ",
               "La intervención generó una mejora estadísticamente significativa en la variable objetivo.</p>")
      } else if (avg_effect < 0 && p_value < 0.05) {
        paste0("<p class='text-danger'><strong>⚠️ Impacto negativo detectado:</strong> ",
               "La intervención tuvo un efecto adverso estadísticamente significativo.</p>")
      } else {
        "<p class='text-warning'><strong>🤔 Sin evidencia de impacto:</strong> No se detectó un efecto estadísticamente significativo de la intervención.</p>"
      }
    )
    
    return(html)
  })
  
  # Gráfico de resumen
  output$summary_plot <- renderPlotly({
    req(values$processed_results)
    
    # Calcular promedios por período
    summary_by_period <- values$processed_results %>%
      group_by(period) %>%
      summarise(
        avg_actual = mean(actual, na.rm = TRUE),
        avg_expected = mean(expected, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Solo mostrar post-intervención para el gráfico de resumen
    post_data <- summary_by_period[summary_by_period$period == "Post-intervención", ]
    
    if (nrow(post_data) > 0) {
      p <- ggplot() +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_expected), 
                 fill = "lightblue", alpha = 0.7, width = 0.5) +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_actual), 
                 fill = "darkblue", alpha = 0.9, width = 0.3) +
        labs(title = "Comparación: Observado vs Esperado (Período Post-Intervención)",
             y = paste("Promedio", values$original_target_name), x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12))
      
      ggplotly(p)
    }
  })
  
  # ==========================================
  # DESCARGAS
  # ==========================================
  
  # Descargar resultados CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("causal_impact_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$processed_results, file, row.names = FALSE)
    }
  )
  
  # Descargar reporte HTML
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("causal_impact_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Generar reporte HTML completo
      html_content <- paste0(
        "<html>",
        "<head>",
        "<title>Reporte Completo Causal Impact</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; }",
        "h1 { color: #2c3e50; border-bottom: 2px solid #3498db; }",
        "h2 { color: #34495e; margin-top: 30px; }",
        "pre { background-color: #f8f9fa; padding: 15px; border-radius: 5px; overflow-x: auto; }",
        ".config { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        ".narrative { background-color: #f0f8f0; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>🚀 Reporte Completo - Análisis Causal Impact</h1>",
        
        "<div class='config'>",
        "<h2>⚙️ Configuración del Análisis</h2>",
        "<p><strong>Variable objetivo:</strong> ", values$original_target_name, "</p>",
        "<p><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</p>",
        "<p><strong>Fecha de intervención:</strong> ", input$intervention_date, "</p>",
        "<p><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</p>",
        "<p><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</p>",
        "<p><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</p>",
        "</div>",
        
        "<h2>📊 Resumen Estadístico</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results)), collapse = "\n"), "</pre>",
        
        "<div class='narrative'>",
        "<h2>📝 Reporte Narrativo Oficial (CausalImpact)</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results, "report")), collapse = "\n"), "</pre>",
        "</div>",
        
        "<h2>📈 Interpretación</h2>",
        "<p>Este reporte fue generado automáticamente por la aplicación Shiny de Análisis Causal Impact.</p>",
        "<p><strong>Fecha de generación:</strong> ", Sys.time(), "</p>",
        
        "</body></html>"
      )
      writeLines(html_content, file)
    }
  )
}

# ============================================
# EJECUTAR APLICACIÓN
# ============================================

shinyApp(ui = ui, server = server)
# ============================================
# APLICACIÓN SHINY - CAUSAL IMPACT ANALYSIS
# Aplicación interactiva para análisis de impacto causal
# ============================================

# Cargar librerías necesarias
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(plotly)) install.packages("plotly")
if (!require(readxl)) install.packages("readxl")
if (!require(CausalImpact)) install.packages("CausalImpact")
if (!require(zoo)) install.packages("zoo")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readxl)
library(CausalImpact)
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)

# ============================================
# INTERFAZ DE USUARIO (UI)
# ============================================

ui <- dashboardPage(
  dashboardHeader(title = "🚀 Análisis Causal Impact - Equipaje JetSmart"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📤 Cargar Datos", tabName = "upload", icon = icon("upload")),
      menuItem("⚙️ Configuración", tabName = "config", icon = icon("cog")),
      menuItem("📊 Análisis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("📈 Resultados", tabName = "results", icon = icon("chart-bar")),
      menuItem("📋 Resumen", tabName = "summary", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f7f7f7;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # ==========================================
      # TAB 1: CARGAR DATOS
      # ==========================================
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "📤 Cargar Archivo de Datos", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  div(
                    h4("📋 Instrucciones:"),
                    tags$ul(
                      tags$li("🔹 Sube un archivo Excel (.xlsx) o CSV (.csv)"),
                      tags$li("🔹 El archivo debe tener una columna de fechas en formato YYYY-MM-DD"),
                      tags$li("🔹 Incluir la variable objetivo (KPI principal) y variables de control"),
                      tags$li("🔹 Las fechas deben ser diarias y consecutivas")
                    )
                  ),
                  
                  br(),
                  
                  fileInput("file", "Seleccionar archivo:",
                            accept = c(".xlsx", ".csv")),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    br(),
                    h4("✅ Archivo cargado exitosamente"),
                    p("Revisa la vista previa de los datos y continúa con la configuración."),
                    br(),
                    h4("👀 Vista previa de los datos:"),
                    DT::dataTableOutput("preview_table")
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 2: CONFIGURACIÓN
      # ==========================================
      tabItem(tabName = "config",
              fluidRow(
                box(
                  title = "⚙️ Configuración del Análisis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.file_uploaded",
                    div(
                      h4("⚠️ Primero carga un archivo de datos"),
                      p("Ve a la pestaña 'Cargar Datos' para subir tu archivo Excel o CSV.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    
                    fluidRow(
                      column(6,
                             h4("🗓️ Configuración de Fechas"),
                             
                             selectInput("date_column", 
                                         "Columna de fechas:",
                                         choices = NULL),
                             
                             dateInput("intervention_date",
                                       "📅 Fecha de intervención:",
                                       value = Sys.Date(),
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("pre_start",
                                       "📅 Inicio período pre-intervención:",
                                       value = Sys.Date() - 90,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("post_end",
                                       "📅 Fin período post-intervención:",
                                       value = Sys.Date() + 30,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1)
                      ),
                      
                      column(6,
                             h4("🎯 Configuración de Variables"),
                             
                             selectInput("target_variable",
                                         "Variable objetivo (KPI principal):",
                                         choices = NULL),
                             
                             selectInput("control_variables",
                                         "Variables de control (confounders):",
                                         choices = NULL,
                                         multiple = TRUE),
                             
                             numericInput("confidence_level",
                                          "Nivel de confianza (%):",
                                          value = 95,
                                          min = 80,
                                          max = 99,
                                          step = 1)
                      )
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "output.config_ready",
                      div(
                        class = "alert alert-info",
                        h4("✅ Configuración lista"),
                        p("Todos los parámetros están configurados. Puedes proceder al análisis en la siguiente pestaña.")
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 3: ANÁLISIS
      # ==========================================
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "📊 Ejecutar Análisis Causal Impact", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.config_ready",
                    div(
                      h4("⚠️ Completa la configuración primero"),
                      p("Ve a la pestaña 'Configuración' para establecer las variables y fechas del análisis.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.config_ready",
                    
                    div(
                      style = "text-align: center;",
                      h4("🚀 ¿Listo para ejecutar el análisis?"),
                      p("El análisis puede tomar unos segundos dependiendo del tamaño de los datos."),
                      br(),
                      
                      actionButton("run_analysis", 
                                   "🚀 Ejecutar Análisis Causal Impact", 
                                   class = "btn-primary btn-lg",
                                   style = "margin: 10px;")
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "input.run_analysis > 0",
                      
                      div(id = "analysis_progress",
                          h4("⏳ Ejecutando análisis..."),
                          div(class = "progress progress-striped active",
                              div(class = "progress-bar progress-bar-primary", 
                                  style = "width: 100%")
                          )
                      ),
                      
                      conditionalPanel(
                        condition = "output.analysis_complete",
                        
                        div(
                          class = "alert alert-success",
                          h4("✅ Análisis completado exitosamente"),
                          p("Los resultados están disponibles en las siguientes pestañas.")
                        ),
                        
                        br(),
                        
                        h4("📊 Gráfico Nativo - CausalImpact (Oficial)"),
                        p("Este es el gráfico estándar generado por la librería CausalImpact con 3 paneles: original, pointwise y cumulative."),
                        plotOutput("native_causal_plot", height = "600px"),
                        
                        br(),
                        
                        h4("📊 Gráfico Principal - Personalizado"),
                        plotlyOutput("main_plot", height = "500px"),
                        
                        br(),
                        
                        fluidRow(
                          column(6,
                                 h4("📈 Efectos Puntuales"),
                                 plotlyOutput("point_effects_plot", height = "400px")
                          ),
                          column(6,
                                 h4("📈 Efectos Acumulativos"),
                                 plotlyOutput("cumulative_effects_plot", height = "400px")
                          )
                        )
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 4: RESULTADOS DETALLADOS
      # ==========================================
      tabItem(tabName = "results",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Análisis no completado", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("Ve a la pestaña 'Análisis' para ejecutar el Causal Impact.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📊 Resultados Estadísticos", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📈 Resumen del Impacto"),
                    verbatimTextOutput("impact_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo"),
                    verbatimTextOutput("impact_narrative_report"),
                    
                    br(),
                    
                    h4("📋 Métricas Clave"),
                    tableOutput("key_metrics")
                  ),
                  
                  box(
                    title = "📈 Gráficos Adicionales", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Gráfico Nativo CausalImpact"),
                    plotOutput("native_results_plot", height = "400px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📊 Análisis Detallado", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Serie de Tiempo Completa"),
                    plotlyOutput("full_series_plot", height = "300px")
                  ),
                  
                  box(
                    title = "📊 Distribución de Efectos", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Distribución de Efectos Post-Intervención"),
                    plotlyOutput("effects_distribution", height = "300px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📋 Datos de Resultados", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h4("📊 Tabla de Resultados Detallados"),
                    p("Datos día por día del análisis causal. Puedes exportar esta tabla."),
                    
                    DT::dataTableOutput("results_table"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_results", 
                                     "📥 Descargar Resultados (CSV)", 
                                     class = "btn-info")
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 5: RESUMEN EJECUTIVO
      # ==========================================
      tabItem(tabName = "summary",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Resumen no disponible", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("El resumen ejecutivo se generará después de completar el análisis.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📋 Resumen Ejecutivo", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h3("🎯 Resumen del Análisis Causal Impact"),
                    
                    htmlOutput("executive_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo CausalImpact (Oficial)"),
                    p("Este es el reporte automático generado por la librería CausalImpact:"),
                    verbatimTextOutput("causal_impact_report"),
                    
                    br(),
                    
                    h4("📊 Gráfico Nativo CausalImpact - Resumen"),
                    plotOutput("native_summary_plot", height = "500px"),
                    
                    br(),
                    
                    h4("📊 Gráfico de Resumen Personalizado"),
                    plotlyOutput("summary_plot", height = "400px"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_report", 
                                     "📥 Descargar Reporte Completo (HTML)", 
                                     class = "btn-success btn-lg")
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================
# LÓGICA DEL SERVIDOR (SERVER)
# ============================================

server <- function(input, output, session) {
  
  # Variables reactivas para almacenar datos
  values <- reactiveValues(
    data = NULL,
    impact_results = NULL,
    processed_results = NULL
  )
  
  # ==========================================
  # CARGAR Y PROCESAR DATOS
  # ==========================================
  
  # Cargar archivo
  observe({
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if (ext == "xlsx") {
      values$data <- read_excel(input$file$datapath)
    } else if (ext == "csv") {
      values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    
    # Actualizar opciones de columnas
    if (!is.null(values$data)) {
      updateSelectInput(session, "date_column", 
                        choices = names(values$data))
      
      numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
      updateSelectInput(session, "target_variable", 
                        choices = numeric_cols)
      updateSelectInput(session, "control_variables", 
                        choices = numeric_cols)
    }
  })
  
  # Indicador de archivo cargado
  output$file_uploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Vista previa de datos
  output$preview_table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(
      head(values$data, 100), 
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # ==========================================
  # ACTUALIZACIÓN DINÁMICA DE VARIABLES DE CONTROL
  # ==========================================
  
  # Actualizar variables de control excluyendo la variable objetivo
  observe({
    req(values$data, input$target_variable)
    
    numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
    # Excluir la variable objetivo de las opciones de control
    control_choices <- numeric_cols[numeric_cols != input$target_variable]
    
    updateSelectInput(session, "control_variables", 
                      choices = control_choices)
  })
  
  # Indicador de configuración lista
  output$config_ready <- reactive({
    return(!is.null(values$data) && 
             !is.null(input$date_column) && 
             !is.null(input$target_variable) && 
             !is.null(input$control_variables) &&
             length(input$control_variables) > 0)
  })
  outputOptions(output, "config_ready", suspendWhenHidden = FALSE)
  
  # ==========================================
  # EJECUTAR ANÁLISIS CAUSAL IMPACT
  # ==========================================
  
  observeEvent(input$run_analysis, {
    req(values$data, input$date_column, input$target_variable, input$control_variables)
    
    tryCatch({
      # Preparar datos
      data_prep <- values$data[, c(input$date_column, input$target_variable, input$control_variables)]
      data_prep[[input$date_column]] <- as.Date(data_prep[[input$date_column]])
      
      # Limpiar nombres de columnas (reemplazar espacios con guiones bajos)
      analysis_vars <- c(input$target_variable, input$control_variables)
      clean_names <- make.names(analysis_vars)
      
      # Renombrar columnas para evitar problemas con espacios
      for (i in seq_along(analysis_vars)) {
        if (analysis_vars[i] != clean_names[i]) {
          names(data_prep)[names(data_prep) == analysis_vars[i]] <- clean_names[i]
        }
      }
      
      # Actualizar nombres de variables para el resto del análisis
      target_clean <- make.names(input$target_variable)
      controls_clean <- make.names(input$control_variables)
      
      # Convertir a numérico las variables de análisis
      for (col in c(target_clean, controls_clean)) {
        data_prep[[col]] <- as.numeric(data_prep[[col]])
      }
      
      # Remover NAs
      data_prep <- data_prep[complete.cases(data_prep), ]
      
      # Crear objeto zoo con nombres limpios
      data_zoo <- zoo(data_prep[, c(target_clean, controls_clean)], 
                      order.by = data_prep[[input$date_column]])
      
      # Definir períodos
      pre.period <- as.Date(c(input$pre_start, input$intervention_date - 1))
      post.period <- as.Date(c(input$intervention_date, input$post_end))
      
      # Ejecutar Causal Impact
      alpha <- (100 - input$confidence_level) / 100
      values$impact_results <- CausalImpact(data_zoo, pre.period, post.period, alpha = alpha)
      
      # Procesar resultados para visualización
      results_df <- data.frame(
        time = index(values$impact_results$series),
        actual = as.numeric(values$impact_results$series$response),
        expected = as.numeric(values$impact_results$series$point.pred),
        lower_bound = as.numeric(values$impact_results$series$point.pred.lower),
        upper_bound = as.numeric(values$impact_results$series$point.pred.upper),
        point_effect = as.numeric(values$impact_results$series$point.effect),
        cumulative_effect = as.numeric(values$impact_results$series$cum.effect)
      )
      
      results_df$period <- ifelse(results_df$time < input$intervention_date, "Pre-intervención", "Post-intervención")
      
      # Guardar nombres originales para etiquetas
      values$original_target_name <- input$target_variable
      values$original_control_names <- input$control_variables
      
      values$processed_results <- results_df
      
      showNotification("✅ Análisis completado exitosamente", type = "success")
      
    }, error = function(e) {
      showNotification(paste("❌ Error en el análisis:", e$message), type = "error")
    })
  })
  
  # Indicador de análisis completado
  output$analysis_complete <- reactive({
    return(!is.null(values$impact_results))
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  # ==========================================
  # VISUALIZACIONES
  # ==========================================
  
  # Gráfico nativo de CausalImpact (pestaña Análisis)
  output$native_causal_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resultados) 
  output$native_results_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resumen)
  output$native_summary_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact con título personalizado
    plot(values$impact_results)
    title(main = paste("Análisis CausalImpact -", values$original_target_name),
          sub = paste("Intervención:", input$intervention_date))
  }, height = 500, width = 800)
  
  # Gráfico principal personalizado
  output$main_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual, color = "Observado"), size = 1) +
      geom_line(aes(y = expected, color = "Esperado (contrafactual)"), size = 1) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                  fill = "lightblue", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = paste("Análisis Causal Impact -", values$original_target_name),
           subtitle = paste("Intervención:", input$intervention_date),
           y = values$original_target_name, x = "Fecha",
           color = "Serie") +
      scale_color_manual(values = c("Observado" = "black", "Esperado (contrafactual)" = "blue")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Efectos puntuales
  output$point_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = point_effect)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Puntuales",
           y = "Efecto Puntual", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Efectos acumulativos
  output$cumulative_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = cumulative_effect)) +
      geom_line(color = "purple", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Acumulativos",
           y = "Efecto Acumulativo", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Serie completa
  output$full_series_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual), color = "black", alpha = 0.7) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red") +
      labs(title = "Serie de Tiempo Completa", y = values$original_target_name, x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de efectos
  output$effects_distribution <- renderPlotly({
    req(values$processed_results)
    
    post_effects <- values$processed_results[values$processed_results$period == "Post-intervención", "point_effect"]
    
    p <- ggplot(data.frame(effects = post_effects), aes(x = effects)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = mean(post_effects), color = "red", linetype = "dashed") +
      labs(title = "Distribución de Efectos Post-Intervención",
           x = "Efecto Puntual", y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ==========================================
  # RESULTADOS Y ESTADÍSTICAS
  # ==========================================
  
  # Resumen del impacto
  output$impact_summary <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results)
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resultados)
  output$impact_narrative_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resumen)
  output$causal_impact_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Métricas clave
  output$key_metrics <- renderTable({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    
    metrics <- data.frame(
      Métrica = c("Efecto absoluto promedio", "Efecto relativo promedio", "Efecto acumulativo"),
      Valor = c(
        round(summary_data$AbsEffect[1], 4),
        paste0(round(summary_data$RelEffect[1] * 100, 2), "%"),
        round(summary_data$AbsEffect[2], 4)
      ),
      IC_Inferior = c(
        round(summary_data$AbsEffect.lower[1], 4),
        paste0(round(summary_data$RelEffect.lower[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.lower[2], 4)
      ),
      IC_Superior = c(
        round(summary_data$AbsEffect.upper[1], 4),
        paste0(round(summary_data$RelEffect.upper[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.upper[2], 4)
      )
    )
    
    return(metrics)
  })
  
  # Tabla de resultados
  output$results_table <- DT::renderDataTable({
    req(values$processed_results)
    
    results_display <- values$processed_results
    results_display$time <- as.character(results_display$time)
    
    DT::datatable(
      results_display,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("actual", "expected", "lower_bound", "upper_bound", 
                                  "point_effect", "cumulative_effect"), digits = 4)
  })
  
  # ==========================================
  # RESUMEN EJECUTIVO
  # ==========================================
  
  output$executive_summary <- renderText({
    req(values$impact_results)
    
    # Extraer métricas principales
    summary_data <- values$impact_results$summary
    avg_effect <- round(summary_data$RelEffect[1] * 100, 2)
    p_value <- summary_data$p[1]
    
    # Determinar significancia
    if (p_value < 0.01) {
      significance <- "altamente significativo (p < 0.01)"
      color <- "success"
    } else if (p_value < 0.05) {
      significance <- "significativo (p < 0.05)"
      color <- "info"
    } else {
      significance <- "no significativo (p > 0.05)"
      color <- "warning"
    }
    
    # Generar HTML del resumen
    html <- paste0(
      "<div class='alert alert-", color, "'>",
      "<h4>📊 Resultado Principal</h4>",
      "<p><strong>La intervención tuvo un efecto promedio de ", 
      ifelse(avg_effect >= 0, "+", ""), avg_effect, "%</strong> en la variable objetivo.</p>",
      "<p>Este efecto es <strong>", significance, "</strong>.</p>",
      "</div>",
      
      "<h5>📋 Detalles del Análisis:</h5>",
      "<ul>",
      "<li><strong>Variable objetivo:</strong> ", values$original_target_name, "</li>",
      "<li><strong>Fecha de intervención:</strong> ", input$intervention_date, "</li>",
      "<li><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</li>",
      "<li><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</li>",
      "<li><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</li>",
      "<li><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</li>",
      "</ul>",
      
      "<h5>🎯 Interpretación:</h5>",
      if (avg_effect > 0 && p_value < 0.05) {
        paste0("<p class='text-success'><strong>✅ Impacto positivo confirmado:</strong> ",
               "La intervención generó una mejora estadísticamente significativa en la variable objetivo.</p>")
      } else if (avg_effect < 0 && p_value < 0.05) {
        paste0("<p class='text-danger'><strong>⚠️ Impacto negativo detectado:</strong> ",
               "La intervención tuvo un efecto adverso estadísticamente significativo.</p>")
      } else {
        "<p class='text-warning'><strong>🤔 Sin evidencia de impacto:</strong> No se detectó un efecto estadísticamente significativo de la intervención.</p>"
      }
    )
    
    return(html)
  })
  
  # Gráfico de resumen
  output$summary_plot <- renderPlotly({
    req(values$processed_results)
    
    # Calcular promedios por período
    summary_by_period <- values$processed_results %>%
      group_by(period) %>%
      summarise(
        avg_actual = mean(actual, na.rm = TRUE),
        avg_expected = mean(expected, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Solo mostrar post-intervención para el gráfico de resumen
    post_data <- summary_by_period[summary_by_period$period == "Post-intervención", ]
    
    if (nrow(post_data) > 0) {
      p <- ggplot() +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_expected), 
                 fill = "lightblue", alpha = 0.7, width = 0.5) +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_actual), 
                 fill = "darkblue", alpha = 0.9, width = 0.3) +
        labs(title = "Comparación: Observado vs Esperado (Período Post-Intervención)",
             y = paste("Promedio", values$original_target_name), x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12))
      
      ggplotly(p)
    }
  })
  
  # ==========================================
  # DESCARGAS
  # ==========================================
  
  # Descargar resultados CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("causal_impact_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$processed_results, file, row.names = FALSE)
    }
  )
  
  # Descargar reporte HTML
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("causal_impact_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Generar reporte HTML completo
      html_content <- paste0(
        "<html>",
        "<head>",
        "<title>Reporte Completo Causal Impact</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; }",
        "h1 { color: #2c3e50; border-bottom: 2px solid #3498db; }",
        "h2 { color: #34495e; margin-top: 30px; }",
        "pre { background-color: #f8f9fa; padding: 15px; border-radius: 5px; overflow-x: auto; }",
        ".config { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        ".narrative { background-color: #f0f8f0; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>🚀 Reporte Completo - Análisis Causal Impact</h1>",
        
        "<div class='config'>",
        "<h2>⚙️ Configuración del Análisis</h2>",
        "<p><strong>Variable objetivo:</strong> ", values$original_target_name, "</p>",
        "<p><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</p>",
        "<p><strong>Fecha de intervención:</strong> ", input$intervention_date, "</p>",
        "<p><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</p>",
        "<p><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</p>",
        "<p><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</p>",
        "</div>",
        
        "<h2>📊 Resumen Estadístico</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results)), collapse = "\n"), "</pre>",
        
        "<div class='narrative'>",
        "<h2>📝 Reporte Narrativo Oficial (CausalImpact)</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results, "report")), collapse = "\n"), "</pre>",
        "</div>",
        
        "<h2>📈 Interpretación</h2>",
        "<p>Este reporte fue generado automáticamente por la aplicación Shiny de Análisis Causal Impact.</p>",
        "<p><strong>Fecha de generación:</strong> ", Sys.time(), "</p>",
        
        "</body></html>"
      )
      writeLines(html_content, file)
    }
  )
}

# ============================================
# EJECUTAR APLICACIÓN
# ============================================

shinyApp(ui = ui, server = server)
# ============================================
# APLICACIÓN SHINY - CAUSAL IMPACT ANALYSIS
# Aplicación interactiva para análisis de impacto causal
# ============================================

# Cargar librerías necesarias
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(plotly)) install.packages("plotly")
if (!require(readxl)) install.packages("readxl")
if (!require(CausalImpact)) install.packages("CausalImpact")
if (!require(zoo)) install.packages("zoo")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readxl)
library(CausalImpact)
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)

# ============================================
# INTERFAZ DE USUARIO (UI)
# ============================================

ui <- dashboardPage(
  dashboardHeader(title = "🚀 Análisis Causal Impact - Equipaje JetSmart"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📤 Cargar Datos", tabName = "upload", icon = icon("upload")),
      menuItem("⚙️ Configuración", tabName = "config", icon = icon("cog")),
      menuItem("📊 Análisis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("📈 Resultados", tabName = "results", icon = icon("chart-bar")),
      menuItem("📋 Resumen", tabName = "summary", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f7f7f7;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # ==========================================
      # TAB 1: CARGAR DATOS
      # ==========================================
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "📤 Cargar Archivo de Datos", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  div(
                    h4("📋 Instrucciones:"),
                    tags$ul(
                      tags$li("🔹 Sube un archivo Excel (.xlsx) o CSV (.csv)"),
                      tags$li("🔹 El archivo debe tener una columna de fechas en formato YYYY-MM-DD"),
                      tags$li("🔹 Incluir la variable objetivo (KPI principal) y variables de control"),
                      tags$li("🔹 Las fechas deben ser diarias y consecutivas")
                    )
                  ),
                  
                  br(),
                  
                  fileInput("file", "Seleccionar archivo:",
                            accept = c(".xlsx", ".csv")),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    br(),
                    h4("✅ Archivo cargado exitosamente"),
                    p("Revisa la vista previa de los datos y continúa con la configuración."),
                    br(),
                    h4("👀 Vista previa de los datos:"),
                    DT::dataTableOutput("preview_table")
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 2: CONFIGURACIÓN
      # ==========================================
      tabItem(tabName = "config",
              fluidRow(
                box(
                  title = "⚙️ Configuración del Análisis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.file_uploaded",
                    div(
                      h4("⚠️ Primero carga un archivo de datos"),
                      p("Ve a la pestaña 'Cargar Datos' para subir tu archivo Excel o CSV.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    
                    fluidRow(
                      column(6,
                             h4("🗓️ Configuración de Fechas"),
                             
                             selectInput("date_column", 
                                         "Columna de fechas:",
                                         choices = NULL),
                             
                             dateInput("intervention_date",
                                       "📅 Fecha de intervención:",
                                       value = Sys.Date(),
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("pre_start",
                                       "📅 Inicio período pre-intervención:",
                                       value = Sys.Date() - 90,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("post_end",
                                       "📅 Fin período post-intervención:",
                                       value = Sys.Date() + 30,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1)
                      ),
                      
                      column(6,
                             h4("🎯 Configuración de Variables"),
                             
                             selectInput("target_variable",
                                         "Variable objetivo (KPI principal):",
                                         choices = NULL),
                             
                             selectInput("control_variables",
                                         "Variables de control (confounders):",
                                         choices = NULL,
                                         multiple = TRUE),
                             
                             numericInput("confidence_level",
                                          "Nivel de confianza (%):",
                                          value = 95,
                                          min = 80,
                                          max = 99,
                                          step = 1)
                      )
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "output.config_ready",
                      div(
                        class = "alert alert-info",
                        h4("✅ Configuración lista"),
                        p("Todos los parámetros están configurados. Puedes proceder al análisis en la siguiente pestaña.")
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 3: ANÁLISIS
      # ==========================================
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "📊 Ejecutar Análisis Causal Impact", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.config_ready",
                    div(
                      h4("⚠️ Completa la configuración primero"),
                      p("Ve a la pestaña 'Configuración' para establecer las variables y fechas del análisis.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.config_ready",
                    
                    div(
                      style = "text-align: center;",
                      h4("🚀 ¿Listo para ejecutar el análisis?"),
                      p("El análisis puede tomar unos segundos dependiendo del tamaño de los datos."),
                      br(),
                      
                      actionButton("run_analysis", 
                                   "🚀 Ejecutar Análisis Causal Impact", 
                                   class = "btn-primary btn-lg",
                                   style = "margin: 10px;")
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "input.run_analysis > 0",
                      
                      div(id = "analysis_progress",
                          h4("⏳ Ejecutando análisis..."),
                          div(class = "progress progress-striped active",
                              div(class = "progress-bar progress-bar-primary", 
                                  style = "width: 100%")
                          )
                      ),
                      
                      conditionalPanel(
                        condition = "output.analysis_complete",
                        
                        div(
                          class = "alert alert-success",
                          h4("✅ Análisis completado exitosamente"),
                          p("Los resultados están disponibles en las siguientes pestañas.")
                        ),
                        
                        br(),
                        
                        h4("📊 Gráfico Nativo - CausalImpact (Oficial)"),
                        p("Este es el gráfico estándar generado por la librería CausalImpact con 3 paneles: original, pointwise y cumulative."),
                        plotOutput("native_causal_plot", height = "600px"),
                        
                        br(),
                        
                        h4("📊 Gráfico Principal - Personalizado"),
                        plotlyOutput("main_plot", height = "500px"),
                        
                        br(),
                        
                        fluidRow(
                          column(6,
                                 h4("📈 Efectos Puntuales"),
                                 plotlyOutput("point_effects_plot", height = "400px")
                          ),
                          column(6,
                                 h4("📈 Efectos Acumulativos"),
                                 plotlyOutput("cumulative_effects_plot", height = "400px")
                          )
                        )
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 4: RESULTADOS DETALLADOS
      # ==========================================
      tabItem(tabName = "results",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Análisis no completado", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("Ve a la pestaña 'Análisis' para ejecutar el Causal Impact.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📊 Resultados Estadísticos", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📈 Resumen del Impacto"),
                    verbatimTextOutput("impact_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo"),
                    verbatimTextOutput("impact_narrative_report"),
                    
                    br(),
                    
                    h4("📋 Métricas Clave"),
                    tableOutput("key_metrics")
                  ),
                  
                  box(
                    title = "📈 Gráficos Adicionales", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Gráfico Nativo CausalImpact"),
                    plotOutput("native_results_plot", height = "400px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📊 Análisis Detallado", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Serie de Tiempo Completa"),
                    plotlyOutput("full_series_plot", height = "300px")
                  ),
                  
                  box(
                    title = "📊 Distribución de Efectos", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Distribución de Efectos Post-Intervención"),
                    plotlyOutput("effects_distribution", height = "300px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📋 Datos de Resultados", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h4("📊 Tabla de Resultados Detallados"),
                    p("Datos día por día del análisis causal. Puedes exportar esta tabla."),
                    
                    DT::dataTableOutput("results_table"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_results", 
                                     "📥 Descargar Resultados (CSV)", 
                                     class = "btn-info")
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 5: RESUMEN EJECUTIVO
      # ==========================================
      tabItem(tabName = "summary",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Resumen no disponible", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("El resumen ejecutivo se generará después de completar el análisis.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📋 Resumen Ejecutivo", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h3("🎯 Resumen del Análisis Causal Impact"),
                    
                    htmlOutput("executive_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo CausalImpact (Oficial)"),
                    p("Este es el reporte automático generado por la librería CausalImpact:"),
                    verbatimTextOutput("causal_impact_report"),
                    
                    br(),
                    
                    h4("📊 Gráfico Nativo CausalImpact - Resumen"),
                    plotOutput("native_summary_plot", height = "500px"),
                    
                    br(),
                    
                    h4("📊 Gráfico de Resumen Personalizado"),
                    plotlyOutput("summary_plot", height = "400px"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_report", 
                                     "📥 Descargar Reporte Completo (HTML)", 
                                     class = "btn-success btn-lg")
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================
# LÓGICA DEL SERVIDOR (SERVER)
# ============================================

server <- function(input, output, session) {
  
  # Variables reactivas para almacenar datos
  values <- reactiveValues(
    data = NULL,
    impact_results = NULL,
    processed_results = NULL
  )
  
  # ==========================================
  # CARGAR Y PROCESAR DATOS
  # ==========================================
  
  # Cargar archivo
  observe({
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if (ext == "xlsx") {
      values$data <- read_excel(input$file$datapath)
    } else if (ext == "csv") {
      values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    
    # Actualizar opciones de columnas
    if (!is.null(values$data)) {
      updateSelectInput(session, "date_column", 
                        choices = names(values$data))
      
      numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
      updateSelectInput(session, "target_variable", 
                        choices = numeric_cols)
      updateSelectInput(session, "control_variables", 
                        choices = numeric_cols)
    }
  })
  
  # Indicador de archivo cargado
  output$file_uploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Vista previa de datos
  output$preview_table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(
      head(values$data, 100), 
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # ==========================================
  # ACTUALIZACIÓN DINÁMICA DE VARIABLES DE CONTROL
  # ==========================================
  
  # Actualizar variables de control excluyendo la variable objetivo
  observe({
    req(values$data, input$target_variable)
    
    numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
    # Excluir la variable objetivo de las opciones de control
    control_choices <- numeric_cols[numeric_cols != input$target_variable]
    
    updateSelectInput(session, "control_variables", 
                      choices = control_choices)
  })
  
  # Indicador de configuración lista
  output$config_ready <- reactive({
    return(!is.null(values$data) && 
             !is.null(input$date_column) && 
             !is.null(input$target_variable) && 
             !is.null(input$control_variables) &&
             length(input$control_variables) > 0)
  })
  outputOptions(output, "config_ready", suspendWhenHidden = FALSE)
  
  # ==========================================
  # EJECUTAR ANÁLISIS CAUSAL IMPACT
  # ==========================================
  
  observeEvent(input$run_analysis, {
    req(values$data, input$date_column, input$target_variable, input$control_variables)
    
    tryCatch({
      # Preparar datos
      data_prep <- values$data[, c(input$date_column, input$target_variable, input$control_variables)]
      data_prep[[input$date_column]] <- as.Date(data_prep[[input$date_column]])
      
      # Limpiar nombres de columnas (reemplazar espacios con guiones bajos)
      analysis_vars <- c(input$target_variable, input$control_variables)
      clean_names <- make.names(analysis_vars)
      
      # Renombrar columnas para evitar problemas con espacios
      for (i in seq_along(analysis_vars)) {
        if (analysis_vars[i] != clean_names[i]) {
          names(data_prep)[names(data_prep) == analysis_vars[i]] <- clean_names[i]
        }
      }
      
      # Actualizar nombres de variables para el resto del análisis
      target_clean <- make.names(input$target_variable)
      controls_clean <- make.names(input$control_variables)
      
      # Convertir a numérico las variables de análisis
      for (col in c(target_clean, controls_clean)) {
        data_prep[[col]] <- as.numeric(data_prep[[col]])
      }
      
      # Remover NAs
      data_prep <- data_prep[complete.cases(data_prep), ]
      
      # Crear objeto zoo con nombres limpios
      data_zoo <- zoo(data_prep[, c(target_clean, controls_clean)], 
                      order.by = data_prep[[input$date_column]])
      
      # Definir períodos
      pre.period <- as.Date(c(input$pre_start, input$intervention_date - 1))
      post.period <- as.Date(c(input$intervention_date, input$post_end))
      
      # Ejecutar Causal Impact
      alpha <- (100 - input$confidence_level) / 100
      values$impact_results <- CausalImpact(data_zoo, pre.period, post.period, alpha = alpha)
      
      # Procesar resultados para visualización
      results_df <- data.frame(
        time = index(values$impact_results$series),
        actual = as.numeric(values$impact_results$series$response),
        expected = as.numeric(values$impact_results$series$point.pred),
        lower_bound = as.numeric(values$impact_results$series$point.pred.lower),
        upper_bound = as.numeric(values$impact_results$series$point.pred.upper),
        point_effect = as.numeric(values$impact_results$series$point.effect),
        cumulative_effect = as.numeric(values$impact_results$series$cum.effect)
      )
      
      results_df$period <- ifelse(results_df$time < input$intervention_date, "Pre-intervención", "Post-intervención")
      
      # Guardar nombres originales para etiquetas
      values$original_target_name <- input$target_variable
      values$original_control_names <- input$control_variables
      
      values$processed_results <- results_df
      
      showNotification("✅ Análisis completado exitosamente", type = "success")
      
    }, error = function(e) {
      showNotification(paste("❌ Error en el análisis:", e$message), type = "error")
    })
  })
  
  # Indicador de análisis completado
  output$analysis_complete <- reactive({
    return(!is.null(values$impact_results))
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  # ==========================================
  # VISUALIZACIONES
  # ==========================================
  
  # Gráfico nativo de CausalImpact (pestaña Análisis)
  output$native_causal_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resultados) 
  output$native_results_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resumen)
  output$native_summary_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact con título personalizado
    plot(values$impact_results)
    title(main = paste("Análisis CausalImpact -", values$original_target_name),
          sub = paste("Intervención:", input$intervention_date))
  }, height = 500, width = 800)
  
  # Gráfico principal personalizado
  output$main_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual, color = "Observado"), size = 1) +
      geom_line(aes(y = expected, color = "Esperado (contrafactual)"), size = 1) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                  fill = "lightblue", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = paste("Análisis Causal Impact -", values$original_target_name),
           subtitle = paste("Intervención:", input$intervention_date),
           y = values$original_target_name, x = "Fecha",
           color = "Serie") +
      scale_color_manual(values = c("Observado" = "black", "Esperado (contrafactual)" = "blue")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Efectos puntuales
  output$point_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = point_effect)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Puntuales",
           y = "Efecto Puntual", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Efectos acumulativos
  output$cumulative_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = cumulative_effect)) +
      geom_line(color = "purple", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Acumulativos",
           y = "Efecto Acumulativo", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Serie completa
  output$full_series_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual), color = "black", alpha = 0.7) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red") +
      labs(title = "Serie de Tiempo Completa", y = values$original_target_name, x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de efectos
  output$effects_distribution <- renderPlotly({
    req(values$processed_results)
    
    post_effects <- values$processed_results[values$processed_results$period == "Post-intervención", "point_effect"]
    
    p <- ggplot(data.frame(effects = post_effects), aes(x = effects)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = mean(post_effects), color = "red", linetype = "dashed") +
      labs(title = "Distribución de Efectos Post-Intervención",
           x = "Efecto Puntual", y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ==========================================
  # RESULTADOS Y ESTADÍSTICAS
  # ==========================================
  
  # Resumen del impacto
  output$impact_summary <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results)
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resultados)
  output$impact_narrative_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resumen)
  output$causal_impact_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Métricas clave
  output$key_metrics <- renderTable({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    
    metrics <- data.frame(
      Métrica = c("Efecto absoluto promedio", "Efecto relativo promedio", "Efecto acumulativo"),
      Valor = c(
        round(summary_data$AbsEffect[1], 4),
        paste0(round(summary_data$RelEffect[1] * 100, 2), "%"),
        round(summary_data$AbsEffect[2], 4)
      ),
      IC_Inferior = c(
        round(summary_data$AbsEffect.lower[1], 4),
        paste0(round(summary_data$RelEffect.lower[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.lower[2], 4)
      ),
      IC_Superior = c(
        round(summary_data$AbsEffect.upper[1], 4),
        paste0(round(summary_data$RelEffect.upper[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.upper[2], 4)
      )
    )
    
    return(metrics)
  })
  
  # Tabla de resultados
  output$results_table <- DT::renderDataTable({
    req(values$processed_results)
    
    results_display <- values$processed_results
    results_display$time <- as.character(results_display$time)
    
    DT::datatable(
      results_display,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("actual", "expected", "lower_bound", "upper_bound", 
                                  "point_effect", "cumulative_effect"), digits = 4)
  })
  
  # ==========================================
  # RESUMEN EJECUTIVO
  # ==========================================
  
  output$executive_summary <- renderText({
    req(values$impact_results)
    
    # Extraer métricas principales
    summary_data <- values$impact_results$summary
    avg_effect <- round(summary_data$RelEffect[1] * 100, 2)
    p_value <- summary_data$p[1]
    
    # Determinar significancia
    if (p_value < 0.01) {
      significance <- "altamente significativo (p < 0.01)"
      color <- "success"
    } else if (p_value < 0.05) {
      significance <- "significativo (p < 0.05)"
      color <- "info"
    } else {
      significance <- "no significativo (p > 0.05)"
      color <- "warning"
    }
    
    # Generar HTML del resumen
    html <- paste0(
      "<div class='alert alert-", color, "'>",
      "<h4>📊 Resultado Principal</h4>",
      "<p><strong>La intervención tuvo un efecto promedio de ", 
      ifelse(avg_effect >= 0, "+", ""), avg_effect, "%</strong> en la variable objetivo.</p>",
      "<p>Este efecto es <strong>", significance, "</strong>.</p>",
      "</div>",
      
      "<h5>📋 Detalles del Análisis:</h5>",
      "<ul>",
      "<li><strong>Variable objetivo:</strong> ", values$original_target_name, "</li>",
      "<li><strong>Fecha de intervención:</strong> ", input$intervention_date, "</li>",
      "<li><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</li>",
      "<li><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</li>",
      "<li><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</li>",
      "<li><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</li>",
      "</ul>",
      
      "<h5>🎯 Interpretación:</h5>",
      if (avg_effect > 0 && p_value < 0.05) {
        paste0("<p class='text-success'><strong>✅ Impacto positivo confirmado:</strong> ",
               "La intervención generó una mejora estadísticamente significativa en la variable objetivo.</p>")
      } else if (avg_effect < 0 && p_value < 0.05) {
        paste0("<p class='text-danger'><strong>⚠️ Impacto negativo detectado:</strong> ",
               "La intervención tuvo un efecto adverso estadísticamente significativo.</p>")
      } else {
        "<p class='text-warning'><strong>🤔 Sin evidencia de impacto:</strong> No se detectó un efecto estadísticamente significativo de la intervención.</p>"
      }
    )
    
    return(html)
  })
  
  # Gráfico de resumen
  output$summary_plot <- renderPlotly({
    req(values$processed_results)
    
    # Calcular promedios por período
    summary_by_period <- values$processed_results %>%
      group_by(period) %>%
      summarise(
        avg_actual = mean(actual, na.rm = TRUE),
        avg_expected = mean(expected, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Solo mostrar post-intervención para el gráfico de resumen
    post_data <- summary_by_period[summary_by_period$period == "Post-intervención", ]
    
    if (nrow(post_data) > 0) {
      p <- ggplot() +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_expected), 
                 fill = "lightblue", alpha = 0.7, width = 0.5) +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_actual), 
                 fill = "darkblue", alpha = 0.9, width = 0.3) +
        labs(title = "Comparación: Observado vs Esperado (Período Post-Intervención)",
             y = paste("Promedio", values$original_target_name), x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12))
      
      ggplotly(p)
    }
  })
  
  # ==========================================
  # DESCARGAS
  # ==========================================
  
  # Descargar resultados CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("causal_impact_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$processed_results, file, row.names = FALSE)
    }
  )
  
  # Descargar reporte HTML
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("causal_impact_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Generar reporte HTML completo
      html_content <- paste0(
        "<html>",
        "<head>",
        "<title>Reporte Completo Causal Impact</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; }",
        "h1 { color: #2c3e50; border-bottom: 2px solid #3498db; }",
        "h2 { color: #34495e; margin-top: 30px; }",
        "pre { background-color: #f8f9fa; padding: 15px; border-radius: 5px; overflow-x: auto; }",
        ".config { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        ".narrative { background-color: #f0f8f0; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>🚀 Reporte Completo - Análisis Causal Impact</h1>",
        
        "<div class='config'>",
        "<h2>⚙️ Configuración del Análisis</h2>",
        "<p><strong>Variable objetivo:</strong> ", values$original_target_name, "</p>",
        "<p><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</p>",
        "<p><strong>Fecha de intervención:</strong> ", input$intervention_date, "</p>",
        "<p><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</p>",
        "<p><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</p>",
        "<p><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</p>",
        "</div>",
        
        "<h2>📊 Resumen Estadístico</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results)), collapse = "\n"), "</pre>",
        
        "<div class='narrative'>",
        "<h2>📝 Reporte Narrativo Oficial (CausalImpact)</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results, "report")), collapse = "\n"), "</pre>",
        "</div>",
        
        "<h2>📈 Interpretación</h2>",
        "<p>Este reporte fue generado automáticamente por la aplicación Shiny de Análisis Causal Impact.</p>",
        "<p><strong>Fecha de generación:</strong> ", Sys.time(), "</p>",
        
        "</body></html>"
      )
      writeLines(html_content, file)
    }
  )
}

# ============================================
# EJECUTAR APLICACIÓN
# ============================================

shinyApp(ui = ui, server = server)
# ============================================
# APLICACIÓN SHINY - CAUSAL IMPACT ANALYSIS
# Aplicación interactiva para análisis de impacto causal
# ============================================

# Cargar librerías necesarias
if (!require(shiny)) install.packages("shiny")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(plotly)) install.packages("plotly")
if (!require(readxl)) install.packages("readxl")
if (!require(CausalImpact)) install.packages("CausalImpact")
if (!require(zoo)) install.packages("zoo")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(ggplot2)) install.packages("ggplot2")

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(readxl)
library(CausalImpact)
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)

# ============================================
# INTERFAZ DE USUARIO (UI)
# ============================================

ui <- dashboardPage(
  dashboardHeader(title = "🚀 Análisis Causal Impact - Equipaje JetSmart"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📤 Cargar Datos", tabName = "upload", icon = icon("upload")),
      menuItem("⚙️ Configuración", tabName = "config", icon = icon("cog")),
      menuItem("📊 Análisis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("📈 Resultados", tabName = "results", icon = icon("chart-bar")),
      menuItem("📋 Resumen", tabName = "summary", icon = icon("file-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f7f7f7;
        }
        .box {
          border-top: 3px solid #3c8dbc;
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #3c8dbc;
          color: white;
        }
      "))
    ),
    
    tabItems(
      # ==========================================
      # TAB 1: CARGAR DATOS
      # ==========================================
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "📤 Cargar Archivo de Datos", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  div(
                    h4("📋 Instrucciones:"),
                    tags$ul(
                      tags$li("🔹 Sube un archivo Excel (.xlsx) o CSV (.csv)"),
                      tags$li("🔹 El archivo debe tener una columna de fechas en formato YYYY-MM-DD"),
                      tags$li("🔹 Incluir la variable objetivo (KPI principal) y variables de control"),
                      tags$li("🔹 Las fechas deben ser diarias y consecutivas")
                    )
                  ),
                  
                  br(),
                  
                  fileInput("file", "Seleccionar archivo:",
                            accept = c(".xlsx", ".csv")),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    br(),
                    h4("✅ Archivo cargado exitosamente"),
                    p("Revisa la vista previa de los datos y continúa con la configuración."),
                    br(),
                    h4("👀 Vista previa de los datos:"),
                    DT::dataTableOutput("preview_table")
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 2: CONFIGURACIÓN
      # ==========================================
      tabItem(tabName = "config",
              fluidRow(
                box(
                  title = "⚙️ Configuración del Análisis", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.file_uploaded",
                    div(
                      h4("⚠️ Primero carga un archivo de datos"),
                      p("Ve a la pestaña 'Cargar Datos' para subir tu archivo Excel o CSV.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    
                    fluidRow(
                      column(6,
                             h4("🗓️ Configuración de Fechas"),
                             
                             selectInput("date_column", 
                                         "Columna de fechas:",
                                         choices = NULL),
                             
                             dateInput("intervention_date",
                                       "📅 Fecha de intervención:",
                                       value = Sys.Date(),
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("pre_start",
                                       "📅 Inicio período pre-intervención:",
                                       value = Sys.Date() - 90,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1),
                             
                             dateInput("post_end",
                                       "📅 Fin período post-intervención:",
                                       value = Sys.Date() + 30,
                                       format = "yyyy-mm-dd",
                                       language = "es",
                                       weekstart = 1)
                      ),
                      
                      column(6,
                             h4("🎯 Configuración de Variables"),
                             
                             selectInput("target_variable",
                                         "Variable objetivo (KPI principal):",
                                         choices = NULL),
                             
                             selectInput("control_variables",
                                         "Variables de control (confounders):",
                                         choices = NULL,
                                         multiple = TRUE),
                             
                             numericInput("confidence_level",
                                          "Nivel de confianza (%):",
                                          value = 95,
                                          min = 80,
                                          max = 99,
                                          step = 1)
                      )
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "output.config_ready",
                      div(
                        class = "alert alert-info",
                        h4("✅ Configuración lista"),
                        p("Todos los parámetros están configurados. Puedes proceder al análisis en la siguiente pestaña.")
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 3: ANÁLISIS
      # ==========================================
      tabItem(tabName = "analysis",
              fluidRow(
                box(
                  title = "📊 Ejecutar Análisis Causal Impact", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12,
                  
                  conditionalPanel(
                    condition = "!output.config_ready",
                    div(
                      h4("⚠️ Completa la configuración primero"),
                      p("Ve a la pestaña 'Configuración' para establecer las variables y fechas del análisis.")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "output.config_ready",
                    
                    div(
                      style = "text-align: center;",
                      h4("🚀 ¿Listo para ejecutar el análisis?"),
                      p("El análisis puede tomar unos segundos dependiendo del tamaño de los datos."),
                      br(),
                      
                      actionButton("run_analysis", 
                                   "🚀 Ejecutar Análisis Causal Impact", 
                                   class = "btn-primary btn-lg",
                                   style = "margin: 10px;")
                    ),
                    
                    br(),
                    
                    conditionalPanel(
                      condition = "input.run_analysis > 0",
                      
                      div(id = "analysis_progress",
                          h4("⏳ Ejecutando análisis..."),
                          div(class = "progress progress-striped active",
                              div(class = "progress-bar progress-bar-primary", 
                                  style = "width: 100%")
                          )
                      ),
                      
                      conditionalPanel(
                        condition = "output.analysis_complete",
                        
                        div(
                          class = "alert alert-success",
                          h4("✅ Análisis completado exitosamente"),
                          p("Los resultados están disponibles en las siguientes pestañas.")
                        ),
                        
                        br(),
                        
                        h4("📊 Gráfico Nativo - CausalImpact (Oficial)"),
                        p("Este es el gráfico estándar generado por la librería CausalImpact con 3 paneles: original, pointwise y cumulative."),
                        plotOutput("native_causal_plot", height = "600px"),
                        
                        br(),
                        
                        h4("📊 Gráfico Principal - Personalizado"),
                        plotlyOutput("main_plot", height = "500px"),
                        
                        br(),
                        
                        fluidRow(
                          column(6,
                                 h4("📈 Efectos Puntuales"),
                                 plotlyOutput("point_effects_plot", height = "400px")
                          ),
                          column(6,
                                 h4("📈 Efectos Acumulativos"),
                                 plotlyOutput("cumulative_effects_plot", height = "400px")
                          )
                        )
                      )
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 4: RESULTADOS DETALLADOS
      # ==========================================
      tabItem(tabName = "results",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Análisis no completado", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("Ve a la pestaña 'Análisis' para ejecutar el Causal Impact.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📊 Resultados Estadísticos", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📈 Resumen del Impacto"),
                    verbatimTextOutput("impact_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo"),
                    verbatimTextOutput("impact_narrative_report"),
                    
                    br(),
                    
                    h4("📋 Métricas Clave"),
                    tableOutput("key_metrics")
                  ),
                  
                  box(
                    title = "📈 Gráficos Adicionales", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Gráfico Nativo CausalImpact"),
                    plotOutput("native_results_plot", height = "400px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📊 Análisis Detallado", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Serie de Tiempo Completa"),
                    plotlyOutput("full_series_plot", height = "300px")
                  ),
                  
                  box(
                    title = "📊 Distribución de Efectos", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 6,
                    
                    h4("📊 Distribución de Efectos Post-Intervención"),
                    plotlyOutput("effects_distribution", height = "300px")
                  )
                ),
                
                fluidRow(
                  box(
                    title = "📋 Datos de Resultados", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h4("📊 Tabla de Resultados Detallados"),
                    p("Datos día por día del análisis causal. Puedes exportar esta tabla."),
                    
                    DT::dataTableOutput("results_table"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_results", 
                                     "📥 Descargar Resultados (CSV)", 
                                     class = "btn-info")
                    )
                  )
                )
              )
      ),
      
      # ==========================================
      # TAB 5: RESUMEN EJECUTIVO
      # ==========================================
      tabItem(tabName = "summary",
              conditionalPanel(
                condition = "!output.analysis_complete",
                fluidRow(
                  box(
                    title = "⚠️ Resumen no disponible", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    h4("Ejecuta el análisis primero"),
                    p("El resumen ejecutivo se generará después de completar el análisis.")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                
                fluidRow(
                  box(
                    title = "📋 Resumen Ejecutivo", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h3("🎯 Resumen del Análisis Causal Impact"),
                    
                    htmlOutput("executive_summary"),
                    
                    br(),
                    
                    h4("📝 Reporte Narrativo CausalImpact (Oficial)"),
                    p("Este es el reporte automático generado por la librería CausalImpact:"),
                    verbatimTextOutput("causal_impact_report"),
                    
                    br(),
                    
                    h4("📊 Gráfico Nativo CausalImpact - Resumen"),
                    plotOutput("native_summary_plot", height = "500px"),
                    
                    br(),
                    
                    h4("📊 Gráfico de Resumen Personalizado"),
                    plotlyOutput("summary_plot", height = "400px"),
                    
                    br(),
                    
                    div(
                      style = "text-align: center;",
                      downloadButton("download_report", 
                                     "📥 Descargar Reporte Completo (HTML)", 
                                     class = "btn-success btn-lg")
                    )
                  )
                )
              )
      )
    )
  )
)

# ============================================
# LÓGICA DEL SERVIDOR (SERVER)
# ============================================

server <- function(input, output, session) {
  
  # Variables reactivas para almacenar datos
  values <- reactiveValues(
    data = NULL,
    impact_results = NULL,
    processed_results = NULL
  )
  
  # ==========================================
  # CARGAR Y PROCESAR DATOS
  # ==========================================
  
  # Cargar archivo
  observe({
    req(input$file)
    
    ext <- tools::file_ext(input$file$datapath)
    
    if (ext == "xlsx") {
      values$data <- read_excel(input$file$datapath)
    } else if (ext == "csv") {
      values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
    
    # Actualizar opciones de columnas
    if (!is.null(values$data)) {
      updateSelectInput(session, "date_column", 
                        choices = names(values$data))
      
      numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
      updateSelectInput(session, "target_variable", 
                        choices = numeric_cols)
      updateSelectInput(session, "control_variables", 
                        choices = numeric_cols)
    }
  })
  
  # Indicador de archivo cargado
  output$file_uploaded <- reactive({
    return(!is.null(values$data))
  })
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
  
  # Vista previa de datos
  output$preview_table <- DT::renderDataTable({
    req(values$data)
    DT::datatable(
      head(values$data, 100), 
      options = list(scrollX = TRUE, pageLength = 10)
    )
  })
  
  # ==========================================
  # ACTUALIZACIÓN DINÁMICA DE VARIABLES DE CONTROL
  # ==========================================
  
  # Actualizar variables de control excluyendo la variable objetivo
  observe({
    req(values$data, input$target_variable)
    
    numeric_cols <- names(values$data)[sapply(values$data, is.numeric)]
    # Excluir la variable objetivo de las opciones de control
    control_choices <- numeric_cols[numeric_cols != input$target_variable]
    
    updateSelectInput(session, "control_variables", 
                      choices = control_choices)
  })
  
  # Indicador de configuración lista
  output$config_ready <- reactive({
    return(!is.null(values$data) && 
             !is.null(input$date_column) && 
             !is.null(input$target_variable) && 
             !is.null(input$control_variables) &&
             length(input$control_variables) > 0)
  })
  outputOptions(output, "config_ready", suspendWhenHidden = FALSE)
  
  # ==========================================
  # EJECUTAR ANÁLISIS CAUSAL IMPACT
  # ==========================================
  
  observeEvent(input$run_analysis, {
    req(values$data, input$date_column, input$target_variable, input$control_variables)
    
    tryCatch({
      # Preparar datos
      data_prep <- values$data[, c(input$date_column, input$target_variable, input$control_variables)]
      data_prep[[input$date_column]] <- as.Date(data_prep[[input$date_column]])
      
      # Limpiar nombres de columnas (reemplazar espacios con guiones bajos)
      analysis_vars <- c(input$target_variable, input$control_variables)
      clean_names <- make.names(analysis_vars)
      
      # Renombrar columnas para evitar problemas con espacios
      for (i in seq_along(analysis_vars)) {
        if (analysis_vars[i] != clean_names[i]) {
          names(data_prep)[names(data_prep) == analysis_vars[i]] <- clean_names[i]
        }
      }
      
      # Actualizar nombres de variables para el resto del análisis
      target_clean <- make.names(input$target_variable)
      controls_clean <- make.names(input$control_variables)
      
      # Convertir a numérico las variables de análisis
      for (col in c(target_clean, controls_clean)) {
        data_prep[[col]] <- as.numeric(data_prep[[col]])
      }
      
      # Remover NAs
      data_prep <- data_prep[complete.cases(data_prep), ]
      
      # Crear objeto zoo con nombres limpios
      data_zoo <- zoo(data_prep[, c(target_clean, controls_clean)], 
                      order.by = data_prep[[input$date_column]])
      
      # Definir períodos
      pre.period <- as.Date(c(input$pre_start, input$intervention_date - 1))
      post.period <- as.Date(c(input$intervention_date, input$post_end))
      
      # Ejecutar Causal Impact
      alpha <- (100 - input$confidence_level) / 100
      values$impact_results <- CausalImpact(data_zoo, pre.period, post.period, alpha = alpha)
      
      # Procesar resultados para visualización
      results_df <- data.frame(
        time = index(values$impact_results$series),
        actual = as.numeric(values$impact_results$series$response),
        expected = as.numeric(values$impact_results$series$point.pred),
        lower_bound = as.numeric(values$impact_results$series$point.pred.lower),
        upper_bound = as.numeric(values$impact_results$series$point.pred.upper),
        point_effect = as.numeric(values$impact_results$series$point.effect),
        cumulative_effect = as.numeric(values$impact_results$series$cum.effect)
      )
      
      results_df$period <- ifelse(results_df$time < input$intervention_date, "Pre-intervención", "Post-intervención")
      
      # Guardar nombres originales para etiquetas
      values$original_target_name <- input$target_variable
      values$original_control_names <- input$control_variables
      
      values$processed_results <- results_df
      
      showNotification("✅ Análisis completado exitosamente", type = "success")
      
    }, error = function(e) {
      showNotification(paste("❌ Error en el análisis:", e$message), type = "error")
    })
  })
  
  # Indicador de análisis completado
  output$analysis_complete <- reactive({
    return(!is.null(values$impact_results))
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  # ==========================================
  # VISUALIZACIONES
  # ==========================================
  
  # Gráfico nativo de CausalImpact (pestaña Análisis)
  output$native_causal_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resultados) 
  output$native_results_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resumen)
  output$native_summary_plot <- renderPlot({
    req(values$impact_results)
    
    # Crear el gráfico nativo de CausalImpact con título personalizado
    plot(values$impact_results)
    title(main = paste("Análisis CausalImpact -", values$original_target_name),
          sub = paste("Intervención:", input$intervention_date))
  }, height = 500, width = 800)
  
  # Gráfico principal personalizado
  output$main_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual, color = "Observado"), size = 1) +
      geom_line(aes(y = expected, color = "Esperado (contrafactual)"), size = 1) +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                  fill = "lightblue", alpha = 0.3) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = paste("Análisis Causal Impact -", values$original_target_name),
           subtitle = paste("Intervención:", input$intervention_date),
           y = values$original_target_name, x = "Fecha",
           color = "Serie") +
      scale_color_manual(values = c("Observado" = "black", "Esperado (contrafactual)" = "blue")) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Efectos puntuales
  output$point_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = point_effect)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Puntuales",
           y = "Efecto Puntual", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Efectos acumulativos
  output$cumulative_effects_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time, y = cumulative_effect)) +
      geom_line(color = "purple", size = 1.2) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.5) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red", size = 1) +
      labs(title = "Efectos Acumulativos",
           y = "Efecto Acumulativo", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Serie completa
  output$full_series_plot <- renderPlotly({
    req(values$processed_results)
    
    p <- ggplot(values$processed_results, aes(x = time)) +
      geom_line(aes(y = actual), color = "black", alpha = 0.7) +
      geom_vline(xintercept = as.numeric(input$intervention_date), 
                 linetype = "dashed", color = "red") +
      labs(title = "Serie de Tiempo Completa", y = values$original_target_name, x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Distribución de efectos
  output$effects_distribution <- renderPlotly({
    req(values$processed_results)
    
    post_effects <- values$processed_results[values$processed_results$period == "Post-intervención", "point_effect"]
    
    p <- ggplot(data.frame(effects = post_effects), aes(x = effects)) +
      geom_histogram(bins = 20, fill = "steelblue", alpha = 0.7) +
      geom_vline(xintercept = mean(post_effects), color = "red", linetype = "dashed") +
      labs(title = "Distribución de Efectos Post-Intervención",
           x = "Efecto Puntual", y = "Frecuencia") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # ==========================================
  # RESULTADOS Y ESTADÍSTICAS
  # ==========================================
  
  # Resumen del impacto
  output$impact_summary <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results)
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resultados)
  output$impact_narrative_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Reporte narrativo de CausalImpact (para pestaña Resumen)
  output$causal_impact_report <- renderPrint({
    req(values$impact_results)
    summary(values$impact_results, "report")
  })
  
  # Métricas clave
  output$key_metrics <- renderTable({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    
    metrics <- data.frame(
      Métrica = c("Efecto absoluto promedio", "Efecto relativo promedio", "Efecto acumulativo"),
      Valor = c(
        round(summary_data$AbsEffect[1], 4),
        paste0(round(summary_data$RelEffect[1] * 100, 2), "%"),
        round(summary_data$AbsEffect[2], 4)
      ),
      IC_Inferior = c(
        round(summary_data$AbsEffect.lower[1], 4),
        paste0(round(summary_data$RelEffect.lower[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.lower[2], 4)
      ),
      IC_Superior = c(
        round(summary_data$AbsEffect.upper[1], 4),
        paste0(round(summary_data$RelEffect.upper[1] * 100, 2), "%"),
        round(summary_data$AbsEffect.upper[2], 4)
      )
    )
    
    return(metrics)
  })
  
  # Tabla de resultados
  output$results_table <- DT::renderDataTable({
    req(values$processed_results)
    
    results_display <- values$processed_results
    results_display$time <- as.character(results_display$time)
    
    DT::datatable(
      results_display,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("actual", "expected", "lower_bound", "upper_bound", 
                                  "point_effect", "cumulative_effect"), digits = 4)
  })
  
  # ==========================================
  # RESUMEN EJECUTIVO
  # ==========================================
  
  output$executive_summary <- renderText({
    req(values$impact_results)
    
    # Extraer métricas principales
    summary_data <- values$impact_results$summary
    avg_effect <- round(summary_data$RelEffect[1] * 100, 2)
    p_value <- summary_data$p[1]
    
    # Determinar significancia
    if (p_value < 0.01) {
      significance <- "altamente significativo (p < 0.01)"
      color <- "success"
    } else if (p_value < 0.05) {
      significance <- "significativo (p < 0.05)"
      color <- "info"
    } else {
      significance <- "no significativo (p > 0.05)"
      color <- "warning"
    }
    
    # Generar HTML del resumen
    html <- paste0(
      "<div class='alert alert-", color, "'>",
      "<h4>📊 Resultado Principal</h4>",
      "<p><strong>La intervención tuvo un efecto promedio de ", 
      ifelse(avg_effect >= 0, "+", ""), avg_effect, "%</strong> en la variable objetivo.</p>",
      "<p>Este efecto es <strong>", significance, "</strong>.</p>",
      "</div>",
      
      "<h5>📋 Detalles del Análisis:</h5>",
      "<ul>",
      "<li><strong>Variable objetivo:</strong> ", values$original_target_name, "</li>",
      "<li><strong>Fecha de intervención:</strong> ", input$intervention_date, "</li>",
      "<li><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</li>",
      "<li><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</li>",
      "<li><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</li>",
      "<li><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</li>",
      "</ul>",
      
      "<h5>🎯 Interpretación:</h5>",
      if (avg_effect > 0 && p_value < 0.05) {
        paste0("<p class='text-success'><strong>✅ Impacto positivo confirmado:</strong> ",
               "La intervención generó una mejora estadísticamente significativa en la variable objetivo.</p>")
      } else if (avg_effect < 0 && p_value < 0.05) {
        paste0("<p class='text-danger'><strong>⚠️ Impacto negativo detectado:</strong> ",
               "La intervención tuvo un efecto adverso estadísticamente significativo.</p>")
      } else {
        "<p class='text-warning'><strong>🤔 Sin evidencia de impacto:</strong> No se detectó un efecto estadísticamente significativo de la intervención.</p>"
      }
    )
    
    return(html)
  })
  
  # Gráfico de resumen
  output$summary_plot <- renderPlotly({
    req(values$processed_results)
    
    # Calcular promedios por período
    summary_by_period <- values$processed_results %>%
      group_by(period) %>%
      summarise(
        avg_actual = mean(actual, na.rm = TRUE),
        avg_expected = mean(expected, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Solo mostrar post-intervención para el gráfico de resumen
    post_data <- summary_by_period[summary_by_period$period == "Post-intervención", ]
    
    if (nrow(post_data) > 0) {
      p <- ggplot() +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_expected), 
                 fill = "lightblue", alpha = 0.7, width = 0.5) +
        geom_col(data = post_data, aes(x = "Post-Intervención", y = avg_actual), 
                 fill = "darkblue", alpha = 0.9, width = 0.3) +
        labs(title = "Comparación: Observado vs Esperado (Período Post-Intervención)",
             y = paste("Promedio", values$original_target_name), x = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12))
      
      ggplotly(p)
    }
  })
  
  # ==========================================
  # DESCARGAS
  # ==========================================
  
  # Descargar resultados CSV
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("causal_impact_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$processed_results, file, row.names = FALSE)
    }
  )
  
  # Descargar reporte HTML
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("causal_impact_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Generar reporte HTML completo
      html_content <- paste0(
        "<html>",
        "<head>",
        "<title>Reporte Completo Causal Impact</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; }",
        "h1 { color: #2c3e50; border-bottom: 2px solid #3498db; }",
        "h2 { color: #34495e; margin-top: 30px; }",
        "pre { background-color: #f8f9fa; padding: 15px; border-radius: 5px; overflow-x: auto; }",
        ".config { background-color: #e8f4f8; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        ".narrative { background-color: #f0f8f0; padding: 15px; border-radius: 5px; margin: 20px 0; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>🚀 Reporte Completo - Análisis Causal Impact</h1>",
        
        "<div class='config'>",
        "<h2>⚙️ Configuración del Análisis</h2>",
        "<p><strong>Variable objetivo:</strong> ", values$original_target_name, "</p>",
        "<p><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</p>",
        "<p><strong>Fecha de intervención:</strong> ", input$intervention_date, "</p>",
        "<p><strong>Período pre-intervención:</strong> ", input$pre_start, " a ", as.Date(input$intervention_date) - 1, "</p>",
        "<p><strong>Período post-intervención:</strong> ", input$intervention_date, " a ", input$post_end, "</p>",
        "<p><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</p>",
        "</div>",
        
        "<h2>📊 Resumen Estadístico</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results)), collapse = "\n"), "</pre>",
        
        "<div class='narrative'>",
        "<h2>📝 Reporte Narrativo Oficial (CausalImpact)</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results, "report")), collapse = "\n"), "</pre>",
        "</div>",
        
        "<h2>📈 Interpretación</h2>",
        "<p>Este reporte fue generado automáticamente por la aplicación Shiny de Análisis Causal Impact.</p>",
        "<p><strong>Fecha de generación:</strong> ", Sys.time(), "</p>",
        
        "</body></html>"
      )
      writeLines(html_content, file)
    }
  )
}

# ============================================
# EJECUTAR APLICACIÓN
# ============================================

shinyApp(ui = ui, server = server)
