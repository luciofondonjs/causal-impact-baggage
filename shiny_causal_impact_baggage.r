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
        
        /* Fix para mostrar correctamente el calendario */
        .datepicker {
          z-index: 9999 !important;
          position: fixed !important;
          top: auto !important;
          bottom: auto !important;
        }
        .datepicker-dropdown {
          z-index: 9999 !important;
          margin-top: 60px !important;
        }
        .datepicker table tr td.active, .datepicker table tr td.active:hover {
          background-color: #337ab7 !important;
          border-color: #2e6da4 !important;
        }
        .datepicker table tr td.today {
          background-color: #fcf8e3 !important;
          border-color: #faebcc !important;
        }
        .datepicker table tr td.today:hover {
          background-color: #f0ad4e !important;
          border-color: #eea236 !important;
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
                             
                             div(style = "position: relative; z-index: 1000;",
                             dateInput("intervention_date",
                                       "📅 Fecha de intervención:",
                                       value = Sys.Date(),
                                           format = "yyyy-mm-dd",
                                           language = "es",
                                           weekstart = 1,
                                           width = "100%")
                             ),
                             
                             div(style = "position: relative; z-index: 1000;",
                             dateInput("pre_start",
                                       "📅 Inicio período pre-intervención:",
                                       value = Sys.Date() - 90,
                                           format = "yyyy-mm-dd",
                                           language = "es",
                                           weekstart = 1,
                                           width = "100%")
                             ),
                             
                             div(style = "position: relative; z-index: 1000;",
                             dateInput("post_end",
                                       "📅 Fin período post-intervención:",
                                       value = Sys.Date() + 30,
                                           format = "yyyy-mm-dd",
                                           language = "es",
                                           weekstart = 1,
                                           width = "100%")
                             )
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
                        plotOutput("native_causal_plot"),
                        
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
                
                # Simplificado al máximo - Solo resumen y gráfico
                fluidRow(
                  box(
                    title = "📋 Resumen Ejecutivo", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    h3("🎯 Análisis CausalImpact - Resultados Clave"),
                    htmlOutput("executive_summary"),
                    
                    # Tabla de métricas principales
                    h4("📊 Métricas Principales"),
                    tableOutput("key_impact_metrics"),
                    
                    # Significancia estadística
                    h4("📈 Significancia Estadística"),
                    htmlOutput("statistical_significance"),
                    
                    # Botón de descarga
                    div(
                      style = "text-align: center; margin: 20px 0;",
                      downloadButton("download_report", 
                                     "📥 Descargar Reporte Completo (HTML)", 
                                     class = "btn-success btn-lg")
                    )
                  )
                ),
                
                # Gráfico CausalImpact - Simplificado al máximo
                fluidRow(
                  box(
                    title = "📊 Gráfico CausalImpact", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    
                    # Intentamos con una altura explícita y ancho completo
                    div(style="height:700px; width:100%; overflow:visible;",
                        plotOutput("native_summary_plot", height = "600px", width = "100%"))
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
      
      # Asegurar que todos los datos sean numéricos
      tryCatch({
        # Verificar y convertir a numérico si es necesario
        numeric_data <- coredata(data_zoo)
        numeric_data <- apply(numeric_data, 2, function(x) {
          # Convertir a numérico con manejo de errores
          as.numeric(as.character(x))
        })
        
        # Crear nuevo objeto zoo con datos numéricos validados
        clean_zoo <- zoo(numeric_data, order.by = index(data_zoo))
        
        # Ejecutar CausalImpact con datos limpios
        values$impact_results <- CausalImpact(clean_zoo, pre.period, post.period, alpha = alpha)
      }, error = function(e) {
        # Si hay error en la conversión, intentar con los datos originales
      values$impact_results <- CausalImpact(data_zoo, pre.period, post.period, alpha = alpha)
      })
      
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
  
  # Debug: Verificar que los resultados se generen correctamente
  output$debug_info <- renderPrint({
    req(values$impact_results)
    cat("=== DEBUG INFO ===\n")
    cat("Tipo de objeto impact_results:", class(values$impact_results), "\n")
    cat("Estructura del objeto:\n")
    str(values$impact_results, max.level = 2)
    cat("=== FIN DEBUG ===\n")
  })
  
  # ==========================================
  # VISUALIZACIONES
  # ==========================================
  
  # Gráfico nativo de CausalImpact (pestaña Análisis)
  output$native_causal_plot <- renderPlot({
    req(values$impact_results)
    
    # Versión original que sabemos que funcionaba
    par(mar = c(5, 4, 4, 2))  # Asegurar márgenes adecuados
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resultados) 
  output$native_results_plot <- renderPlot({
    req(values$impact_results)
    
    # Versión original que sabemos que funcionaba
    plot(values$impact_results)
  })
  
  # Gráfico nativo de CausalImpact (pestaña Resumen) - EXACTAMENTE igual al de Análisis
  output$native_summary_plot <- renderPlot({
    req(values$impact_results)
    
    # Versión original que sabemos que funcionaba en la pestaña Análisis
    par(mar = c(5, 4, 4, 2))  # Asegurar márgenes adecuados
    plot(values$impact_results)
  })
  
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
    
    # Crear gráfico más informativo
      p <- ggplot() +
      geom_col(data = summary_by_period, aes(x = period, y = avg_expected, fill = "Esperado"), 
               alpha = 0.7, width = 0.6) +
      geom_col(data = summary_by_period, aes(x = period, y = avg_actual, fill = "Observado"), 
               alpha = 0.9, width = 0.4) +
      geom_text(data = summary_by_period, 
                aes(x = period, y = avg_expected + max(avg_expected) * 0.05, 
                    label = sprintf("%.4f", avg_expected)), 
                vjust = -0.5, size = 3) +
      geom_text(data = summary_by_period, 
                aes(x = period, y = avg_actual + max(avg_actual) * 0.05, 
                    label = sprintf("%.4f", avg_actual)), 
                vjust = -0.5, size = 3) +
      labs(title = "Comparación: Observado vs Esperado por Período",
           y = paste("Promedio", values$original_target_name), x = "Período",
           fill = "Tipo de Dato") +
      scale_fill_manual(values = c("Esperado" = "lightblue", "Observado" = "darkblue")) +
        theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            legend.position = "bottom")
      
      ggplotly(p)
  })
  
  # ==========================================
  # NUEVOS OUTPUTS PARA EL RESUMEN MEJORADO
  # ==========================================
  
  # Métricas clave del impacto
  output$key_impact_metrics <- renderTable({
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
  
  # Significancia estadística
  output$statistical_significance <- renderText({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    p_value <- summary_data$p[1]
    
    if (p_value < 0.01) {
      significance <- "altamente significativo (p < 0.01)"
      color <- "success"
      icon <- "✅"
    } else if (p_value < 0.05) {
      significance <- "significativo (p < 0.05)"
      color <- "info"
      icon <- "✅"
    } else {
      significance <- "no significativo (p > 0.05)"
      color <- "warning"
      icon <- "⚠️"
    }
    
    html <- paste0(
      "<div class='alert alert-", color, "'>",
      "<h5>", icon, " Nivel de Significancia</h5>",
      "<p><strong>p-value:</strong> ", round(p_value, 4), "</p>",
      "<p><strong>Interpretación:</strong> ", significance, "</p>",
      "</div>"
    )
    
    return(html)
  })
  
  # Impacto financiero estimado
  output$financial_impact <- renderText({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    avg_effect <- summary_data$RelEffect[1]
    
    # Calcular días post-intervención
    post_days <- sum(values$processed_results$period == "Post-intervención")
    
    # Estimación simple del impacto
    if (avg_effect > 0) {
      impact_type <- "positivo"
      color <- "success"
      icon <- "📈"
    } else {
      impact_type <- "negativo"
      color <- "danger"
      icon <- "📉"
    }
    
    html <- paste0(
      "<div class='alert alert-", color, "'>",
      "<h5>", icon, " Impacto Estimado</h5>",
      "<p><strong>Tipo:</strong> ", impact_type, "</p>",
      "<p><strong>Días analizados:</strong> ", post_days, "</p>",
      "<p><strong>Efecto promedio:</strong> ", round(avg_effect * 100, 2), "%</p>",
      "</div>"
    )
    
    return(html)
  })
  
  # Tendencias pre-intervención
  output$pre_trend_plot <- renderPlotly({
    req(values$processed_results)
    
    pre_data <- values$processed_results[values$processed_results$period == "Pre-intervención", ]
    
    if (nrow(pre_data) > 0) {
      p <- ggplot(pre_data, aes(x = time, y = actual)) +
        geom_line(color = "darkgreen", size = 1) +
        geom_smooth(method = "lm", color = "red", linetype = "dashed") +
        labs(title = "Tendencia Pre-Intervención",
             y = values$original_target_name, x = "Fecha") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Tendencias post-intervención
  output$post_trend_plot <- renderPlotly({
    req(values$processed_results)
    
    post_data <- values$processed_results[values$processed_results$period == "Post-intervención", ]
    
    if (nrow(post_data) > 0) {
      p <- ggplot(post_data, aes(x = time, y = actual)) +
        geom_line(color = "purple", size = 1) +
        geom_smooth(method = "lm", color = "orange", linetype = "dashed") +
        labs(title = "Tendencia Post-Intervención",
             y = values$original_target_name, x = "Fecha") +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Resumen ejecutivo estructurado
  output$structured_summary <- renderText({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    avg_effect <- round(summary_data$RelEffect[1] * 100, 2)
    p_value <- summary_data$p[1]
    
    html <- paste0(
      "<div class='well'>",
      "<h5>📊 Resumen Ejecutivo</h5>",
      "<p><strong>Objetivo:</strong> Analizar el impacto de la intervención del ", 
      input$intervention_date, " en ", values$original_target_name, "</p>",
      
      "<h6>🎯 Resultados Principales:</h6>",
      "<ul>",
      "<li><strong>Efecto promedio:</strong> ", ifelse(avg_effect >= 0, "+", ""), avg_effect, "%</li>",
      "<li><strong>Significancia:</strong> ", ifelse(p_value < 0.05, "Estadísticamente significativo", "No significativo"), "</li>",
      "<li><strong>Confianza:</strong> ", input$confidence_level, "%</li>",
      "</ul>",
      
      "<h6>📈 Interpretación:</h6>",
      if (avg_effect > 0 && p_value < 0.05) {
        "<p>✅ La intervención tuvo un impacto positivo y estadísticamente significativo.</p>"
      } else if (avg_effect < 0 && p_value < 0.05) {
        "<p>⚠️ La intervención tuvo un impacto negativo y estadísticamente significativo.</p>"
      } else {
        "<p>🤔 No se detectó un impacto estadísticamente significativo.</p>"
      },
      "</div>"
    )
    
    return(html)
  })
  
  # Recomendaciones
  output$recommendations <- renderText({
    req(values$impact_results)
    
    summary_data <- values$impact_results$summary
    avg_effect <- summary_data$RelEffect[1]
    p_value <- summary_data$p[1]
    
    if (avg_effect > 0 && p_value < 0.05) {
      recommendations <- paste0(
        "<div class='alert alert-success'>",
        "<h6>🎯 Recomendaciones (Impacto Positivo):</h6>",
        "<ul>",
        "<li>✅ <strong>Mantener la intervención</strong> - Los resultados son positivos</li>",
        "<li>📊 <strong>Monitorear continuamente</strong> - Seguir evaluando el impacto</li>",
        "<li>🚀 <strong>Considerar escalar</strong> - La intervención es efectiva</li>",
        "<li>📈 <strong>Optimizar</strong> - Buscar formas de mejorar aún más</li>",
        "</ul>",
        "</div>"
      )
    } else if (avg_effect < 0 && p_value < 0.05) {
      recommendations <- paste0(
        "<div class='alert alert-danger'>",
        "<h6>🎯 Recomendaciones (Impacto Negativo):</h6>",
        "<ul>",
        "<li>⚠️ <strong>Revisar la intervención</strong> - Los resultados son negativos</li>",
        "<li>🔄 <strong>Considerar cambios</strong> - Modificar el enfoque</li>",
        "<li>⏸️ <strong>Pausar si es necesario</strong> - Evaluar antes de continuar</li>",
        "<li>📋 <strong>Analizar causas</strong> - Entender qué salió mal</li>",
        "</ul>",
        "</div>"
      )
    } else {
      recommendations <- paste0(
        "<div class='alert alert-warning'>",
        "<h6>🎯 Recomendaciones (Sin Impacto Significativo):</h6>",
        "<ul>",
        "<li>🤔 <strong>Evaluar la intervención</strong> - No hay evidencia clara de impacto</li>",
        "<li>⏰ <strong>Dar más tiempo</strong> - El efecto puede tardar en manifestarse</li>",
        "<li>📊 <strong>Mejorar el análisis</strong> - Considerar más variables o tiempo</li>",
        "<li>🔄 <strong>Revisar el diseño</strong> - La intervención puede necesitar ajustes</li>",
        "</ul>",
        "</div>"
      )
    }
    
    return(recommendations)
  })
  
  # Limitaciones y consideraciones
  output$limitations <- renderText({
    html <- paste0(
      "<div class='alert alert-info'>",
      "<h6>⚠️ Limitaciones y Consideraciones:</h6>",
      "<ul>",
      "<li><strong>Datos históricos:</strong> La calidad del análisis depende de la cantidad y calidad de datos pre-intervención</li>",
      "<li><strong>Variables de control:</strong> El modelo asume que las variables de control capturan todos los factores externos</li>",
      "<li><strong>Estacionalidad:</strong> Los patrones estacionales pueden afectar la precisión del contrafactual</li>",
      "<li><strong>Intervenciones múltiples:</strong> Si hubo otras intervenciones simultáneas, pueden confundir los resultados</li>",
      "<li><strong>Generalización:</strong> Los resultados pueden no ser aplicables a otros contextos o períodos</li>",
      "</ul>",
      "</div>"
    )
    
    return(html)
  })
  
  # Estabilidad del modelo
  output$model_stability_plot <- renderPlotly({
    req(values$processed_results)
    
    # Calcular residuos del modelo
    residuals <- values$processed_results$actual - values$processed_results$expected
    
    p <- ggplot(data.frame(residuals = residuals, time = values$processed_results$time), 
                aes(x = time, y = residuals)) +
      geom_line(color = "steelblue", alpha = 0.7) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", color = "orange") +
      labs(title = "Estabilidad del Modelo (Residuos)",
           y = "Residuos", x = "Fecha") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Comparación de períodos
  output$period_comparison_plot <- renderPlotly({
    req(values$processed_results)
    
    # Calcular estadísticas por período
    period_stats <- values$processed_results %>%
      group_by(period) %>%
      summarise(
        mean_val = mean(actual, na.rm = TRUE),
        sd_val = sd(actual, na.rm = TRUE),
        min_val = min(actual, na.rm = TRUE),
        max_val = max(actual, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Crear gráfico de barras comparativo
    p <- ggplot(period_stats, aes(x = period, y = mean_val, fill = period)) +
      geom_col(alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_val - sd_val, ymax = mean_val + sd_val), 
                    width = 0.2, color = "black") +
      geom_text(aes(label = sprintf("%.4f", mean_val)), 
                vjust = -0.5, size = 3) +
      labs(title = "Comparación de Períodos",
           y = paste("Promedio", values$original_target_name), x = "Período") +
      scale_fill_manual(values = c("Pre-intervención" = "lightgreen", "Post-intervención" = "lightcoral")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
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
  
  # Función para capturar el gráfico como PNG y convertirlo a base64
  get_plot_base64 <- function() {
    # Crear un archivo temporal para guardar el gráfico
    tmp <- tempfile(fileext = ".png")
    
    # Guardar el gráfico en el archivo temporal
    png(tmp, width = 800, height = 600, res = 100)
    
    # Usar el mismo enfoque simple que funciona en la pestaña Análisis
    tryCatch({
      req(values$impact_results)
      par(mar = c(5, 4, 4, 2))  # Asegurar márgenes adecuados
      plot(values$impact_results)
    }, error = function(e) {
      # Si hay error, mostrar mensaje informativo
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
      text(0.5, 0.5, "No se pudo generar el gráfico", col = "red")
    }, finally = {
      dev.off()
    })
    
    # Convertir el PNG a base64
    base64 <- base64enc::base64encode(tmp)
    
    # Eliminar el archivo temporal
    unlink(tmp)
    
    # Devolver la cadena base64
    return(base64)
  }
  
  # Descargar reporte HTML
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("causal_impact_report_", Sys.Date(), ".html")
    },
    content = function(file) {
      # Obtener el gráfico como base64
      plot_base64 <- get_plot_base64()
      
      # Generar reporte HTML simplificado
      html_content <- paste0(
        "<html>",
        "<head>",
        "<title>Reporte Causal Impact</title>",
        "<style>",
        "body { font-family: Arial, sans-serif; margin: 40px; line-height: 1.6; }",
        "h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }",
        "h2 { color: #34495e; margin-top: 30px; border-left: 4px solid #3498db; padding-left: 10px; }",
        "h3 { color: #2c3e50; margin-top: 25px; }",
        "pre { background-color: #f8f9fa; padding: 15px; border-radius: 5px; overflow-x: auto; }",
        ".section { background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin: 20px 0; border-left: 4px solid #3498db; }",
        "table { border-collapse: collapse; width: 100%; margin: 20px 0; }",
        "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
        "th { background-color: #f2f2f2; }",
        ".significant { color: #28a745; font-weight: bold; }",
        ".not-significant { color: #dc3545; font-weight: bold; }",
        ".impact-positive { color: #28a745; }",
        ".impact-negative { color: #dc3545; }",
        "</style>",
        "</head>",
        "<body>",
        "<h1>📊 Análisis Causal Impact - Resumen Ejecutivo</h1>",
        
        "<div class='section'>",
        "<h2>⚙️ Información del Análisis</h2>",
        "<p><strong>Variable objetivo:</strong> ", values$original_target_name, "</p>",
        "<p><strong>Variables de control:</strong> ", paste(values$original_control_names, collapse = ", "), "</p>",
        "<p><strong>Fecha de intervención:</strong> ", input$intervention_date, "</p>",
        "<p><strong>Período analizado:</strong> ", input$pre_start, " a ", input$post_end, "</p>",
        "<p><strong>Nivel de confianza:</strong> ", input$confidence_level, "%</p>",
        "</div>",
        
        "<div class='section'>",
        "<h2>📊 Resultados Principales</h2>",
        
        "<table>",
        "<tr><th>Métrica</th><th>Valor</th><th>IC Inferior</th><th>IC Superior</th></tr>",
        "<tr><td>Efecto absoluto promedio</td><td>", round(values$impact_results$summary$AbsEffect[1], 4), "</td><td>", round(values$impact_results$summary$AbsEffect.lower[1], 4), "</td><td>", round(values$impact_results$summary$AbsEffect.upper[1], 4), "</td></tr>",
        "<tr><td>Efecto relativo promedio</td><td class='", ifelse(values$impact_results$summary$RelEffect[1] > 0, "impact-positive", "impact-negative"), "'>", ifelse(values$impact_results$summary$RelEffect[1] > 0, "+", ""), round(values$impact_results$summary$RelEffect[1] * 100, 2), "%</td><td>", round(values$impact_results$summary$RelEffect.lower[1] * 100, 2), "%</td><td>", round(values$impact_results$summary$RelEffect.upper[1] * 100, 2), "%</td></tr>",
        "<tr><td>Efecto acumulativo</td><td>", round(values$impact_results$summary$AbsEffect[2], 4), "</td><td>", round(values$impact_results$summary$AbsEffect.lower[2], 4), "</td><td>", round(values$impact_results$summary$AbsEffect.upper[2], 4), "</td></tr>",
        "</table>",
        
        "<h3>📊 Significancia Estadística</h3>",
        "<p><strong>p-value:</strong> <span class='", ifelse(values$impact_results$summary$p[1] < 0.05, "significant", "not-significant"), "'>", round(values$impact_results$summary$p[1], 4), "</span></p>",
        "<p><strong>Interpretación:</strong> <span class='", ifelse(values$impact_results$summary$p[1] < 0.05, "significant", "not-significant"), "'>", 
        if (values$impact_results$summary$p[1] < 0.01) {
          "Altamente significativo (p < 0.01)"
        } else if (values$impact_results$summary$p[1] < 0.05) {
          "Significativo (p < 0.05)"
        } else {
          "No significativo (p > 0.05)"
        }, "</span></p>",
        
        "<h3>💡 Interpretación</h3>",
        if (values$impact_results$summary$RelEffect[1] > 0 && values$impact_results$summary$p[1] < 0.05) {
          "<p>✅ La intervención tuvo un <strong>impacto positivo</strong> y estadísticamente significativo.</p>"
        } else if (values$impact_results$summary$RelEffect[1] < 0 && values$impact_results$summary$p[1] < 0.05) {
          "<p>⚠️ La intervención tuvo un <strong>impacto negativo</strong> y estadísticamente significativo.</p>"
        } else {
          "<p>🤔 No se detectó un impacto estadísticamente significativo en la variable objetivo.</p>"
        },
        "</div>",
        
        "<div class='section'>",
        "<h2>📊 Gráfico CausalImpact</h2>",
        "<p>Este gráfico muestra los resultados del análisis con tres paneles: original, pointwise y cumulative:</p>",
        "<div style='text-align: center; margin: 20px 0;'>",
        "<img src='data:image/png;base64,", plot_base64, "' style='max-width: 100%; height: auto;' alt='Gráfico CausalImpact'>",
        "</div>",
        "</div>",
        
        "<div class='section'>",
        "<h2>📝 Reporte Narrativo Oficial</h2>",
        "<pre>", paste(capture.output(summary(values$impact_results, "report")), collapse = "\n"), "</pre>",
        "</div>",
        
        "<h2>📈 Información Adicional</h2>",
        "<p>Este reporte fue generado automáticamente por la aplicación Shiny de Análisis Causal Impact.</p>",
        "<p><strong>Fecha de generación:</strong> ", Sys.time(), "</p>",
        "<p><strong>Nota:</strong> Los gráficos interactivos no se incluyen en este reporte HTML. Para visualizaciones completas, use la aplicación Shiny.</p>",
        
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
