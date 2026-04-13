library(shiny)

# ==============================================================================
# 1. THE SOLVENT DATABASE
# ==============================================================================
solventDB <- data.frame(
  name = c("Water", "Ethanol", "1-Propanol", "2-Propanol (IPA)", "Acetone", "Ethyl Acetate", "Butyl Acetate", 
           "Anisole", "Propylene Carbonate", "Ethyl Lactate", "Acetic Acid", "Formic Acid", "Glycerol", 
           "Dimethyl Carbonate", "Methyl Ethyl Ketone (MEK)", "Isobutanol", "Methanol", "Acetonitrile", 
           "Cyclohexane", "Methylcyclohexane", "Heptane", "Methyl tert-butyl ether (MTBE)", "Dimethyl Sulfoxide (DMSO)", 
           "2-Methyltetrahydrofuran (2-MeTHF)", "p-Xylene", "Isoamyl Alcohol", "Benzyl Alcohol", "Tetralin", 
           "Toluene", "Xylene (mix)", "Tetrahydrofuran (THF)", "Pentane", "Dichloromethane (DCM)", "Diethyl Ether", 
           "Diisopropyl Ether", "1,4-Dioxane", "Dimethylformamide (DMF)", "N-Methyl-2-pyrrolidone (NMP)", 
           "Pyridine", "Triethylamine (TEA)", "Chlorobenzene", "Trifluoroacetic Acid (TFA)", "Benzene", 
           "Chloroform", "Carbon Tetrachloride", "Hexane", "1,2-Dichloroethane", "Carbon Disulfide", 
           "Trichloroethylene", "Nitrobenzene"),
  hz = c(0, 1.0, 1.5, 1.5, 1.2, 1.5, 1.8, 2.0, 1.5, 1.5, 2.0, 2.5, 1.0, 2.0, 3.0, 3.0, 3.5, 4.5, 4.0, 4.0, 
         4.5, 4.0, 3.0, 3.5, 4.0, 3.5, 3.0, 4.5, 7.0, 7.0, 7.0, 6.5, 8.5, 7.5, 7.5, 8.0, 8.5, 8.5, 8.0, 6.0, 
         7.5, 7.0, 10.0, 9.5, 10.0, 8.5, 10.0, 10.0, 10.0, 9.0),
  price = c(5, 55, 60, 45, 40, 50, 55, 90, 65, 70, 60, 80, 40, 65, 55, 60, 40, 90, 70, 75, 90, 65, 120, 150, 
            60, 70, 85, 80, 50, 55, 100, 110, 50, 90, 100, 120, 85, 95, 180, 120, 80, 300, 150, 110, 200, 80, 
            100, 140, 120, 110)
)
solventDB <- solventDB[order(solventDB$name), ]
solvent_choices <- c("Select Solvent..." = "", solventDB$name)

get_k_factor <- function(n) {
  if (is.na(n)) return(0)
  if (n <= 3) return(5.31); if (n == 4) return(4.16); if (n == 5) return(3.50)
  if (n == 6) return(3.09); if (n == 7) return(2.89); if (n == 8) return(2.75)
  if (n == 9) return(2.63); if (n == 10) return(2.57); if (n <= 15) return(2.33)
  if (n <= 20) return(2.20); if (n <= 30) return(2.08); return(1.96)
}

# ==============================================================================
# 2. USER INTERFACE 
# ==============================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    :root { --c-red: #d32f2f; --c-green: #2e7d32; --c-blue: #1565c0; --c-violet: #7b1fa2; --c-dark: #212121; }
    body { background-color: #eef2f6; font-family: 'Segoe UI', Roboto, sans-serif; }
    
    /* Header */
    .header-bar { background: var(--c-dark); color: white; padding: 15px 0; border-bottom: 6px solid var(--c-blue); margin-bottom: 30px; text-align: center; }
    .header-bar h2 { margin: 0; font-weight: bold; font-size: 28px; }
    
    /* Inputs Override (Method Name) */
    #methodName { border: 4px solid var(--c-dark) !important; font-weight: 900 !important; text-transform: uppercase !important; font-size: 20px !important; height: 55px !important; margin-bottom: 20px; padding: 15px;}
    .control-label { font-weight: 700; color: var(--c-dark); font-size: 14px; }
    .form-control { border: 2px solid #ccc; font-weight: 600; }
    
    /* Tabs Override */
    .nav-tabs { border-bottom: 4px solid var(--c-dark); margin-bottom: 0px; border-radius: 0; }
    .nav-tabs > li > a { background: #f0f0f0; font-weight: 800; color: #666; text-transform: uppercase; border: none; border-top: 5px solid transparent; border-radius: 0; padding: 12px 20px; font-size: 16px;}
    .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { background: white; color: black; border: none; border-top-color: black; border-bottom: 4px solid white; margin-bottom: -4px; }
    .nav-tabs > li:nth-child(1).active > a { border-top-color: var(--c-red); color: var(--c-red); }
    .nav-tabs > li:nth-child(2).active > a { border-top-color: var(--c-green); color: var(--c-green); }
    .nav-tabs > li:nth-child(3).active > a { border-top-color: var(--c-blue); color: var(--c-blue); }
    .nav-tabs > li:nth-child(4).active > a { border-top-color: var(--c-violet); color: var(--c-violet); }
    .tab-content { padding: 25px; border: 4px solid var(--c-dark); background: white; border-radius: 0 0 12px 12px; border-top: none; margin-bottom: 20px; }

    /* Inner Result Boxes */
    .result-box { padding: 10px; font-weight: bold; border-radius: 6px; margin-top: 8px; margin-bottom: 15px; font-size: 14px; display: flex; justify-content: space-between; align-items: center; }
    .res-green { background: #e8f5e9; border-left: 5px solid var(--c-green); color: #1b5e20; }
    .res-blue { background: #e3f2fd; border-left: 5px solid var(--c-blue); color: #0d47a1; }
    .res-red { background: #ffebee; border-left: 5px solid var(--c-red); color: #b71c1c; }

    /* Visualizer Mosaic */
    #visualizer-wrapper { background: white; border-radius: 16px; padding: 30px 20px; box-shadow: 0 10px 30px rgba(0,0,0,0.08); display: flex; flex-direction: column; align-items: center; gap: 20px; }
    .index-square { position: relative; width: 180px; height: 150px; border: 6px solid black; border-radius: 16px; display: flex; flex-direction: column; justify-content: center; align-items: center; transition: background-color 0.5s ease; }
    .index-val { font-size: 75px; font-weight: 900; color: white; -webkit-text-stroke: 2.5px black; paint-order: stroke fill; line-height: 1; margin: 0; }
    .validity-badge { font-size: 16px; font-weight: 900; padding: 4px 10px; border-radius: 4px; margin-top: 5px; border: 2px solid black; background: white; }
    .mosaic-container { width: 100%; height: 500px; display: flex; gap: 12px; }
    .box-left { flex: 45; } 
    .right-stack { flex: 55; display: flex; flex-direction: column; gap: 12px; height: 100%; }
    .box-green { flex: 25; } .box-blue { flex: 20; } .box-violet { flex: 10; } 
    .wac-box { border: 6px solid black; border-radius: 12px; display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 10px; transition: background 0.3s linear; }
    .m-label { font-size: 22px; font-weight: 900; text-transform: uppercase; color: white; -webkit-text-stroke: 2px black; paint-order: stroke fill; margin-bottom: 5px; text-align: center;}
    .m-score { font-size: 85px; font-weight: 900; color: white; -webkit-text-stroke: 3.5px black; paint-order: stroke fill; line-height: 1; }
  "))),
  
  div(class="header-bar", h2("Mosaic-WAC: Enough Thinking, Time for Decisions")),
  
  fluidRow(
    column(5, offset = 1,
           textInput("methodName", NULL, placeholder = "METHOD NAME"),
           tabsetPanel(
             # --- RED ---
             tabPanel("Red", br(),
                      h4("PERFORMANCE (45%)", style="color:#d32f2f; font-weight:bold; border-bottom:1px solid #ddd; padding-bottom:10px;"),
                      selectInput("r_spec", "1. Method Target (Spec Limit)", 
                                  choices=c("Select Spec Limit..."="", "API / Assay (±2.0%)"=2.0, "Finished Product (±5.0%)"=5.0, "Related Substances (±10.0%)"=10.0, "Trace / Bioanalysis (±20.0%)"=20.0)),
                      fluidRow(
                        column(6, numericInput("r_analytes", "No. of Analytes", value=1, min=1)),
                        column(6, selectInput("r_matrix", "Matrix Complexity", 
                                              choices=c("Select Matrix..."="", "Pure Solvent"=10, "Tablet / Powder"=40, "Cream / Suspension"=70, "Blood / Soil"=100)))
                      ),
                      fluidRow(
                        column(4, numericInput("r_n", "Samples (n)", value=9, min=3)),
                        column(4, numericInput("r_rec", "Limiting Acc%", value=NA)),
                        column(4, numericInput("r_rsd", "Limiting RSD%", value=NA))
                      ),
                      uiOutput("r_ti_box"),
                      
                      selectInput("r_conc", "2. Precision Context (HorRat)", 
                                  choices=c("Select Concentration..."="", "Major Component (100% - 1%)"=1, "Minor Component (0.1% - ppm)"=0.001, "Trace / Residue (ppb)"=0.000001)),
                      uiOutput("r_hor_box")
             ),
             
             # --- GREEN ---
             tabPanel("Green", br(),
                      h4("GREENNESS (25%)", style="color:#2e7d32; font-weight:bold; border-bottom:1px solid #ddd; padding-bottom:10px;"),
                      h5(strong("Liquid Solvents")),
                      fluidRow(column(7, selectInput("sol1_name", "Solvent (Database)", choices = solvent_choices)), column(5, numericInput("sol1_vol", "Vol (mL)", value = NA, min=0))),
                      fluidRow(column(7, selectInput("sol2_name", "Solvent (Database)", choices = solvent_choices)), column(5, numericInput("sol2_vol", "Vol (mL)", value = NA, min=0))),
                      h5(strong("Solid Reagents")),
                      fluidRow(
                        column(4, textInput("sol_name", "Name", placeholder="Reagent")),
                        column(3, numericInput("sol_mass", "Mass(g)", value = NA)),
                        column(3, selectInput("sol_haz", "Hazard", choices=c("None (0)"=0,"Low (3)"=3,"High (7)"=7,"Extr (10)"=10))),
                        column(2, numericInput("sol_cost", "Cost($)", value = NA))
                      ),
                      uiOutput("g_summary_box")
             ),
             
             # --- BLUE ---
             tabPanel("Blue", br(),
                      h4("PRACTICALITY (20%)", style="color:#1565c0; font-weight:bold; border-bottom:1px solid #ddd; padding-bottom:10px;"),
                      selectInput("b_inst", "1. Instrument Type", 
                                  choices=c("Select Instrument..."="", "UV-Vis / Colorimeter"=5, "Voltammetry / Potentiometry"=5, "HPLC"=15, "GC / UHPLC"=25, "LC-MS / GC-MS"=50, "ICP-MS / NMR"=70)),
                      fluidRow(
                        column(6, numericInput("b_time", "Run Time (min)", value=NA)), 
                        column(6, numericInput("b_prep_time", "Prep Time (min)", value=NA))
                      ),
                      numericInput("b_consumables", "Consumables / Amortized Parts ($)", value=NA),
                      helpText("Cost per run for columns, cartridges, strips, filters.", style="color:#888; font-size:12px; margin-top:-10px; margin-bottom:15px;"),
                      selectInput("b_prep_type", "2. Sample Preparation", 
                                  choices=c("Select Prep Type..."="", "Direct / Dilute"=100, "Filtration / Centrifuge"=80, "Extraction (SPE/LLE)"=50, "Derivatization / Evaporation"=20)),
                      uiOutput("b_summary_box")
             ),
             
             # --- VIOLET ---
             tabPanel("Vio", br(),
                      h4("INNOVATION (10%)", style="color:#7b1fa2; font-weight:bold; border-bottom:1px solid #ddd; padding-bottom:10px;"),
                      checkboxInput("v1", tags$b("AI / Machine Learning / AQbD"), value=FALSE),
                      checkboxInput("v2", tags$b("Novel Materials (Nano/MOF)"), value=FALSE),
                      checkboxInput("v3", tags$b("Miniaturized System"), value=FALSE),
                      checkboxInput("v4", tags$b("Smartphone Detection"), value=FALSE)
             )
           )
    ),
    
    # --- VISUALIZER ---
    column(5, uiOutput("mosaic_diagram"))
  )
)

# ==============================================================================
# 3. SERVER LOGIC & DYNAMIC MOSAIC GENERATION
# ==============================================================================
server <- function(input, output) {
  
  output$mosaic_diagram <- renderUI({
    
    # --- Calculate Red ---
    r <- 0
    ti <- NA; horrat <- NA; sti <- NA; shor <- NA
    
    if(all(input$r_spec!="", !is.na(input$r_rec), !is.na(input$r_rsd), !is.na(input$r_n), input$r_conc!="", input$r_matrix!="")) {
      spec <- as.numeric(input$r_spec); conc <- as.numeric(input$r_conc); smat <- as.numeric(input$r_matrix)
      ti <- abs(100 - input$r_rec) + (get_k_factor(input$r_n) * input$r_rsd)
      sti <- if(ti <= spec) 100 else if(ti > spec*2) 0 else 100 - ((ti-spec)/spec*100)
      prsd <- 2^(1 - 0.5 * log10(conc))
      horrat <- if(input$r_rsd == 0) 0 else input$r_rsd / prsd
      shor <- if(horrat <= 1) 100 else if(horrat >= 2) 0 else 100 - ((horrat-1)*100)
      r <- ((sti*0.5 + shor*0.5) * 0.70) + (smat * 0.20) + (min(100, 20*input$r_analytes) * 0.10)
    }
    
    # Render Red Inner Boxes
    output$r_ti_box <- renderUI({
      if(is.na(ti)) { HTML('<div class="result-box res-red">Awaiting Inputs...</div>') } 
      else { HTML(paste0('<div class="result-box res-red"><span>Total Error: ±', sprintf("%.2f", ti), '%</span> <span>', ifelse(ti <= as.numeric(input$r_spec), '✅ PASS', '❌ FAIL'), '</span></div>')) }
    })
    output$r_hor_box <- renderUI({
      if(is.na(horrat)) { HTML('<div class="result-box res-red">Awaiting Inputs...</div>') } 
      else { HTML(paste0('<div class="result-box res-red"><span>HorRat: ', sprintf("%.2f", horrat), '</span> <span>', ifelse(horrat <= 1, '✅ OK', '⚠️ High'), '</span></div>')) }
    })
    
    # --- Calculate Green ---
    load <- 0; waste <- 0; cost <- 0
    solvents <- list(list(n=input$sol1_name, v=input$sol1_vol), list(n=input$sol2_name, v=input$sol2_vol))
    for(s in solvents) {
      if(!is.null(s$n) && s$n != "" && !is.na(s$v) && s$v > 0) {
        idx <- match(s$n, solventDB$name)
        load <- load + (s$v * solventDB$hz[idx])
        waste <- waste + s$v
        cost <- cost + ((s$v/1000) * solventDB$price[idx])
      }
    }
    if(!is.na(input$sol_mass) && input$sol_mass > 0) {
      load <- load + (input$sol_mass * as.numeric(input$sol_haz))
      waste <- waste + input$sol_mass
      cost <- cost + ifelse(is.na(input$sol_cost), 0, input$sol_cost)
    }
    
    g <- 0
    if(waste > 0 || load > 0) g <- max(0, min(100, 100 * exp(-load/600) - min(20, waste/100)))
    
    # Render Green Inner Box
    output$g_summary_box <- renderUI({
      if(waste == 0 && load == 0) { HTML('<div class="result-box res-green">Awaiting Inputs...</div>') } 
      else { HTML(paste0('<div class="result-box res-green"><span>Waste: ', sprintf("%.0f", waste), ' unit | Haz Load: ', sprintf("%.0f", load), '</span> <span>Score: ', round(g), '</span></div>')) }
    })
    
    # --- Calculate Blue ---
    b <- 0
    tPA <- NA; cPA <- NA
    if(input$b_inst != "" && input$b_prep_type != "" && !is.na(input$b_time) && !is.na(input$b_prep_time)) {
      consumables <- ifelse(is.na(input$b_consumables), 0, input$b_consumables)
      rawT <- input$b_time + input$b_prep_time
      totalC <- ((rawT/60) * as.numeric(input$b_inst)) + cost + consumables
      tPA <- rawT / input$r_analytes
      cPA <- totalC / input$r_analytes
      scost <- max(0, 100 * (1 - (cPA/100)))
      stime <- max(0, 100 * (1 - (tPA/60)))
      b <- (scost*0.40) + (stime*0.40) + (as.numeric(input$b_prep_type)*0.20)
    }
    
    # Render Blue Inner Box
    output$b_summary_box <- renderUI({
      if(is.na(tPA)) { HTML('<div class="result-box res-blue">Awaiting Inputs...</div>') } 
      else { HTML(paste0('<div class="result-box res-blue"><span>', sprintf("%.1f", tPA), ' min | $', sprintf("%.2f", cPA), ' /analyte</span> <span>Score: ', round(b), '</span></div>')) }
    })
    
    # --- Calculate Violet ---
    v <- sum(c(input$v1, input$v2, input$v3, input$v4) * 25)
    
    # --- Totals & Colors ---
    t <- (r*0.45) + (g*0.25) + (b*0.20) + (v*0.10)
    gray_val <- round(128 + (t * 1.27))
    
    # Validity Badge Logic
    if(r==0 && g==0 && b==0 && v==0) {
      b_txt <- "AWAITING DATA"; b_col <- "black"
    } else if(t < 60) {
      b_txt <- "❌ INVALID"; b_col <- "red"
    } else {
      b_txt <- "✅ VALID"; b_col <- "green"
    }
    
    # Generate Exact HTML Replica
    HTML(paste0('
      <div id="visualizer-wrapper">
          <div class="index-square" style="background-color: rgb(',gray_val,',',gray_val,',',gray_val,');">
              <span class="index-val">', sprintf("%.1f", t), '</span>
              <span class="validity-badge" style="color:',b_col,'; border-color:',b_col,';">', b_txt, '</span>
          </div>
          <div class="mosaic-container">
              <div class="wac-box box-left" style="background: linear-gradient(0deg, #d32f2f ',round(r),'%, white ',round(r),'%);">
                  <span class="m-label">PERFORMANCE</span><span class="m-score">', round(r), '</span>
              </div>
              <div class="right-stack">
                  <div class="wac-box box-green" style="background: linear-gradient(0deg, #2e7d32 ',round(g),'%, white ',round(g),'%);">
                      <span class="m-label">Greenness</span><span class="m-score">', round(g), '</span>
                  </div>
                  <div class="wac-box box-blue" style="background: linear-gradient(0deg, #1565c0 ',round(b),'%, white ',round(b),'%);">
                      <span class="m-label">Practical</span><span class="m-score">', round(b), '</span>
                  </div>
                  <div class="wac-box box-violet" style="background: linear-gradient(0deg, #7b1fa2 ',round(v),'%, white ',round(v),'%);">
                      <span class="m-label">Innovation</span><span class="m-score">', round(v), '</span>
                  </div>
              </div>
          </div>
      </div>
    '))
  })
}

shinyApp(ui = ui, server = server)
