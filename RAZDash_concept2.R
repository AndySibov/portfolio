


repos <- "https://raw.githubusercontent.com/AndySibov/portfolio/main/"

library(tidyverse)
library(readr)
library(forcats)
library(ggplot2)
library(sf)
library(stringr)
library(rlang)
library(viridisLite)
library(httr)
library(knitr)
library(RColorBrewer)
library(purrr)
library(readr)
library(tools)
library(scales)
library(ggiraph)
library(RColorBrewer)
library(GWalkR)
library(forcats)
library(janitor)
library(rmarkdown)
library(shiny)
library(DT)
library(bs4Dash)
library(shinyWidgets)
library(colorspace)
library(shinyjs)
library(gt)
library(lme4)
library(shinyjs)
library(glue)

# Function to get files from GitHub and return their contents
load_git_files <- function(repos, filenames, token = NULL, invisible = TRUE, csv_delimiter = NULL) {
  
  stopifnot(requireNamespace('tools'))
  
  #standaard delimiter instellen
  if(is.null(csv_delimiter)) csv_delimiter <- ';'
  
  # Functie om verschillende files te kunnen lezen
  read_common_file <- function(file_path, delimit = csv_delimiter) {
    file_ext <- tools::file_ext(file_path)
    
    # Switch op file extension
    switch(file_ext,
           csv = read_delim(file_path, delim = delimit),   # Use separator if provided
           tsv = read_delim(file_path), 
           rds = read_rds(file_path),
           str_c(file_ext, ' is not supported')
    )
  }
  
  
  
  # Functie om 1 file te kunnen lezen
  process_file <- function(filename) {
    # Encoden URL
    encoded_filename <- URLencode(filename)
    
    # Defineer URL
    url <- str_c(repos, encoded_filename)
    
    # GET request
    if (!is.null(token)) {
      request <- GET(url, add_headers(Authorization = paste("token", token)))
    } else {
      request <- GET(url)
    }
    
    # Opslaan in een tijdelijke folder
    temp_file <- tempfile(fileext = str_c(".", sub(".*\\.", "", filename)))
    writeBin(content(request, "raw"), temp_file)
    
    # Inlezen van de tijdelijk opgeslagen file
    return(read_common_file(temp_file))
  }
  
  # Map the process file functie op de filenames argument
  if(invisible) {
    results <- map(filenames, process_file) %>% invisible() %>% suppressMessages()
  } else {
    results <- map(filenames, process_file)
  }
  
  names(results) <- filenames
  
  # Unlisten alse er maar 1 file is
  if (length(filenames) == 1) {
    return(results[[1]])
  } else {
    return(results)
  }
}


# prestatie tijden

df_2022_vr <- load_git_files(repos, 'df_2022_vr.rds')
df_2022_prov <- load_git_files(repos, 'df_2022_prov.rds')

# tooltips

stand_vr <- load_git_files(repos, 'stand_vr.rds')
stand_pv <- load_git_files(repos, 'stand_pv.rds')


# referenties tussen provincies en veiligheidsregios

ref_reg <- load_git_files(repos, 'ref_reg.rds')



#regio geo data
PV_ref <- load_git_files(repos, 'PV_ref_data.rds') %>% select(-VR_naam) %>% distinct 
VR_ref <- load_git_files(repos, 'VR_ref.rds')

#inwoners per VR in 2022 voor weighted means van veiligheidsregio -> provincie

inwoners <- load_git_files(repos, 'inwoners.rds')%>%
  mutate(VR_naam = replace(VR_naam, VR_naam == "FryslÃ¢n", "Fryslân")) 


#extra statistieken

RAZ_2 <- load_git_files(repos, 'RAZ_2.rds')

df_inzetten_type_2022 <- load_git_files(repos, 'df_inzetten_type_2022.rds')
# standplaatsen data 2022
# standplaatsen data 2022
df_standplaatsen <- load_git_files(repos, 'df_standplaatsen.rds')



# inzetten tussen regios

inter_regio <- load_git_files(repos, 'interregio_inz.rds')






ui <- bs4DashPage(
  title = "Data Analytics Portfolio",
  header = bs4DashNavbar(
    title = "Data Analytics Portfolio",
    status = "primary",
    skin = "dark",
    navbarMenu(
      id = "navmenu",
      navbarTab(tabName = "home", text = "Home"),
      navbarTab(tabName = "projects", text = "Projects"),
      navbarTab(tabName = 'skills', text = 'Skills'),
      navbarTab(tabName =  'resume', text = 'CV'),
      navbarTab(tabName = 'contact', text = 'Contact'))
  ),
  sidebar = bs4DashSidebar(
    disable = FALSE,
    skin = "dark",
    brandColor = "primary",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Home", tabName = "home", icon = icon("home")),
      bs4SidebarMenuItem("Projects", tabName = "projects", icon = icon("briefcase")),
      bs4SidebarMenuItem("Skills", tabName = "skills", icon = icon("tools")),
      bs4SidebarMenuItem("Resume", tabName = "resume", icon = icon("file")),
      bs4SidebarMenuItem("Contact", tabName = "contact", icon = icon("envelope"))
    )
  ),
  body = bs4DashBody(
    
    tags$style(HTML("
  .dropdown-menu li a {
    text-align: center;
  }
")),
    useShinyjs(),  # Initialize shinyjs to enable JS functions
    bs4TabItems(
      bs4TabItem(tabName = "home",
                 fluidRow(
                   # Introduction and Welcome Section
                   box(
                     title = "Welcome to My Data Analytics Portfolio",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     fluidRow(
                       column(
                         width = 4,
                         tags$img(
                           src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/kantoorfoto2_opt.jpg",
                           height = "300px",
                           style = "display: block; margin-left: auto; margin-right: auto;"
                         )
                       ),
                       column(
                         width = 8,
                         tags$div(
                           tags$p("Welcome, I'm Andy Sibov, a passionate Data Analyst with expertise in transforming raw data into meaningful insights. 
                My portfolio showcases my journey through various projects and my diverse skill set in the world of data analytics."),
                           tags$p("In this portfolio (made in R Shiny), you'll find interactive projects, dashboards, and examples of my work with R. 
                Please feel free to explore the sections on projects, skills, resume, and contact information."),
                           tags$strong("Let's dive in!")
                         )
                       )
                     )
                   ),
                   
                   # Project, Skills, and Contact Highlight Section
                   fluidRow(
                     column(4, 
                            box(title = "Project Highlights", status = "primary", solidHeader = TRUE, width = 12,
                                tags$p("Explore my key projects, where I applied advanced analytics, and data visualization techniques."),
                                tags$ul(
                                  tags$li("Project 1: Data-driven insights for Ambulance Response Times."),
                                  tags$li("Project 2: Predictive Modeling for Customer Retention."),
                                  tags$li("Project 3: Interactive Data Visualization Dashboards.")
                                ),
                                actionButton("see_projects", "See All Projects", class = "btn-primary", icon = icon("briefcase"))  # See All Projects Button
                            )
                     ),
                     column(4, 
                            box(
                              title = "Skills Overview",
                              status = "primary",
                              solidHeader = TRUE,
                              width = 12,
                              tags$p("Here are some of the tools and technologies I work with:"),
                              tags$div(
                                style = "display: flex; flex-wrap: wrap; justify-content: space-around;",
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/R_Project_logo.jpeg", height = "60px")
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/Python_logo.png", height = "60px")
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/Excel_logo.png", height = "40px")
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/sql.png", height = "35px")
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/Github_logo.png", height = "50px"),
                                  tags$p('GitHub')
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/agile.png", height = "50px"),
                                  tags$p('Agile')
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/jira.png", height = "50px"),
                                  tags$p('Jira')
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/Presentation_logo.png", height = "40px"),
                                  tags$p('Presenting')
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/Automate.png", height = "55px"),
                                  tags$p('Automating')
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/Data_analysis.jpg", height = "40px"),
                                  tags$p('Analysis')
                                ),
                                tags$div(
                                  style = "margin: 15px; text-align: center;",
                                  tags$img(src = "https://raw.githubusercontent.com/AndySibov/portfolio/main/images/pred_model.png", height = "40px"),
                                  tags$p('Modeling')
                                )
                                # Add more skill divs as needed
                              ),
                              actionButton("see_skills", "See My Skills", class = "btn-primary", icon = icon("tools"))
                            )
                     ),
                     column(4, 
                            box(title = "Contact Me", status = "primary", solidHeader = TRUE, width = 12,
                                tags$p("Let's work together! Reach out to discuss potential collaborations or to learn more about my work."),
                                tags$p("I'm available for freelance projects, consulting, or full-time roles."),
                                actionButton("see_contact", "Get in Touch", class = "btn-primary", icon = icon("envelope"))
                            )
                     )
                   )
                   # more elements
                 )
      ),
      bs4TabItem(tabName = "projects",
                 fluidRow(
                   column(12,
                          box(
                            title = "Projects Display", status = "primary", solidHeader = TRUE, width = 12,
                            tabsetPanel(
                              tabPanel("Ambulancezorg",
                                       fluidRow(
                                         # Left Column: Filters
                                         column(3,  # Adjusted width from 2 to 3
                                                box(title = "Responsetijden", status = "primary", solidHeader = TRUE, width = 12,
                                                    # Removed tabsetPanel and placed "Prestatie Ambulancezorg" content directly
                                                    pickerInput("regio", "Select Regio:",
                                                                choices = c('veiligheidsregio', 'provincie'),  
                                                                selected = 'provincie',
                                                                multiple = FALSE,
                                                                options = list(`actions-box` = TRUE)
                                                    ),
                                                    pickerInput("soort", "Select Soort:",
                                                                choices = unique(df_2022_vr$soort),
                                                                selected = 'Aanrijden',
                                                                multiple = FALSE,
                                                                options = list(`actions-box` = TRUE)
                                                    ),
                                                    pickerInput("urgentie", "Select Urgentie:",
                                                                choices = unique(df_2022_vr$urgentie),
                                                                selected = 'A1',
                                                                multiple = TRUE
                                                    ),
                                                    pickerInput("indeling_urgentie", "Indeling Urgentie:",
                                                                choices = unique(df_2022_vr$indeling_urgentie),
                                                                selected = 'alle inzetten',
                                                                multiple = TRUE
                                                    ),
                                                    actionButton("apply_filters", "Apply Filters", class = "btn-primary")
                                                ),
                                                # Second box in the left column
                                                box(title = "Inzetten, klachten, ambulances, standplaatsen", status = "primary", solidHeader = TRUE, width = 12,
                                                    # Adjusted input IDs to avoid duplicates
                                                    pickerInput("regio2", "Select Regio:",
                                                                choices = c('veiligheidsregio', 'provincie'),  
                                                                selected = 'provincie',
                                                                multiple = FALSE,
                                                                options = list(`actions-box` = TRUE)
                                                    ),
                                                    pickerInput(
                                                      inputId = "dynamic_picker",
                                                      label = "Sub regio:",
                                                      choices = NULL,  # Initialize with no choices
                                                      selected = NULL,
                                                      multiple = TRUE,
                                                      options = list(`actions-box` = TRUE)
                                                    ),
                                                    div(id = 'ggz_toggle',
                                                        pickerInput("ggz_filter", "GGZ Standplaatsen:",
                                                                    choices =  c('(met) ggz', 'zonder ggz'),
                                                                    selected =  '(met) ggz',
                                                                    multiple = TRUE,
                                                                    options = list(`actions-box` = TRUE)
                                                        )),
                                                    div(id = 'type_toggle',
                                                        pickerInput("type_filter", "Type Standplaatsen:",
                                                                    choices = c("24 uur", "dagpost","dag/avond","DAM/VWS"),
                                                                    selected = '24 uur',
                                                                    multiple = TRUE,
                                                                    options = list(`actions-box` = TRUE)
                                                        )),
                                                    pickerInput("fill_selection", "Meting:",
                                                                choices = c("inwoners", "inzetten per 1k", 
                                                                            "klachten per 1k", "uren per 1k",
                                                                            "aantal ambulances", "aantal inzetten", "aantal klachten", 
                                                                            "ambulance per 100k", "beschikbare uren", 'klachten per 1k inzetten','leeg')%>%sort,
                                                                selected = 'klachten per 1k inzetten',
                                                                multiple = FALSE,
                                                                options = list(`actions-box` = TRUE)
                                                    ),
                                                    actionButton("apply_filters2", "Apply Filters", class = "btn-primary"),
                                                    actionButton("switch_button1", "Toon Standplaatsen", class = "btn-primary")
                                                )
                                         ),
                                         # Middle Column: Plots
                                         column(6,  # Adjusted width from 8 to 6
                                                box(title = "Prestaties Ambulancezorg", status = "primary", solidHeader = TRUE, width = 12,
                                                    girafeOutput("dataPlot", height = "500px") %>% addSpinner(),
                                                    downloadButton("downloadPlot", "Download Plot")
                                                ),
                                                # Second plot
                                                box(title = "Additionele statistieken & standplaatsen", status = "primary", solidHeader = TRUE, width = 12,
                                                    girafeOutput("dataPlot2", height = "500px") %>% addSpinner(),
                                                    downloadButton("downloadPlot2", "Download Plot"),
                                                    actionButton("switch_button2", "Toon Standplaatsen", class = "btn-primary")
                                                )
                                         ),
                                         # Right Column: Tune Color Scale Filters
                                         column(3,  # New column for the "Tune Color Scale" filters
                                                box(title = "Tune Color Scale", status = "primary", solidHeader = TRUE, width = 12,
                                                    h6("Select Theme"),
                                                    pickerInput('color_scheme', 'Select color theme:',
                                                                choices = c('Traffic White', 'Ocean', 'Heat', 'Earth', 'Charcoal', 'Viridis'),
                                                                selected = 'Traffic White',
                                                                multiple = FALSE
                                                    ),
                                                    h6("Schaal Limiet:"),
                                                    fluidRow(
                                                      column(6, numericInput("min_limit",
                                                                             label =  div("Minimum:", style = "color: gray; font-size: 12px; text-align: center;"),
                                                                             value = NULL,
                                                                             step = 1
                                                      )),
                                                      column(6, numericInput("max_limit",
                                                                             label =  div("Maximum:", style = "color: gray; font-size: 12px; text-align: center;"),
                                                                             value = NULL,
                                                                             step = 1
                                                      ))
                                                    ),
                                                    sliderInput("lighten_slider", "Tint:",
                                                                min = 0, max = .8, value = 0.25, step = 0.01, ticks = F
                                                    )
                                                )
                                         )
                                       ) 
                              )
                              # ... other project tabs ...
                            )
                          )
                   )
                 )
      )
      ,
      bs4TabItem(tabName = "skills",
                 fluidRow(
                   box(title = "Skills", status = "primary", solidHeader = TRUE, width = 12,
                       "List of skills with proficiency levels."
                   )
                 )
      ),
      bs4TabItem(tabName = "resume",
                 fluidRow(
                   box(title = "Resume", status = "primary", solidHeader = TRUE, width = 12,
                       "Link or downloadable version of your resume."
                   )
                 )
      ),
      bs4TabItem(tabName = "contact",
                 fluidRow(
                   box(title = "Contact", status = "primary", solidHeader = TRUE, width = 12,
                       "Form or contact details."
                   )
                 )
      )
    )
  ),
  footer = bs4DashFooter(
    "My Portfolio | © 2024"
  )
)




server <- function(input, output, session) {
  
  # Enable shinyjs for the app
  shinyjs::useShinyjs()
  
  
  # link action buttons on home page to tabs  # Observe Events for Home Page Action Buttons
  observeEvent(input$see_projects, {
    updateTabsetPanel(session, "navmenu", selected = "projects")})
  
  observeEvent(input$see_skills, {
    updateTabsetPanel(session, "navmenu", selected = "skills")})
  
  
  observeEvent(input$see_contact, {
    updateTabsetPanel(session, "navmenu", selected = "contact")})
  
  
  
  
  # plot 1
  {
    
    
    # Update inputs for the first set of filters
    observe({
      
      updatePickerInput(session, "soort",
                        choices = unique(df_2022_vr$soort),
                        selected = 'Respons')
      
      updatePickerInput(session, "urgentie",
                        choices = unique(df_2022_vr$urgentie),
                        selected = 'A1')
      
      updatePickerInput(session, "indeling_urgentie",
                        choices = unique(df_2022_vr$indeling_urgentie),
                        selected = 'alle inzetten')
      
    })
    
    
    
    
    # Reactive expression for the first plot's filtered data
    filteredData <- eventReactive(input$apply_filters, {
      req(input$soort, input$urgentie, input$indeling_urgentie)
      
      # selector function
      select_region <- function(region) {
        switch (region,
                'veiligheidsregio' = list(regio = 'VR_naam', 
                                          locaties = 'VR_ref', 
                                          geometry = 'geo_vr', 
                                          sub_regio = unique(ref_reg%>%pluck('VR_naam')),
                                          standplaatsen = 'stand_vr',
                                          df1 = 'df_2022_vr'),
                'provincie' = list(regio = 'Provincie', 
                                   locaties = 'PV_ref', 
                                   geometry = 'geo_pv',
                                   sub_regio = unique(ref_reg%>%pluck('Provincie')),
                                   standplaatsen = 'stand_pv',
                                   df1 = 'df_2022_prov')
        )
      }
      
      regio <- select_region(input$regio)
      
      
      
      filteredData <- get(regio[['df1']]) %>%
        filter(soort == input$soort,
               urgentie %in% input$urgentie,
               indeling_urgentie %in% input$indeling_urgentie) 
      
      
    })
    
    values1 <- reactiveValues(
      applied_regio = NULL,
      label_urgentie = ''
      
    )
    
    
    observeEvent(input$apply_filters, {
      values1$applied_regio <- input$regio
      values1$label_urgentie <- input$indeling_urgentie
    })
    
    
    
    
    # Reactive expression for the first plot
    plot_reactive <- reactive({
      data <- filteredData()
      
      
      # set regio parameters
      select_region <- function(region) {
        switch (region,
                'veiligheidsregio' = list(regio = 'VR_naam', 
                                          locaties = 'VR_ref', 
                                          geometry = 'geo_vr', 
                                          sub_regio = unique(ref_reg%>%pluck('VR_naam')),
                                          standplaatsen = 'stand_vr',
                                          df1 = 'df_2022_vr'),
                'provincie' = list(regio = 'Provincie', 
                                   locaties = 'PV_ref', 
                                   geometry = 'geo_pv',
                                   sub_regio = unique(ref_reg%>%pluck('Provincie')),
                                   standplaatsen = 'stand_pv',
                                   df1 = 'df_2022_prov')
        )
      }
      regio <- select_region(values1$applied_regio)
      
      RColorBrewer::display.brewer.all()
      
      # Custom palette for the plot
      color_schemes <- list(
        `Traffic White` =
          c("#00441B", "#006D2C", "#238B45", "#41AB5D", "#74C476", "#C7E9C0", "#E5F5E0", "#F7FCF5", "#FFFFBF", "#FFFF00", "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000"),
        Ocean = RColorBrewer::brewer.pal(9, 'Blues'),
        Heat = RColorBrewer::brewer.pal(9, 'YlOrRd'),
        Earth = RColorBrewer::brewer.pal(9, 'BrBG')%>%rev,
        Charcoal = c('beige','gray10'),
        Viridis = viridis::viridis(11, direction = -1)
      )
      
      # Adjust the color palette based on user input
      color_scheme_tuned <- colorspace::lighten(color_schemes[[input$color_scheme]], amount = input$lighten_slider)
      
      # Set color scale limits
      plot_limits <- c(input$min_limit, input$max_limit)
      if (is.null(input$min_limit) || is.null(input$max_limit)) {
        plot_limits <- NULL
      }
      
      # Create the plot
      plot1_gf <- ggplot(get(regio[['locaties']]) %>%left_join(data)) +
        geom_sf_interactive(aes(fill = tijd/60, geometry = !!sym(regio[['geometry']]), tooltip = tooltip), color = "black") +
        scale_fill_gradientn(colors = color_scheme_tuned,
                             limits = plot_limits) +
        facet_grid(rows = vars(fct(urgentie)),
                   cols = vars(indeling_urgentie), switch = 'y') +
        labs(title = "Prestaties Ambulancezorg", fill = 'Minuten', subtitle = glue('{str_c(sort(values1$label_urgentie), collapse = ", ")}')) +
        theme_void() +
        theme(text = element_text(color = "gray20", family = 'serif'),
              plot.title = element_text(hjust = 0, size = 16, margin = margin(b = 10)),
              strip.text = element_text(hjust = 0.5, size = 10),
              legend.position = 'top',
              plot.subtitle = element_text(face = "italic", size = 8, margin = margin(b = 20))) +
        guides(fill = guide_colorbar(barwidth = unit(0.8, "npc"),
                                     barheight = unit(.01, 'npc'),
                                     title.position = 'top',
                                     frame.colour = 'black',
                                     title.hjust = 0.5))
      
      plot1_gf
    })
    
    # Render the first plot
    output$dataPlot <- renderGirafe({
      girafe(ggobj = plot_reactive(),
             options = list(
               opts_toolbar(saveaspng = FALSE)
             ))
    })
    
    # Download handler for the first plot
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("ambulance_plot", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_reactive(), device = "png", dpi = 300, width = 10, height = 8)
      }
    )
    
  }
  
  
  #second plot
  
  
  
  
  
  
  observeEvent(input$regio2,{
    
    new_choices <- if_else(input$regio2 == 'provincie', list(unique(ref_reg %>% pluck('Provincie'))), 
                           list(unique(ref_reg %>% pluck('VR_naam')))) %>% unlist
    
    updatePickerInput(
      session = session,
      inputId = "dynamic_picker",
      choices = new_choices,
      selected = new_choices  # Or use valid_selection to preserve selections
    )
  })
  
  # At the top of your server function
  values <- reactiveValues(
    applied_regio = NULL,
    applied_type_filter = NULL,
    applied_ggz_filter = NULL,
    applied_dynamic_picker = NULL,
    applied_fill_selection = NULL
  )
  
  
  observeEvent(input$apply_filters2, {
    
    list_regions <- if_else(input$regio2 == 'provincie', list(unique(ref_reg %>% pluck('Provincie'))),
                            list(unique(ref_reg %>% pluck('VR_naam')))) %>% unlist
    
    
    updatePickerInput(
      session = session,
      inputId = "dynamic_picker",
      choices = list_regions,
      selected = input$dynamic_picker 
    )
    
    
    # Store the applied filters
    values$applied_regio <- input$regio2
    values$applied_type_filter <- input$type_filter
    values$applied_ggz_filter <- input$ggz_filter
    values$applied_dynamic_picker <- input$dynamic_picker
    values$applied_fill <- input$fill_selection
    
    
    
    updatePickerInput(session, "regio2", "Select Regio:",
                      choices = c('veiligheidsregio', 'provincie'),
                      selected = input$regio2)
    
    
    updatePickerInput(session,"ggz_filter",
                      choices =  c('(met) ggz', 'zonder ggz'),
                      selected = input$ggz_filter)
    
    updatePickerInput(session,"type_filter",
                      choices = c("24 uur", "dagpost","dag/avond","DAM/VWS"),
                      selected = input$type_filter)
    
    updatePickerInput(session,"fill_selection",
                      choices = c("inwoners", "inzetten per 1k", 
                                  "klachten per 1k", "uren per 1k",
                                  "aantal ambulances", "aantal inzetten", "aantal klachten", 
                                  "ambulance per 100k", "beschikbare uren", 'klachten per 1k inzetten',
                                  'leeg')%>%sort,
                      selected = input$fill_selection)
    
    # 
  })
  
  
  
  # Reactive expression for the second plot's filtered data
  filteredData2 <- eventReactive(input$apply_filters2, {
    
    #req(values$applied_regio, values$ggz_filter, values$applied_type_filter, values$applied_dynamic_picker)
    
    select_region <- function(region) {
      switch (region,
              'veiligheidsregio' = list('VR_naam', 
                                        'VR_ref', 
                                        'geo_vr', 
                                        unique(ref_reg%>%pluck('VR_naam')),
                                        'stand_vr'),
              'provincie' = list('Provincie', 
                                 'PV_ref', 
                                 'geo_pv',
                                 unique(ref_reg%>%pluck('Provincie')),
                                 'stand_pv')
      )
    }
    
    # set variables
    regio <- select_region(values$applied_regio)
    names(regio) <- c('regio', 'locaties', 'geometry', 'sub_regio', 'standplaatsen')
    
    # #test data
    # regio <- select_region('provincie')
    # names(regio) <- c('regio', 'locaties', 'geometry', 'sub_regio', 'standplaatsen')
    
    
    
    df <- RAZ_2 %>%
      mutate(klachten_pd_inz = round(n_klachten/(n_inzetten/1000),2))%>%
      # switch voor regio
      {if(regio[["regio"]] == 'Provincie'){
        group_by(., Provincie) %>%
          #weighted means berekenen
          summarise(`beschikbare uren` = sum(`beschikbare uren`),
                    n_ambu = sum(n_ambu),
                    n_inzetten = sum(n_inzetten),
                    inwoners = sum(inwoners),
                    n_klachten = sum(n_klachten))}else{.}}%>%
      mutate(`uren per 1k` = round(`beschikbare uren`/(inwoners/1000)), 
             `ambulance per 100k` = round(n_ambu/(inwoners/100000),2),
             `inzetten per 1k` = round(n_inzetten/ (inwoners/1000)),
             klachten_pd = round(n_klachten/ (inwoners/1000),2),
             klachten_pd_inz = round(n_klachten/(n_inzetten/1000),2),
             leeg = 0) %>%
      rename(`klachten per 1k` = "klachten_pd",
             `klachten per 1k inzetten` = "klachten_pd_inz",
             `aantal klachten` = 'n_klachten',
             `aantal ambulances` = 'n_ambu',
             `aantal inzetten` = 'n_inzetten') 
    
    left_join(get(regio[['standplaatsen']]),df)
    
    
    
    
  })
  
  # set initial value for the switch and hide related inputs
  standplaatsen_switch <- FALSE
  hide(id = 'ggz_toggle')
  hide(id = 'type_toggle')
  
  # reset de standplaatsen nog niet
  observeEvent(input$switch_button1, {
    
    standplaatsen_switch <<- !standplaatsen_switch
    
    # Update button label
    if (standplaatsen_switch) {
      updateActionButton(session, "switch_button1", label = "Verberg Standplaatsen")
      updateActionButton(session, "switch_button2", label = "Verberg Standplaatsen")
      showElement(id = 'ggz_toggle')
      showElement(id = 'type_toggle')
      click('apply_filters2')
    } else {
      updateActionButton(session, "switch_button1", label = "Toon Standplaatsen")
      updateActionButton(session, "switch_button2", label = "Toon Standplaatsen")
      hide(id = 'ggz_toggle')
      hide(id = 'type_toggle')
      values$applied_ggz_filter <<- NULL
      values$applied_type_filter <<- NULL
    }
  })
  
  
  observeEvent(input$switch_button2, {
    
    standplaatsen_switch <<- !standplaatsen_switch
    
    # Update button label
    if (standplaatsen_switch) {
      updateActionButton(session, "switch_button1", label = "Verberg Standplaatsen")
      updateActionButton(session, "switch_button2", label = "Verberg Standplaatsen")
      showElement(id = 'ggz_toggle')
      showElement(id = 'type_toggle')
      click('apply_filters2')
    } else {
      updateActionButton(session, "switch_button1", label = "Toon Standplaatsen")
      updateActionButton(session, "switch_button2", label = "Toon Standplaatsen")
      hide(id = 'ggz_toggle')
      hide(id = 'type_toggle')
      values$applied_ggz_filter <<- NULL
      values$applied_type_filter <<- NULL
    }
  })
  
  
  
  
  
  # Reactive expression for the second plot
  plot_reactive2 <- reactive({
    data_standplaatsen <- filteredData2()
    
    
    select_region <- function(region) {
      switch (region,
              'veiligheidsregio' = list('VR_naam',
                                        'VR_ref',
                                        'geo_vr',
                                        unique(ref_reg%>%pluck('VR_naam')),
                                        'stand_vr'),
              'provincie' = list('Provincie',
                                 'PV_ref',
                                 'geo_pv',
                                 unique(ref_reg%>%pluck('Provincie')),
                                 'stand_pv')
      )
    }
    
    # set variables
    regio <- select_region(values$applied_regio)
    names(regio) <- c('regio', 'locaties', 'geometry', 'sub_regio', 'standplaatsen')
    
    
    
    
    
    # Custom palette for the plot
    color_schemes <- list(
      `Traffic White` =
        c("#00441B", "#006D2C", "#238B45", "#41AB5D", "#74C476", "#C7E9C0", "#E5F5E0", "#F7FCF5", "#FFFFBF", "#FFFF00", "#FFCC00", "#FF9900", "#FF6600", "#FF3300", "#FF0000"),
      Ocean = RColorBrewer::brewer.pal(9, 'Blues'),
      Heat = RColorBrewer::brewer.pal(9, 'YlOrRd'),
      Earth = RColorBrewer::brewer.pal(9, 'BrBG')%>%rev,
      Charcoal = c('beige', 'gray10'),
      Viridis = viridis::viridis(11, direction = -1)
    )
    
    # Adjust the color palette based on user input
    color_scheme_tuned <- colorspace::lighten(color_schemes[[input$color_scheme]], amount = input$lighten_slider)
    
    labels_plot <- switch(values$applied_fill,
                          "inwoners" = 'aantal inwoners per regio',
                          "inzetten per 1k" = 'aantal inzetten per regio (per 1.000 inwoners)', 
                          "klachten per 1k" = 'aantal klachten per regio (per 1.000 inwoners)',
                          "klachten per 1k inzet" = 'aantal klachten per regio (per 1.000 inzetten)',
                          "uren per 1k" = 'aantal beschikbare uren per regio (per 1.000 inwoners)',
                          "aantal ambulances" = 'aantal ambulances per regio', 
                          "aantal inzetten" = 'aantal inzetten per regio', 
                          "aantal klachten" = 'aantal klachten per regio', 
                          "ambulance per 100k" = 'aantal ambulances per regio (per 100.000 inwoners)', 
                          "beschikbare uren" = 'aantal beschikbare uren per regio',
                          'leeg' = '',
                          'aantal klachten per 1k inzetten' = 'aantal klachten per regio (per 1.000 inzetten)')
    
    
    # overwrite values if toggle is on
    if(!standplaatsen_switch){
      values$applied_ggz_filter <- NULL
      values$applied_type_filter <- NULL
    }
    
    
    
    # # adapt tooltip in accordance with the location switch
    if(!standplaatsen_switch){
      data_standplaatsen <-
        data_standplaatsen %>%
        mutate(
          tooltip =
            glue('<b/>{eval_tidy(sym(regio[["regio"]]))}<b/>\n{values$applied_fill}: {eval_tidy(sym(values$applied_fill))}')
        )
    }
    
    if(!standplaatsen_switch & values$applied_fill == 'leeg') data_standplaatsen <- data_standplaatsen %>% mutate(tooltip = glue('<b/>{eval_tidy(sym(regio[["regio"]]))}<b/>'))
    
    
    
    
    
    plot2_gf <- 
      
      
      # render the plot based on some logic
      if(length(values$applied_dynamic_picker) < length(regio[['sub_regio']])){
        
        
        
        
        # get legend
        g_with_legend <- ggplot(data = data_standplaatsen) +
          geom_sf(aes(geometry = get(regio[['geometry']]), fill = !!sym(values$applied_fill)), show.legend = ifelse(values$applied_fill == 'leeg', FALSE, TRUE)) +
          scale_fill_gradientn(colors = color_scheme_tuned, limits = range(data_standplaatsen %>% pull(!!sym(values$applied_fill))))+
          
          geom_sf_interactive(data = df_standplaatsen %>% 
                                filter(ggz_crisis %in% values$applied_ggz_filter, 
                                       type %in% values$applied_type_filter), 
                              aes(geometry = points, shape = type, tooltip = tooltip), size = 3) +
          scale_shape_manual(
            values = c(
              "24 uur" = 1,      
              "dagpost" = 17,     
              "dag/avond" = 19,   
              "DAM/VWS" = 6      
            )
          ) +
          theme_void() +
          theme(legend.position = 'top',
                legend.box = "vertical") +
          guides(shape = guide_legend(order = 1, nrow = 1,
                                      override.aes = list(fill = 'gray95')),
                 fill = guide_colorbar(barwidth = unit(0.8, "npc"),
                                       barheight = unit(.01, 'npc'),
                                       title.position = 'top',
                                       frame.colour = 'black',
                                       title.hjust = 0.5,
                                       order = 2,
                                       nrow = 1))+
          labs(fill = glue('{labels_plot}'))
        
        
        
        legend <- cowplot::get_plot_component(g_with_legend, 'guide-box-top', return_all = TRUE)
        
        
        
        
        
        g <- purrr::map2(values$applied_dynamic_picker, 
                         values$applied_fill, 
                         function(x, applied_fill_var) {
                           #calculate range for color mapping
                           
                           min_max <- data_standplaatsen %>%
                             pluck(values$applied_fill) %>% range
                           
                           
                           # Filter data for the current facet
                           
                           filtered_data <- data_standplaatsen %>%
                             filter(!!sym(regio[['regio']]) %in% x)
                           
                           
                           
                           # Extract unique fill values for caption
                           fill_value <- unique(filtered_data[[applied_fill_var]])
                           
                           # Construct the caption based on the fill aesthetic
                           ggpl <- ggplot(data = filtered_data) + 
                             geom_sf_interactive(aes(geometry = get(regio[['geometry']]), fill = !!sym(applied_fill_var), tooltip = tooltip), show.legend = F) 
                           
                           if(standplaatsen_switch){
                             
                             ggpl <- ggpl + geom_sf_interactive(data = df_standplaatsen %>% 
                                                                  filter(ggz_crisis %in% values$applied_ggz_filter, 
                                                                         type %in% values$applied_type_filter,
                                                                         !!sym(regio[['regio']]) %in% x), 
                                                                aes(geometry = points, shape = type, tooltip = adres), alpha = .75, size = 2, show.legend = F)
                           }
                           
                           ggpl + scale_shape_manual(
                             values = c(
                               "24 uur" = 1,      
                               "dagpost" = 17,     
                               "dag/avond" = 19,   
                               "DAM/VWS" = 6      
                             )
                           ) +
                             theme_void() +
                             theme(legend.position = 'top',
                                   legend.box = "vertical") +
                             labs(title = x, 
                                  
                                  subtitle = if (applied_fill_var == 'leeg') {
                                    ''
                                  } else {
                                    glue("{applied_fill_var}: {fill_value}")
                                  },
                                  show.legend = ifelse(values$applied_fill == 'leeg', FALSE, TRUE)) +
                             scale_fill_gradientn(colors = color_scheme_tuned,
                                                  limits = min_max) +
                             theme(text = element_text(color = "gray20", family = 'serif'),
                                   plot.title = element_text(hjust = 0.5, size = 16-((length(values$applied_dynamic_picker))%/%3)*1.5),
                                   strip.text = element_text(hjust = 0.5, size = 10-((length(values$applied_dynamic_picker))%/%3)*1.3),
                                   plot.subtitle = element_text(face = "italic", size = 12-((length(values$applied_dynamic_picker))%/%3)*1, hjust = .5)) 
                           
                         })
        
        cowplot::plot_grid(
          legend,  # Plot with the legend
          cowplot::plot_grid(plotlist = g),  # The grid of the actual plots
          ncol = 1,  # Stack legend plot on top
          rel_heights = c(0.2, 0.8)  # Adjust the heights, 0.1 for legend and 0.9 for the plots
        )
        
        
      } else{
        #else plot whole of NL
        
        
        ggplot(data = data_standplaatsen) + 
          geom_sf_interactive(aes(geometry = get(regio[['geometry']]), fill = !!sym(values$applied_fill),
                                  tooltip = tooltip, data_id = !!sym(regio[['regio']])), show.legend = ifelse(values$applied_fill == 'leeg', FALSE, TRUE))+
          geom_sf_interactive(data = df_standplaatsen %>% 
                                filter(ggz_crisis %in% values$applied_ggz_filter, 
                                       type %in% values$applied_type_filter), 
                              aes(geometry = points, shape = type, tooltip = tooltip, data_id = type), alpha = .65, size = 1.75) +
          labs(title = 'Ambulance zorg', subtitle = glue('{labels_plot}')) +
          scale_fill_gradientn(colors = color_scheme_tuned) +
          scale_shape_manual(
            values = c(
              "24 uur" = 1,      
              "dagpost" = 17,     
              "dag/avond" = 19,   
              "DAM/VWS" = 6      
            )
          ) +
          theme_void() +
          theme(legend.position = 'top',
                legend.box = "vertical") +
          theme(text = element_text(color = "gray20", family = 'serif'),
                plot.title = element_text(hjust = 0.5, size = 16),
                strip.text = element_text(hjust = 0.5, size = 10),
                plot.subtitle = element_text(face = "italic", size = 12, hjust = .5))+
          guides(shape = guide_legend(order = 1, nrow = 1),
                 fill = guide_colorbar(barwidth = unit(0.8, "npc"),
                                       barheight = unit(.01, 'npc'),
                                       title.position = 'top',
                                       frame.colour = 'black',
                                       title.hjust = 0.5,
                                       order = 2,
                                       nrow = 1))
        
        
      }
    
    plot2_gf
    
    
    
    
  })
  
  
  
  
  
  
  
  
  # Render the second plot
  output$dataPlot2 <- renderGirafe({
    girafe(ggobj = plot_reactive2())%>%
      girafe_options(.,
                     opts_tooltip(
                       css = "background-color: white; color: none; padding: 5px; border: black;",
                       offx = 20, 
                       offy = 20,
                       opacity = .8
                     ),
                     opts_toolbar(hidden = c('lasso_select', 'lasso_deselect', 'saveaspng')),
                     opts_zoom(max = 6),
                     opts_hover(css = "fill:orange;stroke:black;")
      )
  })
  
  # Download handler for the second plot
  output$downloadPlot2 <- downloadHandler(
    filename = function() {
      paste("second_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive2(), device = "png", dpi = 300, width = 10, height = 8)
    }
  )
  
  
  
  
  # Observe when the "Projects" tab is clicked and trigger the "Apply Filters" button click
  observeEvent(input$sidebarItemExpanded == "projects", {
    shinyjs::click("apply_filters")  # Simulate a click on the "Apply Filters" button
  })
  
  observeEvent(input$sidebarItemExpanded == "projects", {
    shinyjs::delay(2000,shinyjs::click("apply_filters2"))# Simulate a   on the "Apply Filters" button
  })
  
  
  
}

shinyApp(ui, server)


