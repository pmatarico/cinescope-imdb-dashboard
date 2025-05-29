library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

dashboardPage(
  dashboardHeader(title = "CineScope"), 
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Directors", tabName = "directors", icon = icon("user-tie")),
      menuItem("Actors", tabName = "actors", icon = icon("users")),
      menuItem("Movies Table", tabName = "table", icon = icon("table")),
      menuItem("Recommender", tabName = "recommender", icon = icon("star")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    hr(),
    uiOutput("genre_selector"),
    uiOutput("year_selector")
  ),
  dashboardBody(
    # Logo and title
    fluidRow(
      column(width = 2, tags$img(src = "logo_put.jpg", height = "90px")),
      column(width = 10,
             tags$h1("CineScope: Explore the IMDB Top 250 Movies", 
                     style = "margin-top:25px; font-weight:bold; font-size:2.2em;")
      )
    ),
    tabItems(
      tabItem("overview",
              fluidRow(
                box(plotlyOutput("genrePlot"), width = 6),
                box(plotlyOutput("ratingsTimePlot"), width = 6)
              ),
              fluidRow(
                box(plotlyOutput("scatterPlot"), width = 6),
                box(plotlyOutput("topActorsPlot"), width = 6)
              )
      ),
      tabItem("directors",
              fluidRow(
                box(plotlyOutput("topDirectorsPlot"), width = 6),
                box(plotlyOutput("directorsRatingPlot"), width = 6)
              ),
              fluidRow(
                box(DTOutput("directorsTable"), width = 5),
                box(uiOutput("directorDetails"), width = 7)
              )
      ),
      tabItem("actors",
              fluidRow(
                box(DTOutput("actorsTable"), width = 6),
                box(uiOutput("actorMoviesDetails"), width = 6)
              )
      ),
      tabItem("table",
              fluidRow(
                box(DTOutput("moviesTable"), width = 12)
              ),
              fluidRow(
                box(uiOutput("movieDetails"), width = 12)
              )
      ),
      tabItem("recommender",
              fluidRow(
                box(
                  title = "Your Preferences", width = 4,
                  sliderInput("rec_runtime", "Maximum Duration (minutes):", min = 60, max = 240, value = 120, step = 10),
                  uiOutput("rec_year_selector"),
                  uiOutput("rec_genre_selector"),
                  sliderInput("rec_rating", "Minimum IMDB Rating:", min = 0, max = 10, value = 7, step = 0.1)
                ),
                box(
                  title = "Recommended Movies", width = 8,
                  uiOutput("rec_results_summary"),
                  DTOutput("recommenderTable"),
                  uiOutput("rec_movie_details")  
                )
              )
      ),
      tabItem("about",
              fluidRow(
                column(width = 9,
                       tags$h3("About CineScope"),
                       p("CineScope is an interactive dashboard for exploring the IMDB Top 250 Movies dataset."),
                       br(),
                       tags$h4("Dashboard Features:"),
                       tags$ul(
                         tags$li("Filter and explore movies by genre and year, with all visualizations updating instantly."),
                         tags$li("Compare directors’ performance, explore their filmography, and view summary statistics for each director."),
                         tags$li("Analyze the evolution of movie ratings over time using an interactive line chart."),
                         tags$li("Search and explore all actors in the Top 250; view all movies for each actor."),
                         tags$li("View detailed information for selected movies, including plot summary, cast, year, genre, and more."),
                         tags$li("Visualize and compare movie durations and ratings using an interactive scatter plot."),
                         tags$li("Receive personalized movie recommendations based on preferred duration, year range, genres, and rating."),
                         tags$li("Benefit from interactive linking: selecting a row or item in one component updates detailed panels or related visualizations.")
                       ),
                       br(),
                       tags$b("Data source: "),
                       a(href = "https://www.kaggle.com/datasets/rajugc/imdb-top-250-movies-dataset", 
                         "IMDB Top 250 Movies (Kaggle)", target = "_blank"),
                       br(), br(),
                       p("Created with Shiny by Paula Mata, Politechnika Poznańska.")
                )
              )
      )
      
  )
)
)


