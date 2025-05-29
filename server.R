library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(stringr)

movies <- read.csv("IMDB Top 250 Movies.csv", stringsAsFactors = FALSE)

movies$year <- as.numeric(movies$year)
movies$rating <- as.numeric(movies$rating)

shinyServer(function(input, output, session) {
  # Géneros y años disponibles dinámicamente
  output$genre_selector <- renderUI({
    all_genres <- unique(unlist(strsplit(as.character(movies$genre), ",")))
    all_genres <- sort(trimws(all_genres))
    selectInput("genre", "Select Genre:", choices = all_genres, selected = NULL, multiple = TRUE)
  })
  output$year_selector <- renderUI({
    sliderInput("year", "Select Year:", min(movies$year, na.rm=TRUE), max(movies$year, na.rm=TRUE),
                value = c(min(movies$year, na.rm=TRUE), max(movies$year, na.rm=TRUE)), sep = "")
  })
  # Inputs para año y género en el recomendador
  output$rec_year_selector <- renderUI({
    sliderInput("rec_year", "Year Range:", 
                min = min(movies$year, na.rm = TRUE), 
                max = max(movies$year, na.rm = TRUE),
                value = c(2000, 2024), sep = "")
  })
  
  output$rec_genre_selector <- renderUI({
    all_genres <- unique(unlist(strsplit(as.character(movies$genre), ",")))
    all_genres <- sort(trimws(all_genres))
    selectInput("rec_genre", "Preferred Genres:", 
                choices = all_genres, selected = NULL, multiple = TRUE)
  })
  
  output$directorMoviesRatingsPlot <- renderPlotly({
    filtered <- filtered_movies()
    all_directors <- sort(unique(trimws(unlist(strsplit(as.character(filtered$directors), ",")))))
    selected <- input$directorsTable_rows_selected
    if (length(selected) == 1 && length(all_directors) >= selected) {
      selected_director <- all_directors[selected]
      movies_dir <- filtered[sapply(filtered$directors, function(x) selected_director %in% trimws(unlist(strsplit(x, ",")))), ]
      if (nrow(movies_dir) == 0) return(NULL)
      plot_ly(
        movies_dir,
        x = ~name, y = ~rating, type = "bar"
      ) %>%
        layout(title = paste0("Ratings for movies by ", selected_director),
               xaxis = list(title = "Movie Title"),
               yaxis = list(title = "IMDB Rating"))
    } else {
      return(NULL)
    }
  })
  
  
  # Filtrado reactivo
  filtered_movies <- reactive({
    df <- movies
    # Filtrado por género
    if (!is.null(input$genre) && length(input$genre) > 0) {
      # Comprobamos, para cada película, si algún género seleccionado está en su lista de géneros
      df <- df[sapply(df$genre, function(g) {
        pelis_generos <- trimws(unlist(strsplit(g, ",")))
        any(input$genre %in% pelis_generos)
      }), ]
    }
    # Filtrado por año
    df <- df[df$year >= input$year[1] & df$year <= input$year[2], ]
    df
  })
  
  # 1. Visualización: películas por género
  output$genrePlot <- renderPlotly({
    filtered <- filtered_movies()
    genres <- unlist(strsplit(as.character(filtered$genre), ","))
    genres <- trimws(genres)
    genre_count <- as.data.frame(table(genres))
    plot_ly(genre_count, x = ~genres, y = ~Freq, type = 'bar') %>%
      layout(title = "Number of Movies per Genre", xaxis = list(title = "Genre"), yaxis = list(title = "Count"))
  })
  
  # 2. Visualización: rating medio por año
  output$ratingsTimePlot <- renderPlotly({
    filtered <- filtered_movies()
    ratings_by_year <- filtered %>%
      group_by(year) %>%
      summarise(mean_rating = mean(rating, na.rm = TRUE))
    plot_ly(ratings_by_year, x = ~year, y = ~mean_rating, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Average IMDB Rating Over Time", xaxis = list(title = "Year"), yaxis = list(title = "Mean Rating"))
  })
  
  # 3. Visualización: duración vs rating (scatter)
  output$scatterPlot <- renderPlotly({
    filtered <- filtered_movies()
    # Convertimos run_time ("2h 32m") a minutos numéricos para graficar
    time_to_min <- function(rt) {
      parts <- str_match(rt, "(\\d+)h\\s*(\\d+)?m?")
      hrs <- as.numeric(parts[,2])
      mins <- as.numeric(parts[,3])
      mins[is.na(mins)] <- 0
      total <- hrs*60 + mins
      return(total)
    }
    filtered$run_time_num <- time_to_min(filtered$run_time)
    plot_ly(filtered, x = ~run_time_num, y = ~rating, text = ~name, type = 'scatter', mode = 'markers') %>%
      layout(title = "Duration vs Rating", xaxis = list(title = "Runtime (min)"), yaxis = list(title = "Rating"))
  })
  
  # 4. Visualización: actores más frecuentes
  output$topActorsPlot <- renderPlotly({
    filtered <- filtered_movies()
    actors <- unlist(strsplit(as.character(filtered$casts), ","))
    actors <- trimws(actors)
    actor_count <- sort(table(actors), decreasing = TRUE)
    df <- head(as.data.frame(actor_count), 10)
    colnames(df) <- c("Actor", "Count")
    plot_ly(df, x = ~reorder(Actor, Count), y = ~Count, type = 'bar') %>%
      layout(title = "Top 10 Most Frequent Actors", xaxis = list(title = "Actor"), yaxis = list(title = "Count"))
  })
  
  # 5. Visualización: directores con más películas
  output$topDirectorsPlot <- renderPlotly({
    filtered <- filtered_movies()
    directors <- unlist(strsplit(as.character(filtered$directors), ","))
    directors <- trimws(directors)
    director_count <- sort(table(directors), decreasing = TRUE)
    df <- head(as.data.frame(director_count), 10)
    colnames(df) <- c("Director", "Count")
    plot_ly(df, x = ~reorder(Director, Count), y = ~Count, type = 'bar') %>%
      layout(title = "Top 10 Directors", xaxis = list(title = "Director"), yaxis = list(title = "Count"))
  })
  
  # 6. Visualización: directores con mejor rating medio (con más de una peli)
  output$directorsRatingPlot <- renderPlotly({
    filtered <- filtered_movies()
    # Esto es más complejo: calcula el rating medio de cada director
    directors <- strsplit(as.character(filtered$directors), ",")
    df_long <- data.frame(
      name = rep(filtered$name, sapply(directors, length)),
      director = unlist(directors),
      rating = rep(filtered$rating, sapply(directors, length))
    )
    df_long$director <- trimws(df_long$director)
    mean_ratings <- df_long %>%
      group_by(director) %>%
      summarise(mean_rating = mean(rating, na.rm=TRUE), n=n()) %>%
      arrange(desc(mean_rating))
    mean_ratings <- mean_ratings[mean_ratings$n > 1, ] # Solo directores con más de una peli
    df <- head(mean_ratings, 10)
    plot_ly(df, x = ~reorder(director, mean_rating), y = ~mean_rating, type = 'bar') %>%
      layout(title = "Top Directors by Average Rating", xaxis = list(title = "Director"), yaxis = list(title = "Average Rating"))
  })
  
  
  
  output$directorsTable <- renderDT({
    filtered <- filtered_movies()
    # Lista todos los directores del filtro actual (puede haber varios por película)
    all_directors <- trimws(unlist(strsplit(as.character(filtered$directors), ",")))
    director_stats <- as.data.frame(table(all_directors))
    colnames(director_stats) <- c("Director", "NumMovies")
    # Calcula la media de rating para cada director
    director_stats$MeanRating <- sapply(director_stats$Director, function(dir) {
      movies_dir <- filtered[sapply(filtered$directors, function(x) dir %in% trimws(unlist(strsplit(x, ",")))), ]
      mean(movies_dir$rating, na.rm = TRUE)
    })
    director_stats <- director_stats[order(-director_stats$NumMovies, -director_stats$MeanRating), ]
    datatable(
      director_stats,
      selection = "single",
      options = list(pageLength = 10, searching = TRUE)
    )
  })
  
  
  output$directorDetails <- renderUI({
    filtered <- filtered_movies()
    # Lista de directores del filtrado actual
    all_directors <- sort(unique(trimws(unlist(strsplit(as.character(filtered$directors), ",")))))
    selected <- input$directorsTable_rows_selected
    
    if (length(selected) == 1 && length(all_directors) >= selected) {
      selected_director <- all_directors[selected]
      # Encuentra todas las pelis de ese director en el Top 250 filtrado
      movies_dir <- filtered[sapply(filtered$directors, function(x) selected_director %in% trimws(unlist(strsplit(x, ",")))), ]
      if (nrow(movies_dir) == 0) return(p("No movies found for this director in the current filter."))
      
      # Géneros más frecuentes
      top_genres <- table(trimws(unlist(strsplit(as.character(movies_dir$genre), ","))))
      top_genres <- sort(top_genres, decreasing = TRUE)
      top_genre <- names(top_genres)[1]
      # Estadísticas
      years <- range(movies_dir$year, na.rm = TRUE)
      mean_rating <- mean(movies_dir$rating, na.rm = TRUE)
      
      tagList(
        tags$h3(selected_director),
        p(strong("Number of Movies in Top 250:"), nrow(movies_dir)),
        p(strong("Mean IMDB Rating:"), round(mean_rating, 2)),
        p(strong("Active Years:"), years[1], "-", years[2]),
        p(strong("Most Frequent Genre:"), top_genre),
        tags$h4("Movies:"),
        DT::datatable(movies_dir[, c("name", "year", "rating", "genre", "run_time")],
                      colnames = c("Title", "Year", "Rating", "Genre", "Duration"),
                      options = list(pageLength = 5)),
        tags$h4("Ratings of this director's movies:"),
        plotly::plotlyOutput("directorMoviesRatingsPlot")
      )
    } else {
      p("Select a director to see details.")
    }
  })
  
  
  
  
  
  # 7. Actors más recurrentes
  output$actorsTable <- renderDT({
    filtered <- filtered_movies()
    # Lista de actores únicos en el filtrado actual
    actors <- unique(unlist(strsplit(as.character(filtered$casts), ",")))
    actors <- sort(trimws(actors))
    datatable(data.frame(Actor = actors), selection = 'single', options = list(pageLength = 10, searching = TRUE))
  })
  
  # 8. Muestra las pelis del actor seleccionado
  output$actorMoviesDetails <- renderUI({
    selected <- input$actorsTable_rows_selected
    filtered <- filtered_movies()
    actors <- unique(unlist(strsplit(as.character(filtered$casts), ",")))
    actors <- sort(trimws(actors))
    
    if (length(selected)) {
      actor_name <- actors[selected]
      # Busca todas las pelis donde aparece ese actor
      movies_with_actor <- filtered[sapply(filtered$casts, function(g) actor_name %in% trimws(unlist(strsplit(g, ",")))), ]
      
      if (nrow(movies_with_actor) == 0) {
        return(p("No movies found for this actor in the current selection."))
      }
      tagList(
        tags$h4(paste("Movies with:", actor_name)),
        DT::datatable(movies_with_actor[, c("name", "year", "rating", "genre", "directors")],
                      colnames = c("Title", "Year", "Rating", "Genre", "Director(s)"),
                      options = list(pageLength = 5))
      )
    } else {
      return(p("Select an actor to see their movies."))
    }
  })
  
  recommender_filtered <- reactive({
    df <- movies
    # Duración a minutos numéricos
    to_minutes <- function(rt) {
      matches <- stringr::str_match(rt, "(\\d+)h\\s*(\\d+)?m?")
      h <- as.numeric(matches[,2])
      m <- as.numeric(matches[,3])
      h[is.na(h)] <- 0
      m[is.na(m)] <- 0
      h * 60 + m
    }
    df$duration_min <- to_minutes(df$run_time)
    
    # Filtrado por duración
    df <- df[df$duration_min <= input$rec_runtime, ]
    # Filtrado por años
    df <- df[df$year >= input$rec_year[1] & df$year <= input$rec_year[2], ]
    # Filtrado por rating
    df <- df[df$rating >= input$rec_rating, ]
    # Filtrado por género
    if (!is.null(input$rec_genre) && length(input$rec_genre) > 0) {
      df <- df[sapply(df$genre, function(g) {
        pelis_generos <- trimws(unlist(strsplit(g, ",")))
        any(input$rec_genre %in% pelis_generos)
      }), ]
    }
    df
  })
  
  output$rec_results_summary <- renderUI({
    df <- recommender_filtered()
    n <- nrow(df)
    if (n == 0) {
      tags$div(style = "color: red; font-weight: bold; font-size: 1.2em; margin: 10px 0;",
               "No movies found that match your criteria.")
    } else {
      tags$div(style = "font-weight: bold; font-size: 1.1em; margin: 10px 0;",
               paste("Results found:", n, ifelse(n == 1, "movie", "movies")))
    }
  })
  
  output$recommenderTable <- renderDT({
    df <- recommender_filtered()
    # Solo muestra columnas clave
    datatable(
      df[, c("name", "year", "rating", "genre", "run_time", "directors")],
      colnames = c("Title", "Year", "Rating", "Genre", "Duration", "Director(s)"),
      options = list(pageLength = 8)
    )
  })
  
  output$rec_movie_details <- renderUI({
    df <- recommender_filtered()
    selected <- input$recommenderTable_rows_selected
    if (length(selected) == 1 && nrow(df) >= selected) {
      movie <- df[selected, ]
      tagList(
        tags$h4(movie$name),
        p(strong("Year: "), movie$year),
        p(strong("Director(s): "), movie$directors),
        p(strong("Casts: "), movie$casts),
        p(strong("Genre: "), movie$genre),
        p(strong("Duration: "), movie$run_time),
        p(strong("Rating: "), movie$rating),
        p(strong("Tagline: "), movie$tagline)
      )
    }
  })
  
  
  # Tabla interactiva
  output$moviesTable <- renderDT({
    datatable(filtered_movies(), selection = 'single', options = list(pageLength = 10))
  })
  
  # Detalles de la película seleccionada
  output$movieDetails <- renderUI({
    selected <- input$moviesTable_rows_selected
    if (length(selected)) {
      movie <- filtered_movies()[selected, ]
      tagList(
        h4(movie$name),
        p(strong("Year: "), movie$year),
        p(strong("Director(s): "), movie$directors),
        p(strong("Casts: "), movie$casts),
        p(strong("Genre: "), movie$genre),
        p(strong("Runtime: "), movie$run_time),
        p(strong("Rating: "), movie$rating),
        p(strong("Tagline: "), movie$tagline)
      )
    }
  })
})


