
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(geniusR)
library(spotifyr)
library(reshape2)
library(shinyjs)
library(tidytext)
library(shinycssloaders)
library(devtools)
library(plotly) 
library(ggplot2)
library(tidytext)
library(glue)
library(stringr)

#############################

#Packages you may need to install 

#############################

#I used the dev versions for the following:
#devtools::install_github("josiahparry/geniusR")
#devtools::install_github('charlie86/spotifyr')
#devtools::install_github('hadley/ggplot2')

#For nicer features :)
#install.packages("shinyWidgets")
#install.packages("shinyjs")

#I also had troubling deploying my final shiny app with
#the genius & spotify dev versions above ^^ because they were missing 
#several dependencies, so I forked them & added them myself. 
#If using the dev versions above ^^ give errors maybe try mines, 
#but they should work if you're just running this locally. 

#devtools::install_github("kaylaipp/geniusR")
#devtools::install_github("kaylaipp/spotifyr")


#spotifyr authentication 
#Set up a Spotify dev account @ https://beta.developer.spotify.com
Sys.setenv(SPOTIFY_CLIENT_ID = '2f31c3a3fd8b4b899d9d7f50e47c7e70')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'baf8422a5f664f229a79cb4005f24772')

ui <- dashboardPage(skin = "green",
               
  dashboardHeader(title = "Lyric Analysis",
                  tags$li(a(href = 'https://github.com/kaylaipp/Lyric-Prediction',
                            icon("github"),
                            title = "Back to Apps Home"),
                          class = "dropdown")),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Search Artist", tabName = "Overview", icon = icon("search")),
      menuItem("Today's Top Songs", tabName = "Top", icon = icon("bar-chart-o")))),
  dashboardBody(
        tags$head(
          tags$style(HTML("
            .fa { font-size: 18px; }
            /* main sidebar */
            .skin-green .main-header .logo,
            .skin-green .main-header .navbar {
                          background-color: #191414;
            }
            .box.box-primary {
                border-top-color:#1db954;
            }
            div.small-box.bg-green {width: 100%;} 
            .info-box-icon {height: 45px; line-height: 45px;} 
            .info-box-content {padding-top: 0px; padding-bottom: 0px;}
            i.fa.fa-list{font-size: 50px;}
            i.fa.fa-music{font-size: 50px;}
            .col-sm-4{width: 120%;}
            .col-sm-2{left: 8%; bottom: 0vh;}
            p {font-size: 23px; }
            div.content-wrapper{height: 340vh;}
            div#bottom{width: 145%; right: 45% !important;Position:relative;}
            div.col-sm-8{top: 9vh; position: absolute; left: 31vw;}
            "))
        ),
        useShinyjs(),

    tabItems(
      # First tab content
        
      tabItem(tabName = "Overview",
          div(style="width: 60%;",
                 box(status = "primary",
                    fluidRow(
                      column(12,
                             textInput(
                               inputId = "search", label = "Search Artist on Spotify",
                               placeholder = "Search",
                               width = "500px"
                             )), align = "center"),
                    shinyjs::hidden(
                    div(id = "pickerDiv",
                    fluidRow(
                      column(12, pickerInput(inputId = "picker", label = "These artists match your search:", choices = NULL, width = "100%")),
                      align = "center"))), 
                    
                    
                    shinyjs::hidden(
                    div(id = "analyzeDiv",
                    fluidRow(
                     column(12, actionButton("analyze", "Get Albums!", width = "100px"))))), 
                    
                    shinyjs :: hidden(
                        div(
                            id = "loading-content",
                            h5("Retriving Albums...")
                            )),
                    
                    shinyjs::hidden(
                    div(id = "checkAlbumsDiv",
                        
                        fluidRow(
                         column(12, checkboxGroupInput(inputId="checkAlbums",label="",choices=character(0)))),
                         fluidRow(
                         column(12, actionButton("analyze2", "Analyze!", width = "100px")))
                    ))
              ),
                  div(id = "analysis", style = "width: 86%", box(title = "Introduction", status = "success",width=7, htmlOutput("analysisText")))
      ),
                  mainPanel(
                      shinyjs::hidden(
                        div(id = "main",
                          fluidRow(
                          box(id = "artist_info",
                            status = "success",
                            solidHeader = F,
                            collapsible = F,
                            width = 5,
                            column(width=3, align="center",
                            htmlOutput("artistpic")),
                            fluidRow(column(width=2,(htmlOutput("artist_name")),align = "right"))),
                          
                            column(width = 3,infoBoxOutput("numAlbums")),
                            column(width = 3,infoBoxOutput("numWords")),
                          
                            fluidRow(
                            box(title = "Sentiment Analysis By Song", status = "success", width = 12, column(width=12, withSpinner(plotlyOutput(outputId = "audioGraph4", width = "100%"))))),
                          
                          
                            fluidRow(
                            box(title = "Most Frequent Words", status = "success", column(width = 12, withSpinner(plotOutput(outputId = "mostUsedWords", width = "100%")))), 
                            box(title = "Popularity vs Valence",status = "success", column(width = 12, withSpinner(plotlyOutput(outputId = "audioGraph", width = "100%"))))),
                          
                            fluidRow(
                            box(title = "Popularity vs Liveness",status = "success",column(width = 12, withSpinner(plotlyOutput(outputId = "audioGraph2", width = "100%")))), 
                            box(title = "Popularity vs Dancibility ",status = "success", column(width = 12, withSpinner(plotlyOutput(outputId = "audioGraph3", width = "100%"))))
                            ), 
                          
                            fluidRow(
                            box(title = "Sentiment Analysis",status = "success",column(width = 12, withSpinner(plotOutput(outputId = "audioGraph5", width = "100%")))),
                            box(title = "Popularity vs Tempo",status = "success",column(width = 12, withSpinner(plotlyOutput(outputId = "audioGraph6", width = "100%"))))
                            ),
                          
                            fluidRow(
                            box(title = "Analysis", status = "success",width=12, textOutput("analysis"))
                            )
                          )
                    ))
                  )),
      
          # Second tab content
    tabItem(tabName = "Top",
            fluidRow(box(title = "Today's Top Hits via Spotify", status = "success",width=12, withSpinner(plotlyOutput(outputId = "topSongs", width = "100%", height = "190%")))),
            fluidRow(box(title = "2000's Top Hits via Spotify", status = "success",width=12, withSpinner(plotlyOutput(outputId = "topSongs6", width = "100%", height = "190%")))),
            fluidRow(box(title = "90's Top Hits via Spotify", status = "success",width=12, withSpinner(plotlyOutput(outputId = "topSongs3", width = "100%", height = "190%")))),
            fluidRow(box(title = "80's Top Hits via Spotify", status = "success",width=12, withSpinner(plotlyOutput(outputId = "topSongs2", width = "100%", height = "190%")))),
            fluidRow(box(title = "70's Top Hits via Spotify", status = "success",width=12, withSpinner(plotlyOutput(outputId = "topSongs4", width = "100%", height = "190%")))),
            fluidRow(box(title = "60's Top Hits via Spotify", status = "success",width=12, withSpinner(plotlyOutput(outputId = "topSongs5", width = "100%", height = "190%"))))
            )
)))


server <- function(input, output, session) {

  #function to get picked artist id & pic for url 
  get_id_picture <- reactive({
    info <- get_artists(input$search)
    info <-  as_tibble(info)
    info <- info %>%
      filter(artist_name == input$picker)
    info
  })
  
  #function to get & list of possible artists to show in picker
  artists <- reactive({
    possibleArtists <- get_artists(input$search)
    possibleArtists <-  as_tibble(possibleArtists)
    myCols <- c("artist_name")
    colNums <- match(myCols,names(possibleArtists))
    possibleArtists <- possibleArtists %>%
      select(colNums)
    possibleArtists
  })
  
  #function to update pickerInput 
  observeEvent(input$search, {
    shinyjs::show(id = "pickerDiv", animType = "slide") 
    shinyjs::show(id = "analyzeDiv")
    a = artists()
    updatePickerInput(
      session = session,
      inputId = "picker",
      choices = a)}, 
    ignoreInit = TRUE)
    
  
  #get artist album data when 'analyze' button is clicked
  albums <- eventReactive(input$analyze, {
      shinyjs::show(id = "checkAlbumsDiv", animType = "slide")
      uri = get_id_picture()[[2]]
      print(uri)
      names <- get_artist_albums(artist_uri = uri, use_artist_uri = TRUE)
      print(names)
      names <- names %>%
          select("album_name")
      names
       
  })
  output$albumtable <- renderText({
      albums()
  })
  
  #get arist picture when 'analyze' button is clicked 
  pic <- eventReactive(input$analyze2, {
      print(get_id_picture()[3])
      url = toString(get_id_picture()[3])
      url 
  })
  
  output$artistpic <- renderText({c('<img src="',pic(),'"width="200%" height="200%">')})
  
  #output albums when 'analyze' button is clicked
  observeEvent(input$analyze, {
      shinyjs::toggle("loading-content", animType = "fade")
      albums = albums()[[1]]
      updateCheckboxGroupInput(session = session, label= "Select albums to analyze", inputId = "checkAlbums", choices = albums)
      hide(id = "loading-content", anim = FALSE)   
  },ignoreInit = TRUE)
  
  artist_name <- eventReactive(input$analyze2,{
      input$picker
  })
  
  #Get artist name to appear in textbox 
  output$artist_name <- renderText({c("<p style = size=\"4\">",artist_name(),"</p>")})
  
  observeEvent(input$analyze2, {
    shinyjs::show("main")
  })
  
  #info boxes on number alumbs, songs & words 
  numAlbums <- eventReactive(input$analyze2, {
      print(input$checkAlbums)
      numAlbums = length(input$checkAlbums)
      print(numAlbums)
      numAlbums
  })
  
  #Get number of albums selected 
  output$numAlbums <- renderValueBox({
      valueBox(
          paste0(numAlbums()), "Albums", icon = icon("list"),
          color = "green"
      )
  })
  
  #gets lyrics for all songs in selected albums
  #and returns table of tokenized words 
  lyricTable <- eventReactive(input$analyze2,{
      alist = selectedAlbums()
      total_lyrics = data.frame()
      for (a in alist){
          df <- data.frame(genius_album(artist_name(), a))
          total_lyrics <- rbind(total_lyrics,df)
      }
      total_lyrics <- total_lyrics %>%
          select(lyric)
      
      total_lyrics <- total_lyrics %>%
          unnest_tokens(word, lyric) %>%  #split words
          anti_join(stop_words) %>%       # take out "a", "an", "the", etc.
          count(word, sort = TRUE) %>%    # count occurrences
      print(total_lyrics)
      total_lyrics
  })
  
  numWords <- eventReactive(input$analyze2,{
      total_words <- nrow(lyricTable())
      print(total_words)
      total_words
  })
  
  output$numWords <- renderValueBox({
      valueBox(
          paste0(numWords()), "Words Analyzed", icon = icon("music"),
          color = "green"
      )
  })
  
  #output table of word counts in songs in albums picked 
  selectedAlbums <- eventReactive(input$analyze2,{
      input$checkAlbums
  })
  #####################################
  
  #barplot of most frequent words 
  
  #####################################
  output$mostUsedWords <- renderPlot({
      total_lyrics <- head(lyricTable(),20)
      
       word_plot <- ggplot(data = total_lyrics, aes(reorder(word,n), n, fill = word)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme(legend.position="none") +
          xlab("Frequency") +
          ylab("Word")
       
       print(word_plot)
  })
  
  #output table of audio features filtered by the albums selected
  audioTable <- eventReactive(input$analyze2,{
      uri = get_id_picture()[[2]]
      audioFeatures <- get_artist_audio_features(artist_uri = uri, use_artist_uri = TRUE)
      albumsChecked <- unlist(selectedAlbums())

      #filter on only albums selected 
      audioFeatures <- filter(audioFeatures, album_name %in% albumsChecked) 
      
      #View(audioFeatures)
      audioFeatures 
  })
  
  #####################################
  
  #Popularity vs Valence graph by song 
  
  #####################################
  output$audioGraph <- renderPlotly({
      audioFeatures <- audioTable()
      
      #get popularity & valence columns 
      audioFeatures <- audioFeatures %>%
        select(track_name, valence, track_popularity, album_name)
      
      gg <- ggplot(audioFeatures, aes(x=valence, y=track_popularity)) + 
              geom_point(audioFeatures, mapping=aes(x=valence, y=track_popularity,colour = album_name), show.legend = F) + 
              xlim(c(0, 1)) + 
              ylim(c(0, 100)) + 
              theme(legend.position='none') + 
              labs(
                   y="Popularity of Song", 
                   x="Valence")
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  #####################################
  
  #Popularity vs Liveness graph by song 
  
  #####################################
  output$audioGraph2 <- renderPlotly({
      audioFeatures <- audioTable()
      
      #get popularity & valence columns 
      audioFeatures <- audioFeatures %>%
        select(track_name, liveness, track_popularity,album_name)
      
      gg <- ggplot(audioFeatures, aes(x=liveness, y=track_popularity)) + 
              geom_point(audioFeatures, mapping=aes(x=liveness, y=track_popularity,colour = album_name), show.legend = F) + 
              xlim(c(0, 1)) + 
              ylim(c(0, 100)) + 
              theme(legend.position='none') + 
              labs(
                   y="Popularity of Song", 
                   x="Liveness")
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  #####################################
  
  #Popularity vs Dancibility graph by song 
  
  ##################################### 
  output$audioGraph3 <- renderPlotly({
      audioFeatures <- audioTable()
      
      #get popularity & valence columns 
      audioFeatures <- audioFeatures %>%
        select(track_name, danceability, track_popularity,album_name)
      
      gg <- ggplot(audioFeatures, aes(x=dancability, y=track_popularity)) + 
              geom_point(audioFeatures, mapping=aes(x=danceability, y=track_popularity, colour = album_name), show.legend = F) + 
              xlim(c(0, 1)) + 
              ylim(c(0, 100)) + 
              theme(legend.position='none') + 
              labs(
                   y="Popularity of Song", 
                   x="Danceability")
      #print(ggplotly(gg, tooltip=c("x", "y","track_name")))
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  #####################################
  
  #Popularity vs Tempo graph by song 
  
  ##################################### 
  output$audioGraph6 <- renderPlotly({
      audioFeatures <- audioTable()
      
      #get popularity & valence columns 
      audioFeatures <- audioFeatures %>%
        select(track_name, tempo, track_popularity,album_name)
      
      #View(audioFeatures)
      
      gg <- ggplot(audioFeatures, aes(x=tempo, y=track_popularity)) + 
              geom_point(audioFeatures, mapping=aes(x=tempo, y=track_popularity, colour = album_name), show.legend = F) + 
              xlim(c(0, 150)) + 
              ylim(c(0, 100)) + 
              theme(legend.position='none') + 
              labs(
                   y="Popularity of Song", 
                   x="Tempo")

      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  #####################################
  
  #main graph - energy vs valence graph
  
  #####################################
  output$audioGraph4 <- renderPlotly({
      audioFeatures <- audioTable()
      
      #get popularity & valence columns 
      audioFeatures <- audioFeatures %>%
        select(track_name, energy, valence, track_popularity, album_name)
      
      gg <- ggplot(audioFeatures, aes(x=valence, y=energy)) +
              geom_point(audioFeatures, mapping=aes(x=valence, y=energy, track_name = track_name, colour = album_name), show.legend = F) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")

      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  #Get sentiments from library 
  nrc_anticipation <- get_sentiments("nrc") %>% 
      filter(sentiment == "anticipation")
      nrc_anticipation
    
  nrc_anger <- get_sentiments("nrc") %>% 
      filter(sentiment == "anger")
      nrc_anger
     
  nrc_joy <- get_sentiments("nrc") %>% 
      filter(sentiment == "joy")
      nrc_joy
      
 #####################################
      
 #bar graph to get sentiment counts 
      
 #####################################
  output$audioGraph5 <- renderPlot({
      
      alist = selectedAlbums()
      total_lyrics = data.frame()
      for (a in alist){
          df <- data.frame(genius_album(artist_name(), a))
          total_lyrics <- rbind(total_lyrics,df)
      }
      total_lyrics <- total_lyrics %>%
          select(lyric)
      
      total_lyrics <- total_lyrics %>%
          unnest_tokens(word, lyric) %>%  #split words
          anti_join(stop_words)           # take out "a", "an", "the", etc.
     
     joy <- total_lyrics %>%
        inner_join(nrc_joy) %>% count(word, sort = TRUE)

     anticipation <- total_lyrics%>%
        inner_join(nrc_anticipation) %>% count(word, sort = TRUE)
  
     anger <- total_lyrics %>%
         inner_join(nrc_anger) %>% count(word, sort = TRUE)
      
      # get the sentiment lyrics
      sentiment <- total_lyrics %>%
        inner_join(get_sentiments("bing")) %>% 
        count(sentiment) %>% 
        spread(sentiment, n, fill = 0) %>% 
        mutate(sentiment = positive - negative) 
     
     # sentiments for every word in lyric
     tidy_lyrics <- total_lyrics %>%
        inner_join(get_sentiments("nrc"))
     
    nrc_plot_before <- tidy_lyrics %>%
      group_by(sentiment) %>%
      summarise(word_count = n()) %>%
      ungroup() %>%
      mutate(sentiment = reorder(sentiment, word_count)) 
    
    gg <- ggplot(data = nrc_plot_before, mapping = aes(x = reorder(sentiment, word_count), reorder(word_count,sentiment), fill = sentiment)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          theme(legend.position="none") + 
          xlab("Sentiment") +
          ylab("Frequency")
    gg
  })
  
  ###################################
  
  #Today's Top Songs
  
  ###################################
  output$topSongs <- renderPlotly({
      
      #table of top songs & their features
      top <- get_playlist_audio_features('spotify', '37i9dQZF1DXcBWIGoYBM5M')
      
      #get popularity, track name, valence & energy columns 
      top <- top %>%
          select(track_name, energy, valence, track_popularity, artist_name)
      
      gg <- ggplot(top, aes(x=valence, y=energy)) +
              geom_point(top, mapping=aes(x=valence, y=energy, track_name = track_name, artist_name = artist_name, colour = artist_name)) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")
      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  ###################################
  
  #80's Top Songs
  
  ###################################
  output$topSongs2 <- renderPlotly({
      
      #table of top songs & their features
      top <- get_playlist_audio_features('myplay.com', '19PgP2QSGPcm6Ve8VhbtpG')
      
      #get popularity, track name, valence & energy columns 
      top <- top %>%
          select(track_name, energy, valence, track_popularity, artist_name)
      
      gg <- ggplot(top, aes(x=valence, y=energy)) +
              geom_point(top, mapping=aes(x=valence, y=energy, track_name = track_name, artist_name = artist_name, colour = artist_name)) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")
      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  ###################################
  
  #90's Top Songs
  
  ###################################
    output$topSongs3 <- renderPlotly({
        
      #table of top songs & their features
      top <- get_playlist_audio_features('myplay.com', '3C64V048fGyQfCjmu9TIGA')
      
      #get popularity, track name, valence & energy columns 
      top <- top %>%
          select(track_name, energy, valence, track_popularity, artist_name)
      
      gg <- ggplot(top, aes(x=valence, y=energy)) +
              geom_point(top, mapping=aes(x=valence, y=energy, track_name = track_name, artist_name = artist_name, colour = artist_name)) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")
      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
  ###################################
  
  #70's Top Songs
  
  ###################################
    output$topSongs4 <- renderPlotly({
        
      #table of top songs & their features
      top <- get_playlist_audio_features('myplay.com', '5KmBulox9POMt9hOt3VV1x')
      
      #get popularity, track name, valence & energy columns 
      top <- top %>%
          select(track_name, energy, valence, track_popularity, artist_name)
      
      gg <- ggplot(top, aes(x=valence, y=energy)) +
              geom_point(top, mapping=aes(x=valence, y=energy, track_name = track_name, artist_name = artist_name, colour = artist_name)) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")
      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
    
  ###################################
  
  #60's Top Songs
  
  ###################################
    output$topSongs5 <- renderPlotly({
        
      #table of top songs & their features
      top <- get_playlist_audio_features('myplay.com', '31LVuXlRYRVq4Z6krWGedS')
      
      #get popularity, track name, valence & energy columns 
      top <- top %>%
          select(track_name, energy, valence, track_popularity, artist_name)
      
      gg <- ggplot(top, aes(x=valence, y=energy)) +
              geom_point(top, mapping=aes(x=valence, y=energy, track_name = track_name, artist_name = artist_name, colour = artist_name)) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")
      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
    
  ###################################
  
  #2000's Top Songs
  
  ###################################
    output$topSongs6 <- renderPlotly({
        
      #table of top songs & their features
      top <- get_playlist_audio_features('myplay.com', '2f6tXtN0XesjONxicAzMIw')
      
      #get popularity, track name, valence & energy columns 
      top <- top %>%
          select(track_name, energy, valence, track_popularity, artist_name)
      
      gg <- ggplot(top, aes(x=valence, y=energy)) +
              geom_point(top, mapping=aes(x=valence, y=energy, track_name = track_name, artist_name = artist_name, colour = artist_name)) +
              xlim(c(0, 1)) +
              ylim(c(0, 1)) +
              labs(
                   y="Energy",
                   x="Valence") +
              geom_vline(xintercept = 0.5) + geom_hline(yintercept = 0.5) +
              annotate("text", x = 0.05, y = 0, label = "Depressing") + 
              annotate("text", x = 0.03, y = 0.98, label = "Turbulent") + 
              annotate("text", x = 0.98, y = 0.98, label = "Happy") + 
              annotate("text", x = 0.98, y = 0, label = "Mellow")
      
      print(ggplotly(gg) %>% config(displayModeBar = F))
  })
  
    output$analysisText = renderUI({
    HTML(paste0("Hi There! <br>",
                "This app utilizes the Spotify API to search your favorite artists and perform several quantiative tests ",
                "that analyze popularity of songs against factors such as tempo, liveness, dancability etc. It also uses the Genius API to grab
                song lyrics and perform sentiment analysis. Get started by simply searching an artist. Gathering artist data is relatively quick 
                depending on how many albums you choose to analyze. But, your wait should at most be 1 or 2 minutes. 
                You can also click the sidebar menu to see a visual of top hits from today back to the 60's. <br><br>", 
                "<strong>Note:</strong> If there is a 'Not Found (HTTP 404)' error, it just means the Genius lyric library unfortunately doesn't
                have lyrics for one of the albums you selected, so you might want to try another album!"
    ))
  })
    
    output$analysis <- renderText({
        "The overall analysis varies for each artist, but theres a positive correlation between high energy and dancability
                and the popularity of a song on the charts. On the contrary, songs with greater popularity tend to have lower valence, where
                valence is a meansure of musical postiveness. In the wider scheme of music, the graphs in the other tab portray sentiment for 
                current top songs and hits in the 90's, 80's, 70's and 60's. Top songs can say a lot about a time period and the overall mood/perspective
                of the generation during the time. Today's current hits are a little more turbulent, there could be a lot of factors causing this (it is nearing summer) 
                and these are also current hits, unlike the 90's-60's graphs which are complied over a decade. 
                However, it's apparent that as you go backwards in time, there are a greater amount of songs falling into the 'sad' catagory rather than the 'turbulent' catagory. 
                Again theres a lot of factors to consider for this, one of the biggest ones being context - the 60's and 70's included such as the Vietnam War, inflation, 
                civil rights movements etc, that influenced music genres. 
                Ideally, I'd like to take this analysis further, because I think theres a lot more to expand
                upon here. But given the time frame I left it at quantifying past popularity of a song with their musical factors. I hope to possibly expand 
                upon this during the summer - I plan on looking into/learning about recurrent netural networks to train a dataset and generate 
                predictive lyrics based on the artists past songs :)"
    })
}

shinyApp(ui, server)