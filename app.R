
library(shiny)
library(leaflet)
library(maps)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(sf)
library(spData)
library(plotly)
library(shinydashboard)
library(forcats)
library(bslib)
library(plotly)
library(ggthemes)


#Importation des données

df <- read.csv("data/Olympic Athletes and Events.csv")

df <- df |> mutate(df, host = ifelse(City == "Rio de Janeiro", "Brazil",
                                ifelse(City == "London", "United Kingdom",
                                  ifelse(City == "Beijing", "China",
                                    ifelse(City == "Athina", "Greece",
                                      ifelse(City %in% c("Sydney", "Melbourne"), "Australia",
                                        ifelse(City %in% c("Atlanta", "Los Angeles", "St. Louis"), "United States",
                                          ifelse(City == "Barcelona", "Spain",
                                            ifelse(City == "Seoul", "South Korea",
                                              ifelse(City == "Moskva", "Russian Federation",
                                                ifelse(City == "Montreal", "Canada",
                                                  ifelse(City %in% c("Munich", "Berlin"), "Germany",
                                                    ifelse(City == "Mexico City", "Mexico",
                                                      ifelse(City == "Tokyo", "Japan",
                                                        ifelse(City == "Roma", "Italy",
                                                          ifelse(City == "Paris", "France",
                                                            ifelse(City == "Helsinki", "Finland",
                                                              ifelse(City == "Amsterdam", "Netherlands",
                                                                ifelse(City == "Antwerpen", "Belgium",
                                                                  ifelse(City == "Stockholm", "Sweden",
                                                                  "Other"))))))))))))))))))))


df$Medal_bin <- 0
df$Medal_bin[df$Medal!="NA"] <- 1
df$Gold <- 0
df$Gold[df$Medal == "Gold"] <- 1
df$Silver <- 0
df$Silver[df$Medal == "Silver"] <- 1
df$Bronze <- 0
df$Bronze[df$Medal == "Bronze"] <- 1

df_noc <- read.csv("data/noc_regions.csv")
df <- merge(df, df_noc, on="NOC")
df <- df |> rename(Country = region)

df$Country[df$Country=="Boliva"] <- "Bolivia"
df$Country[df$Country=="Ivory Coast"] <- "Côte d'Ivoire"
df$Country[df$Country=="Republic of Congo"] <- "Republic of the Congo"
df$Country[df$Country=="Russia"] <- "Russian Federation"
df$Country[df$Country=="South Korea"] <- "Republic of Korea"
df$Country[df$Country=="Trinidad"] <- "Trinidad and Tobago"
df$Country[df$Country=="UK"] <- "United Kingdom"
df$Country[df$Country=="USA"] <- "United States"
df$Country[df$Country=="North Korea"] <- "Dem. Rep. Korea"

data_init_summer <- df[df$Season == "Summer",]
data_init_winter <- df[df$Season == "Winter",]

mapData <- world[c(2,11)]

data_summer <- data_init_summer|> 
  filter(Medal %in% c("Bronze", "Silver", "Gold")) |> 
  group_by(Year, Team, Event) |> 
  summarise(Medal = unique(Medal), Sex = first(Sex), Sport = first(Sport), Country = first(Country), 
            Medal_bin = first(Medal_bin), Gold = first(Gold), Silver = first(Silver), Bronze = first(Bronze), host=first(host))

data_winter <- data_init_winter|> 
  filter(Medal %in% c("Bronze", "Silver", "Gold")) |> 
  group_by(Year, Team, Event) |> 
  summarise(Medal = unique(Medal), Sex = first(Sex), Sport = first(Sport), Country = first(Country), 
            Medal_bin = first(Medal_bin), Gold = first(Gold), Silver = first(Silver), Bronze = first(Bronze), host=first(host))

data_summer_host <- data_summer |> filter(Country %in% unique(data_summer$host))
medals_summary <- data_summer_host |> group_by(Country, Year) |> summarise(`total medals` = sum(Medal_bin), `total golds` = sum(Gold))
medals_summary <- medals_summary |> group_by(Country) |> summarise(`avg total medals` = round(mean(`total medals`),3), `avg total golds` = round(mean(`total golds`),3))
medals_not_host <- data_summer_host |> filter(Country != host) |> group_by(Country, Year) |> summarise(`total medals` = sum(Medal_bin), `total golds` = sum(Gold))
medals_host <- data_summer_host |> filter(Country == host) |> group_by(Country, Year) |> summarise(`total medals host` = sum(Medal_bin), `total golds host` = sum(Gold))

c <- merge(medals_summary, medals_host, on="Country")

d <- left_join(medals_not_host, c, by="Country")

# Define UI for application
ui <- navbarPage(
  title = div(tags$a(href = "https://olympics.com/fr/olympic-games", span(img(src="olympics.png", height=30))), "120 YEARS OF OLYMPIC GAMES"),
    theme = shinythemes::shinytheme("cosmo"),
    br(),
    tabPanel("Home", column(7,
             wellPanel(
             h1("Home"),
             p("This app is based on 120 years of Olympic Games, from 1896 to 2016. The database used is", em("Olympic Athletes and Events"), 
             "from Kaggle. And composed with the athletes' name, their characteristics, their sports, the city of the OG, the year or the season."),
             h3("Summer & Winter games"),
             p(strong("Goal:"), "Have some statistics about the repartition of medals among every participating countries and have the performance of these countries."),
               p("We separate summer and winter games because the results for the countries are very different depending on the season.
               To see the repartition of medals we made a choropleth map, with the possibiliy to filter.
               And, to see the performance we made a table."),
             h3("Hosting impact"),
             p(strong("Goal:"), "Answer the question : does hosting games has an impact on medals performances ?"),
             p("We made a barplot to see the results depending on every host country, with a distinction between every types of medals.
               We create a diagram with points, to compare the host countries with the total number of medals the year which they host the games and 
               the mean of every other years.
               We also made the same graphic but only with the gold medals, because the ranking are made with this."), 
             h4("See you in 2024..."),
             downloadLink('downloadData', 'Download data'))),
            column(3,
                   img(src="ceremony.jpg", align = "center", height=500)),),
      tabPanel("Summer",
               sidebarLayout(
                 sidebarPanel(h3("Choose your filters:"), width=3, 
                    selectInput(inputId = "sex", label = h4("Sex:"), 
                              choices = unique(data_summer$Sex) |> append("All") |> sort(), 
                              selected = "All"),
                    selectInput(inputId = "year", label = h4("Year:"), 
                                choices = unique(data_summer$Year) |> append("All") |> sort(), 
                                selected = "All"),
                   selectInput(inputId = "sport", label = h4("Sport:"), 
                                 choices = unique(data_summer$Sport) |> append("All") |> sort(), 
                                 selected = "All"),
                   selectInput(inputId = "event", label = h4("Event:*"), 
                               choices = NULL),
                   p("*please select a sport before selecting an event"),
                   br(),
                   h4("Performance calculation:"),
                   p("Medalists / Total athletes")),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Medal distribution map", leafletOutput("map", height="600px", width="1100px")),
                    tabPanel("Performance table", dataTableOutput("tableau")),
                    tabPanel("Data athletes", dataTableOutput("tableinit")),
                    tabPanel("Data medals", dataTableOutput("table"))
                    )))),
       tabPanel("Winter", 
              sidebarLayout(
                sidebarPanel(h3("Choose your filters:"), width=3,
                  selectInput(inputId = "varsex", label = h4("Sex:"), 
                              choices = unique(data_winter$Sex) |> append("All") |> sort(), 
                              selected = "All"),
                  selectInput(inputId = "varyear", label = h4("Year:"), 
                              choices = unique(data_winter$Year) |> append("All") |> sort(), 
                              selected = "All"),
                  selectInput(inputId = "varsport", label = h4("Sport:"), 
                              choices = unique(data_winter$Sport) |> append("All") |> sort(), 
                              selected = "All"),
                  selectInput(inputId = "varevent", label = h4("Event:*"), 
                              choices = NULL),
                  p("*please select a sport before selecting an event"),
                  br(),
                  h4("Performance calculation:"),
                  p("Medalists / Total athletes")),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Medal distribution map", leafletOutput("mapwinter", height="600px", width="1100px")), 
                   tabPanel("Performance table", dataTableOutput("tableauwinter")),
                   tabPanel("Data athletes", dataTableOutput("tableinitwinter")),
                   tabPanel("Data medals", dataTableOutput("tablewinter"))
                   )))),
        tabPanel("Hosting impact",
           sidebarLayout(
             sidebarPanel(h3("Choose your filters:"), width=3,
                          selectInput(inputId = "var_country", label=h4("Country:"),
                                      choices = unique(data_summer$host) |> sort(),
                                      selected = "United Kingdom"),
                          sliderInput("slider_year", label = h4("Year:"), min = 1896, 
                                      max = 2016, value = c(1948, 2016), sep="", ticks = F)),
              mainPanel(plotlyOutput("barplot_host"), br(), 
                        tabsetPanel(tabPanel("Total", plotlyOutput("plot_host")),
                                    tabPanel("Gold", plotlyOutput("plot_gold_host"))))
           )))
            

# Define server logic required
server <- function(input, output,session) {
  
  observeEvent(input$sport,{
    updateSelectInput(session, input = "event", choices = data_summer$Event[data_summer$Sport == input$sport]|> append("All"), selected="All")})
  
  output$tableau <- renderDataTable({
    if (input$sex != "All"){
      data_init_summer <- filter(data_init_summer, Sex == input$sex)
    }
    if (input$year != "All"){
      data_init_summer <- filter(data_init_summer, Year == input$year)
    }
    if (input$sport != "All"){
      data_init_summer <- filter(data_init_summer, Sport == input$sport)
    }
    if (input$event != "All"){
      data_init_summer <- filter(data_init_summer, Event == input$event)
    
    }  
    
    tab_summer <- data_init_summer |> group_by(Country) |> 
      summarise(`Medalists` = sum(Medal_bin), `Total athletes` = length(unique(ID)))

    tab_summer$Performance <- round(tab_summer$`Medalists`/tab_summer$`Total athletes`, 3)
    tab_summer
  })
  
  observeEvent(input$sport,{
    updateSelectInput(session, input = "event", choices = data_summer$Event[data_summer$Sport == input$sport]|> append("All"), selected="All")})
  
  output$map <- renderLeaflet({
    if (input$year != "All"){
      data_summer <- filter(data_summer, Year == input$year)
    }
    if (input$sport != "All"){
      data_summer <- filter(data_summer, Sport == input$sport)
    }
    if (input$sex != "All"){
      data_summer <- filter(data_summer, Sex == input$sex)
    }
    
    if (input$event != "All"){
      data_summer <- filter(data_summer, Event == input$event)  
    }  
    
    countries <- data_summer |> group_by(Country) |> summarise(nbmedal = sum(Medal_bin), nbgold = sum(Gold), nbsilver = sum(Silver), nbbronze = sum(Bronze))
    countries <- left_join(countries, mapData, c("Country" = "name_long"))
    labels <- sprintf("<strong>%s</strong><br/>%g medals<br/><font color=#FFBF00>%g gold<font color/><br/><font color=#BDC3C7>%g silver<font color/><br/><font color=#7E5109>%g bronze", countries$Country, countries$nbmedal, countries$nbgold, countries$nbsilver, countries$nbbronze) |>  lapply(htmltools::HTML)
    pal <- colorNumeric(palette = "Reds", domain = countries$nbmedal)
    map <- leaflet() |> addTiles() |> setView(-7.210814, 30.161823, zoom = 1.5) |> addProviderTiles(providers$CartoDB.Positron)
    map |> addPolygons(data=countries$geom, 
                       fillColor = pal(countries$nbmedal), 
                       fillOpacity = .7, color="grey",weight=1,
                       highlightOptions = highlightOptions(weight=3, color="grey", fillOpacity = .7, bringToFront = TRUE),
                       label = labels) |> 
          addLegend(pal=pal, values = countries$nbmedal, position="bottomleft", title="Number of Medals")
  })
  

  observeEvent(input$varsport,{
    updateSelectInput(session, input = "varevent", choices = data_winter$Event[data_winter$Sport == input$varsport]|> append("All"), selected="All")})
  
  output$tableauwinter <- renderDataTable({
    
    if (input$varsex != "All"){
      data_init_winter <- filter(data_init_winter, Sex == input$varsex)
    }
    if (input$varyear != "All"){
      data_init_winter <- filter(data_init_winter, Year == input$varyear)
    }
    if (input$varsport != "All"){
      data_init_winter <- filter(data_init_winter, Sport == input$varsport)
    }
    if (input$varevent != "All"){
      data_init_winter <- filter(data_init_winter, Event == input$varevent)
      
    }
    
    tab_winter <- data_init_winter |> group_by(Country) |> 
      summarise(`Medalists` = sum(Medal_bin), `Total athletes` = length(unique(ID)))

    tab_winter$Performance <- round(tab_winter$`Medalists`/tab_winter$`Total athletes`, 3)
    tab_winter 
  })
  
  observeEvent(input$varsport,{
    updateSelectInput(session, input = "varevent", choices = data_winter$Event[data_winter$Sport == input$varsport]|> append("All"), selected="All")})
  
  output$mapwinter <- renderLeaflet({
    if (input$varyear != "All"){
      data_winter <- filter(data_winter, Year == input$varyear)
    }
    if (input$varsport != "All"){
      data_winter <- filter(data_winter, Sport == input$varsport)
    }
    if (input$varsex != "All"){
      data_winter <- filter(data_winter, Sex == input$varsex)
    }
    if (input$varevent != "All"){
      data_winter <- filter(data_winter, Event == input$varevent)
    } 
    
    countries <- data_winter |> group_by(Country) |> summarise(nbmedal = sum(Medal_bin), nbgold = sum(Gold), nbsilver = sum(Silver), nbbronze = sum(Bronze))
    countries <- left_join(countries, mapData, c("Country" = "name_long"))
    labels <- sprintf("<strong>%s</strong><br/>%g medals<br/><font color=#FFBF00>%g gold<font color/><br/><font color=#BDC3C7>%g silver<font color/><br/><font color=#7E5109>%g bronze", countries$Country, countries$nbmedal, countries$nbgold, countries$nbsilver, countries$nbbronze) |>  lapply(htmltools::HTML)
    pal <- colorNumeric(palette = "Blues", domain = countries$nbmedal)
    map <- leaflet() |> addTiles() |> setView(-7.210814, 30.161823, zoom = 1.5) |> addProviderTiles(providers$CartoDB.Positron)
    map |> addPolygons(data=countries$geom, 
                       fillColor = pal(countries$nbmedal), 
                       fillOpacity = .7, color="grey",weight=1,
                       highlightOptions = highlightOptions(weight=3, color="grey", fillOpacity = .7, bringToFront = TRUE),
                       label = labels) |> 
      addLegend(pal=pal, values = countries$nbmedal, position="bottomleft", title="Number of Medals")
  })
  
  output$barplot_host <- renderPlotly({
    if (input$var_country != "United Kingdom"){
      data_summer <- data_summer |> filter(input$var_country %in% unique(data_summer$host))
    }
    
    output$range <- renderPrint({input$slider_year})
    
    data_host <- data_summer[data_summer$Country == input$var_country,] |>
      mutate(Host = (input$var_country == host))
    
    host <- ggplot(data_host)+
      aes(x=Year, y=Medal_bin, fill=fct_relevel(Medal, "Silver", after=1), alpha = Host)+
      geom_bar(stat="identity")+
      scale_fill_manual(values = c("#7E5109", "#BDC3C7", "#FFBF00"))+
      scale_alpha_manual(values = c(.5, 1))+
      xlim(min(input$slider_year)-2, max(input$slider_year)+2)+ #ylim(0,max(input$slider_medals))+
      labs(x="Games Year", y="Medals number", title = paste("Hosting impact on medals for", input$var_country), fill="Medal")+
      theme_minimal() +
      guides(alpha = F) 
    ggplotly(host, tooltip = c("Year", "Host"))
  })
  
  output$plot_host <- renderPlotly({
    
    plot_host <- ggplot(d)+
      geom_point(aes(x=`total medals`, y=fct_reorder(Country, `avg total medals`)), alpha=.1, size=2)+
      geom_point(aes(x=`avg total medals`, y=fct_reorder(Country, `avg total medals`), color="Average"), size=3)+
      geom_point(aes(x=`total medals host`, y=fct_reorder(Country, `avg total medals`), color="Hosting"), size=3)+
      labs(x="Total medals", y="", title="Medals by country")+
      theme_minimal()+
      scale_color_manual(name= "", breaks = c("Average", "Hosting"), values=c("Average"="black", "Hosting"="red3"))+
      theme(legend.position="top", axis.text.y = element_text(family="mono", color = "black"))
    ggplotly(plot_host, tooltip = c("total medals", "total medals host", "avg total medals"))
  })
  
  output$plot_gold_host <- renderPlotly({
    
    plot_gold_host <- ggplot(d)+
      geom_point(aes(x=`total golds`, y=fct_reorder(Country, `avg total golds`)), alpha=.1, size=2, color="#FFBF00")+
      geom_point(aes(x=`avg total golds`, y=fct_reorder(Country, `avg total golds`), color="Average"), size=3)+
      geom_point(aes(x=`total golds host`, y=fct_reorder(Country, `avg total golds`), color="Hosting"), size=3)+
      labs(x="Total gold medals", y="", title="Gold medals by country")+
      scale_color_manual(name= "", breaks = c("Average", "Hosting"), values=c("Average"="#FFBF00", "Hosting"="red3"))+
      theme_minimal()+
      theme(legend.position="top", axis.text.y = element_text(family="mono", color = "black"))
    ggplotly(plot_gold_host, tooltip = c("total golds", "total golds host", "avg total golds"))
  })
  
  output$table <- renderDataTable(data_summer)
  output$tablewinter <- renderDataTable(data_winter)
  output$tableinit <- renderDataTable(data_init_summer)
  output$tableinitwinter <- renderDataTable(data_init_winter)
  
  output$downloadData <- downloadHandler(filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(df, con)
    }
  )

  }

# Run the application 
shinyApp(ui = ui, server = server)
