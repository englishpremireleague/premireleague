#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("tidyverse")
library("lubridate")
library("gganimate")
library("dplyr")
library("gifski")
library("png")
library("plotly")
library("igraph")
library("visNetwork")
#Preparing the data
final = read.csv("final.csv")
prem = final #Making copy of data to be manipulated

prem = prem %>% 
    select(Date:FTR) %>% 
    mutate(Date = dmy(Date)) #Converting date to dmy format and removing unwanted columns

prem %>% mutate_if(is.factor, as.character) -> prem #Converting all factors to character vectors
prem[sort(unique(prem$HomeTeam))] = NA

for(i in 1:nrow(prem)) {
    if(prem$FTR[i] == "H") {
        prem[i, prem$HomeTeam[i]] = 3
        prem[i, prem$AwayTeam[i]] = 0
    } else if(prem$FTR[i] == "A") {
        prem[i, prem$AwayTeam[i]] = 3
        prem[i, prem$HomeTeam[i]] = 0
    } else{
        prem[i, c(prem$AwayTeam[i], prem$HomeTeam[i])] = 1
    }
}
prem_points = prem  %>% 
    gather(Team, Points, Arsenal:`West Ham`) %>% 
    select(Date, Team, Points) %>% 
    drop_na(Points)

empty = data.frame(Date = rep(unique(prem$Date), each = 20),
                   Team = unique(prem$HomeTeam),
                   stringsAsFactors = FALSE)

prem_points = left_join(empty, prem_points)
prem_points[is.na(prem_points)] = 0
prem_points = prem_points %>% 
    group_by(Team) %>% 
    arrange(Date) %>% 
    mutate(Points = cumsum(Points)) %>% 
    ungroup()
prem_points %>% 
    filter(Team == "Arsenal") %>% 
    arrange(Date)
prem[sort(unique(prem$HomeTeam))] = NA
for(i in 1:nrow(prem)){
    prem[i, prem$HomeTeam[i]] = prem$FTHG[i] - prem$FTAG[i]
    prem[i, prem$AwayTeam[i]] = prem$FTAG[i] - prem$FTHG[i]
}
prem_gd = prem  %>% 
    gather(Team, GD, Arsenal:`West Ham`) %>% 
    select(Date, Team, GD) %>% 
    drop_na(GD)
prem_gd = left_join(empty, prem_gd)
prem_gd[is.na(prem_gd)] = 0
prem_gd = prem_gd %>% 
    group_by(Team) %>% 
    arrange(Date) %>% 
    mutate(GD = cumsum(GD)) %>% 
    ungroup() 
prem_total = left_join(prem_points, prem_gd)
prem_total = prem_total %>% 
    group_by(Date) %>% 
    arrange(desc(Points), desc(GD)) %>% 
    mutate(Position = row_number()) %>% 
    ungroup()
Qual = function(x){
    if(x <= 4){
        y = "Champions League"
    } else if(x <= 7){
        y = "Europa League"
    } else if(x <= 10){
        y = "Top Half"
    } else if(x <= 17){
        y = "Bottom Half"
    } else { 
        y = "Relegation"
    }
    return(y)
}
prem_total = prem_total %>% 
    mutate(Status = map_chr(Position, Qual),
           Status = factor(Status, levels = c("Champions League",
                                              "Europa League",
                                              "Top Half",
                                              "Bottom Half",
                                              "Relegation")))
g = prem_total %>%
    arrange(Date) %>%
    ggplot(aes(GD, Points)) +
    geom_label(aes(label = Team, fill = Status), label.padding = unit(0.1, "lines")) +
    theme_minimal() +
    labs(title = "PL Team Points vs Goal Difference 17/18",
         subtitle =  "Date: {frame_time}") +
    scale_colour_brewer(type = "qual",
                        palette = "Paired") +
    theme(legend.position = "bottom") +
    transition_time(Date)

###################################################################


epl <- final %>% dplyr::select(HomeTeam, AwayTeam, FTHG, FTAG) %>% 
    dplyr::rename(team1=HomeTeam, team2= AwayTeam, team1FT = FTHG, team2FT = FTAG) %>%
    dplyr::filter(team1!="")
epl <- bind_rows(list(epl %>% 
                          dplyr::group_by(team1,team2) %>%
                          dplyr::summarize(points = sum(case_when(team1FT>team2FT~3,
                                                                  team1FT==team2FT~1,
                                                                  TRUE ~ 0))),
                      epl %>% dplyr::rename(team2=team1,team1=team2) %>%
                          dplyr::group_by(team1,team2) %>%
                          dplyr::summarize(points = sum(case_when(team2FT>team1FT~3,
                                                                  team2FT==team1FT~1,
                                                                  TRUE ~ 0))))) %>%
    dplyr::group_by(team1, team2) %>% dplyr::summarize(tot_points = sum(points)) %>% 
    dplyr::ungroup() %>% dplyr::arrange(team1,team2)

nodes <- dplyr::group_by(epl, team1) %>% 
    dplyr::summarize(value = sum(tot_points)) %>%
    dplyr::rename(id = team1) %>% 
    dplyr::arrange(desc(value)) 

edge_list <- epl %>% dplyr::filter(as.character(team1)<as.character(team2)) %>% 
    dplyr::filter(!tot_points %in% c(0,6)) %>%
    dplyr::rename(from=team1,to=team2,value=tot_points) %>% dplyr::select(from, to)

temp1 <- visNetwork(nodes,edge_list,main = "EPL 2018-19 Season",width="800px") %>%
    visEdges(color = list(color="gray",opacity=0.25)) %>%
    visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visEvents(stabilizationIterationsDone="function () {this.setOptions( { physics: false } );}") %>%
    visLayout(randomSeed=91)

#####################################################################

#UI element
ui <- navbarPage("Premire League Data",
                 tabPanel("Home",
                         sidebarLayout(sidebarPanel(textOutput("animText")),
                                       mainPanel(imageOutput("animation"))
                                    )
                          ),
                 
                 tabPanel("Team Path",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput(
                                      "teams",
                                      "Select Teams",
                                      sort(unique(prem$HomeTeam))
                                  )
                              ),
                              
                              mainPanel(plotOutput("distPlot"),
                                        textOutput("pathText"))
                          )
                          ),
                 tabPanel("Results",
                          mainPanel(plotOutput("resultPlot"),
                                    textOutput("resText"))),
                 
                 tabPanel("Team Position",
                          sidebarLayout(
                              sidebarPanel(
                                  checkboxGroupInput("select", "Select Teams:", sort(unique(prem$HomeTeam)),
                                                     selected = c("Arsenal","Man United", "Liverpool", "Chelsea"))
                              ),
                              mainPanel(plotlyOutput("teamSel"),
                                        textOutput("posText"))
                          )),
                 tabPanel("Team Network",
                          mainPanel(visNetworkOutput("net"),
                                    textOutput("netText")))
    
)

############################################################################################
# Server element
server <- function(input, output) {

    output$distPlot <- renderPlot({
        prem_total %>%
            filter(Team == input$teams) %>% 
            arrange(Date) %>% 
            ggplot(aes(GD, Points)) + 
            geom_point(aes(colour = Status), size = 3) +
            geom_path(linetype = 2, alpha = 0.4) +  
            theme_minimal() + 
            labs(title = "NUFC Points/Goal Difference Path",
                 subtitle = "Season 2018/2019") + 
            theme(legend.position="bottom") + 
            scale_colour_brewer(type = "qual",
                                palette = "Paired")

    })

    #output$info <- renderText({paste0("x=", input$plot_hover$x, "\ny=", input$plot_hover$y)})
    #output$sel <-  renderText(input$select)

    output$animation <- renderImage({
        outfile = tempfile(fileext = ".gif")
        anim_save(animate(g, nframes = 200, fps = 2), filename = "outfile.gif")
        list(src = "outfile.gif",
             contentType = "image/gif",
             alt = "Loading, please wait...")
    })

    output$resultPlot <- renderPlot(
        {
                final %>%
                # remove unplayed games
                filter(!is.na(FTHG)) %>%
                ggplot(., aes(x = AwayTeam, y = HomeTeam, fill = FTHG-FTAG)) +
                geom_tile() +
                # add the scorelines
                geom_label(aes(label = paste(FTHG, FTAG, sep = "-")), fill = "white") +
                # colour where green shows home win and red an away win
                scale_fill_gradient2(low = "darkred", high = "green", midpoint = 0, guide = FALSE) +
                scale_x_discrete(limits = levels(final$HomeTeam), position = "top") +
                scale_y_discrete(limits = rev(levels(final$AwayTeam))) +
                theme_minimal()
        }
    )
    
    output$teamSel <- renderPlotly(
        {
            tempDate = prem_total
            tempDate$Date = as.Date(tempDate$Date)
            
            allTeams = sort(input$select)
            filtered = as.data.frame(tempDate[tempDate$Team %in% allTeams[1],])
            indexOrder = order(as.Date(filtered$Date))
            teampoints = filtered[order(filtered$Date),]
            p = plot_ly(teampoints, x=~Date, y=~Position,name = allTeams[1] ,type = "scatter", mode = "lines+markers")%>%
                layout(yaxis = list(autorange = "reversed"))
            for(i in allTeams[2:20])
            {
                filtered = as.data.frame(tempDate[tempDate$Team %in% i,])
                indexOrder = order(as.Date(filtered$Date))
                teampoints = filtered[order(filtered$Date),]
                p = p %>% add_trace(data = teampoints, y = ~Position, name = i, mode = "lines+markers")
            }
         p   
        })
    
    output$net <- renderVisNetwork({
        temp1
    })
    ###############################################################################################
    #Descriptions
    
    output$animText <- renderText(
        'In this animation, we have "Goal Difference" on the X-axis and "Points" on the Y-axis.\nThe name of each English Premier League Club floats on the graph after every match based on \nthe Goal Difference and Total Points of that particular club. The background box surrounding the club name changes on the basis of the position of the club in the Premier League Table which is divided in 5 categories. The categories are Champions League (Top 4 in Table), Europa League (5th Place), Top Half (6-10), Bottom Half (11-17th Position) and Relegation (Bottom Three). On the basis of each passing match, the team position in the graph changes according to the change in values of their respective Goal Difference and Points.
        \n Team members: \nKaustubh Joshi - 10509351\nShashank Tyagi - 10359922\nWarren Dsilva - 10510085')
    
    output$pathText <- renderText('In this plot we can see the position of each team. The team can be selected from the list on the left. As the season advances based on their Goal Difference (x-axis) and Points (y-axis). Depending upon their position the color of the pointer changes from green to light blue which indicates the team position in the League Table.')
    
    output$resText <- renderText('In this plot, we have the results of each and every match that took place in the whole season. The plot consists of both away and home game results including the final score of the match.')
    
    output$posText <- renderText('In this line plot, it tells us about the position of a selected club throughout the whole season on the basis of how many points did the team scored after every month. X-axis consist of the months from August 2018(start of the PL season) to May 2019 and on the y-axis we have the positions in the Premier League table which ranges from 20 to 1 as 1st position is the top position in table.')
    
    output$netText <- renderText('In this interactive plot when we click on a particular bubble with the team name in it, all the bubbles with the teams that have been defeated by the selected bubble gets highlighted in the plot. The size of the bubble of any club shows the quantity of points that that team possess.')
}

# Run the application 
shinyApp(ui = ui, server = server)
