#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)
library(geomtextpath)
library(ggimage)
library(plotly)
options(bitmapType='cairo')

matchup_final = read.csv("matchup_final.csv", check.names = FALSE)
positional_stats = read.csv('positional_stats.csv', check.names = FALSE)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
    tabPanel("About",
             fluidRow(column(12, style='padding-left:150px; padding-right:150px;',
                             h1("NBA Matchup Data (Updated 4/15/2024)"))),
             fluidRow(column(12, style='padding-left:150px; padding-right:150px;',
                             h4(strong("About the App:"), "Using NBA API matchups data through Saiem Gilani's hoopR package, I've developed a Shiny app that lets you view box score stats in matchups between a chosen offensive and defensive player. In the 'Matchup Stats' tab, pick your matchup and then you will see a table that shows the matchup stats, the offensive player's average stats, and the difference between the two (all per 100 possessions). On the bottom, you can find two scatterplots, one showing the offensive player's points per 100 possession data, and one for the defensive player's data. The x axis shows partial possessions, which represents 'the sum total of partial possessions that were spent defending that player. The second tab allows you to view a defender's points per 100 possessions stats, filtering for his opponent's average scoring. For example, you can look at how Luka has defended players who average >20 points per 100 possessions."))),
             fluidRow(column(12, style='padding-left:150px; padding-right:150px;',
                             h4(strong("Uses:"), "The main use for this app is to evaluate how players perform against specific matchups. While this is more of a proof of concept, a fully developed app would include more years of data and more advanced statistics. Still, the table and plots allow the reader to get an overview of how a player has performed as an attacker/defender while understanding the context behind the performance (e.g. number of possessions, positional matchup). The plots specifically allow readers to see player's offensive/defensive vulnerabilities and strengths against specific players / player types."))),
             fluidRow(column(12, style='padding-left:150px; padding-right:150px;',
                             h4(strong("Limitations:"), "Asides from sample size issues, the main limitations with this analysis concerns positional differences in matchup scenarios. Mainly, it seems to me that big men are essentially penalized for helping. That is, when a player drives by his backcourt matchup and the frontcourt player comes over to help, the frontcourt player gets assigned the matchup. From that point on, one could argue they should still hold some responsibility if the player scores, but here the blame is fully distributed only to the big man. To illustrate this difference, the table below shows that frontcourt players give up more points and more shot attempts. Therefore, I would utilize this tool mostly to compare players of similar positions and would be careful when evaluating big men."))),
             fluidRow(column(12, style='padding-left:150px; padding-right:150px;',
                             DTOutput("positional")))),
    
    tabPanel("Matchup Stats",
             selectizeInput("offp1", "Offensive Player", choices = NULL),
             selectizeInput("defp1", label = "Defensive Player",  choices = NULL),
             plotOutput("playersPlot"),
             DTOutput("leaders"),
             fluidRow(
               splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("playersdefPlot"), plotlyOutput("playersoffPlot"))
             )),
    tabPanel("Matchup Filters",
             selectizeInput("defp2", "Defensive Player", choices = NULL),
             sliderInput(
               "pts",
               label = "Matchup: Points per 100 Possessions Avg",
               min = 0, 
               max = 60, 
               value = c(0, 60)
             ),
             DTOutput("playerstats")
)))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  updateSelectizeInput(session, 'defp1', choices = matchup_final$DEF_PLAYER_NAME, server = TRUE)
  updateSelectizeInput(session, 'offp1', choices = matchup_final$OFF_PLAYER_NAME, server = TRUE)
  updateSelectizeInput(session, 'defp2', choices = matchup_final$DEF_PLAYER_NAME, server = TRUE)
  
  #positional breakdown
  output$positional = renderDT(datatable(positional_stats, options = list(dom = 't'),colnames = c('Position'='D_pos', 'Mean Points Allowed per 100 Poss'='mean_points', 'Mean FGA Allowed per 100 Poss'='mean_fga'),  caption = "Matchup Stats Allowed Per 100 Possessions") %>%
    formatRound(columns=2:3, digits=2))
  
  #filter players
  player_plots = reactive({matchup_final %>% filter(OFF_PLAYER_NAME==input$offp1, DEF_PLAYER_NAME==input$defp1)})
  output$playersPlot <- renderPlot({
    ggplot(player_plots()) +
    geom_image(mapping=aes(y=0,x=-1,image=off_head), size=.5) +
    geom_image(mapping=aes(y=0,x=1,image=def_head), size=0.5) +
    scale_x_continuous(limits = c(-1.5, 1.5)) +
    theme_nothing()
  })
  matchup_a = reactive({matchup_final %>% filter(OFF_PLAYER_NAME==input$offp1, DEF_PLAYER_NAME==input$defp1) %>% 
    mutate('Type'='Matchup') %>% 
    select(GP, PARTIAL_POSS, Type, contains('Matchup'), OFF_PLAYER_NAME, DEF_PLAYER_NAME) %>% 
    rename_all(~stringr::str_replace(.,"Matchup ",""))})
  avg_a = reactive({matchup_final %>% filter(OFF_PLAYER_NAME==input$offp1, DEF_PLAYER_NAME==input$defp1) %>% 
    mutate('Type'='Avg') %>% 
    select(GP, PARTIAL_POSS, Type, contains('Avg'), OFF_PLAYER_NAME, DEF_PLAYER_NAME)%>% 
    rename_all(~stringr::str_replace(.,"Avg ",""))})
  diff_a=  reactive({matchup_final %>% filter(OFF_PLAYER_NAME==input$offp1, DEF_PLAYER_NAME==input$defp1) %>% 
    mutate('Type'='Diff') %>% 
    select(GP, PARTIAL_POSS, Type, contains('Diff'), OFF_PLAYER_NAME, DEF_PLAYER_NAME)%>% 
    rename_all(~stringr::str_replace(.," Diff",""))})
  data = reactive({matchup_a() %>%  rbind(avg_a(), diff_a()) %>%  select(1:8)})
  output$leaders = renderDT(datatable(data(), options = list(dom = 't'),colnames = c('Partial Possessions'='PARTIAL_POSS'),  caption = "Matchup Stats Per 100 Possessions. 'Avg' represents offensive player's season average stats scraped from Basketball Reference. Only includes matchups with at least 10 possessions") %>%
                              formatRound(columns=4:8, digits=2))  
  #plot of matchups
  plot_data = reactive(({matchup_final %>%  filter(DEF_PLAYER_NAME==input$defp1) %>% select(DEF_PLAYER_NAME, OFF_PLAYER_NAME, PARTIAL_POSS, `Matchup PTS`)}))
  wt_avg_d=reactive(({matchup_final %>%  filter(DEF_PLAYER_NAME==input$defp1) %>% select(DEF_PLAYER_NAME, OFF_PLAYER_NAME, PARTIAL_POSS, `Matchup PTS`) %>%
      mutate(pointsposs=PARTIAL_POSS* `Matchup PTS`) %>%  summarise(tot_poss=sum(PARTIAL_POSS), avg_pts=sum(pointsposs)/tot_poss)}))
  output$playersdefPlot = renderPlotly({
    ggplotly(ggplot(plot_data(), aes(y=`Matchup PTS`, x=PARTIAL_POSS)) +
      geom_point(aes(text=paste0(OFF_PLAYER_NAME, ", ",round(`Matchup PTS`, 1), ' points per 100 poss'))) +
        geom_hline(data = wt_avg_d(), aes(yintercept = mean(avg_pts)), linetype='dashed') +
        labs(title = paste0("Offensive Matchups vs ", input$defp1), x="Partial Possessions", y='Points per 100 Possessions') +
        theme_bw() +
      # geom_text(data = plot_data(), label=ifelse(OFF_PLAYER_NAME==input$offp1,as.character(OFF_PLAYER_NAME),''))+
      geom_point(data =matchup_a(), aes(y=`PTS`, x=PARTIAL_POSS, text=paste0(OFF_PLAYER_NAME, ", ",round(`PTS`, 1), ' points per 100 poss')), colour='red'), tooltip = 'text')
  })
  #offensive plot
  off_plot_data = reactive(({matchup_final %>%  filter(OFF_PLAYER_NAME==input$offp1) %>% select(DEF_PLAYER_NAME, OFF_PLAYER_NAME, PARTIAL_POSS, `Matchup PTS`)}))
  wt_avg_o=reactive(({matchup_final %>%  filter(OFF_PLAYER_NAME==input$offp1) %>% select(DEF_PLAYER_NAME, OFF_PLAYER_NAME, PARTIAL_POSS, `Matchup PTS`) %>% 
      mutate(pointsposs=PARTIAL_POSS* `Matchup PTS`) %>%  summarise(tot_poss=sum(PARTIAL_POSS), avg_pts=sum(pointsposs)/tot_poss)}))
  output$playersoffPlot = renderPlotly({
    ggplotly(ggplot(off_plot_data(), aes(y=`Matchup PTS`, x=PARTIAL_POSS)) +
               geom_point(aes(text=paste0(DEF_PLAYER_NAME, ", ",round(`Matchup PTS`, 1), ' points per 100 poss'))) +
               geom_hline(data = wt_avg_o(), aes(yintercept = mean(avg_pts)), linetype='dashed') +
               labs(title = paste0(input$offp1, " vs Defensive Matchups"), x="Partial Possessions", y='Points per 100 Possessions') +
               theme_bw() +
               # geom_text(data = plot_data(), label=ifelse(OFF_PLAYER_NAME==input$offp1,as.character(OFF_PLAYER_NAME),''))+
               geom_point(data =matchup_a(), aes(y=`PTS`, x=PARTIAL_POSS, text=paste0(DEF_PLAYER_NAME, ", ",round(`PTS`, 1), ' points per 100 poss')), colour='red'), tooltip = 'text')
  })
 #add scatterplot of matchups OR add option to filter to all matchups based on player avgs (e.g Jokic v 30pt averages)
  player_filter = reactive({matchup_final %>% filter(DEF_PLAYER_NAME==input$defp2, `Avg PTS`>=input$pts[1], 
                                                     `Avg PTS`<=input$pts[2]) %>% 
      mutate(matchup_tot_pts=`Matchup PTS`*PARTIAL_POSS/100, matchup_avg_poss= `Avg PTS`*PARTIAL_POSS) %>% 
      summarise(GP=sum(GP), Tot_PARTIAL_POSS=sum(PARTIAL_POSS), `Matchup PTS`=sum(matchup_tot_pts)/Tot_PARTIAL_POSS*100,
                `Avg PTS`=sum(matchup_avg_poss)/Tot_PARTIAL_POSS, 'Diff PTS'=`Matchup PTS`-`Avg PTS`)}) 
  output$playerstats = renderDT(datatable(player_filter(), options = list(dom = 't'), colnames = c("Total Partial Possessions"='Tot_PARTIAL_POSS'), caption = 'Matchup Stats Per 100 Possessions')%>%
                                  formatRound(columns=3:5, digits=2))
}
# Run the application 
shinyApp(ui = ui, server = server)
