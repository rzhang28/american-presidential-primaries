#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(foreign)
library(tidyverse)
library(shinythemes)
library(leaflet)
library(survival)
library(tidyverse)
library(Hmisc)
library(rqPen)
library(vctrs)
library(foreign)
library(margins)
library(mfx)
library(VGAM)
library(erer)
library(nnet)
library(RStata)
library(mlogit)
library(jtools)
library(ggstance)
library(clogitboost)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(scales)
library(survival)
library(ggeffects)
library(effects)
library(dplyr)

df08national_d <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df08phone_d.dta")
df08national_r <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df08phone_r.dta")
df04national_d <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df04_national_d.dta")
df00national_d <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df00_national_d.dta")
df00national_r <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df00_national_r.dta")
df88st_d <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df88st_d.dta")
df88st_r <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df88st_r.dta")
df84national_d <- read.dta("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/clean_data/df84cm_d.dta")

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
  "American Presidential Primaries",

############## PAGE 1 ##############
    
    tabPanel("The Puzzle",
             h2("Setting the Scene"),
             p("In the final Democratic debate of 2019, seven candidates 
             competed to present their cases for receiving their party’s 
             nomination for the presidency. Much of the three hours were spent 
             discussing stark policy disagreements,
             but equally significant emphasis was placed on resolving who would be 
             most likely to beat the Republican incumbent Donald Trump."),
             p("According to polls, electability concerns loomed persistently among a majority of 
             Democratic voters in the 2020 race. A national survey of likely 
             Democratic primary voters by The Economist asked respondents whether 
             it was more important for the Democratic party to select a nominee 
             who “agrees with your position on most issues” or “can win the general 
             election in November,” to which 65 percent of respondents prioritized 
             electability, compared to only 35 percent who chose policy positions.
             In one of the largest exit polls conducted in the race for the nomination, 
             Edison Media Research found that 33 percent of Democratic primary voters 
             in New Hampshire preferred a nominee who agreed with them on major issues, 
             while twice as many respondents–63 percent–preferred a nominee who could 
             defeat Trump."),
             
             # Generate a 2x2 column/row layout.
             
             fluidPage(
                 fluidRow(style = 'padding:30px;',
                          column(7, plotOutput("figure5")),
                          column(5,
                                 h3("Mentions of electability have increased over time."),
                                 p("Electability is not a novel phenomenon. Upon 
                                 casual glance, one might believe that voting based
                                 on electability is a unique response to Trump. However, 
                                 electability’s presence steadily risen in our national discourse. 
                                 Google's N-gram Viewer reveals
                                 the rising usage of “electability” and “electable” in recent
                                 years in American English. Strikingly, mentions of these two
                                 terms were extremely infrequent until the beginning of the 1970s, 
                                 but rose from significantly from the 1970s onwards, with particularly
                                 steep increases during presidential election years, and with 
                                 no clear sign of abating in the future."))),
                 fluidRow(column(5, 
                                 h3("News coverage has focused increasingly on electability."),
                                 p("Mentions of “electability” and “electable” have steadily increased since 
                                 the 1980s, the year that LexisUni’s record of newspapers begin, through 2020,
                                 occupying an ever-increasing role in American political 
                                 discourse. These findings reinforce results from the Google 
                                 N-gram Viewer – namely that usages of “electability” and “electable” have 
                                 increased over time and were pointedly prominent in 2020.")),
                          column(7,
                                 plotOutput("figure6"))),
                 fluidRow(style = 'padding:30px;',
                          column(7, plotOutput("figure8")),
                          column(5,
                                 h3("Polls have made it easier than ever to talk about electability."),
                                 p("The rise of polls has subsequently affected 
                                 the tone of political media coverage. Specifically, 
                                 scholars have found that media coverage of political 
                                 campaigns have shifted significantly away from policy 
                                 issues and instead towards what political scientists 
                                 call the “horse race.” One of the earliest studies by Klein
                                 and Maccoby (1954) found that 60 percent of stories concerned
                                 policy or issues, 16 percent candidates‘ personal qualities, 
                                 and 5 percent covered scandals.  By 2016, the majority of news
                                 coverage was about the horse race, rather than specific policy issues."))),
                 h2("Our Goal"),
                 p("Political scientist James Ceaser once declared, “No systematic
                 theory about primary voting is likely to develop for some time 
                 because each campaign is so different.”  By studying eight nominating 
                 contests that span 30 years, we attempt to meet Ceaser’s challenge."))),

############## PAGE 2 ##############

tabPanel("Sincere Considerations",
         fluidPage(
           h2("The Feeling Thermometer"),
           p("The ANES and NAES make extensive use of feeling thermometers, which 
           ask respondents to rate candidates on a scale that ranges from 0–100, 
           where 0 is very unfavorable and 100 is very favorable."),
           p("One difficulty in using feeling thermometers is that respondents provide 
           ranges of ratings that vary considerably. For example, one respondent 
           might rate his or her favorite and least favorite candidate 90 and 10, 
           respectively, while another might rate his or her favorite and least favorite
           candidate 60 and 40, respectively. The true utility of feeling 
           thermometers lies not in raw scores, but in the relative distance of values
           recorded between the candidates for each respondent. Therefore, to make responses comparable between 
           respondents, we normalized eachrating according to the following formula, 
           which is derived from a similar convention in the literature:"),
           p("u_j=(U_j-U_n)/(U_1-U_n)"),
           p("In this formula, u_i is the “normalized” feeling thermometer 
           value, U_j is the original feeling thermometer value, U_1 is the 
           highest thermometer rating used by the respondent, and U_n is the lowest. 
           After normalizing, each respondent’s highest-rated candidate is always given 
           a score of 1, the lowest 0, and other candidates some value in-between. For
           example, if a respondent rates three candidates 80, 60, 40, and 30, the normalized
           scores would be 1, 0.60, 0.20, and 0, respectively. The expectation is that as 
           these values increase for each candidate so will the probabilities of a given 
           respondent selecting that candidate over another."),
           titlePanel(""),
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "plot_type1",
                 "Election Year (Party)",
                 c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e", "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h")
               )),
           mainPanel(plotOutput("distPlot1"))),
           h2("Ideological Distance"),
           p("In both the ANES and NAES, respondents are asked to rank themselves, 
           as well as each of the primary candidates, on an ideological scale, from 1 
           (“most liberal”) to 5 (“most conservative”)."),
           p("Because we are not interested in the absolute values of each respondent’s 
           rankings, but rather, the distance between where respondents placed themselves 
           on the ideological scale and where they placed each candidate, we calculate 
           ideological distance by taking the difference between each respondent’s ranking 
           of themselves and of the candidate, squaring the difference (since otherwise 
           positive and negative differences would cancel each other out), and scaling to 
           1 to make later comparisons between coefficients in our model easier to interpret. 
           These calculations can be summarized by the following formula:"),
           p("d_j=〖(D_c- D_r)〗^2/5"),
           p("In this formula, d_j is the normalized ideological distance, 
           D_c is the respondent’s ranking of a candidate’s ideology, and D_r 
           is the respondent’s ranking of his or her own ideology."),
           titlePanel(""),
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "plot_type3",
                 "Election Year (Party)",
                 c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e", "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h")
               )),
             mainPanel(plotOutput("distPlot3"))))),

############## PAGE 3 ##############

tabPanel("Strategic Considerations",
         fluidPage(
           p("Respondents are asked in the ANES and NAES to assess their 
           expectations of the chances of each primary candidate winning the 
           party’s nomination (viability) and the general election (electability)."),
           p("Since we are interested in respondents’ relative rankings of candidates,
           and since the responses for all candidates frequently summed to 
           over 100 percent, we normalized responses to both the viability and
           electability questions according to the below question, where P_j is 
           the raw score given by the respondent, n is the number of candidates 
           that were rated, and p_j is the normalized score for a given candidate:"),
           p("p_j=P_j/(P_1+⋯+ P_n )"),
           p("In this way, each respondent had two series of responses that summed 
           to 1, one for each party. For example, imagine that one respondent
           indicates that five candidates have a 12 percent chance of winning 
           and a sixth candidate who has an 80 percent chance of winning. The
           first five would get a normalized score of .08 and the sixth 
           candidate a 0.60. To be clear, this does not suggest that the sixth 
           candidate had a 60 percent chance of winning the election, but instead, 
           the 0.60 represents the proper proportion of the 150 percentage points the 
           respondent initially allocated out. Ultimately, normalization allows us to
           capture the relationship between candidate responses, and to treat equally
           respondents who provided appropriately scaled responses, in which their
           responses summed to 100 percent, with those who did not."),
           titlePanel("Viability"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type2",
                             "Election Year (Party)",
                             c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e",  "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h")
                         )),
                     mainPanel(plotOutput("distPlot2"))),
                 
                 titlePanel("Electability"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "plot_type4",
                       "Election Year (Party)",
                       c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e", "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h")
                     )),
                   mainPanel(plotOutput("distPlot4"))))),

############## PAGE 4 ##############

    tabPanel("The Model",
             h2("Analyzing Voter Decision-Making"),
             p(
                 "This project's dataset is the first publicly-available data source to provide comprehensive statistics at a basin level for all of the world's major River Basins.
        Combining data from the Oregon State University Program on Water Conflict Transformation, the World Bank, and the EIU Democracy Index, this dataset covers all 254 major river basins of the world from 1960 to 2005 (see the", tags$a("About page", href="https://wyatthurt.shinyapps.io/water_conflict/_w_32445f8c/#tab-9893-5"), "for more information on these data sources):"
             ),
             tags$ul(
                 tags$li(
                     tags$b("Favorability"), "is the total number of water conflict events for each river basin, summed over the period from 1960 to 2005. (source: OSU Program on Water Conflict Transformation)."
                 ),
                 tags$li(
                     tags$b("Ideology"), "is country-level GDP for each year, either summed or averaged across each river basin (source: World Bank)."
                 ),
                 tags$li(
                     tags$b("Viability"), "is country-level population for each year, either summed or averaged across each river basin (source: World Bank)."
                 ),
                 tags$li(
                     tags$b("Electability"), "is calculated at the country level for each year and averaged across each river basin (source: World Bank)."
                 ),
                 tags$li(
                     "All of the above variables can be logged as well, which can improve the quality of regression for clustered data."
                 )
             ),
             p(
                 "Having so many statistics at the river basin level provides a rich opportunity to test various hypotheses about water conflict. Use the dashboard below to construct a model.
        Then, use the sliders at left to predict the expected level of water conflict over a 50 year period, given the selected inputs.
        Note that models at using country-level units and country-year-level units were explored as part of this project, but neither resulted in strong correlations between any of the given explanatory variables and the propensity for water conflict within a given river bain in a given year.
        However, as can be seen below, strong correlations can be observed when data is aggregated at the river basin level over a 50 year period. This suggests that it is very difficult to predict whether water conflict will occur within a given year or given country, but that long-range estimates at the basin level can be quite accurate."
             ),
             sidebarLayout(
                 sidebarPanel(
                     h4("Construct the Model:"),
                     selectInput(
                         "varOI_x",
                         "X variable:",
                         choices = c(
                             "Feeling Thermometer" = "NORMALIZED_FT",
                             "Viability" = "NORMALIZED_VIABILITY",
                             "Electability" = "NORMALIZED_ELECTABILITY",
                             "Ideological Distance" = "NORMALIZED_IDEOLOGY"
                         ),
                         multiple = TRUE,
                         selected = c("NORMALIZED_FT", "NORMALIZED_VIABILITY", "NORMALIZED_ELECTABILITY",
                                      "NORMALIZED_IDEOLOGY")
                     ),
                     selectInput(
                       "varOI_d",
                       "Year:",
                       choices = c(
                         "2008 (D)" = "df08national_d",
                         "2008 (R)" = "df08national_r",
                         "2004 (D)" = "df04national_d",
                         "2000 (D)" = "df00national_d",
                         "2000 (R)" = "df00national_r",
                         "1988 (D)" = "df88st_d",
                         "1988 (R)" = "df88st_r",
                         "1984 (D)" = "df84national_d"
                       ),
                       selected = "df08national_d"
                     ),
                     checkboxInput("toggleLogit", label = "Conditional Logistic Regression", value = TRUE),
                     checkboxInput("toggleMulti", label = "Multivariate Regression", value = FALSE)
                 ),
                 mainPanel(
                     
                     # Output scatterplot with line of best fit based on model.
                     
                     plotOutput("water_regression", height = 500),
                     
                     # Output summary of regression output. I would like to present this in a nicer format over the next 10 days.
                     
                     verbatimTextOutput(outputId = "RegSum"),
                     p("Now, let's convert to factor odds."),
                     
                     # Output predict() results. 
                     
                     verbatimTextOutput(outputId = "predictSum"),
                     
                     # Use CSS styling to hide all error messages. This is necessary
                     # because the scatterplot displays an error if more than 1 X variable
                     # is selected. I ran many tests and could not find a reason why this
                     # would be a problem (i.e. where displaying error results would be
                     # necessary).
                     
                     tags$style(
                         type = "text/css",
                         ".shiny-output-error {display: none;}",
                         ".shiny-output-error:before {display: none;}"
                     )
                 )
             )),

############## PAGE 5 ############## 

    tabPanel("About", 
             h2("Background"),
             p("TODO"),
             p("My Github repository, which includes all files relevant to 
             Milestone 4, can be reached ", a("here.", href = "https://github.com/rzhang28/milestone-4")), 
             h2("The Data"),
             p("My analysis is based on two landmark surveys: the",
               a("American National Election Studies (ANES),",href =
                   "https://www.davidkane.info/files/gov_50_fall_2020.html/"), "and the",
               a("National Annenberg Election Surveys (NAES),",href = 
                 "https://www.davidkane.info/files/gov_50_fall_2020.html/"),
             "both of which sample likely American voters. The ANES has been 
             conducted every four years since 1948, 
             while the NAES was conducted three times in 2000, 2004, and 2008. Between these 
             recurring surveys, I identified the iterations that contained the 
             variables necessary to build our decision-making models: feeling thermometer
             ratings of candidates, assessments of candidate viability and electability, 
             and perceived ideological distance between candidates and respondents."),
             p("In total, I analyzed eight presidential nominating contests spanning 
             24 years, specifically the Democratic nominating contests of 1984, 
             1988, 2000, 2004, and 2008, as well as the Republican nominating contests
             of 1988, 2000, and 2008. Every effort was also made to use ANES data from 1980,
             but those efforts proved ill-fated, as there were insufficient 
             valid cases to support proper analysis."),
             "This research also uses data from the following sources:",
             tags$ol(
               tags$li(
                 a("Google's N-Gram", href = "https://data.worldbank.org"), "accessed using the",
                 a("ngramr() package.", href = "https://cran.r-project.org/web/packages/ngramr/index.html")
               ),
               tags$li(
                 a("Nexis-Uni", href = "https://www.eiu.com/topic/democracy-index"), "accessed using the",
                 a("LexisNexisTools() package.", href = "https://cran.r-project.org/web/packages/LexisNexisTools/index.html")
               ),
               tags$li(
                 "American election and polling data accessed using the",
                 a("politicaldata() package.", href = "https://cran.r-project.org/web/packages/politicaldata/")
               )
             ),
             h2("Acknowledgements"),
             p("This project conceptually builds on several of my previous and 
             current research projects, completed under the supervision of 
             Professor Jennifer Hochschild. I am also grateful for the 
             guidance provided by Professor Jon Rogowski, as well as by 
             Mitchell Kilborn, who helped me learn the art of data science."),
             h2("About Me"),
             p("My name is Ryan Zhang, and I'm a lover of all-things politics.
             I'm also a current junior at Harvard College stuying Social 
             Studies. This is my final project for ",
               a("Gov 1005,",href = 
               "https://www.davidkane.info/files/gov_50_fall_2020.html/"),
               "a data science course taught in R. When I'm not click-clacking
               away in RStudio, you can catch me hanging out at the ",
             a("Institute of Politics,", href = "https://iop.harvard.edu/"), 
               "writing for the ", a("Harvard College Law Review, ", href = "https://hulr.org/"),
             "or exploring Boston with my friends!"),
             p("Thanks for stopping by! Feel free to say hello at ",
               a("ryanzhang@college.harvard.edu",
                 href = "mailto: ryanzhang@college.harvard.edu"), "or on ",
               a("LinkedIn.",
                 href = "https://www.linkedin.com/in/r-zhang/"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  output$figure5 <- renderImage({
    figure5 <- file.path("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/figures/figure_5/figure_5.bmp")
    list(src = figure5, width = 600, length = 1400)
  },
  deleteFile = FALSE
  )
  
  output$figure6 <- renderImage({
    figure6 <- file.path("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/figures/figure_6/figure_6.bmp")
    list(src = figure6, width = 600, length = 1400)
  },
  deleteFile = FALSE
  )
  
  output$figure8 <- renderImage({
    figure8 <- file.path("/Users/ryan/Desktop/Gov-50/american-presidential-primaries/figures/figure_8/figure_8.bmp")
    list(src = figure8, width = 600, length = 1400)
  },
  deleteFile = FALSE
  )

  output$distPlot1 <- renderPlot({
    if(input$plot_type1 == "a") {
      ggplot(df08national_d, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Democratic Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        )
    }
    else if(input$plot_type1 == "b") {
      ggplot(df08national_r, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Republican Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        ) 
    }
    else if(input$plot_type1 == "c") {
      ggplot(df04national_d, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2004 Democratic Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2004)"
        ) 
    }
    else if(input$plot_type1 == "d") {
      ggplot(df00national_d, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Democratic Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type1 == "e") {
      ggplot(df00national_r, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Republican Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type2 == "f") {
      ggplot(df88st_d, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Democratic Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type2 == "g") {
      ggplot(df88st_r, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Republican Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type2 == "h") {
      ggplot(df84national_d, aes(x = NORMALIZED_FT, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1984 Democratic Primaries",
          x = "Normalized Feeling Thermometer Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1984)"
        )
    }
  })
  
  output$distPlot2 <- renderPlot({
    if(input$plot_type2 == "a") {
      ggplot(df08national_d, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Democratic Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        )
    }
    else if(input$plot_type2 == "b") {
      ggplot(df08national_r, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Republican Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        ) 
    }
    else if(input$plot_type2 == "c") {
      ggplot(df04national_d, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2004 Democratic Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2004)"
        ) 
    }
    else if(input$plot_type2 == "d") {
      ggplot(df00national_d, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Democratic Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type2 == "e") {
      ggplot(df00national_r, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Republican Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type2 == "f") {
      ggplot(df88st_d, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Democratic Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type2 == "g") {
      ggplot(df88st_r, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Republican Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type2 == "h") {
      ggplot(df84national_d, aes(x = NORMALIZED_VIABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1984 Democratic Primaries",
          x = "Normalized Viability Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1984)"
        ) 
    }
  })
  
  output$distPlot3 <- renderPlot({
    if(input$plot_type3 == "a") {
      ggplot(df08national_d, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Democratic Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        )
    }
    else if(input$plot_type3 == "b") {
      ggplot(df08national_r, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Republican Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        ) 
    }
    else if(input$plot_type3 == "c") {
      ggplot(df04national_d, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2004 Democratic Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: NAES (2004)"
        ) 
    }
    else if(input$plot_type3 == "d") {
      ggplot(df00national_d, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Democratic Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type3 == "e") {
      ggplot(df00national_r, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Republican Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type2 == "f") {
      ggplot(df88st_d, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Democratic Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type2 == "g") {
      ggplot(df88st_r, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Republican Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type2 == "h") {
      ggplot(df84national_d, aes(x = NORMALIZED_IDEOLOGY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1984 Democratic Primaries",
          x = "Normalized Ideological Distance",
          y = "Number of Respondents",
          caption = "Source: ANES (1984)"
        ) 
    }
  })
  
  output$distPlot4 <- renderPlot({
    if(input$plot_type4 == "a") {
      ggplot(df08national_d, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Democratic Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2008)"
        )
    }
    else if(input$plot_type4 == "b") {
      ggplot(df08national_r, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2008 Republican Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "RSource: NAES (2008)"
        ) 
    }
    else if(input$plot_type4 == "c") {
      ggplot(df04national_d, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2004 Democratic Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2004)"
        ) 
    }
    else if(input$plot_type4 == "d") {
      ggplot(df00national_d, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Democratic Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type4 == "e") {
      ggplot(df00national_r, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "2000 Republican Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: NAES (2000)"
        ) 
    }
    else if(input$plot_type4 == "f") {
      ggplot(df88st_d, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Democratic Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type4 == "g") {
      ggplot(df88st_r, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1988 Republican Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1988)"
        ) 
    }
    else if(input$plot_type4 == "h") {
      ggplot(df84national_d, aes(x = NORMALIZED_ELECTABILITY, fill = Candidate)) +
        geom_histogram(binwidth = 0.1) + 
        facet_wrap(~ Candidate) +
        theme_minimal() +
        labs(
          title = "1984 Democratic Primaries",
          x = "Normalized Electability Rating",
          y = "Number of Respondents",
          caption = "Source: ANES (1984)"
        ) 
    }
    
    output$RegSum <- renderPrint({
      if (input$toggleLogit)
        lmsum <-
          reactive({
            clogit(reformulate(input$varOI_x, VoteCandidate),
               data = input$varOI_d)
          })
      if (input$toggleMulti)
        lmsum <-
          reactive({
            lm(
              reformulate(input$varOI_x, VoteCandidate),
              data = input$varOI_d,
              method = "loess"
            )
          })
      print(summary(lmsum()))
      })
    })
  }

shinyApp(ui = ui, server = server)
