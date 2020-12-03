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
library(survival)
library(tidyverse)
library(clogitboost)
library(dplyr)

df08national_d <- read.dta("clean_data/df08phone_d.dta")
df08national_r <- read.dta("clean_data/df08phone_r.dta")
df04national_d <- read.dta("clean_data/df04_national_d.dta")
df00national_d <- read.dta("clean_data/df00_national_d.dta")
df00national_r <- read.dta("clean_data/df00_national_r.dta")
df88st_d <- read.dta("clean_data/df88st_d.dta")
df88st_r <- read.dta("clean_data/df88st_r.dta")
df84national_d <- read.dta("clean_data/df84cm_d.dta")

df08phone_d_counts_aggregate <- read.csv("clean_data/df08phone_d_counts_aggregate.csv")
df08phone_d_counts <- read.csv("clean_data/df08phone_d_counts.csv")
df08phone_r_counts_aggregate <- read.csv("clean_data/df08phone_r_counts_aggregate.csv")
df08phone_r_counts <- read.csv("clean_data/df08phone_r_counts.csv")
df04_national_d_counts_aggregate <- read.csv("clean_data/df04_national_d_counts_aggregate.csv")
df04_national_d_counts <- read.csv("clean_data/df04_national_d_counts.csv")
df00_national_d_counts_aggregate <- read.csv("clean_data/df00_national_d_counts_aggregate.csv")
df00_national_d_counts <- read.csv("clean_data/df00_national_d_counts.csv")
df00_national_r_counts_aggregate <- read.csv("clean_data/df00_national_r_counts_aggregate.csv")
df00_national_r_counts <- read.csv("clean_data/df00_national_r_counts.csv")
df88st_d_counts_aggregate <- read.csv("clean_data/df88st_d_counts_aggregate.csv")
df88st_d_counts <- read.csv("clean_data/df88st_d_counts.csv")
df88st_r_counts_aggregate <- read.csv("clean_data/df88st_r_counts_aggregate.csv")
df88st_r_counts <- read.csv("clean_data/df88st_r_counts.csv")
df84cm_d_counts_aggregate <- read.csv("clean_data/df84cm_d_counts_aggregate.csv")
df84cm_d_counts <- read.csv("clean_data/df84cm_d_counts.csv")

# Define UI for application that draws a histogram
ui <- navbarPage(
  theme = shinytheme("flatly"),
  "American Presidential Primaries",

############## PAGE 1 ##############
    
    tabPanel("The Puzzle",
             h2("Setting the Scene"),
             p("Throughout 2019, seven Democrats competed to present their 
             cases for receiving their party’s nomination for the presidency. 
             Much of the contest was spent debating policy disagreements,
             but equally significant emphasis was placed on who 
             would be most likely to beat the Republican incumbent Donald 
             Trump."),
             p("According to polls, concerns about candidate electability loomed 
             among a majority of Democratic primary voters. A national survey of
             likely Democratic primary voters by The Economist asked respondents whether 
             it was more important for the Democratic party to select a nominee 
             who “agrees with your position on most issues” or “can win the general 
             election in November.” 65 percent of respondents prioritized 
             electability, compared to only 35 percent who prioritized policy positions.
             Similarly, in one of the largest exit polls conducted in the race, 
             Edison Media Research found that 33 percent of Democratic primary voters 
             in New Hampshire preferred a nominee who agreed with them on major issues, 
             while 63 percent preferred a nominee who could defeat Trump."),
             
             # Generate a 2x2 column/row layout.
             
             fluidPage(
                 fluidRow(style = 'padding:30px;',
                          column(7, plotOutput("figure5")),
                          column(5,
                                 h3("Mentions of electability have increased over time."),
                                 p("Electability is not a novel phenomenon. At
                                 first, one might believe that voting based
                                 on electability is a unique response to Trump. However, 
                                 electability’s presence steadily risen in our national political discourse. 
                                 Google's N-gram Viewer reveals
                                 the rising usage of “electability” and “electable” in American English. 
                                 Strikingly, mentions of these two
                                 terms were extremely until the 1970s, 
                                 after which mentions rose from significantly, with particularly
                                 steep increases during presidential election years."))),
                 fluidRow(column(5, 
                                 h3("News coverage has increasingly covered electability."),
                                 p("Our findings from Google's N-gram Viewer are 
                                 confirmed by Nexis-Uni’s database of newspaper articles.
                                 Mentions of “electability” and “electable” in newspapers have steadily increased since 
                                 the 1980s, thereby suggesting that the news media is increasingly
                                 focused on the “horse race” rather than issue positions.")),
                          column(7,
                                 plotOutput("figure6"))),
                 fluidRow(style = 'padding:30px;',
                          column(7, plotOutput("figure8")),
                          column(5,
                                 h3("Polls have made it easier than ever to talk about electability."),
                                 p("The past few decades have witnessed a 
                                 dramatic increase in the number of polls 
                                 being conducted during presidential election cycles, 
                                 from 17 polls in 1952 to 308 polls in 2016.
                                 The rise of polls has subsequently affected 
                                 the tone of political media coverage. 
                                 Polls convey critical information about the 
                                 competitivity of each candidate, but they 
                                 often do not reveal substance about candidate 
                                 policy positions."))),
                 h2("Our Goal"),
                 p("In this project, I investigate the behavior of 
                 primary voters, aiming to understand to what extent concerns 
                 about candidate electability influence candidate choice. I 
                 strive to answer two fundamental questions:"),
                 tags$ol(
                   tags$li(
                     p("How heavily dostrategic considerations weigh vis-a-vis 
                     sincere considerations when primary voters cast their ballots?")),
                   tags$li(
                     p("In any given election, what percentage of primary voters 
                     vote strategically?"))),
                 p("In addressing these questions, I will study eight 
                 nominating contests that span 30 years. Political scientist 
                 James Ceaser once declared, “No systematic
                 theory about primary voting is likely to develop for some time 
                 because each campaign is so different.” I attempt to meet Ceaser’s challenge."))),
                   
############## PAGE 2 ##############

tabPanel("Sincere Considerations",
         fluidPage(
           h2("The Feeling Thermometer"),
           p("The variables in a voter's decision-making calculus can be boiled
           down to two categories: sincere considerations and strategic 
           considerations. Two variables comprise a voter's sincere considerations, the 
           first of which is favorability."),
           p("The National American Election Studies (NAES) and Annenberg National
           Election Studies (ANES) both extensively use feeling thermometers, which 
           ask respondents to rate candidates on a scale that ranges from 0–100, 
           where 0 is very unfavorable and 100 is very favorable."),
           p("One difficulty in using feeling thermometers is that respondents provide 
           ranges of ratings that vary considerably. For example, one respondent 
           might rate his or her favorite and least favorite candidate 90 and 10, 
           respectively, while another might rate his or her favorite and least favorite
           candidate 60 and 40, respectively. However, the utility of feeling 
           thermometers lies not in raw scores, but in the relative distance of values
           recorded between the candidates for each respondent. Thus, to make responses comparable between 
           respondents, we normalized eachrating according to the following formula, 
           which is derived from similar conventions in the political science literature:"),
           p('$$u_j = \\frac{U_j - U_n}{U_1 - U_n}$$'),
           p("In this formula, ui is the “normalized” feeling thermometer 
           value, Uj is the original feeling thermometer value, U1 is the 
           highest thermometer rating used by the respondent for any candidate, and Un is the lowest. 
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
                 c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e", "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h"))),
           mainPanel(plotOutput("distPlot1"))),
           h2("Ideological Distance"),
           p("The second constituent variable of sincere considerations is ideological distance. 
           In both the ANES and NAES, respondents are asked to rank themselves, 
           as well as each of the primary candidates, on an ideological scale, from 1 
           (“most liberal”) to 5 (“most conservative”)."),
           p("We are less concerned about the absolute values of each respondent’s 
           rankings, but more interested in the distance between where respondents placed themselves 
           on the ideological scale and where they placed each candidate. As such, we calculate 
           ideological distance by taking the difference between each respondent’s ranking 
           of themselves and of the candidate, squaring the difference (since otherwise 
           positive and negative differences would cancel each other out), and scaling to 
           1 to make later comparisons between coefficients in our model easier to interpret. 
           These calculations can be summarized by the following formula:"),
           withMathJax(),
           p('$$d_j = \\frac{(D_c - D_r)^2}{5}$$'),
           p("In this formula, dj is the normalized ideological distance, 
           Dc is the respondent’s ranking of a candidate’s ideology, and Dr 
           is the respondent’s ranking of his or her own ideology. We expect that 
           the farther away ideologically a voter is from a candidate, the less likely 
           he or she is to vote for that candidate."),
           titlePanel(""),
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "plot_type3",
                 "Election Year (Party)",
                 c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e", "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h"))),
             mainPanel(plotOutput("distPlot3"))))),

############## PAGE 3 ##############

tabPanel("Strategic Considerations",
         fluidPage(
           withMathJax(),
           p("Equally important in a voter's decision-making calculus are strategic concerns.
           Respondents in both the ANES and NAES are asked to assess the chances 
           of each primary candidate winning the 
           party’s nomination (viability) and the general election (electability)."),
           p("Since we are interested in respondents’ relative rankings of candidates,
           and since the responses for all candidates frequently summed to 
           over 100 percent, we normalized responses to both the viability and
           electability questions according to the below equation, where Pj is 
           the raw score given by the respondent, n is the number of candidates 
           that were rated, and pj is the normalized score for a given candidate:"),
           p('$$p_j = \\frac{P_j}{(P_1+ ... + P_n)}$$'),
           p("Using this formula, each respondent's responses sum to 1.
           For example, one respondent might
           indicate that five candidates each have a 12 percent chance of winning their party's nomination 
           and a sixth candidate has an 80 percent chance of winning. The
           first five would get a normalized score of .08 and the sixth 
           candidate a 0.60. To be clear, this does not suggest that the sixth 
           candidate had a 60 percent chance of winning the election, but instead, 
           the 0.60 represents the proper proportion of the 150 percentage points the 
           respondent initially allocated out."),
           titlePanel("Viability"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type2",
                             "Election Year (Party)",
                             c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e",  "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h"))),
                     mainPanel(plotOutput("distPlot2"))),
                 
                 titlePanel("Electability"),
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "plot_type4",
                       "Election Year (Party)",
                       c("2008 (D)" = "a", "2008 (R)" = "b", "2004 (D)" = "c", "2000 (D)" = "d", "2000 (R)" = "e", "1988 (D)" = "f", "1988 (R)" = "g", "1984 (D)" = "h"))),
                   mainPanel(plotOutput("distPlot4"))))),

############## PAGE 4 ##############

    tabPanel("The Model",
             h2("Putting It Together"),
             p("Having explored each of our four variables, we now put them together 
             into a model that reveals their relative importance in voter decision-making.  
             To summarize, our four variables of interest are:"
             ),
             tags$ul(
                 tags$li(
                     tags$b("Favorability:"), "the extent to which a voter finds a particular candidate personally favorable, as measured by a feeling thermometer."
                 ),
                 tags$li(
                     tags$b("Ideological Distance:"), "the absolute distance between a voter's ideology and that of a candidate."
                 ),
                 tags$li(
                     tags$b("Viability:"), "the chances that a voter believes a candidate has at winning his or her party's nomination."
                 ),
                 tags$li(
                     tags$b("Electability:"), "the chances that a voter believes a candidate has at winning the general election in November."
                 )
             ),
             p("Our model of choice is conditional logistic regression, 
             an extension of logistic regression that incorporates 
             stratification. 
             Conditional logistic regressions offer several advantages.
             First, whereas logistic models only allow for one 
             possible outcome, conditional logistic models allow for multiple -- 
             a feature indispensable for modeling multi-candidate elections. 
             Second, conditional logistic regressions facilitate analysis of 
             explanatory variables that are choice specific (attributes of the 
             choice alternatives), as well as variables that are individual 
             specific (characteristics of the individuals making the choices). 
             Economists frequently use 
             conditional logistic models, but political scientists have only recently 
             drawn on spatial modeling conditioned on characteristics 
             of the choices, like conditional logistic modeling, to inform analyses
             of multi-candidate elections."),
             p("In each regression, the candidate who went on to eventually 
             the party's nomination that year serves as the baseline."),
             sidebarLayout(
                 sidebarPanel(
                     h4("Choose the Model:"),
                     selectInput(
                       "varOI_d",
                       "Election Year (Party):",
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
                       multiple = FALSE,
                       selected = "df08national_d"
                     ),
                 ),
                 mainPanel(
                     verbatimTextOutput(outputId = "RegSum"),
                     tags$style(
                         type = "text/css",
                         ".shiny-output-error {display: none;}",
                         ".shiny-output-error:before {display: none;}"
                     )
                 )
             ),
             h2("Intepretation"),
             p("To interpret conditional logistic regressions, the coefficients
             indicate that for every one-unit increase in that variable, the
             effect on the natural log of the odds of selecting one candidate
             over another candidate changes by the value of that coefficient. 
             Because each variable is scaled 
             from 0 to 1, a one-unit increase in that variable represents 
             the entire range of the variable. Conditional logistic 
             regressions allow us to directly compare the impact of 
             each variable on choice relative to another by comparing the 
             coefficients generated in the model."),
             p("The effect of a given variable is easier to interpret when 
             presented as an odds ratio, which is calculated by taking the 
             natural exponent of the coefficient: e^β.  A one-unit increase 
             of a variable, holding other variables constant, leads to 
             a factor change in the odds of selecting one candidate over
             another of that value. For example, if a respondent were 
             to change his or her viability rating for a candidate from 
             a 0 to a 1, and if the odds of him or her selecting that candidate 
             increase by a factor of 12.607, the respondent is 12.607 
             times more likely to select that candidate.")),

############## PAGE 5 ############## 

tabPanel("Typology of Voters",
         h2("Four Types of Voters"),
         p("Combining strategic and sincere considerations, we can then create a 
         four-part typology for voter behavior:"),
         tags$ul(
           tags$li(
             tags$b("Ideal Voter:"), "A voter who casts his or her ballot for the candidate that he or she ranked most sincere and most strategic."
           ),
           tags$li(
             tags$b("Expressive Voter:"), "A voter who casts his or her ballot for the candidate that he or she ranked most sincere but not most strategic."
           ),
           tags$li(
             tags$b("Instrumental Voter:"), "A voter who casts his or her ballot for the candidate that he or she ranked most strategic but not most sincere."
           ),
           tags$li(
             tags$b("Irrational Voter:"), "A voter who casts his or her ballot for the candidate that he or she ranked neither most sincere nor most strategic."
           )
         ),
         p("As seen in the tables below, the majority of ANES and NAES respondents are ideal voters, 
           followed by expressive voters and then instrumental voters. Overall, these findings
           suggest that roughly 10 percent of primary voters in any given election cycle 
           cast strategic votes. The three candidates who received the greatest 
           percentages of expressive votes were Jesse Jackson, John Edwards, and John McCain,
           while the three candiates who received the greatest percentages of instrumental 
           votes were George W. Bush, Al Gore, and Walter Mondale."
         ),
         sidebarLayout(
           sidebarPanel(
             h4("Construct the Model:"),
             selectInput(
               "tableYear1",
               "Year",
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
               multiple = FALSE,
               selected = "df08national_d"
             ),
             checkboxInput("toggleCandidate", label = "Disaggregate by Candidate", value = FALSE)
           ),
           mainPanel(
             dataTableOutput("table1"),
           )
         )),

############## PAGE 6 ############## 

    tabPanel("About", 
             h2("Background"),
             p("In recent years, the terms “electability” and “viability” have 
             become increasingly common in our nation’s political discourse. 
             As political polarization worsens, political parties seem to be 
             less interested in participating in a contest of ideas and 
             instead fixated on “beating the other guy.” News coverage, 
             in a bid for viewership, has shifted its focus away from policy 
             issues and towards the “horse race.”"),
             p("Despite its prevalence, strategic voting is understudied 
               in the political science literature.",
               a("Brady and Johnston (1987)",href =
                   "https://d1wqtxts1xzle7.cloudfront.net/40269085/The_Study_of_Political_Campaigns20151122-786-133uwx1.pdf?1448220863=&response-content-disposition=inline%3B+filename%3DThe_study_of_political_campaigns.pdf&Expires=1606968470&Signature=ONYbGzKbhoxDbRMZfau-4eGLE1unYssPj-gUkRfqidhI-R~NEPLxegiPlWNwTSUZAaIIdSOIFJ34tq0G1-g3yGHIavL4ygWh76zz6jOFdaJZ6X7kKKdqEiYTxre7BHf4gQUDsJB~cytD1MKF32sNCLBxTYyHAR2WCLtui7Nr01W5tJ2p-kwqJIgL8-paQ90zJdPs-CQ25p~FM9pZ7MbNjy0hyZqcaxrQ-iL5f5MggTG0U-okkB8OwsqNzCoGcyO-REcdsuJquxp0zM7CkwqjR0b7RBRYBF3BtwjAPFHcu2duQEGlKAXziSTEI1O6XHkpqwHZWCtDrPDh5zueGWvX-g__&Key-Pair-Id=APKAJLOHF5GGSLRBV4ZA"), 
               "studied electability during the 1984 Democratic primaries, but 
               their findings are limited by their binary models, which assume
               primaries to be two candidate affairs when, in reality, most 
               primary contests include a multitude of candidates. Other 
               scholars, led by",
               a("Rickershauser and Aldrich (2007),", href =
                   "https://www.sciencedirect.com/science/article/abs/pii/S0261379406000977"), 
               "argue that 
               electability considerations are on par with policy concerns.
               However, since their work was done in laboratory settings, 
               rather than surveying real voters in real elections, these 
               findings face questions of ecological validity. This project
               aims to fill that research gap combining eight large datasets 
               from 1984-2008. In so doing, it seeks to take a first step toward
               evaluating the nature and magnitude of strategic considerations 
               in American presidential primaries."),
             p("My Github repository, which includes all files relevant to 
             this project, can be reached ", a("here.", href = "https://github.com/rzhang28/american-presidential-primaries")), 
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
             "My research also uses data from the following sources:",
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
             I'm also a senior at Harvard College stuying Social 
             Studies. This is my final project for ",
               a("Gov 50,",href = 
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
    figure5 <- file.path("figures/figure_5/figure_5.bmp")
    list(src = figure5, width = 650, length = 1450)
  },
  deleteFile = FALSE
  )
  
  output$figure6 <- renderImage({
    figure6 <- file.path("figures/figure_6/figure_6.bmp")
    list(src = figure6, width = 600, length = 1400)
  },
  deleteFile = FALSE
  )
  
  output$figure8 <- renderImage({
    figure8 <- file.path("figures/figure_8/figure_8.bmp")
    list(src = figure8, width = 650, length = 1450)
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
  })
    
    output$RegSum <- renderPrint({
      if(input$varOI_d == "df08national_d")
        lmsum <- clogit(VoteCandidate ~ CLINTON + EDWARDS + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                        NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df08national_d)
      if(input$varOI_d == "df08national_r")
        lmsum <- clogit(VoteCandidate ~ ROMNEY + HUCKABEE + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df08national_r)
      if(input$varOI_d == "df04national_d")
        lmsum <- clogit(VoteCandidate ~ EDWARDS + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df04national_d)
      if(input$varOI_d == "df00national_d")
        lmsum <- clogit(VoteCandidate ~ BRADLEY + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df00national_d)
      if(input$varOI_d == "df00national_r")
        lmsum <- clogit(VoteCandidate ~ MCCAIN + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df00national_r)
      if(input$varOI_d == "df88st_d")
        lmsum <- clogit(VoteCandidate ~ GEPHARDT + GORE + HART + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df88st_d)
      if(input$varOI_d == "df88st_r")
        lmsum <- clogit(VoteCandidate ~ DOLE + KEMP + ROBERTSON + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df88st_r)
      if(input$varOI_d == "df84national_d")
        lmsum <- clogit(VoteCandidate ~ HART + JACKSON + NORMALIZED_FT + NORMALIZED_VIABILITY + 
                          NORMALIZED_ELECTABILITY + NORMALIZED_IDEOLOGY + strata(ID),
                        data = df84national_d)
      print(summary(lmsum))
    })
    
    output$table1 <- renderDataTable({
      if(input$tableYear1 == "df08national_d") 
        if(input$toggleCandidate)
          df08phone_d_counts
        else
          df08phone_d_counts_aggregate
      else if(input$tableYear1 == "df08national_r") 
        if(input$toggleCandidate)
          df08phone_r_counts
        else
          df08phone_r_counts_aggregate
      else if(input$tableYear1 == "df04national_d") 
        if(input$toggleCandidate)
          df04_national_d_counts
        else
          df04_national_d_counts_aggregate
      else if(input$tableYear1 == "df00national_d") 
        if(input$toggleCandidate)
          df00_national_d_counts
        else
          df00_national_d_counts_aggregate
      else if(input$tableYear1 == "df00national_r") 
        if(input$toggleCandidate)
          df00_national_d_counts
        else
          df00_national_d_counts_aggregate
      else if(input$tableYear1 == "df88st_d") 
        if(input$toggleCandidate)
          df88st_d_counts
        else
          df88st_d_counts_aggregate
      else if(input$tableYear1 == "df88st_r") 
        if(input$toggleCandidate)
          df88st_r_counts
        else
          df88st_r_counts_aggregate
      else if(input$tableYear1 == "df84national_d") 
        if(input$toggleCandidate)
          df84cm_d_counts
        else
          df84cm_d_counts_aggregate
    })
  }

shinyApp(ui = ui, server = server)
