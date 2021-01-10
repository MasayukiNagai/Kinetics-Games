library(shiny)
library(shinythemes)
library(bsplus)
library(htmltools)
library(magrittr)

ui<-fluidPage(theme = shinytheme("flatly"),
              navbarPage("Kinetic Games",
                         tabPanel("Instruction",
                                  titlePanel("Kinetic Games"),
                                  fluidRow(column(12, align = "center",
                                         imageOutput("instruction_image")
                                         )),
                                  htmlOutput("instruction")
                         ),
                         
                         tabPanel("Play Games",
                                  sidebarPanel(
                                    selectInput("games","Type of Reaction: ",
                                                choices = list("monomolecular", "bimolecular", "catalytic", "autocatalytic", "consecutive", "equilibrium"), 
                                                selected = "monomolecular"),
                                    sliderInput("a", "A: ",
                                                       min = 0, max = 150, value = 100
                                    ) %>%
                                    shinyInput_label_embed(
                                      shiny_iconlink(name = "question-circle") %>%
                                        bs_embed_tooltip(title = "The initial number of A-balls",
                                                         placement = "left")
                                    ),
                                    sliderInput("b", "B: ",
                                                min = 0, max = 150, value = 0) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "The initial number of B-balls",
                                                           placement = "left")
                                      ),
                                    sliderInput("x", "X: ",
                                                min = 0, max = 150, value = 0) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "The initial number of X-balls",
                                                           placement = "left")
                                      ),
                                    sliderInput("time", "Time: ",
                                                min = 1, max = 1000, value = 500) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "How many times you draw balls from a box (X-axis)",
                                                           placement = "left")
                                      ),
                                    sliderInput("probability", "Probability %: ",
                                                min = 0, max = 100, value = 100,
                                                animate = animationOptions(interval = 500, playButton = c("Play Simulation"), pauseButton = c("Stop Simulation"))) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "A probability that a reaction will occur after drawing ball(s)",
                                                           placement = "left")
                                      ),
                                    sliderInput("repeats", "Repeat: ",
                                                min = 1, max = 100, value = 1) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "How many times you repeat a simulation (individual thin lines) *Play Simulation does not work more than 10 repeats",
                                                           placement = "left")
                                      ),
                                    checkboxGroupInput("display", "Display: ", 
                                                       c("A(individuals)" = 1, "B(individuals)" = 2, "X(individuals)" = 3, "A(average)" = 4, "B(average)" = 5, "X(average)" = 6 ), selected = c(1,2,4,5), inline = TRUE),
                                    hr(),
                                    helpText("Version 1.0. Developed by Moon as a Chem project in Summer 2019")
                                    
                                  ),
                                  mainPanel(plotOutput("graph", height = "600px"),
                                            htmlOutput("caption")
                                  ),
                                  use_bs_tooltip(),
                                  use_bs_popover()
                         ),
                         
                         tabPanel("Compare Multiple Games",
                                  sidebarPanel(
                                    sliderInput("a_multi", "A: ",
                                                min = 0, max = 150, value = 140) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "The initial number of A-balls",
                                                           placement = "left")
                                      ),
                                    sliderInput("b_multi", "B: ",
                                                min = 0, max = 150, value = 0)%>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "The initial number of B-balls",
                                                           placement = "left")
                                      ),
                                    sliderInput("x_multi", "X: ",
                                                min = 0, max = 150, value = 0) %>%
                                    shinyInput_label_embed(
                                      shiny_iconlink(name = "question-circle") %>%
                                        bs_embed_tooltip(title = "The initial number of X-balls",
                                                         placement = "left")
                                    ),
                                    sliderInput("time_multi", "Time: ",
                                                min = 1, max = 1000, value = 500) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "How many times you draw balls from a box (X-axis)",
                                                           placement = "left")
                                      ),
                                    sliderInput("probability_multi", "Probability %: ",
                                                min = 0, max = 100, value = 100) %>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "A probability that a reaction will occur",
                                                           placement = "left")
                                      ),
                                    sliderInput("repeats_multi", "Repeat: ",
                                                min = 1, max = 100, value = 5)%>%
                                      shinyInput_label_embed(
                                        shiny_iconlink(name = "question-circle") %>%
                                          bs_embed_tooltip(title = "How many times you repeat a simulation (individual thin lines)",
                                                           placement = "left")
                                      ),
                                    checkboxGroupInput("display_multi", "Display: ", 
                                                       c("A(individuals)" = 1, "B(individuals)" = 2, "X(individuals)" = 3, "A(average)" = 4, "B(average)" = 5, "X(average)" = 6 ), selected = c(1,2,4,5), inline = TRUE),
                                    hr(),
                                    helpText("Version 1.0. Developed by Moon as a Chem project in Summer 2019")
                                  ),
                                  mainPanel(
                                    plotOutput("multiple_graphs",height = "600px"),
                                    checkboxGroupInput("multiple_games","Select games which you want to disiplay: ",
                                                       c("monomolecular (A:pink, B:red)" = "monomolecular", 
                                                         "bimolecular (A:lightblue, B:blue)" = "bimolecular", 
                                                         "catalytic (A:yellow, B:orange)" = "catalytic",
                                                         "autocatalytic (A:lightbrown, B:brown)" = "autocatalytic", 
                                                         "consecutive (A:greenyellow, B:green)" = "consecutive", 
                                                         "equilibrium (A:violet, B:purple)" = "equilibrium"),
                                                       selected = c("monomolecular", "bimolecular"), inline = TRUE
                                    )
                                  )
                              )
                  )
)

                                  
                                  

                         
                         
              
            
              
              
              

