library(shiny)

source("repeat_game.R")
source("rep_plot.R")
source("kinetics_reactions/monomolecular.R")
source("kinetics_reactions/bimolecular.R")
source("kinetics_reactions/catalytic.R")
source("kinetics_reactions/autocatalytic.R")
source("kinetics_reactions/consecutive.R")
source("kinetics_reactions/equilibrium.R")
source("multiple_repeat_game.R")
source("multiple_rep_plot.R")
source("create_caption.R")

server = function(input, output, session) {
    
    spectrum = reactive({
            out = repeat_game(reps = input$repeats,
                              game = input$games,
                              a = input$a,
                              b = input$b,
                              x = input$x,
                              cycles = input$time,
                              coins = input$probability)
            out
    })
    
    multiple_spectrum = reactive({
        out = multiple_repeat_game(reps = input$repeats_multi,
                                   game = input$multiple_games,
                                   a = input$a_multi,
                                   b = input$b_multi,
                                   x = input$x_multi,
                                   cycles = input$time_multi,
                                   coins = input$probability_multi)
        out        
    })
    
    output$instruction_image = renderImage({
        return(list(
            src = "images/kingames.png", 
            contentType = "image/png", 
            width = 900, height = 300, 
            alt = "image of instruction"))
     }, deleteFile = FALSE
    )
    
    getPage = function(){
        return(includeHTML("captions/instruction.html"))
    }

    output$instruction = renderUI({
        getPage()
    })
 
    output$graph = renderPlot({
            rep_plot(file = spectrum(),
                     a_individual = 1 %in% input$display, 
                     b_individual = 2 %in% input$display, 
                     x_individual = 3 %in% input$display, 
                     a_average = 4 %in% input$display, 
                     b_average = 5 %in% input$display, 
                     x_average = 6 %in% input$display)
    })
    
    output$caption = renderUI({
        create_caption(input$games)
    })
    
    output$multiple_graphs = renderPlot({
        multiple_rep_plot(file = multiple_spectrum(),
                          a_individual = 1 %in% input$display_multi, 
                          b_individual = 2 %in% input$display_multi, 
                          x_individual = 3 %in% input$display_multi, 
                          a_average = 4 %in% input$display_multi, 
                          b_average = 5 %in% input$display_multi, 
                          x_average = 6 %in% input$display_multi)
    })
    
    observe({
        selected_game = input$games
        selected_repeat = input$repeats
        update_a = 140
        update_b = 0
        update_x = 0
        update_display_x = FALSE
        update_probability = 100
        if(selected_game == "monomolecular"){
            update_a = 140
            update_b = 0
            update_x = 0
        } else if(selected_game == "bimolecular"){
            update_a = 140
            update_b = 0
            update_x = 0        
        } else if(selected_game == "catalytic"){
            update_a = 70
            update_b = 0
            update_x = 40
        } else if(selected_game == "autocatalytic"){
            update_a = 139
            update_b = 1
            update_x = 0
        } else if(selected_game == "consecutive"){
            update_a = 140
            update_b = 0
            update_x = 0
            update_display_x = TRUE
        } else {
            update_a = 140
            update_b = 0
            update_x = 0
        }
        updateSliderInput(session, "a", value = update_a)
        updateSliderInput(session, "b", value = update_b)
        updateSliderInput(session, "x", value = update_x)
        if(update_display_x){
            updateCheckboxGroupInput(session, "display", selected = c(1,2,3,4,5,6))
        }
        updateSliderInput(session, "probability", value = update_probability)
    })

}
