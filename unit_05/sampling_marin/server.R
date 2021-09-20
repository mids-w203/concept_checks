#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(plotly)
library(magrittr)
library(MASS)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$marin <- renderPlotly({

        d <- data.frame(
            id = 1:10000,
            x = c(
                runif(n=5000, min = 0, max = 10),
                runif(n=5000, min = 5, max = 15)
            ), 
            w = rnorm(n = 10000, mean = 0, sd = 3)
        )
        
        d <- d %>%  
            mutate(
                y = ifelse(
                    id <= 5000, 
                    10 - 2*x + x**2 + w,
                    -10 + 20*x - (x - 5)**2 + w
                ))
        if(input$kernel_auto == 'Yes') { 
            d %>% 
                filter(id %in% sample(1:n(), size = input$samples, replace = FALSE)) %$%
                kde2d(x=x,y=y, n=300) %$%
                plot_ly(x=x,y=y,z=z) %>%  
                add_surface()
        } else if (input$kernel_auto == 'No') {
            if(input$kernel_flat == 'Flat') { 
                d %>% 
                    filter(id %in% sample(1:n(), size = input$samples, replace = FALSE)) %$%
                    kde2d(x=x,y=y, h=input$kernel_size, n=300) %$%
                    plot_ly(x=x,y=y,z=z) %>%  
                    add_surface()
            } else if(input$kernel_flat == 'Two-Dimensional') { 
                d %>% 
                    filter(id %in% sample(1:n(), size = input$samples, replace = FALSE)) %$%
                    kde2d(x=x,y=y, h=c(input$kernel_size, input$kernel_size * 10), n=300) %$%
                    plot_ly(x=x,y=y,z=z) %>%  
                    add_surface()
                }
            }

    })

})
