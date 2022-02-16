#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny)
library(dplyr)
library(plotly)
library(magrittr)
library(MASS)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    theme = 'bootstrap.css',

    # Application title
    titlePanel("Sampling and Kernel Density Estimation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                'samples', 'Number of Samples', 
                choices = c(10, 20, 30, 40, 50, 100, 300, 500, 1000, 10000), selected = 10), 
            radioButtons(
                'kernel_auto', 'Automatically Select the Kernel?', 
                choices = c('Yes', 'No')
            ),
            radioButtons(
                'kernel_flat', 'Flat or 2-d kernel?',
                choices = c('Flat', 'Two-Dimensional')
            ),
            sliderInput(
                inputId = 'kernel_size', label = 'Kernel Size', min = 0.5, max = 10.5, step = 1, value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("marin", width = 800, height = 800),
            h1('How Does Sample Size Affect Learning?'),
            'Suppose that you take very many samples.', 
            'Look at how nuanced a statement you can make about the joint density function.', 
            'You can say,', 
            HTML('"Conditional on <math> X = x<sub>i</sub> </math>, what can I say about the empirical probability density function of Y?"'), 
            br(), br(),
            'Without changing the bin-width from being automatically selected, down sample the amount of data to take only 10 samples. Does what you learn from this estimator change? If so, how?', 
            br(), br(), 
            'At what point does is feel like additional sampling density is not improving the precision of what you have learned? Is this the same or different from the "Rule of Thumb of 30" in the CLT?', 
            h1('How Does Kernel Bandwidth Affect Learning?'),
            'Now, control the bandwith of the kernel to explore how this method works.',
            h2('A Flat Kernel'), 
            'The default on this plot is a "flat" kernel. (Note that it is not <it> actually </it> flat, it is just very flat due to the differences in the scales on the axes.', 
            'Set samples to a very small number, and then examine the resulting plot. What shapes do you see?', 
            'Increase the samples. Does this change what you see?', 
            h2('A 2d Kernel'), 
            'Now, set the samples to be very small again, but change the kernel to be two dimensional. What shapes do you see? What do you learn from this data?',
            'Increase the number of samples. How does this change what you learn from the plot?'
        )
    )
    
))
