#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggtext)
library(scales)
library(shinythemes)
library(ggh4x)

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("slate"),
    
    # Application title
    titlePanel("Simple Regression with Standard Scores (M=100, SD=15)"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "r",
                "Correlation between X & Y:",
                min = 0,
                max = 1,
                value = 0.6,
                step = 0.01,
                round = 2
            ),
            sliderInput(
                inputId = "x",
                "Predictor Score (X):",
                min = 40,
                max = 160,
                value = 70,
                step = 1,
                width = "100%"
            ),
            sliderInput(
                inputId = "y",
                "Outcome Score (Y):",
                min = 40,
                max = 160,
                value = 80,
                step = 1,
                width = "100%"
            ),
            htmlOutput("yhat"),
            sliderInput(
                inputId = "x2",
                "Comparison X:",
                min = 40,
                max = 160,
                value = 100,
                step = 1,
                width = "100%"
            ),
            checkboxInput("showRR", "Show Comparison Distribution")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput(
            "distPlot", height = "600px", width = "800px"
        ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # Table ----
    output$yhat <- renderTable({
        mu <- 100L
        sigma = 15L
        yhat <- input$r * (input$x - mu) + mu
        see = sigma * sqrt(1 - input$r ^ 2)
        data.frame(
            Statistic = c(
                "Predicted Outcome (<em>Y&#770;</em>)",
                "Std. Error (&sigma;<sub>e</sub>)",
                "Prediction Error<br>(<em>e = Y &minus; Y&#770;</em>)",
                paste0("P(<em>Y</em> < ", input$y, ")"),
                paste0("P(<em>Y</em> < ", input$y, " | <em>X</em> = ", input$x, ")"),
                paste0("P(<em>Y</em> < ", input$y, " | <em>X</em> = ", input$x2, ")"),
                paste0(
                    "Relative Risk =<br>P(<em>Y</em> < ",
                    input$y ,
                    " | <em>X</em> = ",
                    input$x,
                    ") &divide;<br> P(<em>Y</em> < ",
                    input$y ,
                    " | <em>X</em> = ", input$x2, ")"
                )
            ),
            Value = c(
                number(yhat, 0.1),
                number(see, 0.1),
                number(input$y - yhat, 0.1),
                number(pnorm(input$y, mu, sigma), 
                       accuracy = 0.001),
                number(pnorm(input$y, yhat, see), 
                       accuracy = 0.001),
                number(pnorm(input$y, mu, see), 
                       accuracy = 0.001),
                number(pnorm(input$y, yhat, see) / 
                           pnorm(input$y, input$x2, see), 
                       accuracy = 0.1)
            )
        )
    }, sanitize.text.function = function(x)
        x, align = "lr")
    
    # Plot ----
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        mu <- 100L
        sigma = 15L
        yhat <- input$r * (input$x - mu) + mu
        yhat2 <- input$r * (input$x2 - mu) + mu
        see = sigma * sqrt(1 - input$r ^ 2)
        yhat_height <- 0.5
        sigma_height = dnorm(mu + sigma, mu, sigma) / 
            dnorm(mu, mu, sigma)
        myblue <- muted("royalblue", l = 75)
        mygray <- "gray80"
        myred <- muted("firebrick", l = 65, c = 110)
        dxy = abs(input$x - input$y)
        
        ## d_label ----
        d_label <- tibble(
            x = c(input$x,
                  input$y,
                  yhat,
                  yhat + see * 0.5,
                  mu,
                  mu + sigma * 0.5),
            y = c(
                ifelse(dxy < 10, 
                       0.06 * sqrt(10^2 - dxy ^ 2) / 10, 
                       0),
                0.00,
                yhat_height,
                yhat_height * sigma_height,
                1,
                sigma_height
            ),
            label = c(
                paste0("*X* = ", round(input$x)),
                paste0("*Y* = ", round(input$y)),
                paste0("*Ŷ* = ", round(yhat, 1)),
                paste0("*&sigma;<sub>e</sub>* = ", round(see, 1)),
                paste0("*μ* = ", mu),
                paste0("*&sigma;* = ", sigma)
            ),
            vjust = c(-0.2, -0.2, -0.2, -0.2, -0.2,-0.2),
            hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
            color = c(
                myred,
                "orchid1",
                myblue,
                myblue,
                mygray,
                mygray
            ),
            nudge_x = c(sign(input$x - mu) * -1,
                        sign(input$y - mu) * -1,
                        0,0,0,0),
            label_x = x + nudge_x
        )
        
        ggplot(data.frame(x = c(mu - 4L * sigma, 
                                mu + 4L * sigma)), 
               aes(x)) +
            stat_function(
                fun = function(x)
                    dnorm(x, mu, sigma) / dnorm(mu, mu, sigma),
                n = 801,
                geom = "area",
                fill = "white",
                alpha = 0.1
            ) +
            stat_function(
                xlim = c(yhat2 - see * 4, yhat2 + see * 4),
                
                n = 801,
                fun = function(x)
                    yhat_height * dnorm(x, yhat2, see) / dnorm(yhat2, yhat2, see),
                fill = "firebrick",
                color = NA,
                alpha = 0.3 * input$showRR,
                geom = "area"
            ) +
            stat_function(
                xlim = c(yhat - see * 4, yhat + see * 4),
                
                n = 801,
                fun = function(x)
                    yhat_height * dnorm(x, yhat, see) / dnorm(yhat, yhat, see),
                fill = "royalblue",
                color = NA,
                alpha = 0.2,
                geom = "area"
            ) +
            stat_function(
                xlim = c(yhat - see * 2, yhat + see * 2),
                n = 801,
                fun = function(x)
                    yhat_height * dnorm(x, yhat, see) / dnorm(yhat, yhat, see),
                fill = "royalblue",
                color = NA,
                alpha = 0.2,
                geom = "area"
            ) +
            stat_function(
                xlim = c(yhat - see * 1, yhat + see * 1),
                n = 801,
                fun = function(x)
                    yhat_height * dnorm(x, yhat, see) / 
                    dnorm(yhat, yhat, see),
                fill = "royalblue",
                color = NA,
                alpha = 0.2,
                geom = "area"
            ) +
            geom_point(aes(
                x = x,
                y = y,
                color = color
            ), data = d_label[1:2, ]) +
            geom_richtext(
                family = "Segoe UI",
                data = d_label,
                size = 5,
                aes(
                    x = label_x,
                    y = y,
                    label = label,
                    hjust = hjust,
                    vjust = vjust,
                    color = color
                ),
                label.padding = unit(0, "mm"),
                label.color = NA,
                fill = NA
                
            ) +
            scale_x_continuous(
                "Standard Scores",
                breaks = seq(mu - 4L * sigma, mu + 4L * sigma, sigma),
                minor_breaks = seq(mu - 4L * sigma, 
                                   mu + 4L * sigma, sigma / 3L)
            ) +
            scale_y_continuous(
                NULL,
                breaks = NULL,
                minor_breaks = NULL,
                expand = expansion()
            ) +
            theme_dark(base_size = 16, base_family = "Segoe UI") +
            guides(x = "axis_minor") +
            theme(
                plot.background = element_rect("#272b30", color = NA),
                panel.background = element_rect("#272b30"),
                axis.title.x = element_text(colour = mygray),
                axis.text.x = element_text(colour = mygray),
                axis.ticks.x = element_line(colour = mygray),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                ggh4x.axis.ticks.length.minor = rel(0.5)
            ) +
            coord_cartesian(
                clip = 'off',
                xlim = c(mu - 4L * sigma, mu + 4L * sigma),
                ylim = c(0, 1.07)
            ) +
            scale_color_identity() +
            geom_segment(
                data = tibble(
                    x = c(mu, yhat),
                    y = sigma_height * c(1, yhat_height),
                    xend = c(mu + sigma, yhat + see),
                    color = c(mygray, myblue)
                ),
                aes(
                    y = y,
                    yend = y,
                    xend = xend,
                    color = color
                ),
                arrow = arrow(
                    angle = 15,
                    type = "closed",
                    length = unit(3, "mm")
                )
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
