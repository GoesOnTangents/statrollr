library(shiny)
library(dplyr)
library(shinyjs)

distributionEnum <- function() {
    list(RNORM = "normal distribution",
         RUNIF = "uniform distribution",
         RBINOM = "binomial distribution")
}
distributions <- distributionEnum()
pointbuycosts <- c(-Inf,
                   -Inf,
                   -9,#3
                   -6,
                   -4,
                   -2,
                   -1,#7
                   0,
                   1,
                   2,
                   3,
                   4,
                   5,#13
                   7,
                   9,
                   12,
                   15,
                   19)

run_attribute_creation <- function(a) {
    min_pbv <- a$pbv_target
    max_pbv <- a$pbv_target + 3
    min_high_stat <- a$high_stat_min
    distribution_type <- a$distribution
    
    rollattrs <- function() {
        attrs <- c(11,11,11,11,11,11)
        if(distribution_type == distributions$RNORM) {
            #each stat has a 0.01 chance for an 18.
            #probability for at least one 18: 0.057, two: 0.00144
            attrs <- pmax(pmin(as.integer(round(rnorm(6,11,3))), 18), 6)
        } 
        if(distribution_type == distributions$RUNIF) {
            #each stat has a 0.042 chance for an 18.
            #probability for at least one 18: 0.2, two: 0.02
            attrs <- as.integer(round(runif(6, 6, 18)))
        } 
        if(distribution_type == distributions$RBINOM) {
            #avg 11-13
            #6-15 realistic range
            #16 has a 0.0046 chance
            #probability for at least one 16: 0.02697
            #probability for at least one 15: 0.12566
            attrs <- rbinom(6, 18, 0.65)
        }
        attrs
    }
    
    modifiers <- function(attrs) {
        as.integer(attrs/2-5)
    }
    
    ensure_one_high <- function(attrs) {
        ret <- attrs
        if (max(attrs) < min_high_stat) {
            ret[first(which.max(attrs))] <- min_high_stat
        }
        ret
    }
    
    point_buy_value <- function(attrs) {
        sum(pointbuycosts[attrs])
    }
    
    increase_stats <- function(attrs) {
        ret <- attrs
        current_pbv <- point_buy_value(attrs)
        lowest_attr_index <- first(which.min(ret))
        
        while(current_pbv < min_pbv && ret[lowest_attr_index] < min_high_stat) {
            ret[lowest_attr_index] <- ret[lowest_attr_index] + 1
            current_pbv <- point_buy_value(ret)
        }
        ret
    }
    
    decrease_stats <- function(attrs) {
        ret <- attrs
        possible_stats <- c(8,10,11,12,13,14,15)
        rm <- intersect(ret, possible_stats)
        possible_stats <- possible_stats[! possible_stats %in% rm]
        stat_floor <- min(possible_stats)
        
        current_pbv <- point_buy_value(ret)
        highest_attr_index <- first(which.max(ret))
        
        while(current_pbv > min_pbv && ret[highest_attr_index] > stat_floor) {
            ret[highest_attr_index] <- ret[highest_attr_index] - 1
            current_pbv <- point_buy_value(ret)
        }
        ret
    }
    
    unextremize_attributes <- function(attrs) {
        ret <- attrs
        
        if (point_buy_value(ret) > max_pbv){
            ret <- decrease_stats(ret)
        }
        if (point_buy_value(ret) < min_pbv){ 
            ret <- increase_stats(ret)
        }
        ret
    }
    
    attrs <- rollattrs()
    original_attrs <- attrs
    value <- point_buy_value(attrs)
    original_pbv <- value
    #attrs <- ensure_one_high(attrs)
    while( value < min_pbv || value > max_pbv){
        attrs <- unextremize_attributes(attrs)
        value <- point_buy_value(attrs)
    }
    
    attrs <- ensure_one_high(attrs)
    value <- point_buy_value(attrs)
    total <- sum(attrs)
    mod_total <- sum(modifiers(attrs))
    ans <- list("original_rolls" = original_attrs,
                "final_attributes" = attrs,
                "original_point_buy_value" = original_pbv,
                "final_point_buy_value" = value,
                "modifier_total" = mod_total,
                "point_total" = total)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    tags$style("#attributes {font-size:120px;
               color:maroon;
               text-align: center;}"),
    fluidRow(
        column(12,
               h1("3d6 but balanced"),
               column(4,
                  sliderInput("pbv_target", label = "Point Buy Value Target",
                              min = 25, max = 35, value = 27, step = 1
                              ),
                  sliderInput("high_stat_min", label = "Minimum value for highest stat",
                              min = 12, max = 15, value = 15, step = 1
                              ),
                  selectInput("distribution", label = "Distribution Tendency",
                              choices = c("Versatile" = distributions$RBINOM, 
                                          "Normal" = distributions$RNORM, 
                                          "Focused" = distributions$RUNIF),
                              selected = c(distributions$RNORM)
                              ),
                  actionButton("start", "Roll!")
                      ),
               column(8,
                      p("This is an attribute generator that uses probability distributions to generate attributes. 
                        It limits disruptive stat arrays by forcing attributes to match 5th edition's point-buy value.
                         "),
                      h4("Versatile"),
                      p("Uses a binomial distribution. On average, it will end up with the highest total values. 
                         However, it rarely generates attributes above 15. It is a good distribution to choose if you don't like surprises,
                         want to multiclass, or if \"MAD\" means a lot to you.
                         If you ever see an 18 here, buy a lottery ticket."),
                      h4("Normal"),
                      p("Uses a normal distribution. On average, it mimicks rolling dice for stats the best.
                         Regarding expected maximum and average stats, it's between \"Versatilie\" and \"Focussed\""),
                      h4("Focused"),
                      p("Uses a uniform distribution, which means that before the algorithm converges on the chosen
                         point buy value, anything goes. After, you may end up with a highly focussed array, with multiple high values.
                         However, the average values generated tend to be the lowest of the three distributions. 
                         ")
                      )
               
               )
    ),
    fluidRow(
        column(12,
               textOutput("attributes")
        )
    ),
    fluidRow(
        column(6,
               verbatimTextOutput("log")
               ),
        column(6)
        )
)


server <- function(input, output) {
    
    observeEvent(input$start, {
        calculated_values <- run_attribute_creation(input)

        output$attributes <- renderText({
            "calculating..."
        })

        delay(1000, {output$attributes <- renderText({
            calculated_values$final_attributes[1]
        })})
        delay(2000, {output$attributes <- renderText({
            calculated_values$final_attributes[1:2]
        })})
        delay(3000, {output$attributes <- renderText({
            calculated_values$final_attributes[1:3]
        })})
        delay(4000, {output$attributes <- renderText({
            calculated_values$final_attributes[1:4]
        })})
        delay(5000, {output$attributes <- renderText({
            calculated_values$final_attributes[1:5]
        })})
        delay(6000, {output$attributes <- renderText({
            calculated_values$final_attributes
        })})
        
        delay(7000, { output$log <- renderPrint({
            calculated_values
        })})
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
