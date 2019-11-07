library(shiny)
library(ggplot2)
library(dplyr)

d <- read.csv('recent_grads.csv')
d$EmploymentRate = 1 - d$Unemployment_rate
d$FulltimeRate = d$Full_time / d$Total
d$ParttimeRate = d$Part_time / d$Total
d$Diff = d$P75th - d$P25th
ui = fluidPage(
    titlePanel('Employment Information for Recent Graduated Students'),
    sidebarLayout(
        sidebarPanel(
            helpText('This visualization allows you to explore the employment data on recent graduated students of different major by yourself.'), 
            selectInput(inputId = 'Category', label = 'Category', 
                        choices = c("Agriculture & Natural Resources", 
                                    "Arts", 
                                    "Biology & Life Science", 
                                    "Business", 
                                    "Communications & Journalism", 
                                    "Computers & Mathematics", 
                                    "Education", 
                                    "Engineering", 
                                    "Health", 
                                    "Humanities & Liberal Arts", 
                                    "Industrial Arts & Consumer Services", 
                                    "Interdisciplinary", 
                                    "Law & Public Policy", 
                                    "Physical Sciences", 
                                    "Psychology & Social Work", 
                                    "Social Science")), 
            selectInput(inputId = 'rank', label = 'Rank the Majors by: (From High to Low)', 
                        choices = c('Total Number of Student' = 'Total', 
                                    'Number of Male Student' = 'Men', 
                                    'Number of Female Student' = 'Women', 
                                    'Percent of Female Student' = 'ShareWomen')), 
            selectInput(inputId = 'y_var', label = 'Choose the Employment Information to Display', 
                        choices = c('Income' = 'Median', 
                                    'Employment Rate' = 'EmploymentRate', 
                                    'Full-time Rate' = 'FulltimeRate', 
                                    'Part-time Rate' = 'ParttimeRate')), 
            selectInput(inputId = 'fill', label = 'Choose to View More Information: (By Color)', 
                        choices = list(
                            'None' = 'NA',
                            'Gender Difference (%Female)' = 'ShareWomen', 
                            'Income Difference (75th Percentile - 25th Percentile' = 'Diff'))
        ),
        mainPanel(
            plotOutput(outputId = 'plot')
        )
    )
)

server = function(input, output) {
    
    
    output$plot = renderPlot({
        title = paste('Employment Information on ', input$Category, 'Majors', sep = '')
        ylab = paste(input$y_var)
        
        if (input$fill=='NA') {
            d %>% 
                filter(Major_category == input$Category) %>% 
                ggplot(aes(x = reorder(Major, get(input$rank)), y = get(input$y_var))) +
                geom_col() + 
                coord_flip() + 
                ggtitle(title) + 
                labs(x = 'Majors',y=ylab)
            #ylab(ylab) + 
            #xlab('Majors')
            
        } else {
            filllab = ifelse(input$fill=='ShareWomen','%Female','$Income')
            d %>% 
                filter(Major_category == input$Category) %>% 
                ggplot(aes(x = reorder(Major, get(input$rank)), y = get(input$y_var), fill = get(input$fill))) +
                geom_col() + 
                coord_flip() + 
                ggtitle(title) + 
                scale_fill_continuous(low = 'grey', high = 'darkred') +
                labs(x = 'Majors',y=ylab,fill=filllab)
            #ylab(ylab) + 
            #xlab('Majors')
        }
        
        
    })
}

shinyApp(ui, server)
