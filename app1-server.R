

# Server function for the shiny app
server <- function(input, output){
  
  dataset <- reactive({
    fifa1[sample(nrow(fifa1), input$sampleSize),]
  })
  
  output$plot <- renderPlotly({
    
    
    
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge"),
                      "scatter" = geom_point()
    )
    
    
    if(input$plot.type=="boxplot" | input$plot.type=="scatter"  )	{		#control for 1D or 2D graphs
      p<-ggplot(dataset(),
                aes_string(
                  x 		= input$x,
                  y 		= input$y,
                  fill 	= input$x
                  #,alpha= 0.4
                  # let type determine plotting
                )
      )  + plot.type
      
      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      if(input$log == TRUE)
      {
        p<-p+ scale_y_log10() 
      }
      if(input$log1 == TRUE)
      {
        p<-p+ scale_x_log10() 
      }
      
      
    } else {
      
      p<-ggplot(dataset(),
                aes_string(
                  x 		= input$x,
                  fill 	= input$y,
                  group 	= input$y
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      
      guide = FALSE
    ) 
    facets <- paste(input$facet_row, '~', input$facet_col)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)
    
    ggplotly(p)
    
  })
  
  
}


shinyApp(ui, server)
