library(markdown)
library(shiny)
library(packcircles)
library(ggplot2)
library(circlepackeR)         
library(data.tree)
library(bslib)
library(plotly)
library(ggthemr)
library(shinythemes)

load("data.Rdata")
# Establish theme 
ggthemr('light',type="outer",layout="minimal")
my_pal <- define_palette(
  swatch = c('yellow', 'salmon', 'forestgreen', 'dodgerblue', 'brown',
             'pink', 'lightgrey',"darkorchid3","orange","honeydew4","violet",
             "indianred1","cyan4","darkgoldenrod3","lightblue3",
             "aquamarine1","coral2","cornsilk3","burlywood2","deepskyblue3",
             "firebrick2","gold1","deeppink3","lawngreen","lightcyan2",
             "hotpink4","maroon","olivedrab","plum2","slategray2","tan1",
             "cornsilk","azure1","blanchedalmond","darkolivegreen2",
             "darkseagreen2","mediumpurple1","salmon1","turquoise1",
             "slateblue2"), 
  gradient = c(lower = 'red', upper = 'green')
)
ggthemr(my_pal)

# Define UI for application that draws a histogram
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # Remove error messages 
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  # Set theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  # Rshiny
  navbarPage("Exploring the Legos!",
             tabPanel("What do we know about themes?",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("yourtheme", "Choose your theme",
                                      choice = set_per_theme_joined$Theme)
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Theme Plot", 
                                               plotlyOutput("set_theme_plot", 
                                                            height = "1500px")),
                                      tabPanel("Themes and sub-themes",
                                               plotOutput("theme_bar")),
                                      tabPanel("What colors can we find 
                                               in this theme?",
                                               plotlyOutput("color_theme_plot"))
                        )
                      )
                      )
             ),
             tabPanel("Changes through the years",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year", "Select year to display",
                                      value = c(1950, 2017),step=1,sep="",
                                      min = min(color_by_year$Year), 
                                      max = max(color_by_year$Year))
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Color range", 
                                               plotlyOutput("color_plot")),
                                      tabPanel("Set produced", 
                                               plotlyOutput("set_prod_plot"))
                      )
                        )
                      )
             ),
             tabPanel("What are the most common parts?",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("num_part", "Number of part types display"
                                    ,value = "10", min = 1,max = 30, step = 1),
                          selectInput("order", "Order"
                                      ,choice = c("Low to High","High to Low"))
                        ),
                        mainPanel(plotlyOutput("part_type_plot")
                                  )
                      )     
             ),
             tabPanel("What can I build with this part?",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("color", "What color is it?"
                                      ,choice = unique(color_part_theme$Color)),
                          selectInput("type", "What type of part is it?"
                                      ,choice = unique(color_part_theme$Category)),
                          selectInput("piece", "What piece is it?"
                                      ,choice = unique(color_part_theme$Name))
                        ),
                        mainPanel(circlepackeROutput("lego_build_piece",
                                                     width = "100%", 
                                                     height = "800px"))
                      )     
             )    
    )
)


# Define server logic required to draw a histogram
server <- function(input, output , session) {
  
  # Plot theme_bar_polar
  theme_bar_polar <- reactive({
    # Choose the parent theme group (rshiny input)
    set_per_theme_selected <- set_per_theme_joined %>% 
      filter(Theme == input$yourtheme) 
    
    # Plot
    theme_plot <- ggplot(set_per_theme_selected, aes(area = n, fill = theme,
                                   label = paste(theme, sep = "\n"))) +
      geom_treemap() +
      geom_treemap_text(
        place = "centre", size = 15) +
      theme(legend.position = "none")
    
    theme_plot
    
  })
  
  # Output of theme_bar_polar
  output$theme_bar <- renderPlot(theme_bar_polar(),height=550 )
  
  
  # Plot color_bar_percent
  color_bar_percent <- reactive({
    color_by_year <- color_by_year %>% 
      filter(as.numeric(Year)>input$year[1] & as.numeric(Year)<input$year[2])
    
    #Convert to color vector
    hex <- as.vector(as.character(color_by_year$Hex))
    
    # Plot
    color_year <- ggplot()+
      geom_col(aes(x=Year, y = Total,group = Color), 
               data = color_by_year, fill = hex,
               position="fill")+
      xlim(input$year[1]-1,input$year[2]+1)+
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust=0.5))+
      labs(y = "Percent by color" , title = "Percent of color by year 1950-2017")
    
    # Interactive
    ggplotly(color_year,tooltip = c("x","y","group"))
  })
  
  # Output of color_bar_percent
  output$color_plot <- renderPlotly(color_bar_percent())
  
  
  # Plot color_theme
  color_theme <- reactive({
    color_in_theme1 <- color_in_theme %>% 
      filter(Theme == input$yourtheme)
    
    color_in_theme1$n <- seq(1,nrow(color_in_theme1))
    color_in_theme1$size <- 150
    
    
    # It gives its center (x and y) and its radius, proportional of the value
    packing <- circleProgressiveLayout(color_in_theme1$size, sizetype='area')
    packing$radius <- 0.9*packing$radius
    
    dat.gg <- circleLayoutVertices(packing, npoints=50)
    dat.gg <- dat.gg %>% 
      left_join(color_in_theme1,by = c("id" = "n"))
    
    color_in_theme1 <- cbind(color_in_theme1, packing)
    
    hex_theme <- as.vector(dat.gg$rgb)
    
    colnames(dat.gg) <- c("x","y","id","Theme","rgb","Color","size")
    
    # Make the plot
    color_theme <- ggplot() + 
      
      # Make the bubbles
      geom_polygon(data = dat.gg, aes(x=x, y=y, group = Color), fill = hex_theme
                   , colour = "black") +
      
      # Add text in the center of each bubble + control its size
      scale_size_continuous(range = c(1,4)) +
      
      # General theme:
      theme_void() + 
      theme(legend.position="none",
            axis.line = element_blank(),
            panel.grid = element_blank()) +
      coord_equal()
    
    # Interactive
    ggplotly(color_theme, tooltip = "group")
  })
  
  # Output of color_theme
  output$color_theme_plot <- renderPlotly(color_theme())
  
  
  # Plot of set_theme_plot
  set_theme_all <- function(){
    
    # Plot
    set_theme <- ggplot(set_per_theme_all, aes(x=Theme, y=Sets)) +      
    geom_segment( aes(xend=Theme, yend=0, color=Sets))+
    coord_flip() +
    labs(x="Theme",y="Sets released",title="Number of lego sets released per theme")+
    theme(plot.title = element_text(hjust=0.5))+
    theme(
      panel.grid = element_blank(),
      legend.position = "none")+
    labs(x = "Theme", y = "Total number of sets")
    
    # Inteactive
    ggplotly(set_theme, tooltip = c("y", "x"))
    
  }

  # Output of set_theme_plot
  output$set_theme_plot <- renderPlotly(set_theme_all())
  
  
  # Plot of part_bar
  part_type <- reactive({
    
    n <- as.numeric(input$num_part)
    if (input$order == "Low to High")
    {
    # Choose n most popular (rshiny input number)
      parts_join_major <- parts_join_new%>%
        arrange((Total))%>%
        slice(1:n) 
    }
    
    if (input$order == "High to Low")
    {
      # Choose n most popular (rshiny input number)
      parts_join_major <- parts_join_new%>%
        arrange(desc(Total))%>%
        slice(1:n) 
    }
    
    # Create the basic bar plot using ggplot
    bar <- ggplot(parts_join_major, aes(x = Category, y = Total)) +
      geom_bar(stat = "identity", aes(fill=Category))+
      theme(
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")+
      labs(x = "Category", y = "Total number of sets")
    
    # Add interactivity using plotly
    ggplotly(bar, tooltip = c("y", "x"))
  })
  
  # Output of part_bar
  output$part_type_plot <- renderPlotly(part_type())
  
  
  # Plot of set_prod_plot
  set_produced <- reactive({
    sets <- sets %>% 
      filter(year>input$year[1] & year<input$year[2])
    
    set_cleaned <- sets%>%
      group_by(year)%>%
      summarize(total=n())
    
    
    # Line plot
    set_prod<- ggplot()+
      geom_line(aes(x=year,y=total),data=set_cleaned)+
      labs(x="Year",y="Sets released",title="Number of lego sets released 1950-2020")+
      scale_x_continuous(limits = c(input$year[1]-1,input$year[2]+1))+
      theme(plot.title = element_text(hjust=0.5),
            panel.grid = element_blank())
    
    # Interactive added
    ggplotly(set_prod)
  })
  
  # Output of set_prod_plot
  output$set_prod_plot <- renderPlotly(set_produced())

  
  # Plot of lego_build_piece
  observe({
    my_color_part <- color_part_theme %>% 
      filter(Color == input$color & Category == input$type)
    updateSelectInput(session,"piece",choices = unique(my_color_part$Name))
  })
  
  lego_build <- reactive({

    my_color_part <- color_part_theme %>% 
      filter(Color == input$color, Category == input$type , Name == input$piece)
    # create a nested data frame giving the info of a nested dataset:
    data <- my_color_part %>% 
      mutate(root=rep("root", nrow(my_color_part)),
             group=Theme.y,
             subgroup=Theme,
             value=n) %>% 
      select(root,group,subgroup,value)
    
    
    # Change the format. This use the data.tree library. This library needs a column that looks like root/group/subgroup/..., so I build it
    data$pathString <- paste("lego", data$group, data$subgroup,
                             sep = "/")
    population <- as.Node(data)
    
    circlepackeR(population, size = "value", color_min = "salmon", 
                 color_max = "hsl(341,30%,40%)")
    
  })
  
  # Output of lego_build_piece
  output$lego_build_piece <- renderCirclepackeR(lego_build())
  
}



# Run the application 
shinyApp(ui = ui, server = server)
