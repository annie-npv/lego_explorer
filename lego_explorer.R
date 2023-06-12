# Install packages
library(tidyverse)
library(maps)
library(ggthemr)
library(plotly)
library(circlepackeR)         
library(data.tree)
library(treemapify)
library(packcircles)
library(ggplot2)

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

# Data downloaded from 
# https://www.kaggle.com/datasets/rtatman/lego-database

# Read data
colors <- read.csv("colors.csv")
inventories <- read.csv("inventories.csv")
inventory_parts <- read.csv("inventory_parts.csv")
inventory_sets <- read.csv("inventory_sets.csv")
part_categories <- read.csv("part_categories.csv")
parts <- read.csv("parts.csv")
sets <- read.csv("sets.csv")
themes <- read.csv("themes.csv")



# Rename columns
colnames(part_categories) <- c("id", "category")

parts_join <- parts%>%
  left_join(part_categories, by = c("part_cat_id" = "id"))

parts_quant <- inventory_parts%>%
  group_by(part_num)%>%
  summarize(total = n())

parts_join <- parts_join%>%
  left_join(parts_quant)%>%
  drop_na()


# Which is the most common sub_theme(rshiny)
  # Rename columns
  colnames(themes) = c("theme_id", "theme", "parent_id")
  
  # Grouping into parent groups
  for (i in 1:nrow(themes))
  {
    if (is.na(themes$parent_id[i]) == TRUE) 
    {
      themes$parent_id[i]= themes$theme_id[i]
    }
  }
  
  # Omit redundant columns
  theme_set <- themes %>% 
    merge(sets, by = "theme_id") %>% 
    select(-name,-year,-num_parts)
  
  # Merge datasets
  theme_set_inv <- merge(inventories, theme_set, by = "set_num")
  
  # Count sets per subthemes
  set_per_theme <- theme_set_inv %>% 
    group_by(parent_id) %>% 
    count(theme)

  # Converting tp majpr themes
  for (i in 1:nrow(set_per_theme))
  {
    if (set_per_theme$parent_id[i] == 5) 
    {
      set_per_theme$parent_id[i] = 1
    }
    
    if (set_per_theme$parent_id[i] == 23 |
        set_per_theme$parent_id[i] == 38) 
    {
      set_per_theme$parent_id[i] = 22
    }
    
    if (set_per_theme$parent_id[i] == 52 |
        set_per_theme$parent_id[i] == 67 |
        set_per_theme$parent_id[i] == 94 |
        set_per_theme$parent_id[i] == 105 ) 
    {
      set_per_theme$parent_id[i] = 50
    }
    
    if (set_per_theme$parent_id[i] == 123 ) 
    {
      set_per_theme$parent_id[i] = 112
    }
    
    if (set_per_theme$parent_id[i] == 148 ) 
    {
      set_per_theme$parent_id[i] = 147
    }
    
    if (set_per_theme$parent_id[i] == 159 |
        set_per_theme$parent_id[i] == 171 |
        set_per_theme$parent_id[i] == 180 ) 
    {
      set_per_theme$parent_id[i] = 158
    }
    
    
    if (set_per_theme$parent_id[i] == 207 |
        set_per_theme$parent_id[i] == 217 |
        set_per_theme$parent_id[i] == 227 ) 
    {
      set_per_theme$parent_id[i] = 206
    }
    
    if (set_per_theme$parent_id[i] == 236 |
        set_per_theme$parent_id[i] == 241 ) 
    {
      set_per_theme$parent_id[i] = 233
    }
    
    if (set_per_theme$parent_id[i] == 280 |
        set_per_theme$parent_id[i] == 287 |
        set_per_theme$parent_id[i] == 290 ) 
    {
      set_per_theme$parent_id[i] = 279
    }
    
    if (set_per_theme$parent_id[i] == 373) 
    {
      set_per_theme$parent_id[i] = 365
    }
    
    if (set_per_theme$parent_id[i] == 454) 
    {
      set_per_theme$parent_id[i] = 443
    }
    
    if (set_per_theme$parent_id[i] == 484) 
    {
      set_per_theme$parent_id[i] = 482
    }
    
    if (set_per_theme$parent_id[i] == 518 |
        set_per_theme$parent_id[i] == 524 |
        set_per_theme$parent_id[i] == 529) 
    {
      set_per_theme$parent_id[i] = 507
    }
    
    if (set_per_theme$parent_id[i] == 562 | 
        set_per_theme$parent_id[i] == 566) 
    {
      set_per_theme$parent_id[i] = 561
    }
  }  
  
  # Data processing
  set_per_theme_all <- theme_set_inv %>% 
    group_by(parent_id,theme_id) %>% 
    count(theme)%>%
    group_by(parent_id) %>% 
    summarise(Value = sum(n)) %>% 
    left_join(themes, by = c("parent_id"="theme_id"))%>%
    select(-parent_id)
  
  set_per_theme_all <- set_per_theme_all %>% 
    group_by(parent_id.y) %>% 
    summarise(Value = sum(Value)) %>% 
    left_join(themes, by = c("parent_id.y"="theme_id"))%>%
    select(-parent_id.y) %>% 
    arrange(Value) %>%    
    mutate(theme=factor(theme, levels=theme))
  
  # Renaming columns
  colnames(set_per_theme_all) <- c("Sets","Theme","parent_id")
  
  set_per_theme_joined <- set_per_theme_all %>% 
    group_by(parent_id) %>% 
    right_join(set_per_theme, by ="parent_id")
  
  # Choose the parent theme group (rshiny input)
  set_per_theme_tech <- set_per_theme %>% 
    filter(parent_id==1) %>% 
    arrange(theme)
    
  # Plot
  tremap <- ggplot(set_per_theme_tech, aes(area = n, fill = theme,
                                 label = paste(theme, sep = "\n"))) +
    geom_treemap() +
    geom_treemap_text(
      place = "centre",
      size = 10) +
    theme(legend.position = "none")
  ggplotly(tremap)

# Number of sets produced 1950-2020
  # Count number of sets per year
  set_cleaned <- sets%>%
    group_by(year)%>%
    summarize(total=n())
  
  # Line plot
  set_prod<- ggplot()+
    geom_line(aes(x=year,y=total),data=set_cleaned)+
    labs(x="Year",y="Sets released",title="Number of lego sets 
         released 1950-2020")+
    scale_x_continuous(limits = c(1948,2022))+
    theme(plot.title = element_text(hjust=0.5),
          panel.grid = element_blank())
  
  # Interactive added
  ggplotly(set_prod)
  

# Which are the most common parts
    # Count number of parts per category
    parts_join_new <- parts_join%>%
      mutate(Category = category)%>%
      group_by(Category)%>%
      select(!category)%>%
      summarize(Total = n())
    
    # Choose 9 most popular (rshiny input number)
    parts_join_major <- parts_join_new%>%
      arrange(desc(Total))%>%
      slice(1:9) 
    
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
    
   
    
# Which is the most common theme 
    
    # Lollipop plot
    set_theme <- ggplot(set_per_theme_all, aes(x=Theme, y=Sets)) +      
      geom_segment( aes(xend=Theme, yend=0))+
      coord_flip() +
      labs(x="Theme",y="Sets released",title="Number of lego sets 
           released per theme")+
      theme(plot.title = element_text(hjust=0.5))+
      theme(
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")+
      labs(x = "Theme", y = "Total number of sets")
    
    # Add interactivity using plotly
    ggplotly(set_theme, tooltip = c("y", "x"))
    
      
# The expansion of color range over the years (rshiny filter)
    setname_inv <- sets%>%
      right_join(inventories, by = "set_num")
    
    setname_inv <- setname_inv%>%
      right_join(inventory_parts, by = c("id" = "inventory_id"))

    setname_inv_color <- setname_inv%>%
      left_join(colors, by = c("color_id" = "id"))%>%
      select(-theme_id,num_parts,id,version)

    color_by_year <- setname_inv_color %>% 
      group_by(year,name.y,rgb) %>% 
      count(name.x) %>% 
      mutate(rgb = paste0("#",rgb))

    colnames(color_by_year) <- c("Year","Color","Hex","Name","Bricks")
    
    color_by_year <- color_by_year %>% 
      group_by(Year,Color,Hex) %>% 
      summarize(Total = n())

    #Convert to color vector
    hex <- as.vector(as.character(color_by_year$Hex))
    
    color_year <- ggplot()+
      geom_col(aes(x=Year, y = Total), data = color_by_year, fill = hex,
               position="fill")+
      xlim(1949,2018)+
      theme(panel.grid = element_blank(),
            plot.title = element_text(hjust=0.5))+
      labs(y = "Percent by color" , title = "Percent of color 
           by year 1950-2017")
    
    ggplotly(color_year,tooltip = c("x","y","fill"))

      
# What color is available in each theme (rshiny) 
    # Cleaning data to see what colors are in each theme
    theme_set_part <- theme_set_inv %>% 
      right_join(inventory_parts, by =c("id"="inventory_id"))
    
    theme_set_part_color <- theme_set_part %>% 
      left_join(colors, by =c("color_id"="id"))
    
    color_in_theme <- theme_set_part_color %>% 
      left_join(set_per_theme_all, by = "parent_id")%>%
      select(Theme,rgb,name) %>% 
      mutate(rgb = paste0("#",rgb)) %>% 
      group_by(Theme,rgb) %>% 
      count(name) 
    
    color_in_theme1 <- color_in_theme %>% 
      filter(Theme == "Legoland")
    
    
    color_in_theme1$n <- seq(1,nrow(color_in_theme1))
    color_in_theme1$size <- 150
    
    
    # It gives its center (x and y) and its radius, proportional 
    # of the value
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
      geom_polygon(data = dat.gg, aes(x=x, y=y, group = Color), 
                   fill = hex_theme
                   , colour = "black") +
      
      # Add text in the center of each bubble + control its size
      scale_size_continuous(range = c(1,4)) +
      
      # General theme:
      theme_void() + 
      theme(legend.position="none",
            axis.line = element_blank(),
            panel.grid = element_blank()) +
      coord_equal()
    
    ggplotly(color_theme, tooltip = "group")
    
    

# What can we build from this piece
    # Group part-num and color and category
    color_part_theme <- theme_set_part_color %>% 
      select(theme,part_num,color_id,name,rgb) %>% 
      left_join(parts, by = "part_num") %>% 
      left_join(part_categories, by =c("part_cat_id"="id")) %>% 
      select(-color_id) %>% 
      mutate(rgb = paste0("#",rgb))
    
    colnames(color_part_theme) <- c("Theme","part_num","Color","rgb","Name",
                              "part_cat_id","Category")

    # Remove duplicate columns
    color_part_theme <- color_part_theme[!duplicated(color_part_theme), ]
    
    color_part_theme <- color_part_theme %>% 
      select(-part_num,) %>% 
      left_join(set_per_theme_joined, by =c("Theme"="theme")) %>% 
      select(-Sets)
    
    my_color_part <- color_part_theme %>% 
      filter(Color =="Red",Category == "Plates",Name == "Plate 1 x 1") 
    

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
    
    
# Save all data
save(color_by_year, color_in_theme, color_part_theme,
     my_pal, sets,parts_join_new, set_per_theme_all, set_per_theme_joined, 
     file = "data.RData")
    
    
