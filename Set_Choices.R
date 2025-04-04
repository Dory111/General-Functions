#===========================================================================================
# This asks the user which type of data they are handling
#===========================================================================================
Ask_For_Dataset_Type <- function()
{
  
  #------------------------------------------------------------------------------- 
  correct <- F
  while(correct == F)
  {
    #------------------------------------------------------------------------------- 
    # Ask for user input
    user_dataset_input <- readline(cat('Is this dataset Spatial Polygons (1)\n\n',
                                       'Is this dataset Spatial Raster (2)\n\n',
                                       'Timeseries (3)\n\n',
                                       'or Scatter (4)\n\n'))
    user_dataset_dict <- data.frame(Selection = c('Spatial Polygons', 'Spatial Raster', 'Timeseries', 'Scatter'),
                                    Number = c(1,2,3,4))
    #------------------------------------------------------------------------------- 
    
    
    #------------------------------------------------------------------------------- 
    # Hack to see if its numeric without converting to numeric
    if(is.na(as.numeric(user_dataset_input)/2) == FALSE)
    {
      
      user_dataset_input <- as.numeric(user_dataset_input)
      
      
    }
    #------------------------------------------------------------------------------- 
    
    
    #------------------------------------------------------------------------------- 
    # If the user successfully entered a number
    if(is.numeric(user_dataset_input) == TRUE)
    {
      #------------------------------------------------------------------------------- 
      # If the user entered a valid number
      if(user_dataset_input >= 1 & user_dataset_input <= 4)
      {
        cat('============================================\n',
            'You selected ', user_dataset_dict$Selection[user_dataset_input],'\n',
            '============================================\n\n')
        user_dataset_input <<- user_dataset_input
        correct <- T
        
        
      }
      #------------------------------------------------------------------------------- 
      
      
      #------------------------------------------------------------------------------- 
      # If the user entered a NON valid number
      if(user_dataset_input < 1 | user_dataset_input > 4)
      {
        cat('Enter a number between 1 and 3\n\n')
        correct <- F
        
      }
      #------------------------------------------------------------------------------- 
      
    }
    #------------------------------------------------------------------------------- 
    
    
    
    
    #------------------------------------------------------------------------------- 
    # If the user did not successfully enter a number
    if(is.numeric(user_dataset_input) == FALSE)
    {
      cat('Please enter a number\n\n')
    }
    #------------------------------------------------------------------------------- 
  }
  #------------------------------------------------------------------------------- 
}
#------------------------------------------------------------------------------- 







#===========================================================================================
# This asks the user whether the data is continuous or categorical
#===========================================================================================
Ask_For_Continuous_Categorical <- function()
{
  
  #------------------------------------------------------------------------------- 
  correct <- F
  while(correct == F)
  {
    #------------------------------------------------------------------------------- 
    # Ask for user input
    user_dataset_type <- readline(cat('Is this dataset Continuous (1)\n\n',
                                       'Categorical (2)\n\n',
                                      'or Diverging (3)\n\n'))
    user_dataset_dict <- data.frame(Selection = c('Continuous', 'Categorical','Diverging'),
                                    Number = c(1,2,3))
    #------------------------------------------------------------------------------- 
    
    
    #------------------------------------------------------------------------------- 
    # Hack to see if its numeric without converting to numeric
    if(is.na(as.numeric(user_dataset_type)/2) == FALSE)
    {
      
      user_dataset_type<- as.numeric(user_dataset_type)
      
      
    }
    #------------------------------------------------------------------------------- 
    
    
    #------------------------------------------------------------------------------- 
    # If the user successfully entered a number
    if(is.numeric(user_dataset_type) == TRUE)
    {
      #------------------------------------------------------------------------------- 
      # If the user entered a valid number
      if(user_dataset_type >= 1 & user_dataset_type <= 3)
      {
        cat('============================================\n',
            'You selected ', user_dataset_dict$Selection[user_dataset_type],'\n',
            '============================================\n\n')
        user_dataset_type <<- user_dataset_type
        correct <- T
        
        
      }
      #------------------------------------------------------------------------------- 
      
      
      #------------------------------------------------------------------------------- 
      # If the user entered a NON valid number
      if(user_dataset_type < 1 | user_dataset_type > 3)
      {
        cat('Enter a number between 1 and 2\n\n')
        correct <- F
        
      }
      #------------------------------------------------------------------------------- 
      
    }
    #------------------------------------------------------------------------------- 
    
    
    
    
    #------------------------------------------------------------------------------- 
    # If the user did not successfully enter a number
    if(is.numeric(user_dataset_type) == FALSE)
    {
      cat('Please enter a number\n\n')
    }
    #------------------------------------------------------------------------------- 
  }
  #------------------------------------------------------------------------------- 
  
  
  
  
  
  #------------------------------------------------------------------------------- 
  # if data is categorical
  if(user_dataset_type == 2 | user_dataset_type == 3)
  {
    
    Ask_For_Number_of_Categories()
    
  }
  #------------------------------------------------------------------------------- 
  
  
  
}
#------------------------------------------------------------------------------- 







#===========================================================================================
# This sets the default data choices
#===========================================================================================
Ask_For_Number_of_Categories <- function()
{
  
  
  #------------------------------------------------------------------------------- 
  correct <- F
  while(correct == F)
  {
    #------------------------------------------------------------------------------- 
    # Ask for user input
    user_dataset_categories <- readline(cat('How many categories (1-8) \n\n'))
    #------------------------------------------------------------------------------- 
    
    
    #------------------------------------------------------------------------------- 
    # Hack to see if its numeric without converting to numeric
    if(is.na(as.numeric(user_dataset_input)/2) == FALSE)
    {
      
      user_dataset_categories  <- as.numeric(user_dataset_categories )
      
    }
    #------------------------------------------------------------------------------- 
    
    
    #------------------------------------------------------------------------------- 
    # If the user successfully entered a number
    if(is.numeric(user_dataset_categories ) == TRUE)
    {
      #------------------------------------------------------------------------------- 
      # If the user entered a valid number
      if(user_dataset_categories  >= 1 & user_dataset_categories  <= 8)
      {
        
        cat('============================================\n',
            'You selected ', user_dataset_categories , 'categories','\n',
            '============================================\n\n')
        user_dataset_categories  <<- user_dataset_categories 
        correct <- T
        
        
      }
      #------------------------------------------------------------------------------- 
      
      
      #------------------------------------------------------------------------------- 
      # If the user entered a NON valid number
      if(user_dataset_categories  < 1 | user_dataset_categories  > 8)
      {
        cat('Enter a number between 1 and 8\n\n')
        correct <- F
        
      }
      #------------------------------------------------------------------------------- 
      
    }
    #------------------------------------------------------------------------------- 
    
    
    
    
    #------------------------------------------------------------------------------- 
    # If the user did not successfully enter a number
    if(is.numeric(user_dataset_categories) == FALSE)
    {
      cat('Please enter a number\n\n')
    }
    #------------------------------------------------------------------------------- 
  }
  #------------------------------------------------------------------------------- 
  
}
#------------------------------------------------------------------------------- 






#===========================================================================================
# This sets the default data choices
#===========================================================================================
Set_Default_Data <- function(spots)
{
  
  #------------------------------------------------------------------------------- 
  # Spatial data
  spatial_data <- st_read(system.file("shape/nc.shp", package="sf"), quiet = T)
  spatial_data <<- spatial_data
  #------------------------------------------------------------------------------- 
  
  
  
  
  
  
  #------------------------------------------------------------------------------- 
  # Timeseries data
  X <- seq(from = 1, to = 25, by = 0.1)
  Y <- seq(from = 1, to = 9, by = 1)
  
  
  timeseries_data <- data.frame(X = X,
                                Y1 = X*runif(length(X), Y[1],Y[2]),
                                Y2 = X*runif(length(X), Y[2]-0.5,Y[3]),
                                Y3 = X*runif(length(X), Y[3]-0.5,Y[4]),
                                Y4 = X*runif(length(X), Y[4]-0.5,Y[5]),
                                Y5 = X*runif(length(X), Y[5]-0.5,Y[6]),
                                Y6 = X*runif(length(X), Y[6]-0.5,Y[7]),
                                Y7 = X*runif(length(X), Y[7]-0.5,Y[8]),
                                Y8 = X*runif(length(X), Y[8]-0.5,Y[9]))
  timeseries_data <<- timeseries_data
  #------------------------------------------------------------------------------- 
  
  
  
  
  
  
  
  #------------------------------------------------------------------------------- 
  # Scatter data
  x <- seq(from = 0, to = 1, length.out = 200)
  scatter_data <- data.frame(X = x,
                             Y = runif(n = 200, min = 2, max = 7)*x)
  scatter_data <<- scatter_data
  #------------------------------------------------------------------------------- 

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #-------------------------------------------------------------------------------
  # only one raster spot
  if(spots == 'Single')
  {
    #-------------------------------------------------------------------------------
    # Raster data
    ncol <- 100
    nrow <- 100
    x1 <- seq(from = 0, to = 1, length.out = ncol/2)
    x2 <- seq(from = 1, to = 0, length.out = ncol/2)
    x <- c(x1,x2)

    y1 <- seq(from = 0, to = pi/2, length.out = ncol/2)
    y2 <- seq(from = pi/2, to = 0, length.out = ncol/2)
    y <- c(y1,y2)



    m <- matrix(rep(y,nrow),
                nrow = nrow,
                ncol = ncol, byrow = T)

    #-------------------------------------------------------------------------------
    for(i in 1:nrow(m))
    {
      #-------------------------------------------------------------------------------
      for(j in 1:ncol(m))
      {

        m[i,j] <- (sin(m[i,j]) * x[i]) + # multiply by row position
          (sin(m[i,j]) * x[j]) # multiply by column position

      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    m <- rast(m)
    m <<- m
  }
  #-------------------------------------------------------------------------------
  
  

  #-------------------------------------------------------------------------------
  # Multiple spot demonstration
  if(spots != 'Single')
  {
    #------------------------------------------------------------------------------- 
    # Raster data
    ncol <- 100
    nrow <- 100
    x1 <- seq(from = 0, to = pi/2, length.out = ncol/2)
    x2 <- seq(from = pi/2, to = 0, length.out = ncol/2)
    x <- c(x1,x2)
    
    
    m <- matrix(rep(NA,nrow),
                nrow = nrow,
                ncol = ncol, byrow = T)
    
    #-------------------------------------------------------------------------------
    for(i in 1:nrow(m))
    {
      #-------------------------------------------------------------------------------
      for(j in 1:ncol(m))
      {
        
        m[i,j] <- sin(x[i]*4) + # multiply by row position
          sin(x[j]*4) # multiply by column position
        
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    m <- rast(m)
    m <<- m
  }
  #-------------------------------------------------------------------------------
}
#------------------------------------------------------------------------------- 






#===========================================================================================
# This sets the palettes
#===========================================================================================
Set_Palettes <- function()
{
  
  Palettes<- matrix(nrow = 100,
                    ncol = 1000)
  Palettes <- as.data.frame(Palettes)
  
  
  
  #------------------------------------------------------------------------------- 
  hcl_palettes_sequential <- hcl.pals(type = 'sequential')
  for(i in 1:length(hcl_palettes_sequential))
  {
    
    Palettes[i] <- sequential_hcl(100, palette = hcl_palettes_sequential[i])
    
  }
  colnames(Palettes) <- c(hcl_palettes_sequential,
                                     seq(1:(ncol(Palettes) - length(hcl_palettes_sequential))))
  #------------------------------------------------------------------------------- 
  
  
  
  #------------------------------------------------------------------------------- 
  Palettes[which(is.na(as.numeric(colnames(Palettes))/2) == FALSE)[1]] <- 
    c('#3D8D7A','#13274f','#ce1141','#e7a801', rep(NA, (nrow(Palettes) - length(c('#3D8D7A','#13274f','#ce1141','#e7a801')))))
  colnames(Palettes)[which(is.na(as.numeric(colnames(Palettes))/2) == FALSE)[1]] <- 'Custom 1'
  
  Palettes <<- Palettes
  #------------------------------------------------------------------------------- 
  
  
}
#------------------------------------------------------------------------------- 





#===========================================================================================
# This plots the correct dataset
#===========================================================================================
Plot_Choices <- function()
{
  
  par(mfrow = c(4,4))
  par(mar = c(1,1,3,1))
  #------------------------------------------------------------------------------- 
  # if the data is spatial polygons
  if(user_dataset_input == 1)
  {
    box_col <- 'black'
    #------------------------------------------------------------------------------- 
    # for every continuous palette
    for(i in 1:ncol(Palettes))
    {
      
      #------------------------------------------------------------------------------- 
      # if palette exists
      if(is.na(as.numeric(colnames(Palettes[i]))/2) == TRUE)
      {
        
        Palette <- Palettes[is.na(Palettes[i]) == FALSE, i] # temporary palette
        
        #------------------------------------------------------------------------------- 
        # if data is categorical
        if(user_dataset_type == 2)
        {
          
          
          
          #------------------------------------------------------------------------------- 
          # if there are more entries than timseries data
          # reassign palette so that each line has a unique color instead of
          # a single line having multiple colors
          if(length(Palette) > user_dataset_categories)
          {
            
            Palette <- Palette[floor(seq(from = 1, to = length(Palette), length.out = ((user_dataset_categories))))]
            
          }
          #------------------------------------------------------------------------------- 
          
          
          
          #------------------------------------------------------------------------------- 
          # if there are more less palettes then categories
          # set box color to notify
          if(length(Palette) < user_dataset_categories)
          {
            
            box_col <- 'red'
            
          }
          #-------------------------------------------------------------------------------
          
        }
        #------------------------------------------------------------------------------- 
        
        
        
        
        
        #------------------------------------------------------------------------------- 
        plot(st_geometry(spatial_data),
             axes = F,
             main = colnames(Palettes)[i],
             col = Palette)
        box(col = box_col)
        #------------------------------------------------------------------------------- 
      }
      #------------------------------------------------------------------------------- 
    }
    #------------------------------------------------------------------------------- 
  }
  #------------------------------------------------------------------------------- 
  
  
  
  
  
  #------------------------------------------------------------------------------- 
  # if the data is spatial raster
  if(user_dataset_input == 2)
  {
    box_col <- 'black'
    #------------------------------------------------------------------------------- 
    # for every continuous palette
    for(i in 1:ncol(Palettes))
    {
      
      #------------------------------------------------------------------------------- 
      # if palette exists
      if(is.na(as.numeric(colnames(Palettes[i]))/2) == TRUE)
      {
        
        
        
        
        Palette <- Palettes[is.na(Palettes[i]) == FALSE, i] # temporary palette
        
        #------------------------------------------------------------------------------- 
        # if data is categorical
        if(user_dataset_type == 2)
        {
          
          
          
          #------------------------------------------------------------------------------- 
          # if there are more entries than timseries data
          # reassign palette so that each line has a unique color instead of
          # a single line having multiple colors
          if(length(Palette) > user_dataset_categories)
          {
            
            Palette <- Palette[floor(seq(from = 1, to = length(Palette), length.out = ((user_dataset_categories))))]
            
          }
          #------------------------------------------------------------------------------- 
          
          
          
          #------------------------------------------------------------------------------- 
          # if there are more less palettes then categories
          # set box color to notify
          if(length(Palette) < user_dataset_categories)
          {
            
            box_col <- 'red'
            
          }
          #-------------------------------------------------------------------------------
          
        }
        #------------------------------------------------------------------------------- 
        
        
        #------------------------------------------------------------------------------- 
        plot(m,
             legend = F,
             axes = F,
             main = colnames(Palettes)[i],
             col = Palette)
        box(col = box_col)
        #------------------------------------------------------------------------------- 
      }
      #------------------------------------------------------------------------------- 
    }
    #------------------------------------------------------------------------------- 
  }
  #------------------------------------------------------------------------------- 
  
  
  
  
  
  
  #------------------------------------------------------------------------------- 
  # if the data is Timeseries
  if(user_dataset_input == 3)
  {
    box_col <- 'black'
    #------------------------------------------------------------------------------- 
    # for every continuous palette
    for(i in 1:ncol(Palettes))
    {
      
      #------------------------------------------------------------------------------- 
      if(is.na(as.numeric(colnames(Palettes[i]))/2) == TRUE)
      {
        
        
        Palette <- Palettes[is.na(Palettes[i]) == FALSE, i] # temporary palette
        index <- ncol(timeseries_data)
        
        #------------------------------------------------------------------------------- 
        # if there are more entries than timseries data
        # reassign palette so that each line has a unique color instead of
        # a single line having multiple colors
        if(length(Palette) > (ncol(timeseries_data) - 1))
        {
          
          Palette <- Palette[floor(seq(from = 1, to = length(Palette), length.out = ((ncol(timeseries_data) - 1))))]
          
        }
        #------------------------------------------------------------------------------- 
        
        
        
        #------------------------------------------------------------------------------- 
        # if data is categorical
        if(user_dataset_type == 2)
        {
          #------------------------------------------------------------------------------- 
          # if there are more entries than timseries data
          # reassign palette so that each line has a unique color instead of
          # a single line having multiple colors
          if(length(Palette) > user_dataset_categories)
          {
            
            Palette <- Palette[floor(seq(from = 1, to = length(Palette), length.out = ((user_dataset_categories))))]
            index <- user_dataset_categories + 1
            
          }
          #-------------------------------------------------------------------------------
          
          
          
          #------------------------------------------------------------------------------- 
          # if there are more less palettes then categories
          # set box color to notify
          if(length(Palette) < user_dataset_categories)
          {
            
            box_col <- 'red'
            
          }
          #-------------------------------------------------------------------------------
        }
        #-------------------------------------------------------------------------------
        
        
        
        #------------------------------------------------------------------------------- 
        plot(x = timeseries_data$X,
             y = timeseries_data$Y1,
             xaxt = 'n',
             yaxt = 'n',
             ylab = '',
             xlab = '',
             ylim = c(0,max(timeseries_data[2:index])),
             main = colnames(Palettes)[i],
             col = 'white')
        box(col = box_col)
        #------------------------------------------------------------------------------- 
        
        for(i in 1:(ncol(timeseries_data)-1))
        {
          
          lines(x = timeseries_data$X,
                y = unlist(timeseries_data[(i+1)]),
                col = Palette[i],
                lwd = 3)
          
        }
        
      }
      #------------------------------------------------------------------------------- 
    }
    #------------------------------------------------------------------------------- 
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  #------------------------------------------------------------------------------- 
  # if the data is scatter
  if(user_dataset_input == 4)
  {
    box_col <- 'black'
    #------------------------------------------------------------------------------- 
    # for every continuous palette
    for(i in 1:ncol(Palettes))
    {
      
      #------------------------------------------------------------------------------- 
      if(is.na(as.numeric(colnames(Palettes[i]))/2) == TRUE)
      {
        
        
        Palette <- Palettes[is.na(Palettes[i]) == FALSE, i] # temporary palette
        
        
        #------------------------------------------------------------------------------- 
        # if data is categorical
        if(user_dataset_type == 1)
        {
          
          scatter_data$Cat <- seq(from = 1,
                                  to = nrow(scatter_data),
                                  length.out = nrow(scatter_data))
          #------------------------------------------------------------------------------- 
          # if there are more points than can be indexed to the palette
          # reassign palette so that each line has a unique color instead of
          # a single line having multiple colors
          if(length(Palette) <  nrow(scatter_data))
          {
            
            Palette <- Palette[floor(seq(from = 1, to = length(Palette), length.out = nrow(scatter_data)))]
            Palette[(length(Palette) - floor(length(Palette)/20)):length(Palette)] <- Palette[length(Palette)]
            
          }
          #-------------------------------------------------------------------------------
          
        }
        #------------------------------------------------------------------------------- 

        
        
        
        #------------------------------------------------------------------------------- 
        # if data is categorical
        if(user_dataset_type == 2)
        {
          
          scatter_data$Cat <- sample(1:user_dataset_categories,
                                    nrow(scatter_data), replace = TRUE)
          #------------------------------------------------------------------------------- 
          # if there are more entries than timseries data
          # reassign palette so that each line has a unique color instead of
          # a single line having multiple colors
          if(length(Palette) > user_dataset_categories)
          {
            
            Palette <- Palette[floor(seq(from = 1, to = length(Palette), length.out = ((user_dataset_categories))))]
            Palette <- Palette[scatter_data$Cat]
            
          }
          #-------------------------------------------------------------------------------
          
          
          
          #------------------------------------------------------------------------------- 
          # if there are more less palettes then categories
          # set box color to notify
          if(length(Palette) < user_dataset_categories)
          {
            
            box_col <- 'red'
            
          }
          #-------------------------------------------------------------------------------
        }
        #-------------------------------------------------------------------------------
        
        
        
        #------------------------------------------------------------------------------- 
        plot(x = scatter_data$X,
             y = scatter_data$Y,
             xaxt = 'n',
             yaxt = 'n',
             ylab = '',
             xlab = '',
             main = colnames(Palettes)[i],
             col = Palette,
             pch = 19,
             cex = 2)
        box(col = box_col)
        #------------------------------------------------------------------------------- 
      }
      #------------------------------------------------------------------------------- 
    }
    #------------------------------------------------------------------------------- 
  }
  #------------------------------------------------------------------------------- 
}
#------------------------------------------------------------------------------- 















