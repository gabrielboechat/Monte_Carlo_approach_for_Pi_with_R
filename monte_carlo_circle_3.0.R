# How many iterations we will have? Choose the desirable number for n 

n = 400

random_coord_x = runif(n, -1, 1)
random_coord_y = runif(n, -1, 1)
random_coord_point = c(random_coord_x,random_coord_y) # Creating the coordinates for each randomized point, evenly distributed between (-1,1) 

distance = sqrt((random_coord_x)^2+(random_coord_y)^2) # Distance from each point from the center (0,0)
inside = distance <= 1 # If the point is inside the circle, then it's distance from the center will be less(or equal) than 1 -> TRUE, it's inside the circle!


random_coord_df = data.frame(random_coord_x,random_coord_y,distance,inside) # A dataframe will organize every information we got so far


inside_red = inside[1:i]
table(inside_red) 

table(random_coord_df$inside[1:i]) # Will be later used

frequency_inside = rep(NA,n) # A non available vector length n will be created for further use 


for (i in c(1:n)) { # We will repeat the process from 1 to n, value already registred

  inside_red = inside[1:i]
  
  frequency = table(inside_red) # We will count how many times red points (that are inside the circle) will appear from a range 1:i
  
  frequency_inside[i] = 4 * (length(random_coord_df$inside[inside == TRUE]) / i) # This vector calculate the aproximation for pi: as long as we search for the length, we will approach pi
  
  
  
  mypath = file.path("C:","Users","gabri","Desktop","Animações","Plots","slot 2",paste("imagem_",i,".jpeg",sep = ""))
  
  jpeg(file = mypath) # This will save each 1:i images generated further below
  
  par(mfrow = c(1,2)) # Dividing the plot in one row, two columns: we'll have each plot beside the other
  
  plot(x = NA, y = NA, 
       asp = 1, 
       xlim = c(-1, 1), 
       ylim = c(-1,1),
       xaxt = "n",
       yaxt = "n",
       xlab = NA,
       ylab = NA,
       axes = FALSE,
       main = paste("Simulation -",i,"points \n Twitter @gab_boechat"),
       mtext(paste("Inside:",frequency[2],"; ","Outside:",frequency[1]))) # Plotting a background for further plot 
  
  par(new = TRUE) # When this command appears, the next plot will be merged with the last one
  
  mtext(paste("4 * (Inside / Total) = ",frequency_inside[i]),side = 1) # Plotting bottom text
  
  par(new = TRUE)

  rect(-1,-1,1,1, lwd = 1) # Rectangle (square) side 2
  
  par(new = TRUE)
  
  plotrix::draw.circle(0,0, radius = 1, lwd = 1) # Circle radius 1 centered at the origin
  
  par(new = TRUE)
  
  plot(random_coord_x[1:i],
       random_coord_y[1:i],
       asp = 1,
       xlim = c(-1, 1), 
       ylim = c(-1,1),
       xaxt = "n",
       yaxt = "n",
       xlab = NA,
       ylab = NA,
       axes = FALSE,
       pch = 20,
       col = ifelse(random_coord_df$inside == TRUE, "blue", "red")) # Will plot the n points. Attention for the ifelse sintax, as ifelse(test, when condition TRUE what do we do?, otherwise do)
  
  
  # Note that there's no par(new=TRUE) for plotting the next plot at the right of the first one we just made!
  
  
  plot(frequency_inside[1:i],
       ylim = c(frequency_inside[i] - 5.0,frequency_inside[i] + 5.0),
       type = "l",
       col = "magenta",
       lwd = 2, 
       xlab = paste("total nº of points: ",i), 
       ylab = "Ratio",
       main = expression(paste("Converging to ",pi))) # Will do the curve for converging pi
  
  par(new = TRUE)
  
  lines(c(0, i), 
        c(3.141592, 3.141592), 
        col = "darkred", 
        lty = 2, 
        lwd = 1) # Add a horizontal line in pi (aproximating with 6 decimals places)
  
  par(new = TRUE)
  
  text(x = i - 80,
       y = 3.141592 + 0.3,
       expression(pi)) # Add the greek letter pi to the plot
  
  
  
  dev.off() # The image will stop being generated in this point, going back to square one (line 26), repeating the process n times! 
}
