library(tidyverse)

# How many iterations we will have?

n = 15000

random_coord_x = runif(n, -1, 1)
random_coord_y = runif(n, -1, 1)
random_coord_point = c(random_coord_x,random_coord_y)

distance = sqrt((random_coord_x)^2+(random_coord_y)^2)
inside = distance <= 1 

random_coord_df = data.frame(random_coord_x,random_coord_y,distance,inside)

inside_red = inside[1:i]
table(inside_red)

table(random_coord_df$inside[1:i])

for (i in seq(0,n,500)) {
  
  inside_red = inside[1:i]
  
  frequency = table(inside_red)
  
  mypath = file.path("C:","Users","gabri","Desktop","Animações","Plots","slot 2",paste("imagem_",i,".jpeg",sep = ""))
  
  jpeg(file = mypath)
  
  plot(x = NA, y = NA, 
       asp = 1, 
       xlim = c(-1, 1), 
       ylim = c(-1,1),
       xaxt = "n",
       yaxt = "n",
       xlab = NA,
       ylab = NA,
       axes = FALSE,
       main = paste("Monte Carlo's Simulation -",i,"points | Gabriel Boechat \n @gab_boechat"),
       mtext(paste("Inside:",frequency[2],"; ","Outside:",frequency[1])))
  
  par(new = TRUE)
  
  mtext(paste("4 * (Inside / Total) = ",round(4*(frequency[2]/i),5),", approximately Pi!"),side = 1)
                    
  par(new = TRUE)

  rect(-1,-1,1,1, lwd = 1)
  
  par(new = TRUE)
  
  plotrix::draw.circle(0,0, radius = 1, lwd = 1)

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
       col = ifelse(random_coord_df$inside == TRUE, "blue", "red"))
  
  dev.off()
}


