#--------------------------------------
#
# First explorations on how to generate a 3D mesh with R
#
#--------------------------------------



#install.packages("plot3D")
#install.packages("magick")
library(plot3D)
library(magick)

# 1 - Make stem and petioles
########################

# parameters
D=2 # density of the top inflorescence
K=2 # shape of the top
theta=2.4 # angle between petioles

# make scene
xs<-c(-5,-5,5,5)
ys<-c(-5,5,-5,5)
zs<-c(0,0,10,10)
scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 30, theta = 20,bty="b")

# make stem
segments3D (x0=0, y0=0, z0=0, x1 = 0, y1 = 0, z1 = 10,col="darkgreen",lwd=3,add=T)
# make petioles
#base:
z<-(10-sort(rlnorm(50,meanlog = 0,sdlog = D)))
#top: it rotates x and y with the golden angle
xy<-matrix(data = NA, nrow = length(z), ncol = 2)
xy[1,]<-c(0,1)
for (i in 2:length(z)) {
  xy[i,1]<-xy[i-1,1]*cos(theta)-xy[i-1,2]*sin(theta)
  xy[i,2]<-xy[i-1,2]*cos(theta)+xy[i-1,1]*sin(theta)
}
#plot petioles
segments3D (x0=rep(0,50), y0=rep(0,50), z0=z, x1 = xy[,1], y1 = xy[,2], z1 = z+.5,add=T,col="darkgreen",lwd=3,
            phi = 30, theta = 20,bty="b")

# Petioles are of length zero a the top, increase, then saturate at a certain distance
# exponential saturating R function
f <- function(x, M, K) {
  M * ( 1 - exp(-K*x) )
}
# take z from the top
z2=10-z
# compute a proportion
(p<-f(x = z2, M = 2, K = K))
# scale x and y with the proportion p
xy2<-xy*p

# New plot
# scene
scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 30, theta = 20,bty="b")
# stem
segments3D (x0=0, y0=0, z0=0, x1 = 0, y1 = 0, z1 = 10,col="darkgreen",lwd=3,add=T)
# petiole
segments3D (x0=rep(0,50), y0=rep(0,50), z0=z, x1 = xy2[,1], y1 = xy2[,2], z1 = z+1,add=T,col="darkgreen",lwd=3)
# flowers
points3D(x = xy2[,1], y =  xy2[,2], z+1, cex = 2,pch=20,col = "grey",add=T)

# movie
for (i in 1:90) {
  png(filename = paste0("../large_files/Inflo_3D/inflo2/inflo_",sprintf("%03d", 1:100)[i],".png"),width = 600,height = 600)
  #scene
  scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 30, theta = i,bty="b")
  # stem
  segments3D (x0=0, y0=0, z0=0, x1 = 0, y1 = 0, z1 = 10,col="darkgreen",lwd=3,add=T)
  # petiole
  segments3D (x0=rep(0,50), y0=rep(0,50), z0=z, x1 = xy2[,1], y1 = xy2[,2], z1 = z+1,add=T,col="darkgreen",lwd=3,
              phi = 30, theta = 20,bty="b")
  # flowers
  points3D(x = xy2[,1], y =  xy2[,2], z+1, cex = 2,pch=20,col = "grey",add=T)
  # save
  dev.off()
}



## list file names and read in
imgs <- list.files(path = "../large_files/Inflo_3D/inflo2/", full.names = TRUE)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 20)
## view animated image
img_animated
## save to disk
image_write(image = img_animated,
            path = "inflo2.gif")

# 2 - Draw petal
################

# petal2d
plot(-2:2,-2:2)
petal<-matrix(data = c(c(1,3,4,3,0,-3,-4,-3,-1),c(3,5,9,11,12,11,9,5,3)-1),ncol = 2,byrow = F)/12
petal2<-matrix(data = c(c(3,5,9,11,12,11,9,5,3)-1,-c(1,3,4,3,0,-3,-4,-3,-1)),ncol = 2,byrow = F)/12

polygon(petal,col = "white")
polygon(-petal)
polygon(petal2)
polygon(-petal2)

# petal 3d
xs<-c(-2,-2,2,2)
ys<-c(-2,2,-2,2)
zs<-c(-2,-2,2,2)
par(bg = "#f7f7f7",fg="black")

for (i in 1:179) {
  # prepare png
  png(filename = paste0("img/petals_",sprintf("%03d", 1:179)[i],".png"),width = 600,height = 600)
  # scene
  #scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = 45,bty="u",col.axis = "black",
  #        col.panel = "#f7f7f7",
  #        col.grid = "#f7f7f7")
  #polygon3D(x = c(1,3,4,3,0,-3,-4,-3,-1)/12,z = c(2,4,8,10,11,10,8,4,2)/12, y = c(0,0,0,0,0,0,0,0,0),add=T,col = "white",border = "black")
  #points3D(0,0,0,add=T)

  #scene
  par(bg = "#f7f7f7",fg="black")
  scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = i,bty="u",
          col.axis = "black",col.panel = "#f7f7f7",col.grid = "#f7f7f7")
  points3D(0,0,0,add=T,pch=21,bg="lightgreen",cex=4)
  
  #petal coordinates
  petal<-matrix(data = c(c(1,3,4,3,0,-3,-4,-3,-1),c(0,0,0,0,0,0,0,0,0),c(2,4,8,10,11,10,8,4,2)),ncol = 3,byrow = F)/12
  #polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")
  
  #zrotation
  angle=pi/2
  petal2<-petal
  petal2[,1]<-petal[,1]*cos(angle)-petal[,2]*sin(angle)+petal[,3]*0
  petal2[,2]<-petal[,1]*sin(angle)+petal[,2]*cos(angle)+petal[,3]*0
  petal2[,3]<-petal[,1]*0+petal[,2]*0+petal[,3]*1
  
  polygon3D(x = petal2[,1], y = petal2[,2], z = petal2[,3],add=T,col = "white",border = "black")


  for (i in (1:4)*pi/2) {
    angle=i
    petal3<-petal2
    petal3[,1]<-petal2[,1]*1+petal2[,2]*0+petal2[,3]*0
    petal3[,2]<-petal2[,1]*0+petal2[,2]*cos(angle)-petal2[,3]*sin(angle)
    petal3[,3]<-petal2[,1]*0+petal2[,2]*sin(angle)+petal2[,3]*cos(angle)

    polygon3D(x = petal3[,1], y = petal3[,2], z = petal3[,3],add=T,col = "white",border = "black")
  }
  # save png
  dev.off()
}

## list file names and read in
imgs <- grep(pattern = ".png",x = list.files(path = "img/", full.names = TRUE),value = T)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 20)
## save to disk
image_write(image = img_animated,
            path = "flower3d.gif")
# remove images 
file.remove(list.files("img/",full.names = T))



# TODO
# Find how to do simply with matrices
xrotation<-function(x,angle){
  y<-x
  y[,1]<-x[,1]*1+x[,2]*0+x[,3]*0
  y[,2]<-x[,1]*0+x[,2]*cos(angle)-x[,3]*sin(angle)
  y[,3]<-x[,1]*0+x[,2]*sin(angle)+x[,3]*cos(angle)
  return(y)
}

yrotation<-function(x,angle){
  y<-x
  y[,1]<-x[,1]*cos(angle)+x[,2]*0+x[,3]*sin(angle)
  y[,2]<-x[,1]*0+x[,2]*1+x[,3]*0
  y[,3]<--x[,1]*sin(angle)+x[,2]*0+x[,3]*cos(angle)
  return(y)
}

zrotation<-function(x,angle){
  y<-x
  y[,1]<-x[,1]*cos(angle)-x[,2]*sin(angle)+x[,3]*0
  y[,2]<-x[,1]*sin(angle)+x[,2]*cos(angle)+x[,3]*0
  y[,3]<-x[,1]*0+x[,2]*0+x[,3]*1
  return(y)
}


par(bg = "#f7f7f7",fg="black")
scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = 45,bty="u",
          col.axis = "black",col.panel = "#f7f7f7",col.grid = "#f7f7f7")

petal<-matrix(data = c(c(5,6,6,5,3,-3,-5,-6,-6,-5),c(0,0,0,0,0,0,0,0,0,0),c(5,6,9,11,13,13,11,9,6,5)),ncol = 3,byrow = F)/12

polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")

petal<-yrotation(petal,pi/2)

polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")

petal<-yrotation(petal,pi/2)

polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")

petal<-yrotation(petal,pi/2)

polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")


petal1<-yrotation(petal,pi/2)
petal2<-yrotation(petal1,pi/2)
petal3<-yrotation(petal2,pi/2)
petal4<-yrotation(petal3,pi/2)

petals<-rbind(petal1,petal4,petal3,petal2)

polygon3D(x = petals[,1], y = petals[,2], z = petals[,3],add=T,col = "white",border = "black")


for (i in 1:359) {
  # prepare png
  png(filename = paste0("img/petals_",sprintf("%03d", 1:359)[i],".png"),width = 600,height = 600)
  # scene
  par(bg = "#f7f7f7",fg="black")
  scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = i,bty="u",
            col.axis = "black",col.panel = "#f7f7f7",col.grid = "#f7f7f7")
  # petals
  polygon3D(x = petals[,1], y = petals[,2], z = petals[,3],add=T,col = "white",border = "black")
  # dot
  points3D(0,0,0,add=T,pch=21,bg="lightgreen",cex=3)
  # save png
  dev.off()
}
## list file names and read in
imgs <- grep(pattern = ".png",x = list.files(path = "img/", full.names = TRUE),value = T)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 20)
## save to disk
image_write(image = img_animated,
            path = "flower3d.gif")
# remove images 
file.remove(list.files("img/",full.names = T))


# rotate the flower not the scene
for (i in 1:99) {
  # prepare png
  png(filename = paste0("img/petals_",sprintf("%03d", 1:99)[i],".png"),width = 600,height = 600)
  # scene
  par(bg = "#f7f7f7",fg="black")
  scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = 0,bty="u",
            col.axis = "black",col.panel = "#f7f7f7",col.grid = "#f7f7f7")
  petals<-zrotation(petals,(2*pi)/100*i)
  # petals
  polygon3D(x = petals[,1], y = petals[,2], z = petals[,3],add=T,col = "white",border = "black")
  # dot
  points3D(0,0,0,add=T,pch=21,bg="lightgreen",cex=3)
  # save png
  dev.off()
}
## list file names and read in
imgs <- grep(pattern = ".png",x = list.files(path = "img/", full.names = TRUE),value = T)
img_list <- lapply(imgs, image_read)
## join the images together
img_joined <- image_join(img_list)
## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 3)
## save to disk
image_write(image = img_animated,
            path = "flower3d.gif")
# remove images 
file.remove(list.files("img/",full.names = T))


petals<-xrotation(petals,pi/5)
polygon3D(x = petals[,1], y = petals[,2], z = petals[,3],add=T,col = "white",border = "black")



# petal 3d

library(plot3D)
library(magick)

xs<-c(-2,-2,2,2)
ys<-c(-2,2,-2,2)
zs<-c(-2,-2,2,2)
i=165
par(bg = "#f7f7f7",fg="black")
scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = i,bty="u",
          col.axis = "black",col.panel = "#f7f7f7",col.grid = "#f7f7f7")

# petals
petal<-matrix(data = c(c(4,5,5,4,2,0,-2,-4,-5,-5,-4),c(0,0,0,0,0,0,0,0,0,0,0),c(5,6,9,11,13,13.5,13,11,9,6,5)),ncol = 3,byrow = F)/6
polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")
petal<-yrotation(petal,pi/2)
polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")
petal<-yrotation(petal,pi/2)
polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")
petal<-yrotation(petal,pi/2)
polygon3D(x = petal[,1], y = petal[,2], z = petal[,3],add=T,col = "white",border = "black")

# basepetal
basepetal<-matrix(data = c(c(-4,4,0),c(0,0,-10),c(5,5,1)),ncol = 3,byrow = F)/6
polygon3D(x = basepetal[,1], y = basepetal[,2], z = basepetal[,3],add=T,col = "grey90",border = "black")
basepetal<-yrotation(basepetal,pi/2)
polygon3D(x = basepetal[,1], y = basepetal[,2], z = basepetal[,3],add=T,col = "grey90",border = "black")
basepetal<-yrotation(basepetal,pi/2)
polygon3D(x = basepetal[,1], y = basepetal[,2], z = basepetal[,3],add=T,col = "grey90",border = "black")
basepetal<-yrotation(basepetal,pi/2)
polygon3D(x = basepetal[,1], y = basepetal[,2], z = basepetal[,3],add=T,col = "grey90",border = "black")

# sepal bas
sepalbas<-matrix(data = c(c(1,5,2,0),c(-10,-7,-7,-10),c(0,2,5,1)),ncol = 3,byrow = F)/6
polygon3D(x = sepalbas[,1], y = sepalbas[,2], z = sepalbas[,3],add=T,col = "darkgreen",border = "black")
sepalbas<-yrotation(sepalbas,pi/2)
polygon3D(x = sepalbas[,1], y = sepalbas[,2], z = sepalbas[,3],add=T,col = "darkgreen",border = "black")
sepalbas<-yrotation(sepalbas,pi/2)
polygon3D(x = sepalbas[,1], y = sepalbas[,2], z = sepalbas[,3],add=T,col = "darkgreen",border = "black")
sepalbas<-yrotation(sepalbas,pi/2)
polygon3D(x = sepalbas[,1], y = sepalbas[,2], z = sepalbas[,3],add=T,col = "darkgreen",border = "black")
# sepal haut
sepalhaut<-matrix(data = c(c(5,5,2),c(-7,2,-7),c(2,5,5)),ncol = 3,byrow = F)/6
polygon3D(x = sepalhaut[,1], y = sepalhaut[,2], z = sepalhaut[,3],add=T,col = "darkgreen",border = "black")
sepalhaut<-yrotation(sepalhaut,pi/2)
polygon3D(x = sepalhaut[,1], y = sepalhaut[,2], z = sepalhaut[,3],add=T,col = "darkgreen",border = "black")
sepalhaut<-yrotation(sepalhaut,pi/2)
polygon3D(x = sepalhaut[,1], y = sepalhaut[,2], z = sepalhaut[,3],add=T,col = "darkgreen",border = "black")
sepalhaut<-yrotation(sepalhaut,pi/2)
polygon3D(x = sepalhaut[,1], y = sepalhaut[,2], z = sepalhaut[,3],add=T,col = "darkgreen",border = "black")

# gynecee TODO
gyn<-matrix(data = c(c(1,1,-1,-1),c(0,0,0,0),c(1,-1,-1,1)),ncol = 3,byrow = F)/6
#rotate elements from origin
gyn1<-yrotation(gyn,pi/4)
gyn2<-xrotation(gyn1,pi/2)
gyn3<-yrotation(gyn2,pi/2)
polygon3D(x = gyn1[,1], y = gyn1[,2], z = gyn1[,3],add=T,col = "lightgreen",border = "black")
polygon3D(x = gyn2[,1], y = gyn2[,2], z = gyn2[,3],add=T,col = "lightgreen",border = "black")
polygon3D(x = gyn3[,1], y = gyn3[,2], z = gyn3[,3],add=T,col = "lightgreen",border = "black")

# Stamen
Stamen<-matrix(data = c(c(1,1,-1,-1),c(0,0,0,0),c(1,-1,-1,1)),ncol = 3,byrow = F)/6
#rotate elements from origin
Stamen1<-yrotation(Stamen,pi/4)
Stamen2<-xrotation(Stamen1,pi/2)
Stamen3<-yrotation(Stamen2,pi/2)
#move to first location
Stamen1[,1]<-Stamen1[,1]+(3.5/6)
Stamen2[,1]<-Stamen2[,1]+(3.5/6)
Stamen3[,1]<-Stamen3[,1]+(3.5/6)
#rotate to the corner
Stamen1<-yrotation(Stamen1,pi/4)
Stamen2<-yrotation(Stamen2,pi/4)
Stamen3<-yrotation(Stamen3,pi/4)
#print
polygon3D(x = Stamen1[,1], y = Stamen1[,2], z = Stamen1[,3],add=T,col = "yellow",border = "black")
polygon3D(x = Stamen2[,1], y = Stamen2[,2], z = Stamen2[,3],add=T,col = "yellow",border = "black")
polygon3D(x = Stamen3[,1], y = Stamen3[,2], z = Stamen3[,3],add=T,col = "yellow",border = "black")
# rotate around y 4 times
for (j in 1:5) {
Stamen1<-yrotation(Stamen1,2*pi/6)
Stamen2<-yrotation(Stamen2,2*pi/6)
Stamen3<-yrotation(Stamen3,2*pi/6)
polygon3D(x = Stamen1[,1], y = Stamen1[,2], z = Stamen1[,3],add=T,col = "yellow",border = "black")
polygon3D(x = Stamen2[,1], y = Stamen2[,2], z = Stamen2[,3],add=T,col = "yellow",border = "black")
polygon3D(x = Stamen3[,1], y = Stamen3[,2], z = Stamen3[,3],add=T,col = "yellow",border = "black")
}

i=i+5