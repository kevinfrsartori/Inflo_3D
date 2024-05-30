#--------------------------------------
#
# First explorations on how to generate a 3D mesh with R
#
#--------------------------------------



#install.packages("plot3D")
#install.packages("magick")
library(plot3D)
library(magick)

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
scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = 45,bty="u",col.axis = "black",
          col.panel = "#f7f7f7",
          col.grid = "#f7f7f7")
polygon3D(x = c(1,3,4,3,0,-3,-4,-3,-1)/12,z = c(2,4,8,10,11,10,8,4,2)/12, y = c(0,0,0,0,0,0,0,0,0),add=T,col = "white",border = "black")
points3D(0,0,0,add=T)

# rotations and duplication
par(bg = "#f7f7f7",fg="black")
scatter3D(xs, ys, zs, pch = 21, cex = .5,colkey = F,phi = 10, theta = 45,bty="u",
          col.axis = "black",col.panel = "#f7f7f7",col.grid = "#f7f7f7")
points3D(0,0,0,add=T,pch=21,bg="white",cex=4)
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

# find how to do simply with matrices
zrotation<-matrix( data = c( c( cos(angle),-sin(angle), 0),
                             c( sin(angle), cos(angle), 0),
                             c( 0         , 0         , 1)), nrow = 3, byrow = T)

petal4<-(petal %*% t(zrotation))
polygon3D(x = petal4[,1], y = petal4[,2], z = petal4[,3],add=T,col = "red",border = "black")
# so far not good

