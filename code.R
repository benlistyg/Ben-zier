library(bezier)
library(magick)
library(imager)

png(filename = 'bez.png',width = 11,height = 8.5,units = 'in', res = 200)

# p_ = toupper(paste("#",c("01befe","ffdd00","ff7d00","ff006d","adff02","8f00ff"),sep = ""))

p_ = c('black','white','gray')

p_ = c('cornflowerblue','red','goldenrod1','black')

par(omi = c(0,0,0,0), mgp = c(0,0,0), mar = c(0,0,0,0), family = "D",bg = 'goldenrod1')

plot(x = c(0:10), 
     y = c(0:10), 
     type ='n',
     xaxt = 'n', 
     yaxt = 'n', 
     ylab = NA, 
     xlab = NA, 
     frame.plot = F,
     ylim = c(0,10),
     xlim = c(0,10))

nc = 5
nr = 5

replicate(n = 100, expr = {
  
  l = 500
  
  t = seq(0, 0.5, length.out = l)
  
  p = matrix(sample(seq(1,9,.25), nc*nr, replace = T), ncol = nc)

  bezier_points = bezier(t=t, p=p)
  
  # bezier_points = bezier_points + rnorm(n = nc*nr, mean = 0, sd = 0.5)
  
  points(bezier_points, 
         col = scales::alpha(plotrix::smoothColors(sample(p_,1), 
                                                   l-2, 
                                                   sample(p_,1)),
                             alpha = .025),
         pch=16,
         cex = sample(c(10,15),1))
  
})

dev.off()

img <- magick::image_read(path = 'bez.png')

# img <- magick::image_trim(img)

img <- image_implode(img, factor = sample(seq(0.6,0.7,0.1),1))

par(omi = c(0,0,0,0), mgp = c(0,0,0), mar = c(0,0,0,0), family = "D",bg = 'white')

img <- image_convert(image = img, format = 'png')

img <- image_noise(img,noisetype = 'Laplacian')

# img <- image_emboss(img, radius = 0.5, sigma = 0.01)

image_write(image = img, path = 'dither_bez.png', format = 'png')

par(omi = c(0,0,0,0), mgp = c(0,0,0), mar = c(0,0,0,0), family = "D",bg = 'white')

plot(img)
