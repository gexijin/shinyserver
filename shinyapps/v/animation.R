library(nCov2019)
library(chinamap)
cn = get_map_china()
x = load_nCov2019(lang="zh")
plot(x, region='china', chinamap=cn, date = '2020-2-10',
     continuous_scale=FALSE,
     palette='Blues')

library(magick)

d <- paste0("2020-01-", seq(19,31,3))
d <- c(d, paste0("2020-02-0", seq(1,27,3))) 

img <- image_graph(600, 450, res = 96)
out <- lapply(d, function(date){
  p <- plot(x, region='china', chinamap=cn, date=date,
            label=FALSE, continuous_scale=FALSE)
  print(p)
})
dev.off()

animation <- image_animate(img, fps = 1)
image_write(animation, "ChinaMapAnimated2.gif")




from = "2020-02-18"
to = "2020-02-20"
y <- load_nCov2019(lang = 'en')
# To generate a historical world map;
# with default figure size and save with default filename
# the gif file will be saved in current working directory 
plot(y, from = from, to = to)
plot(y, 
     width = 900, height = 900,
     label=FALSE,
     from="2020-01-15", to="2020-02-29", 
     filename='world.gif')


# China 

y <- load_nCov2019(lang = 'en')
cn = get_map_china()
cn$province <- trans_province(cn$province)
plot(y, region="china", chinamap=cn,
      width = 600, height = 600,
     label=FALSE,
     from="2020-01-15", to="2020-02-10", 
     filename='china4.gif')
