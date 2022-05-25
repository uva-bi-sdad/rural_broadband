require(sp)
spPlot <- function(length = NULL,
                   col_text = NULL,
                   data_frame = NULL,
                   shape = NULL,
                   zlim = NULL,
                   xlim = NULL,
                   ylim = NULL,
                   main="",
                   contour.plot=T){
  col_br  <- colorRampPalette(RColorBrewer::brewer.pal(length,col_text))
  surf <- MBA::mba.surf(data_frame,no.X=300,no.Y=300,extend=T,sp=T)$xyz.est
  if(!is.null(shape)){
    sp::proj4string(surf) <- sp::proj4string(shape)
    surf <- surf[!is.na(over(surf,shape))[,1],]
    surf <- as.image.SpatialGridDataFrame(surf)
    if(is.null(zlim)) zlim <- range(surf[["z"]],na.rm=T)#; print(zlim)
    if(is.null(xlim)) xlim <- shape@bbox["x",]
    if(is.null(ylim)) ylim <- shape@bbox["y",]
    fields::image.plot(surf, xaxs = "i", yaxs = "i", col = col_br(1e2), axes = T,
                       zlim=zlim,
                       xlim=xlim,
                       ylim=ylim,
                       xlab=latex2exp::TeX("Longitude$\\degree$"),
                       ylab=latex2exp::TeX("Latitude$\\degree$"))
    title(main=main)
   plot(shape,add=T, border="darkred")
  }else{
    fields::image.plot(surf, xaxs = "i", yaxs = "i", col = col_br(1e2), axes = T,
                       zlim=range(surf$z),
                       xlim=xlim,
                       ylim=ylim,
                       xlab=latex2exp::TeX("Longitude$\\degree$"),
                       ylab=latex2exp::TeX("Latitude$\\degree$"))
  }
  
  #points(data_frame[,c(1,2)], pch="+", cex=0.2)
  if(contour.plot) contour(surf,add=T)
}