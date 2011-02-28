##----------------------------------------------------------------##
##                Create a dockable zoomable view
##----------------------------------------------------------------##

widgetZoom <<- Qt$QWidget()
layoutZoom <<- Qt$QGridLayout()
widgetZoom$setLayout(layoutZoom)

sceneZoomChrom <<-qscene()
##  sceneZoomScale<<-qscene()
sceneZoomTrack <<- qscene()
layerZoomChrom<<-qlayer(sceneZoomChrom,paintFun=gDrawSingleChrom,
                        limits=qrect(c(0,550),c(-50,40)),cache=FALSE)
##  layerZoomScale<<-qlayer(sceneZoomScale,paintFun=scaleTrack,
## limits=qrect(c(0,20),c(0,5)),
## cache=FALSE)
layerZoomTrack<<-qlayer(sceneZoomTrack,wheelFun=wheelZoom,
                        geometry=qrect(0,0,600,600),cache=FALSE)

layerZoomTrackExon<<-qlayer(layerZoomTrack,paintFun=exonTrack,row=1,cache=FALSE)
layerZoomTrackTx <<- qlayer(layerZoomTrack,paintFun=txTrack,row=2,cache=FALSE)
layerZoomTrackCov<<-qlayer(layerZoomTrack,paintFun=covPaintFun,row=3,cache=FALSE)
xlim <- c(min(zoomObj@start),max(zoomObj@end))
layerZoomTrackExon$setLimits(qrect(xlim,c(0,5+5)))
layerZoomTrackTx$setLimits(qrect(xlim,c(0,5+5)))
layerZoomTrackCov$setLimits(qrect(xlim,c(-10,80+10)))
viewZoomChrom <<-qplotView(sceneZoomChrom)
##  viewZoomScale <<-qplotView(sceneZoomScale)
viewZoomTrack <<-qplotView(sceneZoomTrack,rescale='none')
layoutZoom$addWidget(viewZoomChrom,0,0)
##  layoutZoom$addWidget(viewZoomScale,1,0)
layoutZoom$addWidget(viewZoomTrack,2,0)
layoutZoom$setRowStretch(0,1)
##  layoutZoom$setRowStretch(1,1)
layoutZoom$setRowStretch(2,5)
##  layerZoomTrack$gridLayout()$setRowStretchFactor(0,1)
layerZoomTrack$gridLayout()$setRowStretchFactor(1,4)
layerZoomTrack$gridLayout()$setRowStretchFactor(2,4)
layerZoomTrack$gridLayout()$setRowStretchFactor(3,5)
layoutZoom$setVerticalSpacing(0)


## experiment with qcursor shape
