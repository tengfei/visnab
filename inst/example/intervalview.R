require(rtracklayer)
require(visnab)
session <- browserSession()
kg <- track(session, "knownGene", "hg19",asRangedData=FALSE)
## you can specify what to show in the tooltips,
## the name should be one of the colnames of the value(obj)
names(values(kg))                       #choose name slots to show in tooltip
obj <- IntervalView(kg,idname='name')
print(obj)
## try show different stuff
obj <- IntervalView(kg,idname='blockCount')
print(obj)
