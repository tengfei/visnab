## Demo
require(rtracklayer)
require(visnab)
data(kgsub)
obj <- IntervalView(kg.chr1.sub,idname='name')
print(obj)

