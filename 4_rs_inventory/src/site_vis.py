#Import Libraries
import ee
ee.Initialize()
import pandas as p
import feather as f
import time

#Read in the water mask
pekel = ee.Image('JRC/GSW1_0/GlobalSurfaceWater')

#Point to the inventory feather
file = '2_rsdata/out/unique_site_inventory.feather'

#Read in our inventory feather
inv = f.read_dataframe(file)

#Get the length of unique sites
obs = len(inv.index) 

#To convert these sites into featurecollections we have to split it up 
#in a loop. 
#List comprehension loop requires exact indecis to convert site lat long
#into gee collections
splitStart = list(range(0, obs, 5000))
splitEnd=list(range(5000,obs + 5000 ,5000))
splitEnd[-1]=obs

#Selct the occurence layer in the pekel mask, which is just the percentage of water occurence
#over a given pixel from 1985-2015. 
occ = pekel.select('occurrence')


#####----------Earth Engine Pull here ---------######


#Define our water extraction function outside of the loop 
#so it is not defined over and over

def waterfunc(buf):
  #Define a 200m buffer around each point
  invBuf = buf.buffer(200).geometry()
#Clip the pekel mask to this buffer
pekclip = occ.clip(invBuf)
#Reduce the buffer to pekel min and max
pekMin = pekclip.reduceRegion(ee.Reducer.minMax(), invBuf, 30)
#Add another reducer to get the median pekel occurnce
pekMed = pekclip.reduceRegion(ee.Reducer.median(),invBuf,30)
#Define the output features
out = buf.set({'max':pekMin.get('occurrence_max')})\
.set({'min':pekMin.get('occurrence_min')})\
.set({'med':pekMed.get('occurrence')})

return out

#Source function to limit number of tasks sent up to earth engine.
exec(open("2_rsdata/src/GEE_pull_functions.py").read())    
#Loop over the index stored in split
for x in range(0,len(splitStart)):
  #turn our inventory into a feature collection by assigning lat longs and a site id.
  #This is done via list comprehension which is similar to a for loop but faster and
  #plays nice with earth engine.  Collections are limited for 5000 to avoid time outs
  #on the server side.
  invOut = ee.FeatureCollection([ee.Feature(
    ee.Geometry.Point([inv['long'][i], inv['lat'][i]]),
    {'SiteID':inv['SiteID'][i]}) for i in range(splitStart[x], splitEnd[x])]) 
#Map this function over the 5000 or so created sites
outdata = invOut.map(waterfunc)
#Define a data export 
dataOut = ee.batch.Export.table.toDrive(collection = outdata,
                                        description = "LandsatSitePull" + str(x),
                                        folder='tempSiteWater',
                                        fileFormat = 'csv')
#Send next task.
dataOut.start()
#Make sure all Earth engine tasks are completed prior to moving on.  
maximum_no_of_tasks(1,60)
print('done')

## End the python bash.
