#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Tue Feb 27 12:07:02 2018

@author: simontopp
"""
import ee
# Can't import because of variable references, #TODO revamp functions to import as module
#from GEE_pull_functions import maximum_no_of_tasks, dpBuff, RefPull
exec(open('2_rsdata/src/GEE_pull_functions.py').read())
ee.Initialize()

#water = ee.Image("JRC/GSW1_1/GlobalSurfaceWater").select('occurrence').gt(80)

## Bring in EE Assets
# Deepest point for CONUS Hydrolakes from Xiao Yang
# Code available https://zenodo.org/record/4136755#.X5d54pNKgUE
#dp = (ee.FeatureCollection('users/michaelfrederickmeyer/NHD_CO')
#  .filterMetadata('type','equals','dp')
#  .filterMetadata('distance', "greater_than", 60))

#CONUS Boundary
us = ee.FeatureCollection("USDOS/LSIB_SIMPLE/2017")\
    .filterMetadata('country_na', 'equals', 'United States')
#US States
states = ee.FeatureCollection("TIGER/2018/States")

## WRS Tiles in descending (daytime) mode for CONUS
wrs = ee.FeatureCollection('users/sntopp/wrs2_asc_desc')\
    .filterMetadata('MODE', 'equals', 'D')
    
dummyBands = ee.Image(-99).rename('Null_CS')\
    .addBands(ee.Image(-99).rename('Null_TIR2'))

def addDummy(i):
    return i.addBands(dummyBands)

#TODO this should all be Collection 2! Need to Updated DSWE first though.
l8 = ee.ImageCollection('LANDSAT/LC08/C01/T1_SR')
l7 = ee.ImageCollection('LANDSAT/LE07/C01/T1_SR')\
    .map(addDummy)
l5 = ee.ImageCollection('LANDSAT/LT05/C01/T1_SR')\
    .map(addDummy)

#Standardize band names between the various collections and aggregate 
#them into one image collection
#TODO Add panchromatic band
bn8 = ['B1','B2','B3', 'B4', 'B5','B6','B7', 'B10','B11','pixel_qa']
bn57 = ['Null_CS', 'B1', 'B2', 'B3', 'B4', 'B5', 'B7', 'B6','Null_TIR2', 'pixel_qa']
bns = ['Aerosol','Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'TIR1','TIR2','pixel_qa']
  
ls5 = l5.select(bn57, bns)
ls7 = l7.select(bn57, bns)
ls8 = l8.select(bn8, bns)

ls = ee.ImageCollection(ls5.merge(ls7).merge(ls8))\
    .filter(ee.Filter.lt('CLOUD_COVER', 75))\
    .filterBounds(us)  

deepest_point_assets = ee.data.listAssets({'parent': 'projects/earthengine-legacy/assets/users/sntopp/NHD/DeepestPoint'})['assets']
## For testing on just a couple spots
#deepest_point_assets = deepest_point_assets[0:2]

for i in deepest_point_assets:
    ## Could potentially swap this and do it by state first. Might make front end filtering
    ## Easier
    state_id = i['id'].split('/')[-1].split('_')[-1]
    state = states.filter(ee.Filter.eq('STUSPS',state_id)).first()

    ### Each state asset has overlap with neighboring states, remove overlap
    deepest_points_state = ee.FeatureCollection(i['id']).filterBounds(state.geometry())\
        .filterMetadata('type','equals','dp')\
        .filterMetadata('distance', "greater_than", 60)

    ##For testing on tiny portion of lakes
    ##deepest_points_state=deepest_points_state.randomColumn().filterMetadata('random','less_than',.05)

    ### Pull tiles
    wrs_state = wrs.filterBounds(state.geometry())
    path_rows = wrs_state.aggregate_array('PR').getInfo()
    for tiles in path_rows:
        tile = wrs_state.filterMetadata('PR', 'equals', tiles).first()

        lakes = deepest_points_state.filterBounds(tile.geometry())\
            .map(dpBuff)

        #lakes = ee.FeatureCollection(lakes.toList(10000))
        stack = ls.filterBounds(tile.geometry().centroid())
        out = stack.map(RefPull).flatten().filterMetadata('cScore_clouds','less_than',.5)
        dataOut = ee.batch.Export.table.toDrive(collection = out,\
                                                description = f"{state_id}_"+str(tiles),\
                                                folder = 'NHD_DP_LakeStacks',\
                                                fileFormat = 'csv',\
                                                selectors = ['Aerosol','Blue', 'Green', 'Red', 'Nir', 'Swir1', 'Swir2', 'TIR1','TIR2','pixel_qa', 'hillShadow', 'sd_NirSD','cScore_clouds','pCount_dswe3','pCount_dswe1','system:index','permanent','distance'])
        #Check how many existing tasks are running and take a break if it's >25
        maximum_no_of_tasks(25, 120)
        #Send next task.
        dataOut.start()
        print('done_' + f"{state_id}_"+str(tiles))

