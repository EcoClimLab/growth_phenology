/////////////////////////////////////////////////////////////////////////
//      Get phenometrics for SCBI and other dendro sites               //  
//      Step 1 = prepare metadata                                      //
//      Step 2 = get the phenometrics from mcd12q2                     //
//      Step 3 = get continuous EVI2 values for the same               //
//               timeframe as step 2                                   //
//                                                                     //
//      Before running: upload csv of neon towers with lat and lon     //
//                        as an asset labeled "table"                  //
//                      ALSO - update your google folder for exporting //
//                        in Steps 2 and 3                             //
//                                                                     //
//      Output: This script outputs two csv files,                     //  
//              - one with the phenometrics for desired NEON tower     //
//                locations ()                                         //
//              - one with the daily EVI2 values for the tower         //  
//                locations for the same timeframe as the phenometrics //
//                                                                     //
// Created by: Ian McGregor, imcgreg@ncsu.edu                          //
// Dec. 2020, last updated Oct 2021                                    //
/////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////
/////////// Step 1: Prepare the metadata ////////////////////////////////
/////////////////////////////////////////////////////////////////////////

print('original csv', table);

var points = ee.FeatureCollection(table)
   .filterMetadata('target', "equals", 1)
   .map(function(feature) {
      var lon = feature.get('field_longitude');
      var lat = feature.get('field_latitude');
      return ee.Feature(ee.Geometry.Point([lon, lat]), {
        'featureID': ee.String(feature.get('field_site_id'))
    });
  });
print('points', points);

Map.centerObject(points.first(), 4);
Map.addLayer(points, {color: "blue"}); 

//id and coordinates for each of the points
var ids = points.aggregate_array('featureID'); print('ids', ids);
var coords = points.geometry().coordinates(); print('coords', coords);

// make index for the points
var index = ee.List.sequence(0,coords.size().subtract(1)); 
print('loop size', index.size());

// get the list of bands for below
//var test = ee.Geometry.Point(coords.get(0));
//var coll = ee.ImageCollection("MODIS/006/MCD12Q2").filterBounds(blarg);
//var bands = coll.first().bandNames(); print('bands', bands)

// create index to map over. Note that because we're only looking
/// at SCBI and Harvard Forest, we're only making a list of 2
var index = ee.List.sequence(0,1);

/////////////////////////////////////////////////////////////////////////
/////////// Step 2: Get phenometrics from MCD12Q2 ///////////////////////
/////////////////////////////////////////////////////////////////////////

/// For a description of the data, see
/// https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2#bands

// run the loop
var loop = index.map(function(ind){
  var thispoint = ee.Geometry.Point(coords.get(ind));
    var coll = ee.ImageCollection("MODIS/006/MCD12Q2").filterBounds(thispoint);
    var getvals = function(img1){
      var bands = img1.bandNames();
      var date = img1.date().format('yyyy-MM-dd'); 
      var spec = thispoint.coordinates();
      var value = img1.select(bands)
        .reduceRegion(ee.Reducer.first(), thispoint);
      
      var output = ee.Feature(ee.Geometry.Point(coords.get(ind)), 
        {date: date, lat: spec.get(0), lon: spec.get(1), pointid: ids.get(ind)})
          .set(value);
      return output;
    };
    var newft = coll.map(getvals);
    return(newft);
  });
print('loopresult', loop);
print('loop example', ee.FeatureCollection(loop.get(0)));

// create the FC to export
var test = ee.FeatureCollection(loop).flatten();

// write list of band values to write to csv table
var mcd12q2bands = ['NumCycles', 'Greenup_1','Greenup_2','MidGreenup_1','MidGreenup_2',
'Peak_1','Peak_2','Maturity_1','Maturity_2','Senescence_1','Senescence_2',
'MidGreendown_1','MidGreendown_2','Dormancy_1','Dormancy_2','EVI_Minimum_1',
'EVI_Minimum_2','EVI_Amplitude_1','EVI_Amplitude_2','EVI_Area_1','EVI_Area_2',
'QA_Overall_1','QA_Overall_2','QA_Detailed_1','QA_Detailed_2'
];
var allBands = ['date', 'lat', 'lon', 'pointid'] + ',' + mcd12q2bands;

// export to csv
Export.table.toDrive(
    {collection: test,
    description: 'scbiPhen',
    folder: 'scbi',
    fileNamePrefix: 'scbiPhen',
    selectors: allBands
    }
  );


/////////////////////////////////////////////////////////////////////////
/////////// Step 3: Get EVI2 from MCD43A4 ///////////////////////////////
/////////////////////////////////////////////////////////////////////////

/// For a description of the data, see 
/// https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD43A4#bands

// MODIS B1=Red; B2=NIR; B3=Blue; B4=Green
// EVI2 equation: 2.5 * ( NIR - RED) / ( NIR + 2.4 * RED + 1.0 )
var getEVI2 = function(image) {
  var RED = image.select('Nadir_Reflectance_Band1');
  var NIR = image.select('Nadir_Reflectance_Band2');
  var EVI2 = ((NIR.subtract(RED)).divide(NIR.add(RED.multiply(2.4)).add(1))).multiply(2.5).rename('EVI2');
  return image.addBands(EVI2);
};

// Create a QA mask function - only want good quality data
var filterqa = function(image){ 
  var mask1 = image.select("BRDF_Albedo_Band_Mandatory_Quality_Band1").eq(0);
  var mask2 = image.select("BRDF_Albedo_Band_Mandatory_Quality_Band2").eq(0);
  return image.updateMask(mask1).updateMask(mask2);
};

// Create the original collection
/// note that the earliest image is 2000-02-18
var origMOD = ee.ImageCollection("MODIS/006/MCD43A4")
                                .map(getEVI2)
                                //.map(filterqa)
                                .filterDate("2000-02-18", "2019-01-01");

// run the loop
var loop = index.map(function(ind){
  var thispoint = ee.Geometry.Point(coords.get(ind));
    var coll = origMOD.filterBounds(thispoint);
    var getvals = function(img1){
      var bands = img1.bandNames();
      var date = img1.date().format('yyyy-MM-dd'); 
      var spec = thispoint.coordinates();
      var value = img1.select(bands)
        .reduceRegion(ee.Reducer.first(), thispoint);
      
      var output = ee.Feature(ee.Geometry.Point(coords.get(ind)), 
        {date: date, lat: spec.get(0), lon: spec.get(1), pointid: ids.get(ind)})
          .set(value);
      return output;
    };
    var newft = coll.map(getvals);
    return(newft);
  });
print('loopresult', loop);
print('loop example', ee.FeatureCollection(loop.get(0)));

// create the FC to export
var test = ee.FeatureCollection(loop).flatten();
//print(test.first())

// write list of band values to write to csv table
var mcd43a4bands = ['Nadir_Reflectance_Band1', 'Nadir_Reflectance_Band2',
'BRDF_Albedo_Band_Mandatory_Quality_Band1',
'BRDF_Albedo_Band_Mandatory_Quality_Band2',
'EVI2'
];
var allBands = ['date', 'lat', 'lon', 'pointid'] + ',' + mcd43a4bands;

// export to csv
Export.table.toDrive(
    {collection: test,
    description: 'scbiEVI2',
    folder: 'scbi',
    fileNamePrefix: 'scbiEVI2',
    selectors: allBands
    }
  );