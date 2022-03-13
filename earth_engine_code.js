

var karnataka = ee.FeatureCollection("users/naiacasina/karnataka_state_boundary"),
    chirps = ee.ImageCollection("UCSB-CHG/CHIRPS/PENTAD"),
    dodd = ee.FeatureCollection("users/naiacasina/region0"),
    hosakote = ee.FeatureCollection("users/naiacasina/hosakote"),
    nelm = ee.FeatureCollection("users/naiacasina/Nelmangala"),
    telangana = ee.FeatureCollection("users/naiacasina/Telangana");


/*===========================================================

   Author: Naia Ormaza Zulueta.
   Computing a drought index through deviations from the LPA
   Regions of interest: Karnataka and Telangana (India)
   
   ==========================================================*/
   
/* Personal notes:

- Calculate Longterm Rainfall Average
- CHIRPS Data comes as Pentad
- Long Period Avergaes (LPA) should by 30 Years or more
*/


// List of 30 years
var lpaYears = ee.List.sequence(1981,2001)
var months = ee.List.sequence(1,12)

print(lpaYears)

//var julyimages = chirps.filter(ee.Filter.calendarRange(7,7,'month'))
//print(julyimages)

// Map over the Years and create a monthly total collection
var monthlyImages = lpaYears.map(function(year){
  return months.map(function(month){
    var filtered = chirps.filter(ee.Filter.calendarRange(year,year,'year'))
    .filter(ee.Filter.calendarRange(month,month,'month'))
    
    var monthlyTotal = filtered.sum()
    return monthlyTotal.set({'month' : month, 'year': year})
  })
}).flatten()

// 360 images out of 30 years (12 months/year)

/* flatten: double list into a single list
print(monthlyImages)

var nestedList = ee.List([['a','b'],['c','d'],['e','f']])
var flattened = nestedList.flatten()
print(flattened) */

var monthlyCol = ee.ImageCollection.fromImages(monthlyImages)

var longTermMeans = months.map(function(month){
  var filtered = monthlyCol.filter(ee.Filter.eq('month',month))
  var monthlyMean = filtered.mean()
  return monthlyMean.set('month',month)
})

var monthlyRainfall = ee.ImageCollection.fromImages(longTermMeans)

print(monthlyRainfall)

var chart = ui.Chart.image.series({
  imageCollection: monthlyRainfall,
  region: telangana.geometry(),
  reducer: ee.Reducer.mean(),
  scale: 5000,
  xProperty: 'month'
}).setOptions({
  linewidth: 1,
  pointSize: 3,
  title: 'Long Term Monthly Mean rainfall over hosakote',
  vAxis: {title: 'Rainfall (mm)'},
  hAxis: {title: 'Month', gridlines: {count: 12}}
})

print(chart)

// Now we take 2020 data and compute monthly average
var filtered = chirps
  .filter(ee.Filter.date('2009-01-01','2009-12-31'))
  .filter(ee.Filter.bounds(telangana))

// Calculate monthly average rainfall

var monthlyTotals = months.map(function(month) {
  return filtered.filter(ee.Filter.calendarRange(month,month,'month'))
    .sum()
    .set('month',month)
  
});
var observedRainfall = ee.ImageCollection.fromImages(monthlyTotals)
print(observedRainfall)

// Caclulate deviation in %

var deviation = months.map(function(month) {
  var longTermMean = monthlyRainfall.filter(ee.Filter.eq('month',month)).first()
  var monthlyObserved = observedRainfall.filter(ee.Filter.eq('month',month)).first()
  var deviation = (monthlyObserved.subtract(longTermMean).divide(longTermMean)).multiply(100)
    .set('month',month)
  return deviation
})

var chart = ui.Chart.image.series({
  imageCollection: deviation,
  region: telangana,
  reducer: ee.Reducer.mean(),
  scale: 10000,
  xProperty: 'month'
}).setOptions({
  linewidth: 1,
  pointSize: 3,
  title: 'Deviation in %',
  vAxis: {title: 'Percentage deviation'},
  hAxis: {title: 'Month', gridlines: {count: 12}}
})

print(chart)

