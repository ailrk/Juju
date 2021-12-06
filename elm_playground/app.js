/* This file defines App entrance and some FFIs */
'use strict';

// embed elm
let app = Elm.Main.init(document.getElementById("elmApp"));

// config map
const config = {
  minZoom: 7,
  maxZoom: 18,
};

const zoom = 12;
const lat = 49.2500;
const lng = -123.1020;
const apiKey_ArcGIS = 'AAPKf6f0b2fbd5c0405988361a50bc878669dUayEjsv3658HpPUkVxQCeQRJpXw6U2FrYdufOKt8btY0iS5p1XdjBw0BjeSa9f7';

// static asserts
const sinkIcon = L.icon({
  iconUrl: 'assets/sink.png',
  iconSize: [32, 32],
  iconAnchor: [16, 32],
  popupAnchor: [-3, -76],
});

const sourceIcon = L.icon({
  iconUrl: 'assets/source.png',
  iconSize: [32, 32],
  iconAnchor: [16, 32],
  popupAnchor: [-3, -76],
});


// mutable states
let themap, markers, paths, currentMode, tiles, searchControl, results;

function init() {
  themap = L.map("mapid", config).setView([lat, lng], zoom);
  markers = [];
  paths = [];
  currentMode = 'sink';
  // geoencoding with ArcGIS and eris
  tiles = L.esri.basemapLayer("Streets").addTo(themap);
  searchControl = L.esri.Geocoding.geosearch({
    providers: [
      L.esri.Geocoding.arcgisOnlineProvider({
        apikey: apiKey_ArcGIS
      })
    ]
  }).addTo(themap);
  results = L.layerGroup().addTo(themap);
  searchControl.on("results", function(data) {
    results.clearLayers();
    for (var i = data.results.length - 1; i >= 0; i--) {
      results.addLayer(L.marker(data.results[i].latlng));
    }
  });
  // tile layer
  L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
    attribution:
      '&copy;<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
  }).addTo(themap);

  setupFFIs();
}

// ffis that depends on themap !== undefined.
function setupFFIs() {
  console.assert(themap !== undefined);
  app.ports.setBounds.send(themap.getBounds().toBBoxString());

  themap.on('moveend', function() {
    let bounds = themap.getBounds().toBBoxString();
    app.ports.setBounds.send(bounds);
  });

  themap.on('click', function(e) {
    pushNewMarker(e.latlng);
  });
}


// force to add a new marker.
app.ports.pushMarker_.subscribe(function(tup) {
  const [mtype, x, y] = tup;
  let oldMode = currentMode;
  currentMode = mtype;
  app.ports.toggleModeJs.send(null);
  {
    pushNewMarker(L.latLng(x, y));
  }
  currentMode = oldMode;
});


app.ports.zoomMap.subscribe(function(zoom) {
  themap.zoomIn(zoom)
});

app.ports.toggleMode_.subscribe(function(mode) {
  currentMode = mode;
});

app.ports.removeMarker.subscribe(function(markerid) {
  themap.removeLayer(markers[markerid]);
  delete markers[markerid];
});

app.ports.findPath.subscribe(function(mks) {
  let [mk1, mk2] = mks;
  shortestPath(mk1, mk2, function(e) {
    app.ports.addPath.send({
      from: mk1,
      to: mk2,
      time: e.routes[0].summary.totalTime,
      distance: e.routes[0].summary.totalDistance,
    });
  });


});

app.ports.clearAll.subscribe(function() {
  reset();
});


// release resources.
function clear() {
  if (themap !== undefined && themap !== null) {
    themap.remove();
    markers = [];
    paths = [];
  }
}

function reset() {
  clear();
  init();
}

// util functions.

function pushNewMarker(latlng) {
  let markerConfig = {
    icon: currentMode == 'source' ? sourceIcon : currentMode == 'sink' ? sinkIcon : undefined
  };

  let newMarker = new L.marker(latlng, markerConfig).addTo(themap);
  let latLng = newMarker.getLatLng();
  app.ports.addMarker.send({
    lat: latLng.lat,
    lng: latLng.lng,
    id: newMarker._leaflet_id
  });
  markers[newMarker._leaflet_id] = newMarker;
}

// find the shortest path from marker1 to marker2.
function shortestPath(marker1, marker2, cb) {
  let control = L.Routing.control({
    waypoints: [
      L.latLng(marker1.lat, marker1.lng),
      L.latLng(marker2.lat, marker2.lng),
    ],
    routeWhileDragging: true,
    // show: false,
    createMarker: function(p1, p2) { }
  });

  control.on('routesfound', function(e) {
    cb(e);
    console.log(e.routes[0].summary);
  });

  control.addTo(themap);
}

// L.latLng (49.257809,-122.990510) ,
// L.latLng(49.248709,-122.960510)

init();
