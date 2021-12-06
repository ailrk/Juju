/* This file defines App entrance and some FFIs */
'use strict';

// embed elm
let app = Elm.Main.init(document.getElementById("elmApp"));
// config map
let config = {
  minZoom: 7,
  maxZoom: 18,
};

const zoom = 12;
const lat = 49.2500;
const lng = -123.1020;

const themap = L.map("mapid", config).setView([lat, lng], zoom);

// Used to load and display tile layers on the map
// Most tile servers require attribution, which you can set under `Layer`
L.tileLayer("https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
  attribution:
    '&copy;<a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
}).addTo(themap);


app.ports.setBounds.send(themap.getBounds().toBBoxString());


themap.on('moveend', function() {
  var bounds = themap.getBounds().toBBoxString();
  app.ports.setBounds.send(bounds);
});

app.ports.zoomMap.subscribe(function(zoom) {
  themap.zoomIn(zoom)
});

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
let markers = {};
let currentMode = 'sink';


themap.on('click', function(e) {
  let markerConfig = {
    icon: currentMode == 'source' ? sourceIcon : sinkIcon
  };

  let newMarker = new L.marker(e.latlng, markerConfig).addTo(themap);
  console.log(newMarker);
  let latLng = newMarker.getLatLng();

  app.ports.addMarker.send({
    lat: latLng.lat,
    lng: latLng.lng,
    id: newMarker._leaflet_id
  });
  markers[newMarker._leaflet_id] = newMarker;
});

app.ports.removeMarker.subscribe(function(markerid) {
  themap.removeLayer(markers[markerid]);
  delete markers[markerid];
});

app.ports.toggleMode.subscribe(function(mode) {
  currentMode = mode;
});
