// web/js/markers.js
import { getMap } from './map.js';

let startMarker = null;
let destinationMarker = null;
let checkpointMarkers = [];

function createMarker(imageUrl, anchorPos=[0, 0], popupPos=[0, 0]) {
    return L.icon({
        iconUrl: imageUrl,
        iconAnchor: anchorPos,
        popupAnchor: popupPos,
    });
}

export function addStartMarker(latlng, options = {}) {
    const map = getMap();
    if (!map) {
        console.error('Map not initialized when adding start marker');
        return null;
    }
    if (startMarker) map.removeLayer(startMarker);
    startMarker = L.marker(latlng, {
        title: 'Start',
        icon: createMarker('img/start_45x32.png', [22, 32], [0, -32]),
        draggable: true,
        ...options
    }).addTo(map);
    return startMarker;
}

export function addDestinationMarker(latlng = [0, 0], options = {}) {
    const map = getMap();
    if (destinationMarker) map.removeLayer(destinationMarker);
    destinationMarker = L.marker(latlng, {
        title: 'Destination',
        icon: createMarker('img/finish_32x20.png', [16, 16], [0, -16]),
        draggable: true,
        ...options
    }).addTo(map);
    return destinationMarker;
}

export function addCheckpointMarkers(checkpoints) {
    const map = getMap();
    checkpointMarkers.forEach(m => map.removeLayer(m));
    checkpointMarkers = checkpoints.map(c =>
        L.marker([c.lat, c.lon], {
            title: c.name || 'Checkpoint',
            icon: L.icon({
                iconUrl: 'img/marker_32x12.png',
                iconAnchor: [16, 16],
                popupAnchor: [0, -16],
            }),
            draggable: false
        }).addTo(map)
    );
    return checkpointMarkers;
}

export { startMarker, destinationMarker, checkpointMarkers };
