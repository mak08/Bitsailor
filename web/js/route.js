// web/js/route.js
import { getMap } from './map.js';

let routeLine = null;

export function displayRoute(routePoints, options = {}) {
    const map = getMap();
    if (routeLine) map.removeLayer(routeLine);
    routeLine = L.polyline(routePoints, {
        color: options.color || 'blue',
        weight: options.weight || 3,
        ...options
    }).addTo(map);
}

export function clearRoute() {
    const map = getMap();
    if (routeLine) map.removeLayer(routeLine);
    routeLine = null;
}

export { routeLine };
