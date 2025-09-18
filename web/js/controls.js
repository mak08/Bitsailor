// web/js/controls.js
import { addStartMarker, addDestinationMarker } from './markers.js';
import { displayRoute, clearRoute } from './route.js';

export function initControls() {
    const setStartBtn = document.getElementById('bt_setstart');
    if (setStartBtn) setStartBtn.onclick = () => addStartMarker();

    const setDestBtn = document.getElementById('bt_setdest');
    if (setDestBtn) setDestBtn.onclick = () => addDestinationMarker();

    const clearRouteBtn = document.getElementById('bt_clearroute');
    if (clearRouteBtn) clearRouteBtn.onclick = () => clearRoute();

    // Add more control/event wiring as needed
}
