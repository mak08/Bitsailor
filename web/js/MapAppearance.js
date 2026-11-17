// Reduce base-map competition without changing weather, depths, routes or labels
// drawn by Bitsailor. The normal map remains available with one checkbox.
export default function addMapAppearanceControl(map, L) {
    const key = 'xx.mutedMap';
    let muted = false;
    try { muted = localStorage.getItem(key) === 'true'; } catch (_) { /* Storage may be disabled. */ }
    const apply = () => map.getContainer().classList.toggle('muted-map', muted);
    apply();
    const control = L.control({position: 'topright'});
    control.onAdd = () => {
        const box = L.DomUtil.create('div', 'map-appearance-control');
        box.innerHTML = '<label title="Reduce base-map colour and contrast to emphasise wind and current arrows"><input type="checkbox"> Muted map</label>';
        const input = box.querySelector('input');
        input.checked = muted;
        input.addEventListener('change', () => {
            muted = input.checked;
            apply();
            try { localStorage.setItem(key, String(muted)); } catch (_) { /* Still works without storage. */ }
        });
        L.DomEvent.disableClickPropagation(box);
        L.DomEvent.disableScrollPropagation(box);
        return box;
    };
    control.addTo(map);
}
