// Reduce base-map competition without changing weather, depths, routes or labels
// drawn by Bitsailor. The normal map remains available with one checkbox.
export default function bindMapAppearance(map, input) {
    const key = 'xx.mutedMap';
    let muted = false;
    try { muted = localStorage.getItem(key) === 'true'; } catch (_) { /* Storage may be disabled. */ }
    const apply = () => map.getContainer().classList.toggle('muted-map', muted);
    apply();
    input.checked = muted;
    input.addEventListener('change', () => {
        muted = input.checked;
        apply();
        try { localStorage.setItem(key, String(muted)); } catch (_) { /* Still works without storage. */ }
    });
}
