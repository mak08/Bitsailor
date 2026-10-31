////////////////////////////////////////////////////////////////////////////////
/// Shared Server Settings loader/cacher


let serverSettings = null;
let settingsPromise = null;

export function loadServerSettings() {
    if (settingsPromise) return settingsPromise;
    settingsPromise = fetch('/function/router.getServerSettings', {
        cache: 'no-cache', signal: AbortSignal.timeout(15000)
    }).then(response => {
        if (!response.ok) throw new Error(`Server settings: HTTP ${response.status}`);
        return response.json();
    }).then(data => {
        if (!data || !Array.isArray(data.datasources)) throw new Error('Invalid server settings');
        serverSettings = data;
        return data;
    }).catch(error => {
        // A rejected cached promise must not prevent a later retry.
        settingsPromise = null;
        throw error;
    });
    return settingsPromise;
}

export function reloadServerSettings() {
    // Reset the cache to force a reload
    serverSettings = null;
    settingsPromise = null;
    return loadServerSettings();
}

export async function ensureServerSettings() {
    if (serverSettings) return serverSettings;
    await loadServerSettings();
    return serverSettings;
}

export function getServerSettings() {
    return serverSettings;
}

// Archived routing cycles must be queried explicitly without replacing the live preview settings.
const cyclePromises = new Map();
export function loadForecastCycle(model, cycle) {
    const cycleString = new Date(cycle).toISOString();
    const key = `${model}/${cycleString}`;
    if (!cyclePromises.has(key)) {
        const query = new URLSearchParams({ forecast_model: model, cycle: cycleString });
        const promise = fetch(`/function/router.getServerSettings?${query}`, {
            cache: 'no-cache', signal: AbortSignal.timeout(15000)
        }).then(response => {
            if (!response.ok) throw new Error(`Forecast cycle metadata: HTTP ${response.status}`);
            return response.json();
        }).catch(error => { cyclePromises.delete(key); throw error; });
        cyclePromises.set(key, promise);
    }
    return cyclePromises.get(key);
}
