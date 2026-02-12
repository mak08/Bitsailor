////////////////////////////////////////////////////////////////////////////////
/// Shared Server Settings loader/cacher

import { doGET } from './Util.js';

let serverSettings = null;
let settingsPromise = null;

export function loadServerSettings() {
    if (settingsPromise) return settingsPromise;
    settingsPromise = new Promise((resolve, reject) => {
        doGET(
            "/function/router.getServerSettings",
            function(xhr) {
                try {
                    serverSettings = JSON.parse(xhr.responseText);
                    resolve(serverSettings);
                } catch (e) {
                    reject(e);
                }
            },
            function(xhr) {
                console.error('Could not load server settings');
                reject(new Error(xhr.statusText || 'Could not load server settings'));
            }
        );
    });
    return settingsPromise;
}

export async function ensureServerSettings() {
    if (serverSettings) return serverSettings;
    await loadServerSettings();
    return serverSettings;
}

export function getServerSettings() {
    return serverSettings;
}

export function getDatasourceByName(name) {
    const ss = serverSettings;
    if (!ss || !ss.datasources) return undefined;
    return ss.datasources.find((entry) => entry.name === name);
}
