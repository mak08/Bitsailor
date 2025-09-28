////////////////////////////////////////////////////////////////////////////////
/// Polar Data Management for BitSailor
/// Handles loading and format detection for polar data

import * as Util from './Util.js';
import VRPolarAnalyzer from './VRPolarAnalyzer.js';
import FWPolarAnalyzer from './FWPolarAnalyzer.js';

/**
 * PolarManager acts as a factory and cache for PolarAnalyzers.
 */
export default class PolarManager {
    constructor() {
        // Cache for analyzers and load promises
        this._analyzerCache = new Map();
        this._loadPromises = new Map();
    }

    /**
     * Get the analyzer for a given polarId.
     * Loads and caches the analyzer if not already available.
     * @param {string} polarId
     * @returns {Promise<PolarAnalyzer>}
     */
    async getAnalyzer(polarId) {
        if (!polarId) {
            throw new Error("getAnalyzer requires a polarId");
        }
        // Return cached analyzer if available
        if (this._analyzerCache.has(polarId)) {
            return this._analyzerCache.get(polarId);
        }
        // If loading is already in progress, return the same promise
        if (this._loadPromises.has(polarId)) {
            return this._loadPromises.get(polarId);
        }
        // Otherwise, load and cache
        const loadPromise = new Promise((resolve, reject) => {
            Util.doGET(`/polars/${encodeURIComponent(polarId)}.json`,
                (request) => {
                    try {
                        const data = JSON.parse(request.responseText);
                        if (data) {
                            let analyzer = this._createAnalyzer(data);
                            this._analyzerCache.set(polarId, analyzer);
                            resolve(analyzer);
                        } else {
                            reject(new Error("Invalid polar data format"));
                        }
                    } catch (e) {
                        reject(e);
                    } finally {
                        this._loadPromises.delete(polarId);
                    }
                },
                (request) => {
                    this._loadPromises.delete(polarId);
                    reject(new Error(request.responseText || "Failed to load polars"));
                }
            );
        });
        this._loadPromises.set(polarId, loadPromise);
        return loadPromise;
    }

    /**
     * Internal: Create the appropriate analyzer for the polar format
     * @param {Object} data - Parsed polar data
     * @returns {PolarAnalyzer}
     */
    _createAnalyzer(data) {
        if (data.scriptData) {
            return new VRPolarAnalyzer(data);
        } else if (data.data_json) {
            return new FWPolarAnalyzer(data);
        }
        throw new Error("Unknown polar data format");
    }
}