////////////////////////////////////////////////////////////////////////////////
/// Polar Data Management for BitSailor
/// Handles loading and format detection for polar data

import * as Util from './Util.js';
import StandardPolarAnalyzer from './StandardPolarAnalyzer.js';

/**
 * PolarManager class for loading and managing boat performance data
 */
export default class PolarManager {
    /**
     * Constructor - loads polar data immediately
     * @param {string} polarId - Required ID/name of polar file to load
     */
    constructor(polarId) {
        if (!polarId) {
            throw new Error("PolarManager requires a polar ID");
        }
        
        this.polars = null;
        this.analyzer = null;
        this.formatParsers = {};
        this.ready = false;
        
        // Register default parser
        this.registerFormat('standard', (data) => data);
        
        // Start loading the polars (returns promise)
        this.initialLoadPromise = this.loadPolars(polarId);
    }

    /**
     * Wait for the polar data to be loaded and ready
     * @returns {Promise} - Promise that resolves when data is ready
     */
    whenReady() {
        return this.initialLoadPromise;
    }

    /**
     * Load polar data from a URL
     * @param {string} id - Polar ID/name
     * @returns {Promise} - Promise that resolves with the loaded polar data
     */
    loadPolars(id) {
        return new Promise((resolve, reject) => {
            Util.doGET(`/polars/${encodeURIComponent(id)}.json`,
                (request) => {
                    try {
                        const data = JSON.parse(request.responseText);
                        if (data) {
                            console.log('Loaded polars: ' + id);
                            this.polars = data;
                            
                            // Create appropriate analyzer based on format
                            const format = this.detectFormat(data);
                            const parsedData = this.parsePolars(data, format);
                            this.analyzer = this.createAnalyzer(format, parsedData);
                            
                            this.ready = true;
                            resolve(this.analyzer);
                        } else {
                            reject(new Error("Invalid polar data format"));
                        }
                    } catch (e) {
                        reject(e);
                    }
                },
                (request) => {
                    const error = new Error(request.responseText || "Failed to load polars");
                    reject(error);
                });
        });
    }

    /**
     * Create the appropriate analyzer for the polar format
     * @param {string} format - Format name
     * @param {Object} data - Parsed polar data
     * @returns {PolarAnalyzer} The created analyzer
     */
    createAnalyzer(format, data) {
        // Currently only supporting standard format
        // In the future, we could have VRPolarAnalyzer, OrcPolarAnalyzer, etc.
        return new StandardPolarAnalyzer(data);
    }
    
    /**
     * Get the currently loaded polar data
     * @returns {Object} - Current polar data
     */
    getPolars() {
        return this.polars;
    }

    /**
     * Get the current analyzer
     * @returns {PolarAnalyzer} - Current analyzer
     * @throws {Error} - If no analyzer is available yet
     */
    getAnalyzer() {
        if (!this.ready || !this.analyzer) {
            throw new Error("Polar data not yet loaded. Use whenReady() before accessing the analyzer.");
        }
        return this.analyzer;
    }
    
    /**
     * Register a new polar format parser
     * @param {string} formatName - Name of the format
     * @param {function} parserFunction - Function to parse this format
     */
    registerFormat(formatName, parserFunction) {
        this.formatParsers[formatName] = parserFunction;
    }
    
    /**
     * Parse polar data in a specific format
     * @param {Object} data - Raw polar data
     * @param {string} format - Format name (optional, auto-detected if missing)
     * @returns {Object} - Normalized polar data
     */
    parsePolars(data, format) {
        // Auto-detect format if not specified
        if (!format) {
            format = this.detectFormat(data);
        }
        
        // Use registered parser or default
        if (this.formatParsers && this.formatParsers[format]) {
            return this.formatParsers[format](data);
        } else {
            // Default parser assumes data is already in correct format
            return data;
        }
    }
    
    /**
     * Attempt to detect the format of polar data
     * @param {Object} data - Raw polar data
     * @returns {string} - Detected format name
     */
    detectFormat(data) {
        // Simple format detection logic
        if (data.scriptData && data.scriptData.polar) {
            return 'vr';
        } else if (data.data_json) {
            return 'standard';
        } else {
            return 'unknown';
        }
    }
}