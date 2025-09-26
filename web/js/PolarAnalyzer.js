////////////////////////////////////////////////////////////////////////////////
/// Base class for analyzing polar data
/// Different implementations can handle various polar formats

import * as Util from './Util.js';

/**
 * Abstract base class for polar data analysis
 */
export default class PolarAnalyzer {
    /**
     * Constructor
     * @param {Object} polarData - The polar data to analyze
     * @param {Array} sailNames - Names of sails in the polar data
     */
    constructor(polarData, sailNames = ["Jib", "Spi", "Stay", "LJ", "C0", "HG", "LG"]) {
        if (new.target === PolarAnalyzer) {
            throw new TypeError("Cannot instantiate abstract PolarAnalyzer directly");
        }
        this.polarData = polarData;
        this.sailNames = sailNames;
    }

    /**
     * Calculate Velocity Made Good for a given wind speed
     * @param {number} windSpeed - Wind speed in knots
     * @param {Array} options - Options affecting boat performance
     */
    getVMG(windSpeed, options) {
        throw new Error("Method 'getVMG' must be implemented by subclass");
    }
    
    /**
     * Calculate boat speed for given conditions
     * @param {number} tws - True wind speed
     * @param {number} twa - True wind angle
     * @param {Array} options - Options affecting boat performance
     */
    boatSpeed(tws, twa, options) {
        throw new Error("Method 'boatSpeed' must be implemented by subclass");
    }
    
    /**
     * Calculate best VMG angles and speeds
     * @param {number} tws - True wind speed
     * @param {Array} options - Options affecting boat performance
     */
    bestVMG(tws, options) {
        throw new Error("Method 'bestVMG' must be implemented by subclass");
    }
    
    /**
     * Get sail names array
     * @returns {Array} - Sail names
     */
    getSailNames() {
        return this.sailNames;
    }

    /**
     * Set sail names array
     * @param {Array} sailNames - New sail names array
     */
    setSailNames(sailNames) {
        this.sailNames = sailNames;
    }
}