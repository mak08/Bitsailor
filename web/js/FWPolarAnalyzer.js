////////////////////////////////////////////////////////////////////////////////
/// Standard Polar Analyzer Implementation for BitSailor "FW" format

import * as Util from './Util.js';
import PolarAnalyzer from './PolarAnalyzer.js';

export default class FWPolarAnalyzer extends PolarAnalyzer {
    constructor(polarData, sailNames) {
        super(polarData, sailNames);
    }

    /**
     * Calculate Velocity Made Good for a given wind speed
     * @param {number} windSpeed - Wind speed in knots
     * @param {Array} options - (ignored for FW format)
     * @returns {Object} - VMG values for upwind and downwind
     */
    getVMG(windSpeed, options = []) {
        const polarsData = this.polarData.data_json;
        if (!polarsData || !polarsData.table) {
            console.warn('No polar data available');
            return null;
        }

        const vmg = this.bestVMG(windSpeed);
        return {
            "up": vmg.vmgUp.toFixed(2) + '@' + vmg.twaUp.toFixed(0),
            "down": Math.abs(vmg.vmgDown).toFixed(2) + '@' + vmg.twaDown.toFixed(0)
        };
    }

    /**
     * Calculate best VMG angles and speeds
     * @param {number} tws - True wind speed
     * @returns {Object} - Best VMG angles and speeds for upwind and downwind
     */
    bestVMG(tws) {
        const polarsData = this.polarData.data_json;
        if (!polarsData || !polarsData.table) {
            return {"vmgUp": 0, "twaUp": 0, "vmgDown": 0, "twaDown": 0};
        }

        const twaSteps = polarsData.twa;
        let best = {"vmgUp": -Infinity, "twaUp": 0, "vmgDown": Infinity, "twaDown": 0};

        for (let i = 0; i < twaSteps.length; i++) {
            const twa = twaSteps[i];
            const speed = this.boatSpeed(tws, twa).speed;
            const vmg = speed * Math.cos(twa * Math.PI / 180);
            if (vmg > best.vmgUp) {
                best.vmgUp = vmg;
                best.twaUp = twa;
            }
            if (vmg < best.vmgDown) {
                best.vmgDown = vmg;
                best.twaDown = twa;
            }
        }
        return best;
    }

    /**
     * Calculate boat speed for given conditions
     * @param {number} tws - True wind speed
     * @param {number} twa - True wind angle
     * @param {Array} options - (ignored for FW format)
     * @returns {Object} - Speed and sail information
     */
    boatSpeed(tws, twa, options = []) {
        const polarsData = this.polarData.data_json;
        if (!polarsData || !polarsData.table) {
            return { "speed": 0, "sail": this.sailNames ? this.sailNames[0] : "" };
        }

        // Find indices and fractions for interpolation
        const twsIdx = this.fractionStep(tws, polarsData.tws);
        const twaIdx = this.fractionStep(twa, polarsData.twa);

        // Bilinear interpolation on [twa][tws]
        const table = polarsData.table;
        const s00 = table[twaIdx.index - 1][twsIdx.index - 1];
        const s10 = table[twaIdx.index][twsIdx.index - 1];
        const s01 = table[twaIdx.index - 1][twsIdx.index];
        const s11 = table[twaIdx.index][twsIdx.index];

        const speed = this.bilinear(
            twaIdx.fraction,
            twsIdx.fraction,
            s00, s10, s01, s11
        );

        return {
            "speed": Util.roundTo(speed, 2),
            "sail": this.sailNames ? this.sailNames[0] : ""
        };
    }

    // --- Helper methods ---

    bilinear(x, y, f00, f10, f01, f11) {
        return f00 * (1 - x) * (1 - y)
            + f10 * x * (1 - y)
            + f01 * (1 - x) * y
            + f11 * x * y;
    }

    fractionStep(value, steps) {
        const absVal = Math.abs(value);
        let index = 0;
        while (index < steps.length && steps[index] <= absVal) {
            index++;
        }
        if (index < steps.length) {
            return {
                index: index,
                fraction: (absVal - steps[index - 1]) / (steps[index] - steps[index - 1])
            };
        } else {
            return {
                index: index - 1,
                fraction: 1.0
            };
        }
    }
}