////////////////////////////////////////////////////////////////////////////////
/// Standard Polar Analyzer Implementation
/// Handles the current format used by BitSailor

import * as Util from './Util.js';
import PolarAnalyzer from './PolarAnalyzer.js';

export default class StandardPolarAnalyzer extends PolarAnalyzer {
    constructor(polarData, sailNames) {
        super(polarData, sailNames);
    }

    /**
     * Calculate Velocity Made Good for a given wind speed
     * @param {number} windSpeed - Wind speed in knots
     * @param {Array} options - Options affecting boat performance
     * @returns {Object} - VMG values for upwind and downwind
     */
    getVMG(windSpeed, options = ["hull", "winch", "foil", "heavy", "light", "reach"]) {
        const polarsData = this.polarData?.data_json;
        if (!polarsData) {
            console.warn('No polar data available');
            return null;
        }

        const vmg = this.bestVMG(windSpeed, options);
        return {
            "up": vmg.vmgUp.toFixed(2) + '@' + vmg.twaUp.toFixed(0),
            "down": Math.abs(vmg.vmgDown).toFixed(2) + '@' + vmg.twaDown.toFixed(0)
        };
    }

    /**
     * Calculate best VMG angles and speeds
     * @param {number} tws - True wind speed
     * @param {Array} options - Options affecting boat performance
     * @returns {Object} - Best VMG angles and speeds for upwind and downwind
     */
    bestVMG(tws, options = ["hull", "winch", "foil", "heavy", "light", "reach"]) {
        const polarsData = this.polarData?.data_json;
        if (!polarsData) {
            return {"vmgUp": 0, "twaUp": 0, "vmgDown": 0, "twaDown": 0};
        }
        
        const best = {"vmgUp": 0, "twaUp": 0, "vmgDown": 0, "twaDown": 0};
        const twaSteps = polarsData.twa;
        
        for (let twa = twaSteps[0]; twa < twaSteps[twaSteps.length-1]; twa++) {
            const speed = this.boatSpeed(tws, twa, options).speed;
            const vmg = speed * Math.cos(twa / 180 * Math.PI);
            if (vmg > best.vmgUp) {
                best.twaUp = twa;
                best.vmgUp = vmg;
            } else if (vmg < best.vmgDown) {
                best.twaDown = twa;
                best.vmgDown = vmg;
            }
        }
        return best;
    }

    /**
     * Calculate boat speed for given conditions
     * @param {number} tws - True wind speed
     * @param {number} twa - True wind angle
     * @param {Array} options - Options affecting boat performance
     * @returns {Object} - Speed and sail information
     */
    boatSpeed(tws, twa, options = ["hull", "winch", "foil", "heavy", "light", "reach"]) {
        const polarsData = this.polarData?.data_json;
        if (!polarsData) {
            return { "speed": 0, "sail": this.sailNames[0] };
        }
        
        const foil = this.foilingFactor(options, tws, twa, polarsData.foil);
        const hull = options.includes("hull") ? 1.003 : 1.0;
        const ratio = polarsData.globalSpeedRatio;
        const twsLookup = this.fractionStep(tws, polarsData.tws);
        const twaLookup = this.fractionStep(twa, polarsData.twa);
        const speed = this.maxSpeed(options, twsLookup, twaLookup, polarsData.sail);
        
        return {
            "speed": Util.roundTo(speed.speed * foil * hull * ratio, 2),
            "sail": this.sailNames[speed.sail]
        };
    }

    // Helper methods moved from original PolarManager
    maxSpeed(options, iS, iA, sailDefs) {
        let maxSpeed = 0;
        let maxSail = "";
        for (const sailDef of sailDefs) {
            if (sailDef.id === 1
                || sailDef.id === 2
                || (sailDef.id === 3 && options.includes("heavy"))
                || (sailDef.id === 4 && options.includes("light"))
                || (sailDef.id === 5 && options.includes("reach"))
                || (sailDef.id === 6 && options.includes("heavy"))
                || (sailDef.id === 7 && options.includes("light"))) {
                const speed = this.pSpeed(iA, iS, sailDef.speed);
                if (speed > maxSpeed) {
                    maxSpeed = speed;
                    maxSail = sailDef.id;
                }
            }
        }
        return {
            speed: maxSpeed,
            sail: maxSail
        };
    }

    pSpeed(iA, iS, speeds) {
        return this.bilinear(
            iA.fraction, 
            iS.fraction,
            speeds[iA.index - 1][iS.index - 1],
            speeds[iA.index][iS.index - 1],
            speeds[iA.index - 1][iS.index],
            speeds[iA.index][iS.index]
        );
    }

    foilingFactor(options, tws, twa, foil) {
        if (!options.includes("foil")) {
            return 1.0;
        }
        
        const speedSteps = [0, foil.twsMin - foil.twsMerge, foil.twsMin, foil.twsMax, foil.twsMax + foil.twsMerge, Infinity];
        const twaSteps = [0, foil.twaMin - foil.twaMerge, foil.twaMin, foil.twaMax, foil.twaMax + foil.twaMerge, Infinity];
        const foilMat = [
            [1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1],
            [1, 1, foil.speedRatio, foil.speedRatio, 1, 1],
            [1, 1, foil.speedRatio, foil.speedRatio, 1, 1],
            [1, 1, 1, 1, 1, 1],
            [1, 1, 1, 1, 1, 1]
        ];
    
        const iS = this.fractionStep(tws, speedSteps);
        const iA = this.fractionStep(twa, twaSteps);
        
        return this.bilinear(
            iA.fraction, 
            iS.fraction,
            foilMat[iA.index - 1][iS.index - 1],
            foilMat[iA.index][iS.index - 1],
            foilMat[iA.index - 1][iS.index],
            foilMat[iA.index][iS.index]
        );
    }

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