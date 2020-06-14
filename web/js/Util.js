////////////////////////////////////////////////////////////////////////////////
/// Helper functions

function m2nm (dist) {
    return dist / 1852;
}

function ms2knots (ms) {
    return  900.0 * (ms / 463.0);
}

function fromDeg (deg) {
    var sign = deg.u || 1;
    var abs = deg.g + (deg.m / 60.0) + (deg.s / 3600.0) + (deg.cs / 360000.0);
    return sign * abs
}

function toDeg(angle) {
    return angle / Math.PI * 180;
}

function toDMS(number) {
    var u = sign(number);
    number = Math.abs(number);
    var g = Math.floor(number);
    var frac = number - g;
    var m = Math.floor(frac * 60);
    frac = frac - m / 60;
    var s = Math.floor(frac * 3600);
    var cs = roundTo(360000 * (frac - s / 3600), 0);
    while (cs >= 100) {
        cs = cs - 100;
        s = s + 1;
    }
    return {
        "u": u,
        "g": g,
        "m": m,
        "s": s,
        "cs": cs
    };
}

function toDHM (seconds) {
    return {
        "days":  Math.floor(seconds / 86400),
        "hours": Math.floor(seconds/3600) % 24,
        "minutes": Math.floor(seconds/60) % 60,
        "seconds": seconds % 60
    };
}



function roundTo(number, digits) {
    var scale = Math.pow(10, digits);
    return (Math.round(number * scale) / scale);
}

function arcLength(a, b) {
    if ( a < 0 ) a += 360;
    if ( b < 0 ) b += 360;
    if ( b < a ) b += 360;
    return b - a;
}

function sign(x) {
    return (x < 0) ? -1 : 1;
}

function pad0(val) {
    if (val < 10) {
        val = "0" + val;
    }
    return val;
}

function toHHMMSS (timestamp) {
    return new Date(timestamp*1000).toTimeString().substring(0,17);
}

function formatDHM (seconds) {
    var dhm = toDHM(seconds);
    return dhm.days + ":" + pad0(dhm.hours) + ":" + pad0(dhm.minutes) + ":" + pad0(dhm.seconds);
}

function formatPosition(lat, lon) {
    var latDMS = toDMS(lat);
    var lonDMS = toDMS(lon);
    var latString = latDMS.g + "°" + pad0(latDMS.m) + "'" + pad0(latDMS.s) + '"';
    var lonString = lonDMS.g + "°" + pad0(lonDMS.m) + "'" + pad0(lonDMS.s) + '"';
    return latString + ((latDMS.u == 1) ? "N" : "S") + " " + lonString + ((lonDMS.u == 1) ? "E" : "W");
}

export { arcLength, m2nm, ms2knots, toDeg, toDMS, toDHM, toHHMMSS, roundTo, formatPosition, formatDHM };
