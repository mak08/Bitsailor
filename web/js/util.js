////////////////////////////////////////////////////////////////////////////////
/// Utils

function formatPosition (lat, lon) {
        var latDMS = toDMS(lat);
        var lonDMS = toDMS(lon);
        var latString = pad0(latDMS.g) + "°" + pad0(latDMS.m) + "'" + pad0(latDMS.s) + '"';
        var lonString = pad0(lonDMS.g) + "°" + pad0(lonDMS.m) + "'" + pad0(lonDMS.s) + '"';
        return  latString + ((latDMS.u==1)?'N':'S') + ' ' + lonString + ((lonDMS.u==1)?'E':'W');
    }

    function toDMS (number) {
        var u = sign(number);
        number = Math.abs(number);
        var g = Math.floor(number);
        var frac = number - g;
        var m = Math.floor(frac * 60);
        frac = frac - m/60;
        var s = Math.floor(frac * 3600);
        var cs = roundTo(360000 * (frac - s/3600), 0);
        while ( cs >= 100 ) {
            cs = cs - 100;
            s = s + 1;
        }
        return {"u":u, "g":g, "m":m, "s":s, "cs":cs};
    }

    function roundTo (number, digits) {
        if (number !== undefined && !isNaN(number)) {
            var scale = Math.pow(10, digits);
            return Math.round(number * scale) / scale;
        } else {
            return '-';
        }
    }

    function sign (x) {
        return (x < 0)? -1: 1;
    }

    function pad0(val) {
        if (val < 10) {
            val = "0" + val;
        }
        return val;
    }

/// EOF
////////////////////////////////////////////////////////////////////////////////
