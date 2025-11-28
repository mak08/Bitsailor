////////////////////////////////////////////////////////////////////////////////
/// GPX export - intended for use with GPXsee https://www.gpxsee.org/

`use strict`;

import * as Util from './Util.js';

function exportRoute ( routeInfo, format='gpx', sailNames) {
        let stream = {"s": ''};
        if (format=='gpx') {
            writeRouteGPX(stream, routeInfo, sailNames);
        } else if (format=='csv') {
            writeRouteCSV(stream, routeInfo, sailNames);
        } else {
            alert('Unsupported file format');
        }
        return stream.s;
}

////////////////////////////////////////////////////////////////////////////////
/// CSV export

function writeRouteCSV (stream, routeInfo, sailNames) {
    stream.s += 'Time;Position;TWA;Sail;Speed(kn);Heading;TWS(kn);TWD;Penalty;P.Time;Energy;DTF(nm)\n';
    for (const routePoint of routeInfo.best) {
        writePointCSV(stream, routePoint, sailNames);
    }
}

function writePointCSV (stream, routePoint, sailNames) {
    let pos = routePoint.position;
    stream.s += routePoint.time;
    stream.s += ';' + Util.formatPosition(pos.lat, pos.lng);
    stream.s += ';' + routePoint.twa;
    stream.s += ';' + sailNames[routePoint.sail];
    stream.s += ';' + Util.ms2knots(routePoint.speed).toFixed(2);
    stream.s += ';' + routePoint.heading.toFixed(1);
    stream.s += ';' + Util.ms2knots(routePoint.tws).toFixed(2);
    stream.s += ';' + routePoint.twd.toFixed(1);
    stream.s += ';' + Util.m2nm(routePoint.dtf).toFixed(2);
    stream.s += '\n';
}

////////////////////////////////////////////////////////////////////////////////
/// GPX export

function writeRouteGPX (stream, routeInfo, sailNames) {
    stream.s += '<?xml version="1.0" encoding="UTF-8"?>\n';
    stream.s += '<gpx version="1.0"\n';
    stream.s += '     creator="BitSailor"\n';
    stream.s += '     xmlns="http://www.topografix.com/GPX/1/1"\n';
    stream.s += '     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n';
    stream.s += '     xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"\n';
    stream.s += '     xmlns:gpxsrx="http://bitsailor.net/xmlschemas/GpxSailRoute/v1">\n';
    writePathAsRoute(stream, routeInfo);
    // writePathAsTrack(stream, routeInfo);
    stream.s += '</gpx>';
}

function writePathAsTrack (stream, routeInfo) {
    stream.s += '  <trk>\n'; 
    stream.s += '    <trkseg>\n'; 
    for (const routePoint of routeInfo.best) {
        writePoint(stream, routePoint, 'trkpt');
    }
    stream.s += '    </trkseg>\n';
    stream.s += '  </trk>\n';
}

function writePathAsRoute (stream, routeInfo) {
    stream.s += '  <rte>\n'; 
    for (const routePoint of routeInfo.best) {
        writePoint(stream, routePoint, 'rtept');
    }
    stream.s += '  </rte>\n';
}

function writePoint (stream, routePoint, type='trkpt') {
    stream.s += `      <${type}\n`;
    stream.s += `        lat="${routePoint.position.lat}"\n`;
    stream.s += `        lon="${routePoint.position.lng}">\n`;
    stream.s += `        <sym>Waypoint</sym>\n`;
    stream.s += `        <time>${routePoint.time}</time>\n`;
    stream.s += `        <extensions>\n`;
    stream.s += `          <gpxsrx:twa>${routePoint.twa}</gpxsrx:twa>\n`;
    stream.s += `          <gpxsrx:sail>${routePoint.sail}</gpxsrx:sail>\n`;
    stream.s += `          <gpxsrx:tws>${routePoint.tws}</gpxsrx:tws>\n`;
    stream.s += `          <gpxsrx:twd>${routePoint.twd}</gpxsrx:twd>\n`;
    stream.s += `        </extensions>\n`;
    stream.s += `        <name>TWA:${routePoint.twa} Sail:${routePoint.sail}</name>\n`;
    stream.s += `      </${type}>\n`;
}

export {
    exportRoute,
    exportSchedule
}

////////////////////////////////////////////////////////////////////////////////
/// Schedule CSV export (timestamp,TWA,<twa> only, on TWA changes)

function exportSchedule(routeInfo) {
    const stream = { s: '' };
    if (!routeInfo || !Array.isArray(routeInfo.best) || routeInfo.best.length === 0) {
        return stream.s;
    }
    let lastTWA = undefined;
    for (const rp of routeInfo.best) {
        const curTWA = rp.twa;
        if (lastTWA === undefined || curTWA !== lastTWA) {
            let fwTWA = - curTWA;
            stream.s += formatTimestampCSV(rp.time) + ',TWA,' + fwTWA + '\n';
            lastTWA = curTWA;
        }
    }
    return stream.s;
}

function formatTimestampCSV(isoOrDate) {
    const d = (isoOrDate instanceof Date) ? isoOrDate : new Date(isoOrDate);
    const yyyy = d.getUTCFullYear();
    const mm = String(d.getUTCMonth() + 1).padStart(2, '0');
    const dd = String(d.getUTCDate()).padStart(2, '0');
    const hh = String(d.getUTCHours()).padStart(2, '0');
    const min = String(d.getUTCMinutes()).padStart(2, '0');
    const ss = String(d.getUTCSeconds()).padStart(2, '0');
    return `${yyyy}-${mm}-${dd} ${hh}:${min}:${ss}`;
}
