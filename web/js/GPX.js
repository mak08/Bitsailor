////////////////////////////////////////////////////////////////////////////////
/// GPX export - intended for use with GPXsee https://www.gpxsee.org/

`use strict`;

import * as Util from './Util.js';
import * as Router from './router.js';

function exportRoute ( routeInfo, format='gpx' ) {
        let stream = {"s": ''};
        if (format=='gpx') {
            writeRouteGPX(stream, routeInfo);
        } else if (format=='csv') {
            writeRouteCSV(stream, routeInfo);
        } else {
            alert('Unsupported file format');
        }
        return stream.s;
}

////////////////////////////////////////////////////////////////////////////////
/// CSV export

function writeRouteCSV (stream, routeInfo) {
    stream.s += 'Time;Position;TWA;Sail;Speed(kn);Heading;TWS(kn);TWD;Penalty;P.Time;Energy;DTF(nm)\n';
    for (const routePoint of routeInfo.best) {
        writePointCSV(stream, routePoint);
    }
}

function writePointCSV (stream, routePoint) {
    let pos = routePoint.position;
    stream.s += routePoint.time;
    stream.s += ';' + Util.formatPosition(pos.lat, pos.lng);
    stream.s += ';' + routePoint.twa;
    stream.s += ';' + Router.getSailnames()[routePoint.sail];
    stream.s += ';' + Util.ms2knots(routePoint.speed).toFixed(2);
    stream.s += ';' + routePoint.heading.toFixed(1);
    stream.s += ';' + Util.ms2knots(routePoint.tws).toFixed(2);
    stream.s += ';' + routePoint.twd.toFixed(1);
    stream.s += ';' + routePoint.penalty;
    stream.s += ';' + routePoint.ptime.toFixed();
    stream.s += ';' + routePoint.energy.toFixed();
    stream.s += ';' + Util.m2nm(routePoint.dtf).toFixed(2);
    stream.s += '\n';
}

////////////////////////////////////////////////////////////////////////////////
/// GPX export

function writeRouteGPX (stream, routeInfo) {
    stream.s += '<?xml version="1.0" encoding="UTF-8"?>\n';
    stream.s += '<gpx version="1.0"\n';
    stream.s += '     creator="BitSailor"\n';
    stream.s += '     xmlns="http://www.topografix.com/GPX/1/1"\n';
    stream.s += '     xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n';
    stream.s += '     xsi:schemaLocation="http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"\n';
    stream.s += '     xmlns:gpxsrx="http://bitsailor.net/xmlschemas/GpxSailRoute/v1">\n';
    writePathAsRoute(stream, routeInfo);
    writePathAsTrack(stream, routeInfo);
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
    exportRoute
}
