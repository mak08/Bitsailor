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
    stream.s += 'Time;Position;TWA;Sail;Speed(kn);Heading;TWS(kn);TWD;Penalty;P.Time;Energy;DTF(nm);Control;Command\n';
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
    const command = steeringControl(routePoint);
    stream.s += ';' + command.mode + ';' + command.value;
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
    const command = steeringControl(routePoint);
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
    stream.s += `          <gpxsrx:controlMode>${command.mode}</gpxsrx:controlMode>\n`;
    stream.s += `          <gpxsrx:controlValue>${command.value}</gpxsrx:controlValue>\n`;
    stream.s += `        </extensions>\n`;
    stream.s += `        <name>${command.mode}:${command.value} Sail:${routePoint.sail}</name>\n`;
    stream.s += `      </${type}>\n`;
}

export {
    exportRoute,
    exportSchedule,
    steeringControl
}

////////////////////////////////////////////////////////////////////////////////
/// Schedule CSV export: distinguish fixed compass HDG from signed TWA commands.

function steeringControl(point) {
    const heading = point['control-mode'] === 'heading';
    return { mode: heading ? 'HDG' : 'TWA',
        // Lisp NIL is serialized as false, so only a numeric value is a command.
        value: Number.isFinite(point['control-value']) ? point['control-value']
            : (heading ? point.heading : point.twa) };
}

function exportSchedule(routeInfo) {
    const stream = { s: '' };
    if (!routeInfo || !Array.isArray(routeInfo.best) || routeInfo.best.length === 0) {
        return stream.s;
    }
    let previous;
    const plan = routeInfo['steering-segments'];
    if (Array.isArray(plan) && plan.length) {
        let time = new Date(plan[0]['start-time']).getTime();
        for (const segment of plan) {
            const command = { mode: segment.mode === 'heading' ? 'HDG' : 'TWA', value: segment.value };
            if (!previous || previous.mode !== command.mode || previous.value !== command.value) {
                stream.s += formatTimestampCSV(new Date(time)) + ',' + command.mode + ','
                    + (command.mode === 'TWA' ? -command.value : command.value) + '\n';
            }
            previous = command;
            time += Number(segment.duration) * 1000;
        }
    } else {
        for (const rp of routeInfo.best.slice(0, -1)) {
            const command = steeringControl(rp);
            if (!previous || previous.mode !== command.mode || previous.value !== command.value) {
                stream.s += formatTimestampCSV(rp.time) + ',' + command.mode + ','
                    + (command.mode === 'TWA' ? -command.value : command.value) + '\n';
            }
            previous = command;
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
