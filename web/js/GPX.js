////////////////////////////////////////////////////////////////////////////////
/// GPX export - intended for use with GPXsee https://www.gpxsee.org/

`use strict`;

function exportRoute ( routeInfo ) {
    let stream = {"s": ''};
    writeRoute(stream, routeInfo);
    return stream.s;
}

function writeRoute (stream, routeInfo) {
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
