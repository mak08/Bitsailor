// Run with node tests/route-info-panel-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
let active;
class Element {
    constructor() {this.attrs = {}; this.events = {}; this.children = []; this.classes = new Set();
        this.classList = {toggle: (name, on) => on ? this.classes.add(name) : this.classes.delete(name)};}
    setAttribute(name, value) {this.attrs[name] = value;}
    getAttribute(name) {return this.attrs[name];}
    hasAttribute(name) {return name in this.attrs;}
    addEventListener(name, action) {this.events[name] = action;}
    contains(node) {return this === node || this.children.includes(node);}
    focus(options) {
        this.lastFocusOptions = options;
        if (this.offscreen && !options?.preventScroll) root.scrollLeft = 340;
        active = this;
    }
}
const root = new Element(); root.clientWidth = 1280;
root.scrollLeft = 0;
let resized;
root.ownerDocument = {defaultView: {addEventListener(name, handler) {
    assert.equal(name, 'resize'); resized = handler;
}}};
function host(key, side, title, defaultOpen = false) {
    const node = new Element(), panel = new Element(), toggle = new Element(), close = new Element();
    node.dataset = {panel: key, side}; node.parentElement = root; panel.parentElement = node;
    panel.children.push(close);
    node.querySelector = selector => ({'.sliding-panel': panel, '.drawer-tab': toggle, '.drawer-close': close,
        h2: {textContent: title}})[selector];
    if (defaultOpen) node.setAttribute('data-open-default', '');
    return {node, panel, toggle, close};
}
const routing = host('routing', 'left', 'Routing', true), races = host('races', 'left', 'Races / NMEA');
const info = host('route-info', 'right', 'Route Info'), settings = host('settings', 'right', 'Settings');
info.close.offscreen = true; // The close button starts beyond the viewport while sliding in.
root.querySelectorAll = () => [routing, races, info, settings].map(x => x.node);
const context = vm.createContext({Map, document: {get activeElement() {return active;}}});
vm.runInContext(fs.readFileSync('web/js/SlidingPanels.js', 'utf8')
    .replace('export class ', 'class ').replace('export default ', '')
    + '\nglobalThis.setup = setupSlidingPanels;', context);
const panels = context.setup(root), drawer = panels.get('route-info');
assert.equal(routing.panel.inert, false); assert.equal(info.panel.inert, true);
info.toggle.focus(); info.toggle.events.click();
assert.equal(active, info.close); assert.equal(info.panel.inert, false);
assert.equal(info.close.lastFocusOptions.preventScroll, true);
assert.equal(root.scrollLeft, 0);
assert.equal(info.toggle.getAttribute('aria-expanded'), 'true');
assert.ok(root.classes.has('left-drawer-open')); assert.ok(root.classes.has('right-drawer-open'));
root.clientWidth = 600; resized();
assert.equal(routing.panel.inert, true); assert.equal(info.panel.inert, false);
root.clientWidth = 1280; panels.get('routing').open();
let prevented = false;
info.panel.events.keydown({key: 'Escape', preventDefault() {prevented = true;}, stopPropagation() {}});
assert.ok(prevented); assert.equal(active, info.toggle); assert.equal(info.panel.inert, true);
assert.equal(info.toggle.lastFocusOptions.preventScroll, true);
active = new Element(); const previousFocus = active;
drawer.open(); assert.equal(active, previousFocus); // Completion does not steal map focus.
settings.toggle.events.click(); assert.equal(info.panel.inert, true); assert.equal(settings.panel.inert, false);
info.toggle.events.click(); assert.equal(settings.panel.inert, true);
info.close.events.click(); assert.equal(active, info.toggle);
panels.get('races').open(); assert.equal(routing.panel.inert, true); assert.equal(races.panel.inert, false);
root.clientWidth = 600;
drawer.open(); assert.equal(races.panel.inert, true); assert.equal(root.classes.has('left-drawer-open'), false);
assert.equal(info.panel.inert, false);

const html = fs.readFileSync('web/router-fw.html', 'utf8');
function content(id) {
    const start = html.indexOf(`<aside id="${id}"`);
    assert.ok(start >= 0, `Missing panel ${id}`);
    return html.slice(start, html.indexOf('</aside>', start));
}
const placement = {
    'route-info-panel': ['lb_from', 'lb_duration', 'lb_minwind', 'lb_mintwa', 'lb_polars', 'lb_fine_tuning',
                         'bt_downloadroute', 'bt_downloadschedule', 'rb_gpx', 'rb_csv', 'reverse_status'],
    'routing-panel': ['cb_reverse', 'reverse_percent', 'bt_getroute', 'pg_getroute', 'sel_polars', 'tb_position',
                      'bt_position', 'cb_startdelayed', 'tb_starttime', 'sel_duration', 'cb_hires_stepper',
                      'sel_forecast_model', 'sel_secondary_forecast_model', 'sel_currents', 'cb_waves'],
    'races-panel': ['sel_race', 'bt_setcourse', 'bt_removerace', 'tb_nmeahost', 'tb_nmeaport', 'bt_nmeaupdate'],
    'settings-panel': ['ir_wind_opacity', 'wind_opacity_value', 'cb_displaycurrents', 'cb_displaytracks', 'cb_muted_map'],
    'twa-panel': ['lb_twa', 'lb_twa_heading'],
    'depth-panel': ['depth-draft', 'depth-margin']
};
for (const [panel, ids] of Object.entries(placement)) for (const id of ids) {
    assert.ok(content(panel).includes(`id="${id}"`), `${id} must live in ${panel}`);
    assert.equal(html.split(`id="${id}"`).length - 1, 1, `${id} must remain unique`);
}
assert.equal(html.includes('id="controls"'), false);
const ordered = ['bt_getroute', 'pg_getroute', 'sel_polars', 'tb_position', 'tb_starttime', 'cb_reverse',
    'sel_duration', 'sel_forecast_model', 'sel_secondary_forecast_model', 'sel_currents', 'cb_waves'];
const positions = ordered.map(id => content('routing-panel').indexOf(`id="${id}"`));
assert.deepEqual(positions, [...positions].sort((a, b) => a - b));

// Exercise the real success/error callbacks, with route drawing stubbed out.
const source = fs.readFileSync('web/js/router-main.js', 'utf8');
function body(name) {
    const start = source.indexOf(`function ${name}(`);
    return source.slice(start, source.indexOf('\n}', start) + 2);
}
let success, failure;
const order = [], elements = new Map();
const routeContext = vm.createContext({
    waypointMarkers: [], settings: {}, startMarker: {getLatLng: () => ({lat: 1, lng: 2})},
    destinationMarker: {getLatLng: () => ({lat: 3, lng: 4})},
    document: {getElementById(id) {
        if (!elements.has(id)) elements.set(id, {value: '24', style: {}, max: 300});
        return elements.get(id);
    }},
    setBusyCursor() {}, restoreCursor() {}, getStartTime: () => '2026-11-17T12:00',
    wrapLon180: x => x, updateGetRouteProgress() {}, makeQuery: () => '?duration=86400',
    window: {setInterval() {}, clearInterval() {}}, setModelHeading() {}, clearRoute() {},
    displayRouting() {order.push('render');}, routeInfoPanel: {open() {order.push('open');}},
    alert() {}, Util: {doGETJSON(_url, onSuccess, onError) {success = onSuccess; failure = onError;},
                      requestErrorText: () => 'failure'}
});
vm.runInContext(body('validateRouteResponse') + body('getRoute'), routeContext);
routeContext.getRoute();
success({best: [{}], isochrones: []});
assert.deepEqual(order, ['render', 'open']);
order.length = 0;
routeContext.getRoute(); failure({responseURL: '/route'}, new Error());
assert.deepEqual(order, []);
assert.throws(() => success({best: [], isochrones: []}), /Invalid route response/);
assert.deepEqual(order, []);
console.log('Route drawer controls, focus, field placement and completion/error behaviour passed.');
