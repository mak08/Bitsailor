// Export real steering commands, not sampled TWA changes under constant compass heading.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('web/js/GPX.js', 'utf8')
  .replace(/^import .*;\n/gm, '').replace(/export\s*\{[^}]+\}/m, '');
const ctx = vm.createContext({ Date });
vm.runInContext(source, ctx);
const time = '2026-11-04T12:00:00Z';
const best = [0, 120, 240, 360].map((s, i) => ({
  time: new Date(Date.parse(time) + s * 1000).toISOString(), twa: 105 + i,
  'control-mode': 'heading', 'control-value': 110,
  position: { lat: 40, lng: 2 }, sail: 0, tws: 10, twd: 5
}));
assert.equal(ctx.exportSchedule({ best }), '2026-11-04 12:00:00,HDG,110\n');
assert.equal(ctx.exportSchedule({ best, 'steering-segments': [
  { mode: 'heading', value: 110, 'start-time': time, duration: 120.25 },
  { mode: 'twa', value: -105, 'start-time': 'ignored', duration: 59.75 },
  { mode: 'heading', value: 111, 'start-time': 'ignored', duration: 60 }
] }), '2026-11-04 12:00:00,HDG,110\n2026-11-04 12:02:00,TWA,105\n2026-11-04 12:03:00,HDG,111\n');
// Preserve legacy TWA polarity; a terminal sample has no outgoing command.
assert.equal(ctx.exportSchedule({ best: [
  { time, twa: 105 }, { time: '2026-11-04T12:02:00Z', twa: 110 },
  { time: '2026-11-04T12:04:00Z', twa: 999 }
] }), '2026-11-04 12:00:00,TWA,-105\n2026-11-04 12:02:00,TWA,-110\n');
const stream = { s: '' };
ctx.writePoint(stream, best[0], 'rtept');
assert.match(stream.s, /<gpxsrx:controlMode>HDG<\/gpxsrx:controlMode>/);
assert.match(stream.s, /<gpxsrx:controlValue>110<\/gpxsrx:controlValue>/);
assert.match(stream.s, /<name>HDG:110/);
assert.equal((stream.s.match(/<extensions>/g) || []).length, 1);
console.log('Mixed HDG/TWA schedule, polarity, terminal-point and GPX command export tests passed.');

// Search trackpoints have Lisp NIL/false metadata when fine-tuning retains them.
const raw = best.map((p, i) => ({ ...p, twa: 105 + i * 5,
  'control-mode': false, 'control-value': false }));
assert.equal(ctx.exportSchedule({ best: raw }),
  '2026-11-04 12:00:00,TWA,-105\n2026-11-04 12:02:00,TWA,-110\n2026-11-04 12:04:00,TWA,-115\n');
const rawStream = { s: '' };
ctx.writePoint(rawStream, raw[0], 'rtept');
assert.match(rawStream.s, /<gpxsrx:controlValue>105<\/gpxsrx:controlValue>/);
assert.equal(ctx.steeringControl({ twa: 5, 'control-mode': 'twa', 'control-value': 0 }).value, 0);
console.log('Unoptimized Lisp false metadata and zero-degree command export tests passed.');
