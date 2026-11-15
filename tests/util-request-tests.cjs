const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('web/js/Util.js', 'utf8').replace(/export\s*\{[\s\S]*?\};/, '');
const requests = [];
class XHR {
    constructor() { requests.push(this); this.status = 200; this.responseText = '{}'; }
    open(method, url) { this.url = url; this.opened = true; }
    get responseType() { return this.type; }
    set responseType(value) { assert(this.opened); this.type = value; }
    send() {}
}
const ctx = vm.createContext({XMLHttpRequest: XHR, TextEncoder, TextDecoder, console: {error() {}, warn() {}}});
vm.runInContext(source, ctx);
let ok = 0, failed = 0;
const encoded = new TextEncoder().encode('{"label":"São Miguel"}');
assert.equal(ctx.parseJSONResponse({
    responseType: 'arraybuffer', response: encoded.buffer,
    get responseText() { throw new Error('responseText is unavailable in binary mode'); },
    getResponseHeader: key => key === 'X-Route-Bytes' ? String(encoded.byteLength) : null
}).label, 'São Miguel');
assert.throws(() => ctx.parseJSONResponse({
    responseType: 'arraybuffer', response: encoded.buffer.slice(0, 10),
    getResponseHeader: key => key === 'X-Route-Bytes' ? String(encoded.byteLength) : null
}), /Incomplete route response/);
assert.match(ctx.requestErrorText({
    responseType: 'arraybuffer', response: new TextEncoder().encode('Server error').buffer,
    status: 500, statusText: 'HTTP 500'
}), /Server error/);
const success = () => { ok++; };
const failure = () => { failed++; };
let xhr = ctx.doGET('/route?x=1', success, failure, {name: 'a & b'}, {responseType: 'text'});
assert.equal(xhr.url, '/route?x=1&name=a%20%26%20b');
xhr.onload(); xhr.onerror();
assert.equal(ok, 1); assert.equal(failed, 0);
for (const event of ['onerror', 'ontimeout', 'onabort']) {
    xhr = ctx.doGET('/route', success, failure);
    xhr[event](); xhr.onload();
}
xhr = ctx.doGET('/route', success, failure);
xhr.status = 0; xhr.onload();
xhr = ctx.doGET('/route', ctx.parseJSONResponse, failure);
xhr.responseText = '{"best":';
xhr.onload();
assert.equal(ok, 1); assert.equal(failed, 5);
assert.match(xhr.errorMessage, /Invalid JSON response.*8 characters/);
assert.throws(() => ctx.parseJSONResponse({
    responseText: '{}', getResponseHeader: key => key === 'X-Route-Bytes' ? '100' : null
}), /Incomplete route response: received 2 of 100/);
assert.equal(ctx.parseJSONResponse({
    responseText: '{"label":"São"}', getResponseHeader: key =>
        key === 'X-Route-Bytes' ? String(new TextEncoder().encode('{"label":"São"}').length) : null
}).label, 'São');
xhr = ctx.doGET('/route', async () => { throw new Error('async callback'); }, failure);
xhr.onload();
setImmediate(() => {
    assert.equal(failed, 6);
    const Original = ctx.XMLHttpRequest;
    ctx.XMLHttpRequest = class extends Original { send() { throw new Error('send failed'); } };
    ctx.doGET('/route', success, failure);
    assert.equal(failed, 7);
    console.log('XHR interruption, status zero, callback failures, setup order and query encoding passed.');
});


// A truncated transfer retries the same URL once and only publishes a complete result.
let receivedJSON = 0, terminal = 0, retried = 0;
ctx.doGETJSON('/route?fixedTime=1', () => receivedJSON++, () => terminal++,
    undefined, {onRetry: () => retried++});
let first = requests.at(-1);
first.send = () => {};
first.response = encoded.buffer.slice(0, 5);
first.getResponseHeader = key => key === 'X-Route-Bytes' ? String(encoded.byteLength) : null;
first.onload();
let second = requests.at(-1);
assert.notEqual(second, first);
assert.equal(second.url, first.url);
second.response = encoded.buffer;
second.getResponseHeader = first.getResponseHeader;
second.onload();
assert.equal(receivedJSON, 1); assert.equal(terminal, 0); assert.equal(retried, 1);
ctx.doGETJSON('/route', () => receivedJSON++, () => terminal++);
first = requests.at(-1);
first.response = encoded.buffer.slice(0, 5);
first.getResponseHeader = second.getResponseHeader;
first.onload();
second = requests.at(-1);
second.response = first.response;
second.getResponseHeader = first.getResponseHeader;
const count = requests.length;
second.onload();
assert.equal(requests.length, count);
assert.equal(terminal, 1);
console.log('Incomplete JSON retry, fixed URL, successful recovery and bounded failure passed.');
