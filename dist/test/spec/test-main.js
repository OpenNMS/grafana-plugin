'use strict';

var _prunk = require('prunk');

var _prunk2 = _interopRequireDefault(_prunk);

var _jsdom = require('jsdom');

var _jsdom2 = _interopRequireDefault(_jsdom);

var _chai = require('chai');

var _chai2 = _interopRequireDefault(_chai);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var JSDOM = _jsdom2.default.JSDOM;

// Mock Grafana modules that are not available outside of the core project
// Required for loading module.js
_prunk2.default.mock('./css/query-editor.css!', 'no css, dude.');
_prunk2.default.mock('app/plugins/sdk', {
  QueryCtrl: null,
  loadPluginCss: function loadPluginCss() {}
});

// Setup jsdom
// Required for loading angularjs
global.document = new JSDOM('<html><head><script></script></head><body></body></html>');
global.window = global.document.parentWindow;

// Setup Chai
_chai2.default.should();
global.assert = _chai2.default.assert;
global.expect = _chai2.default.expect;
//# sourceMappingURL=test-main.js.map
