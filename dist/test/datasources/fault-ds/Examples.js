"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.Examples = exports.Example = undefined;

var _opennms = require("../../opennms");

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var Example = exports.Example = function Example(description, apiFilter) {
    _classCallCheck(this, Example);

    this.apiFilter = apiFilter;
    this.description = description;
};

// Some examples to use


var Examples = exports.Examples = [new Example("Show all alarms", new _opennms.API.Filter()), new Example("Show all alarms for location 'Default'", new _opennms.API.Filter().withAndRestriction(new _opennms.API.Restriction("node.location.locationName", _opennms.API.Comparators.EQ, 'Default'))), new Example("Show all alarms with a severity between 'Warning' and 'Major'", new _opennms.API.Filter().withAndRestriction(new _opennms.API.Restriction("alarm.severity", _opennms.API.Comparators.GE, _opennms.Model.Severities.WARNING.label)).withAndRestriction(new _opennms.API.Restriction("alarm.severity", _opennms.API.Comparators.LE, _opennms.Model.Severities.MAJOR.label))), new Example("Show all alarms for nodes in category 'Servers'", new _opennms.API.Filter().withAndRestriction(new _opennms.API.Restriction("category.name", _opennms.API.Comparators.EQ, 'Severs'))), new Example("Show all unacknowledged alarms", new _opennms.API.Filter().withAndRestriction(new _opennms.API.Restriction("alarmAckTime", _opennms.API.Comparators.EQ, 'null'))), new Example("Show all acknowledged alarms", new _opennms.API.Filter().withAndRestriction(new _opennms.API.Restriction("alarmAckTime", _opennms.API.Comparators.NE, 'null')))];
//# sourceMappingURL=Examples.js.map
