"use strict";

System.register(["../../opennms"], function (_export, _context) {
    "use strict";

    var API, Model, Example, Examples;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_opennms) {
            API = _opennms.API;
            Model = _opennms.Model;
        }],
        execute: function () {
            _export("Example", Example = function Example(description, apiFilter) {
                _classCallCheck(this, Example);

                this.apiFilter = apiFilter;
                this.description = description;
            });

            _export("Example", Example);

            _export("Examples", Examples = [new Example("Show all alarms", new API.Filter()), new Example("Show all alarms for location 'Default'", new API.Filter().withAndRestriction(new API.Restriction("node.location.locationName", API.Comparators.EQ, 'Default'))), new Example("Show all alarms with a severity between 'Warning' and 'Major'", new API.Filter().withAndRestriction(new API.Restriction("alarm.severity", API.Comparators.GE, Model.Severities.WARNING.label)).withAndRestriction(new API.Restriction("alarm.severity", API.Comparators.LE, Model.Severities.MAJOR.label))), new Example("Show all alarms for nodes in category 'Servers'", new API.Filter().withAndRestriction(new API.Restriction("category.name", API.Comparators.EQ, 'Severs'))), new Example("Show all unacknowledged alarms", new API.Filter().withAndRestriction(new API.Restriction("alarmAckTime", API.Comparators.EQ, 'null'))), new Example("Show all acknowledged alarms", new API.Filter().withAndRestriction(new API.Restriction("alarmAckTime", API.Comparators.NE, 'null')))]);

            _export("Examples", Examples);
        }
    };
});
//# sourceMappingURL=Examples.js.map
