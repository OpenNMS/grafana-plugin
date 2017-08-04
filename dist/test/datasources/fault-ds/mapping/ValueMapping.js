"use strict";

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ValueMapping = exports.ValueMapping = function () {
    function ValueMapping() {
        _classCallCheck(this, ValueMapping);
    }

    _createClass(ValueMapping, [{
        key: "getApiValue",
        value: function getApiValue(internalAttribute, value) {
            // TODO MVR value mapping should be moved to the opennms-js implementation
            // if (internalAttribute === 'alarmAckTime') {
            //     if (value === 'null') {
            //         value = new Date(0);
            //     }
            //
            //     // yyyy-MM-ddTHH:mm:ss.sssZ
            //     // 2017-06-08T10:17:17.173+0200
            //     value = value.toJSON().replace("Z", "+0000"); // make it parsable by java
            // }
            if ("null" === value) {
                value = "\0";
            }
            return value;
        }
    }, {
        key: "getUiValue",
        value: function getUiValue(internalAttribute, value) {
            // TODO MVR value mapping should be moved to the opennms-js implementation
            // if (internalAttribute === 'alarmAckTime') {
            //     if (value === new Date(0)) {
            //         return 'null';
            //     }
            //     var stringified = JSON.parse(value.replace("+0000", "Z"));
            //     return new Date(stringified);
            // }
            if (value === "\0") {
                return "null";
            }
            return value;
        }
    }]);

    return ValueMapping;
}();
//# sourceMappingURL=ValueMapping.js.map
