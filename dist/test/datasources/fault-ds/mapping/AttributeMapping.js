'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.AttributeMapping = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var AttributeMapping = exports.AttributeMapping = function () {
    function AttributeMapping() {
        _classCallCheck(this, AttributeMapping);

        this.attributeMapping = {
            'location': 'node.location.locationName',
            'service': 'service',
            'category': 'category.name',
            'ipAddress': 'ipAddr',
            'severity': 'alarm.severity'
        };
    }

    _createClass(AttributeMapping, [{
        key: 'getUiAttribute',
        value: function getUiAttribute(externalAttribute) {
            var internalAttribute = _lodash2.default.findKey(this.attributeMapping, function (value) {
                return value === externalAttribute;
            });
            return internalAttribute || externalAttribute;
        }
    }, {
        key: 'getApiAttribute',
        value: function getApiAttribute(internalAttribute) {
            return this.attributeMapping[internalAttribute] || internalAttribute;
        }
    }]);

    return AttributeMapping;
}();
//# sourceMappingURL=AttributeMapping.js.map
