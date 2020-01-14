'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.OperatorMapping = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../../opennms');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var OperatorMapping = exports.OperatorMapping = function () {
    function OperatorMapping() {
        _classCallCheck(this, OperatorMapping);
    }

    _createClass(OperatorMapping, [{
        key: 'getUiOperator',
        value: function getUiOperator(apiOperator) {
            var theOperator = _opennms.API.Operators[apiOperator.label];
            if (theOperator && theOperator.label) {
                return theOperator.label;
            }
            throw { message: "No operator found with label " + apiOperator.label, operators: _opennms.API.Operators };
        }
    }, {
        key: 'getApiOperator',
        value: function getApiOperator(uiOperator) {
            var apiOperator = _lodash2.default.find(_opennms.API.Operators, function (eachOperator) {
                return eachOperator.matches(uiOperator);
            });
            if (!apiOperator) {
                throw new Error("Could not map uiOperator '" + uiOperator + "' to API operator.");
            }
            return apiOperator;
        }
    }]);

    return OperatorMapping;
}();
//# sourceMappingURL=OperatorMapping.js.map
