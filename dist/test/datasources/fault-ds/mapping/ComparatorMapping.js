'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.ComparatorMapping = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../../opennms');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var ComparatorMapping = exports.ComparatorMapping = function () {
    function ComparatorMapping() {
        _classCallCheck(this, ComparatorMapping);
    }

    _createClass(ComparatorMapping, [{
        key: 'getUiComparator',
        value: function getUiComparator(apiComparator) {
            var theComparator = _opennms.API.Comparators[apiComparator.label];
            if (theComparator !== _opennms.API.Comparators.NULL && theComparator !== _opennms.API.Comparators.NOTNULL && theComparator != _opennms.API.LIKE && theComparator != _opennms.API.ILIKE && theComparator.aliases && theComparator.aliases.length > 0) {
                return theComparator.aliases[0];
            }
            throw new Error("No matching UI comparator found for '" + apiComparator.label + "'.");
        }
    }, {
        key: 'getApiComparator',
        value: function getApiComparator(uiComparator) {
            var apiComparator = _lodash2.default.find(_opennms.API.Comparators, function (comparator) {
                return comparator.matches(uiComparator);
            });
            if (!apiComparator) {
                throw new Error("No API comparator for alias '" + uiComparator + "' found.");
            }
            return apiComparator;
        }
    }]);

    return ComparatorMapping;
}();
//# sourceMappingURL=ComparatorMapping.js.map
