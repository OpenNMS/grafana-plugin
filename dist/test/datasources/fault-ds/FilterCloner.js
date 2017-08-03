'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
exports.FilterCloner = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _opennms = require('../../opennms');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * The filter may be reloaded from a persisted state.
 * The internal opennms.js API requires a concrete implementation of Comparators or Operators in order to work.
 * As the object was persisted, the references DO NOT MATCH. In order to make them match, we just rebuild the filter.
 */
var FilterCloner = exports.FilterCloner = function () {
    function FilterCloner() {
        _classCallCheck(this, FilterCloner);
    }

    _createClass(FilterCloner, [{
        key: 'cloneFilter',
        value: function cloneFilter(filter) {
            var newFilter = new _opennms.API.Filter();
            newFilter.limit = filter.limit;
            newFilter.clauses = this.cloneNestedRestriction(filter).clauses;
            return newFilter;
        }
    }, {
        key: 'cloneClause',
        value: function cloneClause(clause) {
            var operator = _lodash2.default.find(_opennms.API.Operators, function (operator) {
                return operator.label === clause.operator.label;
            });

            // Nested restriction
            if (clause.restriction.clauses) {
                var nestedRestriction = this.cloneNestedRestriction(clause.restriction);
                return new _opennms.API.Clause(nestedRestriction, operator);
            } else {
                // Normal restriction
                var restriction = this.cloneRestriction(clause.restriction);
                return new _opennms.API.Clause(restriction, operator);
            }
        }
    }, {
        key: 'cloneNestedRestriction',
        value: function cloneNestedRestriction(nestedRestriction) {
            var self = this;
            var newNestedRestriction = new _opennms.API.NestedRestriction();
            _lodash2.default.each(nestedRestriction.clauses, function (clause) {
                newNestedRestriction.withClause(self.cloneClause(clause));
            });
            return newNestedRestriction;
        }
    }, {
        key: 'cloneRestriction',
        value: function cloneRestriction(restriction) {
            var comparator = _lodash2.default.find(_opennms.API.Comparators, function (comparator) {
                return comparator.label === restriction.comparator.label;
            });
            return new _opennms.API.Restriction(restriction.attribute, comparator, restriction.value);
        }
    }]);

    return FilterCloner;
}();
//# sourceMappingURL=FilterCloner.js.map
