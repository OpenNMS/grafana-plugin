'use strict';

System.register(['lodash', '../../opennms'], function (_export, _context) {
    "use strict";

    var _, API, _createClass, FilterCloner;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_lodash) {
            _ = _lodash.default;
        }, function (_opennms) {
            API = _opennms.API;
        }],
        execute: function () {
            _createClass = function () {
                function defineProperties(target, props) {
                    for (var i = 0; i < props.length; i++) {
                        var descriptor = props[i];
                        descriptor.enumerable = descriptor.enumerable || false;
                        descriptor.configurable = true;
                        if ("value" in descriptor) descriptor.writable = true;
                        Object.defineProperty(target, descriptor.key, descriptor);
                    }
                }

                return function (Constructor, protoProps, staticProps) {
                    if (protoProps) defineProperties(Constructor.prototype, protoProps);
                    if (staticProps) defineProperties(Constructor, staticProps);
                    return Constructor;
                };
            }();

            _export('FilterCloner', FilterCloner = function () {
                function FilterCloner() {
                    _classCallCheck(this, FilterCloner);
                }

                _createClass(FilterCloner, [{
                    key: 'cloneFilter',
                    value: function cloneFilter(filter) {
                        var newFilter = new API.Filter();
                        newFilter.limit = filter.limit;
                        newFilter.clauses = this.cloneNestedRestriction(filter).clauses;
                        return newFilter;
                    }
                }, {
                    key: 'cloneClause',
                    value: function cloneClause(clause) {
                        var operator = _.find(API.Operators, function (operator) {
                            return operator.label === clause.operator.label;
                        });

                        // Nested restriction
                        if (clause.restriction.clauses) {
                            var nestedRestriction = this.cloneNestedRestriction(clause.restriction);
                            return new API.Clause(nestedRestriction, operator);
                        } else {
                            // Normal restriction
                            var restriction = this.cloneRestriction(clause.restriction);
                            return new API.Clause(restriction, operator);
                        }
                    }
                }, {
                    key: 'cloneNestedRestriction',
                    value: function cloneNestedRestriction(nestedRestriction) {
                        var self = this;
                        var newNestedRestriction = new API.NestedRestriction();
                        _.each(nestedRestriction.clauses, function (clause) {
                            newNestedRestriction.withClause(self.cloneClause(clause));
                        });
                        return newNestedRestriction;
                    }
                }, {
                    key: 'cloneRestriction',
                    value: function cloneRestriction(restriction) {
                        var comparator = _.find(API.Comparators, function (comparator) {
                            return comparator.label === restriction.comparator.label;
                        });
                        return new API.Restriction(restriction.attribute, comparator, restriction.value);
                    }
                }]);

                return FilterCloner;
            }());

            _export('FilterCloner', FilterCloner);
        }
    };
});
//# sourceMappingURL=FilterCloner.js.map
