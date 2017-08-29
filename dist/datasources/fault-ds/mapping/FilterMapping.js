'use strict';

System.register(['lodash', '../../../opennms', '../UI', './ClauseMapping'], function (_export, _context) {
    "use strict";

    var _, API, UI, ClauseMapping, _createClass, FilterMapping;

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
        }, function (_UI) {
            UI = _UI.UI;
        }, function (_ClauseMapping) {
            ClauseMapping = _ClauseMapping.ClauseMapping;
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

            _export('FilterMapping', FilterMapping = function () {
                function FilterMapping(uiSegmentSrv) {
                    _classCallCheck(this, FilterMapping);

                    this.uiSegmentSrv = uiSegmentSrv;
                }

                _createClass(FilterMapping, [{
                    key: 'getApiFilter',
                    value: function getApiFilter(uiFilter) {
                        // Ensure we can migrate
                        if (!uiFilter instanceof UI.Filter) {
                            throw new TypeError("uiFilter is not of type UI.Filter.");
                        }

                        var self = this;
                        var filter = new API.Filter();
                        filter.limit = 0;

                        _.each(uiFilter.query.clauses, function (eachClause) {
                            var apiClause = new ClauseMapping(self.uiSegmentSrv).getApiClause(eachClause);
                            if (apiClause !== null) {
                                filter.withClause(apiClause);
                            }
                        });
                        return filter;
                    }
                }, {
                    key: 'getUiFilter',
                    value: function getUiFilter(apiFilter) {
                        if (!apiFilter instanceof API.Filter) {
                            throw new TypeError("apiFilter is not of type API.Filter");
                        }

                        var self = this;
                        var uiFilter = new UI.Filter(this.uiSegmentSrv);

                        _.each(apiFilter.clauses, function (apiClause) {
                            var uiClause = new ClauseMapping(self.uiSegmentSrv).getUiClause(apiClause);
                            uiFilter.addClause(uiClause);

                            // set parentQuery for all nested queries
                            self.applyParentQuery(uiClause, uiFilter.query);
                        });

                        return uiFilter;
                    }
                }, {
                    key: 'applyParentQuery',
                    value: function applyParentQuery(clause, parentQuery) {
                        if (clause.restriction instanceof UI.Query) {
                            clause.restriction.parentQuery = parentQuery;
                            this.applyParentQuery(clause.restriction.clauses, clause.restriction);
                        }
                    }
                }]);

                return FilterMapping;
            }());

            _export('FilterMapping', FilterMapping);
        }
    };
});
//# sourceMappingURL=FilterMapping.js.map
