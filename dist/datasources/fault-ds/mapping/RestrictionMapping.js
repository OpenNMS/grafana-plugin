'use strict';

System.register(['lodash', './ComparatorMapping', './ClauseMapping', './AttributeMapping', './ValueMapping', '../UI', '../../../opennms'], function (_export, _context) {
    "use strict";

    var _, ComparatorMapping, ClauseMapping, AttributeMapping, ValueMapping, UI, API, _createClass, RestrictionMapping;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_lodash) {
            _ = _lodash.default;
        }, function (_ComparatorMapping) {
            ComparatorMapping = _ComparatorMapping.ComparatorMapping;
        }, function (_ClauseMapping) {
            ClauseMapping = _ClauseMapping.ClauseMapping;
        }, function (_AttributeMapping) {
            AttributeMapping = _AttributeMapping.AttributeMapping;
        }, function (_ValueMapping) {
            ValueMapping = _ValueMapping.ValueMapping;
        }, function (_UI) {
            UI = _UI.UI;
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

            _export('RestrictionMapping', RestrictionMapping = function () {
                function RestrictionMapping(uiSegmentSrv) {
                    _classCallCheck(this, RestrictionMapping);

                    this.uiSegmentSrv = uiSegmentSrv;
                }

                _createClass(RestrictionMapping, [{
                    key: 'getUiRestriction',
                    value: function getUiRestriction(apiRestriction) {
                        if (apiRestriction instanceof API.NestedRestriction) {
                            return this.getUiQuery(apiRestriction);
                        } else {
                            var uiRestriction = new UI.Restriction(this.uiSegmentSrv, new UI.RestrictionDTO(new AttributeMapping().getUiAttribute(apiRestriction.attribute), new ComparatorMapping().getUiComparator(apiRestriction.comparator), new ValueMapping().getUiValue(apiRestriction.attribute, apiRestriction.value)));
                            return uiRestriction;
                        }
                    }
                }, {
                    key: 'getUiQuery',
                    value: function getUiQuery(apiNestedRestriction) {
                        var self = this;
                        var uiQuery = new UI.Query(this.uiSegmentSrv);
                        var uiClauses = _.map(apiNestedRestriction.clauses, function (clause) {
                            return new ClauseMapping(self.uiSegmentSrv).getUiClause(clause);
                        });
                        _.each(uiClauses, function (uiClause) {
                            uiQuery.addClause(uiClause);
                        });
                        return uiQuery;
                    }
                }, {
                    key: 'getApiRestriction',
                    value: function getApiRestriction(uiRestriction) {
                        if (uiRestriction instanceof UI.Query) {
                            return this.getApiNestedRestriction(uiRestriction);
                        } else {
                            var restrictionDTO = uiRestriction.asRestrictionDTO();
                            if (restrictionDTO !== null) {
                                var attribute = new AttributeMapping().getApiAttribute(restrictionDTO.attribute);
                                var comparator = new ComparatorMapping().getApiComparator(restrictionDTO.comparator);
                                var value = new ValueMapping().getApiValue(restrictionDTO.attribute, restrictionDTO.value);
                                return new API.Restriction(attribute, comparator, value);
                            }
                            return null;
                        }
                    }
                }, {
                    key: 'getApiNestedRestriction',
                    value: function getApiNestedRestriction(uiQuery) {
                        var self = this;
                        var nestedRestriction = new API.NestedRestriction();
                        _.each(uiQuery.clauses, function (uiClause) {
                            var apiClause = new ClauseMapping(self.uiSegmentSrv).getApiClause(uiClause);
                            if (apiClause !== null) {
                                nestedRestriction.withClause(apiClause);
                            }
                        });
                        return nestedRestriction;
                    }
                }]);

                return RestrictionMapping;
            }());

            _export('RestrictionMapping', RestrictionMapping);
        }
    };
});
//# sourceMappingURL=RestrictionMapping.js.map
