'use strict';

System.register(['lodash', './OperatorMapping', './RestrictionMapping', '../UI', '../../../opennms'], function (_export, _context) {
    "use strict";

    var _, OperatorMapping, RestrictionMapping, UI, API, _createClass, ClauseMapping;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_lodash) {
            _ = _lodash.default;
        }, function (_OperatorMapping) {
            OperatorMapping = _OperatorMapping.OperatorMapping;
        }, function (_RestrictionMapping) {
            RestrictionMapping = _RestrictionMapping.RestrictionMapping;
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

            _export('ClauseMapping', ClauseMapping = function () {
                function ClauseMapping(uiSegmentSrv) {
                    _classCallCheck(this, ClauseMapping);

                    this.uiSegmentSrv = uiSegmentSrv;
                }

                _createClass(ClauseMapping, [{
                    key: 'getUiClause',
                    value: function getUiClause(apiClause) {
                        if (!apiClause instanceof API.Clause) {
                            throw new TypeError("apiClause is not of type API.Clause");
                        }
                        var uiOperator = new OperatorMapping().getUiOperator(apiClause.operator);
                        var uiRestriction = new RestrictionMapping(this.uiSegmentSrv).getUiRestriction(apiClause.restriction);
                        return new UI.Clause(this.uiSegmentSrv, uiOperator, uiRestriction);
                    }
                }, {
                    key: 'getApiClause',
                    value: function getApiClause(uiClause) {
                        if (!uiClause instanceof UI.Clause) {
                            throw new TypeError("uiClause is not of type UI.Clause");
                        }
                        var apiOperator = new OperatorMapping().getApiOperator(uiClause.operator.value);
                        var apiRestriction = new RestrictionMapping(this.uiSegmentSrv).getApiRestriction(uiClause.restriction);
                        if (apiRestriction !== null) {
                            return new API.Clause(apiRestriction, apiOperator);
                        }
                        return null;
                    }
                }]);

                return ClauseMapping;
            }());

            _export('ClauseMapping', ClauseMapping);
        }
    };
});
//# sourceMappingURL=ClauseMapping.js.map
