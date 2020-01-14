'use strict';

System.register(['lodash', '../../../opennms'], function (_export, _context) {
    "use strict";

    var _, API, _createClass, OperatorMapping;

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

            _export('OperatorMapping', OperatorMapping = function () {
                function OperatorMapping() {
                    _classCallCheck(this, OperatorMapping);
                }

                _createClass(OperatorMapping, [{
                    key: 'getUiOperator',
                    value: function getUiOperator(apiOperator) {
                        var theOperator = API.Operators[apiOperator.label];
                        if (theOperator && theOperator.label) {
                            return theOperator.label;
                        }
                        throw { message: "No operator found with label " + apiOperator.label, operators: API.Operators };
                    }
                }, {
                    key: 'getApiOperator',
                    value: function getApiOperator(uiOperator) {
                        var apiOperator = _.find(API.Operators, function (eachOperator) {
                            return eachOperator.matches(uiOperator);
                        });
                        if (!apiOperator) {
                            throw new Error("Could not map uiOperator '" + uiOperator + "' to API operator.");
                        }
                        return apiOperator;
                    }
                }]);

                return OperatorMapping;
            }());

            _export('OperatorMapping', OperatorMapping);
        }
    };
});
//# sourceMappingURL=OperatorMapping.js.map
