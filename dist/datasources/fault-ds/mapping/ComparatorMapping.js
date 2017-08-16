'use strict';

System.register(['lodash', '../../../opennms'], function (_export, _context) {
    "use strict";

    var _, API, _createClass, ComparatorMapping;

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

            _export('ComparatorMapping', ComparatorMapping = function () {
                function ComparatorMapping() {
                    _classCallCheck(this, ComparatorMapping);
                }

                _createClass(ComparatorMapping, [{
                    key: 'getUiComparator',
                    value: function getUiComparator(apiComparator) {
                        var theComparator = API.Comparators[apiComparator.label];
                        if (theComparator !== API.Comparators.NULL && theComparator !== API.Comparators.NOTNULL && theComparator != API.LIKE && theComparator != API.ILIKE && theComparator.aliases && theComparator.aliases.length > 0) {
                            return theComparator.aliases[0];
                        }
                        throw new Error("No matching UI comparator found for '" + apiComparator.label + "'.");
                    }
                }, {
                    key: 'getApiComparator',
                    value: function getApiComparator(uiComparator) {
                        var apiComparator = _.find(API.Comparators, function (comparator) {
                            return comparator.matches(uiComparator);
                        });
                        if (!apiComparator) {
                            throw new Error("No API comparator for alias '" + uiComparator + "' found.");
                        }
                        return apiComparator;
                    }
                }]);

                return ComparatorMapping;
            }());

            _export('ComparatorMapping', ComparatorMapping);
        }
    };
});
//# sourceMappingURL=ComparatorMapping.js.map
