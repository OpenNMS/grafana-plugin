'use strict';

System.register(['lodash'], function (_export, _context) {
    "use strict";

    var _, _createClass, AttributeMapping;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_lodash) {
            _ = _lodash.default;
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

            _export('AttributeMapping', AttributeMapping = function () {
                function AttributeMapping() {
                    _classCallCheck(this, AttributeMapping);

                    this.attributeMapping = {
                        'location': 'location.locationName',
                        'service': 'serviceType.name',
                        'category': 'category.name',
                        'ipAddress': 'ipAddr',
                        'severity': 'severity'
                    };
                }

                _createClass(AttributeMapping, [{
                    key: 'getUiAttribute',
                    value: function getUiAttribute(externalAttribute) {
                        var internalAttribute = _.findKey(this.attributeMapping, function (value) {
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
            }());

            _export('AttributeMapping', AttributeMapping);
        }
    };
});
//# sourceMappingURL=AttributeMapping.js.map
