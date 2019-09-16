'use strict';

System.register(['lodash', '../UI'], function (_export, _context) {
    "use strict";

    var _, UI, _createClass, Clause;

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_lodash) {
            _ = _lodash.default;
        }, function (_UI) {
            UI = _UI.UI;
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

            _export('Clause', Clause = function () {
                function Clause(uiSegmentSrv, operator, restriction) {
                    _classCallCheck(this, Clause);

                    if (restriction instanceof UI.RestrictionDTO) {
                        restriction = new UI.Restriction(uiSegmentSrv, restriction);
                    }
                    this.uiSegmentSrv = uiSegmentSrv;
                    this.restriction = restriction;
                    this.controls = [];
                    this.setOperator(operator);
                }

                _createClass(Clause, [{
                    key: 'setOperator',
                    value: function setOperator(newOperator) {
                        // initialize if not initialized
                        if (this.operator === void 0) {
                            this.operator = this.uiSegmentSrv.newCondition(newOperator);
                        }
                        // Update the value if already initialized
                        this.operator.value = newOperator;
                    }
                }, {
                    key: 'updateControls',
                    value: function updateControls(query) {
                        var controls = [new UI.Controls.RemoveControl(), new UI.Controls.AddControl(), new UI.Controls.AddNestedControl()];

                        var self = this;
                        this.controls = _.filter(controls, function (control) {
                            return control.filter(query, self);
                        });

                        if (this.restriction instanceof UI.Query) {
                            this.restriction.updateControls();
                        }
                    }
                }]);

                return Clause;
            }());

            _export('Clause', Clause);
        }
    };
});
//# sourceMappingURL=Clause.js.map
