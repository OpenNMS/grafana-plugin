'use strict';

System.register(['lodash', './Query'], function (_export, _context) {
    "use strict";

    var _, Query, _createClass, Control, RemoveControl, AddControl, AddNestedControl;

    function _possibleConstructorReturn(self, call) {
        if (!self) {
            throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
        }

        return call && (typeof call === "object" || typeof call === "function") ? call : self;
    }

    function _inherits(subClass, superClass) {
        if (typeof superClass !== "function" && superClass !== null) {
            throw new TypeError("Super expression must either be null or a function, not " + typeof superClass);
        }

        subClass.prototype = Object.create(superClass && superClass.prototype, {
            constructor: {
                value: subClass,
                enumerable: false,
                writable: true,
                configurable: true
            }
        });
        if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass;
    }

    function _classCallCheck(instance, Constructor) {
        if (!(instance instanceof Constructor)) {
            throw new TypeError("Cannot call a class as a function");
        }
    }

    return {
        setters: [function (_lodash) {
            _ = _lodash.default;
        }, function (_Query) {
            Query = _Query.Query;
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

            Control = function () {
                function Control(title, icon) {
                    _classCallCheck(this, Control);

                    this.title = title;
                    this.icon = icon;
                }

                _createClass(Control, [{
                    key: 'action',
                    value: function action() /* query, clause */{
                        throw new Error("Method action(...) not implemented");
                    }
                }, {
                    key: 'filter',
                    value: function filter() /* query, clause */{
                        throw new Error("Method filter(...) not implemented");
                    }
                }]);

                return Control;
            }();

            _export('RemoveControl', RemoveControl = function (_Control) {
                _inherits(RemoveControl, _Control);

                function RemoveControl() {
                    _classCallCheck(this, RemoveControl);

                    return _possibleConstructorReturn(this, (RemoveControl.__proto__ || Object.getPrototypeOf(RemoveControl)).call(this, "Remove clause", 'fa-minus'));
                }

                _createClass(RemoveControl, [{
                    key: 'action',
                    value: function action(query, clause) {
                        query.removeClause(clause);
                        if (query.root === false && query.getSize() === 0 && query.parentQuery !== undefined) {
                            var parentClause = _.find(query.parentQuery.clauses, function (clause) {
                                return clause.restriction == query;
                            });
                            query.parentQuery.removeClause(parentClause);
                        }
                    }
                }, {
                    key: 'filter',
                    value: function filter(query, clause) {
                        if (clause.restriction instanceof Query) {
                            return false;
                        }
                        if (query.root === true) {
                            return query.clauses.length > 1 || clause.restriction.asRestrictionDTO() !== null;
                        }
                        return true;
                    }
                }]);

                return RemoveControl;
            }(Control));

            _export('RemoveControl', RemoveControl);

            _export('AddControl', AddControl = function (_Control2) {
                _inherits(AddControl, _Control2);

                function AddControl() {
                    _classCallCheck(this, AddControl);

                    return _possibleConstructorReturn(this, (AddControl.__proto__ || Object.getPrototypeOf(AddControl)).call(this, 'Add new clause', 'fa-plus'));
                }

                _createClass(AddControl, [{
                    key: 'action',
                    value: function action(query, clause) {
                        var index = query.clauses.indexOf(clause) + 1;
                        query.createNewEmptyClause(index);
                    }
                }, {
                    key: 'filter',
                    value: function filter(query, clause) {
                        if (clause.restriction instanceof Query) {
                            return false;
                        }
                        return true;
                    }
                }]);

                return AddControl;
            }(Control));

            _export('AddControl', AddControl);

            _export('AddNestedControl', AddNestedControl = function (_Control3) {
                _inherits(AddNestedControl, _Control3);

                function AddNestedControl() {
                    _classCallCheck(this, AddNestedControl);

                    return _possibleConstructorReturn(this, (AddNestedControl.__proto__ || Object.getPrototypeOf(AddNestedControl)).call(this, 'Add nested clause', 'fa-file'));
                }

                _createClass(AddNestedControl, [{
                    key: 'action',
                    value: function action(query, clause) {
                        var index = query.clauses.indexOf(clause) + 1;
                        query.createNewEmptyNestedClause(index);
                    }
                }, {
                    key: 'filter',
                    value: function filter(query, clause) {
                        return query.root === true && !(clause.restriction instanceof Query);
                    }
                }]);

                return AddNestedControl;
            }(Control));

            _export('AddNestedControl', AddNestedControl);
        }
    };
});
//# sourceMappingURL=Controls.js.map
