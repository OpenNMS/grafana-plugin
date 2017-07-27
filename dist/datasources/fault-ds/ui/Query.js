'use strict';

System.register(['lodash', '../UI'], function (_export, _context) {
    "use strict";

    var _, UI, _createClass, Query;

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

            _export('Query', Query = function () {
                function Query(uiSegmentSrv, parentQuery) {
                    _classCallCheck(this, Query);

                    this.uiSegmentSrv = uiSegmentSrv;
                    this.clauses = [];
                    this.root = false;
                    this.parentQuery = parentQuery;
                }

                _createClass(Query, [{
                    key: 'clear',
                    value: function clear() {
                        this.clauses = [];
                    }
                }, {
                    key: 'isEmpty',
                    value: function isEmpty() {
                        return this.getSize() == 0;
                    }
                }, {
                    key: 'getSize',
                    value: function getSize() {
                        return this.clauses.length;
                    }
                }, {
                    key: 'getLastClause',
                    value: function getLastClause() {
                        if (this.clauses.length == 0) {
                            return null;
                        }
                        return this.clauses[this.getSize() - 1];
                    }
                }, {
                    key: 'updateControls',
                    value: function updateControls() {
                        // at least one row should be available even if it is a dummy row
                        if (this.getSize() == 0) {
                            this.createNewEmptyClause();
                        }
                        var self = this;
                        _.each(this.clauses, function (clause) {
                            clause.updateControls(self);
                        });
                    }
                }, {
                    key: 'addClause',
                    value: function addClause(clause, index) {
                        if (clause) {
                            if (!clause.uiSegmentSrv) {
                                clause.uiSegmentSrv = this.uiSegmentSrv;
                            }
                            if (index !== undefined) {
                                this.clauses.splice(index, 0, clause);
                            } else {
                                this.clauses.push(clause);
                            }
                        }
                    }
                }, {
                    key: 'removeClause',
                    value: function removeClause(clause) {
                        if (clause) {
                            var index = this.clauses.indexOf(clause);
                            if (index >= 0) {
                                this.clauses.splice(index, 1);
                                return true;
                            }
                        }
                        return false;
                    }
                }, {
                    key: 'asString',
                    value: function asString() {
                        if (this.isEmpty()) {
                            return "";
                        }
                        return _.map(this.clauses, function (clause, index) {
                            var string = '';
                            if (clause.restriction instanceof UI.Query) {
                                var subString = clause.restriction.asString();
                                if (subString && subString.length > 0) {
                                    string += "(" + subString + ")";
                                }
                            } else if (clause.restriction) {
                                var restrictionString = clause.restriction.asString();
                                if (restrictionString && restrictionString.length > 0) {
                                    string += restrictionString;
                                }
                            } else {
                                throw { message: "Clause does not contain restriction. Bailing", clause: clause };
                            }

                            // Append operator if we have anything generated
                            if (string.length > 0 && index > 0 && clause.operator && clause.operator.value) {
                                string = " " + clause.operator.value.toLowerCase() + " " + string;
                            }
                            return string;
                        }).join("");
                    }
                }, {
                    key: 'createNewEmptyClause',
                    value: function createNewEmptyClause(index) {
                        var newClause = new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv));
                        newClause.restriction.addSegment(this.uiSegmentSrv.newKey('select attribute'));
                        newClause.restriction.addSegment(this.uiSegmentSrv.newOperator('='));
                        newClause.restriction.addSegment(this.uiSegmentSrv.newFake('select value', 'value', 'query-segment-value'));
                        this.addClause(newClause, index);
                        return newClause;
                    }
                }, {
                    key: 'createNewEmptyNestedClause',
                    value: function createNewEmptyNestedClause(index) {
                        var newQuery = new UI.Query(this.uiSegmentSrv, this);
                        newQuery.createNewEmptyClause();
                        var newClause = new UI.Clause(this.uiSegmentSrv, UI.Operators.AND, newQuery);
                        this.addClause(newClause, index);
                        return newQuery;
                    }
                }, {
                    key: 'findParent',
                    value: function findParent() {
                        if (this.parentQuery) {
                            return this.parentQuery.findParent();
                        }
                        return this;
                    }
                }, {
                    key: 'segmentUpdated',
                    value: function segmentUpdated(clause, segment) {
                        if (segment.type === 'value') {
                            segment.fake = false;
                        }

                        if (segment.type === 'condition') {
                            clause.setOperator(segment.value);
                        }

                        // Ensure that we always have a plus button
                        this.updateControls();
                    }
                }]);

                return Query;
            }());

            _export('Query', Query);
        }
    };
});
//# sourceMappingURL=Query.js.map
