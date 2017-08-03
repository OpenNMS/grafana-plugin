'use strict';

var _q = require('q');

var _q2 = _interopRequireDefault(_q);

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _UI = require('../datasources/fault-ds/UI');

var _opennms = require('../opennms');

var _Mapping = require('../datasources/fault-ds/Mapping');

var _FilterCloner = require('../datasources/fault-ds/FilterCloner');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

describe("OpenNMS_FaultManagement_Datasource", function () {
    var uiSegmentSrv = {
        newSegment: function newSegment(value, type) {
            return { value: value, type: type };
        },
        newKey: function newKey(key) {
            return this.newSegment(key, 'key');
        },
        newOperator: function newOperator(operator) {
            return this.newSegment(operator, 'operator');
        },
        newFake: function newFake(text, type, cssClass) {
            var segment = this.newSegment(text, type);
            segment.fake = true;
            return segment;
        },
        newPlusButton: function newPlusButton() {
            return this.newFake('', 'plus-button');
        },
        newKeyValue: function newKeyValue(value) {
            return this.newSegment(value, 'value');
        },
        newCondition: function newCondition(condition) {
            return this.newSegment(condition, 'condition');
        }
    };

    describe('Mapping', function () {
        describe('ComparatorMapping', function () {
            var mapping = new _Mapping.Mapping.ComparatorMapping();

            it("should map from api to ui comparator", function (done) {
                expect(mapping.getUiComparator(_opennms.API.Comparators.EQ)).to.eql("=");
                expect(mapping.getUiComparator(_opennms.API.Comparators.NE)).to.eql("!=");
                expect(mapping.getUiComparator(_opennms.API.Comparators.GE)).to.eql(">=");
                expect(mapping.getUiComparator(_opennms.API.Comparators.LE)).to.eql("<=");
                expect(mapping.getUiComparator(_opennms.API.Comparators.GT)).to.eql(">");
                expect(mapping.getUiComparator(_opennms.API.Comparators.LT)).to.eql("<");

                done();
            });

            it("should NOT map from api to ui comparator", function (done) {
                expect(function () {
                    return mapping.getUiComparator(_opennms.API.Comparators.NULL);
                }).to.throw("No matching UI comparator found for '" + _opennms.API.Comparators.NULL.label + "'.");
                expect(function () {
                    return mapping.getUiComparator(_opennms.API.Comparators.NOTNULL);
                }).to.throw("No matching UI comparator found for '" + _opennms.API.Comparators.NOTNULL.label + "'.");
                expect(function () {
                    return mapping.getUiComparator(_opennms.API.Comparators.LIKE);
                }).to.throw("No matching UI comparator found for '" + _opennms.API.Comparators.LIKE.label + "'.");
                expect(function () {
                    return mapping.getUiComparator(_opennms.API.Comparators.ILIKE);
                }).to.throw("No matching UI comparator found for '" + _opennms.API.Comparators.ILIKE.label + "'.");

                done();
            });

            it("should map from ui to api comparator", function (done) {
                expect(mapping.getApiComparator(_UI.UI.Comparators.EQ)).to.eql(_opennms.API.Comparators.EQ);
                expect(mapping.getApiComparator(_UI.UI.Comparators.NEQ)).to.eql(_opennms.API.Comparators.NE);
                expect(mapping.getApiComparator(_UI.UI.Comparators.GE)).to.eql(_opennms.API.Comparators.GE);
                expect(mapping.getApiComparator(_UI.UI.Comparators.LE)).to.eql(_opennms.API.Comparators.LE);
                expect(mapping.getApiComparator(_UI.UI.Comparators.GT)).to.eql(_opennms.API.Comparators.GT);
                expect(mapping.getApiComparator(_UI.UI.Comparators.LT)).to.eql(_opennms.API.Comparators.LT);

                done();
            });
        });

        describe('OperatorMapping', function () {
            var mapping = new _Mapping.Mapping.OperatorMapping();

            it("should map from api to ui operator", function (done) {
                expect(mapping.getUiOperator(_opennms.API.Operators.AND)).to.eql("AND");
                expect(mapping.getUiOperator(_opennms.API.Operators.OR)).to.eql("OR");

                done();
            });

            it("should map from ui to api operator", function (done) {
                expect(mapping.getApiOperator(_UI.UI.Operators.AND)).to.eql(_opennms.API.Operators.AND);
                expect(mapping.getApiOperator(_UI.UI.Operators.OR)).to.eql(_opennms.API.Operators.OR);

                done();
            });
        });

        describe('RestrictionMapping', function () {
            var mapping = new _Mapping.Mapping.RestrictionMapping(uiSegmentSrv);

            it("should map from api restriction", function (done) {
                expect(mapping.getUiRestriction(new _opennms.API.Restriction("my-property", _opennms.API.Comparators.LE, 'some-value'))).to.eql(new _UI.UI.Restriction(uiSegmentSrv, new _UI.UI.RestrictionDTO('my-property', '<=', 'some-value')));
                done();
            });

            it("should map from api nested restriction", function (done) {
                var nestedRestriction = new _opennms.API.NestedRestriction().withOrRestriction(new _opennms.API.Restriction("my-property", _opennms.API.Comparators.LE, 'some-value')).withOrRestriction(new _opennms.API.Restriction("my-property", _opennms.API.Comparators.GE, 'some-other-value'));

                var actualUiQuery = mapping.getUiRestriction(nestedRestriction);

                var expectedUiQuery = new _UI.UI.Query(uiSegmentSrv);
                expectedUiQuery.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.OR, new _UI.UI.Restriction(uiSegmentSrv, new _UI.UI.RestrictionDTO("my-property", "<=", "some-value"))));
                expectedUiQuery.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.OR, new _UI.UI.Restriction(uiSegmentSrv, new _UI.UI.RestrictionDTO("my-property", ">=", "some-other-value"))));

                expect(actualUiQuery).to.eql(expectedUiQuery);

                done();
            });
        });

        describe('ClauseMapping', function () {
            var mapping = new _Mapping.Mapping.ClauseMapping(uiSegmentSrv);

            it('should ignore not initialized clauses (restrictionDTO is null)', function (done) {

                var clause = new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.Restriction(this.uiSegmentSrv));
                expect(mapping.getApiClause(clause)).to.eql(null);

                done();
            });
        });

        describe('FilterMapping', function () {

            var mapping = new _Mapping.Mapping.FilterMapping(uiSegmentSrv);

            it('should map from empty ui to api filter', function (done) {
                var apiFilter = new _opennms.API.Filter();
                apiFilter.limit = 0;
                expect(mapping.getApiFilter(new _UI.UI.Filter(uiSegmentSrv))).to.eql(apiFilter);

                done();
            });

            it('should map from api to ui filter and vice versa', function (done) {
                var apiFilter = new _opennms.API.Filter().withClause(new _opennms.API.Clause(new _opennms.API.Restriction("key", _opennms.API.Comparators.EQ, "value"), _opennms.API.Operators.OR)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction("key2", _opennms.API.Comparators.NE, "value2"), _opennms.API.Operators.AND));
                apiFilter.limit = 0;

                var uiFilter = new _UI.UI.Filter(uiSegmentSrv).withClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.OR, new _UI.UI.RestrictionDTO("key", "=", "value"))).withClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("key2", "!=", "value2")));

                expect(mapping.getUiFilter(apiFilter)).to.eql(uiFilter);
                expect(mapping.getApiFilter(uiFilter)).to.eql(apiFilter);

                done();
            });
        });
    });

    describe('FilterCloner', function () {

        var apiFilter = new _opennms.API.Filter().withClause(new _opennms.API.Clause(new _opennms.API.Restriction('key', _opennms.API.Comparators.EQ, 'value'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction('key2', _opennms.API.Comparators.EQ, 'value2'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction("key3", _opennms.API.Comparators.NE, "value3"), _opennms.API.Operators.OR)), _opennms.API.Operators.AND));

        it('should clone already initialized', function (done) {
            var otherFilter = new _FilterCloner.FilterCloner().cloneFilter(apiFilter);
            expect(apiFilter).to.eql(otherFilter);

            done();
        });

        it('should clone', function (done) {
            var jsonString = JSON.stringify(apiFilter);
            var object = JSON.parse(jsonString);
            expect(object).not.to.be.an.instanceof(_opennms.API.Filter);

            var filterObject = new _FilterCloner.FilterCloner().cloneFilter(object);
            expect(filterObject).to.be.an.instanceof(_opennms.API.Filter);
            expect(apiFilter).to.eql(filterObject);

            done();
        });
    });

    describe("UI.Query", function () {
        var query = void 0;

        beforeEach(function () {
            query = new _UI.UI.Filter(uiSegmentSrv).query;
        });

        it('should add new empty clause', function (done) {
            expect(query.clauses.length).to.eql(0);
            query.createNewEmptyClause();
            expect(query.clauses.length).to.eql(1);

            done();
        });

        it('should add new empty nested clause', function (done) {

            expect(query.clauses.length).to.eql(0);
            query.createNewEmptyNestedClause();
            expect(query.clauses.length).to.eql(1);

            expect(query.clauses[0].restriction.clauses.length).to.eql(1);

            done();
        });
    });

    describe("UI.Controls", function () {

        var uiFilter = void 0;

        beforeEach(function () {
            uiFilter = new _UI.UI.Filter(uiSegmentSrv);
        });

        describe('AddControl', function () {
            var control = new _UI.UI.Controls.AddControl();

            describe("filter", function () {
                it('always show, except for nested controls', function (done) {
                    expect(control.filter(uiFilter.query, new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.Restriction(uiSegmentSrv)))).to.eql(true);

                    expect(control.filter(uiFilter.query, new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.Query(uiSegmentSrv)))).to.eql(false);

                    done();
                });
            });

            describe("action", function () {

                it('should add new empty clause', function (done) {
                    var newClause = uiFilter.query.createNewEmptyClause();
                    expect(uiFilter.query.clauses.length).to.eql(1);

                    control.action(uiFilter.query, newClause);
                    expect(uiFilter.query.clauses.length).to.eql(2);

                    done();
                });
            });
        });

        describe('RemoveControl', function () {

            var control = new _UI.UI.Controls.RemoveControl();

            describe("filter", function () {
                it('do not show on first empty clause', function (done) {
                    uiFilter.query.createNewEmptyClause();

                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[0])).to.eql(false);

                    done();
                });

                it('show on nested and children of nested clause', function (done) {

                    uiFilter.query.createNewEmptyNestedClause();

                    expect(uiFilter.query.clauses.length).to.eql(1);
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[0])).to.eql(false); // no control on nested clause
                    expect(control.filter(uiFilter.query.clauses[0].restriction, uiFilter.query.clauses[0].restriction.clauses[0])).to.eql(true); // control on clause

                    done();
                });

                it('show on other clauses', function (done) {
                    uiFilter.query.createNewEmptyClause();
                    uiFilter.query.createNewEmptyClause();

                    _lodash2.default.each(uiFilter.query.clauses, function (clause) {
                        expect(control.filter(uiFilter.query, clause)).to.eql(true);
                    });

                    done();
                });
            });

            describe("action", function () {
                it('should remove clause', function (done) {
                    // add dummy clause
                    uiFilter.query.createNewEmptyClause();
                    expect(uiFilter.query.clauses.length).to.eql(1);

                    // perform action
                    control.action(uiFilter.query, uiFilter.query.clauses[0]);

                    // verify it was removed
                    expect(uiFilter.query.clauses.length).to.eql(0);

                    done();
                });

                it('should remove query from parent clause if last clause was removed', function (done) {
                    // dummy clause added yet
                    uiFilter.query.createNewEmptyClause();
                    expect(uiFilter.query.clauses.length).to.eql(1);

                    // add nested clause
                    var newQuery = uiFilter.query.createNewEmptyNestedClause();
                    expect(uiFilter.query.clauses.length).to.eql(2);
                    expect(newQuery.clauses.length).to.eql(1);

                    // perform action ...
                    control.action(newQuery, newQuery.clauses[0]);

                    // ... and verify that it was removed
                    expect(newQuery.clauses.length).to.eql(0);
                    expect(uiFilter.query.clauses.length).to.eql(1);

                    done();
                });
            });
        });

        describe('NestedControl', function () {
            var control = new _UI.UI.Controls.AddNestedControl();

            describe("filter", function () {
                it('show on all 1st level clauses, except nested clause', function (done) {
                    uiFilter.query.createNewEmptyClause();
                    uiFilter.query.createNewEmptyClause();
                    uiFilter.query.createNewEmptyClause();

                    _lodash2.default.each(uiFilter.query.clauses, function (clause) {
                        expect(control.filter(uiFilter.query, clause)).to.eql(true);
                    });

                    // Try nested
                    uiFilter.query.createNewEmptyNestedClause();
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[3])).to.eql(false);

                    done();
                });

                it('do not show on 2nd level clauses ', function (done) {
                    uiFilter.query.createNewEmptyNestedClause();
                    uiFilter.query.createNewEmptyClause();

                    var newQuery = uiFilter.query.clauses[0].restriction;
                    newQuery.createNewEmptyClause();
                    newQuery.createNewEmptyClause();
                    newQuery.createNewEmptyClause();

                    // verify 2nd level
                    _lodash2.default.each(newQuery.clauses, function (clause) {
                        expect(control.filter(newQuery, clause)).to.eql(false);
                    });

                    // verify 1st level
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[0])).to.eql(false);
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[1])).to.eql(true);

                    done();
                });
            });
        });
    });

    describe('UI.Filter', function () {
        var uiFilter = void 0;

        beforeEach(function () {
            uiFilter = new _UI.UI.Filter(uiSegmentSrv);
        });

        describe('addClause', function () {
            it('should allow adding a single restriction', function (done) {
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("severity", _UI.UI.Comparators.EQ, 'CLEARED')));
                expect(uiFilter.query.clauses).to.have.lengthOf(1);
                expect(uiFilter.query.clauses[0].restriction.segments).to.have.lengthOf(3);
                expect(uiFilter.query.clauses[0].restriction.segments[0].value).to.eql('severity');
                expect(uiFilter.query.clauses[0].restriction.segments[1].value).to.eql("=");
                expect(uiFilter.query.clauses[0].restriction.segments[2].value).to.eql('CLEARED');
                expect(uiFilter.query.clauses[0].operator.value).to.eql("AND");

                done();
            });

            it('should fail when unsupported type', function (done) {

                expect(function () {
                    return uiFilter.addClause("string");
                }).to.throw("Clause type is not supported");

                done();
            });
        });

        describe('removeClause', function () {
            var clause = new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("key", "=", "value"));

            it("should not remove non existing clause", function (done) {
                expect(uiFilter.query.clauses).to.have.lengthOf(0);
                uiFilter.withClause(clause);
                uiFilter.removeClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("x", "=", "0")));
                expect(uiFilter.query.clauses).to.have.lengthOf(1);

                done();
            });

            it("should remove existing clause", function (done) {
                expect(uiFilter.query.clauses).to.have.lengthOf(0);
                uiFilter.withClause(clause);
                expect(uiFilter.query.clauses).to.have.lengthOf(1);
                uiFilter.removeClause(clause);
                expect(uiFilter.query.clauses).to.have.lengthOf(0);

                done();
            });
        });

        describe('clear', function () {
            it('should reset query', function (done) {
                uiFilter.query.root = false; // make it pass
                expect(uiFilter.query).to.eql(new _UI.UI.Query(uiSegmentSrv));

                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("key", "=", "value")));
                expect(uiFilter.query).not.to.eql(new _UI.UI.Query(uiSegmentSrv));

                uiFilter.clear();
                expect(uiFilter.query).to.eql(new _UI.UI.Query(uiSegmentSrv));

                done();
            });
        });

        describe('getQueryString', function () {
            it('should work with empty clause', function (done) {
                expect(uiFilter.getQueryString()).to.eql("select all alarms");
                done();
            });

            it('should work with single clause', function (done) {
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO('severity', _UI.UI.Comparators.EQ, 'MINOR')));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where severity = 'MINOR'");
                done();
            });

            it('should not include not initialized clauses (restrictionDTO is not fully initialized)', function (done) {
                var expected = "select all alarms where severity >= 'WARNING'";

                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.OR, new _UI.UI.RestrictionDTO("severity", _UI.UI.Comparators.GE, 'WARNING')));
                expect(uiFilter.getQueryString()).to.eql(expected);

                // It does not have any attribute, comparator or value data (valid state), but should not be considered when generating the string
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.OR, new _UI.UI.Restriction(uiSegmentSrv)));
                expect(uiFilter.getQueryString()).to.eql(expected);

                done();
            });

            it('should handle null values', function (done) {
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("location", _UI.UI.Comparators.EQ, "null")));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where location is null");

                uiFilter.clear();
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO("location", _UI.UI.Comparators.NEQ, "null")));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where location is not null");

                done();
            });

            it('should work with multiple clauses', function (done) {
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO('severity', _UI.UI.Comparators.EQ, 'MINOR')));
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.OR, new _UI.UI.RestrictionDTO('severity', _UI.UI.Comparators.EQ, 'MAJOR')));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where severity = 'MINOR' or severity = 'MAJOR'");

                uiFilter.clear();
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO('severity', _UI.UI.Comparators.EQ, 'MINOR')));
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO('severity', _UI.UI.Comparators.EQ, 'MAJOR')));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where severity = 'MINOR' and severity = 'MAJOR'");

                done();
            });

            it('should work with nested clauses', function (done) {
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO('location', _UI.UI.Comparators.EQ, 'Stuttgart')));
                uiFilter.addClause(new _opennms.API.Clause(new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction('severity', _opennms.API.Comparators.GE, 'WARNING'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction('severity', _opennms.API.Comparators.LE, 'MAJOR'), _opennms.API.Operators.AND)), _opennms.API.Operators.OR));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where location = 'Stuttgart' or (severity >= 'WARNING' and severity <= 'MAJOR')");

                // let's try the other way around
                uiFilter.clear();
                uiFilter.addClause(new _opennms.API.Clause(new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction('severity', _opennms.API.Comparators.GE, 'WARNING'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction('severity', _opennms.API.Comparators.LE, 'MAJOR'), _opennms.API.Operators.AND)), _opennms.API.Operators.OR));
                uiFilter.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.RestrictionDTO('location', _UI.UI.Comparators.EQ, 'Stuttgart')));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where (severity >= 'WARNING' and severity <= 'MAJOR') and location = 'Stuttgart'");

                // let's try 2 nested restrictions
                uiFilter.clear();
                uiFilter.addClause(new _opennms.API.Clause(new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction('location', _opennms.API.Comparators.EQ, 'Stuttgart'), _opennms.API.Operators.OR)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction('location', _opennms.API.Comparators.EQ, 'Fulda'), _opennms.API.Operators.OR)), _opennms.API.Operators.AND));
                uiFilter.addClause(new _opennms.API.Clause(new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction('severity', _opennms.API.Comparators.GE, 'WARNING'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction('severity', _opennms.API.Comparators.LE, 'MAJOR'), _opennms.API.Operators.AND)), _opennms.API.Operators.AND));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where (location = 'Stuttgart' or location = 'Fulda') and (severity >= 'WARNING' and severity <= 'MAJOR')");

                done();
            });

            it('should handle deep nested clauses', function (done) {
                var nestedRestriction = new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction("severity", _opennms.API.Comparators.GE, 'WARNING'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.Restriction("severity", _opennms.API.Comparators.LE, 'MAJOR'), _opennms.API.Operators.AND)).withClause(new _opennms.API.Clause(new _opennms.API.NestedRestriction().withClause(new _opennms.API.Clause(new _opennms.API.Restriction("location", _opennms.API.Comparators.EQ, "Fulda"), _opennms.API.Operators.OR)), _opennms.API.Operators.OR), _opennms.API.Operators.OR);

                uiFilter.addClause(new _opennms.API.Clause(nestedRestriction, _opennms.API.Operators.OR));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where (severity >= 'WARNING' and severity <= 'MAJOR' or (location = 'Fulda'))");

                done();
            });

            it('should render real nested clauses correctly', function (done) {
                // Dummy clause should not influence the query
                uiFilter.query.createNewEmptyNestedClause();
                expect(uiFilter.getQueryString()).to.eql("select all alarms");

                // update the values
                var query = uiFilter.query.clauses[0].restriction;
                query.clauses[0].restriction.setAttribute("key");
                query.clauses[0].restriction.setComparator("=");
                query.clauses[0].restriction.setValue("value");

                // should now be influenced
                expect(uiFilter.getQueryString()).to.eql("select all alarms where (key = 'value')");

                done();
            });
        });

        describe("updateControls", function () {

            var verifyNoControls = function verifyNoControls(query) {
                _lodash2.default.each(query.clauses, function (clause) {
                    expect(clause.controls.length).to.eql(0);
                });
            };

            var verifyFullControls = function verifyFullControls(clause) {
                verifyControls(clause, [_UI.UI.Controls.RemoveControl, _UI.UI.Controls.AddControl, _UI.UI.Controls.AddNestedControl]);
            };

            var verifyControls = function verifyControls(clause) {
                var controls = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];

                expect(clause.controls.length).to.eql(controls.length); // add, add nested and remove
                if (controls.length > 0) {
                    _lodash2.default.each(controls, function (control, index) {
                        expect(clause.controls[index]).to.be.an.instanceof(control);
                    });
                }
            };

            it('should create controls for add and add nested', function (done) {
                verifyNoControls(uiFilter.query);
                expect(uiFilter.query.clauses.length).to.eql(0);

                // Update controls
                uiFilter.updateControls();
                expect(uiFilter.query.clauses.length).to.eql(1); // dummy row

                // now the controls should be there
                _lodash2.default.each(uiFilter.query.clauses, function (clause) {
                    verifyControls(clause, [_UI.UI.Controls.AddControl, _UI.UI.Controls.AddNestedControl]);
                });

                done();
            });

            it('should create controls for add, add nested and remove', function (done) {
                verifyNoControls(uiFilter.query);

                uiFilter.query.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.Restriction(uiSegmentSrv, new _UI.UI.RestrictionDTO("key", "=", "value"))));
                uiFilter.updateControls();

                expect(uiFilter.query.clauses.length).to.eql(1);
                _lodash2.default.each(uiFilter.query.clauses, function (clause) {
                    verifyFullControls(clause);
                });

                done();
            });

            it('should not add nested controls on level 2', function (done) {
                verifyNoControls(uiFilter.query);

                uiFilter.query.addClause(new _UI.UI.Clause(uiSegmentSrv, _UI.UI.Operators.AND, new _UI.UI.Restriction(uiSegmentSrv, new _UI.UI.RestrictionDTO("key", "=", "value"))));
                uiFilter.query.createNewEmptyNestedClause();
                uiFilter.updateControls();

                expect(uiFilter.query.clauses.length).to.eql(2);
                expect(uiFilter.query.clauses[1].restriction.clauses.length).to.eql(1);
                verifyFullControls(uiFilter.query.clauses[0]); // all controls on simple clause
                verifyControls(uiFilter.query.clauses[1], []); // no controls on nested clause
                verifyControls(uiFilter.query.clauses[1].restriction.clauses[0], [_UI.UI.Controls.RemoveControl, _UI.UI.Controls.AddControl]); // limited controls on clause of nested clause

                done();
            });
        });
    });
});
//# sourceMappingURL=fault_ds_datasource_spec.js.map
