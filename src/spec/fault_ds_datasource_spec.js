import Q from "q";
import _ from 'lodash';
import {UI} from '../datasources/fault-ds/UI';
import {API} from '../opennms';
import {Mapping} from '../datasources/fault-ds/Mapping';
import {FilterInitializer} from '../datasources/fault-ds/client_delegate';

describe("OpenNMS_FaultManagement_Datasource", function() {
    let uiSegmentSrv = {
        newSegment: function (value, type) {
            return {value: value, type: type};
        },
        newKey: function (key) {
            return this.newSegment(key, 'key');
        },
        newOperator: function (operator) {
            return this.newSegment(operator, 'operator');
        },
        newFake: function (text, type, cssClass) {
            let segment = this.newSegment(text, type);
            segment.fake = true;
            return segment;
        },
        newPlusButton: function () {
            return this.newFake('', 'plus-button');
        },
        newKeyValue: function (value) {
            return this.newSegment(value, 'value');
        },
        newCondition: function (condition) {
            return this.newSegment(condition, 'condition');
        }
    };

    describe('Mapping', function () {
        describe('ComparatorMapping', function () {
            let mapping = new Mapping.ComparatorMapping();

            it("should map from api to ui comparator", function (done) {
                expect(mapping.getUiComparator(API.Comparators.EQ)).to.eql("=");
                expect(mapping.getUiComparator(API.Comparators.NE)).to.eql("!=");
                expect(mapping.getUiComparator(API.Comparators.GE)).to.eql(">=");
                expect(mapping.getUiComparator(API.Comparators.LE)).to.eql("<=");
                expect(mapping.getUiComparator(API.Comparators.GT)).to.eql(">");
                expect(mapping.getUiComparator(API.Comparators.LT)).to.eql("<");

                done();
            });

            it("should NOT map from api to ui comparator", function(done) {
                expect(() => mapping.getUiComparator(API.Comparators.NULL)).to.throw("No matching UI comparator found for '" + API.Comparators.NULL.label + "'.");
                expect(() => mapping.getUiComparator(API.Comparators.NOTNULL)).to.throw("No matching UI comparator found for '" + API.Comparators.NOTNULL.label + "'.");
                expect(() => mapping.getUiComparator(API.Comparators.LIKE)).to.throw("No matching UI comparator found for '" + API.Comparators.LIKE.label + "'.");
                expect(() => mapping.getUiComparator(API.Comparators.ILIKE)).to.throw("No matching UI comparator found for '" + API.Comparators.ILIKE.label + "'.");

                done();
            });

            it("should map from ui to api comparator", function (done) {
                expect(mapping.getApiComparator(UI.Comparators.EQ)).to.eql(API.Comparators.EQ);
                expect(mapping.getApiComparator(UI.Comparators.NEQ)).to.eql(API.Comparators.NE);
                expect(mapping.getApiComparator(UI.Comparators.GE)).to.eql(API.Comparators.GE);
                expect(mapping.getApiComparator(UI.Comparators.LE)).to.eql(API.Comparators.LE);
                expect(mapping.getApiComparator(UI.Comparators.GT)).to.eql(API.Comparators.GT);
                expect(mapping.getApiComparator(UI.Comparators.LT)).to.eql(API.Comparators.LT);

                done();
            });
        });

        describe('OperatorMapping', function () {
            let mapping = new Mapping.OperatorMapping();

            it("should map from api to ui operator", function (done) {
                expect(mapping.getUiOperator(API.Operators.AND)).to.eql("AND");
                expect(mapping.getUiOperator(API.Operators.OR)).to.eql("OR");

                done();
            });

            it("should map from ui to api operator", function(done) {
                expect(mapping.getApiOperator(UI.Operators.AND)).to.eql(API.Operators.AND);
                expect(mapping.getApiOperator(UI.Operators.OR)).to.eql(API.Operators.OR);

                done();
            })
        });

        describe('RestrictionMapping', function () {
            let mapping = new Mapping.RestrictionMapping(uiSegmentSrv);

            it("should map from api restriction", function (done) {
                expect(mapping.getUiRestriction(new API.Restriction("my-property", API.Comparators.LE, 'some-value')))
                    .to.eql(new UI.Restriction(uiSegmentSrv, new UI.RestrictionDTO('my-property', '<=', 'some-value')));
                done();
            });

            it("should map from api nested restriction", function (done) {
                const nestedRestriction = new API.NestedRestriction()
                    .withOrRestriction(new API.Restriction("my-property", API.Comparators.LE, 'some-value'))
                    .withOrRestriction(new API.Restriction("my-property", API.Comparators.GE, 'some-other-value'));

                const actualUiQuery = mapping.getUiRestriction(nestedRestriction);

                const expectedUiQuery = new UI.Query(uiSegmentSrv);
                expectedUiQuery.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.Restriction(uiSegmentSrv, new UI.RestrictionDTO("my-property", "<=", "some-value"))));
                expectedUiQuery.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.Restriction(uiSegmentSrv, new UI.RestrictionDTO("my-property", ">=", "some-other-value"))));

                expect(actualUiQuery).to.eql(expectedUiQuery);

                done();
            });

        });

        describe('ClauseMapping', function() {
            let mapping = new Mapping.ClauseMapping(uiSegmentSrv);

           it ('should ignore not initialized clauses (restrictionDTO is null)', function(done) {

               let clause = new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv));
               expect(mapping.getApiClause(clause)).to.eql(null);

               done();
           }) ;
        });

        describe('FilterMapping', function() {

            let mapping = new Mapping.FilterMapping(uiSegmentSrv);

            it ('should map from empty ui to api filter', function(done) {
                let apiFilter = new API.Filter();
                apiFilter.limit = 0;
                expect(mapping.getApiFilter(new UI.Filter(uiSegmentSrv))).to.eql(apiFilter);

                done();
            });

            it ('should map from api to ui filter and vice versa', function(done) {
                const apiFilter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction("key", API.Comparators.EQ, "value"), API.Operators.OR))
                    .withClause(new API.Clause(new API.Restriction("key2", API.Comparators.NE, "value2"), API.Operators.AND));
                apiFilter.limit = 0;

                const uiFilter = new UI.Filter(uiSegmentSrv)
                    .withClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.RestrictionDTO("key", "=", "value")))
                    .withClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("key2", "!=", "value2")));

                expect(mapping.getUiFilter(apiFilter)).to.eql(uiFilter);
                expect(mapping.getApiFilter(uiFilter)).to.eql(apiFilter);

                done();
            });
        });

    });

    describe('FilterInitializer', function() {

        let apiFilter = new API.Filter()
            .withClause(new API.Clause(new API.Restriction('key', API.Comparators.EQ, 'value'), API.Operators.AND))
            .withClause(new API.Clause(new API.Restriction('key2', API.Comparators.EQ, 'value2'), API.Operators.AND))
            .withClause(new API.Clause(new API.NestedRestriction()
                .withClause(new API.Clause(new API.Restriction("key3", API.Comparators.NE, "value3"), API.Operators.OR)), API.Operators.AND));

        it('should initialize already initialized', function(done) {
            const otherFilter = new FilterInitializer().createFilter(apiFilter);
            expect(apiFilter).to.eql(otherFilter);

            done();
        });

        it('should initialize', function(done) {
            const jsonString = JSON.stringify(apiFilter);
            const object = JSON.parse(jsonString);
            expect(object).not.to.be.an.instanceof(API.Filter);

            const filterObject = new FilterInitializer().createFilter(object);
            expect(filterObject).to.be.an.instanceof(API.Filter);
            expect(apiFilter).to.eql(filterObject);

            done();
        });


    });

    describe("UI.Query", function() {
        let query;

        beforeEach(function () {
            query = new UI.Filter(uiSegmentSrv).query;
        });

        it('should add new empty clause', function(done) {
            expect(query.clauses.length).to.eql(0);
            query.createNewEmptyClause();
            expect(query.clauses.length).to.eql(1);

            done();
        });

        it('should add new empty nested clause', function(done) {

            expect(query.clauses.length).to.eql(0);
            query.createNewEmptyNestedClause();
            expect(query.clauses.length).to.eql(1);

            expect(query.clauses[0].restriction.clauses.length).to.eql(1);

            done();
        });
    });

    describe("UI.Controls", function() {

        let uiFilter;

        beforeEach(function() {
            uiFilter = new UI.Filter(uiSegmentSrv);
        });

        describe('AddControl', function() {
            let control = new UI.Controls.AddControl();

            describe("filter", function() {
                it('always show, except for nested controls', function(done) {
                    expect(control.filter(uiFilter.query, new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Restriction(uiSegmentSrv)))).to.eql(true);

                    expect(control.filter(uiFilter.query, new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Query(uiSegmentSrv)))).to.eql(false);

                    done();
                })
            });

            describe("action", function() {

              it ('should add new empty clause', function(done) {
                  const newClause = uiFilter.query.createNewEmptyClause();
                  expect(uiFilter.query.clauses.length).to.eql(1);

                  control.action(uiFilter.query, newClause);
                  expect(uiFilter.query.clauses.length).to.eql(2);

                  done();
              });
            });

        });

        describe('RemoveControl', function() {

            let control = new UI.Controls.RemoveControl();

            describe("filter", function() {
                it('do not show on first empty clause', function(done) {
                    uiFilter.query.createNewEmptyClause();

                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[0])).to.eql(false);

                    done();
                });

                it('show on nested and children of nested clause', function(done) {

                    uiFilter.query.createNewEmptyNestedClause();

                    expect(uiFilter.query.clauses.length).to.eql(1);
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[0])).to.eql(false); // no control on nested clause
                    expect(control.filter(uiFilter.query.clauses[0].restriction, uiFilter.query.clauses[0].restriction.clauses[0])).to.eql(true); // control on clause

                    done();
                });

                it('show on other clauses', function(done) {
                    uiFilter.query.createNewEmptyClause();
                    uiFilter.query.createNewEmptyClause();

                    _.each(uiFilter.query.clauses, clause => {
                        expect(control.filter(uiFilter.query, clause)).to.eql(true);
                    });

                    done();
                });
            });


            describe("action", function() {
                it ('should remove clause', function(done) {
                    // add dummy clause
                    uiFilter.query.createNewEmptyClause();
                    expect(uiFilter.query.clauses.length).to.eql(1);

                    // perform action
                    control.action(uiFilter.query, uiFilter.query.clauses[0]);

                    // verify it was removed
                    expect(uiFilter.query.clauses.length).to.eql(0);

                    done();
                });

                it ('should remove query from parent clause if last clause was removed', function(done) {
                    // dummy clause added yet
                    uiFilter.query.createNewEmptyClause();
                    expect(uiFilter.query.clauses.length).to.eql(1);

                    // add nested clause
                    const newQuery = uiFilter.query.createNewEmptyNestedClause();
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

        describe('NestedControl', function() {
            let control = new UI.Controls.AddNestedControl();

            describe("filter", function() {
                it('show on all 1st level clauses, except nested clause', function(done) {
                    uiFilter.query.createNewEmptyClause();
                    uiFilter.query.createNewEmptyClause();
                    uiFilter.query.createNewEmptyClause();

                    _.each(uiFilter.query.clauses, clause => {
                        expect(control.filter(uiFilter.query, clause)).to.eql(true);
                    });

                    // Try nested
                    uiFilter.query.createNewEmptyNestedClause();
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[3])).to.eql(false);

                    done();
                });

                it('do not show on 2nd level clauses ', function(done) {
                    uiFilter.query.createNewEmptyNestedClause();
                    uiFilter.query.createNewEmptyClause();

                    const newQuery = uiFilter.query.clauses[0].restriction;
                    newQuery.createNewEmptyClause();
                    newQuery.createNewEmptyClause();
                    newQuery.createNewEmptyClause();

                    // verify 2nd level
                    _.each(newQuery.clauses, clause => {
                        expect(control.filter(newQuery, clause)).to.eql(false);
                    });

                    // verify 1st level
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[0])).to.eql(false);
                    expect(control.filter(uiFilter.query, uiFilter.query.clauses[1])).to.eql(true);


                    done();
                })
            });
        });
    });

    describe('UI.Filter', function () {
        let uiFilter;

        beforeEach(function () {
            uiFilter = new UI.Filter(uiSegmentSrv);
        });

        describe('addClause', function () {
            it('should allow adding a single restriction', function (done) {
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("severity", UI.Comparators.EQ, 'CLEARED')));
                expect(uiFilter.query.clauses).to.have.lengthOf(1);
                expect(uiFilter.query.clauses[0].restriction.segments).to.have.lengthOf(3);
                expect(uiFilter.query.clauses[0].restriction.segments[0].value).to.eql('severity');
                expect(uiFilter.query.clauses[0].restriction.segments[1].value).to.eql("=");
                expect(uiFilter.query.clauses[0].restriction.segments[2].value).to.eql('CLEARED');
                expect(uiFilter.query.clauses[0].operator.value).to.eql("AND");

                done();
            });

            it('should fail when unsupported type', function (done) {

                expect(() => uiFilter.addClause("string")).to.throw("Clause type is not supported");

                done();
            })
        });

        describe('removeClause', function() {
            const clause = new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("key", "=", "value"));

            it("should not remove non existing clause", function(done) {
                expect(uiFilter.query.clauses).to.have.lengthOf(0);
                uiFilter.withClause(clause);
                uiFilter.removeClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("x", "=", "0")));
                expect(uiFilter.query.clauses).to.have.lengthOf(1);

                done();
            });

            it("should remove existing clause", function(done) {
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
                expect(uiFilter.query).to.eql(new UI.Query(uiSegmentSrv));

                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("key", "=", "value")));
                expect(uiFilter.query).not.to.eql(new UI.Query(uiSegmentSrv));

                uiFilter.clear();
                expect(uiFilter.query).to.eql(new UI.Query(uiSegmentSrv));

                done();
            });
        });

        describe('getQueryString', function () {
            it('should work with empty clause', function (done) {
                expect(uiFilter.getQueryString()).to.eql("select all alarms");
                done();
            });

            it('should work with single clause', function (done) {
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO('severity', UI.Comparators.EQ, 'MINOR')));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where severity = 'MINOR'");
                done();
            });

            it('should not include not initialized clauses (restrictionDTO is not fully initialized)', function(done) {
                const expected = "select all alarms where severity >= 'WARNING'";

                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.RestrictionDTO("severity", UI.Comparators.GE, 'WARNING')));
                expect(uiFilter.getQueryString()).to.eql(expected);

                // It does not have any attribute, comparator or value data (valid state), but should not be considered when generating the string
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.Restriction(uiSegmentSrv)));
                expect(uiFilter.getQueryString()).to.eql(expected);

                done();
            });


            it('should handle null values', function (done) {
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("location", UI.Comparators.EQ, "null")));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where location is null");

                uiFilter.clear();
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("location", UI.Comparators.NEQ, "null")));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where location is not null");

                done();
            });

            it('should work with multiple clauses', function (done) {
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO('severity', UI.Comparators.EQ, 'MINOR')));
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.RestrictionDTO('severity', UI.Comparators.EQ, 'MAJOR')));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where severity = 'MINOR' or severity = 'MAJOR'");

                uiFilter.clear();
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO('severity', UI.Comparators.EQ, 'MINOR')));
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO('severity', UI.Comparators.EQ, 'MAJOR')));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where severity = 'MINOR' and severity = 'MAJOR'");

                done();
            });

            it('should work with nested clauses', function (done) {
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO('location', UI.Comparators.EQ, 'Stuttgart')));
                uiFilter.addClause(new API.Clause(new API.NestedRestriction()
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.GE, 'WARNING'), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.LE, 'MAJOR'), API.Operators.AND)), API.Operators.OR));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where location = 'Stuttgart' or (severity >= 'WARNING' and severity <= 'MAJOR')");


                // let's try the other way around
                uiFilter.clear();
                uiFilter.addClause(new API.Clause(new API.NestedRestriction()
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.GE, 'WARNING'), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.LE, 'MAJOR'), API.Operators.AND)), API.Operators.OR));
                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO('location', UI.Comparators.EQ, 'Stuttgart')));
                expect(uiFilter.getQueryString()).to.eql("select all alarms where (severity >= 'WARNING' and severity <= 'MAJOR') and location = 'Stuttgart'");

                // let's try 2 nested restrictions
                uiFilter.clear();
                uiFilter.addClause(new API.Clause(new API.NestedRestriction()
                    .withClause(new API.Clause(new API.Restriction('location', API.Comparators.EQ, 'Stuttgart'), API.Operators.OR))
                    .withClause(new API.Clause(new API.Restriction('location', API.Comparators.EQ, 'Fulda'), API.Operators.OR)), API.Operators.AND)
                );
                uiFilter.addClause(new API.Clause(new API.NestedRestriction()
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.GE, 'WARNING'), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.LE, 'MAJOR'), API.Operators.AND)), API.Operators.AND)
                );
                expect(uiFilter.getQueryString()).to.eql("select all alarms where (location = 'Stuttgart' or location = 'Fulda') and (severity >= 'WARNING' and severity <= 'MAJOR')");

                done();
            });

            it('should handle deep nested clauses', function (done) {
                const nestedRestriction = new API.NestedRestriction()
                    .withClause(new API.Clause(new API.Restriction("severity", API.Comparators.GE, 'WARNING'), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction("severity", API.Comparators.LE, 'MAJOR'), API.Operators.AND))
                    .withClause(new API.Clause(new API.NestedRestriction()
                        .withClause(new API.Clause(new API.Restriction("location", API.Comparators.EQ, "Fulda"), API.Operators.OR)), API.Operators.OR), API.Operators.OR);

                uiFilter.addClause(new API.Clause(nestedRestriction, API.Operators.OR));

                expect(uiFilter.getQueryString()).to.eql("select all alarms where (severity >= 'WARNING' and severity <= 'MAJOR' or (location = 'Fulda'))");

                done();
            });

            it('should render real nested clauses correctly', function(done) {
                // Dummy clause should not influence the query
                uiFilter.query.createNewEmptyNestedClause();
                expect(uiFilter.getQueryString()).to.eql("select all alarms");

                // update the values
                const query = uiFilter.query.clauses[0].restriction;
                query.clauses[0].restriction.setAttribute("key");
                query.clauses[0].restriction.setComparator("=");
                query.clauses[0].restriction.setValue("value");

                // should now be influenced
                expect(uiFilter.getQueryString()).to.eql("select all alarms where (key = 'value')");

                done();
            });
        });

        describe("updateControls", function() {

            const verifyNoControls = function(query) {
                _.each(query.clauses, clause => {
                    expect(clause.controls.length).to.eql(0);
                });
            };

            const verifyFullControls = function(clause) {
                verifyControls(clause, [UI.Controls.RemoveControl, UI.Controls.AddControl, UI.Controls.AddNestedControl]);
            };

            const verifyControls = function(clause, controls = []) {
                expect(clause.controls.length).to.eql(controls.length); // add, add nested and remove
                if (controls.length > 0) {
                    _.each(controls, (control, index) => {
                       expect(clause.controls[index]).to.be.an.instanceof(control)
                    });
                }
            };

            it ('should create controls for add and add nested', function(done) {
                verifyNoControls(uiFilter.query);
                expect(uiFilter.query.clauses.length).to.eql(0);

                // Update controls
                uiFilter.updateControls();
                expect(uiFilter.query.clauses.length).to.eql(1); // dummy row

                // now the controls should be there
                _.each(uiFilter.query.clauses, clause => {
                    verifyControls(clause, [UI.Controls.AddControl, UI.Controls.AddNestedControl]);
                });

                done();
            });

            it ('should create controls for add, add nested and remove', function(done) {
                verifyNoControls(uiFilter.query);

                uiFilter.query.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Restriction(uiSegmentSrv, new UI.RestrictionDTO("key", "=", "value"))));
                uiFilter.updateControls();

                expect(uiFilter.query.clauses.length).to.eql(1);
                _.each(uiFilter.query.clauses, clause => {
                    verifyFullControls(clause);
                });

                done();

            });

            it ('should not add nested controls on level 2', function(done) {
                verifyNoControls(uiFilter.query);

                uiFilter.query.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Restriction(uiSegmentSrv, new UI.RestrictionDTO("key", "=", "value"))));
                uiFilter.query.createNewEmptyNestedClause();
                uiFilter.updateControls();

                expect(uiFilter.query.clauses.length).to.eql(2);
                expect(uiFilter.query.clauses[1].restriction.clauses.length).to.eql(1);
                verifyFullControls(uiFilter.query.clauses[0]); // all controls on simple clause
                verifyControls(uiFilter.query.clauses[1], [ ]); // no controls on nested clause
                verifyControls(uiFilter.query.clauses[1].restriction.clauses[0], [ UI.Controls.RemoveControl, UI.Controls.AddControl ]); // limited controls on clause of nested clause

                done();
            });
        })

    });

});