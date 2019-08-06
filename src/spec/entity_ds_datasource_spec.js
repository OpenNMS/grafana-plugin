import Q from "q";
import _ from 'lodash';
import moment from 'moment';
import {UI} from '../datasources/entity-ds/UI';
import {API} from 'opennms';
import {Mapping} from '../datasources/entity-ds/Mapping';
import AlarmEntity from '../datasources/entity-ds/AlarmEntity';
import {OpenNMSEntityDatasource as Datasource, entityTypes} from '../datasources/entity-ds/datasource';
import {ClientDelegate} from '../lib/client_delegate';

import {TemplateSrv} from './template_srv';

import { KEY_PLACEHOLDER, VALUE_PLACEHOLDER } from '../datasources/entity-ds/constants';

describe("OpenNMS_Entity_Datasource", function() {
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
        newFake: function (text, type /*, cssClass */) {
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
            });
        });

        describe('RestrictionMapping', function () {
            const alarmEntity = new AlarmEntity(undefined, {name: 'OpenNMS Entity Datasource'});
            let mapping = new Mapping.RestrictionMapping(uiSegmentSrv, alarmEntity);

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
            const alarmEntity = new AlarmEntity(undefined, {name: 'OpenNMS Entity Datasource'});
            let mapping = new Mapping.ClauseMapping(uiSegmentSrv, alarmEntity);

           it ('should ignore not initialized clauses (restrictionDTO is null)', function(done) {

               let clause = new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Restriction(this.uiSegmentSrv));
               expect(mapping.getApiClause(clause)).to.eql(null);

               done();
           }) ;
        });

        describe('FilterMapping', function() {
            const alarmEntity = new AlarmEntity(undefined, {name: 'OpenNMS Entity Datasource'});
            let mapping = new Mapping.FilterMapping(uiSegmentSrv, alarmEntity);

            it ('should map from empty ui to api filter', function(done) {
                let apiFilter = new API.Filter();
                apiFilter.limit = 0;
                expect(mapping.getApiFilter(new UI.Filter(uiSegmentSrv, alarmEntity))).to.eql(apiFilter);

                done();
            });

            it ('should map from api to ui filter and vice versa', function(done) {
                const apiFilter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction("key", API.Comparators.EQ, "value"), API.Operators.OR))
                    .withClause(new API.Clause(new API.Restriction("key2", API.Comparators.NE, "value2"), API.Operators.AND));
                apiFilter.limit = 0;

                const uiFilter = new UI.Filter(uiSegmentSrv, alarmEntity)
                    .withClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.RestrictionDTO("key", "=", "value")))
                    .withClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("key2", "!=", "value2")));

                expect(mapping.getUiFilter(apiFilter)).to.eql(uiFilter);
                expect(mapping.getApiFilter(uiFilter)).to.eql(apiFilter);

                done();
            });

            it('should map from api to ui filter with nested restrictions when serialized and deserialized again', () => {
                // Create the filter
                const apiFilter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction("alarmAckUser", API.Comparators.EQ, "Administrator"), API.Operators.AND))
                    .withClause(new API.Clause(
                        new API.NestedRestriction()
                            .withClause(new API.Clause(new API.Restriction("severity", API.Comparators.GE, "WARNING"), API.Operators.AND)),
                        API.Operators.AND));

                // Simulate persisting and reloading
                const serialized = JSON.stringify(apiFilter);
                const deserialized = JSON.parse(serialized);
                const cloned = API.Filter.fromJson(deserialized);

                // Now try to map it to an ui filter
                const uiFilter = mapping.getUiFilter(cloned);
                expect(uiFilter.getQueryString()).to.eql("select all alarms where alarmAckUser = 'Administrator' and (severity >= 'WARNING')");
            });
        });

    });

    describe("UI.Restriction", function() {
        // See HELM-25
        it('should only convert to DTO when fully defined', () => {
            // Should be null when not initialized
            const restriction = new UI.Restriction(uiSegmentSrv);
            expect(restriction.asRestrictionDTO()).to.eql(null);

            // Should be null when initialized with defaults
            restriction.setAttribute(KEY_PLACEHOLDER);
            restriction.setComparator("=");
            restriction.setValue(VALUE_PLACEHOLDER);

            // Should be null for all other Comparators
            Object.keys(UI.Comparators).forEach(key => {
                restriction.setComparator(UI.Comparators[key]);
                expect(restriction.asRestrictionDTO()).to.eql(null);
            });

            // Should be null if value is set
            restriction.setValue("my value");
            expect(restriction.asRestrictionDTO()).to.eql(null);

            // Should be null if attribute is set
            restriction.setValue(VALUE_PLACEHOLDER);
            restriction.setAttribute("my attribute");
            expect(restriction.asRestrictionDTO()).to.eql(null);

            // should not be null if attribute and value is set
            restriction.setAttribute("my attribute");
            restriction.setComparator("=");
            restriction.setValue("my value");
            expect(restriction.asRestrictionDTO()).not.to.eql(null);
            expect(restriction.asRestrictionDTO()).to.eql(new UI.RestrictionDTO("my attribute", "=", "my value"));
        });
    });

    describe("UI.Query", function() {
        let query;

        beforeEach(function () {
            const alarmEntity = new AlarmEntity(undefined, {name: 'OpenNMS Entity Datasource'});
            query = new UI.Filter(uiSegmentSrv, alarmEntity).query;
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
            const alarmEntity = new AlarmEntity(undefined, {name: 'OpenNMS Entity Datasource'});
            uiFilter = new UI.Filter(uiSegmentSrv, alarmEntity);
        });

        describe('AddControl', function() {
            let control = new UI.Controls.AddControl();

            describe("filter", function() {
                it('always show, except for nested controls', function(done) {
                    expect(control.filter(uiFilter.query, new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Restriction(uiSegmentSrv)))).to.eql(true);

                    expect(control.filter(uiFilter.query, new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.Query(uiSegmentSrv)))).to.eql(false);

                    done();
                });
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
                });
            });
        });
    });

    describe('UI.Filter', function () {
        let uiFilter;

        beforeEach(function () {
            const alarmEntity = new AlarmEntity(undefined, {name: 'OpenNMS Entity Datasource'});
            uiFilter = new UI.Filter(uiSegmentSrv, alarmEntity);
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
            });
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
                       expect(clause.controls[index]).to.be.an.instanceof(control);
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
        });
    });

    describe('ClientDelegate', () => {
       let ctx = {};

       beforeEach(() => {
            ctx.backendSrv = {};
            ctx.$q = Q;
            ctx.settings = {
                type: "opennms-entity",
                name: "dummy-name",
                url: "http://localhost:8980/opennms"
            };
       });

       it('should not throw an exception when supported version is used', (done) => {
           // All requests assume the /rest/info call
           ctx.backendSrv.datasourceRequest = function(request) {
               return ctx.$q.when({
                   _request: request,
                   status: 200,
                   headers: {
                       'content-type': 'application/json'
                   },
                   data: {'packageDescription':'OpenNMS Meridian','displayVersion':'2017.1.0','packageName':'meridian','version':'2017.1.0'}
               });
           };

           // Instantiate and try to do any operation on the delegate
           const delegate = new ClientDelegate(ctx.settings, ctx.backendSrv, ctx.$q);
           delegate.getClientWithMetadata().then(() => {
               done();
           });
       });

       it('should throw exception when unsupported version is used', (done) => {
           // All requests assume the /rest/info call
           ctx.backendSrv.datasourceRequest = function(request) {
               return ctx.$q.when({
                    _request: request,
                   status: 200,
                   headers: {
                        'content-type': 'application/json'
                   },
                   data: API.OnmsResult.ok({'packageDescription':'OpenNMS','displayVersion':'19.1.0','packageName':'opennms','version':'19.1.0'})
                });
           };

           // Instantiate and try to do any operation on the delegate
           const delegate = new ClientDelegate(ctx.settings, ctx.backendSrv, ctx.$q);
           delegate.getClientWithMetadata().catch(err => {
               expect(err.message).to.eql("Unsupported Version");
               done();
           });
       });
    });

    describe('Datasource', () => {
        let ctx = {};

        const defaultSettings = {
            "type": "opennms-entity",
            "url": "http://localhost:8980/opennms",
            "name": "OpenNMS Entity Datasource"
        };

        const createDatasource = function(settings, ctx) {
            ctx.datasource = new Datasource(settings, ctx.$q, ctx.backendSrv, ctx.templateSrv, ctx.contextSrv, ctx.dashboardSrv);
            return ctx.datasource;
        };

        beforeEach(() => {
            // Context initialization
            ctx.$q = Q;
            ctx.backendSrv = {};
            ctx.templateSrv = new TemplateSrv();
            ctx.uiSegmentSrv = uiSegmentSrv;
            ctx.contextSrv = {user: {login: "admin", email: "admin@opennms.org", name:"The Administrator"}};
            ctx.dashboardSrv = {
                dashboard: {
                    panels: []
                }
            }
            ctx.dashboardSrv.getCurrent = function() {
                return ctx.dashboardSrv.dashboard;
            }
            ctx.range_from = moment();
            ctx.range_to = ctx.range_from.add(1, 'days');
            createDatasource(defaultSettings, ctx);
        });

        describe('user field', () => {
           it('should not be instantiated by default', () => {
               expect(ctx.datasource.user).to.be.undefined;
           });

           it('should be ignored if useGrafanaUser is false', () => {
               const settings = Object.assign({}, defaultSettings);
               settings.jsonData = {
                   useGrafanaUser: false,
                   grafanaUserField: 'email'
               };
               expect(createDatasource(settings, ctx).user).to.be.undefined;
           });

           it('should be login if undefined', () => {
                const settings = Object.assign({}, defaultSettings);
                settings.jsonData = {
                   useGrafanaUser: true,
                };
                expect(createDatasource(settings, ctx).user).to.equal("admin");
           });

           it('should be login if defined', () => {
               const settings = Object.assign({}, defaultSettings);
               settings.jsonData = {
                   useGrafanaUser: true,
                   grafanaUserField: 'login'
               };
               expect(createDatasource(settings, ctx).user).to.equal("admin");
           });

           it('should be email if defined', () => {
               const settings = Object.assign({}, defaultSettings);
               settings.jsonData = {
                   useGrafanaUser: true,
                   grafanaUserField: 'email'
               };
               expect(createDatasource(settings, ctx).user).to.equal("admin@opennms.org");
           });

           it('should be name if defined', () => {
               const settings = Object.assign({}, defaultSettings);
               settings.jsonData = {
                   useGrafanaUser: true,
                   grafanaUserField: 'name'
               };
               expect(createDatasource(settings, ctx).user).to.equal("The Administrator");
           });

           it('should fall back to login if field does not exist', () => {
               delete ctx.contextSrv.user.email;
               const settings = Object.assign({}, defaultSettings);
               settings.jsonData = {
                   useGrafanaUser: true,
                   grafanaUserField: 'email'
               };
               expect(createDatasource(settings, ctx).user).to.equal("admin");
           });
        });

        describe('buildQuery', () => {
            it('should substitute scoped variables', () => {
                // The filter with variables
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction("key", API.Comparators.EQ, "$variable1"), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction("key2", API.Comparators.EQ, "Hello this is my [[variable1]]"), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction("key3", API.Comparators.EQ, "value3"), API.Operators.AND));

                // The scoped variables
                const options = {
                    scopedVars: {
                        "variable1": {value: "dummy-value"}
                    }
                };

                const substitutedFilter = ctx.datasource.buildQuery(filter, options);

                // Verify
                expect(substitutedFilter.clauses[0].restriction.value).to.equal("dummy-value");
                expect(substitutedFilter.clauses[1].restriction.value).to.equal("Hello this is my dummy-value");
                expect(substitutedFilter.clauses[2].restriction.value).to.equal("value3");
            });

            it('should substitude $range_from and $range_to accordingly', () => {
                // The input filter
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction("key", API.Comparators.EQ, "$range_from"), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction("key2", API.Comparators.EQ, "$range_to"), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction("key3", API.Comparators.EQ, "[[range_from]]"), API.Operators.AND))
                    .withClause(new API.Clause(new API.Restriction("key4", API.Comparators.EQ, "[[range_to]]"), API.Operators.AND));

                // Options
                const options = {
                    targets: [filter],
                    range: {
                        from: ctx.range_from,
                        to: ctx.range_to,
                    },
                    scopedVars: {}
                };

                // Build query and verify
                const substitutedFilter = ctx.datasource.buildQuery(filter, options);
                expect(substitutedFilter.clauses[0].restriction.value).to.equal(ctx.range_from);
                expect(substitutedFilter.clauses[1].restriction.value).to.equal(ctx.range_to);
                expect(substitutedFilter.clauses[2].restriction.value).to.equal(ctx.range_from);
                expect(substitutedFilter.clauses[3].restriction.value).to.equal(ctx.range_to);
            });

            it ('should include $range_from and $range_to when building the query', () => {
               const filter = new API.Filter();
               let actualFilter = ctx.datasource.buildQuery(filter, {});
               expect(actualFilter.clauses.length).to.equal(0);

               // Try building it with enforced range
               actualFilter = ctx.datasource.buildQuery(filter, {
                   enforceTimeRange: true,
                   range: {
                       from: ctx.range_from,
                       to: ctx.range_to,
                   },
               });
               expect(filter).not.to.equal(actualFilter);
               expect(actualFilter.clauses.length).to.equal(1);
               expect(actualFilter.clauses[0].restriction.clauses[0].restriction.value).to.equal(ctx.range_from);
               expect(actualFilter.clauses[0].restriction.clauses[1].restriction.value).to.equal(ctx.range_to);

            });

            it ('should turn a node criteria fs:fid restriction into 2 separate clauses', () => {
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction('node', API.Comparators.EQ, 'FS:FID'), API.Operators.AND));

                const actualFilter = ctx.datasource.buildQuery(filter, {});
                expect(filter).not.to.equal(actualFilter);
                expect(actualFilter.clauses.length).to.equal(1);
                expect(actualFilter.clauses[0].restriction.clauses[0].restriction.value).to.equal('FS');
                expect(actualFilter.clauses[0].restriction.clauses[1].restriction.value).to.equal('FID');
            });

            it ('should turn a node criteria ID restriction into a node.id clause', () => {
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction('node', API.Comparators.EQ, '1'), API.Operators.AND));

                const actualFilter = ctx.datasource.buildQuery(filter, {});
                expect(filter).not.to.equal(actualFilter);
                expect(actualFilter.clauses.length).to.equal(1);
                expect(actualFilter.clauses[0].restriction.attribute).to.equal('node.id');
                expect(actualFilter.clauses[0].restriction.value).to.equal('1');
            });

            it ('should handle multi-select with 0 values selected', () => {
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.EQ, '$severity'), API.Operators.AND));

                ctx.templateSrv.init([{
                    name: 'severity',
                    multi: true,
                    current: {
                        value: []
                    }
                }]);

                const actualFilter = ctx.datasource.buildQuery(filter, {});
                expect(filter).not.to.equal(actualFilter);
                expect(actualFilter.clauses.length).to.equal(1);
                expect(actualFilter.clauses[0].restriction.clauses).not.to.equal(null);
                expect(actualFilter.clauses[0].restriction.clauses.length).to.equal(0);
            });

            it ('should handle multi-select with 1 value selected', () => {
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.EQ, '$severity'), API.Operators.AND));

                ctx.templateSrv.init([{
                    name: 'severity',
                    multi: true,
                    current: {
                        value: ['NORMAL']
                    }
                }]);

                const actualFilter = ctx.datasource.buildQuery(filter, {});
                expect(filter).not.to.equal(actualFilter);
                expect(actualFilter.clauses.length).to.equal(1);
                expect(actualFilter.clauses[0].restriction.attribute).to.equal('severity');
                expect(actualFilter.clauses[0].restriction.value).to.equal('NORMAL');
            });

            it ('should handle multi-select with 2 values selected', () => {
                const filter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction('severity', API.Comparators.EQ, '$severity'), API.Operators.AND));

                ctx.templateSrv.init([{
                    name: 'severity',
                    multi: true,
                    current: {
                        value: ['NORMAL', 'WARNING']
                    }
                }]);

                const actualFilter = ctx.datasource.buildQuery(filter, {});
                expect(filter).not.to.equal(actualFilter);
                expect(actualFilter.clauses.length).to.equal(1);
                expect(actualFilter.clauses[0].restriction.clauses).not.to.equal(null);
                expect(actualFilter.clauses[0].restriction.clauses.length).to.equal(2);
                expect(actualFilter.clauses[0].restriction.clauses[0].restriction.value).to.equal('NORMAL');
                expect(actualFilter.clauses[0].restriction.clauses[1].restriction.value).to.equal('WARNING');
            });

            it ('should add additional criteria when a filter panel is configured', () => {
                ctx.datasource.templateSrv = ctx.templateSrv;
                const selected =  {
                    datasource: ctx.datasource.name,
                    inputType: 'text',
                    entityType: entityTypes[0],
                    resource: 'logMsg',
                    text: 'foo*bar',
                    value: 'foo*bar',
                };
                ctx.dashboardSrv.dashboard.panels.push({
                    type: 'opennms-helm-filter-panel',
                    columns: [
                        Object.assign({}, {
                            selected: selected,
                        }, selected),
                    ],
                });

                const entity = new AlarmEntity({}, ctx.datasource);
                const restrictions = entity.getPanelRestrictions();
                expect(restrictions.clauses.length).to.equal(1);
                expect(restrictions.clauses[0].restriction.attribute).to.equal('logMsg');
                expect(restrictions.clauses[0].restriction.value).to.equal('*foo*bar*');
            });

            it ('should not add * to the criteria value if it starts or ends with *', () => {
                const selected =  {
                    datasource: ctx.datasource.name,
                    inputType: 'text',
                    entityType: entityTypes[0],
                    resource: 'logMsg',
                    text: 'foo*bar*',
                    value: 'foo*bar*',
                };
                ctx.dashboardSrv.dashboard.panels.push({
                    type: 'opennms-helm-filter-panel',
                    columns: [
                        Object.assign({}, {
                            selected: selected,
                        }, selected),
                    ],
                });

                const entity = new AlarmEntity({}, ctx.datasource);
                const restrictions = entity.getPanelRestrictions();
                expect(restrictions.clauses.length).to.equal(1);
                expect(restrictions.clauses[0].restriction.attribute).to.equal('logMsg');
                expect(restrictions.clauses[0].restriction.value).to.equal('foo*bar*');
            });

            it ('should not add * to the criteria value if it is an empty string', () => {
                const selected =  {
                    datasource: ctx.datasource.name,
                    inputType: 'text',
                    entityType: entityTypes[0],
                    resource: 'logMsg',
                    text: '',
                    value: '',
                };
                ctx.dashboardSrv.dashboard.panels.push({
                    type: 'opennms-helm-filter-panel',
                    columns: [
                        Object.assign({}, {
                            selected: selected,
                        }, selected),
                    ],
                });

                const entity = new AlarmEntity({}, ctx.datasource);
                const restrictions = entity.getPanelRestrictions();
                expect(restrictions).to.be.undefined;
            });
        });
    });

});
