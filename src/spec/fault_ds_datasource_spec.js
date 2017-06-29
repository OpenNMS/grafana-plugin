import Q from "q";
import _ from 'lodash';
import {UI} from '../datasources/fault-ds/ui';
import {API} from '../opennms'
import {Mapping} from '../datasources/fault-ds/Mapping'

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
                expect(mapping.getApiFilter(new UI.Filter(uiSegmentSrv))).to.eql(new API.Filter());

                done();
            });

            it ('should map from api to ui filter and vice versa', function(done) {
                const apiFilter = new API.Filter()
                    .withClause(new API.Clause(new API.Restriction("key", API.Comparators.EQ, "value"), API.Operators.OR))
                    .withClause(new API.Clause(new API.Restriction("key2", API.Comparators.NE, "value2"), API.Operators.AND));

                const uiFilter = new UI.Filter(uiSegmentSrv)
                    .withClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.RestrictionDTO("key", "=", "value")))
                    .withClause(new UI.Clause(uiSegmentSrv, UI.Operators.AND, new UI.RestrictionDTO("key2", "!=", "value2")));

                expect(mapping.getUiFilter(apiFilter)).to.eql(uiFilter);
                expect(mapping.getApiFilter(uiFilter)).to.eql(apiFilter);

                done();
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
            })
        });

        describe('clear', function () {
            it('should reset query', function (done) {
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

                uiFilter.addClause(new UI.Clause(uiSegmentSrv, UI.Operators.OR, new UI.RestrictionDTO("severity", UI.Comparators.GE, 'WARNING')))
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

                expect(uiFilter.getQueryString()).to.eql("select all alarms where (severity >= 'WARNING' and severity <= 'MAJOR' or (location = 'Fulda'))")

                done();
            });
        })

    });

});