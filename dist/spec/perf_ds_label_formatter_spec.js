'use strict';

System.register(['../datasources/perf-ds/function_formatter'], function (_export, _context) {
    "use strict";

    var FunctionFormatter, FUNCS;
    return {
        setters: [function (_datasourcesPerfDsFunction_formatter) {
            FunctionFormatter = _datasourcesPerfDsFunction_formatter.FunctionFormatter;
        }],
        execute: function () {
            FUNCS = {
                exclamationer: function exclamationer(arg) {
                    return arg + '!!!';
                },
                insulter: function insulter(person, insult) {
                    return person + ' is a total ' + insult + '!';
                }
            };


            describe('OpenNMSPMDatasource :: LabelFormatter', function () {
                describe('findFunctions()', function () {
                    it('find a simple function with no arguments', function () {
                        var found = FunctionFormatter.findFunctions('simpleFunction()');
                        expect(found).to.exist;
                        expect(found.length).to.equal(1);
                        expect(found).to.deep.equal([{
                            name: 'simpleFunction',
                            arguments: []
                        }]);
                    });
                    it('find a simple function with one argument', function () {
                        var found = FunctionFormatter.findFunctions('simpleFunction(foo)');
                        expect(found).to.exist;
                        expect(found.length).to.equal(1);
                        expect(found).to.deep.equal([{
                            name: 'simpleFunction',
                            arguments: ['foo']
                        }]);
                    });
                    it('find a simple function with many arguments', function () {
                        var found = FunctionFormatter.findFunctions('simpleFunction(foo, bar, baz)');
                        expect(found).to.exist;
                        expect(found.length).to.equal(1);
                        expect(found).to.deep.equal([{
                            name: 'simpleFunction',
                            arguments: ['foo', 'bar', 'baz']
                        }]);
                    });
                    it('find multiple functions with arguments', function () {
                        var found = FunctionFormatter.findFunctions('simpleFunction(foo, bar, baz), anotherThing($nodes)');
                        expect(found).to.exist;
                        expect(found.length).to.equal(2);
                        expect(found).to.deep.equal([{
                            name: 'simpleFunction',
                            arguments: ['foo', 'bar', 'baz']
                        }, {
                            name: 'anotherThing',
                            arguments: ['$nodes']
                        }]);
                    });
                });

                describe('replace()', function () {
                    it('do not replace an unmatched function', function () {
                        var res = FunctionFormatter.replace('unmatchedFunction()', {
                            simpleFunction: function simpleFunction() {
                                return 'foo';
                            }
                        });
                        expect(res).to.equal('unmatchedFunction()');
                    });
                    it('replace a simple function without arguments', function () {
                        var res = FunctionFormatter.replace('simpleFunction()', {
                            simpleFunction: function simpleFunction() {
                                return 'foo';
                            }
                        });
                        expect(res).to.equal('foo');
                    });
                    it('replace a simple function with an argument', function () {
                        var res = FunctionFormatter.replace('exclamationer(foo)', FUNCS);
                        expect(res).to.equal('foo!!!');
                    });
                    it('replace a simple function with multiple arguments', function () {
                        var res = FunctionFormatter.replace('insulter(Bob, jerk)', FUNCS);
                        expect(res).to.equal('Bob is a total jerk!');
                    });
                    it('replace multiple functions', function () {
                        var res = FunctionFormatter.replace('exclamationer(Hey)  insulter(Bob, jerk)', FUNCS);
                        expect(res).to.equal('Hey!!!  Bob is a total jerk!');
                    });
                });

                var metadata = {
                    "resources": [{
                        "id": "node[situation-test:nodea].responseTime[127.0.0.1]",
                        "label": "127.0.0.1",
                        "name": "localhost",
                        "parent-id": "node[situation-test:nodea]",
                        "node-id": 1
                    }, {
                        "id": "node[situation-test:nodeb].responseTime[127.0.0.1]",
                        "label": "127.0.0.1",
                        "name": "localhost",
                        "parent-id": "node[situation-test:nodeb]",
                        "node-id": 2
                    }, {
                        "id": "node[situation-test:nodeb].interfaceSnmp[en0-000123456789]",
                        "label": "en0 (127.0.0.1, 304.2 Mbps)",
                        "name": "en0-000123456789",
                        "parent-id": "node[situation-test:nodeb]",
                        "node-id": 2
                    }],
                    "nodes": [{
                        "id": 1,
                        "label": "ThisIsAVeryLongNodeLabel1",
                        "foreign-source": "situation-test",
                        "foreign-id": "nodea"
                    }, {
                        "id": 2,
                        "label": "ThisIsAVeryLongNodeLabel2",
                        "foreign-source": "situation-test",
                        "foreign-id": "nodeb"
                    }]
                };

                describe('format()', function () {
                    it('nodeToLabel(foreignSource:foreignId)', function () {
                        var res = FunctionFormatter.format('nodeToLabel(situation-test:nodea)', metadata);
                        expect(res).to.equal('ThisIsAVeryLongNodeLabel1');
                    });
                    it('nodeToLabel(nodeId)', function () {
                        var res = FunctionFormatter.format('nodeToLabel(1)', metadata);
                        expect(res).to.equal('ThisIsAVeryLongNodeLabel1');
                    });
                    it('nodeToLabel(invalid)', function () {
                        var res = FunctionFormatter.format('nodeToLabel(3)', metadata);
                        expect(res).to.equal('3');
                    });

                    it('resourceToLabel(resourceId)', function () {
                        var res = FunctionFormatter.format('resourceToLabel(node[situation-test:nodea].responseTime[127.0.0.1])', metadata);
                        expect(res).to.equal('127.0.0.1');
                    });
                    it('resourceToLabel(situation-test:nodea, responseTime[127.0.0.1])', function () {
                        var res = FunctionFormatter.format('resourceToLabel(situation-test:nodea, responseTime[127.0.0.1])', metadata);
                        expect(res).to.equal('127.0.0.1');
                    });
                    it('resourceToLabel(invalid)', function () {
                        var res = FunctionFormatter.format('resourceToLabel(foo)', metadata);
                        expect(res).to.equal('foo');
                    });
                    it('resourceToLabel(invalid, invalid)', function () {
                        var res = FunctionFormatter.format('resourceToLabel(foo, bar)', metadata);
                        expect(res).to.equal('foo.bar');
                    });

                    it('resourceToName(resourceId)', function () {
                        var res = FunctionFormatter.format('resourceToName(node[situation-test:nodea].responseTime[127.0.0.1])', metadata);
                        expect(res).to.equal('localhost');
                    });
                    it('resourceToName(situation-test:nodea, responseTime[127.0.0.1])', function () {
                        var res = FunctionFormatter.format('resourceToName(situation-test:nodea, responseTime[127.0.0.1])', metadata);
                        expect(res).to.equal('localhost');
                    });
                    it('resourceToName(invalid)', function () {
                        var res = FunctionFormatter.format('resourceToName(foo)', metadata);
                        expect(res).to.equal('foo');
                    });
                    it('resourceToName(invalid, invalid)', function () {
                        var res = FunctionFormatter.format('resourceToName(foo, bar)', metadata);
                        expect(res).to.equal('foo.bar');
                    });

                    it('resourceToInterface(resourceId)', function () {
                        var res = FunctionFormatter.format('resourceToInterface(node[situation-test:nodeb].interfaceSnmp[en0-000123456789])', metadata);
                        expect(res).to.equal('en0');
                    });
                    it('resourceToInterface(situation-test:nodeb, interfaceSnmp[en0-000123456789])', function () {
                        var res = FunctionFormatter.format('resourceToInterface(situation-test:nodeb, interfaceSnmp[en0-000123456789])', metadata);
                        expect(res).to.equal('en0');
                    });
                    it('resourceToInterface(invalid)', function () {
                        var res = FunctionFormatter.format('resourceToInterface(foo)', metadata);
                        expect(res).to.equal('foo');
                    });
                    it('resourceToInterface(invalid, invalid)', function () {
                        var res = FunctionFormatter.format('resourceToInterface(foo, bar)', metadata);
                        expect(res).to.equal('foo.bar');
                    });
                });
            });
        }
    };
});
//# sourceMappingURL=perf_ds_label_formatter_spec.js.map
