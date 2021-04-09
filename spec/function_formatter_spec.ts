const {FunctionFormatter} = require('../lib/function_formatter');

const FUNCS = {
    exclamationer: (arg) => {
        return arg + '!!!';
    },
    insulter: (person, insult) => {
        return person + ' is a total ' + insult + '!';
    }
};

describe('OpenNMSPMDatasource :: LabelFormatter', () => {
    describe('parenthesize', () => {
        it('no parentheses', () => {
            const parsed = FunctionFormatter.parenthesize('this is just a string');
            expect(parsed).toBeDefined();
            expect(parsed.length).toEqual(1);
            expect(parsed).toStrictEqual([
                'this is just a string'
            ]);
        });
        it('simple()', () => {
            const parsed = FunctionFormatter.parenthesize('simple()');
            expect(parsed).toBeDefined();
            expect(parsed.length).toEqual(1);
            expect(parsed).toStrictEqual([
                {
                    name: 'simple',
                    arguments: []
                }
            ]);
        });
        it('nestedParens((foo) or (bar))', () => {
            const parsed = FunctionFormatter.parenthesize('nestedParens((foo) or (bar))');
            expect(parsed).toBeDefined();
            expect(parsed.length).toEqual(1);
            expect(parsed).toStrictEqual([
                {
                    name: 'nestedParens',
                    arguments: ['(foo) or (bar)']
                }
            ]);
        });
        it('prefix nestedParens((yah)) and anotherNested((foo) or (bar)) or something', () => {
            const parsed = FunctionFormatter.parenthesize('prefix nestedParens((yah)) and anotherNested((foo) or (bar), baz) or something');
            expect(parsed).toBeDefined();
            expect(parsed.length).toEqual(5);
            expect(parsed).toStrictEqual([
                'prefix ',
                {
                    name: 'nestedParens',
                    arguments: ['(yah)']
                },
                ' and ',
                {
                    name: 'anotherNested',
                    arguments: ['(foo) or (bar), baz']
                },
                ' or something'
            ]);
        });
        /*
        it('outerFunction(nestedFunction(withArgs))', () => {
            const parsed = FunctionFormatter.parenthesize('outerFunction(nestedFunction(withArgs))');
            expect(parsed).toBeDefined();
            expect(parsed.length).toEqual(1);
            expect(parsed).toStrictEqual([
                {
                    name: 'outerFunction',
                    arguments: [{
                        name: 'nestedFunction',
                        arguments: ['withArgs']
                    }]
                }
            ]);
        });
        it('outerFunction(nestedFunction(withArgs), (foo) (bar))', () => {
            const parsed = FunctionFormatter.parenthesize('outerFunction(nestedFunction(withArgs), (foo) (bar))', true);
            expect(parsed).toBeDefined();
            expect(parsed.length).toEqual(1);
            expect(parsed).toStrictEqual([
                {
                    name: 'outerFunction',
                    arguments: [
                        {
                            name: 'nestedFunction',
                            arguments: ['withArgs']
                        },
                        '(foo) (bar)'
                    ]
                }
            ]);
        });
        */
    });

    describe('findFunctions()', () => {
        it('find a simple function with no arguments', () => {
            const found = FunctionFormatter.findFunctions('simpleFunction()');
            expect(found).toBeDefined();
            expect(found.length).toEqual(1);
            expect(found).toStrictEqual([
                {
                    name: 'simpleFunction',
                    arguments: []
                }
            ]);
        });
        it('find a simple function with one argument', () => {
            const found = FunctionFormatter.findFunctions('simpleFunction(foo)');
            expect(found).toBeDefined();
            expect(found.length).toEqual(1);
            expect(found).toStrictEqual([
                {
                    name: 'simpleFunction',
                    arguments: ['foo']
                }
            ]);
        });
        it('find a simple function with many arguments', () => {
            const found = FunctionFormatter.findFunctions('simpleFunction(foo, bar, baz)');
            expect(found).toBeDefined();
            expect(found.length).toEqual(1);
            expect(found).toStrictEqual([
                {
                    name: 'simpleFunction',
                    arguments: ['foo', 'bar', 'baz']
                }
            ]);
        });
        it('find multiple functions with arguments', () => {
            const found = FunctionFormatter.findFunctions('simpleFunction(foo, bar, baz), anotherThing($nodes)');
            expect(found).toBeDefined();
            expect(found.length).toEqual(2);
            expect(found).toStrictEqual([
                {
                    name: 'simpleFunction',
                    arguments: ['foo', 'bar', 'baz']
                },
                {
                    name: 'anotherThing',
                    arguments: ['$nodes']
                }
            ]);
        });
        it('find multiple functions with parens inside them', () => {
            const found = FunctionFormatter.findFunctions('foo((bar) or (baz), uh-huh) yo(something)');
            expect(found).toBeDefined();
            expect(found.length).toEqual(2);
            expect(found).toStrictEqual([
                {
                    name: 'foo',
                    arguments: ['(bar) or (baz)', 'uh-huh'],
                },
                {
                    name: 'yo',
                    arguments: ['something'],
                },
            ]);
        });
        it('HELM-131', () => {
            const found = FunctionFormatter.findFunctions('nodeFilter((nodeLabel like ‘This%’) or (nodeLabel like ‘Down%’))');
            expect(found).toBeDefined();
            expect(found.length).toEqual(1);
            expect(found).toStrictEqual([
                {
                    name: 'nodeFilter',
                    arguments: ['(nodeLabel like ‘This%’) or (nodeLabel like ‘Down%’)'],
                },
            ]);
        });
        /*
        it('find multiple recursive functions with arguments', () => {
            const found = FunctionFormatter.findFunctions('complexFunction(simpleFunction(foo), bar, baz), anotherThing(extraThing(recursiveThing($nodes)))');
            expect(found).toBeDefined();
            expect(found.length).toEqual(5);
            expect(found).toStrictEqual([
                {
                    name: 'simpleFunction',
                    arguments: ['foo']
                },
                {
                    name: 'complexFunction',
                    arguments: ['simpleFunction(foo)', 'bar', 'baz']
                },
                {
                    name: 'recursiveThing',
                    arguments: ['$nodes']
                },
                {
                    name: 'extraThing',
                    arguments: ['recursiveThing($nodes)']
                },
                {
                    name: 'anotherThing',
                    arguments: ['extraThing(recursiveThing($nodes))']
                }
            ]);
        });
        */
    });

    describe('replace()', () => {
        it('do not replace an unmatched function', () => {
            const res = FunctionFormatter.replace('unmatchedFunction()', {
                simpleFunction: () => {
                    return 'foo';
                }
            });
            expect(res).toEqual('unmatchedFunction()');
        });
        it('replace a simple function without arguments', () => {
            const res = FunctionFormatter.replace('simpleFunction()', {
                simpleFunction: () => {
                    return 'foo';
                }
            });
            expect(res).toEqual('foo');
        });
        it('replace a simple function with an argument', () => {
            const res = FunctionFormatter.replace('exclamationer(foo)', FUNCS);
            expect(res).toEqual('foo!!!');
        });
        it('replace a simple function with multiple arguments', () => {
            const res = FunctionFormatter.replace('insulter(Bob, jerk)', FUNCS);
            expect(res).toEqual('Bob is a total jerk!');
        });
        it('replace multiple functions', () => {
            const res = FunctionFormatter.replace('exclamationer(Hey)  insulter(Bob, jerk)', FUNCS);
            expect(res).toEqual('Hey!!!  Bob is a total jerk!');
        });
        /*
        it('handles nested parentheses', () => {
            const res = FunctionFormatter.replace('exclamationer((foo) or (bar))  insulter((baz) (monkey(shoe)) (this is raw parentheses), jerk)', FUNCS, true);
            expect(res).toEqual('(foo) or (bar)!!!  (baz) (monkey(shoe)) (this is raw parentheses) is a total jerk!');
        });
        */
    });

    const metadata = {
        "resources": [
            {
                "id": "node[situation-test:nodea].responseTime[127.0.0.1]",
                "label": "127.0.0.1",
                "name": "localhost",
                "parent-id": "node[situation-test:nodea]",
                "node-id": 1
            },
            {
                "id": "node[situation-test:nodeb].responseTime[127.0.0.1]",
                "label": "127.0.0.1",
                "name": "localhost",
                "parent-id": "node[situation-test:nodeb]",
                "node-id": 2
            },
            {
                "id": "node[situation-test:nodeb].interfaceSnmp[en0-000123456789]",
                "label": "en0 (127.0.0.1, 304.2 Mbps)",
                "name": "en0-000123456789",
                "parent-id": "node[situation-test:nodeb]",
                "node-id": 2
            }
        ],
        "nodes": [
            {
                "id": 1,
                "label": "ThisIsAVeryLongNodeLabel1",
                "foreign-source": "situation-test",
                "foreign-id": "nodea"
            },
            {
                "id": 2,
                "label": "ThisIsAVeryLongNodeLabel2",
                "foreign-source": "situation-test",
                "foreign-id": "nodeb"
            }
        ]
    };

    describe('format()', () => {
        it('nodeToLabel(foreignSource:foreignId)', () => {
            const res = FunctionFormatter.format('nodeToLabel(situation-test:nodea)', metadata);
            expect(res).toEqual('ThisIsAVeryLongNodeLabel1');
        });
        it('nodeToLabel(nodeId)', () => {
            const res = FunctionFormatter.format('nodeToLabel(1)', metadata);
            expect(res).toEqual('ThisIsAVeryLongNodeLabel1');
        });
        it('nodeToLabel(invalid)', () => {
            const res = FunctionFormatter.format('nodeToLabel(3)', metadata);
            expect(res).toEqual('3');
        });

        it('resourceToLabel(resourceId)', () => {
            const res = FunctionFormatter.format('resourceToLabel(node[situation-test:nodea].responseTime[127.0.0.1])', metadata);
            expect(res).toEqual('127.0.0.1');
        });
        it('resourceToLabel(situation-test:nodea, responseTime[127.0.0.1])', () => {
            const res = FunctionFormatter.format('resourceToLabel(situation-test:nodea, responseTime[127.0.0.1])', metadata);
            expect(res).toEqual('127.0.0.1');
        });
        it('resourceToLabel(invalid)', () => {
            const res = FunctionFormatter.format('resourceToLabel(foo)', metadata);
            expect(res).toEqual('foo');
        });
        it('resourceToLabel(invalid, invalid)', () => {
            const res = FunctionFormatter.format('resourceToLabel(foo, bar)', metadata);
            expect(res).toEqual('foo.bar');
        });

        it('resourceToName(resourceId)', () => {
            const res = FunctionFormatter.format('resourceToName(node[situation-test:nodea].responseTime[127.0.0.1])', metadata);
            expect(res).toEqual('localhost');
        });
        it('resourceToName(situation-test:nodea, responseTime[127.0.0.1])', () => {
            const res = FunctionFormatter.format('resourceToName(situation-test:nodea, responseTime[127.0.0.1])', metadata);
            expect(res).toEqual('localhost');
        });
        it('resourceToName(invalid)', () => {
            const res = FunctionFormatter.format('resourceToName(foo)', metadata);
            expect(res).toEqual('foo');
        });
        it('resourceToName(invalid, invalid)', () => {
            const res = FunctionFormatter.format('resourceToName(foo, bar)', metadata);
            expect(res).toEqual('foo.bar');
        });

        it('resourceToInterface(resourceId)', () => {
            const res = FunctionFormatter.format('resourceToInterface(node[situation-test:nodeb].interfaceSnmp[en0-000123456789])', metadata);
            expect(res).toEqual('en0');
        });
        it('resourceToInterface(situation-test:nodeb, interfaceSnmp[en0-000123456789])', () => {
            const res = FunctionFormatter.format('resourceToInterface(situation-test:nodeb, interfaceSnmp[en0-000123456789])', metadata);
            expect(res).toEqual('en0');
        });
        it('resourceToInterface(invalid)', () => {
            const res = FunctionFormatter.format('resourceToInterface(foo)', metadata);
            expect(res).toEqual('foo');
        });
        it('resourceToInterface(invalid, invalid)', () => {
            const res = FunctionFormatter.format('resourceToInterface(foo, bar)', metadata);
            expect(res).toEqual('foo.bar');
        });
    });
});
