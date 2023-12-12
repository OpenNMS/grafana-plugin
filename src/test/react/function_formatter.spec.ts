import {
  findFunctions,
  formatFunctions,
  parenthesize,
  replaceFunctions
} from '../../lib/function_formatter'

const FUNCS = {
  exclamationer: (arg) => {
    return arg + '!!!'
  },
  insulter: (person, insult) => {
    return person + ' is a total ' + insult + '!'
  }
}

describe('OpenNMSPMDatasource :: LabelFormatter', () => {
  describe('parenthesize', () => {
    test.each([
      ['no parentheses', 'this is just a string', 1, ['this is just a string']],
      ['simple()', 'simple()', 1,
        [
          {
            name: 'simple',
            arguments: []
          }
        ]
      ],
      ['nestedParens((foo) or (bar))', 'nestedParens((foo) or (bar))', 1,
        [
          {
            name: 'nestedParens',
            arguments: ['(foo) or (bar)']
          }
        ]
      ],
      [
        'prefix nestedParens((yah)) and anotherNested((foo) or (bar)) or something',
        'prefix nestedParens((yah)) and anotherNested((foo) or (bar), baz) or something',
        5,
        [
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
        ]
      ]
    ]) (
      '%s',
      (title, query, length, expected) => {
        const parsed = parenthesize(query)

        expect(parsed).toBeDefined()
        expect(parsed.length).toEqual(length)
        expect(parsed).toStrictEqual(expected)
      }
    )
  })

  describe('findFunctions()', () => {
    test.each([
      ['find a simple function with no arguments', 'simpleFunction()', 1,
        [
          {
            name: 'simpleFunction',
            arguments: []
          }
        ]
      ],
      ['find a simple function with no arguments', 'simpleFunction()', 1,
        [
          {
            name: 'simpleFunction',
            arguments: []
          }
        ]
      ],
      ['find a simple function with one argument', 'simpleFunction(foo)', 1,
        [
          {
            name: 'simpleFunction',
            arguments: ['foo']
          }
        ]
      ],
      ['find a simple function with many arguments', 'simpleFunction(foo, bar, baz)', 1,
        [
          {
            name: 'simpleFunction',
            arguments: ['foo', 'bar', 'baz']
          }
        ]
      ],
      ['find multiple functions with arguments', 'simpleFunction(foo, bar, baz), anotherThing($nodes)', 2,
        [
          {
            name: 'simpleFunction',
            arguments: ['foo', 'bar', 'baz']
          },
          {
            name: 'anotherThing',
            arguments: ['$nodes']
          }
        ]
      ],
      ['find multiple functions with parens inside them', 'foo((bar) or (baz), uh-huh) yo(something)', 2,
        [
          {
            name: 'foo',
            arguments: ['(bar) or (baz)', 'uh-huh'],
          },
          {
            name: 'yo',
            arguments: ['something'],
          }
        ]
      ],
      ['GRAFANA-PLUGIN-131', 'nodeFilter((nodeLabel like ‘This%’) or (nodeLabel like ‘Down%’))', 1,
        [
          {
            name: 'nodeFilter',
            arguments: ['(nodeLabel like ‘This%’) or (nodeLabel like ‘Down%’)'],
          }
        ]
      ]
    ]) (
      'findFunctions: %s',
      (title, query, length, expected) => {
        const found = findFunctions(query)

        expect(found).toBeDefined()
        expect(found.length).toEqual(length)
        expect(found).toStrictEqual(expected)
      }
    )
  })

  describe('replace()', () => {
    it('do not replace an unmatched function', () => {
      const res = replaceFunctions('unmatchedFunction()', {
          simpleFunction: () => {
              return 'foo';
          }
      })

      expect(res).toEqual('unmatchedFunction()')
    })

    it('replace a simple function without arguments', () => {
      const res = replaceFunctions('simpleFunction()', {
          simpleFunction: () => {
              return 'foo';
          }
      })

      expect(res).toEqual('foo')
    })

    test.each([
        ['replace a simple function with an argument', 'exclamationer(foo)', 'foo!!!'],
        ['replace a simple function with multiple arguments', 'insulter(Bob, jerk)', 'Bob is a total jerk!'],
        ['replace multiple functions', 'exclamationer(Hey)  insulter(Bob, jerk)', 'Hey!!!  Bob is a total jerk!']
      ]) (
        '%s',
        (title, query, expected) => {
          const res = replaceFunctions(query, FUNCS)
          expect(res).toEqual(expected)
        }
      )
  })

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
  }

  describe('formatFunctions()', () => {
    test.each([
      ['nodeToLabel(foreignSource:foreignId)', 'nodeToLabel(situation-test:nodea)', 'ThisIsAVeryLongNodeLabel1'],
      ['nodeToLabel(nodeId)', 'nodeToLabel(1)', 'ThisIsAVeryLongNodeLabel1'],
      ['nodeToLabel(invalid)', 'nodeToLabel(3)', '3'],
      ['resourceToLabel(resourceId)', 'resourceToLabel(node[situation-test:nodea].responseTime[127.0.0.1])', '127.0.0.1'],
      ['resourceToLabel(situation-test:nodea, responseTime[127.0.0.1])', 'resourceToLabel(situation-test:nodea, responseTime[127.0.0.1])', '127.0.0.1'],
      ['resourceToLabel(invalid)', 'resourceToLabel(foo)', 'foo'],
      ['resourceToLabel(invalid, invalid)', 'resourceToLabel(foo, bar)', 'foo.bar'],
      ['resourceToName(resourceId)', 'resourceToName(node[situation-test:nodea].responseTime[127.0.0.1])', 'localhost'],
      ['resourceToName(situation-test:nodea, responseTime[127.0.0.1])', 'resourceToName(situation-test:nodea, responseTime[127.0.0.1])', 'localhost'],
      ['resourceToName(invalid)', 'resourceToName(foo)', 'foo'],
      ['resourceToName(invalid, invalid)', 'resourceToName(foo, bar)', 'foo.bar'],
      ['resourceToInterface(resourceId)', 'resourceToInterface(node[situation-test:nodeb].interfaceSnmp[en0-000123456789])', 'en0'],
      ['resourceToInterface(situation-test:nodeb, interfaceSnmp[en0-000123456789])', 'resourceToInterface(situation-test:nodeb, interfaceSnmp[en0-000123456789])', 'en0'],
      ['resourceToInterface(invalid)', 'resourceToInterface(foo)', 'foo'],
      ['resourceToInterface(invalid, invalid)', 'resourceToInterface(foo, bar)', 'foo.bar']
    ]) (
      'formatFunctions: %s',
      (title, query, expected) => {
        const res = formatFunctions(query, metadata)
        expect(res).toEqual(expected)
      }
    )
  })
})
