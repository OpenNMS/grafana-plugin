// avoid warning about angular not being there
const warn = console.warn;
console.warn = () => {};

import { Datasource } from '../datasources/perf-ds/module';
import { AllowedProperties } from '../datasources/perf-ds/datasource';

console.warn = warn;

describe('OpenNMSPMDatasource', function () {
  let ctx = {} as any;

  beforeEach(function () {
    ctx.backendSrv = {};
    ctx.templateSrv = {replace: val => val, containsVariable: () => true};
    ctx.ds = new Datasource({url: 'http://opennms'}, ctx.backendSrv, ctx.templateSrv);
  });

  describe('querying with one target', function () {
    let query = {
      range: {from: 'now-1h', to: 'now'},
      targets: [{
        type: "attribute",
        nodeId: '1',
        resourceId: 'nodeSnmp[]',
        attribute: 'loadavg1',
        aggregation: 'AVERAGE'
      }],
      interval: '1s'
    };

    let response = {
      "step": 300000,
      "start": 1424211730000,
      "end": 1424226130000,
      "timestamps": [1424211730001],
      "labels": ["loadavg1"],
      "columns": [
        {
          "values": [5.0]
        }
      ]
    };

    it('should return a list of series', function (done) {
      ctx.backendSrv.datasourceRequest = function (request) {
        return Promise.resolve({
          _request: request,
          status: 200,
          data: response
        });
      };

      ctx.ds.query(query).then(function (result) {
        expect(result.data).toHaveLength(1);
        expect(result.data[0].target).toEqual('loadavg1');
        expect(result.data[0].datapoints).toHaveLength(1);
        done();
      });
    });
  });

  describe('relaxed mode', function () {
    let query = {
      range: {from: 'now-1h', to: 'now'},
      targets: [{
        type: "attribute",
        nodeId: '1',
        resourceId: 'nodeSnmp[]',
        attribute: 'does-not-exist',
        aggregation: 'AVERAGE'
      }],
      interval: '1s'
    };

    let response = {
      "step": 300000,
      "start": 1424211730000,
      "end": 1424226130000,
      "timestamps": [1424211730001],
      "labels": ["loadavg1"],
      "columns": [
        {
          "values": [NaN]
        }
      ]
    };

    it('should filter series that contain only NaNs', function (done) {
      ctx.backendSrv.datasourceRequest = function (request) {
        return Promise.resolve({
          _request: request,
          status: 200,
          data: response
        });
      };

      ctx.ds.query(query).then(function (result) {
        expect(result.data).toHaveLength(0);
        done();
      });
    });
  });

  describe('testing for connectivity', function () {
    it('should make a request to /rest/info', function (done) {
      ctx.backendSrv.datasourceRequest = function (request) {
        expect(request.url).toEqual('http://opennms/rest/info');
        return Promise.resolve({
          status: 200
        });
      };

      ctx.ds.testDatasource().then(function () {
        done();
      });
    });
  });

  describe('using templates', function () {
    it('should perform simple variable substitution', function () {
      ctx.templateSrv.variables = [
        {name: 'variable', current: {value: 'loadavg1'}}
      ];

      let options = {
        range: {from: 'now-1h', to: 'now'},
        targets: [{
          type: "attribute",
          nodeId: '1',
          resourceId: 'nodeSnmp[]',
          attribute: '$variable',
          aggregation: 'AVERAGE'
        }],
        interval: '1s'
      };
      let [query,] = ctx.ds.buildQuery(options);

      expect(query.source.length).toEqual(1);
      expect(query.source[0].attribute).toEqual("loadavg1");
    });

    it('should support scoped variables', function () {
      ctx.templateSrv.variables = [
        {name: 'variable', current: {value: 'loadavg1'}}
      ];

      let options = {
        range: {from: 'now-1h', to: 'now'},
        targets: [{
          type: "attribute",
          nodeId: '1',
          resourceId: 'nodeSnmp[]',
          attribute: '$variable',
          aggregation: 'AVERAGE'
        }],
        interval: '1s',
        scopedVars: {
          'variable': {
            value: 'loadavg5'
          }
        }
      };
      let [query,] = ctx.ds.buildQuery(options);

      expect(query.source.length).toEqual(1);
      expect(query.source[0].attribute).toEqual("loadavg5");
    });

    it('should use node[] or nodeSource[] based on the contents of the variable', function () {
      ctx.templateSrv.variables = [
        {name: 'node', current: {value: ['1', 'FS:FID']}}
      ];

      let options = {
        range: {from: 'now-1h', to: 'now'},
        targets: [{
          type: "attribute",
          nodeId: '$node',
          resourceId: 'nodeSnmp[]',
          attribute: 'CpuRawIdle',
          aggregation: 'AVERAGE',
          label: 'idle on $node'
        }],
        interval: '1s'
      };
      let [query,] = ctx.ds.buildQuery(options);

      expect(query.source.length).toEqual(2);
      expect(query.source[0].resourceId).toEqual("node[1].nodeSnmp[]");
      expect(query.source[1].resourceId).toEqual("nodeSource[FS:FID].nodeSnmp[]");
    });

    it('should generate multiple sources for multi-valued template variables', function () {
      ctx.templateSrv.variables = [
        {name: 'v1', current: {value: ['1', '2']}},
        {name: 'v2', current: {value: ['x', 'y']}}
      ];

      let options = {
        range: {from: 'now-1h', to: 'now'},
        targets: [{
          type: "attribute",
          nodeId: '1',
          resourceId: 'nodeSnmp[]',
          attribute: '$v1-$v2',
          aggregation: 'AVERAGE'
        }],
        interval: '1s'
      };
      let [query,] = ctx.ds.buildQuery(options);

      expect(query.source.length).toEqual(4);
      expect(query.source[0].attribute).toEqual("1-x");
      expect(query.source[1].attribute).toEqual("1-y");
      expect(query.source[2].attribute).toEqual("2-x");
      expect(query.source[3].attribute).toEqual("2-y");
    });

    it('should handle substituting "All" with all of the available values', function () {
      ctx.templateSrv.variables = [
        {
          name: 'x',
          current: {value: ['$__all']},
          options: [
            {value: '$__all'},
            {value: 'a'},
            {value: 'b'},
            {value: 'c'},
            {value: 'd'}
          ]
        }
      ];

      let options = {
        range: {from: 'now-1h', to: 'now'},
        targets: [{
          type: "attribute",
          nodeId: '1',
          resourceId: 'nodeSnmp[]',
          attribute: '$x',
          aggregation: 'AVERAGE'
        }],
        interval: '1s'
      };
      let [query,] = ctx.ds.buildQuery(options);

      expect(query.source.length).toEqual(4);
      expect(query.source[0].attribute).toEqual("a");
      expect(query.source[1].attribute).toEqual("b");
      expect(query.source[2].attribute).toEqual("c");
      expect(query.source[3].attribute).toEqual("d");
    });

    it('should perform variable substitution on all filter parameters', function () {
      ctx.templateSrv.variables = [
        {name: 'variable', current: {value: ['x', 'y']}}
      ];

      let options = {
        range: {from: 'now-1h', to: 'now'},
        targets: [
          {
            type: "attribute",
            nodeId: '1',
            resourceId: 'nodeSnmp[]',
            attribute: '$variable',
            aggregation: 'AVERAGE'
          },
          {
            type: "filter",
            filter: {
              name: "some-filter"
            },
            filterParameters: {
              param1: 1,
              param2: '$variable'
            }
          }
        ],
        interval: '1s'
      };
      let [query,] = ctx.ds.buildQuery(options);

      expect(query.filter.length).toEqual(2);
      expect(query.filter[0].name).toEqual("some-filter");
      expect(query.filter[0].parameter.length).toEqual(2);
      expect(query.filter[0].parameter[0].key).toEqual("param1");
      expect(query.filter[0].parameter[0].value).toEqual(1);
      expect(query.filter[0].parameter[1].key).toEqual("param2");
      expect(query.filter[0].parameter[1].value).toEqual("x");

      expect(query.filter[1].name).toEqual("some-filter");
      expect(query.filter[1].parameter.length).toEqual(2);
      expect(query.filter[1].parameter[0].key).toEqual("param1");
      expect(query.filter[1].parameter[0].value).toEqual(1);
      expect(query.filter[1].parameter[1].key).toEqual("param2");
      expect(query.filter[1].parameter[1].value).toEqual("y");
    });
  });

  describe('preserving order', function () {
      it('should preserve a single label', function () {
          let options = {
              range: {from: 'now-1h', to: 'now'},
              targets: [{
                  type: "attribute",
                  nodeId: '1',
                  resourceId: 'nodeSnmp[]',
                  attribute: 'loadavg1',
                  aggregation: 'AVERAGE'
              }],
              interval: '1s'
          };

          let [,labels] = ctx.ds.buildQuery(options);

          expect(labels.length).toEqual(1);
          expect(labels[0]).toEqual("loadavg1");
      });

      it('should preserve multiple labels', function () {
          let options = {
              range: {from: 'now-1h', to: 'now'},
              targets: [{
                  type: "attribute",
                  nodeId: '1',
                  resourceId: 'nodeSnmp[]',
                  attribute: 'loadavg1',
                  aggregation: 'AVERAGE'
              },
                  {
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: 'loadavg5',
                      aggregation: 'AVERAGE'
                  }],
              interval: '1s'
          };

          let [,labels] = ctx.ds.buildQuery(options);

          expect(labels.length).toEqual(2);
          expect(labels[0]).toEqual("loadavg1");
          expect(labels[1]).toEqual("loadavg5");
      });

      it('should preserve multiple labels (reverse)', function () {
          let options = {
              range: {from: 'now-1h', to: 'now'},
              targets: [
                  {
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: 'loadavg5',
                      aggregation: 'AVERAGE'
                  },
                  {
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: 'loadavg1',
                      aggregation: 'AVERAGE'
                  }
              ],
              interval: '1s'
          };

          let [,labels] = ctx.ds.buildQuery(options);

          expect(labels.length).toEqual(2);
          expect(labels[0]).toEqual("loadavg5");
          expect(labels[1]).toEqual("loadavg1");
      });

      it('should reorder the series', async function () {
          let query = {
              range: {from: 'now-1h', to: 'now'},
              targets: [
                  {
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: 'a',
                      aggregation: 'AVERAGE'
                  },
                  {
                      label: "b",
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: 'woot',
                      aggregation: 'AVERAGE'
                  },
                  {
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: 'missing',
                      aggregation: 'AVERAGE'
                  },
                  {
                      type: "attribute",
                      nodeId: '1',
                      resourceId: 'nodeSnmp[]',
                      attribute: '$variable',
                      aggregation: 'AVERAGE'
                  },
              ],
              scopedVars: {
                  'variable': {
                      value: 'c'
                  }
              },
              interval: '1s'
          };

          let response = {
              "step": 5,
              "start": 0,
              "end": 10,
              "timestamps": [0, 5, 10],
              'labels': ['a', 'c', 'b'],
              "columns": [
                  {'values': [1, 2, 3]},
                  {'values': [9, 9, 9]},
                  {'values': [3, 2, 1]},
              ],
          };

          ctx.templateSrv.variables = [
              {name: 'variable', current: {value: 'x'}},
              {name: 'nodeId', current: {value: '1'}},
          ];
          ctx.backendSrv.datasourceRequest = function (request) {
              return Promise.resolve({
                  _request: request,
                  status: 200,
                  data: response
              });
          };

          const result = await ctx.ds.query(query);
          expect(result.data.length).toEqual(3);
          expect(result.data[0].target).toEqual('a');
          expect(result.data[0].datapoints).toStrictEqual([[1, 0], [2, 5], [3, 10]]);
          expect(result.data[1].target).toEqual('b');
          expect(result.data[1].datapoints).toStrictEqual([[3, 0], [2, 5], [1, 10]]);
          expect(result.data[2].target).toEqual('c');
          expect(result.data[2].datapoints).toStrictEqual([[9, 0], [9, 5], [9, 10]]);
      });

    it('should identify source with glob expressions only', function () {
      let options = {
        range: { from: 'now-1h', to: 'now' },
        targets: [
          {
            type: "attribute",
            nodeId: '1',
            resourceId: 'nodeSnmp[*]',
            attribute: 'loadavg5|loadavg5',
            aggregation: 'AVERAGE'
          },
          {
            type: "attribute",
            nodeId: '1',
            resourceId: 'nodeSnmp[]',
            attribute: 'loadavg1',
            aggregation: 'AVERAGE'
          }
        ],
        interval: '1s'
      };

      let [query,] = ctx.ds.buildQuery(options);
      let globSources = ctx.ds.getGlobExpressionsOnly(query.source);
      expect(query.source.length).toEqual(2);
      expect(globSources.size).toEqual(1);
    });

    it('Get matches from response with | glob expression', function () {

      let response = {
        "id": "node[selfmonitor:1].nodeSnmp[]",
        "label": "",
        "name": "",
        "link": null,
        "typeLabel": "",
        "parentId": "node[selfmonitor:1]",
        "children": {
          "totalCount": null,
          "count": null,
          "offset": 0,
          "resource": []
        },
        "stringPropertyAttributes": {},
        "externalValueAttributes": {},
        "rrdGraphAttributes": {
          "loadavg11": {
            "name": "loadavg11",
            "relativePath": "/path/to/loadavg11",
            "rrdFile": "loadavg11.jrb"
          },
          "loadavg12": {
            "name": "loadavg12",
            "relativePath": "/path/to/loadavg12",
            "rrdFile": "loadavg12.jrb"
          },
          "loadavg23": {
            "name": "loadavg23",
            "relativePath": "/path/to/loadavg23",
            "rrdFile": "loadavg23.jrb"
          },
          "loadavg21": {
            "name": "loadavg21",
            "relativePath": "/path/to/loadavg21",
            "rrdFile": "loadavg21.jrb"
          }

        }
      };

      let options = {
        range: { from: 'now-1h', to: 'now' },
        targets: [
          {
            type: "attribute",
            nodeId: '1',
            resourceId: 'nodeSnmp[]',
            attribute: 'loadavg11|loadavg12',
            aggregation: 'AVERAGE'
          }
        ],
        interval: '1s'
      };


      let [query,] = ctx.ds.buildQuery(options);

      const propsToMatch: Map<AllowedProperties, string> = ctx.ds.getGlobQueries(query.source[0]);

      const matchingProps = ctx.ds.getMatchingProperties(response, propsToMatch, '1');

      const sources = ctx.ds.generateMatchingSourcesForMatchingProperties(matchingProps, query.source[0], propsToMatch);

      expect(propsToMatch.size).toEqual(2);
      expect(matchingProps.length).toEqual(2);
      expect(sources.length).toEqual(2);
      expect(sources[0].attribute).toEqual('loadavg11');
      expect(sources[1].attribute).toEqual('loadavg12');
    });

    it('Get matches from response with * glob expression', function () {

      let response = {
        "id": "node[selfmonitor:1].nodeSnmp[]",
        "label": "",
        "name": "",
        "link": null,
        "typeLabel": "",
        "parentId": "node[selfmonitor:1]",
        "children": {
          "totalCount": null,
          "count": null,
          "offset": 0,
          "resource": []
        },
        "stringPropertyAttributes": {},
        "externalValueAttributes": {},
        "rrdGraphAttributes": {
          "loadavg11": {
            "name": "loadavg11",
            "relativePath": "/path/to/loadavg11",
            "rrdFile": "loadavg11.jrb"
          },
          "loadavg12": {
            "name": "loadavg12",
            "relativePath": "/path/to/loadavg12",
            "rrdFile": "loadavg12.jrb"
          },
          "loadavg23": {
            "name": "loadavg23",
            "relativePath": "/path/to/loadavg23",
            "rrdFile": "loadavg23.jrb"
          },
          "loadavg21": {
            "name": "loadavg21",
            "relativePath": "/path/to/loadavg21",
            "rrdFile": "loadavg21.jrb"
          }

        }
      };

      let options = {
        range: { from: 'now-1h', to: 'now' },
        targets: [
          {
            type: "attribute",
            nodeId: '1',
            resourceId: 'nodeSnmp[]',
            attribute: 'loadavg2*',
            aggregation: 'AVERAGE'
          }
        ],
        interval: '1s'
      };


      let [query,] = ctx.ds.buildQuery(options);

      const propsToMatch: Map<AllowedProperties, string> = ctx.ds.getGlobQueries(query.source[0]);

      const matchingProps = ctx.ds.getMatchingProperties(response, propsToMatch, '1');

      const sources = ctx.ds.generateMatchingSourcesForMatchingProperties(matchingProps, query.source[0], propsToMatch);

      expect(propsToMatch.size).toEqual(2);
      expect(matchingProps.length).toEqual(2);
      expect(sources.length).toEqual(2);
      expect(sources[0].attribute).toEqual('loadavg23');
      expect(sources[1].attribute).toEqual('loadavg21');
    });

   
  });
});
