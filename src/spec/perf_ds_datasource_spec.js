import {Datasource} from "../datasources/perf-ds/module";
import Q from "q";

describe('OpenNMSPMDatasource', function () {
  let ctx = {};

  beforeEach(function () {
    ctx.$q = Q;
    ctx.backendSrv = {};
    ctx.templateSrv = {replace: val => val, containsVariable: () => true};
    ctx.ds = new Datasource({url: ''}, ctx.$q, ctx.backendSrv, ctx.templateSrv);
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
        return ctx.$q.when({
          _request: request,
          status: 200,
          data: response
        });
      };

      ctx.ds.query(query).then(function (result) {
        expect(result.data).to.have.length(1);
        expect(result.data[0].target).to.equal('loadavg1');
        expect(result.data[0].datapoints).to.have.length(1);
        done();
      });
    });
  });

  describe('testing for connectivity', function () {
    it('should make a request to /rest/info', function (done) {
      ctx.backendSrv.datasourceRequest = function (request) {
        expect(request.url).to.equal('/rest/info');
        return ctx.$q.when({
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
      let query = ctx.ds.buildQuery(options);

      expect(query.source.length).to.equal(1);
      expect(query.source[0].attribute).to.equal("loadavg1");
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
      let query = ctx.ds.buildQuery(options);

      expect(query.source.length).to.equal(1);
      expect(query.source[0].attribute).to.equal("loadavg5");
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
      let query = ctx.ds.buildQuery(options);

      expect(query.source.length).to.equal(2);
      expect(query.source[0].resourceId).to.equal("node[1].nodeSnmp[]");
      expect(query.source[1].resourceId).to.equal("nodeSource[FS:FID].nodeSnmp[]");
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
      let query = ctx.ds.buildQuery(options);

      expect(query.source.length).to.equal(4);
      expect(query.source[0].attribute).to.equal("1-x");
      expect(query.source[1].attribute).to.equal("1-y");
      expect(query.source[2].attribute).to.equal("2-x");
      expect(query.source[3].attribute).to.equal("2-y");
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
      let query = ctx.ds.buildQuery(options);

      expect(query.source.length).to.equal(4);
      expect(query.source[0].attribute).to.equal("a");
      expect(query.source[1].attribute).to.equal("b");
      expect(query.source[2].attribute).to.equal("c");
      expect(query.source[3].attribute).to.equal("d");
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
      let query = ctx.ds.buildQuery(options);

      expect(query.filter.length).to.equal(2);
      expect(query.filter[0].name).to.equal("some-filter");
      expect(query.filter[0].parameter.length).to.equal(2);
      expect(query.filter[0].parameter[0].key).to.equal("param1");
      expect(query.filter[0].parameter[0].value).to.equal(1);
      expect(query.filter[0].parameter[1].key).to.equal("param2");
      expect(query.filter[0].parameter[1].value).to.equal("x");

      expect(query.filter[1].name).to.equal("some-filter");
      expect(query.filter[1].parameter.length).to.equal(2);
      expect(query.filter[1].parameter[0].key).to.equal("param1");
      expect(query.filter[1].parameter[0].value).to.equal(1);
      expect(query.filter[1].parameter[1].key).to.equal("param2");
      expect(query.filter[1].parameter[1].value).to.equal("y");
    });
  });
});
