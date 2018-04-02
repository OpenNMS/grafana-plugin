"use strict";

System.register(["../datasources/flow-ds/datasource"], function (_export, _context) {
  "use strict";

  var FlowDatasource;
  return {
    setters: [function (_datasourcesFlowDsDatasource) {
      FlowDatasource = _datasourcesFlowDsDatasource.FlowDatasource;
    }],
    execute: function () {

      describe("OpenNMS_Flow_Datasource", function () {

        var flowSeriesExample = {
          "start": 1516358909932,
          "end": 1516373309932,
          "columns": [{
            "label": "domain",
            "ingress": true
          }, {
            "label": "domain",
            "ingress": false
          }],
          "timestamps": [1516358909932],
          "values": [[1], [2]]
        };

        describe('Mapping', function () {
          it("should map series response to Grafana series", function (done) {
            var actualResponse = FlowDatasource.toSeries({}, flowSeriesExample);
            var expectedResponse = [{
              "datapoints": [[1, 1516358909932]],
              "target": "domain (In)"
            }, {
              "datapoints": [[2, 1516358909932]],
              "target": "domain (Out)"
            }];

            expect(expectedResponse).to.eql(actualResponse);
            done();
          });

          it("should combine ingress and egress when set", function (done) {
            var target = {
              'functions': [{
                'name': 'combineIngressEgress'
              }]
            };
            var actualResponse = FlowDatasource.toSeries(target, flowSeriesExample);
            var expectedResponse = [{
              "datapoints": [[3, 1516358909932]],
              "target": "domain"
            }];

            expect(expectedResponse).to.eql(actualResponse);
            done();
          });
        });
      });
    }
  };
});
//# sourceMappingURL=flow_ds_datasource_spec.js.map
