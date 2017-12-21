import {FlowDatasource} from '../datasources/flow-ds/datasource';

describe("OpenNMS_Flow_Datasource", function () {

  let flowSeriesExample = {
    "start": 1516358909932,
    "end": 1516373309932,
    "columns": [
      {
        "label": "domain",
        "ingress": true
      },
      {
        "label": "domain",
        "ingress": false
      }
    ],
    "timestamps": [
      1516358909932
    ],
    "values": [
      [
        1
      ],
      [
        2
      ]
    ]
  };

  describe('Mapping', function () {
    it("should map series response to Grafana series", function (done) {
      let actualResponse = FlowDatasource.toSeries({}, flowSeriesExample);
      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        }
      ];

      expect(expectedResponse).to.eql(actualResponse);
      done();
    });


    it("should combine ingress and egress when set", function (done) {
      let target = {
        'functions': [
          {
            'name': 'combineIngressEgress'
          }
        ]
      };
      let actualResponse = FlowDatasource.toSeries(target, flowSeriesExample);
      let expectedResponse = [
        {
          "datapoints": [
            [
              3,
              1516358909932
            ]
          ],
          "target": "domain"
        }
      ];

      expect(expectedResponse).to.eql(actualResponse);
      done();
    });
  });

});
