import { FlowDatasource } from '../datasources/flow-ds/datasource';
import {TemplateSrv} from "./template_srv";
import {dateTimeAsMoment} from "@grafana/data";
import {OnmsFlowSeries} from "opennms/src/model/OnmsFlowSeries";

describe("OpenNMS_Flow_Datasource", function () {

  const flowDatasource = new FlowDatasource({ url: "http://localhost" }, null, new TemplateSrv())

  let flowSeriesExample = {
    "start": dateTimeAsMoment(1516358909932),
    "end": dateTimeAsMoment(1516373309932),
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
  } as OnmsFlowSeries

  let flowSeriesExampleNaN = {
    "start": dateTimeAsMoment(1516358909932),
    "end": dateTimeAsMoment(1516373309932),
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
        "NaN"
      ],
      [
        2
      ]
    ]
  } as OnmsFlowSeries

  describe('Mapping', function () {
    it("should map series response to Grafana series", function (done) {
      let actualResponse = flowDatasource.toSeries({ metric: '', refId: ''}, flowSeriesExample);
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

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });


    it("should combine ingress and egress when set", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'combineIngressEgress'
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExample);
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

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should convert bytes to bits when set", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'toBits'
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExample);
      let expectedResponse = [
        {
          "datapoints": [
            [
              8,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              16,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should only show ingress when set", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'onlyIngress'
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExample);
      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should only show egress when set", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'onlyEgress'
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExample);
      let expectedResponse = [
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

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should apply prefix and suffix to labels when set", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'withPrefix',
            'parameters': ['prefix-']
          },
          {
            'name': 'withSuffix',
            'parameters': ['-suffix']
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExample);
      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "prefix-domain (In)-suffix"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "target": "prefix-domain (Out)-suffix"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    })

    it("should convert 'NaN' values in response to Grafana series", function (done) {
      let actualResponse = flowDatasource.toSeries({ metric: '', refId: ''}, flowSeriesExampleNaN);
      let expectedResponse = [
        {
          "datapoints": [
            [
              0,
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

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

  });

});
