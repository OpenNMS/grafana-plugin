import *  as helpers from '../../datasources/flow-ds-react/helpers';
import {
  FlowQueryData,
  FlowParsedQueryData,
} from '../../datasources/flow-ds-react/types';
import { FlowStrings } from '../../datasources/flow-ds-react/constants';
import { OnmsFlowSeries } from 'opennms/src/model';
import { dateTimeAsMoment } from "@grafana/data";

describe("OpenNMS_Flow_Datasource", function () {

  let options,
    partialQueryData,
    fullQueryData,
    dataFromOpenNMS,
    dataFromOpenNMSWithNaN,
    exporterNodes;

  beforeEach(() => {

    options = {
      range: {
        from: dateTimeAsMoment(1516358909932),
        to: dateTimeAsMoment(1516358909932),
        raw: {
          from: "now-6h",
          to: "now"
        }
      },
      targets: [{
        refId: "A",
        datasource: {
          "type": "opennms-helm-flow-datasource-react",
          "uid": "KLhKw1DVk"
        },
        segment: 0,
        functions: [{ "label": "topN" }],
        functionParameters: ["10"],
        parameterOptions: []
      }]
    };

    partialQueryData = [
      {
        "segment": 0,
        "functionParameters": [
          "10"
        ],
        "functions": [
          {
            "label": "topN"
          }
        ],
        "parameterOptions": [],
        "refId": "A"
      }
    ] as FlowQueryData[];

    dataFromOpenNMS = {
      "start": dateTimeAsMoment(1516358909932),
      "end": dateTimeAsMoment(1516358909932),
      "columns": [
        {
          "label": "domain",
          "ingress": false
        },
        {
          "label": "domain",
          "ingress": true
        },
        {
          "label": "domain1",
          "ingress": true
        }
      ],
      "timestamps": [
        1516358909932
      ],
      "values": [
        [1],
        [2],
        [5]]
    } as OnmsFlowSeries;

    dataFromOpenNMSWithNaN = {
      "start": dateTimeAsMoment(1516358909932),
      "end": dateTimeAsMoment(1516358909932),
      "columns": [
        {
          "label": "domain",
          "ingress": false
        },
        {
          "label": "domain",
          "ingress": true
        },
        {
          "label": "domain1",
          "ingress": true
        }
      ],
      "timestamps": [
        1516358909932
      ],
      "values": [
        [1],
        "NaN",
        [5]]
    } as OnmsFlowSeries;

    exporterNodes = [
      {
        "text": "NYC-Cisco-ASR100-Core-Router",
        "value": 1,
        "expandable": true
      },
      {
        "text": "NYC-Cisco-ASR100-Core-Router-MIMIC27-172.16.33.101",
        "value": 23,
        "expandable": true
      },
      {
        "text": "LON-Juniper-T4000-Core-Router",
        "value": 2,
        "expandable": true
      }
    ]

  });

  describe('Mapping', function () {

    it("Should return return an array of FlowDataQuery", function (done) {
      const expected: FlowQueryData[] = partialQueryData;
      expect(helpers.extractDataFromQuery(options.targets)).toEqual(expected);
      done();
    });

    it("Should return return FlowParsedQueryData, an array of FlowParsedQueryRow ", function (done) {
      const expected: FlowParsedQueryData = [
        {
          "segment": {
            "id": 0,
            "label": "Applications"
          },
          "queryFunctions": [
            { "topN": "10", },
            { "withPrefix": "pre-" },
            { "withExporterNode": "1" },
            { "swapIngressEgress": '' },
            { "toBits": '' }
          ],
          "refId": "A"
        }
      ] as FlowParsedQueryData;

      partialQueryData = [
        {
          "segment": 0,
          "functionParameters": [
            "10",
            "pre-",
            "1",
            undefined,
            null
          ],
          "functions": [
            { "label": "topN" },
            { "label": "withPrefix" },
            { "label": "withExporterNode" },
            { "label": "swapIngressEgress" },
            { "label": "toBits" }
          ],
          "parameterOptions": [],
          "refId": "A"
        }
      ] as FlowQueryData[];
      expect(helpers.buildFullQueryData(partialQueryData, { replace: (v) => v })).toEqual(expected);
      done();
    });

    it("should map series response to Grafana series", function (done) {
      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [[1, 1516358909932]],
          "target": "domain (Out)"
        },
        {
          "datapoints": [[2, 1516358909932]],
          "target": "domain (In)"
        },
        {
          "datapoints": [[5, 1516358909932]],
          "target": "domain1 (In)"
        }
      ];

      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(actualResponse).toEqual(expectedResponse);
      done();
    });

    it("should combine ingress and egress when set", function (done) {
      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              combineIngressEgress: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [[3, 1516358909932]],
          "target": "domain"
        },
        {
          "datapoints": [[5, 1516358909932]],
          "target": "domain1"
        }
      ];

      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(actualResponse).toEqual(expectedResponse);
      done();
    });

    it("should convert bytes to bits when set", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              toBits: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              8,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        },
        {
          "datapoints": [
            [
              16,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              40,
              1516358909932
            ]
          ],
          "target": "domain1 (In)"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should only show ingress when set", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              onlyIngress: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "target": "domain1 (In)"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should only show egress when set", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              onlyEgress: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should apply prefix and suffix to labels when set", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              withPrefix: 'prefix-',
              withSuffix: '-suffix',
              combineIngressEgress: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              3,
              1516358909932
            ]
          ],
          "target": "prefix-domain-suffix"
        },
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "target": "prefix-domain1-suffix"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should apply prefix and suffix and combineIngressEgress to labels when set", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              withPrefix: 'prefix-',
              withSuffix: '-suffix',

            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "prefix-domain (Out)-suffix"
        }, {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "target": "prefix-domain (In)-suffix"
        },
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "target": "prefix-domain1 (In)-suffix"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should convert 'NaN' to 0 values in response to Grafana series", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              nanToZero: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        }, {
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
              5,
              1516358909932
            ]
          ],
          "target": "domain1 (In)"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMSWithNaN);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should convert 'NaN' to null values in response to Grafana series", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {

            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        }, {
          "datapoints": [
            [
              null,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "target": "domain1 (In)"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMSWithNaN);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should Swap Ingress/Egress labels in response to Grafana series", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              swapIngressEgress: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "target": "domain (In)"
        }, {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "target": "domain (Out)"
        },
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "target": "domain1 (Out)"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should Swap Ingress/Egress and convert toBits, labels in response to Grafana series", function (done) {
      dataFromOpenNMS = {
        "start": null,
        "end": null,
        "headers": [
          "Application",
          "Bytes In",
          "Bytes Out",
          "ECN"
        ],
        "rows": [
          [
            "app0",
            1,
            2,
            3
          ],
          [
            "app1",
            5,
            null,
            3
          ]
        ]
      }

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            { swapIngressEgress: '' },
            { toBits: '' }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [{
        refId: '',
        "columns": [
          {
            "text": "Application"
          },
          {
            "text": "Bits In"
          },
          {
            "text": "Bits Out"
          },
          {
            "text": "ECN"
          }
        ],
        "rows": [
          [
            "app0",
            16,
            8,
            "non-ect / ce"
          ],
          [
            "app1",
            0,
            40,
            "non-ect / ce"
          ]
        ],
        "type": "table",
      }];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.summaries, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should combine multiple with uneven qty of ingress and egress when set", function (done) {

      fullQueryData = [
        {
          segment: {
            id: 0,
            label: ''
          },
          queryFunctions: [
            {
              combineIngressEgress: ''
            }
          ],
          refId: ''
        }
      ] as FlowParsedQueryData;

      let expectedResponse = [
        {
          "datapoints": [
            [
              3,
              1516358909932
            ]
          ],
          "target": "domain"
        },
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "target": "domain1"
        }
      ];
      let actualResponse = helpers.processDataBasedOnType(FlowStrings.series, fullQueryData[0], options, dataFromOpenNMS);
      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("Filter exporter nodes by location", function (done) {
      let simpleRequest = {};
      let client = {
        getNode: (nodeId: any) => {
          let location = "Default"
          if (nodeId > 2) {
            location = "Unknown";
          }
          return Promise.resolve(
            {
              "id": 1,
              "label": "localhost",
              "labelSource": {},
              "foreignSource": "selfmonitor",
              "foreignId": "1",
              "location": location,
              "createTime": "",
              "type": {},
              "lastCapsdPoll": "",
              "snmpInterfaces": [],
              "ipInterfaces": [],
              "categories": [],
              "assets": {}
            });
        }
      };

      let actualResponse = helpers.getFilteredNodes({ client, simpleRequest }, exporterNodes, "location='Default'");
      let expectedResponse = [
        {
          "text": "NYC-Cisco-ASR100-Core-Router",
          "value": 1,
          "expandable": true
        },
        {
          "text": "LON-Juniper-T4000-Core-Router",
          "value": 2,
          "expandable": true
        }
      ];
      actualResponse.then(response => {
        expect(response.length).toEqual(2);
        expect(response).toEqual(expectedResponse);
      });

      done();
    });

  });
});

