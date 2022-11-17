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
    dataFromOpenNMS;

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

    fullQueryData = [
      {
        "segment": {
          "id": 0,
          "label": "Applications"
        },
        "queryFunctions": [
          {
            "topN": "10"
          }
        ],
        "refId": "A"
      }
    ] as FlowParsedQueryData;

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
  });

  describe('Mapping', function () {

    it("Should return return an array of FlowDataQuery", function (done) {
      const expected: FlowQueryData[] = partialQueryData;
      expect(helpers.extractDataFromQuery(options.targets)).toEqual(expected);
      done();
    });

    it("Should return return FlowParsedQueryData, an array of FlowParsedQueryRow ", function (done) {
      const expected: FlowParsedQueryData = fullQueryData;
      expect(helpers.buildFullQueryData(partialQueryData)).toEqual(expected);
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


  });
});

