import { FlowDatasource } from '../datasources/flow-ds/datasource';
import { TemplateSrv } from "./template_srv";
import { dateTimeAsMoment } from "@grafana/data";
import { OnmsFlowSeries } from "opennms/src/model/OnmsFlowSeries";
import { OnmsFlowTable } from "opennms/src/model/OnmsFlowTable";

describe("OpenNMS_Flow_Datasource", function () {

  const flowDatasource = new FlowDatasource({ url: "http://localhost" }, null, new TemplateSrv())

  let flowSeriesExample, flowSeriesExampleNaN, flowSummaryExample, exporterNodes, flowSeriesExampleCalc;

  beforeEach(() => {

    flowSeriesExample = {
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

    flowSeriesExampleNaN = {
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

    flowSummaryExample = {
      "start": dateTimeAsMoment(1516358909932),
      "end": dateTimeAsMoment(1516373309932),
      "rows":
        [
          [
            "app0",
            5352721,
            5301360,
            3
          ],
          [
            "app1",
            3398268,
            2939031,
            3
          ]
        ],
      "headers": [
        "Application",
        "Bytes In",
        "Bytes Out",
        "ECN"
      ]
    } as OnmsFlowTable

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

    flowSeriesExampleCalc = {
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
        },
        {
          "label": "domain1",
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
        ],
        [
          5
        ]
      ]
    } as OnmsFlowSeries

    flowDatasource.simpleRequest.getResourcesForNode = async (node) => [
      {
        "id": "node[selfmonitor:1].interfaceSnmp[opennms-jvm]",
        "label": "opennms-jvm (*)",
        "name": "opennms-jvm",
        "link": "element/snmpinterface.jsp?node=1&ifindex=2",
        "typeLabel": "SNMP Interface Data",
        "parentId": "node[selfmonitor:1]",
        "stringPropertyAttributes": {},
        "externalValueAttributes": {},
        "rrdGraphAttributes": {}
      }
    ]
  });

  describe('Mapping', function () {
    it("should map series response to Grafana series", function (done) {
      let actualResponse = flowDatasource.toSeries({ metric: '', refId: '' }, flowSeriesExample);
      let expectedResponse = [
        {
          "datapoints": [
            [
              1,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
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
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
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
          "meta": {
            "custom": {
              "metric": "",
              "toBits": true,
            },
          },
          "refId": "",
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              16,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
              "toBits": true,
            },
          },
          "refId": "",
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
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
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
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
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
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "prefix-domain (In)-suffix"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "prefix-domain (Out)-suffix"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should convert 'NaN' to 0 values in response to Grafana series", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'nanToZero'
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExampleNaN);
      let expectedResponse = [
        {
          "datapoints": [
            [
              0,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (Out)"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("should convert 'NaN' to null values in response to Grafana series", function (done) {
      let target = {
        metric: '',
        refId: ''
      };
      let actualResponse = flowDatasource.toSeries(target, flowSeriesExampleNaN);
      let expectedResponse = [
        {
          "datapoints": [
            [
              null,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (Out)"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("Swap Ingress/Egress labels in response to Grafana series", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'swapIngressEgress'
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
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (Out)"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (In)"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("No Swap Ingress/Egress labels in response to Grafana series", function (done) {
      let target = {
        metric: '',
        refId: ''
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
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (In)"
        },
        {
          "datapoints": [
            [
              2,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain (Out)"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("Swap Ingress/Egress response to Grafana summary", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'swapIngressEgress'
          }
        ]
      };
      let actualResponse = flowDatasource.toTable(target, flowSummaryExample);
      let expectedResponse = {
        "meta": {
          "custom": {
            "metric": "",
          },
        },
        refId: '',
        "columns": [
          {
            "text": "Application"
          },
          {
            "text": "Bytes In"
          },
          {
            "text": "Bytes Out"
          },
          {
            "text": "ECN"
          }
        ],
        "rows": [
          [
            "app0",
            5301360,
            5352721,
            "non-ect / ce"
          ],
          [
            "app1",
            2939031,
            3398268,
            "non-ect / ce"
          ]
        ],
        "type": "table",
      };

      expect(expectedResponse).toEqual(actualResponse);
      done();
    });

    it("Filter exporter nodes by location", function (done) {
      flowDatasource.client.getNode = (nodeId: any) => {
        let location = "Default"
        if(nodeId > 2 ){
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
      }); };

      let actualResponse = flowDatasource.getFilteredNodes(exporterNodes, "location='Default'");
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

    it("should combine multiple with uneven qty of ingress and egress when set", function (done) {
      let target = {
        metric: '',
        refId: '',
        'functions': [
          {
            'name': 'combineIngressEgress'
          }
        ]
      };
      let actualResponse = flowDatasource.toSeries(target,  flowSeriesExampleCalc);

      let expectedResponse = [
        {
          "datapoints": [
            [
              3,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",          
          "target": "domain"
        }
        ,
        {
          "datapoints": [
            [
              5,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",          
          "target": "domain1"
        }
      ];

      expect(expectedResponse).toEqual(actualResponse);

      flowSeriesExampleCalc.columns.push(
        {
        "label": "domain1",
        "ingress": false
      });
      flowSeriesExampleCalc.values.push([10]);
      actualResponse = flowDatasource.toSeries(target,  flowSeriesExampleCalc);

      expectedResponse = [
        {
          "datapoints": [
            [
              3,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain"
        }
        ,
        {
          "datapoints": [
            [
              15,
              1516358909932
            ]
          ],
          "meta": {
            "custom": {
              "metric": "",
            },
          },
          "refId": "",
          "target": "domain1"
        }
      ];
      expect(expectedResponse).toEqual(actualResponse);

      done();
    });

    it("should return ifIndex when interface name or label are passed instead", async () => {
      let expectedResponse = "2";
      let nodeQuery = "node[20]";
      let iface = "interfaceSnmp[opennms-jvm]";

      let actualResponse = await flowDatasource.lookupIfIndex(nodeQuery, iface);
      expect(expectedResponse).toEqual(actualResponse);

      expectedResponse = "2";
      iface = "2";

      actualResponse = await flowDatasource.lookupIfIndex(nodeQuery, iface);
      expect(expectedResponse).toEqual(actualResponse);

      expectedResponse = "2";
      iface = "opennms-jvm";

      actualResponse = await flowDatasource.lookupIfIndex(nodeQuery, iface);
      expect(expectedResponse).toEqual(actualResponse);

      actualResponse = await flowDatasource.lookupIfIndex(nodeQuery, null);
      expect(actualResponse).toBeNull();

      let numExpectedResponse = 2;
      let numIface = 2;

      let numActualResponse = await flowDatasource.lookupIfIndex(nodeQuery, numIface);
      expect(numExpectedResponse).toEqual(numActualResponse);

      expectedResponse = "opennms-jvm-notfound";
      iface = "opennms-jvm-notfound";

      actualResponse = await flowDatasource.lookupIfIndex(nodeQuery, iface);
      expect(expectedResponse).toEqual(actualResponse);

    });

  });

});
