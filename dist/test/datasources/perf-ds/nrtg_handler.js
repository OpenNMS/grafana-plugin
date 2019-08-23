'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.NRTGHandler = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _moment = require('moment');

var _moment2 = _interopRequireDefault(_moment);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/*
Prototype code used to stream metrics from NRTG to the Grafana's streaming APIs.

You can use the following to invoke this from a datasource:
  query(options) {
    // NRTG Streaming
    var handler = new NRTGHandler(options, this);
    handler.start();
    return Promise.resolve(handler);
  }
*/
var NRTGHandler = exports.NRTGHandler = function () {
  function NRTGHandler(options, datasource) {
    _classCallCheck(this, NRTGHandler);

    this.options = options;
    this.datasource = datasource;
    this.pollingInterval = 1000;
    this.subscribers = [];

    this.startTime = null;
    this.endTime = null;
    this.datapoints = [];
  }

  _createClass(NRTGHandler, [{
    key: 'start',
    value: function start() {
      var _this = this;
      this.datasource.backendSrv.datasourceRequest({
        url: this.datasource.url + '/nrt/starter',
        method: 'GET',
        params: {
          resourceId: "node[10].nodeSnmp[]",
          report: "mib2.tcpopen"
        }
      }).then(function (response) {
        _this.handleCollectionDetails(response.data);
      });
    }
  }, {
    key: 'handleCollectionDetails',
    value: function handleCollectionDetails(nrtgCollectionDetails) {
      var _this = this;

      // Start polling
      this.collectionTaskId = nrtgCollectionDetails.collectionTaskId;
      var poll = function () {
        _this.poll();
      }.bind(_this);
      poll();
      setInterval(poll, _this.pollingInterval);
    }
  }, {
    key: 'poll',
    value: function poll() {
      var self = this;
      if (self.pollInProgress === true) {
        // If another poll is already in progress, then skip this one
        return;
      }

      self.pollInProgress = true;
      this.datasource.backendSrv.datasourceRequest({
        url: this.datasource.url + '/nrt/starter',
        method: 'GET',
        params: {
          poll: "true",
          nrtCollectionTaskId: self.collectionTaskId
        }
      }).then(function (response) {
        self.pollInProgress = false;
        self.handleMeasurementSets(response.data.measurement_sets);
      });
    }
  }, {
    key: 'handleMeasurementSets',
    value: function handleMeasurementSets(measurementSets) {
      var i, nsets, nsubscribers;
      for (i = 0, nsets = measurementSets.length; i < nsets; i++) {
        var measurements = measurementSets[i];
        for (var k = 0, nmeasurements = measurements.length; k < nmeasurements; k++) {
          if (!this.startTime) {
            this.startTime = measurements[k].timeStamp;
          }
          this.endTime = measurements[k].timeStamp;
          this.datapoints.push([measurements[k].value, measurements[k].timeStamp]);
        }
      }

      if (!this.startTime) {
        return;
      }

      var seriesList = [{
        target: "x",
        datapoints: this.datapoints
      }];

      for (i = 0, nsubscribers = this.subscribers.length; i < nsubscribers; i++) {
        this.subscribers[i].next({ data: seriesList, range: { from: (0, _moment2.default)(this.startTime), to: (0, _moment2.default)(this.endTime) } });
      }
    }
  }, {
    key: 'subscribe',
    value: function subscribe(options) {
      this.subscribers.push(options);
    }
  }]);

  return NRTGHandler;
}();
//# sourceMappingURL=nrtg_handler.js.map
