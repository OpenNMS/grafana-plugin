// eslint-disable-next-line no-restricted-imports
import moment from 'moment';

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
export class NRTGHandler {
  pollingInterval = 1000;
  datapoints = [] as any[];
  subscribers = [] as any[];
  startTime = null as any;
  endTime = null as any;
  private pollInProgress = false;
  private collectionTaskId: any;

  constructor(public options: any, public datasource: any) {
  }

  start() {
    const _this = this;
    this.datasource.backendSrv.datasourceRequest({
      url: this.datasource.url + '/nrt/starter',
      method: 'GET',
      params: {
        resourceId: "node[10].nodeSnmp[]",
        report: "mib2.tcpopen"
      }
    }).then(response => {
      _this.handleCollectionDetails(response.data);
    });
  }

  handleCollectionDetails(nrtgCollectionDetails) {
    const _this = this;

    // Start polling
    this.collectionTaskId = nrtgCollectionDetails.collectionTaskId;
    const poll = function () {_this.poll();}.bind(_this);
    poll();
    setInterval(poll, _this.pollingInterval);
  }

  poll() {
    const self = this;
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
    }).then(response => {
      self.pollInProgress = false;
      self.handleMeasurementSets(response.data.measurement_sets);
    });
  }

  handleMeasurementSets(measurementSets) {
    let i, nsets, nsubscribers;
    for (i = 0, nsets = measurementSets.length; i < nsets; i++) {
      let measurements = measurementSets[i];
      for (let k = 0, nmeasurements = measurements.length; k < nmeasurements; k++) {
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

    const seriesList = [{
      target: "x",
      datapoints: this.datapoints
    }];

    for (i = 0, nsubscribers = this.subscribers.length; i < nsubscribers; i++) {
      this.subscribers[i].next({data: seriesList, range: {from: moment(this.startTime), to: moment(this.endTime)}});
    }
  }

  subscribe(options) {
    this.subscribers.push(options);
  }
}
