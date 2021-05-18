import _ from 'lodash';
import angular from 'angular';

import { API, Client, DAO, Model, Rest } from 'opennms';

let Q;

export class ClientDelegate {
    type?: string;
    url?: string;
    name?: string;
    searchLimit: number;
    timeout?: number;
    client: Client;
    clientWithMetadata?: angular.IPromise<Client>;

    /** @ngInject */
    constructor(settings: any, public backendSrv: any, public $q: angular.IQService) {
        this.type = settings.type;
        this.url = settings.url;
        this.name = settings.name;
        this.searchLimit = 1000;
        if (!Q) {
            Q = $q;
        }

        if (settings.jsonData && settings.jsonData.timeout) {
            this.timeout = parseInt(settings.jsonData.timeout,10) * 1000;
        }

        let authConfig = undefined;
        if (settings.basicAuth) {
          // If basic auth is configured, pass the username and password to the client
          // This allows the datasource to work in direct mode
          // We need the raw username and password, so we decode the token
          const token = settings.basicAuth.split(' ')[1];
          const decodedToken = atob(token);
          const username = decodedToken.split(':')[0];
          const password = decodedToken.substring(username.length+1, decodedToken.length);
          authConfig = new API.OnmsAuthConfig(username, password);
        }

        const server = API.OnmsServer.newBuilder(this.url).setName(this.name).setAuth(authConfig).build();
        const http = new Rest.GrafanaHTTP(this.backendSrv, server, this.timeout);
        this.client = new Client(http);
        this.clientWithMetadata = undefined;
     }

    decorateError(err) {
        let ret = err;
        if (err.err) {
            ret = err.err;
        }
        if (err.data && err.data.err) {
            ret = err.data.err;
        }
        let statusText = 'Request failed.';

        // cancelled property causes the UI to never complete on failure
        if (err.cancelled) {
            statusText = 'Request timed out.';
            delete err.cancelled;
        }
        if (err.data && err.data.cancelled) {
            statusText = 'Request timed out.';
            delete err.data.cancelled;
        }

        if (!ret.message) {
            ret.message = ret.statusText || statusText;
        }
        if (ret.message && ret.message.indexOf('too_many_buckets_exception') !== -1) {
            ret.message = ret.message + '; - alternatively you can edit the "Query options" of this panel\'s query and set "Max data points" to a smaller value.';
        }
        if (!ret.status) {
            ret.status = 'error';
        }
        return Q.reject(ret);
    }

    getClientWithMetadata(): angular.IPromise<Client> {
        if (!this.clientWithMetadata) {
              const self = this;
              const http = self.client.http;
              const client = Client.getMetadata(http.server, http, self.timeout)
                .then(function(metadata) {
                    // Ensure the OpenNMS we are talking to is compatible
                    if (metadata.apiVersion() < 2) {
                        throw new Error("Unsupported Version");
                    }
                    const server = API.OnmsServer.newBuilder(http.server.url)
                        .setName(http.server.name)
                        .setAuth(http.server.auth)
                        .setMetadata(metadata)
                        .build();
                    http.server = server;
                    return self.client;
                }).catch(function(e) {
                    // in case of error, reset the client, otherwise
                    // the datasource may never recover
                    self.clientWithMetadata = void 0;
                    throw e;
                });

          // Grafana functions that invoke the datasource expect the
          // promise to be one that is returned by $q.
          this.clientWithMetadata = this.$q.when(client) as angular.IPromise<Client>;
        }
        return this.clientWithMetadata as angular.IPromise<Client>;
    }

    // Inventory (node) related functions

    getNodeDao(): angular.IPromise<DAO.NodeDAO> {
        return this.getClientWithMetadata()
            .then((client) => this.$q.when(client.nodes()));
    }

    findNodes(filter): angular.IPromise<Model.OnmsNode[]> {
        return this.getNodeDao()
            .then((nodeDao) => this.$q.when(nodeDao.find(filter)))
            .catch(this.decorateError);
    }

    getNode(nodeId): angular.IPromise<Model.OnmsNode> {
      return this.getNodeDao()
        .then((nodeDao) => this.$q.when(nodeDao.get(nodeId)))
        .catch(this.decorateError);
    }

    getNodeProperties(): angular.IPromise<any[]> {
        return this.getNodeDao()
            .then((nodeDao) => this.$q.when(nodeDao.searchProperties())).catch(this.decorateError);
    }

    findNodeProperty(propertyId) {
        return this.getNodeProperties()
            .then((properties) => {
                return _.find(properties, (property) => property.id === propertyId);
            });
    }

    getNodePropertyComparators(propertyId): angular.IPromise<any[]> {
        return this.findNodeProperty(propertyId)
            .then(property => {
                if (property) {
                    const comparators = property.type.getComparators();
                    if (comparators && comparators.length > 0) {
                        return comparators;
                    }
                }
                console.warn(`No comparators found for property with id '${propertyId}'. Falling back to EQ.`);
                // This may be the case when the user entered a property, which does not exist
                // therefore fallback to EQ
                return [ API.Comparators.EQ ];
            }).catch(this.decorateError);
    }

    // Fault related functions

    getAlarmDao(): angular.IPromise<DAO.AlarmDAO> {
        return this.getClientWithMetadata()
            .then((client) => this.$q.when(client.alarms()));
    }

    findAlarms(filter): angular.IPromise<Model.OnmsAlarm[]> {
        return this.getAlarmDao()
            .then((alarmDao) => this.$q.when(alarmDao.find(filter)))
            .catch(this.decorateError);
    }

    getAlarm(alarmId): angular.IPromise<Model.OnmsAlarm> {
      return this.getAlarmDao()
        .then((alarmDao) => this.$q.when(alarmDao.get(alarmId)))
        .catch(this.decorateError);
    }

    doEscalate(alarmId, user) {
        return this.getAlarmDao()
            .then((alarmDao) => this.$q.when(alarmDao.escalate(alarmId, user)))
            .catch(this.decorateError);
    }

    doClear(alarmId, user) {
        return this.getAlarmDao()
            .then((alarmDao) => this.$q.when(alarmDao.clear(alarmId, user)))
            .catch(this.decorateError);
    }

    doUnack(alarmId, user) {
        return this.getAlarmDao()
            .then((alarmDao) => this.$q.when(alarmDao.unacknowledge(alarmId, user)))
            .catch(this.decorateError);
    }

    doAck(alarmId, user) {
        return this.getAlarmDao()
            .then((alarmDao) => this.$q.when(alarmDao.acknowledge(alarmId, user)))
            .catch(this.decorateError);
    }

    doTicketAction(alarmId, action) {
        const supportedActions = ["create", "update", "close"];
        if (supportedActions.indexOf(action) < 0) {
            throw {message: "Action '" + action + "' not supported."};
        }
        const self = this;
        return this.backendSrv.datasourceRequest({
            url: self.url + '/api/v2/alarms/' + alarmId + "/ticket/" + action,
            method: 'POST',
        }).catch(this.decorateError);
    }

    saveSticky(alarmId, sticky, user) {
      return this.getAlarmDao()
        .then((alarmDao) => this.$q.when(alarmDao.saveStickyMemo(alarmId, sticky, user)))
        .catch(this.decorateError);
    }

    deleteSticky(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.deleteStickyMemo(alarmId);
        }).catch(this.decorateError);
    }

    saveJournal(alarmId, journal, user) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.saveJournalMemo(alarmId, journal, user);
        }).catch(this.decorateError);
    }

    deleteJournal(alarmId) {
      return this.getAlarmDao()
        .then(function(alarmDao) {
          return alarmDao.deleteJournalMemo(alarmId);
        }).catch(this.decorateError);
    }

    findOperators(): angular.IPromise<API.Operator[]> {
        const operators = _.map(API.Operators, (operator) => {
            return {
                id: operator.id,
                label: operator.label
            }
        });
        return this.$q.when(operators);
    }

    getAlarmProperties(): angular.IPromise<API.SearchProperty[]> {
        return this.getAlarmDao()
            .then(alarmDao => {
                return this.$q.when(alarmDao.searchProperties());
            }).catch(this.decorateError);
    }

    findAlarmProperty(propertyId): API.SearchProperty {
        return this.getAlarmProperties()
            .then(properties => {
                return _.find(properties, function(property) {
                    return property.id === propertyId;
                });
            });
    }

    getAlarmPropertyComparators(propertyId): Promise<API.Comparator[]> {
        return this.findAlarmProperty(propertyId)
            .then(property => {
                if (property) {
                    const comparators = property.type.getComparators();
                    if (comparators && comparators.length > 0) {
                        return comparators;
                    }
                }
                console.warn(`No comparators found for property with id '${propertyId}'. Falling back to EQ.`);
                // This may be the case when the user entered a property, which does not exist
                // therefore fallback to EQ
                return [ API.Comparators.EQ ];
            }).catch(this.decorateError);
    }


    // Situation Feedback functions

    getSituationfeedbackDao(): angular.IPromise<DAO.SituationFeedbackDAO> {
        return this.getClientWithMetadata()
            .then((client) => this.$q.when(client.situationfeedback()))
            .catch(this.decorateError);
    }

    getSituationfeedback(situationId): angular.IPromise<Model.OnmsSituationFeedback> {
        return this.getSituationfeedbackDao()
        .then((dao) => this.$q.when(dao.getFeedback(situationId)))
        .catch(this.decorateError);
    }

    submitSituationFeedback(situationId, feedback): angular.IPromise<any> {
        return this.getSituationfeedbackDao()
            .then((dao) => this.$q.when(dao.saveFeedback(feedback, situationId)))
            .catch(this.decorateError);
    }

    // Flow related functions
    // FIXME: angular.IPromise<DAO.FlowDAO>
    getFlowDao(): angular.IPromise<any> {
        return this.getClientWithMetadata().then(function(c) {
            return c.flows();
        }).catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getApplications(prefix, start, end, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getApplications(prefix, start, end, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowSeries>
    getSeriesForTopNApplications(N, start, end, step, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSeriesForTopNApplications(N, start, end, step, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowSeries>
    getSeriesForApplications(applications, start, end, step, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSeriesForApplications(applications, start, end, step, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getSummaryForTopNApplications(N, start, end, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSummaryForTopNApplications(N, start, end, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getSummaryForApplications(applications, start, end, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSummaryForApplications(applications, start, end, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowSeries>
    getSeriesForTopNConversations(N, start, end, step, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSeriesForTopNConversations({
                    N: N,
                    start: start,
                    end: end,
                    step: step,
                    exporterNode: nodeCriteria,
                    ifIndex: interfaceId,
                    dscp: dscp,
                    includeOther: includeOther,
                }))
            ).catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowSeries>
    getSeriesForConversations(conversations, start, end, step, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSeriesForConversations(conversations, start, end, step, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getSummaryForTopNConversations(N, start, end, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSummaryForTopNConversations({
                    N: N,
                    start: start,
                    end: end,
                    exporterNode: nodeCriteria,
                    ifIndex: interfaceId,
                    dscp: dscp,
                    includeOther: includeOther
                }))
            ).catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getSummaryForConversations(conversations, start, end, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSummaryForConversations(conversations, start, end, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getHosts(prefix, start, end, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getHosts(prefix + '.*', start, end, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowSeries>
    getSeriesForHosts(hosts, start, end, step, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSeriesForHosts(hosts, start, end, step, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowSeries>
    getSeriesForTopNHosts(N, start, end, step, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSeriesForTopNHosts(N, start, end, step, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getSummaryForTopNHosts(N, start, end, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSummaryForTopNHosts(N, start, end, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowTable>
    getSummaryForHosts(hosts, start, end, includeOther, nodeCriteria, interfaceId, dscp): angular.IPromise<any> {
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getSummaryForHosts(hosts, start, end, includeOther, nodeCriteria, interfaceId, dscp)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowExporterSummary[]>
    getExporters(): angular.IPromise<any[]> {
        const searchLimit = this.searchLimit;
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getExporters(searchLimit)))
            .catch(this.decorateError);
    }

    // FIXME: angular.IPromise<Model.OnmsFlowExporter>
    getExporter(nodeCriteria) {
        const searchLimit = this.searchLimit;
        return this.getFlowDao()
            .then((dao) => this.$q.when(dao.getExporter(nodeCriteria, searchLimit)))
            .catch(this.decorateError);
    }

    getDscpValues(nodeCriteria, interfaceId, start, end) {
        return this.getClientWithMetadata().then(function(c) {
            const metadata = c.http.server.metadata;
            if (metadata.tos()) {
                return c.flows().getDscpValues(nodeCriteria, interfaceId, start, end);
            } else {
                return Promise.resolve([]);
            }
        }).catch(this.decorateError);
    }

    getSummaryForDscps(start, end, nodeCriteria, interfaceId, dscp) {
      return this.getFlowDao()
          .then(function(flowDao) {
            return flowDao.getSummaryForDscps(start, end, nodeCriteria, interfaceId, dscp);
          }).catch(this.decorateError);
    }

    getSeriesForDscps(start, end, step, nodeCriteria, interfaceId, dscp) {
      return this.getFlowDao()
          .then(function(flowDao) {
            return flowDao.getSeriesForDscps(start, end, step, nodeCriteria, interfaceId, dscp);
          }).catch(this.decorateError);
    }

}
