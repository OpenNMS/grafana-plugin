import {OpenNMSFMDatasourceModalCtrl} from './search_modal_ctrl';

export class OpenNMSFMDatasourceSearchService {
    constructor(datasource, $rootScope, $q, $modal) {
        this.type = datasource.type;
        this.url = datasource.url;
        this.name = datasource.name;
        this.backendSrv = datasource.backendSrv;
        this.searchLimit = 100; // TODO MVR?!
        this.$q = $q;
        this.$rootScope = $rootScope;
        this.$modal = $modal;
    }

    search(query) {
        if (query.find === 'users') {

        }
        if (query.find === 'nodes') {
            this.showNodesDialog();
        }
        if (query.find === 'locations') {

        }
        if (query.find === 'categories') {

        }
        if (query.find === 'severities') {

        }
    }

    showNodesDialog(callback) {
        let self = this;
        this.showSelectionModal("nodes", {
            '#': 'id',
            'Label': 'label',
            'Foreign ID': 'foreignId',
            'sysName': 'sysName',
        }, function (query) {
            return self.backendSrv.datasourceRequest({
                url: self.url + '/rest/nodes',
                method: 'GET',
                params: {
                    limit: self.searchLimit,
                    match: 'any',
                    comparator: 'ilike',
                    orderBy: 'id',
                    order: 'asc',
                    label: '%' + query + '%',
                    sysName: '%' + query + '%',
                    'ipInterface.ipAddress': '%' + query + '%',
                    'ipInterface.ipHostName': '%' + query + '%',
                    'foreignId': query + '%' // doesn't support leading '%'
                }
            }).then(function (results) {
                    return {
                        'count': results.data.count,
                        'totalCount': results.data.totalCount,
                        'rows': results.data.node
                    };
                });
        }, function(result) {
            if (callback) {
                callback(result);
            }
        });
    }

    showSelectionModal(label, columns, search, callback) {
        let scope = this.$rootScope.$new();
        scope.label = label;
        scope.columns = columns;
        scope.search = search;
        scope.result = this.$q.defer();
        scope.result.promise.then(callback);

        let modal = this.$modal({
            template: 'public/plugins/opennms-helm-app/datasources/fault-ds/partials/modal.html',
            persist: false,
            show: false,
            scope: scope,
            keyboard: false
        });
        this.$q.when(modal).then(function (theModal) { theModal.modal('show'); });
    }
}