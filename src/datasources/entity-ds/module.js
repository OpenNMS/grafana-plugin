import {OpenNMSEntityDatasource} from './datasource';
import {OpenNMSEntityDatasourceQueryCtrl} from './query_ctrl';
import {Examples} from './Examples';
import '../../components/timeout';

class OpenNMSEntityDatasourceConfigCtrl {}
OpenNMSEntityDatasourceConfigCtrl.templateUrl = 'datasources/entity-ds/partials/config.html';

class OpenNMSEntityDatasourceQueryOptionsCtrl {
  /** @ngInject */
  constructor(uiSegmentSrv) {
    this.uiSegmentSrv = uiSegmentSrv;
    this.examples = Examples;
  }

  createQueryFromExample(example) {
      const target = {
        isNew: true,
        filter: example.apiFilter
      };

      this.panelCtrl.panel.targets.push(target);
  }
}
OpenNMSEntityDatasourceQueryOptionsCtrl.templateUrl = 'datasources/entity-ds/partials/query.options.html';

export {
  OpenNMSEntityDatasource as Datasource,
  OpenNMSEntityDatasourceQueryCtrl as QueryCtrl,
  OpenNMSEntityDatasourceConfigCtrl as ConfigCtrl,
  OpenNMSEntityDatasourceQueryOptionsCtrl as QueryOptionsCtrl,
};
