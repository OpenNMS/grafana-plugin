import {OpenNMSFMDatasource} from './datasource';
import {OpenNMSFMDatasourceQueryCtrl} from './query_ctrl';
import {Examples} from './Examples';
import '../../components/timeout';

class OpenNMSFMDatasourceConfigCtrl {}
OpenNMSFMDatasourceConfigCtrl.templateUrl = 'datasources/fault-ds/partials/config.html';

class OpenNMSFMDatasourceQueryOptionsCtrl {
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
OpenNMSFMDatasourceQueryOptionsCtrl.templateUrl = 'datasources/fault-ds/partials/query.options.html';

export {
  OpenNMSFMDatasource as Datasource,
  OpenNMSFMDatasourceQueryCtrl as QueryCtrl,
  OpenNMSFMDatasourceConfigCtrl as ConfigCtrl,
  OpenNMSFMDatasourceQueryOptionsCtrl as QueryOptionsCtrl,
};
