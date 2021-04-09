import { OpenNMSEntityDatasource } from './datasource';
import { OpenNMSEntityDatasourceQueryCtrl } from './query_ctrl';
import { Example, Examples } from './Examples';
import '../../components/timeout';

class OpenNMSEntityDatasourceConfigCtrl {
  static templateUrl = 'datasources/entity-ds/partials/config.html';
}

class OpenNMSEntityDatasourceQueryOptionsCtrl {
  static templateUrl = 'datasources/entity-ds/partials/query.options.html';

  examples: Example[];
  panelCtrl: any;
  uiSegmentSrv: any;

  /** @ngInject */
  constructor(uiSegmentSrv: any) {
    this.uiSegmentSrv = uiSegmentSrv;
    this.examples = Examples;
  }

  createQueryFromExample(example: Example) {
    const target = {
      isNew: true,
      filter: example.apiFilter,
    };

    this.panelCtrl.panel.targets.push(target);
  }
}

export {
  OpenNMSEntityDatasource as Datasource,
  OpenNMSEntityDatasourceQueryCtrl as QueryCtrl,
  OpenNMSEntityDatasourceConfigCtrl as ConfigCtrl,
  OpenNMSEntityDatasourceQueryOptionsCtrl as QueryOptionsCtrl,
};
