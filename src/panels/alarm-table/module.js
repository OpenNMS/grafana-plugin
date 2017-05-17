import {PanelCtrl} from  'app/plugins/sdk';

class AlarmTableCtrl extends PanelCtrl {

    constructor($scope, $injector) {
        super($scope, $injector);
    }

}
AlarmTableCtrl.template = '<h2>OpenNMS Helm - Alarm Table Panel!<h2>';

export {
    AlarmTableCtrl as PanelCtrl
};
