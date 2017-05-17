import {PanelCtrl} from  'app/plugins/sdk';

class AlarmHistogramCtrl extends PanelCtrl {

    constructor($scope, $injector) {
        super($scope, $injector);
    }

}
AlarmHistogramCtrl.template = '<h2>OpenNMS Helm - Alarm Histogram Panel!<h2>';

export {
    AlarmHistogramCtrl as PanelCtrl
};
