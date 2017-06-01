import {MetricsPanelCtrl} from  'app/plugins/sdk';

import _ from 'lodash';


class AlarmHistogramCtrl extends MetricsPanelCtrl {

    constructor($scope, $injector) {
        super($scope, $injector);

        _.defaults(this.panel,  {
            groupProperty: 'acknowledged',
        });

        this.events.on('render', this.onRender.bind(this));
        this.events.on('data-received', this.onDataReceived.bind(this));
        this.events.on('data-error', this.onDataError.bind(this));
        this.events.on('data-snapshot-load', this.onDataReceived.bind(this));
        this.events.on('init-edit-mode', this.onInitEditMode.bind(this));
    }

    onRender() {

    }

    onDataReceived(data) {
        console.log(data);

        switch (this.panel.groupProperty) {
            case 'acknowledged': {
                const counts = _.countBy(this.query(data, 'AckedBy'), _.isNil);
                this.series = [
                    {
                        name: "Pending",
                        count: _.defaultTo(counts[true], 0),
                    },
                    {
                        name: "Acknowledged",
                        count: _.defaultTo(counts[false], 0),
                    },
                ];
                break;
            }

            case 'severity': {
                const counts = _.countBy(this.query(data, 'Severity'));
                this.series = _.map(counts, function (group, count) {
                    return {
                        name: group,
                        count: count,
                    }
                });
                break;
            }
        }

        console.log(this.series);

        this.render();
    }

    onDataError() {
        this.series = [];
        this.render();
    }

    onInitEditMode() {
        this.addEditorTab('Grouping', 'public/plugins/opennms-helm-app/panels/alarm-histogram/editor.html', 2);
    }

    query(data, column) {
        // TODO: Make this a generator to save memory
        let result = []

        for (let i = 0; i < data.length; i++) {
            let columnIndex = _.findIndex(data[i].columns, {text: column});
            for (let j = 0; j < data[i].rows.length; j++) {
                result.push(data[i].rows[j][columnIndex]);
            }
        }

        return result;
    }
}

AlarmHistogramCtrl.templateUrl = 'panels/alarm-histogram/module.html';

export {
    AlarmHistogramCtrl
};
