import _ from 'lodash';

export class OnmsSeverity {
    constructor() {
        this.severities = [
            {id: 1, label: 'Indeterminate'},
            {id: 2, label: 'Cleared'},
            {id: 3, label: 'Normal'},
            {id: 4, label: 'Warning'},
            {id: 5, label: 'Minor'},
            {id: 6, label: 'Major'},
            {id: 7, label: 'Critical'}
        ];
    }

    getSeverityById(id) {
        var severity =_.find(this.severities, severity => {
            return severity.id === id
        });
        return severity;
    }

    getSeverityByLabel(label) {
        var severity = _.find(this.severities, severity => {
            return severity.label === label;
        });
        return severity;
    }

    getSeverities() {
        return this.severities;
    }
}