import {API, Model} from '../../opennms';

export class Example {
    constructor(description, apiFilter) {
        this.apiFilter = apiFilter;
        this.description = description;
    }
}

// Some examples to use
export const Examples = [
    new Example("Show all alarms", new API.Filter()),
    new Example("Show all alarms for location 'Default'",
        new API.Filter().withAndRestriction(new API.Restriction("node.location.locationName", API.Comparators.EQ, 'Default'))),
    new Example("Show all alarms with a severity between 'Warning' and 'Major'",
        new API.Filter()
            .withAndRestriction(new API.Restriction("alarm.severity", API.Comparators.GE, Model.Severities.WARNING.label))
            .withAndRestriction(new API.Restriction("alarm.severity", API.Comparators.LE, Model.Severities.MAJOR.label))),
    new Example("Show all alarms for nodes in category 'Servers'",
        new API.Filter().withAndRestriction(new API.Restriction("category.name", API.Comparators.EQ, 'Severs'))),
    new Example("Show all unacknowledged alarms",
        new API.Filter().withAndRestriction(new API.Restriction("alarmAckTime", API.Comparators.EQ, 'null'))),
    new Example("Show all acknowledged alarms",
        new API.Filter().withAndRestriction(new API.Restriction("alarmAckTime", API.Comparators.NE, 'null'))),

];

