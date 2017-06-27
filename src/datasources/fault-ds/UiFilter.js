import _ from 'lodash';
import {API} from '../../opennms';

export class UiFilter {

    constructor(uiSegmentSrv) {
        this.uiSegmentSrv = uiSegmentSrv;
        this.table = new Table(this.uiSegmentSrv);
    }

    addPlusButtonIfRequired() {
        this.table.addPlusButtonIfRequired();
    }

    clear() {
        this.table = new Table(this.uiSegmentSrv);
        this.addPlusButtonIfRequired();
    }

    removeRow(row) {
        var removed = this.table.removeRow(row);
        this.addPlusButtonIfRequired();
        return removed;
    }

    addRow() {
        this.table.addRow(new Row(this.uiSegmentSrv));
        return this.table.getLastRow();
    }

    getSize() {
        return this.table.getSize();
    }

    toFilter() {
        const self = this;
        const filter = new API.Filter();
        _.each(this.table.rows, function(eachRow) {
            const data = eachRow.getData();
            if (data) {
                const operator = _.find(API.Operators, function(operator) {
                    return operator.matches(eachRow.operator);
                });
                const restriction = self.toRestriction(data);
                const clause = new API.Clause(restriction, operator);
                filter.withClause(clause);
            }
        });
        return filter;
    }

    addRowFromClause(clause) {
        const attribute = new AttributeMapping().getInternalAttribute(clause.restriction.attribute);
        const comparator = new ComparatorMapping().getInternalComparator(clause.restriction.comparator);
        const value = new ValueMapping().getInternalValue(attribute, clause.restriction.value);
        const operator = new OperatorMapping().getInternalValue(clause.operator);

        const row = this.addRow();
        row.setAttribute(attribute);
        row.setComparator(comparator);
        row.setValue(value);
        row.setOperator(operator);
    }

    toRestriction(data) {
        const attribute = new AttributeMapping().getExternalAttribute(data.attribute);
        const comparator = new ComparatorMapping().getExternalComparator(data.comparator);
        const value = new ValueMapping().getExternalValue(data.attribute, data.value);
        return new API.Restriction(attribute, comparator, value);
    }

    toString() {
        let query = "select all alarms";

        if (this.table.rows.length == 0) {
            return query;
        }
        let rowText = this.renderRows(this.table.rows);
        if (rowText && rowText.length > 0) {
            return query + " WHERE " + rowText;
        }
        return query;
    }

    renderRows(rows) {
        const getComparator = function(restriction) {
            if (restriction.value === 'null') {
                if (restriction.comparator === '=') {
                    return "is";
                }
                if (restriction.comparator === '!=') {
                    return "is not";
                }
            }
            return restriction.comparator;
        };

        const getValue = function(restriction) {
            if (restriction.value === 'null') {
                return 'null';
            }
            return "'" + restriction.value + "'";
        };

        var rows = _.filter(rows, function(row) {
            return row.getData() != null;
        });

        var query = _.map(rows, function(row, index) {
            let string = '';
            if (index > 0) {
                string = " " + row.operator + " ";
            }
            const restriction = row.getData();
            const restrictionString = restriction.attribute + " " + getComparator(restriction) + " " + getValue(restriction);
            string += restrictionString;
            return string;

        }).join("");
        return query;
    }

    segmentUpdated(row, segment) {
        // If plus button was clicked, it is now an input field
        if (segment.type === 'plus-button') {
            // make the plus button an actual attribute input
            segment.type = 'key';
            segment.cssClass = 'query-segment-key';

            // It is the last element in the row, so move it to it's own row
            const segmentIndex = row.columns.indexOf(segment);
            if (segmentIndex > 0) {
                // remove plus button
                row.removeLastColumn();

                // Create new row
                row = this.addRow();

                // add key (was plus button)
                row.addColumn(segment);
            }

            // Add comparator and value
            row.addColumn(this.uiSegmentSrv.newOperator('='));
            row.addColumn(this.uiSegmentSrv.newFake('select attribute value', 'value', 'query-segment-value'));
        }

        if (segment.type === 'value') {
            segment.fake = false;
        }

        // Ensure that we always have a plus button
        this.addPlusButtonIfRequired();
    }
}

class Table {
    constructor(uiSegmentSrv) {
        this.rows = [];
        this.uiSegmentSrv = uiSegmentSrv;
    }

    getSize() {
        return this.rows.length;
    }

    getLastRow() {
        if (this.rows.length == 0) {
            return null;
        }
        return this.rows[this.getSize() - 1];
    }

    addPlusButtonIfRequired() {
        // at least one row should be available even if it is an empty row
        if (this.getSize() == 0) {
            this.rows.push(new Row(this.uiSegmentSrv));
        }
        const lastRow = this.getLastRow();
        lastRow.addPlusButtonIfRequired();
    }

    addRow(row) {
        if (row) {
            this.rows.push(row);
            if (!row.uiSegmentSrv) {
                row.uiSegmentSrv = this.uiSegmentSrv;
            }
        }
    }

    removeRow(row) {
        if (row) {
            var index = this.rows.indexOf(row);
            if (index >= 0) {
                this.rows.splice(index, 1);
                this.addPlusButtonIfRequired();
                return true;
            }
        }
    }
}

class Row {

    constructor(uiSegmentSrv) {
        this.operator = 'AND'; // TODO MVR use Operators.AND // OR
        this.columns = [];
        this.uiSegmentSrv = uiSegmentSrv;
    }

    addPlusButtonIfRequired() {
        const columnIndex = Math.max(this.getColumnCount() - 1, 0);
        const lastSegment = this.columns[columnIndex];
        if (!lastSegment || lastSegment.type !== 'plus-button') {
            this.columns.push(this.uiSegmentSrv.newPlusButton());
        }
    }

    getColumnCount() {
        return this.columns.length;
    }

    removeLastColumn() {
        this.columns.pop();
    }

    addColumn(segment) {
        if (segment) {
            this.columns.push(segment);
        }
    }

    setAttribute(attribute) {
        if (this.columns.length == 0) {
            this.columns.push({});
        }
        this.columns[0] = this.uiSegmentSrv.newKey(attribute);
    }

    setComparator(comparator) {
        if (this.columns.length == 1) {
            this.columns.push({});
        }
        this.columns[1] = this.uiSegmentSrv.newOperator(comparator);
    }

    setValue(value) {
        if (this.columns.length == 2) {
            this.columns.push({});
        }
        this.columns[2] = this.uiSegmentSrv.newKeyValue(value);
    }

    setOperator(operator) {
        this.operator = operator;
    }

    getData() {
        const columns = _.filter(this.columns, function(segment) {
            return segment.type !== 'plus-button' && (segment.fake === undefined || segment.fake === false)
        });
        if (columns.length > 0 && columns.length % 3 == 0) {
            var data = {};
            _.each(columns, (segment, segmentIndex) => {
                if (segment.type === 'key') {
                    data.attribute = segment.value;
                } else if (segment.type === 'operator') {
                    data.comparator = segment.value;
                } else if (segment.type === 'value') {
                    data.value = segment.value;
                }
            });
            return data;
        }
        return null;
    }
}

class AttributeMapping {
    constructor() {
        this.attributeMapping = {
            'location': 'node.location.locationName',
            'service': 'service',
            'ipAddress': 'ipAddr',
            'severity': 'alarm.severity'
        };
    }

    getInternalAttribute(externalAttribute) {
        const internalAttribute = _.findKey(this.attributeMapping, function(value) {
            return value === externalAttribute;
        });
        return internalAttribute || externalAttribute;
    }

    getExternalAttribute(internalAttribute) {
        return this.attributeMapping[internalAttribute] || internalAttribute;
    }
}

class ComparatorMapping {
    getInternalComparator(externalComparator) {
        const theComparator = API.Comparators[externalComparator.label];
        if (theComparator.aliases && theComparator.aliases.length > 0) {
            return theComparator.aliases[0];
        }
        throw {message: "No alias for comparator " + externalComparator.label + " defined", comparator: externalComparator}
    }

    getExternalComparator(internalComparator) {
        const externalComparator = _.find(API.Comparators, function(comparator) {
            return comparator.matches(internalComparator);
        });
        if (!externalComparator) {
            throw { message: "No comparator found for alias " + internalComparator + " mapping found", comparators: API.Comparators };
        }
        return externalComparator;
    }
}

class ValueMapping {
    getExternalValue(internalAttribute, value) {
        if (internalAttribute === 'alarmAckTime') {
            if (value === 'null') {
                value = new Date(0);
            }

            // yyyy-MM-ddTHH:mm:ss.sssZ
            // 2017-06-08T10:17:17.173+0200
            value = value.toJSON().replace("Z", "+0000"); // make it parsable by java
        }
        if ("null" === value) {
            value = '\u0000';
        }
        return encodeURIComponent(value);
    }

    getInternalValue(internalAttribute, value) {
        if (internalAttribute === 'alarmAckTime') {
            if (value === new Date(0)) {
                return 'null';
            }
            var stringified = JSON.parse(value.replace("+0000", "Z"));
            return new Date(stringified);
        }
        if (value === "\u0000") {
            return "null";
        }
        return decodeURIComponent(value);
    }
}

class OperatorMapping {
    getInternalValue(operator) {
        const theOperator = API.Operators[operator.label];
        if (theOperator && theOperator.label) {
            return theOperator.label;
        }
        throw { message: "No operator found with label " + operator.label, operators: API.Operators};
    }
}