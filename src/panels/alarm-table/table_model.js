import {Model} from 'opennms';

export class TableModel {
  constructor() {
    this.columns = [];
    this.rows = [];
    this.type = 'table';
  }

  severityForLabel(label) {
    const sev = Model.Severities[label];
    if (sev) {
      return sev.id;
    } else {
      console.warn('Unable to determine severity for "' + label + '".');
      return -1;
    }
  }

  sort(options) {
    if (options.col === null || this.columns.length <= options.col) {
      return;
    }

    var self = this;
    this.rows.sort(function(a, b) {
      const colInfo = self.columns[options.col];

      // by default just use the column as-is (a string)
      a = '' + a[options.col];
      b = '' + b[options.col];

      if (colInfo && colInfo.style) {
        const type = colInfo.style.type;

        if (type === 'number') {
          // if it's a number type, cast it
          a = Number(a);
          b = Number(b);
        } else if (type === 'severity') {
          // if it's a severity, get the numeric value
          a = self.severityForLabel(a);
          b = self.severityForLabel(b);
        }
      }

      if (a < b) {
        return -1;
      }
      if (a > b) {
        return 1;
      }
      return 0;
    });

    this.columns.forEach(col => col.sort = false);
    this.columns[options.col].sort = true;

    if (options.desc) {
      this.rows.reverse();
      this.columns[options.col].desc = true;
    } else {
      this.columns[options.col].desc = false;
    }
  }
}
