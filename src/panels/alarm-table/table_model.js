import {Model} from '../../opennms';

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

      if (colInfo.style.type === 'severity') {
        a = self.severityForLabel(a[options.col]);
        b = self.severityForLabel(b[options.col]);
      } else {
        a = a[options.col];
        b = b[options.col];
      }

      if (a < b) {
        return -1;
      }
      if (a > b) {
        return 1;
      }
      return 0;
    });

    this.columns[options.col].sort = true;

    if (options.desc) {
      this.rows.reverse();
      this.columns[options.col].desc = true;
    } else {
      this.columns[options.col].desc = false;
    }
  }
}
