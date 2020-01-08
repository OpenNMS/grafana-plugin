import {Model} from 'opennms';

export class TableModel {
  constructor() {
    this.columns = [];
    this.rows = [];
    this.meta = {
      entity_metadata: [],
    };
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

    // We need to sort both the rows and entity meta-data arrays the same way
    // 1) combine the arrays:
    let rowsAndMeta = [];
    for (let i = 0; i < this.rows.length; i++) {
      rowsAndMeta.push({'row': this.rows[i], 'meta': this.meta.entity_metadata[i]});
    }

    // 2) sort
    rowsAndMeta.sort(function(combinedA, combinedB) {
      // Only look at the row values when sorting
      var a = combinedA.row;
      var b = combinedB.row;
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

    if (options.desc) {
      rowsAndMeta.reverse();
      this.columns[options.col].desc = true;
    } else {
      this.columns[options.col].desc = false;
    }

    // 3) separate them back out:
    for (let i = 0; i < this.rows.length; i++) {
      this.rows[i] = rowsAndMeta[i].row;
      this.meta.entity_metadata[i] = rowsAndMeta[i].meta;
    }

    this.columns.forEach(col => col.sort = false);
    this.columns[options.col].sort = true;


  }
}
