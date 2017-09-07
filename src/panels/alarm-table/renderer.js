import _ from 'lodash';
import moment from 'moment';
import kbn from 'app/core/utils/kbn';

export class TableRenderer {

  constructor(panel, table, isUtc, sanitize, selectionMgr) {
    this.panel = panel;
    this.table = table;
    this.isUtc = isUtc;
    this.sanitize = sanitize;
    this.selectionMgr = selectionMgr;

    this.initColumns();
  }

  setTable(table) {
    this.table = table;

    this.initColumns();
  }

  initColumns() {
    this.formatters = [];
    this.colorState = {};

    for (let colIndex = 0; colIndex < this.table.columns.length; colIndex++) {
      let column = this.table.columns[colIndex];
      column.title = column.text;

      for (let i = 0; i < this.panel.styles.length; i++) {
        let style = this.panel.styles[i];

        let regex = kbn.stringToJsRegex(style.pattern);
        if (column.text.match(regex)) {
          column.style = style;

          if (style.alias) {
            column.title = column.text.replace(regex, style.alias);
          }

          break;
        }
      }

      this.formatters[colIndex] = this.createColumnFormatter(column);
    }
  }

  static getColorForValue(value, style) {
    if (!style.thresholds) {
      return null;
    }

    for (let i = style.thresholds.length; i > 0; i--) {
      if (value >= style.thresholds[i - 1]) {
        return style.colors[i];
      }
    }
    return _.first(style.colors);
  }

  defaultCellFormatter(v, style) {
    if (v === null || v === void 0 || v === undefined) {
      return '';
    }

    if (_.isArray(v)) {
      v = v.join(', ');
    }

    if (style && style.sanitize) {
      return this.sanitize(v);
    } else {
      return _.escape(v);
    }
  }

  createColumnFormatter(column) {
    if (!column.style) {
      return this.defaultCellFormatter;
    }

    if (column.style.type === 'hidden') {
      return v => {
        return undefined;
      };
    }

    if (column.style.type === 'date') {
      return v => {
        if (v === undefined || v === null) {
          return '-';
        }

        if (_.isArray(v)) {
          v = v[0];
        }
        let date = moment(v);
        if (this.isUtc) {
          date = date.utc();
        }
        return date.format(column.style.dateFormat);
      };
    }

    if (column.style.type === 'number') {
      let valueFormatter = kbn.valueFormats[column.unit || column.style.unit];

      return v => {
        if (v === null || v === void 0) {
          return '-';
        }

        if (_.isString(v)) {
          return this.defaultCellFormatter(v, column.style);
        }

        if (column.style.colorMode) {
          this.colorState[column.style.colorMode] = TableRenderer.getColorForValue(v, column.style);
        }

        return valueFormatter(v, column.style.decimals, null);
      };
    }

    return (value) => {
      return this.defaultCellFormatter(value, column.style);
    };
  }

  formatColumnValue(colIndex, value) {
    return this.formatters[colIndex] ? this.formatters[colIndex](value) : value;
  }

  renderCell(columnIndex, value, addWidthHack = false) {
    value = this.formatColumnValue(columnIndex, value);
    let column = this.table.columns[columnIndex];
    let styles = {};
    let classes = [];

    if (this.colorState.cell) {
      styles['background-color'] = this.colorState.cell;
      styles['color'] = 'white';
      this.colorState.cell = null;
    } else if (this.colorState.value) {
      styles['color'] = this.colorState.value;
      this.colorState.value = null;
    }

    // because of the fixed table headers css only solution
    // there is an issue if header cell is wider the cell
    // this hack adds header content to cell (not visible)
    let widthHack = '';
    if (addWidthHack) {
      widthHack = '<div class="table-panel-width-hack">' + column.title + '</div>';
    }

    if (value === undefined) {
      styles['display'] = 'none';
      column.hidden = true;
    } else {
      column.hidden = false;
    }

    if (column.style.width) {
      styles['width'] = column.style.width;
      if (column.style.clip) {
        styles['max-width'] = column.style.width;
        styles['white-space'] = 'nowrap';
      }
    }

    if (column.style.clip) {
      styles['overflow'] = 'hidden';
      styles['text-overflow'] = 'ellipsis';
    }

    let stylesAsString = 'style="' + _.reduce(_.map(styles, function(val, key){ return key + ':' + val; }),
      (memo, style) => {
        if (memo.length > 0) {
          return memo + '; ' + style;
        } else {
          return style;
        }
      }, '') + '"';

    return '<td ' + stylesAsString + '>' + value + widthHack + '</td>';
  }

  static getIconForSeverity(severity) {
    let icon = 'ion-help';
    switch(severity) {
      case 'indeterminate':
        icon = 'ion-help';
        break;
      case 'warning':
        icon = 'ion-alert-circled';
        break;
      case 'minor':
        icon = 'ion-flash';
        break;
      case 'major':
        icon = 'ion-flame';
        break;
      case 'critical':
        icon = 'ion-nuclear';
        break;
      case 'normal':
        icon = 'ion-leaf';
        break;
      case 'cleared':
        icon = 'ion-checkmark-circled';
        break;
    }
    return icon;
  }

  isRowSelected(row) {
    return this.selectionMgr.isRowSelected({
      source: row.meta.source,
      alarmId: row.meta.alarm.id
    });
  }

  render(page) {
    let pageSize = this.panel.pageSize || 100;
    let startPos = page * pageSize;
    let endPos = Math.min(startPos + pageSize, this.table.rows.length);
    let html = "";

    for (let y = startPos; y < endPos; y++) {
      let row = this.table.rows[y];
      let nextRow;
      if (y+1 < endPos) {
        nextRow = this.table.rows[y+1];
      }

      let cellHtml = '';
      let rowStyle = '';
      let rowClasses = [];

      let source = row.meta.source.replace(/'/g, '\\\'');
      let alarm = row.meta.alarm;
      let severity = alarm.severity.label.toLowerCase();

      if (this.panel.severityIcons) {
        let icon = TableRenderer.getIconForSeverity(severity);
        cellHtml += `<td class="severity-icon text-center"><i class="icon ${icon}"></i></td>`;
      }

      for (let i = 0; i < this.table.columns.length; i++) {
        cellHtml += this.renderCell(i, row[i], y === startPos);
      }

      if (this.colorState.row) {
        rowStyle = ' style="background-color:' + this.colorState.row + ';color: white"';
        this.colorState.row = null;
      }

      if (this.panel.severity) {
        rowClasses.push(severity);
      }

      if (this.isRowSelected(row)) {
        rowClasses.push("selected");
      }

      if (nextRow && this.isRowSelected(nextRow)) {
        rowClasses.push("next-selected");
      }

      let rowClass = 'class="' + rowClasses.join(' ') + '"';
      html += '<tr ' + rowStyle + rowClass + ` ng-click="ctrl.onRowClick($event, '${source}', ${alarm.id})"  ng-dblclick="ctrl.onRowDoubleClick($event, '${source}', ${alarm.id})" context-menu="ctrl.getContextMenu($event, '${source}', ${alarm.id})">` + cellHtml + '</tr>';
    }

    return html;
  }

  render_values() {
    let rows = [];

    for (let y = 0; y < this.table.rows.length; y++) {
      let row = this.table.rows[y];
      let new_row = [];
      for (let i = 0; i < this.table.columns.length; i++) {
        new_row.push(this.formatColumnValue(i, row[i]));
      }
      rows.push(new_row);
    }
    return {
      columns: this.table.columns,
      rows: rows,
    };
  }
}
