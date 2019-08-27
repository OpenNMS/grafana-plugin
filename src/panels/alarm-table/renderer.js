import 'ionicons/dist/css/ionicons.css';

import _ from 'lodash';
import { getValueFormat, getColorFromHexRgbOrName, stringToJsRegex } from '@grafana/ui';

// Grafana 6.3+ uses `dateTime` from @grafana/data but we're staying compatible with 6.0+
// so always use `moment` (for now).
import moment from 'moment';

import {Model} from 'opennms';

moment.defineLocale('en-short', {
  parentLocale: 'en',
  relativeTime: {
    future: "+%s",
    past:   "%s",
    s  : "1s",
    ss : "%ds",
    m:  "1m",
    mm: "%dm",
    h:  "1h",
    hh: "%dh",
    d:  "1d",
    dd: "%dd",
    M:  "1m",
    MM: "%dm",
    y:  "1y",
    yy: "%dy"
  }
});

export class TableRenderer {
  /** @ngInject */
  constructor(panel, table, isUtc, sanitize, selectionMgr, templateSrv, theme) {
    this.panel = panel;
    this.table = table;
    this.isUtc = isUtc;
    this.sanitize = sanitize;
    this.selectionMgr = selectionMgr;
    this.templateSrv = templateSrv;
    this.theme = theme;

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
      const column = this.table.columns[colIndex];
      column.title = column.text;

      for (let i = 0; i < this.panel.styles.length; i++) {
        const style = this.panel.styles[i];

        const regex = stringToJsRegex(style.pattern);
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

  getColorForValue(value, style) {
    if (!style.thresholds) {
      return null;
    }
    for (let i = style.thresholds.length; i > 0; i--) {
      if (value >= style.thresholds[i - 1]) {
        return getColorFromHexRgbOrName(style.colors[i], this.theme);
      }
    }
    return getColorFromHexRgbOrName(_.first(style.colors), this.theme);
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
      return () => undefined;
    }

    if (column.style.type === 'date') {
      return v => {
        if (v === undefined || v === null) {
          return '-';
        }

        if (_.isArray(v)) {
          v = v[0];
        }

        // if is an epoch (numeric string and len > 12)
        if (_.isString(v) && !isNaN(v) && v.length > 12) {
          v = parseInt(v, 10);
        }

        let date = moment(v);

        if (this.isUtc) {
          date = date.utc();
        }

        if (column.style.dateFormat === 'relative') {
          return date.fromNow();
        } else if (column.style.dateFormat === 'relative-short') {
          const dur = moment.duration(moment().diff(date));
          return dur.locale('en-short').humanize();
        } else {
          return date.format(column.style.dateFormat);
        }
      };
    }

    if (column.style.type === 'string') {
      return v => {
        if (_.isArray(v)) {
          v = v.join(', ');
        }

        const mappingType = column.style.mappingType || 0;

        if (mappingType === 1 && column.style.valueMaps) {
          for (let i = 0; i < column.style.valueMaps.length; i++) {
            const map = column.style.valueMaps[i];

            if (v === null) {
              if (map.value === 'null') {
                return map.text;
              }
              continue;
            }

            // Allow both numeric and string values to be mapped
            if ((!_.isString(v) && Number(map.value) === Number(v)) || map.value === v) {
              this.setColorState(v, column.style);
              return this.defaultCellFormatter(map.text, column.style);
            }
          }
        }

        if (mappingType === 2 && column.style.rangeMaps) {
          for (let i = 0; i < column.style.rangeMaps.length; i++) {
            const map = column.style.rangeMaps[i];

            if (v === null) {
              if (map.from === 'null' && map.to === 'null') {
                return map.text;
              }
              continue;
            }

            if (Number(map.from) <= Number(v) && Number(map.to) >= Number(v)) {
              this.setColorState(v, column.style);
              return this.defaultCellFormatter(map.text, column.style);
            }
          }
        }

        if (v === null || v === void 0) {
          return '-';
        }

        this.setColorState(v, column.style);
        return this.defaultCellFormatter(v, column.style);
      };
    }

    if (column.style.type === 'number') {
      const valueFormatter = getValueFormat(column.unit || column.style.unit);

      return v => {
        if (v === null || v === void 0) {
          return '-';
        }

        if (isNaN(v) || _.isArray(v)) {
          return this.defaultCellFormatter(v, column.style);
        }

        this.setColorState(v, column.style);
        return valueFormatter(v, column.style.decimals, null);
      };
    }

    if (column.style.type === 'severity') {
      return v => {
        if (v === null || v === void 0) {
          return '-';
        }

        if (column.style.displayAs === 'label') {
          return Model.Severities[v].toDisplayString();
        } else if (column.style.displayAs === 'labelCaps') {
          return v;
        } else {
          var icon = TableRenderer.getIconForSeverity(v.toLowerCase());
          return `<i class="icon severity-icon ${icon}" title="${v}"></i>`;
        }
      };
    }

    if (column.style.type === 'checkbox') {
      return v => {
        // coerce the value into a boolean
        const checked = (''+v).match(/^(true|t|y|yes)$/i) !== null;

        // then turn the value to an icon
        return checked ? '\u2713' : '';
      };
    }

    return value => {
      return this.defaultCellFormatter(value, column.style);
    };
  }

  setColorState(value, style) {
    if (!style.colorMode) {
      return;
    }

    if (value === null || value === void 0 || _.isArray(value)) {
      return;
    }

    const numericValue = Number(value);
    if (isNaN(numericValue)) {
      return;
    }

    this.colorState[style.colorMode] = this.getColorForValue(numericValue, style);
  }

  renderRowVariables(rowIndex) {
    const scopedVars = {};
    let cellVariable;
    const row = this.table.rows[rowIndex];
    for (let i = 0; i < row.length; i++) {
      cellVariable = `__cell_${i}`;
      scopedVars[cellVariable] = { value: row[i], text: row[i] ? row[i].toString() : '' };
    }
    return scopedVars;
  }

  formatColumnValue(colIndex, value) {
    return this.formatters[colIndex] ? this.formatters[colIndex](value) : value;
  }

  renderCell(columnIndex, rowIndex, value, addWidthHack, columnClasses) {
    const title = !_.isNil(value) && _.isString(value) ? ' title="' + value.trim().replace(/"/g, '&quot;') + '"' : '';

    value = this.formatColumnValue(columnIndex, value);

    const column = this.table.columns[columnIndex];
    const cellStyles = [];
    let cellStyle = '';
    const textStyles = [];
    let textStyle = '';
    const cellClasses = [].concat(columnClasses);
    let cellClass = '';

    if (this.colorState.cell) {
      cellStyles.push('background-color:' + this.colorState.cell);
      cellStyles.push('color:white');
      cellClasses.push('table-panel-color-cell');
      this.colorState.cell = null;
    } else if (this.colorState.value) {
      textStyles.push('color:' + this.colorState.value);
      this.colorState.value = null;
    }

    // because of the fixed table headers css only solution
    // there is an issue if header cell is wider the cell
    // this hack adds header content to cell (not visible)
    let columnHtml = '';
    if (addWidthHack) {
      columnHtml = '<div class="table-panel-width-hack">' + column.title + '</div>';
    }

    if (value === undefined) {
      cellStyle = ' style="display:none;"';
      column.hidden = true;
    } else {
      column.hidden = false;
    }

    if (column.hidden === true) {
      return '';
    }

    if (column.style && column.style.preserveFormat) {
      cellClasses.push('table-panel-cell-pre');
    }

    if (column.style && column.style.align) {
      cellClasses.push('text-' + column.style.align);
    }

    if (column.style && column.style.width) {
      textStyles.push('width:' + column.style.width);
      if (column.style.clip) {
        textStyles.push('max-width:' + column.style.width);
        textStyles.push('white-space:nowrap');
      }
    }

    if (column.style && column.style.clip) {
      textStyles.push('overflow:hidden');
      textStyles.push('text-overflow:ellipsis');
      if (!column.style.preserveFormat) {
        textStyles.push('white-space:nowrap');
      }
    }

    if (textStyles.length) {
      textStyle = ' style="' + textStyles.join(';') + '"';
    }

    if (column.style && column.style.link) {
      // Render cell as link
      const scopedVars = this.renderRowVariables(rowIndex);
      scopedVars['__cell'] = { value: value, text: value ? value.toString() : '' };

      const cellLink = this.templateSrv.replace(column.style.linkUrl, scopedVars, encodeURIComponent);
      const cellLinkTooltip = this.templateSrv.replace(column.style.linkTooltip, scopedVars);
      const cellTarget = column.style.linkTargetBlank ? '_blank' : '';

      cellClasses.push('table-panel-cell-link');

      columnHtml += `
        <a href="${cellLink}" target="${cellTarget}" data-link-tooltip data-original-title="${cellLinkTooltip}" data-placement="right"${textStyle}>
          ${value}
        </a>
      `;
      textStyle = '';
    } else {
      columnHtml += value;
    }

    if (column.filterable) {
      cellClasses.push('table-panel-cell-filterable');
      columnHtml += `
        <a class="table-panel-filter-link" data-link-tooltip data-original-title="Filter out value" data-placement="bottom"
           data-row="${rowIndex}" data-column="${columnIndex}" data-operator="!=">
          <i class="fa fa-search-minus"></i>
        </a>
        <a class="table-panel-filter-link" data-link-tooltip data-original-title="Filter for value" data-placement="bottom"
           data-row="${rowIndex}" data-column="${columnIndex}" data-operator="=">
          <i class="fa fa-search-plus"></i>
        </a>`;
    }

    if (cellClasses.length) {
      cellClass = ' class="' + cellClasses.join(' ') + '"';
    }

    columnHtml = '<td' + cellClass + cellStyle + textStyle + title + '>' + columnHtml + '</td>';
    return columnHtml;
  }

  render(page) {
    const pageSize = this.panel.pageSize || 100;
    const startPos = page * pageSize;
    const endPos = Math.min(startPos + pageSize, this.table.rows.length);
    let html = '';

    for (let y = startPos; y < endPos; y++) {
      const row = this.table.rows[y];
      let cellHtml = '';
      let rowStyle = '';
      const rowClasses = [];
      let rowClass = '';

      let prevRow, nextRow;
      if (y-1 >= 0) {
        prevRow = this.table.rows[y-1];
      }
      if (y+1 < endPos) {
        nextRow = this.table.rows[y+1];
      }

      const source = row.meta && row.meta.source ? row.meta.source.replace(/'/g, '\\\'') : undefined;
      const alarm = row.meta && row.meta.alarm ? row.meta.alarm : {};
      const severity = alarm && alarm.severity && alarm.severity.label ? alarm.severity.label.toLowerCase() : undefined;

      for (let i = 0; i < this.table.columns.length; i++) {
        let columnClasses = [];
        const col = this.table.columns[i];

        if (col && !col.style) {
          col.style = {};
        }

        // upgrade old "center" style definitions
        if (col.style.center) {
          col.style.align = 'center';
          delete col.style.center;
        }

        if (!col.style.align) {
          col.style.align = 'left';
        }
        if (col.style.type) {
          if (col.style.type === 'severity' && this.panel.severity === 'column') {
            columnClasses.push(severity);
          }
          columnClasses.push('type-' + col.style.type);
        }

        cellHtml += this.renderCell(i, y, row[i], y === startPos, columnClasses);
      }

      if (this.colorState.row) {
        rowStyle = ' style="background-color:' + this.colorState.row;
        rowClasses.push('table-panel-color-row');
        this.colorState.row = null;
      }

      if (this.panel.severity === true || this.panel.severity === 'row') {
        rowClasses.push(severity);
      }

      if (this.isRowSelected(row)) {
        rowClasses.push("selected");
      }

      if (prevRow && this.isRowSelected(prevRow)) {
        rowClasses.push("prev-selected");
      }

      if (nextRow && this.isRowSelected(nextRow)) {
        rowClasses.push("next-selected");
      }

      if (rowClasses.length) {
        rowClass = 'class="' + rowClasses.join(' ') + '"';
      }

      html += '<tr ' + rowClass + rowStyle + ` ng-click="ctrl.onRowClick($event, '${source}', ${alarm.id})"  context-menu="ctrl.getContextMenu($event, '${source}', ${alarm.id})">` + cellHtml + '</tr>';
    }

    return html;
  }

  render_values() {
    const rows = [];

    for (let y = 0; y < this.table.rows.length; y++) {
      const row = this.table.rows[y];
      const newRow = [];
      for (let i = 0; i < this.table.columns.length; i++) {
        newRow.push(this.formatColumnValue(i, row[i]));
      }
      rows.push(newRow);
    }
    return {
      columns: this.table.columns,
      rows: rows,
    };
  }

  isRowSelected(row) {
    return this.selectionMgr.isRowSelected({
      source: row.meta.source,
      alarmId: row.meta.alarm ? row.meta.alarm.id : undefined,
    });
  }

  static getIconForSeverity(severity) {
    let icon = 'ion-ios-help';
    switch(severity) {
      case 'indeterminate':
        icon = 'ion-ios-help';
        break;
      case 'warning':
        icon = 'ion-ios-warning';
        break;
      case 'minor':
        icon = 'ion-ios-flash';
        break;
      case 'major':
        icon = 'ion-ios-flame';
        break;
      case 'critical':
        icon = 'ion-ios-nuclear';
        break;
      case 'normal':
        icon = 'ion-ios-leaf';
        break;
      case 'cleared':
        icon = 'ion-ios-checkmark-circle';
        break;
    }
    return icon;
  }
}
