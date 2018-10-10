'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TableRenderer = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _moment = require('moment');

var _moment2 = _interopRequireDefault(_moment);

var _kbn = require('app/core/utils/kbn');

var _kbn2 = _interopRequireDefault(_kbn);

var _opennms = require('../../opennms');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var TableRenderer = exports.TableRenderer = function () {
  function TableRenderer(panel, table, isUtc, sanitize, selectionMgr) {
    _classCallCheck(this, TableRenderer);

    this.panel = panel;
    this.table = table;
    this.isUtc = isUtc;
    this.sanitize = sanitize;
    this.selectionMgr = selectionMgr;

    this.initColumns();
  }

  _createClass(TableRenderer, [{
    key: 'setTable',
    value: function setTable(table) {
      this.table = table;

      this.initColumns();
    }
  }, {
    key: 'initColumns',
    value: function initColumns() {
      this.formatters = [];
      this.colorState = {};

      for (var colIndex = 0; colIndex < this.table.columns.length; colIndex++) {
        var column = this.table.columns[colIndex];
        column.title = column.text;

        for (var i = 0; i < this.panel.styles.length; i++) {
          var style = this.panel.styles[i];

          var regex = _kbn2.default.stringToJsRegex(style.pattern);
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
  }, {
    key: 'defaultCellFormatter',
    value: function defaultCellFormatter(v, style) {
      if (v === null || v === void 0 || v === undefined) {
        return '';
      }

      if (_lodash2.default.isArray(v)) {
        v = v.join(', ');
      }

      if (style && style.sanitize) {
        return this.sanitize(v);
      } else {
        return _lodash2.default.escape(v);
      }
    }
  }, {
    key: 'createColumnFormatter',
    value: function createColumnFormatter(column) {
      var _this = this;

      if (!column.style) {
        return this.defaultCellFormatter;
      }

      if (column.style.type === 'hidden') {
        return function (v) {
          return undefined;
        };
      }

      if (column.style.type === 'date') {
        return function (v) {
          if (v === undefined || v === null) {
            return '-';
          }

          if (_lodash2.default.isArray(v)) {
            v = v[0];
          }
          var date = (0, _moment2.default)(v);
          if (_this.isUtc) {
            date = date.utc();
          }
          return date.format(column.style.dateFormat);
        };
      }

      if (column.style.type === 'number') {
        var valueFormatter = _kbn2.default.valueFormats[column.unit || column.style.unit];

        return function (v) {
          if (v === null || v === void 0) {
            return '-';
          }

          if (_lodash2.default.isString(v)) {
            return _this.defaultCellFormatter(v, column.style);
          }

          if (column.style.colorMode) {
            _this.colorState[column.style.colorMode] = TableRenderer.getColorForValue(v, column.style);
          }

          return valueFormatter(v, column.style.decimals, null);
        };
      }

      if (column.style.type === 'severity') {
        return function (v) {
          if (v === null || v === void 0) {
            return '-';
          }

          if (column.style.displayAs === 'label') {
            return _opennms.Model.Severities[v].toDisplayString();
          } else if (column.style.displayAs === 'labelCaps') {
            return v;
          } else {
            var icon = TableRenderer.getIconForSeverity(v.toLowerCase());
            return '<i class="icon severity-icon ' + icon + '" title="' + v + '"></i>';
          }
        };
      }

      return function (value) {
        return _this.defaultCellFormatter(value, column.style);
      };
    }
  }, {
    key: 'formatColumnValue',
    value: function formatColumnValue(colIndex, value) {
      return this.formatters[colIndex] ? this.formatters[colIndex](value) : value;
    }
  }, {
    key: 'renderCell',
    value: function renderCell(columnIndex, value, addWidthHack, columnClasses) {
      value = this.formatColumnValue(columnIndex, value);
      var column = this.table.columns[columnIndex];
      var styles = {};
      var classes = column.classes || [];
      classes = classes.concat(columnClasses);

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
      var widthHack = '';
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

      var stylesAsString = '';
      if (styles.length > 0) {
        stylesAsString = 'style="' + _lodash2.default.reduce(_lodash2.default.map(styles, function (val, key) {
          return key + ':' + val;
        }), function (memo, style) {
          if (memo.length > 0) {
            return memo + '; ' + style;
          } else {
            return style;
          }
        }, '') + '"';
      }

      if (column.style.type === 'severity' && column.style.displayAs === 'icon') {
        classes.push('text-center');
      }

      var classesAsString = '';
      if (classes.length > 0) {
        classesAsString = 'class="' + classes.join(' ') + '"';
      }

      return '<td ' + stylesAsString + ' ' + classesAsString + '>' + value + widthHack + '</td>';
    }
  }, {
    key: 'isRowSelected',
    value: function isRowSelected(row) {
      return this.selectionMgr.isRowSelected({
        source: row.meta.source,
        alarmId: row.meta.alarm.id
      });
    }
  }, {
    key: 'render',
    value: function render(page) {
      var pageSize = this.panel.pageSize || 100;
      var startPos = page * pageSize;
      var endPos = Math.min(startPos + pageSize, this.table.rows.length);
      var html = "";

      for (var y = startPos; y < endPos; y++) {
        var row = this.table.rows[y];
        var nextRow = void 0;
        if (y + 1 < endPos) {
          nextRow = this.table.rows[y + 1];
        }

        var cellHtml = '';
        var rowStyle = '';
        var rowClasses = [];

        var source = row.meta.source.replace(/'/g, '\\\'');
        var alarm = row.meta.alarm;
        var severity = alarm.severity.label.toLowerCase();

        for (var i = 0; i < this.table.columns.length; i++) {
          var columnClasses = [];
          if (this.panel.severity === 'column') {
            var col = this.table.columns[i];
            if (col && col.style && col.style.type === 'severity') {
              columnClasses.push(severity);
            }
          }
          cellHtml += this.renderCell(i, row[i], y === startPos, columnClasses);
        }

        if (this.colorState.row) {
          rowStyle = ' style="background-color:' + this.colorState.row + ';color: white"';
          this.colorState.row = null;
        }

        if (this.panel.severity === true || this.panel.severity === 'row') {
          rowClasses.push(severity);
        }

        if (this.isRowSelected(row)) {
          rowClasses.push("selected");
        }

        if (nextRow && this.isRowSelected(nextRow)) {
          rowClasses.push("next-selected");
        }

        var rowClass = 'class="' + rowClasses.join(' ') + '"';
        html += '<tr ' + rowStyle + rowClass + (' ng-click="ctrl.onRowClick($event, \'' + source + '\', ' + alarm.id + ')"  ng-dblclick="ctrl.onRowDoubleClick($event, \'' + source + '\', ' + alarm.id + ')" context-menu="ctrl.getContextMenu($event, \'' + source + '\', ' + alarm.id + ')">') + cellHtml + '</tr>';
      }

      return html;
    }
  }, {
    key: 'render_values',
    value: function render_values() {
      var rows = [];

      for (var y = 0; y < this.table.rows.length; y++) {
        var row = this.table.rows[y];
        var new_row = [];
        for (var i = 0; i < this.table.columns.length; i++) {
          new_row.push(this.formatColumnValue(i, row[i]));
        }
        rows.push(new_row);
      }
      return {
        columns: this.table.columns,
        rows: rows
      };
    }
  }], [{
    key: 'getColorForValue',
    value: function getColorForValue(value, style) {
      if (!style.thresholds) {
        return null;
      }

      for (var i = style.thresholds.length; i > 0; i--) {
        if (value >= style.thresholds[i - 1]) {
          return style.colors[i];
        }
      }
      return _lodash2.default.first(style.colors);
    }
  }, {
    key: 'getIconForSeverity',
    value: function getIconForSeverity(severity) {
      var icon = 'ion-help';
      switch (severity) {
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
  }]);

  return TableRenderer;
}();
//# sourceMappingURL=renderer.js.map
