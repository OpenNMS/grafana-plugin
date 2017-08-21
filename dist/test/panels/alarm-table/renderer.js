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

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var TableRenderer = exports.TableRenderer = function () {
  function TableRenderer(panel, table, isUtc, sanitize) {
    _classCallCheck(this, TableRenderer);

    this.panel = panel;
    this.table = table;
    this.isUtc = isUtc;
    this.sanitize = sanitize;

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
    value: function renderCell(columnIndex, value) {
      var addWidthHack = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

      value = this.formatColumnValue(columnIndex, value);
      var column = this.table.columns[columnIndex];
      var styles = {};
      var classes = [];

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

      var stylesAsString = 'style="' + _lodash2.default.reduce(_lodash2.default.map(styles, function (val, key) {
        return key + ':' + val;
      }), function (memo, style) {
        if (memo.length > 0) {
          return memo + '; ' + style;
        } else {
          return style;
        }
      }, '') + '"';

      return '<td ' + stylesAsString + '>' + value + widthHack + '</td>';
    }
  }, {
    key: 'render',
    value: function render(page) {
      var _this2 = this;

      var pageSize = this.panel.pageSize || 100;
      var startPos = page * pageSize;
      var endPos = Math.min(startPos + pageSize, this.table.rows.length);
      var html = "";

      var _loop = function _loop(y) {
        var row = _this2.table.rows[y];
        var cellHtml = '';
        var rowStyle = '';
        var rowClass = '';

        var source = row.meta.source.replace(/'/g, '\\\'');
        var alarm = row.meta.alarm;
        var ticketerConfig = row.meta.ticketerConfig;
        var severity = alarm.severity.label.toLowerCase();

        if (_this2.panel.severityIcons) {
          var icon = TableRenderer.getIconForSeverity(severity);
          cellHtml += '<td ng-click="ctrl.alarmDetails(\'' + source + '\', ' + alarm.id + ')" class="severity-icon text-center"><i class="icon ' + icon + '"></i></td>';
        }

        for (var i = 0; i < _this2.table.columns.length; i++) {
          cellHtml += _this2.renderCell(i, row[i], y === startPos);
        }

        if (_this2.panel.actions) {
          cellHtml += '<td>';
          cellHtml += new Menu().withGroup(new Group().withItem(new MenuItem("Details", 'ctrl.alarmDetails(\'' + source + '\', ' + alarm.id + ')'))).withGroup(new Group().withItem(new MenuItem("Acknowledge", 'ctrl.acknowledgeAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return alarm.ackTime === void 0;
          })).withItem(new MenuItem("Unacknowledge", 'ctrl.unacknowledgeAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return alarm.ackTime;
          })).withItem(new MenuItem("Escalate", 'ctrl.escalateAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return alarm.severity.index == _opennms.Model.Severities.CLEARED.index || alarm.severity.index >= _opennms.Model.Severities.NORMAL.index && alarm.severity.index < _opennms.Model.Severities.CRITICAL.index;
          })).withItem(new MenuItem("Clear", 'ctrl.clearAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return alarm.severity.index >= _opennms.Model.Severities.NORMAL.index && alarm.severity.index <= _opennms.Model.Severities.CRITICAL.index;
          }))).withGroup(new Group().withVisibility(function () {
            return ticketerConfig && ticketerConfig.enabled;
          }).withItem(new MenuItem("Create Ticket", 'ctrl.createTicketForAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return !alarm.troubleTicketState || alarm.troubleTicketState === _opennms.Model.TroubleTicketStates.CREATE_FAILED;
          })).withItem(new MenuItem("Update Ticket", 'ctrl.updateTicketForAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return alarm.troubleTicketState && alarm.troubleTicket;
          })).withItem(new MenuItem("Close Ticket", 'ctrl.closeTicketForAlarm(\'' + source + '\', ' + alarm.id + ')', function () {
            return alarm.troubleTicketState && (alarm.troubleTicketState === _opennms.Model.TroubleTicketStates.OPEN || alarm.troubleTicketState == _opennms.Model.TroubleTicketStates.CLOSE_FAILED);
          }))).render();
          cellHtml += '</td>';
        }

        if (_this2.colorState.row) {
          rowStyle = ' style="background-color:' + _this2.colorState.row + ';color: white"';
          _this2.colorState.row = null;
        }

        if (_this2.panel.severity) {
          rowClass = ' class="' + severity + '"';
        }

        html += '<tr ' + rowStyle + rowClass + '>' + cellHtml + '</tr>';
      };

      for (var y = startPos; y < endPos; y++) {
        _loop(y);
      }

      return html;
    }
  }, {
    key: 'render_values',
    value: function render_values() {
      var rows = [];

      for (var y = 0; y < this.table.rows.length; y++) {
        var _row = this.table.rows[y];
        var new_row = [];
        for (var i = 0; i < this.table.columns.length; i++) {
          new_row.push(this.formatColumnValue(i, _row[i]));
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

var Group = function () {
  function Group() {
    _classCallCheck(this, Group);

    this.items = [];
    this.visibilityFn = function () {
      return true;
    };
  }

  _createClass(Group, [{
    key: 'withVisibility',
    value: function withVisibility(visibilityFn) {
      this.visibilityFn = visibilityFn;
      return this;
    }
  }, {
    key: 'withItem',
    value: function withItem(itemOrMenu) {
      this.items.push(itemOrMenu);
      return this;
    }
  }, {
    key: 'withGroup',
    value: function withGroup(group) {
      this.withItem(group);
      return this;
    }
  }, {
    key: 'isVisible',
    value: function isVisible() {
      if (this.visibilityFn()) {
        return this.getVisibleItems().length >= 1;
      }
      return false;
    }
  }, {
    key: 'getVisibleItems',
    value: function getVisibleItems() {
      var visibleItems = _lodash2.default.filter(this.items, function (item) {
        return item.isVisible();
      });
      return visibleItems;
    }
  }, {
    key: 'render',
    value: function render() {
      var visibleItems = _lodash2.default.filter(this.items, function (item) {
        return item.isVisible();
      });

      var renderedItems = _lodash2.default.map(visibleItems, function (item) {
        return item.render();
      });
      return renderedItems.join('');
    }
  }]);

  return Group;
}();

var Menu = function (_Group) {
  _inherits(Menu, _Group);

  function Menu() {
    _classCallCheck(this, Menu);

    return _possibleConstructorReturn(this, (Menu.__proto__ || Object.getPrototypeOf(Menu)).call(this));
  }

  _createClass(Menu, [{
    key: 'render',
    value: function render() {
      var html = '<div class="gf-form gf-form-no-margin">';
      html += '<label class="gf-form-label gf-smaller-form-label dropdown">';
      html += '<a class="pointer dropdown-toggle" data-toggle="dropdown" tabindex="1"><i class="fa fa-bars"></i></a>';
      html += '<ul class="dropdown-menu dropdown-menu-with-smaller-form-label pull-right"role="menu">';

      var visibleItems = this.getVisibleItems();
      html += _lodash2.default.map(visibleItems, function (item, index) {
        var rendered = item.render();
        if (index < visibleItems.length - 1) {
          rendered += '<li class="divider"></li>';
        }
        return rendered;
      }).join('');

      html += '</ul></label></div>';
      return html;
    }
  }]);

  return Menu;
}(Group);

var MenuItem = function () {
  function MenuItem(label, action, visibilityFn) {
    _classCallCheck(this, MenuItem);

    this.label = label;
    this.action = action;
    this.visibilityFn = visibilityFn;
    if (!visibilityFn) {
      this.visibilityFn = function () {
        return true;
      };
    }
  }

  _createClass(MenuItem, [{
    key: 'isVisible',
    value: function isVisible() {
      return this.visibilityFn();
    }
  }, {
    key: 'render',
    value: function render() {
      return '<li role="menuitem"><a tabindex="1" ng-click="' + this.action + '">' + this.label + '</a></li>';
    }
  }]);

  return MenuItem;
}();
//# sourceMappingURL=renderer.js.map
