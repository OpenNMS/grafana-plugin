'use strict';

System.register(['lodash', 'moment', 'app/core/utils/kbn'], function (_export, _context) {
  "use strict";

  var _, moment, kbn, _createClass, TableRenderer;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }, function (_moment) {
      moment = _moment.default;
    }, function (_appCoreUtilsKbn) {
      kbn = _appCoreUtilsKbn.default;
    }],
    execute: function () {
      _createClass = function () {
        function defineProperties(target, props) {
          for (var i = 0; i < props.length; i++) {
            var descriptor = props[i];
            descriptor.enumerable = descriptor.enumerable || false;
            descriptor.configurable = true;
            if ("value" in descriptor) descriptor.writable = true;
            Object.defineProperty(target, descriptor.key, descriptor);
          }
        }

        return function (Constructor, protoProps, staticProps) {
          if (protoProps) defineProperties(Constructor.prototype, protoProps);
          if (staticProps) defineProperties(Constructor, staticProps);
          return Constructor;
        };
      }();

      _export('TableRenderer', TableRenderer = function () {
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

                var regex = kbn.stringToJsRegex(style.pattern);
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

            if (_.isArray(v)) {
              v = v.join(', ');
            }

            if (style && style.sanitize) {
              return this.sanitize(v);
            } else {
              return _.escape(v);
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

                if (_.isArray(v)) {
                  v = v[0];
                }
                var date = moment(v);
                if (_this.isUtc) {
                  date = date.utc();
                }
                return date.format(column.style.dateFormat);
              };
            }

            if (column.style.type === 'number') {
              var valueFormatter = kbn.valueFormats[column.unit || column.style.unit];

              return function (v) {
                if (v === null || v === void 0) {
                  return '-';
                }

                if (_.isString(v)) {
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

            var stylesAsString = 'style="' + _.reduce(_.map(styles, function (val, key) {
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
            var pageSize = this.panel.pageSize || 100;
            var startPos = page * pageSize;
            var endPos = Math.min(startPos + pageSize, this.table.rows.length);
            var html = "";

            for (var y = startPos; y < endPos; y++) {
              var row = this.table.rows[y];
              var cellHtml = '';
              var rowStyle = '';
              var rowClass = '';

              // FIXME: Sources with ' in the name will be problematic
              var source = row.meta.source;
              var alarm = row.meta.alarm;
              var severity = alarm.severity.label.toLowerCase();

              if (this.panel.severityIcons) {
                var icon = TableRenderer.getIconForSeverity(severity);
                cellHtml += '<td ng-click="ctrl.alarmDetails(\'' + source + '\', ' + alarm.id + ')" class="severity-icon text-center"><i class="icon ' + icon + '"></i></td>';
              }

              for (var i = 0; i < this.table.columns.length; i++) {
                cellHtml += this.renderCell(i, row[i], y === startPos);
              }

              if (this.panel.actions) {
                cellHtml += '<td>\n                    <div class="gf-form gf-form-no-margin">\n                        <label class="gf-form-label gf-smaller-form-label dropdown">\n                            <a class="pointer dropdown-toggle" data-toggle="dropdown" tabindex="1">\n                                <i class="fa fa-bars"></i>\n                            </a>\n                            <ul class="dropdown-menu dropdown-menu-with-smaller-form-label pull-right"role="menu">\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.alarmDetails(\'' + source + '\', ' + alarm.id + ')">Details</a>\n                                </li>\n                                <li class="divider"></li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.acknowledgeAlarm(\'' + source + '\', ' + alarm.id + ')">Acknowledge</a>\n                                </li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.unacknowledgeAlarm(\'' + source + '\', ' + alarm.id + ')">Unacknowledge</a>\n                                </li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.clearAlarm(\'' + source + '\', ' + alarm.id + ')">Clear</a>\n                                </li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.escalateAlarm(\'' + source + '\', ' + alarm.id + ')">Escalate</a>\n                                </li>\n                                <li class="divider"></li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.createTicketForAlarm(\'' + source + '\', ' + alarm.id + ')">Create Ticket</a>\n                                </li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.updateTicketForAlarm(\'' + source + '\', ' + alarm.id + ')">Update Ticket</a>\n                                </li>\n                                <li role="menuitem">\n                                    <a tabindex="1" ng-click="ctrl.closeTicketForAlarm(\'' + source + '\', ' + alarm.id + ')">Close Ticket</a>\n                                </li>\n                            </ul>\n                        </label>\n                    </div>\n                </td>';
              }

              if (this.colorState.row) {
                rowStyle = ' style="background-color:' + this.colorState.row + ';color: white"';
                this.colorState.row = null;
              }

              if (this.panel.severity) {
                rowClass = ' class="' + severity + '"';
              }

              html += '<tr ' + rowStyle + rowClass + '>' + cellHtml + '</tr>';
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
            return _.first(style.colors);
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
      }());

      _export('TableRenderer', TableRenderer);
    }
  };
});
//# sourceMappingURL=renderer.js.map
