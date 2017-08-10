'use strict';

System.register(['lodash', 'jquery', 'app/plugins/sdk', './transformers', './editor', './column_options', './renderer', 'app/core/core_module', './alarm_details', './memo_editor', '../css/styles.css!', '../css/ionicons.css!'], function (_export, _context) {
  "use strict";

  var _, $, MetricsPanelCtrl, transformDataToTable, tablePanelEditor, columnOptionsTab, TableRenderer, coreModule, alarmDetailsAsDirective, memoEditorAsDirective, _createClass, _get, AlarmTableCtrl;

  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }

  function _possibleConstructorReturn(self, call) {
    if (!self) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }

    return call && (typeof call === "object" || typeof call === "function") ? call : self;
  }

  function _inherits(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function, not " + typeof superClass);
    }

    subClass.prototype = Object.create(superClass && superClass.prototype, {
      constructor: {
        value: subClass,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
    if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass;
  }

  return {
    setters: [function (_lodash) {
      _ = _lodash.default;
    }, function (_jquery) {
      $ = _jquery.default;
    }, function (_appPluginsSdk) {
      MetricsPanelCtrl = _appPluginsSdk.MetricsPanelCtrl;
    }, function (_transformers) {
      transformDataToTable = _transformers.transformDataToTable;
    }, function (_editor) {
      tablePanelEditor = _editor.tablePanelEditor;
    }, function (_column_options) {
      columnOptionsTab = _column_options.columnOptionsTab;
    }, function (_renderer) {
      TableRenderer = _renderer.TableRenderer;
    }, function (_appCoreCore_module) {
      coreModule = _appCoreCore_module.default;
    }, function (_alarm_details) {
      alarmDetailsAsDirective = _alarm_details.alarmDetailsAsDirective;
    }, function (_memo_editor) {
      memoEditorAsDirective = _memo_editor.memoEditorAsDirective;
    }, function (_cssStylesCss) {}, function (_cssIoniconsCss) {}],
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

      _get = function get(object, property, receiver) {
        if (object === null) object = Function.prototype;
        var desc = Object.getOwnPropertyDescriptor(object, property);

        if (desc === undefined) {
          var parent = Object.getPrototypeOf(object);

          if (parent === null) {
            return undefined;
          } else {
            return get(parent, property, receiver);
          }
        } else if ("value" in desc) {
          return desc.value;
        } else {
          var getter = desc.get;

          if (getter === undefined) {
            return undefined;
          }

          return getter.call(receiver);
        }
      };

      _export('PanelCtrl', _export('AlarmTableCtrl', AlarmTableCtrl = function (_MetricsPanelCtrl) {
        _inherits(AlarmTableCtrl, _MetricsPanelCtrl);

        function AlarmTableCtrl($scope, $injector, $rootScope, annotationsSrv, $sanitize, $compile, datasourceSrv, timeSrv) {
          _classCallCheck(this, AlarmTableCtrl);

          var _this = _possibleConstructorReturn(this, (AlarmTableCtrl.__proto__ || Object.getPrototypeOf(AlarmTableCtrl)).call(this, $scope, $injector));

          _this.$rootScope = $rootScope;
          _this.annotationsSrv = annotationsSrv;
          _this.$sanitize = $sanitize;
          _this.$compile = $compile;
          _this.datasourceSrv = datasourceSrv;
          _this.timeSrv = timeSrv;

          var panelDefaults = {
            targets: [{}],
            transform: 'table',
            pageSize: null,
            showHeader: true,
            styles: [{
              type: 'date',
              pattern: 'Time',
              alias: 'Time',
              dateFormat: 'YYYY-MM-DD HH:mm:ss'
            }, {
              unit: 'short',
              type: 'number',
              alias: '',
              decimals: 2,
              colors: ["rgba(245, 54, 54, 0.9)", "rgba(237, 129, 40, 0.89)", "rgba(50, 172, 45, 0.97)"],
              colorMode: null,
              pattern: '/.*/',
              thresholds: []
            }],
            columns: [],
            scroll: true,
            fontSize: '100%',
            sort: { col: 0, desc: true }
          };

          _this.pageIndex = 0;

          if (_this.panel.styles === void 0) {
            _this.panel.styles = _this.panel.columns;
            _this.panel.columns = _this.panel.fields;
            delete _this.panel.columns;
            delete _this.panel.fields;
          }

          _.defaults(_this.panel, panelDefaults);

          _this.events.on('data-received', _this.onDataReceived.bind(_this));
          _this.events.on('data-error', _this.onDataError.bind(_this));
          _this.events.on('data-snapshot-load', _this.onDataReceived.bind(_this));
          _this.events.on('init-edit-mode', _this.onInitEditMode.bind(_this));
          return _this;
        }

        _createClass(AlarmTableCtrl, [{
          key: 'onInitEditMode',
          value: function onInitEditMode() {
            this.addEditorTab('Options', tablePanelEditor, 2);
            this.addEditorTab('Column Styles', columnOptionsTab, 3);
          }
        }, {
          key: 'issueQueries',
          value: function issueQueries(datasource) {
            this.pageIndex = 0;

            if (this.panel.transform === 'annotations') {
              this.setTimeQueryStart();
              return this.annotationsSrv.getAnnotations({ dashboard: this.dashboard, panel: this.panel, range: this.range }).then(function (annotations) {
                return { data: annotations };
              });
            }

            return _get(AlarmTableCtrl.prototype.__proto__ || Object.getPrototypeOf(AlarmTableCtrl.prototype), 'issueQueries', this).call(this, datasource);
          }
        }, {
          key: 'onDataError',
          value: function onDataError(err) {
            this.dataRaw = [];
            this.render();
          }
        }, {
          key: 'onDataReceived',
          value: function onDataReceived(dataList) {
            this.dataRaw = dataList;
            this.pageIndex = 0;

            // automatically correct transform mode based on data
            if (this.dataRaw && this.dataRaw.length) {
              if (this.dataRaw[0].type === 'table') {
                this.panel.transform = 'table';
              } else {
                if (this.dataRaw[0].type === 'docs') {
                  this.panel.transform = 'json';
                } else {
                  if (this.panel.transform === 'table' || this.panel.transform === 'json') {
                    this.panel.transform = 'timeseries_to_rows';
                  }
                }
              }
            }

            this.render();
          }
        }, {
          key: 'render',
          value: function render() {
            this.table = transformDataToTable(this.dataRaw, this.panel);
            this.table.sort(this.panel.sort);

            this.renderer = new TableRenderer(this.panel, this.table, this.dashboard.isTimezoneUtc(), this.$sanitize);

            return _get(AlarmTableCtrl.prototype.__proto__ || Object.getPrototypeOf(AlarmTableCtrl.prototype), 'render', this).call(this, this.table);
          }
        }, {
          key: 'toggleColumnSort',
          value: function toggleColumnSort(col, colIndex) {
            // remove sort flag from current column
            if (this.table.columns[this.panel.sort.col]) {
              this.table.columns[this.panel.sort.col].sort = false;
            }

            if (this.panel.sort.col === colIndex) {
              if (this.panel.sort.desc) {
                this.panel.sort.desc = false;
              } else {
                this.panel.sort.col = null;
              }
            } else {
              this.panel.sort.col = colIndex;
              this.panel.sort.desc = true;
            }
            this.render();
          }
        }, {
          key: 'link',
          value: function link(scope, elem, attrs, ctrl) {
            var data = void 0;
            var panel = ctrl.panel;
            var pageCount = 0;
            var formaters = [];

            function getTableHeight() {
              var panelHeight = ctrl.height;

              if (pageCount > 1) {
                panelHeight -= 26;
              }

              return panelHeight - 31 + 'px';
            }

            function appendTableRows(tbodyElem) {
              ctrl.renderer.setTable(data);
              tbodyElem.empty();
              tbodyElem.html(ctrl.renderer.render(ctrl.pageIndex));
              // Compile the HTML generated by the renderer - this is required for the actions dropdown to function
              ctrl.$compile(tbodyElem.contents())(scope);
            }

            function switchPage(e) {
              var el = $(e.currentTarget);
              ctrl.pageIndex = parseInt(el.text(), 10) - 1;
              renderPanel();
            }

            function appendPaginationControls(footerElem) {
              footerElem.empty();

              var pageSize = panel.pageSize || 100;
              pageCount = Math.ceil(data.rows.length / pageSize);
              if (pageCount === 1) {
                return;
              }

              var startPage = Math.max(ctrl.pageIndex - 3, 0);
              var endPage = Math.min(pageCount, startPage + 9);

              var paginationList = $('<ul></ul>');

              for (var i = startPage; i < endPage; i++) {
                var activeClass = i === ctrl.pageIndex ? 'active' : '';
                var pageLinkElem = $('<li><a class="table-panel-page-link pointer ' + activeClass + '">' + (i + 1) + '</a></li>');
                paginationList.append(pageLinkElem);
              }

              footerElem.append(paginationList);
            }

            function renderPanel() {
              var panelElem = elem.parents('.panel');
              var rootElem = elem.find('.table-panel-scroll');
              var tbodyElem = elem.find('tbody');
              var footerElem = elem.find('.table-panel-footer');

              elem.css({ 'font-size': panel.fontSize });
              panelElem.addClass('table-panel-wrapper');

              appendTableRows(tbodyElem);
              appendPaginationControls(footerElem);

              rootElem.css({ 'max-height': panel.scroll ? getTableHeight() : '' });
            }

            elem.on('click', '.table-panel-page-link', switchPage);

            var unbindDestroy = scope.$on('$destroy', function () {
              elem.off('click', '.table-panel-page-link');
              unbindDestroy();
            });

            ctrl.events.on('render', function (renderData) {
              data = renderData || data;
              if (data) {
                renderPanel();
              }
              ctrl.renderingCompleted();
            });
          }
        }, {
          key: 'findAlarm',
          value: function findAlarm(source, alarmId) {
            var alarm = void 0;
            _.each(this.dataRaw, function (table) {
              var matchedRow = _.find(table.rows, function (row) {
                return row.meta.source === source && row.meta.alarm.id === alarmId;
              });
              if (matchedRow !== undefined) {
                alarm = matchedRow.meta.alarm;
              }
            });
            return alarm;
          }
        }, {
          key: 'alarmDetails',
          value: function alarmDetails(source, alarmId) {
            var alarm = this.findAlarm(source, alarmId);
            if (alarm === undefined) {
              this.$rootScope.appEvent('alert-error', ['Unable to find matching alarm', '']);
              return;
            }

            var newScope = this.$rootScope.$new();
            newScope.alarm = alarm;
            newScope.source = source;
            this.$rootScope.appEvent('show-modal', {
              templateHtml: '<alarm-details-as-modal dismiss="dismiss()"></alarm-details-as-modal>',
              scope: newScope
            });
          }
        }, {
          key: 'performAlarmActionOnDatasource',
          value: function performAlarmActionOnDatasource(source, action, alarmId) {
            var self = this;
            this.datasourceSrv.get(source).then(function (ds) {
              if (ds.type && ds.type.indexOf("fm-ds") < 0) {
                throw { message: 'Only OpenNMS datasources are supported' };
              } else {
                if (!ds[action]) {
                  throw { message: 'Action ' + action + ' not implemented by datasource ' + ds.name + " of type " + ds.type };
                }
                return ds[action](alarmId);
              }
            }).then(function () {
              // Action was successful, remove any previous error
              delete self.error;
              // Refresh the dashboard
              self.timeSrv.refreshDashboard();
            }).catch(function (err) {
              self.error = err.message || "Request Error";
            });
          }
        }, {
          key: 'acknowledgeAlarm',
          value: function acknowledgeAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'acknowledgeAlarm', alarmId);
          }
        }, {
          key: 'unacknowledgeAlarm',
          value: function unacknowledgeAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'unacknowledgeAlarm', alarmId);
          }
        }, {
          key: 'clearAlarm',
          value: function clearAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'clearAlarm', alarmId);
          }
        }, {
          key: 'escalateAlarm',
          value: function escalateAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'escalateAlarm', alarmId);
          }
        }, {
          key: 'createTicketForAlarm',
          value: function createTicketForAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'createTicketForAlarm', alarmId);
          }
        }, {
          key: 'updateTicketForAlarm',
          value: function updateTicketForAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'updateTicketForAlarm', alarmId);
          }
        }, {
          key: 'closeTicketForAlarm',
          value: function closeTicketForAlarm(source, alarmId) {
            this.performAlarmActionOnDatasource(source, 'closeTicketForAlarm', alarmId);
          }
        }]);

        return AlarmTableCtrl;
      }(MetricsPanelCtrl)));

      AlarmTableCtrl.templateUrl = 'panels/alarm-table/module.html';

      _export('AlarmTableCtrl', AlarmTableCtrl);

      _export('PanelCtrl', AlarmTableCtrl);

      coreModule.directive('alarmDetailsAsModal', alarmDetailsAsDirective);
      coreModule.directive('memoEditor', memoEditorAsDirective);
    }
  };
});
//# sourceMappingURL=module.js.map
