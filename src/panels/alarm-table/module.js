import _ from 'lodash';
import $ from 'jquery';
import {MetricsPanelCtrl} from 'app/plugins/sdk';
import {transformDataToTable} from './transformers';
import {tablePanelEditor} from './editor';
import {columnOptionsTab} from './column_options';
import {TableRenderer} from './renderer';
import coreModule from 'app/core/core_module';
import {alarmDetailsAsDirective} from './alarm_details';
import {memoEditorAsDirective} from "./memo_editor"
import {contextMenuAsDirective} from "./context_menu";
import {loadPluginCss} from 'app/plugins/sdk';
import {SelectionMgr} from "./selection_mgr";
import {ActionMgr} from "./action_mgr";

import * as XLSX from 'xlsx';

loadPluginCss({
  dark: 'plugins/opennms-helm-app/panels/alarm-table/css/table.dark.css',
  light: 'plugins/opennms-helm-app/panels/alarm-table/css/table.light.css'
});

const doubleClickDelay = 250;

class AlarmTableCtrl extends MetricsPanelCtrl {
  /** @ngInject */
  constructor($scope, $injector, $rootScope, annotationsSrv, $sanitize, $compile, backendSrv, datasourceSrv, templateSrv, timeSrv, variableSrv) {
    super($scope, $injector);
    this.$rootScope = $rootScope;
    this.annotationsSrv = annotationsSrv;
    this.$sanitize = $sanitize;
    this.$compile = $compile;
    this.backendSrv = backendSrv;
    this.datasourceSrv = datasourceSrv;
    this.templateSrv = templateSrv;
    this.timeSrv = timeSrv;
    this.variableSrv = variableSrv;

    let panelDefaults = {
      targets: [{}],
      transform: 'table',
      pageSize: 5,
      pagingPausesRefresh: false,
      showHeader: true,
      styles: [
        {
          type: 'severity',
          pattern: 'Severity',
          displayAs: 'icon',
        },
        {
          type: 'checkbox',
          pattern: '/^Is /',
          width: '9em',
        },
        {
          type: 'date',
          pattern: '/.*Time/', // Render all "* Time" columns as date, e.g. "Last Event Time", "First Event Time", etc.
          dateFormat: 'YYYY-MM-DD HH:mm:ss',
        },
        {
          type: 'date',
          pattern: 'Suppressed Until',
          dateFormat: 'YYYY-MM-DD HH:mm:ss',
        },
        {
          type: 'string',
          pattern: '/.*ID/', // Render all "* ID" columns as string, otherwise ID 1000 appears as 1.0 K
        },
        {
          type: 'string',
          pattern: 'Description',
          sanitize: true
        },
        {
          type: 'string',
          pattern: 'Log Message',
          sanitize: true
        },
        {
          unit: 'short',
          type: 'number',
          decimals: 0,
          pattern: 'Count',
        },
        {
          unit: 'short',
          type: 'number',
          alias: '',
          decimals: 2,
          colors: ['rgba(245, 54, 54, 0.9)', 'rgba(237, 129, 40, 0.89)', 'rgba(50, 172, 45, 0.97)'],
          colorMode: null,
          pattern: '/.*/',
          thresholds: [],
        },
      ],
      columns: [
          {text: 'Severity'},
          {text: 'UEI'},
          {text: 'Log Message'},
          {text: 'Node Label'},
          {text: 'Count'},
          {text: 'Last Event Time'},
        ],
      scroll: false, // disable scrolling as the actions popup is not working properly otherwise
      fontSize: '100%',
      sort: {col: 0, desc: true},
      severity: true
    };

    this.pageIndex = 0;

    if (this.panel.styles === void 0) {
      this.panel.styles = this.panel.columns;
      this.panel.columns = this.panel.fields;
      delete this.panel.columns;
      delete this.panel.fields;
    }

    _.defaults(this.panel, panelDefaults);

    let self = this;
    this.selectionMgr = new SelectionMgr((from,to) => self.getRowsInRange(from,to), () => self.render());
    this.events.on('data-received', this.onDataReceived.bind(this));
    this.events.on('data-error', this.onDataError.bind(this));
    this.events.on('data-snapshot-load', this.onDataReceived.bind(this));
    this.events.on('init-edit-mode', this.onInitEditMode.bind(this));

    if (this.panel.severity === true) {
      this.panel.severity = 'row';
    }

    if (this.panel.severityIcons === true) {
      delete this.panel.severityIcons;
      if (this.panel.sort && this.panel.sort.col !== undefined) {
        this.panel.sort.col++;
      }
      this.panel.styles.unshift({
        type: 'severity',
        pattern: 'Severity',
        displayAs: 'icon'
      });
      this.panel.columns.unshift({
        hidden: false,
        text: 'Severity',
        title: 'Severity',
        style: {
          type: 'severity',
          pattern: 'Severity',
          displayAs: 'icon'
        }
      });
      if (this.table && this.table.rows) {
        // put a placeholder value in until data refreshes
        this.table.rows = this.table.rows.map((row) => {
          row.unshift(undefined);
        });
      }
    }

    self.refreshAppConfig();
  }

  refreshAppConfig() {
    const self = this;
    self.backendSrv.get(`api/plugins/opennms-helm-app/settings`).then(result => {
      if (result && result.jsonData) {
        self.appConfig = result.jsonData;
      } else {
        console.warn('No settings found.');
      }
    });
  }

  onInitEditMode() {
    this.addEditorTab('Options', tablePanelEditor, 2);
    this.addEditorTab('Column Styles', columnOptionsTab, 3);
  }

  issueQueries(datasource) {
    if (this.panel.transform === 'annotations') {
      if (this.setTimeQueryStart) this.setTimeQueryStart();
      return this.annotationsSrv
        .getAnnotations({
          dashboard: this.dashboard,
          panel: this.panel,
          range: this.range,
        })
        .then(annotations => {
          return {data: annotations};
        });
    }

    return super.issueQueries(datasource);
  }

  onDataError() {
    this.dataRaw = [];
    this.render();
  }

  onDataReceived(dataList) {
    this.newDataRaw = dataList;

    if (!this.panel.pagingPausesRefresh || this.pageIndex === 0 || !this.dataRaw || this.dataRaw.length === 0) {
      this.updateData();
    }

    this.render();
  }

  refreshData() {
    this.updateData();
    this.scope.$evalAsync(() => {
      this.render();
    });
  }

  updateData() {
    this.dataRaw = this.newDataRaw;
    delete this.newDataRaw;

    // automatically correct transform mode based on data
    if (this.dataRaw && this.dataRaw.length) {
      if (this.dataRaw[0].type === 'table') {
        this.panel.transform = 'table';
      } else {
        if (this.dataRaw[0].type === 'docs') {
          this.panel.transform = 'json';
        } else {
          if (this.panel.transform === 'table' || this.panel.transform === 'json') {
            this.panel.transform = 'table';
          }
        }
      }
    }
    this.pageIndex = 0;
  }

  render() {
    this.table = transformDataToTable(this.dataRaw, this.panel);
    this.table.sort(this.panel.sort);

    this.renderer = new TableRenderer(
      this.panel,
      this.table,
      this.dashboard.isTimezoneUtc(),
      this.$sanitize,
      this.selectionMgr,
      this.templateSrv,
      // Grafana 6:
      // config.theme.type,
    );

    return super.render(this.table);
  }

  _getWorkbook() {
    const workbook = XLSX.utils.book_new();
    workbook.Props = {
      Title: this.panel.title,
    }

    const columns = this.table.columns.map((col) => {
      return col.text;
    });
    const rows = this.table.rows.map((row) => {
      const ret = {};
      row.forEach((col, index) => {
        ret[columns[index]] = col;
      });
      return ret;
    });

    const worksheet = XLSX.utils.json_to_sheet(rows, {header:columns});
    XLSX.utils.book_append_sheet(workbook, worksheet, this.panel.title);
  }

  exportCSV() {
    const workbook = this._getWorkbook();
    XLSX.writeFile(workbook, this.panel.title + '.csv');
  }

  exportExcel() {
    const workbook = this._getWorkbook();
    XLSX.writeFile(workbook, this.panel.title + '.xlsx');
  }

  toggleColumnSort(col, colIndex) {
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

  link(scope, elem, attrs, ctrl) {
    let data;
    const panel = ctrl.panel;
    let pageCount = 0;

    scope.getColumnStyle = (col) => {
      const ret = {};
      if (col && col.style) {
        if (col.style.width !== undefined) {
          ret.width = col.style.width;
          if (col.style.clip) {
            ret['max-width'] = col.style.width;
            ret['white-space'] = 'nowrap';
          }
        }
        if (col.style.clip) {
          ret['overflow'] = 'hidden';
          ret['text-overflow'] = 'ellipsis';
          ret['white-space'] = 'nowrap';
        }
      }
      return ret;
    };

    function getTableHeight() {
      let panelHeight = ctrl.height;

      if (pageCount > 1) {
        panelHeight -= 26;
      }

      return (panelHeight - 31) + 'px';
    }

    function appendTableRows(tbodyElem) {
      ctrl.renderer.setTable(data);
      tbodyElem.empty();
      tbodyElem.html(ctrl.renderer.render(ctrl.pageIndex));
      // Compile the HTML generated by the renderer - this is required the row related callbacks (click, dblclick, etc...)
      ctrl.$compile(tbodyElem.contents())(scope);
    }

    function switchPage(e) {
      const el = $(e.currentTarget);
      ctrl.pageIndex = parseInt(el.text(), 10) - 1;
      renderPanel();
    }

    function appendPaginationControls(footerElem) {
      footerElem.empty();

      const pageSize = panel.pageSize || 100;
      pageCount = Math.ceil(data.rows.length / pageSize);
      if (pageCount === 1) {
        return;
      }

      const startPage = Math.max(ctrl.pageIndex - 3, 0);
      const endPage = Math.min(pageCount, startPage + 9);

      const paginationList = $('<ul></ul>');

      for (let i = startPage; i < endPage; i++) {
        const activeClass = i === ctrl.pageIndex ? 'active' : '';
        const pageLinkElem = $(
          '<li><a class="table-panel-page-link pointer ' + activeClass + '">' + (i + 1) + '</a></li>'
        );
        paginationList.append(pageLinkElem);
      }

      if (ctrl.newDataRaw) {
        paginationList.append($('<li><a class="table-panel-page-refresh pointer" name="refresh"><i class="fa fa-refresh" aria-hidden="true"></i> refresh</a></li>'));
      }
      footerElem.append(paginationList);
    }

    function refreshData() {
      ctrl.refreshData();
    }
  
    function renderPanel() {
      const panelElem = elem.parents('.panel-content');
      const rootElem = elem.find('.table-panel-scroll');
      const tbodyElem = elem.find('tbody');
      const footerElem = elem.find('.table-panel-footer .pagination');

      elem.css({'font-size': panel.fontSize});
      panelElem.addClass('table-panel-content');

      appendTableRows(tbodyElem);
      appendPaginationControls(footerElem);

      rootElem.css({'max-height': panel.scroll ? getTableHeight() : ''});
    }

    // hook up link tooltips
    elem.tooltip({
      selector: '[data-link-tooltip]',
    });

    function addFilterClicked(e) {
      const filterData = $(e.currentTarget).data();
      const options = {
        datasource: panel.datasource,
        key: data.columns[filterData.column].text,
        value: data.rows[filterData.row][filterData.column],
        operator: filterData.operator,
      };

      ctrl.variableSrv.setAdhocFilter(options);
    }

    elem.on('click', '.table-panel-page-link', switchPage);
    elem.on('click', '.table-panel-filter-link', addFilterClicked);
    elem.on('click', '.table-panel-page-refresh', refreshData);

    const unbindDestroy = scope.$on('$destroy', function () {
      elem.off('click', '.table-panel-page-link');
      elem.off('click', '.table-panel-filter-link');
      elem.off('click', '.table-panel-page-refresh');
      unbindDestroy();
    });

    ctrl.events.on('render', renderData => {
      data = renderData || data;
      if (data) {
        renderPanel();
      }
      ctrl.renderingCompleted();
    });
  }

  // Alarm related actions

  findTableRow(source, alarmId) {
    let matchedRow;
    _.each(this.dataRaw, table => {
      let filteredRow = _.find(table.rows, row => {
        return row.meta.source === source && row.meta.alarm.id === alarmId;
      });
      if (filteredRow !== undefined) {
        matchedRow = filteredRow;
      }
    });
    return matchedRow;
  }

  getContextMenu($event, source, alarmId) {
    // Treat the right click as a left click on the row, if the row is not part of the current selection
    if (!this.selectionMgr.isRowSelected({
        source: source,
        alarmId: alarmId
      })) {
      this.onSingleClick($event, source, alarmId);
    }

    // Grab the current selection
    let selectedRows = this.selectionMgr.getSelectedRows();

    // Load up the actual alarms in the rows
    let self = this;
    selectedRows = _.map(selectedRows, row => {
      // Create new objects instead of modifying the existing rows
      // returned by SelectionMgr#getSelectedRows()
      const tableRow = self.findTableRow(row.source, row.alarmId);
      return Object.assign({}, row, {
        alarm: tableRow ? tableRow.meta.alarm : undefined,
        ticketerConfig: tableRow ? tableRow.meta.ticketerConfig : undefined
      });
    });

    // Filter out any rows for which we couldn't find the alarm
    selectedRows = _.filter(selectedRows, row => row.alarm !== void 0);

    // Generate selection-based context menu
    return new ActionMgr(this, selectedRows, this.appConfig).getContextMenu();
  }

  scheduleClickCheck($event, source, alarmId, clickTime) {
    const self = this;
    if (!self.clicks) {
      self.clicks = {};
    }

    const thisEvent = $event;
    const alarmClick = self.clicks[alarmId] || {};

    // always handle left-click, because if we're double-clicking, it's OK to deselect everything anyways
    if (thisEvent.button !== 2) {
      self.onSingleClick(thisEvent, source, alarmId);
    }

    clearTimeout(alarmClick.timeout);
    alarmClick.timeout = setTimeout(() => {
      if (self.clicks[alarmId] && self.clicks[alarmId].lastClick === clickTime) {
        if (thisEvent.button === 2) {
          // right click
          self.getContextMenu(thisEvent, source, alarmId);
        } else {
          // left click was already handled
        }
      } else {
        // pre-scheduled click time does not match last click; assuming double-click'
      }
    }, doubleClickDelay);
  }

  onRowClick($event, source, alarmId) {
    const self = this;
    if (!self.clicks) {
      self.clicks = {};
    }

    const now = Date.now();
    const thisEvent = $event;

    if (self.clicks[alarmId]) {
      const delay = now - self.clicks[alarmId].lastClick;

      if (delay < doubleClickDelay) {
        // on a double click, delete previous record and trigger the double-click action
        delete self.clicks[alarmId];
        self.onDoubleClick(thisEvent, source, alarmId);
      } else {
        // delay is too long, this is a single click now
        self.scheduleClickCheck(thisEvent, source, alarmId, now);
      }
    } else {
      // no previous click record, create a fresh one and schedule a click check
      self.clicks[alarmId] = {
        timeout: undefined
      };
      self.scheduleClickCheck(thisEvent, source, alarmId, now);
    }
    if (self.clicks[alarmId]) {
      self.clicks[alarmId].lastClick = now;
    }
  }

  onSingleClick($event, source, alarmId) {
    const isMac = navigator.platform.toUpperCase().indexOf('MAC')>=0;
    const exclusiveModifier = isMac? $event.metaKey : $event.ctrlKey;
    const rangeModifier = $event.shiftKey;
    this.selectionMgr.handleRowClick({source: source, alarmId: alarmId}, exclusiveModifier, rangeModifier);
  }

  onDoubleClick($event, source, alarmId) {
    this.selectionMgr.handleSelection([], false);
    // Show the alarm details pane
    this.alarmDetails(source, alarmId);
  }

  alarmDetails(source, alarmId) {
    const row = this.findTableRow(source, alarmId);
    if (row === undefined) {
      this.$rootScope.appEvent('alert-error', ['Unable to find matching alarm', '']);
      return;
    }

    let newScope = this.$rootScope.$new();
    newScope.severity = this.panel.severity;
    newScope.source = source;
    newScope.theme = this.panel.theme;
    newScope.alarm = row.meta.alarm;
    newScope.ticketerConfig = row.meta.ticketerConfig;

    this.$rootScope.appEvent('show-modal', {
      templateHtml: '<alarm-details-as-modal dismiss="dismiss()"></alarm-details-as-modal>',
      scope: newScope
    });
  }

  performAlarmActionOnDatasource(source, action, alarmId) {
    let self = this;
    this.datasourceSrv.get(source).then(ds => {
      if (ds.type && ds.type.indexOf("entity-datasource") < 0) {
        throw {message: 'Only OpenNMS datasources are supported'};
      } else {
        if (!ds[action]) {
          throw {message: 'Action ' + action + ' not implemented by datasource ' + ds.name + " of type " + ds.type};
        }
        return ds[action](alarmId);
      }
    }).then(() => {
      // Action was successful, remove any previous error
      delete self.error;
      // Refresh the dashboard
      self.timeSrv.refreshDashboard();
    }).catch(err => {
      self.error = err.message || "Request Error";
    });
  }

  acknowledgeAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'acknowledgeAlarm', alarmId);
  }

  unacknowledgeAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'unacknowledgeAlarm', alarmId);
  }

  clearAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'clearAlarm', alarmId);
  }

  escalateAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'escalateAlarm', alarmId);
  }

  createTicketForAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'createTicketForAlarm', alarmId);
  }

  updateTicketForAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'updateTicketForAlarm', alarmId);
  }

  closeTicketForAlarm(source, alarmId) {
    this.performAlarmActionOnDatasource(source, 'closeTicketForAlarm', alarmId);
  }

  // Multi-select handling
  getRowsInRange(from, to) {
    let rows = [];
    if (!this.table) {
      return rows;
    }

    let findIdx = selectionToMatch => {
      return _.findIndex(this.table.rows, row => {
        return row.meta.source === selectionToMatch.source && row.meta.alarm.id === selectionToMatch.alarmId;
      });
    };

    let startIdx = findIdx(from);
    if (startIdx < 0) {
      return rows;
    }

    let endIdx = findIdx(to);
    if (endIdx < 0) {
      return rows;
    }

    if (endIdx < startIdx) {
      // Swap
      [startIdx,endIdx] = [endIdx,startIdx];
    }

    for (let i = startIdx; i <= endIdx; i++) {
      rows.push({
        source: this.table.rows[i].meta.source,
        alarmId: this.table.rows[i].meta.alarm.id
      });
    }
    return rows;
  }
}

AlarmTableCtrl.templateUrl = 'panels/alarm-table/module.html';

export { AlarmTableCtrl, AlarmTableCtrl as PanelCtrl };

coreModule.directive('alarmDetailsAsModal',  alarmDetailsAsDirective);
coreModule.directive('memoEditor',  memoEditorAsDirective);
coreModule.directive('contextMenu', contextMenuAsDirective());

coreModule.directive('dynamicHeight', function($window) {
  // Used to dynamically size the alarm details modal window
  return{
    link: function(scope, element /*, attrs */) {
      const doResize = () => {
        element.css('max-height', $window.innerHeight * 0.8 + 'px');
      };

      doResize();
      element.on('$destroy', () => {
        $window.removeEventListener('resize', doResize);
      });
      $window.addEventListener('resize', doResize);
    }
  }
});
