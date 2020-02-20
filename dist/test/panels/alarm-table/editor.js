'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.TablePanelEditorCtrl = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

exports.tablePanelEditor = tablePanelEditor;

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _transformers = require('./transformers');

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var TablePanelEditorCtrl = exports.TablePanelEditorCtrl = function () {
  /** @ngInject */
  function TablePanelEditorCtrl($scope, $q, uiSegmentSrv) {
    var _this = this;

    _classCallCheck(this, TablePanelEditorCtrl);

    this.$q = $q;
    this.$scope = $scope;
    this.uiSegmentSrv = uiSegmentSrv;
    $scope.editor = this;
    this.panelCtrl = $scope.ctrl;
    this.panel = this.panelCtrl.panel;
    this.transformers = _transformers.transformers;
    this.fontSizes = ['80%', '90%', '100%', '110%', '120%', '130%', '150%', '160%', '180%', '200%', '220%', '250%'];

    this.srcIndex = undefined;
    this.destIndex = undefined;

    this.addColumnSegment = uiSegmentSrv.newPlusButton();
    var editor = document.querySelectorAll('.editor-row')[0];
    var _arr = ['dragstart', 'dragover', 'dragleave', 'drop'];

    var _loop = function _loop() {
      var e = _arr[_i];
      //console.log('adding listener: ' + e);
      editor.addEventListener(e, function (evt) {
        _this.handleEvent(e, evt);
      }, false);
    };

    for (var _i = 0; _i < _arr.length; _i++) {
      _loop();
    }
  }

  _createClass(TablePanelEditorCtrl, [{
    key: 'removeClasses',
    value: function removeClasses() {
      for (var _len = arguments.length, classes = Array(_len), _key = 0; _key < _len; _key++) {
        classes[_key] = arguments[_key];
      }

      var _iteratorNormalCompletion = true;
      var _didIteratorError = false;
      var _iteratorError = undefined;

      try {
        var _loop2 = function _loop2() {
          var c = _step.value;

          var cols = document.querySelectorAll('.' + c);
          [].forEach.call(cols, function (col) {
            col.classList.remove(c);
          });
        };

        for (var _iterator = classes[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
          _loop2();
        }
      } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
      } finally {
        try {
          if (!_iteratorNormalCompletion && _iterator.return) {
            _iterator.return();
          }
        } finally {
          if (_didIteratorError) {
            throw _iteratorError;
          }
        }
      }
    }
  }, {
    key: 'getTarget',
    value: function getTarget(evt) {
      if (evt.srcElement && evt.srcElement.offsetParent) {
        var target = evt.srcElement.offsetParent;
        if (target && target.id && target.classList && target.classList.contains('column-reorder')) {
          return target;
        }
      }
      // dragleave is only fired for the label and not the parent container
      if (evt.target && evt.target.parent && evt.target.parent.classList && evt.target.parent.classList.contains('column-reorder')) {
        return evt.target.parent;
      }
    }
  }, {
    key: 'handleEvent',
    value: function handleEvent(type, evt) {
      var _this2 = this;

      var target = this.getTarget(evt);
      var id = evt.srcElement.id;

      switch (type) {
        case 'dragstart':
          evt.srcElement.classList.add('picked-up');
          evt.dataTransfer.effectAllowed = 'move';
          // Internet Explorer doesn't support "text/html":
          // https://stackoverflow.com/a/28740710
          try {
            evt.dataTransfer.setData('text/html', evt.srcElement.innerHTML);
          } catch (error) {
            evt.dataTransfer.setData('text', evt.srcElement.innerHTML);
          }
          if (id) {
            this.srcIndex = parseInt(id.replace(/^column-/, ''), 10);
            console.log('picking up "' + this.panel.columns[this.srcIndex].text + '"');
          }
          break;
        case 'dragover':
          if (evt.preventDefault) {
            evt.preventDefault();
          }
          evt.dataTransfer.dropEffect = 'move';
          if (target && target.id && target.classList && target.classList.contains('column-reorder')) {
            var columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
            if (!target.classList.contains('over')) {
              //console.log('entering ' + this.panel.columns[columnIndex].text);
              this.removeClasses('over');
              target.classList.add('over');
              this.destIndex = columnIndex;
            }
          }
          break;
        case 'dragleave':
          if (target && evt.screenX !== 0 && evt.screenY !== 0) {
            //const columnIndex = parseInt(target.id.replace(/^column-/, ''), 10);
            //console.log('leaving ' + this.panel.columns[columnIndex].text);
            this.destIndex = undefined;
            this.removeClasses('over');
          }
          break;
        case 'drop':
          if (eval.stopPropagation) {
            evt.stopPropagation();
          }
          if (this.srcIndex !== undefined && this.destIndex !== undefined) {
            this.$scope.$apply(function () {
              _this2.panel.columns.splice(_this2.destIndex, 0, _this2.panel.columns.splice(_this2.srcIndex, 1)[0]);
              _this2.panelCtrl.render();
            });
            console.log('dropped "' + this.panel.columns[this.srcIndex].text + '" onto "' + this.panel.columns[this.destIndex].text + '"');
          } else {
            var targetIndex = this.srcIndex == undefined ? 'source' : 'destination';
            console.log('WARNING: drop event received but ' + targetIndex + ' was unset.');
          }
          this.removeClasses('over', 'picked-up');
          return false;
        default:
          console.log('WARNING: unhandled event type: ' + type);
      }
    }
  }, {
    key: 'getColumnOptions',
    value: function getColumnOptions() {
      var _this3 = this;

      if (!this.panelCtrl.dataRaw) {
        return this.$q.when([]);
      }
      var columns = this.transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
      // Filter out columns that have already been selected
      var self = this;
      columns = columns.filter(function (a) {
        return self.panel.columns.indexOf(a) < 0;
      });
      var segments = _lodash2.default.map(columns, function (c) {
        return _this3.uiSegmentSrv.newSegment({ value: c.text });
      });
      return this.$q.when(segments);
    }
  }, {
    key: 'addColumn',
    value: function addColumn() {
      var columns = _transformers.transformers[this.panel.transform].getColumns(this.panelCtrl.dataRaw);
      var column = _lodash2.default.find(columns, { text: this.addColumnSegment.value });

      if (column) {
        this.panel.columns.push(column);
        this.render();
      }

      var plusButton = this.uiSegmentSrv.newPlusButton();
      this.addColumnSegment.html = plusButton.html;
      this.addColumnSegment.value = plusButton.value;
    }
  }, {
    key: 'transformChanged',
    value: function transformChanged() {
      this.panel.columns = [];
      if (this.panel.transform === 'timeseries_aggregations') {
        this.panel.columns.push({ text: 'Avg', value: 'avg' });
      }

      this.render();
    }
  }, {
    key: 'render',
    value: function render() {
      this.panelCtrl.render();
    }
  }, {
    key: 'removeColumn',
    value: function removeColumn(column) {
      this.panel.columns = _lodash2.default.without(this.panel.columns, column);
      this.panelCtrl.render();
    }
  }]);

  return TablePanelEditorCtrl;
}();

/** @ngInject */


function tablePanelEditor($q, uiSegmentSrv) {
  // eslint-disable-line no-unused-vars
  'use strict';

  return {
    restrict: 'E',
    scope: true,
    templateUrl: '/public/plugins/opennms-helm-app/panels/alarm-table/editor.html',
    controller: TablePanelEditorCtrl
  };
}
//# sourceMappingURL=editor.js.map
