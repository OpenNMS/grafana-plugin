'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.SelectionMgr = undefined;

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

var SelectionMgr = exports.SelectionMgr = function () {
  function SelectionMgr(getRowsInRange, onSelectionChangeCallback) {
    _classCallCheck(this, SelectionMgr);

    this.clearSelectedRows();
    this.getRowsInRange = getRowsInRange;
    this.onSelectionChangeCallback = onSelectionChangeCallback;
  }

  _createClass(SelectionMgr, [{
    key: 'clearSelectedRows',
    value: function clearSelectedRows() {
      this._selectedRows = new Set();
      this._lastSelectedRow = undefined;
    }
  }, {
    key: 'handleRowClick',
    value: function handleRowClick(row, exclusiveModifier, rangeModifier) {
      var selectedRows = void 0;
      if (!rangeModifier && !exclusiveModifier && this._selectedRows.size === 1 && this._lastSelectedRow && _lodash2.default.isEqual(this._lastSelectedRow, row)) {
        selectedRows = new Set();
      } else if (!rangeModifier || this._lastSelectedRow === undefined) {
        // No other row was previously selected, use the row that was clicked on
        selectedRows = [row];
      } else {
        // Build the list of rows between the last row that was clicked
        // and the current row
        selectedRows = this.getRowsInRange(this._lastSelectedRow, row);
      }
      this.handleSelection(selectedRows, exclusiveModifier);
    }
  }, {
    key: 'isRowSelected',
    value: function isRowSelected(row) {
      var _iteratorNormalCompletion = true;
      var _didIteratorError = false;
      var _iteratorError = undefined;

      try {
        for (var _iterator = this._selectedRows[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
          var selectedRow = _step.value;

          if (_lodash2.default.isEqual(selectedRow, row)) {
            return true;
          }
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

      return false;
    }
  }, {
    key: 'getSelectedRows',
    value: function getSelectedRows() {
      return Array.from(this._selectedRows);
    }
  }, {
    key: 'addRowToSelection',
    value: function addRowToSelection(row) {
      this._selectedRows.add(row);
      this._lastSelectedRow = row;
    }
  }, {
    key: 'removeRowFromSelection',
    value: function removeRowFromSelection(row) {
      this._selectedRows = new Set(Array.from(this._selectedRows).filter(function (r) {
        return r.alarmId !== row.alarmId;
      }));
      this._lastSelectedRow = this._selectedRows[0];
    }
  }, {
    key: 'handleSelection',
    value: function handleSelection(selectionRows, exclusiveModifier) {
      var _this = this;

      var didSelectionChange = false;
      if (!exclusiveModifier) {
        // Determine the rows we need to add to the selection
        var rowsToAddToSelection = _lodash2.default.filter(selectionRows, function (selectionRow) {
          return !_this.isRowSelected(selectionRow);
        });

        // Determine the rows we need to remove from the selection
        var rowsToRemoveFromSelection = _lodash2.default.filter(this.getSelectedRows(), function (selectedRow) {
          return _lodash2.default.findIndex(selectionRows, function (selectionRow) {
            return _lodash2.default.isEqual(selectedRow, selectionRow);
          }) < 0;
        });

        if (rowsToRemoveFromSelection.length > 0) {
          // Clear everything and add all the selected rows
          this.clearSelectedRows();
          _lodash2.default.each(selectionRows, function (selectionRow) {
            return _this.addRowToSelection(selectionRow);
          });
          didSelectionChange = true;
        } else if (rowsToAddToSelection.length > 0) {
          // Add the selected rows
          _lodash2.default.each(rowsToAddToSelection, function (rowToAddToSelection) {
            return _this.addRowToSelection(rowToAddToSelection);
          });
          didSelectionChange = true;
        }
      } else {
        var selected = this.isRowSelected(selectionRows[selectionRows.length - 1]);
        // Add the rows to the current selection
        _lodash2.default.each(selectionRows, function (selectionRow) {
          if (selected) {
            _this.removeRowFromSelection(selectionRow);
            didSelectionChange = true;
          } else {
            _this.addRowToSelection(selectionRow);
            didSelectionChange = true;
          }
        });
      }

      if (didSelectionChange && this.onSelectionChangeCallback) {
        this.onSelectionChangeCallback();
      }
    }
  }]);

  return SelectionMgr;
}();
//# sourceMappingURL=selection_mgr.js.map
