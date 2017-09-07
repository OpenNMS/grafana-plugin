import _ from 'lodash';

export class SelectionMgr {

  constructor(getRowsInRange, onSelectionChangeCallback) {
    this.clearSelectedRows();
    this.getRowsInRange = getRowsInRange;
    this.onSelectionChangeCallback = onSelectionChangeCallback;
  }

  clearSelectedRows() {
    this._selectedRows = new Set();
    this._lastSelectedRow = undefined;
  }

  handleRowClick(row, exclusiveModifier, rangeModifier) {
    let selectedRows;
    if (!rangeModifier || this._lastSelectedRow === undefined) {
      // No other row was previously selected, use the row that was clicked on
      selectedRows = [row];
    } else {
      // Build the list of rows between the last row that was clicked
      // and the current row
      selectedRows = this.getRowsInRange(this._lastSelectedRow, row);
    }
    this.handleSelection(selectedRows, exclusiveModifier);
  }

  isRowSelected(row) {
    for (let selectedRow of this._selectedRows) {
      if (_.isEqual(selectedRow, row)) {
        return true;
      }
    }
    return false;
  }

  getSelectedRows() {
    return Array.from(this._selectedRows);
  }

  addRowToSelection(row) {
    this._selectedRows.add(row);
    this._lastSelectedRow = row;
  }

  handleSelection(selectionRows, exclusiveModifier) {
    let didSelectionChange = false;
    if (!exclusiveModifier) {
      // Determine the rows we need to add to the selection
      let rowsToAddToSelection = _.filter(selectionRows, selectionRow => !this.isRowSelected(selectionRow));

      // Determine the rows we need to remove from the selection
      let rowsToRemoveFromSelection = _.filter(this.getSelectedRows(), selectedRow => {
        return _.findIndex(selectionRows, selectionRow => _.isEqual(selectedRow, selectionRow)) < 0;
      });

      if (rowsToRemoveFromSelection.length > 0) {
        // Clear everything and add all the selected rows
        this.clearSelectedRows();
        _.each(selectionRows, selectionRow => this.addRowToSelection(selectionRow));
        didSelectionChange = true;
      } else if (rowsToAddToSelection.length > 0) {
        // Add the selected rows
        _.each(rowsToAddToSelection, rowToAddToSelection =>  this.addRowToSelection(rowToAddToSelection));
        didSelectionChange = true;
      }
    } else {
      // Add the rows to the current selection
      _.each(selectionRows, selectionRow => {
        if (!this.isRowSelected(selectionRow)) {
          this.addRowToSelection(selectionRow);
          didSelectionChange = true;
        }
      });
    }

    if (didSelectionChange && this.onSelectionChangeCallback) {
      this.onSelectionChangeCallback();
    }
  }
}
