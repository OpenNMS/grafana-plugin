import _ from 'lodash';

export class SelectionMgr {
  private _selectedRows = new Set();
  private _lastSelectedRow: any;
  getRowsInRange: (first: number, last: number) => any[];
  onSelectionChangeCallback: () => void;

  constructor(getRowsInRange, onSelectionChangeCallback) {
    this.clearSelectedRows();
    this.getRowsInRange = getRowsInRange;
    this.onSelectionChangeCallback = onSelectionChangeCallback;
  }

  clearSelectedRows() {
    this._selectedRows = new Set();
    this._lastSelectedRow = undefined;
  }

  handleRowClick(row: number, exclusiveModifier: boolean, rangeModifier: boolean) {
    let selectedRows: any[];
    if (!rangeModifier && !exclusiveModifier && this._selectedRows.size === 1 && this._lastSelectedRow && _.isEqual(this._lastSelectedRow, row)) {
      selectedRows = [];
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

  removeRowFromSelection(row) {
    this._selectedRows = new Set(Array.from(this._selectedRows).filter((r: any) => r.alarmId !== row.alarmId ));
    this._lastSelectedRow = this._selectedRows[0];
  }

  handleSelection(selectionRows: any[], exclusiveModifier: boolean) {
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
      const selected = this.isRowSelected(selectionRows[selectionRows.length - 1]);
      // Add the rows to the current selection
      _.each(selectionRows, selectionRow => {
        if (selected) {
          this.removeRowFromSelection(selectionRow);
          didSelectionChange = true;
        } else {
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
