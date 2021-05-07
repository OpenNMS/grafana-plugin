import _ from 'lodash';
import TableModel, { OnmsMutableColumn } from './table_model';
import { Column } from '@grafana/data';

export interface TableTransform {
  description: string;
  getColumns(data?: any): any[];
  transform(data: TableModel[], panel: any, model: TableModel): void;
  transformTable(data: TableModel, columnsToInclude: Column[]): TableModel;
  mergeTables(tables): TableModel;
}

const transformers: { [key: string]: TableTransform } = {};

transformers.table = {
  description: 'Table',
  getColumns: (data) => {
    if (!data || data.length === 0) {
      return [];
    }

    // Create a list containing the column names from each table
    let columns = [] as Column[][];
    _.forEach(data, (table: TableModel) => {
      columns.push(table.columns as Column[]);
    });
    // Return the intersection of all column names only returning
    // those that are present in each table
    // NOTE: _.intersection([array]) should work here, but appears to be broken
    const first = columns.shift();
    if (!first) {
      return [] as Column[];
    }
    return first.filter((v) => {
      return columns.every((a) => {
        return a.indexOf(v) !== -1;
      });
    });
  },

  transformTable: (data: TableModel, columnsToInclude: Column[]): TableModel => {
    const model = new TableModel();

    // Convert the rows to columns
    let cellsByColumnIndex = [] as any[];
    _.each(data.columns, () => cellsByColumnIndex.push([]));

    // For every row
    _.each(data.rows, (row) => {
      // And every cell in the row
      for (let i = 0; i < row.length; i++) {
        // Append the cell to the appropriate column list
        cellsByColumnIndex[i].push(row[i]);
      }
    });

    // Used when no matching column is found
    let emptyColumn = new Array(data.rows.length);

    // Now reorder the columns according the list of columns in the panel definition
    let cellsByPanelColumnIndex = [] as any[];
    for (let j = 0; j < columnsToInclude.length; j++) {
      let colDef = columnsToInclude[j];
      // Determine the column's index in the given data
      let idx = _.findIndex(data.columns, (col) => {
        return (col as unknown as string) === colDef.text || col.text === colDef.text;
      });
      if (idx < 0) {
        // The column does not exist
        cellsByPanelColumnIndex[j] = emptyColumn;
        continue;
      }
      // Re-order
      cellsByPanelColumnIndex[j] = cellsByColumnIndex[idx];
    }

    // Convert the columns back to rows
    model.columns = columnsToInclude as OnmsMutableColumn[];
    model.rows = [];
    for (let k = 0; k < cellsByPanelColumnIndex[0].length; k++) {
      let row = [] as any[];
      for (let l = 0; l < columnsToInclude.length; l++) {
        row.push(cellsByPanelColumnIndex[l][k]);
      }
      model.rows.push(row);
      model.meta.entity_metadata.push(data.meta && data.meta.entity_metadata ? data.meta.entity_metadata[k] : undefined);
    }

    return model;
  },

  mergeTables: (tables): TableModel => {
    let model = new TableModel();

    // Use the list of columns from the first table:
    //  transformTable() will ensure that all tables have the same columns
    model.columns = tables[0].columns;

    // Concatenate the rows
    _.each(tables, table => {
      const tableRows = _.map(table.rows, (row, idx) => {
        // temporarily stuff the meta into the row object so we can `uniqBy` easily
        if (table && table.meta && table.meta.entity_metadata && table.meta.entity_metadata[idx]) {
          row.meta = table.meta.entity_metadata[idx];
        }
        return row;
      });
      model.rows = _.concat(model.rows, tableRows);
    });

    // De-duplicate by (source, alarm.id) tuple
    model.rows = _.uniqBy(model.rows, row => {
      if (row.meta && row.meta.alarm) {
        return JSON.stringify({
          source: row.meta.source,
          alarmId: row.meta.alarm.id
        });
      } else {
        return JSON.stringify(row);
      }
    });

    model.meta.entity_metadata = _.map(model.rows, row => {
      const meta = row.meta;
      delete row.meta;
      return meta;
    });

    return model;
  },

  transform: function(data: TableModel[], panel: any, model: TableModel) {
    if (!data || data.length === 0) {
      return;
    }

    // Determine the list of columns to include
    let columnsToInclude = [] as any[];
    if (panel.columns && panel.columns.length > 0) {
      // One or more columns were specified in the column definition, use these
      columnsToInclude = panel.columns as any[];
    } else {
      // Use all the available columns
      columnsToInclude = this.getColumns(data as any[]);
    }

    // Transform each of the datasource results individually
    let transformedTables = [] as TableModel[];
    _.each(data, (dat: TableModel) => {
      if (dat.type !== 'table') {
        throw {message: 'Query result is not in table format, try using another transform.'};
      }
      transformedTables.push(this.transformTable(dat, columnsToInclude));
    });

    // Merge the results and update the model
    let mergedTables = this.mergeTables(transformedTables);
    model.columns = mergedTables.columns;
    model.meta = mergedTables.meta;
    model.rows = mergedTables.rows;
  }
} as TableTransform;

function transformDataToTable(data, panel) {
  let model = new TableModel();

  if (!data || data.length === 0) {
    return model;
  }

  let transformer = transformers[panel.transform];
  if (!transformer) {
    throw {message: 'Transformer ' + panel.transform + ' not found'};
  }

  transformer.transform(data, panel, model);
  return model;
}

export {transformers, transformDataToTable};
