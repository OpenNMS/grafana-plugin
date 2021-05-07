import { TableTransform, transformers } from '../panels/alarm-table/transformers';
import TableModel, { OnmsMutableColumn } from '../panels/alarm-table/table_model';

const toColumn = (val: any) => {
  return val as unknown as OnmsMutableColumn;
};

const toColumns = (val: any[]) => {
  return val.map((v) => toColumn(v));
};

describe('Table transformer', function() {
  let transformer: TableTransform;

  beforeEach(function() {
    transformer = transformers['table'];
  });

  describe('Listing the columns', function() {
    it('should return an empty list of columns when no data is present', function() {
      expect(transformer.getColumns([])).toHaveLength(0);
    });

    it('should return an empty list of columns when the given table has no columns', function() {
      let table = new TableModel();
      expect(transformer.getColumns([table])).toHaveLength(0);
    });

    it('should return the list of columns when a table has columns', function() {
      let table = new TableModel();
      table.columns = toColumns(['col1', 'col2', 'col3']);
      expect(transformer.getColumns([table])).toEqual(['col1', 'col2', 'col3']);
    });

    it('should return the intersection of all columns names when given multiple tables', function() {
      let t1 = new TableModel();
      t1.columns = toColumns(['col1', 'col2', 'col3']);
      let t2 = new TableModel();
      t2.columns = toColumns(['col1', 'col2', 'colx']);
      expect(transformer.getColumns([t1,t2])).toEqual(['col1', 'col2']);
    });
  });

  describe('Transforming the data', function() {
    it('should return all columns if none are specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push(toColumn('A'));

      let row = [1];
      table.rows.push(row);

      let panel = {};
      let model = new TableModel();

      transformer.transform([table], panel, model);
      expect(model.columns).toEqual(table.columns);
      expect(model.rows).toEqual(table.rows);
    });

    it('should filter the columns if one or more are specified in the panel definition', () => {
      let table = new TableModel();
      (table.columns as any).push('A', 'B', 'C');

      let actualRow = [1,2,3];
      let metadata = {
        'alarm': 'abc'
      };
      table.meta.entity_metadata.push(metadata);
      table.rows.push(actualRow);

      let panel = {columns: [{
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      // The meta-data that was on the original row should also be present on the
      // transformed row
      let expected = new TableModel();
      expected.columns.push(toColumn('B'));
      expected.rows.push([2]);
      expected.meta.entity_metadata.push(metadata);

      expect(model.columns).toEqual(panel.columns);
      expect(model.rows).toEqual(expected.rows);
    });

    it('should re-order the columns according the order specified in the panel definition', function() {
      let table = new TableModel();
      (table.columns as any).push('A', 'B', 'C');
      table.rows.push([1,2,3]);

      let panel = {columns: [{
        'text': 'C'
      }, {
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      expect(model.columns).toEqual(panel.columns);
      expect(model.rows).toEqual([[3, 2]]);
    });

    it('should use an undefined value when a column is present in the panel definition, but not in any of the tables', function() {
      let table = new TableModel();
      (table.columns as any).push('A', 'B', 'C');
      table.rows.push([1,2,3]);

      let panel = {columns: [{
        'text': 'C'
      }, {
        'text': 'Z'
      }, {
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      expect(model.columns).toEqual(panel.columns);
      expect(model.rows).toEqual([[3, undefined, 2]]);
    });

    it('should combine multiple tables into a single table', function() {
      let t1 = new TableModel();
      t1.columns.push(toColumn('A'));
      t1.rows.push([1]);

      let t2 = new TableModel();
      t2.columns.push(toColumn('A'));
      t2.rows.push([2]);

      let panel = {};
      let model = new TableModel();

      transformer.transform([t1,t2], panel, model);
      expect(model.columns).toEqual(t1.columns);
      expect(model.rows).toEqual([[1], [2]]);
    });

    it('should deduplicate alarms originating from the same datasource', function() {
      let alarm_from_ds1_as_row = [1];
      let alarm_from_ds1_as_row_meta = {
        source: 'ds1',
        alarm: {
          id: 1
        }
      };

      let alarm_from_ds2_as_row = [2];
      let alarm_from_ds2_as_row_meta = {
        source: 'ds2',
        alarm: {
          id: 1
        }
      };

      let t1 = new TableModel();
      t1.columns.push(toColumn('ID'));
      t1.rows.push(alarm_from_ds1_as_row);
      t1.meta.entity_metadata.push(alarm_from_ds1_as_row_meta);

      let t2 = new TableModel();
      t2.columns.push(toColumn('ID'));
      t2.rows.push(alarm_from_ds1_as_row);
      t2.meta.entity_metadata.push(alarm_from_ds1_as_row_meta);
      t2.rows.push(alarm_from_ds2_as_row);
      t2.meta.entity_metadata.push(alarm_from_ds2_as_row_meta);

      let panel = {};
      let model = new TableModel();

      transformer.transform([t1,t2], panel, model);
      expect(model.columns).toEqual(t1.columns);
      // alarm_from_ds1_as_row should only appear once
      expect(model.rows).toEqual([alarm_from_ds1_as_row, alarm_from_ds2_as_row]);
      // alarm_from_ds1_as_row_meta should only appear once
      expect(model.meta.entity_metadata).toEqual([alarm_from_ds1_as_row_meta, alarm_from_ds2_as_row_meta]);
    });
  });

});
