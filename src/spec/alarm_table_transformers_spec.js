import {transformers} from '../panels/alarm-table/transformers';
import {TableModel} from '../panels/alarm-table/table_model';

describe('Table transformer', function() {
  let transformer;

  beforeEach(function() {
    transformer = transformers['table'];
  });

  describe('Listing the columns', function() {
    it('should return an empty list of columns when no data is present', function() {
      expect(transformer.getColumns([])).to.have.length(0);
    });

    it('should return an empty list of columns when the given table has no columns', function() {
      let table = new TableModel();
      expect(transformer.getColumns([table])).to.have.length(0);
    });

    it('should return the list of columns when a table has columns', function() {
      let table = new TableModel();
      table.columns = ['col1', 'col2', 'col3'];
      expect(transformer.getColumns([table])).to.eql(['col1', 'col2', 'col3']);
    });

    it('should return the intersection of all columns names when given multiple tables', function() {
      let t1 = new TableModel();
      t1.columns = ['col1', 'col2', 'col3'];
      let t2 = new TableModel();
      t2.columns = ['col1', 'col2', 'colx'];
      expect(transformer.getColumns([t1,t2])).to.eql(['col1', 'col2']);
    });
  });

  describe('Transforming the data', function() {
    it('should return all columns if none are specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push("A");

      let row = [1];
      table.rows.push(row);

      let panel = {};
      let model = new TableModel();

      transformer.transform([table], panel, model);
      expect(model.columns).to.eql(table.columns);
      expect(model.rows).to.eql(table.rows);
    });

    it('should filter the columns if one or more are specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push('A', 'B', 'C');

      let actualRow = [1,2,3];
      let metadata = {
        'alarm': 'abc'
      };
      actualRow.meta = metadata;
      table.rows.push(actualRow);

      let panel = {columns: [{
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      // The meta-data that was on the original row should also be present on the
      // transformed row
      let expectedRow = [2];
      expectedRow.meta = metadata;

      expect(model.columns).to.eql(panel.columns);
      expect(model.rows).to.eql([expectedRow]);
    });

    it('should re-order the columns according the order specified in the panel definition', function() {
      let table = new TableModel();
      table.columns.push('A', 'B', 'C');
      table.rows.push([1,2,3]);

      let panel = {columns: [{
        'text': 'C'
      }, {
        'text': 'B'
      }]};
      let model = new TableModel();

      transformer.transform([table], panel, model);

      expect(model.columns).to.eql(panel.columns);
      expect(model.rows).to.eql([[3, 2]]);
    });

    it('should use an undefined value when a column is present in the panel definition, but not in any of the tables', function() {
      let table = new TableModel();
      table.columns.push('A', 'B', 'C');
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

      expect(model.columns).to.eql(panel.columns);
      expect(model.rows).to.eql([[3, undefined, 2]]);
    });

    it('should combine multiple tables into a single table', function() {
      let t1 = new TableModel();
      t1.columns.push("A");
      t1.rows.push([1]);

      let t2 = new TableModel();
      t2.columns.push("A");
      t2.rows.push([2]);

      let panel = {};
      let model = new TableModel();

      transformer.transform([t1,t2], panel, model);
      expect(model.columns).to.eql(t1.columns);
      expect(model.rows).to.eql([[1], [2]]);
    });

    it('should deduplicate alarms originating from the same datasource', function() {
      let alarm_from_ds1_as_row = [1];
      alarm_from_ds1_as_row.meta = {
        source: 'ds1',
        alarm: {
          id: 1
        }
      };

      let alarm_from_ds2_as_row = [2];
      alarm_from_ds2_as_row.meta = {
        source: 'ds2',
        alarm: {
          id: 1
        }
      };

      let t1 = new TableModel();
      t1.columns.push("ID");
      t1.rows.push(alarm_from_ds1_as_row);

      let t2 = new TableModel();
      t2.columns.push("ID");
      t2.rows.push(alarm_from_ds1_as_row);
      t2.rows.push(alarm_from_ds2_as_row);

      let panel = {};
      let model = new TableModel();

      transformer.transform([t1,t2], panel, model);
      expect(model.columns).to.eql(t1.columns);
      // alarm_from_ds1_as_row should only appear once
      expect(model.rows).to.eql([alarm_from_ds1_as_row, alarm_from_ds2_as_row]);
    });
  });

});