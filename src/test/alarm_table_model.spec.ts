import TableModel, { OnmsMutableColumn } from '../panels/alarm-table/table_model';

describe('TableModel', function() {

  it('should work', function () {
    let a1_row = [1,2,"a1"];
    let a1_row_meta  = {
      source: 'ds1',
      alarm: {
        id: 1
      }
    };

    let a2_row = [2,1,"a2"];
    let a2_row_meta  = {
      source: 'ds1',
      alarm: {
        id: 2
      }
    };

    let table = new TableModel();
    table.columns.push({'label': 'ID'} as unknown as OnmsMutableColumn);
    table.columns.push({'label': 'Count'} as unknown as OnmsMutableColumn);
    table.columns.push({'label': 'Label'} as unknown as OnmsMutableColumn);
    // Add both alarms to the table
    table.rows.push(a1_row);
    table.meta.entity_metadata.push(a1_row_meta);
    table.rows.push(a2_row);
    table.meta.entity_metadata.push(a2_row_meta);

    expect(table.rows).toHaveLength(2);

    // Sort by id - the first column
    table.sort({'col': 0});
    // IDs should be ordered, [1,2]
    expect(table.rows[0][0]).toEqual(1);
    expect(table.rows[1][0]).toEqual(2);
    // Meta data should also be ordered
    expect(table.meta.entity_metadata[0].alarm.id).toEqual(1);
    expect(table.meta.entity_metadata[1].alarm.id).toEqual(2);

    // Sort by id - the first column - but descending
    table.sort({'col': 0, 'desc': true});
    // IDs should be ordered, [2,1]
    expect(table.rows[0][0]).toEqual(2);
    expect(table.rows[1][0]).toEqual(1);
    // Meta data should also be ordered
    expect(table.meta.entity_metadata[0].alarm.id).toEqual(2);
    expect(table.meta.entity_metadata[1].alarm.id).toEqual(1);

    // Now sort by count - the second column
    table.sort({'col': 1});
    // Counts should be ordered, [1,2]
    expect(table.rows[0][1]).toEqual(1);
    expect(table.rows[1][1]).toEqual(2);
    // Meta data should also be ordered
    expect(table.meta.entity_metadata[0].alarm.id).toEqual(2);
    expect(table.meta.entity_metadata[1].alarm.id).toEqual(1);

  });

});
