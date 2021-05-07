import _ from "lodash";

import { Model } from 'opennms/src/API';
import { OnmsAlarm } from 'opennms/src/model/OnmsAlarm';
import { OnmsParm } from 'opennms/src/model/OnmsParm';

import AlarmEntity from '../datasources/entity-ds/AlarmEntity';

describe('AlarmEntity', function() {

  let toTable = function(alarms) {
    let columns = Array.from(AlarmEntity.builtinColumns());
    return AlarmEntity.toTable(alarms, columns, {}, "");
  };

  describe('toTable', function() {
    it('should convert an empty list of alarms to an empty table', function() {
      let table = toTable([]);
      expect(table[0]['rows']).toHaveLength(0);
    });

    it('should convert a single alarm to a valid table', function() {
      let alarm = new Model.OnmsAlarm();
      let table = toTable([alarm]);
      expect(table[0]['rows']).toHaveLength(1);
    });

    it('should convert many alarms to a valid table', function() {
      let NUM_ALARMS = 500;
      let NUM_UNIQUE_PARM_NAMES = 50;
      let NUM_PARMS_PER_ALARM = 5;
      let alarms = [] as OnmsAlarm[];
      for (let i = 0; i < NUM_ALARMS; i++) {
        let alarm = new Model.OnmsAlarm();

        // Append a bunch of parameters
        let parms = [] as OnmsParm[];
        for (let j = 0; j < NUM_PARMS_PER_ALARM; j++) {
          parms.push(new Model.OnmsParm(
            ".1.3.6.1.2.1.15.3.1.14.39.252.8." + ((i + j) % NUM_UNIQUE_PARM_NAMES),
            "10.1.1.1",
            "string")
          );
        }
        alarm.lastEvent = new Model.OnmsEvent();
        alarm.lastEvent.parameters = parms;

        alarms.push(alarm);
      }

      // Convert
      let table = toTable(alarms);

      // Expect # rows = # alarms
      expect(table[0]['rows']).toHaveLength(NUM_ALARMS);

      // Fetch all the column names that start with Param_
      let columnNamesFromParms = _.map(table[0]['columns'], column => {
        return column.text;
      }).filter(name => name.startsWith("Param_"));
      // The count should match the number of unique params we generated
      expect(columnNamesFromParms).toHaveLength(NUM_UNIQUE_PARM_NAMES);
    });
  });

});
