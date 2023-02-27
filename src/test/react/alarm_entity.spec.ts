import _ from "lodash";

// import { Model } from 'opennms/src/API';
import { OnmsEvent } from 'opennms/src/model/OnmsEvent'
import { OnmsAlarm } from 'opennms/src/model/OnmsAlarm'
import { OnmsParm } from 'opennms/src/model/OnmsParm'
import { Client } from 'opennms/src/Client'
import { queryAlarms } from 'datasources/entity-ds-react/queries/queryAlarms'
import { ClientDelegate } from 'lib/client_delegate'

describe('queryAlarms', function () {
  const settings = { url: 'http://localhost', type: null, name: null }
  const client = new ClientDelegate(settings, undefined)
  client.getClientWithMetadata = async () => {
    return Promise.resolve(new Client())
  }

  describe('should convert an empty list of alarms to an empty table', () => {
    client.findAlarms = (filter) => { return Promise.resolve([]) }
    it('should convert an empty list of alarms to an empty table', () => {
      queryAlarms(client, undefined).then(table => {
        expect(table['rows']).toHaveLength(0)
      })
    })
  })

  it('should convert a single alarm to a valid table', function () {
    let alarm = new OnmsAlarm();
    client.findAlarms = (filter) => { return Promise.resolve([alarm]) }
    queryAlarms(client, undefined).then(table => {
      expect(table['rows']).toHaveLength(1)
    })
  })

  it('should convert many alarms to a valid table', function () {
    let NUM_ALARMS = 500;
    let NUM_UNIQUE_PARM_NAMES = 50;
    let NUM_PARMS_PER_ALARM = 5;
    let alarms = [] as OnmsAlarm[];
    for (let i = 0; i < NUM_ALARMS; i++) {
      let alarm = new OnmsAlarm();

      // Append a bunch of parameters
      let parms = [] as OnmsParm[];
      for (let j = 0; j < NUM_PARMS_PER_ALARM; j++) {
        parms.push(new OnmsParm(
          ".1.3.6.1.2.1.15.3.1.14.39.252.8." + ((i + j) % NUM_UNIQUE_PARM_NAMES),
          "10.1.1.1",
          "string")
        );
      }
      alarm.lastEvent = new OnmsEvent();
      alarm.lastEvent.parameters = parms;

      alarms.push(alarm);
    }

    client.findAlarms = (filter) => { return Promise.resolve(alarms) }

    queryAlarms(client, undefined).then(table => {

      // Expect # rows = # alarms
      expect(table['rows']).toHaveLength(NUM_ALARMS);

      // Fetch all the column names that start with Param_
      let columnNamesFromParms = table['columns']
        .map(column => { return column.text })
        .filter(name => name.startsWith("Param_"))

      // The count should match the number of unique params we generated
      expect(columnNamesFromParms).toHaveLength(NUM_UNIQUE_PARM_NAMES);
    })
  })

})


