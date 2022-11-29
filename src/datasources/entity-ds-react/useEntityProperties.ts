import { API } from 'opennms'
import { ClientDelegate } from 'lib/client_delegate'
import { useState, useEffect } from 'react'
import { EntityHelper } from '.';
import { EntityTypes } from './constants';
import { Properties, SearchOption } from './types';

export const useEntityProperties = (entityName: string, client: ClientDelegate) => {
    const [properties, setProperties] = useState<Properties>({});
    const [propertiesAsArray, setPropertiesAsArray] = useState<SearchOption[]>([]);
    const [propertiesLoading, setPropertiesLoading] = useState(false);

    const loadProperties = async () => {
        let newProperties: API.SearchProperty[] = []

        switch (entityName) {
            case EntityTypes.Alarms:
                newProperties = await client.getAlarmProperties();
                break;
            case EntityTypes.Nodes:
                newProperties = await client.getNodeProperties();
                break;
            case EntityTypes.IPInterfaces:
                newProperties = await client.getIpInterfaceProperties();
                break;
            case EntityTypes.MonitoredServices:
                newProperties = await client.getMonitoredServiceProperties();
                break;
            case EntityTypes.Outages:
                newProperties = await client.getOutageProperties();
                break;
            case EntityTypes.SNMPInterfaces:
                newProperties = await client.getSnmpInterfaceProperties();
                break;
        }
        let filteredProps = EntityHelper.filterProperties(entityName, newProperties);

        setProperties(() => filteredProps)
    }

    useEffect(() => {
        setPropertiesLoading(true);
        loadProperties();
        setPropertiesLoading(false);

        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [entityName, client])

    useEffect(() => {
        const newPropertyArray: SearchOption[] = [];
        for (let [_, value] of Object.entries(properties)) {
            newPropertyArray.push(value as SearchOption)
        }
        setPropertiesAsArray(newPropertyArray);
    }, [properties])

    return { properties, propertiesLoading, propertiesAsArray }
}
