import { API } from 'opennms'
import { ClientDelegate } from 'lib/client_delegate'
import { useState, useEffect } from 'react'
import { EntityHelper } from '../datasources/entity-ds-react';
import { EntityTypes } from '../constants/constants';
import { Properties, SearchOption } from '../datasources/entity-ds-react/types';

export const useEntityProperties = (entityName: string, featuredAttributes: boolean, client: ClientDelegate) => {
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

        const filteredProps = featuredAttributes ?
            EntityHelper.filterProperties(entityName, newProperties) :
            EntityHelper.generateProperties(newProperties)

        setProperties(() => filteredProps)
    }

    useEffect(() => {
        const updateProperties = async () => {
            setPropertiesLoading(true);
            await loadProperties();
            setPropertiesLoading(false);
        }
        updateProperties();
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [entityName, featuredAttributes, client])

    useEffect(() => {
        const newPropertyArray: SearchOption[] = [];
        for (let [_, value] of Object.entries(properties)) {
            newPropertyArray.push(value as SearchOption)
        }
        setPropertiesAsArray(newPropertyArray);
    }, [properties])

    return { properties, propertiesLoading, propertiesAsArray }
}
