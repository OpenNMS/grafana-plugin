import { API } from 'opennms'
import { ClientDelegate } from 'lib/client_delegate'
import { useState, useEffect } from 'react'
import { EntityHelper } from '../datasources/entity-ds';
import { Properties, SearchOption } from '../datasources/entity-ds/types';

export const useEntityProperties = (entityName: string, featuredAttributes: boolean, client: ClientDelegate) => {
    const [properties, setProperties] = useState<Properties>({});
    const [propertiesAsArray, setPropertiesAsArray] = useState<SearchOption[]>([]);
    const [propertiesLoading, setPropertiesLoading] = useState(false);

    const loadProperties = async () => {
        const newProperties: API.SearchProperty[] = await EntityHelper.getSearchProperties(entityName, client)

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
