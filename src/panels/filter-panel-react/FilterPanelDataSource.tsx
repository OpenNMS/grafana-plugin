import { SelectableValue } from '@grafana/data';
import { InlineField, Select } from '@grafana/ui'
import { GrafanaDatasource, useDatasources } from 'hooks/useDataSources';
import React, { useState, useEffect } from 'react'

export const FilterPanelDataSource: React.FC<{ onChange: Function }> = ({ onChange }) => {

    const allowedDatasources = ['opennms-helm-entity-datasource-react']
    const { datasources } = useDatasources();
    const entityDatasources = datasources?.filter((d) => allowedDatasources.includes(d.type))
    const [datasource, setDatasource] = useState<SelectableValue<GrafanaDatasource>>()
    const selectOptions: Array<SelectableValue<GrafanaDatasource>> = entityDatasources?.map((d) => ({ label: d.name, value: d }))
    useEffect(() => {
        if (datasource) {
            onChange(datasource);
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [datasource])
    return (
        <InlineField label='Data source entity'>
            <div style={{ display: 'flex', gap: '10px' }}>
                <Select options={selectOptions} onChange={(e) => setDatasource(e)} value={datasource} />
            </div>
        </InlineField>
    )
}
