import { SelectableValue } from '@grafana/data';
import { InlineField, Select } from '@grafana/ui'
import { GrafanaDatasource, useDatasources } from 'hooks/useDataSources';
import React from 'react'

interface FilterPanelDataSourceProps {
    datasource?: SelectableValue<GrafanaDatasource>,
    onChange: Function
}

export const FilterPanelDataSource: React.FC<FilterPanelDataSourceProps> = ({ datasource, onChange }) => {
    const allowedDatasources = ['opennms-helm-entity-datasource']
    const { datasources } = useDatasources();
    const entityDatasources = datasources?.filter((d) => allowedDatasources.includes(d.type))
    const selectOptions: Array<SelectableValue<GrafanaDatasource>> = entityDatasources?.map((d) => ({ label: d.name, value: d }))

    return (
        <InlineField label='Data source entity'>
            <div style={{ display: 'flex', gap: '10px' }}>
                <Select
                    options={selectOptions}
                    onChange={(e) => onChange(e)}
                    value={datasource} />
            </div>
        </InlineField>
    )
}
