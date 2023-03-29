import React, { useEffect, useState } from 'react'
import { PanelOptionsEditorProps } from '@grafana/data'
import { Input, Select, Switch } from '@grafana/ui'
import { OnmsInlineField } from 'components/OnmsInlineField'
import { SwitchBox } from 'components/SwitchBox'
import { DirectionOptions, DisplayOptions, ModeOptions, PositionOptions, UnitOptions } from './FlowHistogramConstants'
import { FlowHistogramOptionsProps } from './FlowHistogramTypes'

interface FlowHistogramProps {
}

export const FlowHistogramOptions: React.FC<PanelOptionsEditorProps<FlowHistogramProps>> = ({ onChange, context }) => {
    const [options, setOptions] = useState<FlowHistogramOptionsProps>({
        direction: context.options?.flowHistogramOptions?.direction || DirectionOptions[0],
        units: context.options?.flowHistogramOptions?.units || UnitOptions[0],
        display: context.options?.flowHistogramOptions?.display || DisplayOptions[0],
        mode: context.options?.flowHistogramOptions?.mode || ModeOptions[0],
        showLegend: context.options?.flowHistogramOptions?.showLegend || true,
        position: context.options?.flowHistogramOptions?.position || PositionOptions[0],
        height: context.options?.flowHistogramOptions?.height || 42
    })

    const updateOptions = (value: any, key: string) => {
        setOptions((oldOptions) => {
            const newOptions = { ...oldOptions }
            newOptions[key] = value;
            return newOptions
        })
    }
    useEffect(() => {
        onChange(options)
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [options])

    return (
        <div>
            <p style={{ marginTop: 12, marginBottom: 3 }}>General</p>
            <OnmsInlineField label='Direction'>
                <Select options={DirectionOptions}
                    value={options?.direction}
                    onChange={(e) => updateOptions(e, 'direction')}
                />
            </OnmsInlineField>
            <OnmsInlineField label='Units'>
                <Select options={UnitOptions}
                    value={options?.units}
                    onChange={(e) => updateOptions(e, 'units')}
                />
            </OnmsInlineField>
            <OnmsInlineField label='Display'>
                <Select options={DisplayOptions}
                    value={options?.display}
                    onChange={(e) => updateOptions(e, 'display')}
                />
            </OnmsInlineField>
            <OnmsInlineField label='Mode'>
                <Select options={ModeOptions}
                    value={options?.mode}
                    onChange={(e) => updateOptions(e, 'mode')}
                />
            </OnmsInlineField>

            <p style={{ marginTop: 20, marginBottom: 3 }}>Legend</p>
            <OnmsInlineField label='Show Legend'>
                <SwitchBox>
                    <Switch
                        value={options?.showLegend}
                        onChange={() => updateOptions(!options?.showLegend, 'showLegend')} />
                </SwitchBox>
            </OnmsInlineField>
            <OnmsInlineField label='Position'>
                <Select options={PositionOptions}
                    value={options?.position}
                    onChange={(e) => updateOptions(e, 'position')}
                />
            </OnmsInlineField>
            <OnmsInlineField label='Height'>
                <Input type='number' max={75} min={20} value={options?.height} onChange={(e) => updateOptions((Number.parseInt(e.currentTarget.value, 10) > 0 ? e.currentTarget.value : 0), 'height')} />
            </OnmsInlineField>
        </div>
    )
}
