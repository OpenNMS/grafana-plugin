import React, { useState } from 'react'
import { PanelProps, SelectableValue } from '@grafana/data'
import { Button, HorizontalGroup, Select, Switch, TextArea, VerticalGroup } from '@grafana/ui'
import { FieldDisplay } from '../../components/FieldDisplay'
import { dashboardConvert } from './convert'

interface DashboardConvertPanelProps {
}

export const DashboardConvertPanelControl: React.FC<PanelProps<DashboardConvertPanelProps>> = (props) => {
  const [sourcePluginVersion, setSourcePluginVersion] = useState<SelectableValue<string>>({ value: 'Version 8', label: 'Version 8' })
  const [sourceDashboardJson, setSourceDashboardJson] = useState<string>()
  const [targetPluginVersion, setTargetPluginVersion] = useState<SelectableValue<string>>({ value: 'Version 9', label: 'Version 9' })
  const [targetDashboardJson, setTargetDashboardJson] = useState<string>()
  const [errorMessage, setErrorMessage] = useState<string>()
  const [unhideAllQueries, setUnhideAllQueries] = useState<boolean>(false)

  const onSourceJsonUpdated = (text: string) => {
    setSourceDashboardJson(text)
  }

  const onTargetJsonUpdated = (text: string) => {
    setTargetDashboardJson(text)
  }

  const doConvert = () => {
    if (sourcePluginVersion.value !== 'Version 8' || targetPluginVersion.value !== 'Version 9') {
      setErrorMessage('You must choose Plugin versions')
      return
    }

    if (!sourceDashboardJson) {
      setErrorMessage('You must enter source Dashboard json')
      return
    }

    const target = dashboardConvert(sourceDashboardJson, sourcePluginVersion.value, targetPluginVersion.value,
      { unhideAllQueries })

    if (target.isError) {
      setErrorMessage(`Error converting: ${target.errorMessage || ''}`)
      setTargetDashboardJson('')
      return
    }

    setTargetDashboardJson(target.json)
    setErrorMessage('')
  }

  return (
    <>
      <style>
          {
              `
              .error {
                color: #f00;
                font-weight: bold;
              }
              `
          }
      </style>
      <div>
        <VerticalGroup spacing={'lg'}>
          <span>Convert Dashboard Json to use updated OpenNMS Plugin for Grafana datasources.</span>
          {
            errorMessage && (
              <div className={'error'}>
                { errorMessage }
              </div>
            )
          }
          <div>
            <span>Options</span>
            <HorizontalGroup>
              <FieldDisplay>{'Unhide all queries:'}</FieldDisplay>
              <Switch
                  value={unhideAllQueries}
                  onChange={() => setUnhideAllQueries(!unhideAllQueries)}
              />
            </HorizontalGroup>
        </div>
         <HorizontalGroup spacing={'lg'}>
            <VerticalGroup>
              <HorizontalGroup>
                <FieldDisplay>{'Source Plugin Version:'}</FieldDisplay>
                <Select
                    options={[ { value: 'Version 8', label: 'Version 8'}]}
                    value={sourcePluginVersion}
                    menuShouldPortal={true}
                    onChange={(value) => setSourcePluginVersion(value)} />
              </HorizontalGroup>
              <TextArea
                placeholder='Enter Source Dashboard Json'
                rows={6}
                value={sourceDashboardJson}
                onChange={(el) => onSourceJsonUpdated(el.currentTarget.value)}
              />
            </VerticalGroup>
            <VerticalGroup>
              <HorizontalGroup>
                <FieldDisplay>{'Target Plugin Version:'}</FieldDisplay>
                <Select
                    options={[ { value: 'Version 9', label: 'Version 9'}]}
                    value={targetPluginVersion}
                    menuShouldPortal={true}
                    onChange={(value) => setTargetPluginVersion(value)} />
              </HorizontalGroup>
              <TextArea
                placeholder='Target Dashboard Json can be copied from here after conversion'
                readOnly={true}
                rows={6}
                value={targetDashboardJson}
                onChange={(el) => onTargetJsonUpdated(el.currentTarget.value)}
              />
            </VerticalGroup>
          </HorizontalGroup>
          <Button
            onClick={() => doConvert()}
          >
            Convert
          </Button>
        </VerticalGroup>
      </div>
    </>
  )
}
