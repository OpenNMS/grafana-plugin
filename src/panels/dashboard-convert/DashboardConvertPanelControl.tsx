import React, { useState } from 'react'
import { PanelProps, SelectableValue } from '@grafana/data'
import { Button, HorizontalGroup, Select, Switch, TextArea, VerticalGroup } from '@grafana/ui'
import { FieldDisplay } from '../../components/FieldDisplay'
import { helmDashboardConvert } from './convert'

interface DashboardConvertPanelProps {
}

export const DashboardConvertPanelControl: React.FC<PanelProps<DashboardConvertPanelProps>> = (props) => {
  const [sourceHelmVersion, setSourceHelmVersion] = useState<SelectableValue<string>>({ value: 'Helm 8', label: 'Helm 8' })
  const [sourceHelmJson, setSourceHelmJson] = useState<string>()
  const [targetHelmVersion, setTargetHelmVersion] = useState<SelectableValue<string>>({ value: 'Helm 9', label: 'Helm 9' })
  const [targetHelmJson, setTargetHelmJson] = useState<string>()
  const [errorMessage, setErrorMessage] = useState<string>()
  const [unhideAllQueries, setUnhideAllQueries] = useState<boolean>(false)

  const onSourceJsonUpdated = (text: string) => {
    setSourceHelmJson(text)
  }

  const onTargetJsonUpdated = (text: string) => {
    setTargetHelmJson(text)
  }

  const doConvert = () => {
    if (sourceHelmVersion.value !== 'Helm 8' || targetHelmVersion.value !== 'Helm 9') {
      setErrorMessage('You must choose Helm versions')
      return
    }

    if (!sourceHelmJson) {
      setErrorMessage('You must enter source Dashboard json')
      return
    }

    const target = helmDashboardConvert(sourceHelmJson, sourceHelmVersion.value, targetHelmVersion.value,
      { unhideAllQueries })

    if (target.isError) {
      setErrorMessage(`Error converting: ${target.errorMessage || ''}`)
      setTargetHelmJson('')
      return
    }

    setTargetHelmJson(target.json)
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
          <span>Convert Dashboard Json to use updated OpenNMS HELM datasources.</span>
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
                <FieldDisplay>{'Source HELM Version:'}</FieldDisplay>
                <Select
                    options={[ { value: 'Helm 8', label: 'Helm 8'}]}
                    value={sourceHelmVersion}
                    menuShouldPortal={true}
                    onChange={(value) => setSourceHelmVersion(value)} />
              </HorizontalGroup>
              <TextArea
                placeholder='Enter Source Dashboard Json'
                rows={6}
                value={sourceHelmJson}
                onChange={(el) => onSourceJsonUpdated(el.currentTarget.value)}
              />
            </VerticalGroup>
            <VerticalGroup>
              <HorizontalGroup>
                <FieldDisplay>{'Target HELM Version:'}</FieldDisplay>
                <Select
                    options={[ { value: 'Helm 9', label: 'Helm 9'}]}
                    value={targetHelmVersion}
                    menuShouldPortal={true}
                    onChange={(value) => setTargetHelmVersion(value)} />
              </HorizontalGroup>
              <TextArea
                placeholder='Target Dashboard Json can be copied from here after conversion'
                readOnly={true}
                rows={6}
                value={targetHelmJson}
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
