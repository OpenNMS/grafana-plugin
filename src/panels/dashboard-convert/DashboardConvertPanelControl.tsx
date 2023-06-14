import React, { useState } from 'react'
import { PanelProps, SelectableValue } from '@grafana/data'
import { Button, HorizontalGroup, Input, Select, Switch, TextArea, VerticalGroup } from '@grafana/ui'
import { FieldDisplay } from '../../components/FieldDisplay'
import { dashboardConvert, getDashboardTitle } from '../../lib/dashboard-convert'

interface DashboardConvertPanelProps {
}

export const DashboardConvertPanelControl: React.FC<PanelProps<DashboardConvertPanelProps>> = (props) => {
  const [sourcePluginVersion, setSourcePluginVersion] = useState<SelectableValue<string>>({ value: 'Version 8', label: 'Version 8' })
  const [sourceDashboardJson, setSourceDashboardJson] = useState<string>()
  const [targetPluginVersion, setTargetPluginVersion] = useState<SelectableValue<string>>({ value: 'Version 9', label: 'Version 9' })
  const [targetDashboardJson, setTargetDashboardJson] = useState<string>()
  const [dashboardTitle, setDashboardTitle] = useState<string>('')
  const [errorMessage, setErrorMessage] = useState<string>()
  const [unhideAllQueries, setUnhideAllQueries] = useState<boolean>(false)
  const [convertGraphToTimeSeries, setConvertGraphToTimeSeries] = useState<boolean>(false)

  const onSourceJsonUpdated = (text: string) => {
    if (!sourceDashboardJson) {
      // initial update, set the initial dashboard title
      const title = getDashboardTitle(text)

      if (title) {
        setDashboardTitle(title)
      }
    }

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

    const options = { unhideAllQueries, convertGraphToTimeSeries }

    const target = dashboardConvert(sourceDashboardJson, sourcePluginVersion.value, targetPluginVersion.value,
      dashboardTitle, options)

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
              .dashboard-title-input {
                min-width: 360px;
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
              <FieldDisplay>{'Convert Graph to Timeseries Panels:'}</FieldDisplay>
              <Switch
                  value={convertGraphToTimeSeries}
                  onChange={() => setConvertGraphToTimeSeries(!convertGraphToTimeSeries)}
              />
            </HorizontalGroup>
        </div>

         <HorizontalGroup spacing={'lg'}>
            <FieldDisplay>{'Dashboard Title:'}</FieldDisplay>
            <Input
              className='dashboard-title-input'
              value={dashboardTitle}
              onChange={(el) => setDashboardTitle(el.currentTarget.value)}
            />
         </HorizontalGroup>
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
                cols={40}
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
                cols={40}
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
